# File structure:
# 1. Main parsing function
# 1.1 Parsing helpers
# 1.2 Parsing utils 
# 2. Import function
# 2.1 Import helpers
# 2.2 Import utils

# 1. Main parsing function ----

parse_qdpx <- function(path) {
  converting_msg <- showNotification(
    "Converting project...",
    duration = NULL,
    closeButton = FALSE
  )
  on.exit(removeNotification(converting_msg), add = TRUE)
  import_dir <- paste0(tempdir(), .Platform$file.sep, "refiqda")
  utils::unzip(path, exdir = import_dir)
  refi_ns <- c(qda = "urn:QDA-XML:project:1.0")
  # xml_file <- xml2::read_xml("~/Downloads/refi/Die-Hexen-und-der-Böse-Feind/project.qde")
  xml_file <- xml2::read_xml(paste0(import_dir, .Platform$file.sep, "project.qde"))
  xml_schema <- xml2::read_xml(system.file('refi.xsd', package = 'requal'))
  

  # check file against validation schema
  if (as.logical(xml2::xml_validate(xml_file, xml_schema))) {
    rql_message("Valid REFI-QDA file.")
  } else {
    rql_message("Invalid REFI-QDA file.")
  }
  # extract data from file
  imp_project <- list()
  imp_project$project <- .refi_get_project(xml_file, refi_ns = refi_ns)
  imp_project$users <- .refi_get_users(xml_file, refi_ns = refi_ns)
  imp_project$codebook <- .refi_get_codebook(xml_file, refi_ns = refi_ns)
  imp_project$sources <- .refi_get_sources(xml_file, refi_ns = refi_ns, import_dir = import_dir)
  #imp_project$sets <- .xml2df(xml_file, "//qda:Project/qda:Sets/qda:Set")
  #xml2::xml_find_all(xml_file, "//qda:Project/qda:Links", ns = refi_ns)
  return(imp_project)
}

# 1.1 Parsing helpers ----
# Parse project information ----
.refi_get_project <- function(xml_file, refi_ns) {
  project_node <- xml2::xml_find_all(xml_file, "//qda:Project", ns = refi_ns)
  description <- xml2::xml_find_first(project_node, ".//qda:Description", ns = refi_ns) |>
    xml2::xml_text()
  origin <- project_node |> xml2::xml_attr("origin")
  createdDateTime <- project_node |>
    xml2::xml_attr("createdDateTime") |>
    strptime(format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  modifiedDateTime <- project_node |>
    xml2::xml_attr("modifiedDateTime") |>
    strptime(format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  project_description <- glue::glue(
    description, "\n---\n",
    "Origin\n: {origin}\n",
    "Created at\n: {createdDateTime}\n",
    "Modified at\n: {modifiedDateTime}\n"
  )
  project_name <- project_node |> xml2::xml_attr("name")
  project_df <- tibble::tibble(
    project_name = as.character(project_name),
    project_description = as.character(project_description),
  )
  return(project_df)
}

# Parse users information ----
.refi_get_users <- function(xml_file, refi_ns) {
  users_node <- xml2::xml_find_all(xml_file, "//qda:Users/qda:User", ns = refi_ns)
  user_names <- users_node |> xml2::xml_attr("name")
  user_ids <- users_node |> xml2::xml_attr("id")
  user_ids[is.na(user_ids)] <- user_names[is.na(user_ids)]
  user_ids <- make.unique(user_ids, sep = "_")
  user_guids <- users_node |> xml2::xml_attr("guid")
  users_df <- tibble::tibble(
    user_login = as.character(user_ids),
    user_name = as.character(user_names),
    user_guid = as.character(user_guids)
  )
  return(users_df)
}

# Parse codebook information ----
.refi_get_codebook <- function(xml_file, refi_ns) {
  codes <- xml2::xml_find_all(xml_file, "//qda:Project/qda:CodeBook/qda:Codes//qda:Code", ns = refi_ns)
  parent_guids <- purrr::map_df(codes, .f = function(x) {
    tibble::tibble(
      guid = xml2::xml_attr(x, "guid"),
      parent_guid = xml2::xml_parent(x) |>
        xml2::xml_attr("guid")
    )
  })

  codes_df <- xml2::xml_attrs(codes) |> dplyr::bind_rows()
  
  # join code relationships to codes dataframe and reassign names
  codes_df_joined <- codes_df |>
    dplyr::left_join(parent_guids, by = "guid")
  codes_df_renamed <- codes_df_joined |> 
   dplyr::mutate(name = purrr::map2_chr(parent_guid, name, .f = function(x, y){
      code_name <- y
      while (!is.na(x)) {
        df <- codes_df_joined |> 
         dplyr::filter(guid == x)
        # get code name
        code_name <- paste(
          df |> dplyr::pull(name), 
        code_name, 
        sep = " :: "
        )
        # update x
        x <- df |> 
         dplyr::pull(parent_guid)
      }
      code_name
    }))

  # get codes descriptions
  descriptions <- purrr::map_chr(codes, .f = function(code_node) {
    desc <- xml2::xml_find_first(code_node, ".//qda:Description", ns = refi_ns)
    if (is.na(desc)) {
      return("")
    } else {
      return(xml2::xml_text(desc))
    }
  })

  # clean up result
  codes_converted <- codes_df_renamed |>
    dplyr::mutate(description = descriptions) |>
    dplyr::mutate(code_color = purrr::map_chr(color,
      .f = function(input_color) {
        rgb_colours <- grDevices::col2rgb(input_color)
        paste0(
          "rgb(",
          rgb_colours[1], ",",
          rgb_colours[2], ",",
          rgb_colours[3], ")"
        )
      }
    )) |>
    dplyr::mutate(isCodable = stringr::str_detect(isCodable, "[Tt]rue")) |>
    # so far no support for non-codable codes in reQual
    dplyr::filter(isCodable) |>
    dplyr::select(
      code_name = name,
      code_description = description,
      code_color,
      guid,
      parent_guid
    )
    codes_df_ids <- codes_converted |> 
    tibble::rownames_to_column("code_id") 
    codes_final <- codes_df_ids |> 
    dplyr::left_join(
      codes_df_ids |> 
      dplyr::select(code_parent_id = code_id, parent_guid = guid),
      by = "parent_guid"
    ) 
  return(codes_final)
}

# Parse sources ----
.refi_get_sources <- function(xml_file, refi_ns, import_dir) {
  sources <- xml2::xml_find_all(xml_file, "//qda:Project/qda:Sources", ns = refi_ns)
  img_df <- .refi_get_src_type(sources, src_type = "PictureSource")
  pdf_df <- .refi_get_src_type(sources, src_type = "PDFSource")
  audio_df <- .refi_get_src_type(sources, src_type = "AudioSource")
  video_df <- .refi_get_src_type(sources, src_type = "VideoSource")

  # TODO when support for sources implemented
  purrr::walk2(list(img_df, pdf_df, audio_df, video_df),
    list("Picture", "PDF", "Audio", "Video"),
    .f = function(x, y) {
      msg <- paste0("Ignoring ", nrow(x), " sources of type '", y, "'.")
      rql_message(msg)
    }
  )
  txt_nodes <- xml2::xml_find_all(sources, ".//qda:TextSource", ns = refi_ns)
  rql_message(paste0("Importing ", length(txt_nodes), " sources of type 'Text'."))
  txt_Description <- .get_xml_el_txt(txt_nodes, "Description") 
  txt_PlainTextContent <- .get_xml_el_txt(txt_nodes, "PlainTextContent")
  txt_NoteRef <- .get_xml_el_txt(txt_nodes, "NoteRef")
  txt_VariableValue <- .get_xml_el_txt(txt_nodes, "VariableValue")
  # xml2::xml_find_all(sources, ".//qda:PlainTextSelection", ns = refi_ns) |> 
  # xml2::xml_attrs() |> dplyr::bind_rows()
  txt_df <- xml2::xml_attrs(txt_nodes) |> 
  dplyr::bind_rows() |> 
  dplyr::mutate(
    doc_description = txt_Description,
    PlainTextContent = txt_PlainTextContent
  ) |> 
  dplyr::mutate(
    doc_text = purrr::map_chr(plainTextPath, ~.read_plaintext_path(.x, import_dir))
  )
}

# Parse selections ----
.refi_get_selections <- function(xml_file, refi_ns, codebook) {
  sources <- xml2::xml_find_all(xml_file, "//qda:TextSource", ns = refi_ns)
  txt_sel <- sources  |> 
  xml2::xml_find_all(".//qda:PlainTextSelection", ns = refi_ns)  |> 
  purrr::map(xml2::xml_attrs)  |> 
  purrr::map(dplyr::bind_rows)
  txt_sel_desc <- purrr::map(sources, .refi_get_child, "//qda:PlainTextSelection/qda:Description")  |> 
  purrr::map(xml2::xml_text)
  txt_sel_coding <- purrr::map(sources, .refi_get_child, "/qda:Coding")  |> 
  purrr::map(xml2::xml_attrs) |> purrr::map(dplyr::bind_rows) 
  txt_sel_coding_ref <- purrr::map(sources, .refi_get_child, "//qda:PlainTextSelection/qda:Coding/qda:CodeRef")  |> 
  purrr::map(xml2::xml_attrs) |> purrr::map(dplyr::bind_rows) |>  purrr::map(~dplyr::rename(.x, guid = targetGUID)) 
  # i need to match coding refs to codebook
  txt_sel_matched <- txt_sel_coding_ref |> purrr::map(~dplyr::right_join(.x, imp_project$codebook))

txt_sel_matched[[1]] |> View()

# Find all <TextSource> elements
sources <- xml2::xml_find_all(xml_file, "//qda:TextSource", ns = refi_ns)
source <- sources[[1]]
# Extract <PlainTextSelection> elements from each <TextSource> element
txt_sel <- purrr::map_df(sources, function(source) {
  text_source <- xml2::xml_find_first(source, "./qda:Description", ns = refi_ns) |> xml2::xml_text()
  selections <- xml2::xml_find_all(source, ".//qda:PlainTextSelection", ns = refi_ns) |>
    purrr::map(xml2::xml_attrs) |>
    dplyr::bind_rows() |> 
    dplyr::mutate(text_source = text_source)
  selections
})
txt_sel |> View()
  # TODO PICK UP HERE
  
}

# 1.2 Parsing utils ----
.refi_get_child <- function(node, xpath, ns = c(qda = "urn:QDA-XML:project:1.0")) {
  node |>
    xml2::xml_find_all(xpath,
      ns = ns
    )
}

.refi_get_src_type <- function(sources, src_type = "TextSource") {
  src <- sources |>
    .refi_get_child(paste0("//qda:", src_type))
  type_descriptions <- purrr::map(src, ~ .refi_get_child(.x, "qda:Description")) |>
    purrr::map_chr(.f = function(x) {
      if (length(x) == 0) {
        ""
      } else {
        xml2::xml_text(x)
      }
    })
  type_df <- sources |>
    .refi_get_child(paste0("//qda:", src_type)) |>
    xml2::xml_attrs() |>
    (function(x) do.call(rbind, x))()
  tibble::as_tibble(type_df) |>
    dplyr::mutate(doc_description = type_descriptions)
}

.get_xml_el_txt <- function(node, el){
  txt_vec <- purrr::map(node, xml2::xml_find_all, paste0("qda:", el), ns = c(qda = "urn:QDA-XML:project:1.0")) |> 
  purrr::map_chr(.f = function(x){
  y <- xml2::xml_text(x)
  if (length(y)==0) {
    y <- as.character("")
  }
  return(y)
  }
  )
  return(txt_vec)
}

.read_plaintext_path <- function(file, import_dir){
  path_to_file <- paste0(import_dir, .Platform$file.sep, "sources", .Platform$file.sep, basename(file))
  if (file.exists(path_to_file)){
  plain_text <- paste0(readLines(path_to_file, encoding = "UTF-8", warn = FALSE), collapse = "\n") 
  } else {
  plain_text <- ""
  }
}

# 2 Main import function ----
import_project <- function(content, user_id, active_project, pool) {
#   require(shiny)
# pool <- pool::dbPool(
#       drv = RPostgreSQL::PostgreSQL(),
#       host = "localhost",
#       dbname = "requal",
#       user = "requal_admin",
#       password = "test"
#     )  
# user_id <- 1
# active_project <- 6
# content <- imp_project

.import_project_info(project_info = content$project, user_id, active_project, pool)
.import_codebook(codebook = content$codebook, user_id, active_project, pool)
.import_sources(sources = content$sources, user_id, active_project, pool)

}

# 2.1 Import helpers ----
# Import project info ----
.import_project_info <- function(project_info, user_id, active_project, pool){
  DBI::dbExecute(pool,
               glue::glue_sql("UPDATE projects
                                SET project_name = {project_info$project_name}, 
                                    project_description = {project_info$project_description}
                                WHERE project_id = {active_project}", .con = pool)
             )
}

# Import codebook ----
.import_codebook <- function(codebook, user_id, active_project, pool){
codebook <- codebook |>
        dplyr::select(
          code_id,
          code_name,
          code_description,
          code_color,
          code_parent_id
        ) |>
        dplyr::mutate(
          project_id = active_project,
          user_id = user_id
        ) |>
        dplyr::mutate(
          code_id = as.integer(code_id),
          code_parent_id = as.integer(code_parent_id)
        )
      # clear contents of the codes table
        clear_tbl_project(pool, "codes", active_project)

      # Make sure column exists for parent information
      query <- "ALTER TABLE codes ADD COLUMN IF NOT EXISTS code_parent_id integer;"
      DBI::dbExecute(pool, query)
      query <- "ALTER TABLE codes ADD COLUMN IF NOT EXISTS original_code_id integer;"
      DBI::dbExecute(pool, query)
      # temporarily write into DB with original code_id
      codebook_renamed <- codebook |>
        dplyr::rename(original_code_id = code_id)
      DBI::dbWriteTable(pool, "codes", codebook_renamed, append = TRUE, row.names = FALSE)
      # read back from DB
      current_codes <- dplyr::tbl(pool, "codes") |>
        dplyr::filter(project_id == as.integer(!!active_project)) |>
        dplyr::select(original_code_id, code_id, code_parent_id) |>
        dplyr::collect()
      # map DB code_id of parents to original code_id
      recoded <- current_codes |>
        dplyr::left_join(
          current_codes |>
            dplyr::select(original_code_id, new_parent_id = code_id),
          by = c("code_parent_id" = "original_code_id")
        ) |> 
        na.omit()
      # update original code_id to DB code_id for parents
      purrr::walk2(recoded$code_id, recoded$new_parent_id,
        .f = function(code, new_parent) {
          DBI::dbExecute(
            pool,
            glue::glue_sql("UPDATE codes
                 SET code_parent_id = {new_parent}
                 WHERE code_id = {code}", .con = pool)
          )
        }
      )
      # remove helper column from DB
      query <- "ALTER TABLE codes DROP COLUMN original_code_id;"
      DBI::dbExecute(pool, query)
      # TODO log
}


# Import sources ----
.import_sources <- function(sources, user_id, active_project, pool){
  # sources <- imp_project$sources
sources_clean <- sources |>
        dplyr::mutate(
          project_id = as.integer(active_project),
          user_id = as.integer(user_id),
          created_at = as.character(strptime(modifiedDateTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
        )  |> 
        dplyr::select(
          project_id,
          user_id,
          doc_name = name,
          doc_description,
          doc_text,
          created_at
        ) 

      # clear contents of the documents table
      clear_tbl_project(pool, "documents", active_project)
  
      DBI::dbWriteTable(pool, "documents", sources_clean, append = TRUE, row.names = FALSE)
      # TODO log
}

# 2.2 Import utils ----
# Clear project from table ----
clear_tbl_project <- function(pool, table, active_project){
  DBI::dbExecute(
    pool,
    glue::glue_sql("DELETE FROM {`DBI::SQL(table)`} 
                               WHERE project_id = {active_project}", .con = pool)
  )
}


