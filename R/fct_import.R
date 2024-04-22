# Main parsing function ----

parse_qdpx <- function(path) {
   
      converting_msg <- showNotification(
        "Converting project...", 
        duration = NULL, 
        closeButton = FALSE
      )
      on.exit(removeNotification(converting_msg), add = TRUE)
    browser()
    # xml_file <- xml2::read_xml("~/Downloads/refi/Die-Hexen-und-der-Böse-Feind/project.qde")
    # refi_ns <- c(qda = "urn:QDA-XML:project:1.0")
  import_dir <- paste0(tempdir(), .Platform$file.sep, "refiqda")
  utils::unzip(path, exdir = import_dir)
  refi_ns <- c(qda = "urn:QDA-XML:project:1.0")
  xml_file <- xml2::read_xml(paste0(import_dir, .Platform$file.sep, "project.qde"))
  xml_schema <- xml2::read_xml("www/refi.xsd")
  # check file against validation schema
  if(xml2::xml_validate(xml_file, xml_schema)) {
    rql_message("Valid REFI-QDA file.") } else {
    rql_message("Invalid REFI-QDA file.")
    }
  # extract data from file
  imp_project <- list()
  imp_project$project   <- .refi_get_project(xml_file)
  imp_project$users     <- .refi_get_users(xml_file)
  imp_project$codebook  <- .refi_get_codebook(xml_file)
  imp_project$sources   <- .refi_get_sources(xml_file)
  imp_project$sets      <- .xml2df(xml_file, "//qda:Project/qda:Sets/qda:Set")

  xml2::xml_find_all(xml_file, "//qda:Project/qda:Links", ns = refi_ns)

}

# Parse project information ----
.refi_get_project <- function(xml_file){
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
    project_description <- glue::glue(description, "\n---\n",
    "Origin\n: {origin}\n",
    "Created at\n: {createdDateTime}\n",
    "Modified at\n: {modifiedDateTime}\n"
    ) 
    project_name  <- project_node |> xml2::xml_attr("name")
    project_df <- tibble::tibble(
      project_name = as.character(project_name),
      project_description = as.character(project_description),
    )
    return(project_df)
}

# Parse users information ----
.refi_get_users <- function(xml_file){
    users_node <- xml2::xml_find_all(xml_file, "//qda:Users/qda:User", ns = refi_ns)
    user_names <- users_node |> xml2::xml_attr("name")
    user_ids <- users_node |> xml2::xml_attr("id")
    user_ids[is.na(user_ids)] <- user_names[is.na(user_ids)]
    user_ids <- make.unique(user_ids, sep = "_")
    user_guids  <-  users_node |> xml2::xml_attr("guid")
    users_df <- tibble::tibble(
      user_login = as.character(user_ids),
      user_name = as.character(user_names),
      user_guid = as.character(user_guids)
    )
    return(users_df)
}

# Parse codebook information ----
.refi_get_codebook <- function(xml_file) {
  codes <- xml2::xml_find_all(xml_file, "//qda:Project/qda:CodeBook/qda:Codes//qda:Code", ns = refi_ns)
codes_paths <- lapply(codes, xml2::xml_path)
# infer hierarchy from xml paths (path enumeration model)
path_enum_model <- sapply(sapply(codes_paths, stringr::str_extract_all, "\\d"), paste0, collapse = "")

codebook <- xml2::xml_find_all(xml_file, "//qda:Project/qda:CodeBook", ns = refi_ns)
codebook_path <- unlist(stringr::str_extract_all(xml2::xml_path(codebook), "\\d")) 
# remove codebook path from codes paths
codes_ids <- stringr::str_replace(path_enum_model, codebook_path, "")
# function to remove last hierarchy level from codes paths
extract_parent <- function(x) {
  substr(as.character(x), 1, nchar(x)-1)
}

# create codes dataframe
codes_df <- purrr::map_df(codes, xml2::xml_attrs) |> 
dplyr::mutate(
  code_id = codes_ids
  )

# create a lookup dataframe for code names
codes_lookup <- codes_df |> 
dplyr::select(code_ref = code_id, name_ref = name)

# function to split code ids into all levels of hierarchy
split_code_id <- function(x) {
  n <- nchar(x)
  sapply(seq(n), function(i) substr(x, 1, i))
}

# get new codenames by using lookup dataframe 
codes_names_df <- codes_df |> 
dplyr::mutate(code_child = purrr::map(code_id, split_code_id)) |> 
tidyr::unnest(code_child) |> 
dplyr::left_join(codes_lookup, by = c("code_child"="code_ref")) |> 
dplyr::summarize(name_modified = paste(name_ref, collapse = " :: "), .by = code_id)

# join code names to codes dataframe and reassign names
codes_df_renamed <- codes_df |> 
dplyr::left_join(codes_names_df, by = "code_id") |> 
dplyr::mutate(name = name_modified) |> 
dplyr::select(-name_modified)

# get codes descriptions
descriptions <- purrr::map_chr(codes, .f = function(code_node){
desc <- xml2::xml_find_first(code_node, ".//qda:Description", ns = refi_ns)
  if (is.na(desc)) {
    return("")  
  } else {
    return(xml2::xml_text(desc))
  }
})

# get codes GUIDs
code_guids <- purrr::map_chr(codes, xml2::xml_attr, "guid")

# clean up result
codes_converted <- codes_df_renamed |> 
dplyr::mutate(description = descriptions) |> 
dplyr::mutate(code_guid = code_guids) |> 
dplyr::mutate(code_color = purrr::map_chr(color, 
.f = function(input_color){
  rgb_colours <- grDevices::col2rgb(input_color)
        paste0("rgb(",
               rgb_colours[1], ",",
               rgb_colours[2], ",",
               rgb_colours[3], ")")
               })
) |> 
dplyr::mutate(isCodable = stringr::str_detect(isCodable, "[Tt]rue")) |> 
dplyr::filter(isCodable) |> 
dplyr::select(
  code_name = name,
  code_description = description,
  code_color,
  code_guid
)
return(codes_converted)
}

# Parse sources ----
.refi_get_sources <- function(xml_file){
    sources <- xml2::xml_find_all(xml_file, "//qda:Project/qda:Sources", ns = refi_ns)
    docs <- sources |> 
    .refi_get_child("//qda:TextSource") 
    doc_descriptions <- lapply(docs, .refi_get_child, "qda:Description") |> 
    purrr::map_chr(.f = function(x) {
      if (length(x) == 0) {
        ""
      } else {
      xml2::xml_text(x)
      }
    }) 
    doc_df <- sources |> .refi_get_child("//qda:TextSource") |> 
        xml2::xml_attrs() |> 
        (function(x) do.call(rbind, x))() |> 
        tibble::as_tibble()  |> 
        dplyr::mutate(doc_description = doc_descriptions)
        #TODO: clean up
}

# Parsing utils ----
.refi_get_child <- function(node, xpath, ns = c(qda = "urn:QDA-XML:project:1.0")) {
node |> 
xml2::xml_find_all(xpath,
ns = ns) 
}

