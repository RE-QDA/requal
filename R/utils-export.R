utils::globalVariables(c("guid"))

# prepare data.frame with codes and categories to export
export_codebook_csv <- function(glob) {
  categories <- read_db_categories(
    glob$pool,
    glob$active_project,
    glob$user,
    modify = FALSE
  ) %>%
    dplyr::mutate(
      category_title = dplyr::if_else(
        !is.na(category_description) & category_description != "",
        paste0(category_name, " (", category_description, ")"),
        category_name
      )
    ) %>%
    dplyr::select(-c(category_name, category_description))

  categories_map <- dplyr::tbl(glob$pool, "categories_codes_map") %>%
    dplyr::collect() %>%
    dplyr::inner_join(categories, by = "category_id")

  dplyr::left_join(glob$codebook, categories_map, by = "code_id") %>%
    dplyr::group_by(code_id, code_name, code_description, code_color) %>%
    dplyr::summarise(
      categories = paste0(category_title, collapse = " | ")
    ) %>%
    dplyr::mutate(
      code_color = purrr::map_chr(code_color, convert_colour_to_hex)
    )
}

# create xml
export_codebook_xml <- function(glob) {
  codebook <- glob$codebook %>%
    dplyr::mutate(
      guid = uuid::UUIDgenerate(n = dplyr::n()),
      code_color = purrr::map_chr(code_color, convert_colour_to_hex)
    )

  categories <- read_db_categories(
    glob$pool,
    glob$active_project,
    glob$user
  ) %>%
    dplyr::mutate(guid = uuid::UUIDgenerate(n = dplyr::n()))

  if (nrow(categories)) {
    categories_map <- dplyr::tbl(glob$pool, "categories_codes_map") %>%
      dplyr::filter(project_id == as.numeric(!!glob$active_project)) %>%
      dplyr::filter(category_id %in% categories$category_id) %>%
      dplyr::collect() %>%
      dplyr::left_join(
        codebook %>% dplyr::select(code_id, membercode_guid = guid),
        by = "code_id"
      ) %>%
      dplyr::left_join(
        categories %>% dplyr::select(category_id, guid),
        by = "category_id"
      )
  }

  validator_file <- "qdc.xsd"
  validator_schema <- xml2::read_xml(system.file(
    validator_file,
    package = "requal"
  ))
  validator_ns <- xml2::xml_attr(validator_schema, "targetNamespace")

  codebook_xml <- xml2::xml_new_root(
    "CodeBook",
    "xmlns" = validator_ns,
    "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
    "xsi:schemaLocation" = paste(validator_ns, validator_file),
    "origin" = paste0(
      "requal (version: ",
      get_project_version(glob),
      ")"
    )
  )

  codes <- xml2::xml_add_child(codebook_xml, "Codes")
  # TODO? does not handle nesting of codes
  for (i in seq_len(nrow(codebook))) {
    code <- xml2::xml_add_child(codes, "Code")
    xml2::xml_attr(code, "guid") <- codebook$guid[i]
    xml2::xml_attr(code, "name") <- codebook$code_name[i]
    xml2::xml_attr(code, "color") <- codebook$code_color[i]
    xml2::xml_attr(code, "isCodable") <- "true"
    if (
      codebook$code_description[i] != "" && !is.na(codebook$code_description[i])
    ) {
      description <- xml2::xml_add_child(
        code,
        "Description",
        codebook$code_description[i]
      )
    }
  }
  if (nrow(categories)) {
    sets <- xml2::xml_add_child(codebook_xml, "Sets")
    for (i in seq_len(nrow(categories))) {
      set <- xml2::xml_add_child(sets, "Set")
      xml2::xml_attr(set, "guid") <- categories$guid[i]
      xml2::xml_attr(set, "name") <- categories$category_name[i]
      if (
        categories$category_description[i] != "" &&
          !is.na(categories$category_description[i])
      ) {
        description <- xml2::xml_add_child(
          set,
          "Description",
          categories$category_description[i]
        )
      }
      if (categories$guid[i] %in% categories_map$guid) {
        tmp <- categories_map %>%
          dplyr::filter(guid == categories$guid[i])
        for (j in 1:nrow(tmp)) {
          membercode <- xml2::xml_add_child(set, "MemberCode")
          xml2::xml_attr(membercode, "guid") <- tmp$membercode_guid[j]
        }
      }
    }
  }

  tmp_qdcfile <- tempfile(fileext = ".qdc")
  xml2::write_xml(
    codebook_xml,
    tmp_qdcfile,
    encoding = "utf-8",
    declaration = TRUE
  )
  validation_res <- xml2::xml_validate(
    xml2::read_xml(tmp_qdcfile), # use codebook_xml instead when checking for errors
    validator_schema
  )
  if (validation_res) {
    showNotification(
      "Codebook successfully exported.",
      type = "message"
    )
    return(codebook_xml)
  } else {
    showNotification(
      div(
        p("Error exporting codebook:"),
        p(paste(attr(validation_res, "errors"), collapse = ", "))
      ),
      type = "error",
      duration = NULL
    )
    return(NULL)
  }
}
