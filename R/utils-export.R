# create xml
export_codebook_xml <- function(glob){
  codebook <- dplyr::tbl(glob$pool, "codes") %>%
  dplyr::filter(project_id == as.numeric(!!glob$active_project)) %>%
  dplyr::collect() %>%
  dplyr::mutate(
    guid = uuid::UUIDgenerate(n = nrow(codebook)),
    code_color = purrr::map_chr(code_color, convert_colour_to_hex)
  )

  categories <- dplyr::tbl(glob$pool, "categories") %>%
    dplyr::filter(project_id == as.numeric(!!glob$active_project)) %>%
    dplyr::collect() %>%
    dplyr::mutate(guid = uuid::UUIDgenerate(n = nrow(categories)))
    
  categories_map <- dplyr::tbl(glob$pool, "categories_codes_map") %>%
    dplyr::filter(project_id == as.numeric(!!glob$active_project)) %>%
    dplyr::collect() %>%
    dplyr::left_join(codebook %>% dplyr::select(code_id, membercode_guid = guid), by = "code_id") %>%
    dplyr::left_join(categories %>% dplyr::select(category_id, guid), by = "category_id") 
    
  codebook_xml <- xml2::xml_new_root("CodeBook")
  xml2::xml_attr(codebook_xml, "xmlns") <- "urn:QDA-XML:codebook:1.0"
  xml2::xml_attr(codebook_xml, "xmlns:xsi") <- "http://www.w3.org/2001/XMLSchema-instance"
  xml2::xml_attr(codebook_xml, "xsi:schemaLocation") <- "urn:QDA-XML:codebook:0:4 Codebook.xsd"
  xml2::xml_attr(codebook_xml, "origin") <- paste0("requal (version: ", get_project_version(glob), ")")

  codes <- xml2::xml_add_child(codebook_xml, "Codes")
  # TODO? does not handle nesting of codes
  for(i in 1:nrow(codebook)){
    code <- xml2::xml_add_child(codes, "Code")
    xml2::xml_attr(code, "guid") <- codebook$guid[i]
    xml2::xml_attr(code, "name") <- codebook$code_name[i]
    xml2::xml_attr(code, "color") <- codebook$code_color[i]
    xml2::xml_attr(code, "isCodable") <- "true"
    if(codebook$code_description[i] != ""){
      description <- xml2::xml_add_child(code, "Description", codebook$code_description[i])
    }
  }

  sets <- xml2::xml_add_child(codebook_xml, "Sets")
  for(i in 1:nrow(categories)){
    set <- xml2::xml_add_child(sets, "Set")
    xml2::xml_attr(set, "guid") <- categories$guid[i]
    xml2::xml_attr(set, "name") <- categories$category_name[i]
    if(categories$category_description[i] != ""){
      description <- xml2::xml_add_child(set, "Description", categories$category_description[i])
    }
    if(categories$guid %in% categories_map$guid){
      tmp <- categories_map %>%
        dplyr::filter(guid == categories$guid[i])
      for(j in 1:nrow(tmp)){
        membercode <- xml2::xml_add_child(set, "MemberCode")
        xml2::xml_attr(membercode, "guid") <- tmp$membercode_guid[j]
      }
    }
  }
  codebook_xml  
}
