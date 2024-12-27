xml_file <- xml2::read_xml("/Users/radimhladik/Downloads/refi/Die-Hexen-und-der-Böse-Feind/project.qde")
xml2df <- function(xml_file, xpath, ns = "urn:QDA-XML:project:1.0") {
qda_ns <- c(qda = ns)
xml_file |> 
xml2::xml_find_all(xpath,
ns = qda_ns) |> 
purrr::map_df(xml2::xml_attrs)
}
users <- xml2df(xml_file, "//qda:Project/qda:Users/qda:User")
codes <- xml2df(xml_file, "//qda:Project/qda:CodeBook/qda:Codes/qda:Code")
sources <- xml2df(xml_file, "//qda:Project/qda:Sources/qda:TextSource")

xml2df(xml_file, "//qda:Project")

dplyr::left_join(sources, users, 
by = dplyr::join_by("creatingUser" == "guid"),
suffix = c("_source", "_user")) 
