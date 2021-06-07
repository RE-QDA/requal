create_project_db <- function(project_directory, project_name) {
  con <- DBI::dbConnect(RSQLite::SQLite(),
                        fs::path_join(c(
                            project_directory,
                            paste0(gsub(
                                "[^a-zA-Z]+", "", iconv(project_name, to = "ASCII//TRANSLIT")
                            ), ".sqlite")
                        )))
  
  DBI::dbDisconnect(con)
}