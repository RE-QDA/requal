list_db_codes <- function(project_db = "/Users/radimhladik/AAaaaa_test_requal/test.requal") {
    
    
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          project_db
    )
    on.exit(DBI::dbDisconnect(con))
    
    
    project_codes <- dplyr::tbl(con, "codes") %>% 
        dplyr::select(code_id, code_name) %>% 
        dplyr::collect()
    
    return(project_codes)
}

gen_codes_ui <- function(code_id, code_name) {
    
    box(
        id = code_id,
        title = code_name, 
        closable = FALSE,
        width = 12,
        background = "light-blue",
        collapsible = TRUE,
        collapsed = TRUE,
        boxToolSize = "md",
        label = boxLabel(text = "code",
                         status = "warning"),
        dropdownMenu = boxDropdown(
            boxDropdownItem("Edit"),
            boxDropdownItem("Merge"),
            boxDropdownItem("Delete")
        ),
        p("Code description")
    )
    
}