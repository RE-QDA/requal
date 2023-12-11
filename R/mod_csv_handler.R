#' download_csv UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_download_csv_ui <- function(id, type){
  ns <- NS(id)
  tagList(
    downloadButton(ns(type), label = "CSV")
  )
}

# csv file download handling
handle_download <- function(filename, df){
  downloadHandler(
    filename = function() {
      filename
    },
    content = function(file) {
      utils::write.csv(req(df), file, row.names = FALSE)
    }
  )
}

# prepare data.frame with codes and categories to export
get_codebook_export_table <- function(glob){
  categories <- dplyr::tbl(glob$pool, "categories") %>% 
    dplyr::filter(project_id == as.numeric(!!glob$active_project)) %>% 
    dplyr::select(category_id, category_name, category_description) %>% 
    dplyr::collect() %>% 
    dplyr::mutate(category_title = dplyr::if_else(
      !is.na(category_description) & category_description != "",
      paste0(category_name, " (", category_description, ")"), 
      category_name)) %>% 
    dplyr::select(-c(category_name, category_description))
  
  dplyr::tbl(glob$pool, "categories_codes_map") %>% 
    dplyr::collect() %>% 
    dplyr::inner_join(., categories, by = "category_id") %>% 
    dplyr::left_join(glob$codebook, ., by = "code_id") %>% 
    dplyr::group_by(code_id, code_name, code_description) %>% 
    dplyr::summarise(categories = paste0(category_title, collapse = ", "))
}

#' download_csv Server Functions
#'
#' @noRd 
mod_download_csv_server <- function(id, glob){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$download_analysis <- handle_download(
      paste0("requal_export-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".csv"), 
      glob$segments_df
    )
    
    output$download_codebook <- handle_download(
      paste0("requal_codebook_export-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".csv"), 
      get_codebook_export_table(glob)
    )
  })
}
    

