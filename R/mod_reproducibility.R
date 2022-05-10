#' reproducibility UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_reproducibility_ui <- function(id){
  ns <- NS(id)
  tagList(

      actionButton(ns("test"), "test"),
      textOutput(ns("mean_no_codes"))
  )
}
    
#' reproducibility Server Functions
#'
#' @noRd 
mod_reproducibility_server <- function(id, project){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$mean_no_codes <- eventReactive(input$test, {
      browser()
      
      segments <- load_all_segments_db(project_db = project()$project_db, 
                                       active_project = project()$active_project)
      # TODO: update function
      segments %>% 
        dplyr::count(user_id) %>% 
        dplyr::pull(n) %>% 
        mean()
      
      # 1. kolik % segmentů má překryv a kolik ne → zobrazit prostý referenční překryv, tzn. číslo 
      # (zohlednit nějak počet kodérů) a jeden dokument se všemi kódovanými texty pod sebou; dvěma 
      # barvami vyznačené překrývající a nepřekrývající segmenty
      
      # 2. stejný výpočet a vizualizaci pro každý kód
      
      # 3. pro vybraný kód spočítat, kolik % segmentů má překryv dvou, tří a čtyř kodérů + 
      # heatmapa hodnot překryvu jednotlivých kodérů
      
    })
    
    
    
  })
}
    
