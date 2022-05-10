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
    selectInput(ns("metrics_select"), 
                "Select metrics", 
                choices = c("", "Total overlap" = "total", "Overlap by code" = "by_code")),
    actionButton(ns("test"), "Calculate"),
    textOutput(ns("mean_no_codes")), 
    tableOutput(ns("mean_by_codes"))
  )
}
    
#' reproducibility Server Functions
#'
#' @noRd 
mod_reproducibility_server <- function(id, project){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      if(input$metrics_select == "total"){
        output$mean_no_codes <- eventReactive(input$test, {
          segments <- load_all_segments_db(project_db = project()$project_db, 
                                           active_project = project()$active_project)
          
          # TODO: update function
          # 1. kolik % segmentů má překryv a kolik ne → zobrazit prostý referenční překryv, tzn. číslo 
          # (zohlednit nějak počet kodérů) a jeden dokument se všemi kódovanými texty pod sebou; dvěma 
          # barvami vyznačené překrývající a nepřekrývající segmenty
          
          segments %>% 
            dplyr::count(user_id) %>% 
            dplyr::pull(n) %>% 
            mean()
        })
      }
      
      # 2. stejný výpočet a vizualizaci pro každý kód
      if(input$metrics_select == "by_code"){
        observeEvent(input$test, {
          segments <- load_all_segments_db(project_db = project()$project_db, 
                                           active_project = project()$active_project)
          
          output$mean_by_codes <- renderTable({segments %>% 
            dplyr::count(code_id, user_id) %>% 
            dplyr::group_by(code_id) %>% 
            dplyr::summarise(mean = mean(n), .groups = "drop")})
        })
        
      }
    })

    
    # 3. pro vybraný kód spočítat, kolik % segmentů má překryv dvou, tří a čtyř kodérů + 
    # heatmapa hodnot překryvu jednotlivých kodérů
    
    
    
  })
}
    
