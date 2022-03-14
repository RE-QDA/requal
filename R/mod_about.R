#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    h2(
      tags$a(href="https://github.com/RE-QDA/requal",
    tags$img(src="www/requal_logo.png", width = "5%"), target = "_blank"),
    "ReQual CAQDAS"
    ), 
    
    textOutput(ns("version_project")),
    
    textOutput(ns("version_package")),
    
    p(),
    
    p("The development of this tool has been supported by", 
      a(href = "https://www.tacr.cz/en/technology-agency-of-the-czech-republic", 
        "The Technology Agency of the Czech Republic",
        target = "_blank"), 
      "project n. TL05000054."
      )
 
  )
}
    
#' about Server Functions
#'
#' @noRd 
mod_about_server <- function(id, project, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    read_version <-
      function(project_db,
               active_project) {
        
        con <- DBI::dbConnect(RSQLite::SQLite(), project_db)
        on.exit(DBI::dbDisconnect(con))
        
        version <- dplyr::tbl(con, "requal_version") %>%
          dplyr::pull(version) 
      }
    
    output$version_project <- renderText({
      if (isTruthy(project()$active_project)) {
        paste0(
          "The current project was created with requal version ",
          read_version(project()$project_db,
                       project()$active_project), ". "
        )
      } else {""}
    })
    
    output$version_package <- renderText({
      paste0("The current version of requal package installed is ", 
      packageVersion("requal"), ".")
    })
 
  })
}
    

