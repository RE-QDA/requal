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
mod_about_server <- function(id, glob){
    moduleServer( id, function(input, output, session){
        ns <- session$ns
        
        output$version_project <- renderText({
            paste0(
                "The current project was created with requal version ",
                dplyr::tbl(glob$pool, "requal_version") %>%
                    dplyr::filter(.data$project_id == local(as.numeric(glob$active_project))) %>% 
                    dplyr::pull(version),
                "."
            )
        })
        
        output$version_package <- renderText({
            paste0("The current version of requal package installed is ", 
                   packageVersion("requal"), ".")
        })
        
    })
}


