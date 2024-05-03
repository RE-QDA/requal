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
            "reQual CAQDAS"
        ), 
        
        textOutput(ns("version_project")),
        
        textOutput(ns("version_package")),
        
        p(),
        HTML(paste0("For help, consult ", 
                    a("reQual Wiki", href="https://github.com/RE-QDA/requal/wiki", target = "_blank"), 
                    ".")),
        p(),
        p("The development of this tool has been supported by:", 
            tags$ul(
                tags$li(
          a(href = "https://www.tacr.cz/en/technology-agency-of-the-czech-republic", 
            "The Technology Agency of the Czech Republic",
            target = "_blank"), 
          "project n. TL05000054."
        )),
        tags$li(
             a(href = "https://clsinfra.io/", 
            "CLS INFRA Fellowship programme",
            target = "_blank")
        )
        ),
        p(
          "If you use this software, please cite it as",
          tags$cite(paste(citation("requal"), citation("requal")$note))
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


