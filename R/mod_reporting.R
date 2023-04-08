#' reporting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_reporting_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      type = "tabs", id = ns("reporting_tabset"),
      # tabPanel("Instructions",
      #   id = ns("instructions"),
      #   value = "instructions",
      #   textOutput(ns("report_instructions"))
      # ),
      tabPanel("Summary",
               id = ns("summary"),
               value = "summary",
               mod_summary_ui("summary_ui_1")
      ),
      tabPanel("Agreement",
               id = ns("agree"),
               value = "agree",
               mod_agreement_ui("agreement_ui_1")
      ),
      tabPanel("Browse coded docs", 
               id = ns("browser"), 
               value = "browser", 
               mod_browser_ui("browser_ui_1")
      ),
      tabPanel("User attributes", 
               id = ns("user_attributes"), 
               value = "user_attributes", 
               mod_user_attributes_ui("user_attributes_ui_1")
      ), 
      tabPanel("Logs",
               id = ns("logs"),
               value = "logs",
               actionButton(ns("logs_refresh"),
                            label = "",
                            icon = icon("sync")
               ) %>%
                 tagAppendAttributes(style = "float:right;"),
               DT::dataTableOutput(ns("report_logs"))
      )
    )
  )
}

#' reporting Server Functions
#'
#' @noRd
mod_reporting_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    loc <- reactiveValues()
    
    # instructions ------------
    # output$report_instructions <- renderText(
    #   "Instructions for using this module..."
    # )
    
    observeEvent( {req(input$reporting_tabset == "logs") | input$logs_refresh}, {
      if (isTruthy(glob$active_project)) {
        loc$logs_df <- load_logs_for_reporting(
          glob$pool,
          glob$active_project
        ) %>%
          dplyr::arrange(dplyr::desc(created_at)) 
        
        if(!is.null(glob$user$data) && 
           !is.null(glob$user$data$report_other_view) &&
           glob$user$data$report_other_view == 1){
          loc$logs_df <- loc$logs_df %>%
            dplyr::mutate(detail = purrr::map_chr(.data$detail, possibly_parse_payload_json)) 
        }else{
          loc$logs_df <- loc$logs_df %>%
            dplyr::mutate(detail = purrr::map_chr(.data$detail, function(x) possibly_parse_payload_json(x, sanitize = TRUE))) 
        }
      } else {
        ""
      }
    })
    
    # logs ------------
    
    output$report_logs <- DT::renderDataTable(server = FALSE, {
      DT::datatable(
        req(loc$logs_df),
        filter = "top",
        extensions = c("Buttons"),
        options = dt_options(),
        class = "display"
      )
    })
  })
}
