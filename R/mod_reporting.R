#' reporting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_reporting_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(type = "tabs", id = ns("reporting_tabset"),
                tabPanel("Instructions",
                         id = ns("instructions"),
                         value = "instructions",
                         textOutput(ns("report_instructions"))),
                tabPanel("Logs",
                         id = ns("logs"),
                         value = "logs",
                         actionButton(ns("logs_refresh"),
                                      label = "",
                                      icon = icon("sync")) %>%
                           tagAppendAttributes(style = "float:right;"),
                         DT::dataTableOutput(ns("report_logs")))
    )

  )
}

#' reporting Server Functions
#'
#' @noRd
mod_reporting_server <- function(id, project){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # instructions ------------
    output$report_instructions <- renderText(
      "Instructions for using this module..."
    )

    logs_df <- eventReactive(input$reporting_tabset == "logs" | input$logs_refresh, {

      if(isTruthy(project()$active_project)){
        load_logs_for_reporting(project()$project_db,
                                project()$active_project) %>%
          dplyr::arrange(dplyr::desc(created_at)) %>%
          dplyr::mutate(detail = purrr::map_chr(.data$detail, possibly_parse_payload_json))
      }else{""}
    })

    # logs ------------

    output$report_logs <- DT::renderDataTable(server = FALSE, {
      DT::datatable(
         logs_df(),
        filter = "top",
        extensions = c("Buttons"),

        options = list(
          paging = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = TRUE,
          dom = "lfrtpBi",
          buttons = c("csv")
        ),

        class = "display"
      )

    })

})}
