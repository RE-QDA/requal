#' app for configuring path to DB
#'
#' @description A shiny app
#'
#' @noRd
#'
#' @importFrom shiny runApp tagList
config_db_app <- function(){
    shiny::runApp(
        list(
            ui = shiny::bootstrapPage(
                shiny::verbatimTextOutput("sel_directory"),
                shinyFiles::shinyDirButton(
                    "sel_directory",
                    "Folder select",
                    "Please select a project folder"
                ),
                shiny::actionButton("config_btn", "Confirm configuration"),
            ),
            server = function(input, output, session) {
                volumes <- c(Home = fs::path_home(), get_volume_paths())
                
                shinyFiles::shinyDirChoose(
                    input,
                    "sel_directory",
                    roots = volumes,
                    defaultRoot = "Home",
                    session = session,
                    restrictions = system.file(package = "base"),
                    allowDirCreate = TRUE
                )
                
                output$sel_directory <- shiny::renderPrint({
                    if (is.integer(input$sel_directory)) {
                        cat("No path selected")
                    } else {
                        shinyFiles::parseDirPath(volumes, input$sel_directory)
                    }
                })
                
                shiny::observeEvent(input$config_btn, {
                    shiny::stopApp(shinyFiles::parseDirPath(volumes, input$sel_directory))
                })
                
            }
        )
    )
}
        
    
    