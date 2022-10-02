#' download_html UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_download_html_ui <- function(id){
  ns <- NS(id)
  tagList(
      if(rmarkdown::pandoc_available()){
          downloadButton(ns("report"), "HTML")
      }else{
          list(downloadButton(ns("report"), "HTML", class = "disabled"), 
               HTML("Install <a href='https://bookdown.org/yihui/rmarkdown-cookbook/install-pandoc.html'>pandoc</a> to enable HTML export"))
      }
  )
}
    
#' download_html Server Functions
#'
#' @noRd 
mod_download_html_server <- function(id, glob){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$report <- downloadHandler(
        # https://shiny.rstudio.com/articles/generating-reports.html
        # For PDF output, change this to "report.pdf"
        filename = "report.html",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            
            tempHTML <- file.path(tempdir(), "report_include.html")
            cat(as.character(tagList(glob$segments_taglist)), file = tempHTML)

            tempReport <- file.path(tempdir(), "report.Rmd")
            fileConn<-file(tempReport)
            writeLines(c("---",
                         "title: 'reQual report'",
                         "output: html_document",
                         "---",
                         "```{r, echo=FALSE}",
                         paste0("htmltools::includeHTML('", tempHTML, "')"),
                         "```"
                         ), fileConn)
            close(fileConn)

            # Knit the document and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file, quiet = TRUE,
                              envir = new.env(parent = globalenv())
            )
        }
    )
 
  })
}
    

