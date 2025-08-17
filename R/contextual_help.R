with_help <- function(tag = NULL, help_item = NULL, visible = FALSE) {
  # Create help button
  help_button <- div(
    class = "help-icon",
    style = paste(
      "
            display: inline-block;
            width: 16px;
            height: 16px;
            border-radius: 50%;
            background-color: #007bff;
            color: white;
            text-align: center;
            line-height: 16px;
            font-size: 11px;
            font-weight: bold;
            margin-left: 5px;
            cursor: pointer;
            vertical-align: middle;
            visibility: ",
      if (visible) "visible;" else "hidden;"
    ),
    onclick = paste0(
      "Shiny.setInputValue('show_help', {item: '",
      help_item,
      "'}, {priority: 'event'});"
    ),
    "?"
  )

  help_data <- help_items(help_item)

  help_element <- div(
    id = paste0("help-", help_item),
    style = "
      position: fixed;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      background-color: white;
      border: 1px solid black;
      padding: 10px;
      z-index: 100000;
      min-width: 60vw;
      max-width: 80vw;
      font-size: initial;  
      font-weight: initial;
      color: black;     
      overflow-y: scroll;
      max-height: 80vh;
      border-radius: 5px;
     ",
    div(
      # Div for closing of help content
      style = "text-align: right;",
      span(
        style = "
          cursor: pointer;
          font-weight: bold;
          color: gray;
        ",
        onclick = paste0(
          "Shiny.setInputValue('hide_help', {item: '",
          help_item,
          "'}, {priority: 'event'});"
        ),
        shiny::icon("x")
      )
    ),
    strong(help_data$title),
    br(),
    br(),
    help_data$content
  )

  # Return wrapped content
  div(
    style = "display: inline-block;",
    div(
      onmouseover = if (!visible) {
        "this.querySelector('.help-icon').style.visibility = 'visible';"
      } else {
        NULL
      },
      onmouseout = if (!visible) {
        "this.querySelector('.help-icon').style.visibility = 'hidden';"
      } else {
        NULL
      },
      tag,
      help_button
    ),
    shinyjs::hidden(help_element)
  )
}
# Function to generate help content based on  help item
help_items <- function(help_item) {
  switch(
    help_item,
    "codebook_csv_import" = list(
      title = "Importing Codebooks from CSV",
      content = div(
        p(
          "To import codebooks from a CSV file, the file can include columns for the following content:",
          tags$ul(
            tags$li(
              tags$b("Code name"),
              "(required) This is the unique identifier for each code."
            ),
            tags$li(
              tags$b("Code description"),
              "(optional) A brief explanation or details about the code."
            ),
            tags$li(
              tags$b("Code color"),
              "(optional) The color associated with the code, which can be specified in RGB (e.g., 'rgb(255, 0, 0)') or HEX format (e.g., '#FF0000')."
            )
          )
        ),
        p(
          "Note that the specific column names in the CSV file do not matter, as long as the CSV input is correctly formatted. You can map the column names to the corresponding codebook content in the import wizard.",
        ),
        p(
          "When importing the CSV file, you can specify certain properties to ensure it is processed correctly:",
          tags$ul(
            tags$li(
              tags$b("Header"),
              "Select this option if the first row of the CSV contains column names. This helps in identifying the columns correctly. The default value assumes that the first row of the file contains column names."
            ),
            tags$li(
              tags$b("Separator"),
              "Define the character used to separate values in the file, such as a comma (',') or semicolon (';'). The default value is a comma (',')."
            )
          )
        )
      )
    )
  )
}
