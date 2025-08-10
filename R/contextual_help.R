with_help <- function(tag = NULL, help_item = NULL) {
  # Create help button with namespaced ID
  help_button <- div(
    class = "help-icon",
    style = "
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
            visibility: hidden;
          ",
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
      onmouseover = "this.querySelector('.help-icon').style.visibility = 'visible';",
      onmouseout = "this.querySelector('.help-icon').style.visibility = 'hidden';",
      tag,
      help_button
    ),
    shinyjs::hidden(help_element)
  )
}
# Function to generate help content based on item
help_items <- function(help_item) {
  switch(
    help_item,
    "codebook_csv_import" = list(
      title = "Importing codebooks from CSV",
      content = div(
        "The CSV file file can have columns designating the following:",
        tags$ul(
          tags$li("Code name (required)"),
          tags$li("Code description (optional)"),
          tags$li("Code color (optional)")
        )
      )
    ),
    "agreement_metrics_select" = list(
      title = "Importing codebooks from CSV",
      content = div(
        "The CSV file file can have columns designating the following:",
        tags$ul(
          tags$li("Code name (required)"),
          tags$li("Code description (optional)"),
          tags$li("Code color (optional)")
        )
      )
    )
  )
}
