create_palette_legend <- function(max_n, palette){
    paste0(c('<p class="legend">', 
             "<h2>Legend</h2>",
             "<ul>",
             paste0(purrr::map_chr(1:max_n, ~paste0(
                 '<li><b id="consensus_',
                 as.character(.x),
                 '" class="segment" style="padding:0; background-color:',
                 palette[.x],
                 dplyr::if_else(.x <= (max_n / 2), "; color:white", ""),
                 '">', 
                 "consensus of ", as.character(.x), 
                 dplyr::if_else(.x == 1, " coder", " coders"), 
                 "</b></li>"
             )), collapse = ""),
             "</ul></p>"), collapse = "")
}