xml_file <- xml2::read_xml("/Users/radimhladik/Downloads/refi/Die-Hexen-und-der-Böse-Feind/project.qde")
xml2df <- function(xml_file, xpath, ns = "urn:QDA-XML:project:1.0") {
qda_ns <- c(qda = ns)
xml_file |> 
xml2::xml_find_all(xpath,
ns = qda_ns) |> 
purrr::map_df(xml2::xml_attrs)
}
users <- xml2df(xml_file, "//qda:Project/qda:Users/qda:User")
codes <- xml2df(xml_file, "//qda:Project/qda:CodeBook/qda:Codes/qda:Code")
sources <- xml2df(xml_file, "//qda:Project/qda:Sources/qda:TextSource")

xml2df(xml_file, "//qda:Project")

dplyr::left_join(sources, users, 
by = dplyr::join_by("creatingUser" == "guid"),
suffix = c("_source", "_user")) 


library(purrr)
library(dplyr)

# Create a dataframe
df <- data.frame(
  label = c("A", "B", "C", "D", "E", "F"),
  start = c(1, 3, 3, 7, 2, 11),
  end = c(4, 6, 8, 10, 3, 12)
)

# Create a new dataframe where each row corresponds to a single point in the range
df_points <- data.frame(
  label = rep(df$label, df$end - df$start + 1),
  position = unlist(mapply(FUN = seq, from = df$start, to = df$end))
)

# Group by position and concatenate labels
df_grouped <- df_points %>%
  group_by(position) %>%
  summarise(labels = paste(sort(unique(label)), collapse = "-")) %>%
  arrange(position)

# Create a new dataframe for the result
df_result <- data.frame()

# Loop over each row in the grouped dataframe
for(i in 1:(nrow(df_grouped) - 1)) {
  # If the labels are the same as the next row, extend the end position
  if(df_grouped$labels[i] == df_grouped$labels[i + 1]) {
    df_grouped$position[i + 1] <- df_grouped$position[i]
  }
  # Otherwise, add the row to the result
  else {
    df_result <- rbind(df_result, df_grouped[i, ])
  }
}

# Add the last row to the result
df_result <- rbind(df_result, df_grouped[nrow(df_grouped), ])

# Rename the columns
colnames(df_result) <- c("start", "labels")

# Add the end positions to the result
df_result$end <- c(df_result$start[-1] - 1, df_grouped$position[nrow(df_grouped)])

# Correct the end position of the last row
df_result$end[nrow(df_result)] <- df$end[df$label == df_result$labels[nrow(df_result)]]

# Print the result
print(df_result)
#########
# Assuming raw_segments is your tibble
raw_segments <- tibble::tibble(
  segment_id = c("A", "B", "C", "D", "E", "F", "X", "Y"),
  code_id = c("A", "A", "C", "B", "A", "C", "D", "E"),
  segment_start = c(1, 3, 3, 7, 2, 11, 13, 1),
  segment_end =c(4, 6, 8, 10, 3, 12, 13, 1)
)

# Assuming raw_segments is your tibble
raw_segments <- tibble::tibble(
  segment_id = c("1", "2", "3", "4", "5"),
  code_id = c("A", "B", "A", "C", "E"),
  segment_start = c(1, 2, 10, 15, 3),
  segment_end =c(5, 6, 15, 20, 4)
)

is_within_range <- function(position, range) {
  position >= range[1] & position <= range[2]
}
ranges <- purrr::map2(raw_segments$segment_start, raw_segments$segment_end, range)

positions <- sort(c(raw_segments$segment_start, raw_segments$segment_end))
duplicated_positions <- purrr::map(positions, .f = function(x){
  times <- sum(purrr::map_lgl(ranges, ~is_within_range(x, .x)))
  rep(x, times)

})
updated_positions <- unlist(duplicated_positions)
highligths_df <- tibble::tibble(
segment_start = updated_positions[seq_along(updated_positions) %% 2 == 1],
segment_end = updated_positions[seq_along(updated_positions) %% 2 == 0]
)
highligths_df |> 
dplyr::mutate(segment_id = purrr::map2_chr(segment_start, segment_end, .f = function(x,y){
  raw_segments |> 
 dplyr::filter(segment_end >= y)    |> 
 dplyr::filter(segment_start <= x) |> 
 dplyr::pull(segment_id) |> 
  paste(collapse = "+")
})) |> 
dplyr::mutate(code_id = purrr::map2_chr(segment_start, segment_end, .f = function(x,y){
  raw_segments |> 
 dplyr::filter(segment_end >= y)    |> 
 dplyr::filter(segment_start <= x) |> 
 dplyr::pull(code_id) |> 
  paste(collapse = "+")
}))   |> 
dplyr::arrange(segment_start, segment_end)  |> 
 dplyr::mutate(
 highlight_id = cumsum(segment_id != dplyr::lag(segment_id, default = ""))
)  |> 
dplyr::summarise(
  segment_id = unique(segment_id),
  code_id = unique(code_id),
  segment_start = min(segment_start),
  segment_end = max(segment_end),
  .by = highlight_id
) 


                                  new_ranges <- range(highligths_df$segment_start, highligths_df$segment_end)

split(updated_positions, ceiling(seq_along(updated_positions)/2))
# Create a vector of unique start and end positions
start_positions <- sort(unique(raw_segments$segment_start))
end_positions <- sort(unique(raw_segments$segment_end))
positions <- sort(c(raw_segments$segment_start, raw_segments$segment_end))
names(ranges) <- raw_segments$segment_id
# Function to check if a position is within a range


# Map through positions and get the names of the lists where the position is within range
within_range_s <- purrr::map(start_positions, ~ purrr::map2(.x, ranges, is_within_range))
within_range_e <- purrr::map(end_positions, ~ purrr::map2(.x, ranges, is_within_range))
within_range <- purrr::map(positions, ~ purrr::map2(.x, ranges, is_within_range))

highlight_id_s <- purrr::map_chr(within_range_s, .f = function(x){
  paste0(raw_segments$segment_id[unlist(x)], collapse = "+")
})
highlight_id_e <- purrr::map_chr(within_range_e, .f = function(x){
  paste0(raw_segments$segment_id[unlist(x)], collapse = "+")
})
code_id_s <- purrr::map_chr(within_range_s, .f = function(x){
  paste0(raw_segments$code_id[unlist(x)], collapse = "+")
})
code_id_e <- purrr::map_chr(within_range_e, .f = function(x){
  paste0(raw_segments$code_id[unlist(x)], collapse = "+")
})

starts <- tibble::tibble(
  highlight_id = highlight_id_s,
  code_id = code_id_s,
  segment_start = start_positions,
) 
ends <- tibble::tibble(
  highlight_id = highlight_id_e,
  code_id = code_id_e,
  segment_end = end_positions,
) 

dplyr::bind_rows(
  starts, ends
) |> 
dplyr::arrange(highlight_id)
dplyr::summarise(
  highlight_id = unique(highlight_id),
  code_id = unique(code_id),
  segment_start = min(segment_start),
  segment_end = max(segment_end),
  .by = highlight_id
)  |> 
dplyr::arrange()
dplyr::mutate(segment_start = ifelse(
  segment_start == dplyr::lag(segment_end, default = 0), segment_start + 1, segment_start)
  )

purrr::map_chr(within_range, .f = function(x){
  paste0(raw_segments$segment_start[unlist(x)], collapse = "+")
})
# Get the names of the lists where the position is within range
names_within_range <- purrr::map2(names(within_range), within_range, ~ if(any(.y)) .x)

purrr::accumulate(positions, .f = function(x){
  
})
purrr::map(positions, .f = function(position) {
  ranges[position <= ranges[1] & position >= ranges[2]]
})

raw_segments
raw_segments |> 
dplyr::filter(position <= range)
dplyr::mutate(range = purrr::(segment_start, segment_end, range))
purrr::map(raw_segments$segment_start, raw_segments$segment_end, range)
range(raw_segments$segment_start, raw_segments$segment_end)
# Initialize an empty dataframe for the result
result <- data.frame()

# Initialize the previous labels to an empty string
prev_segment_ids <- ""
prev_code_ids <- ""

# Loop over each position
for(i in positions) {
  # Get the segment_ids and code_ids for the current position
  segment_ids <- raw_segments$segment_id[raw_segments$segment_start <= i & raw_segments$segment_end >= i]
  code_ids <- raw_segments$code_id[raw_segments$segment_start <= i & raw_segments$segment_end >= i]
  
  # Concatenate the segment_ids and code_ids separately
  labels_segment <- paste(sort(segment_ids), collapse = "+")
  labels_code <- paste(sort(unique(code_ids)), collapse = "+")

  # Add the current position to the result
  result <- rbind(result, data.frame(
    segment_id = labels_segment,
    code_id = labels_code,
    segment_start = i,
    segment_end = i
  ))
  
  # Update the previous labels
  prev_segment_ids <- labels_segment
  prev_code_ids <- labels_code
}

result |> 
dplyr::summarise(
    code_id = unique(code_id),
    segment_start = min(segment_start),
    segment_end = max(segment_end),
    .by = segment_id
)
# Print the result
print(result)

# Print the result
print(result)
