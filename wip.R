pool <- pool::dbPool(
      drv = RPostgreSQL::PostgreSQL(),
      host = "localhost",
      dbname = "requal",
      user = "requal_admin",
      password = "test"
    )  
user_id <- 1
active_project <- 6

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
  segment_id = c(11, 21, 32, 39, 40, 42, 44, 46, 47, 50, 51, 61, 62, 63, 64, 65, 66),
  code_id = c(84, 73, 88, 76, 74, 76, 74, 74, 76, 73, 76, 75, 77, 73, 74, 74, 73),
  segment_start = c(4153, 2676, 1553, 3754, 3767, 1219, 1842, 2170, 2179, 475, 471, 485, 682, 696, 723, 37, 877),
  segment_end = c(4733, 3443, 1850, 3760, 3767, 1223, 1952, 2183, 2191, 476, 471, 689, 713, 726, 742, 373, 1210)
)
raw_segments  <- raw_segments |> 
dplyr::arrange(segment_start)
positions <- sort(c(raw_segments$segment_start, raw_segments$segment_end))

starts <- purrr::map(positions, .f = function(pos) {
   raw_segments |> 
   dplyr::filter(pos >= segment_start & pos < segment_end) |> 
   dplyr::pull(code_id) 
})

ends <- purrr::map(positions, .f = function(pos) {
   raw_segments |> 
   dplyr::filter(pos > segment_start & pos <= segment_end) |> 
   dplyr::pull(code_id)
})

labels_s_pos <- which(purrr::map_lgl(starts, .f = \(x) length(x) > 0))
labels_e_pos <- which(purrr::map_lgl(ends, .f = \(x) length(x) > 0))
labels_matched <- unique(starts[labels_s_pos], ends[labels_e_pos])
labels <- purrr::map_chr(labels_matched, paste0, collapse = "+")
  
labels <- unlist(labels[!purrr::map_lgl(labels, is.null)])
labels_keep <- labels[labels != c(labels[-1], TRUE )]
starts_keep <- positions[purrr::map_lgl(starts, .f = function(x) length(x)>0)]
ends_keep <- positions[purrr::map_lgl(ends, .f = function(x) length(x)>0)]

tibble::tibble(
  code_id = labels_keep,
  segment_start = seg_starts,
  segment_end = seg_ends
)



tags <- starts[-length(starts)]
tags <- starts[-1]

tags[nzchar(tags)]
tibble::tibble(
  code_id = tags[nzchar(tags)],
  seg_starts,
  seg_ends
)
ends[-1]
length(starts[[6]])

ranges <- purrr::map2(raw_segments$segment_start, raw_segments$segment_end, range)
names(ranges) <- purrr::map2(ranges, raw_segments$code_id, stats::setNames)
starts_at_position <- function(ranges, position) {
    if (position == range[1] & position < range[2]) {
      return(names(rang))
    } else {
       NULL
  }
}
purrr::map(positions, .f = function(x){
  browser()
  purrr::map(ranges, ~starts_at_position(.x, x)
  })
ends_at_position <- function(range, position) {
  position > range[1] & position = range[2]
  
}
runs_at_position <- function(range, position) {
  position > range[1] & position < range[2]
}

purrr::map(positions, .f = function(position){
  browser()
  raw_segments$code_id[raw_segments$segment_start >= position & raw_segments$segment_end < position]
  purrr::map(ranges, starts_at_position, position)
})

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

# push
push <- function(x, values) (assign(as.character(substitute(x)), c(x, values), parent.frame()))

# pop
pop <- function(x) (assign(as.character(substitute(x)), x[-length(x)], parent.frame()))

# example
z <- 1:3
push(z, 4)
z
pop(z)
pop(z)
z
push(z, 5)
z
pop(z)
pop(z)
pop(z)
pop(z)
pop(z)
push(z, 1:10)
z

library(shiny)
library(htmltools)
library(dplyr)
doc_selector <- 77
    coded_segments <- dplyr::tbl(pool, "segments") %>%
            dplyr::filter(project_id == 12, 
                          doc_id == 410) %>%
            dplyr::select(segment_id,
                          code_id,
                          segment_start,
                          segment_end, 
                          user_id) %>%
            dplyr::collect() |> 
        calculate_code_overlap()
text <- "Text \n Texts \n Texts \n kjdjs"
text <- load_doc_db(pool, 12, 410)
ptext <- strsplit(text, "[\n\r]")[[1]]
end_index <- (purrr::map_int(ptext, nchar)  |> cumsum()) + seq_along(ptext)
start_index <- c(1, head(end_index, -1) + 1)
pindex <- tibble::tibble(
  pid = seq_along(ptext),
  pid_start = start_index,
  pid_end = end_index
  )
### solve pids for coded segments
segments_pid <- coded_segments |> 
dplyr::mutate(
  pid_i_start = purrr::map_int(segment_start, ~pindex$pid[which(.x >= pindex$pid_start & .x <= pindex$pid_end)]),
  pid_i_end = purrr::map_int(segment_end, ~pindex$pid[which(.x >= pindex$pid_start & .x <= pindex$pid_end)]),
  pid = purrr::map2(pid_i_start, pid_i_end, seq, 1)
  ) |> 
  tidyr::unnest(pid) |> 
  dplyr::left_join(pindex, by = "pid") |> 
  dplyr::select(pid, code_id, segment_start, segment_end, pid_start, pid_end)  |> 
  dplyr::mutate(
    segment_start = ifelse(segment_start < pid_start, pid_start, segment_start),
    segment_end = ifelse(segment_end > pid_end, pid_end, segment_start),
    )

# find pids that must me updated
target_pids <- unique(segments_pid$pid)

update_par_content <- function(target_pid, ptext, segments_pid) {

target_content <- ptext[[target_pid]]
target_segment <- segments_pid |> 
dplyr::filter(pid == target_pid) 

indices <- sort(unique(
  c(target_segment$segment_start, 
  target_segment$segment_end, 
  target_segment$pid_start[1], 
  target_segment$pid_end[1]
  )))
if (length(indices) <= 1) {
  indices <- c(  
    target_segment$pid_start[1], 
    target_segment$pid_end[1]
    )
}
# indices in paragraph context
indices <- (indices-target_segment$pid_start[1])+1
# Indices to a list of ranges
tag_ranges <- purrr::map(1:(length(indices) - 1), ~c(indices[.x], (indices[.x + 1]-1)))
# make sure to get paragraph end
tag_ranges[[length(tag_ranges)]][2] <- max(indices)

updated_par <- purrr::map(tag_ranges, 
            .f = function(tag_range) {
              attributes_df <- target_segment |> 
              dplyr::filter(segment_start == tag_range[1]) 
              if (nrow(attributes_df)) {
              span(class = "highlight", `data-highlight` = attributes_df$code_id, substr(target_content, tag_range[1], tag_range[2]))
              } else {
                 span(class = "no-highlight", substr(target_content, tag_range[1], tag_range[2]))
              }
            }
)
return(tagList(updated_par))

}



for (i in seq_along(target_pids)) {
[[target_pids[i]]] <- update_par_content(target_pids[i], ptext, segments_pid) 
}

article <- tags$article((purrr::map2(ptext, seq_along(ptext), 
~p(id = paste0("pid-", .y), class = "docpar", span(class = "text", .x)) |> 
tagAppendChildren(span("\\&#8203", class = "br"))
)))


query <- tagQuery(article)$
  find("p")
  
query |> div()
query$children[[1]]
tagList(query)
test <- query
test
