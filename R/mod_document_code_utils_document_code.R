read_doc_db <- function(pool, active_project) {
  doc_name <- NULL
  if (isTruthy(active_project)) {
    db_docs_df <- dplyr::tbl(pool, "documents") %>%
      dplyr::filter(.data$project_id == as.integer(active_project)) %>%
      dplyr::select(doc_name, doc_id) %>%
      dplyr::collect() %>%
      dplyr::mutate(
        doc_name = ifelse(is.na(doc_name), as.character(doc_id), doc_name)
      )

    named_ids <- db_docs_df %>%
      dplyr::select(dplyr::ends_with("_name")) %>%
      dplyr::pull()
    ids <- db_docs_df %>%
      dplyr::select(dplyr::ends_with("_id")) %>%
      dplyr::pull()

    choices <- ids
    names(choices) <- named_ids

    return(choices)
  } else {
    ""
  }
}

read_visible_docs <- function(pool, active_project, user_id) {
  docs <- dplyr::tbl(pool, "documents") %>%
    dplyr::filter(
      .data$project_id == !!as.integer(active_project),
      .data$user_id == !!as.integer(user_id)
    ) %>%
    dplyr::select(doc_name, doc_id) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      doc_name = ifelse(is.na(doc_name), as.character(doc_id), doc_name)
    )

  stats::setNames(docs$doc_id, docs$doc_name)
}

# Load documents to display -----------------------------

load_doc_db <- function(pool, active_project, doc_id) {
  if (isTruthy(active_project)) {
    doc_text <- dplyr::tbl(pool, "documents") %>%
      dplyr::filter(
        .data$project_id == as.integer(active_project),
        .data$doc_id == as.integer(.env$doc_id)
      ) %>%
      dplyr::pull(doc_text)

    return(doc_text)
  } else {
    ""
  }
}

load_doc <- function(pool, project_id, doc_id) {
  doc_text <- NULL

  dplyr::tbl(pool, "documents") %>%
    dplyr::filter(
      .data$project_id == as.integer(.env$project_id),
      .data$doc_id == as.integer(.env$doc_id)
    ) %>%
    dplyr::pull(doc_text)
}

# Load segments for document display  -------------------------------------------

load_segments_db <- function(pool, active_project, user, doc_selector) {
  code_id <- segment_start <- segment_end <- NULL
  if (isTruthy(active_project)) {
    segments <- dplyr::tbl(pool, "segments") %>%
      dplyr::filter(
        .data$project_id == as.integer(active_project),
        .data$doc_id == as.integer(.env$doc_selector)
      ) %>%
      dplyr::select(
        segment_id,
        code_id,
        segment_start,
        segment_end,
        user_id
      ) %>%
      dplyr::collect()

    if (user$data$annotation_other_view == 0) {
      segments <- segments %>%
        dplyr::filter(user_id == !!user$user_id)
    }

    return(
      segments %>%
        dplyr::select(segment_id, code_id, segment_start, segment_end)
    )
  } else {
    ""
  }
}

# Check overlap between already coded segments and newly coded segment
# using its start and end position
check_overlap <- function(coded_segments, startOff, endOff) {
  overlapping <- segment_start <- segment_end <- NULL
  if (!nrow(coded_segments)) {
    return(data.frame())
  }

  overlapping_segments <- coded_segments %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      overlapping = dplyr::between(segment_start, startOff, endOff) |
        dplyr::between(segment_end, startOff, endOff)
    ) %>%
    dplyr::filter(overlapping)

  if (!nrow(overlapping_segments)) {
    return(data.frame())
  }

  overlapping_segments
}

summarise_new_segment_range <- function(overlap_df, startOff, endOff) {
  code_id <- segment_start <- segment_end <- NULL
  overlap_df %>%
    dplyr::group_by(project_id, doc_id, code_id) %>%
    dplyr::summarise(
      segment_start = min(startOff, segment_start),
      segment_end = max(endOff, segment_end)
    ) %>%
    dplyr::ungroup()
}

get_segment_text <- function(pool, project_id, doc_id, start, end) {
  load_doc(pool, project_id, doc_id) %>%
    substr(start = start, stop = end)
}

# Write document segments to DB -------------------------------------------

write_segment_db <- function(
  pool,
  active_project,
  user_id,
  doc_id,
  code_id,
  startOff,
  endOff
) {
  # Check overlap
  # - return segments with the same document ID and code ID from the DB
  coded_segments <- dplyr::tbl(pool, "segments") %>%
    dplyr::filter(
      .data$project_id == as.integer(active_project),
      .data$user_id == as.integer(.env$user_id),
      .data$doc_id == as.integer(.env$doc_id),
      .data$code_id == as.integer(.env$code_id)
    ) %>%
    dplyr::select(
      project_id,
      user_id,
      doc_id,
      code_id,
      segment_id,
      segment_start,
      segment_end
    ) %>%
    dplyr::collect()
  # - check overlap
  overlap <- check_overlap(coded_segments, startOff, endOff)

  if (nrow(overlap)) {
    # update existing record(s)
    segment_df <- overlap %>%
      summarise_new_segment_range(startOff, endOff) %>%
      dplyr::mutate(
        segment_text = get_segment_text(
          pool,
          project_id = active_project,
          doc_id,
          .data$segment_start,
          .data$segment_end
        ),
        user_id = .env$user_id
      )

    # delete_segments that are to be replaced by segment_df
    query <- glue::glue_sql(
      "DELETE FROM segments
                       WHERE project_id = {active_project}
                       AND doc_id = {doc_id}
                       AND code_id = {code_id}
                       AND user_id = {user_id}
                       AND segment_start = {overlap$segment_start}
                       AND segment_end = {overlap$segment_end}",
      .con = pool
    )

    purrr::walk(query, function(x) {
      DBI::dbExecute(pool, x)
      log_delete_segment_record(
        pool,
        active_project,
        overlap$segment_id,
        user_id
      )
    })

    res <- DBI::dbWriteTable(
      pool,
      "segments",
      segment_df,
      append = TRUE,
      row.names = FALSE
    )
  } else {
    # in case of no overlap write in DB directly
    segment_df <- data.frame(
      project_id = active_project,
      user_id = user_id,
      doc_id = doc_id,
      code_id = code_id,
      segment_start = startOff,
      segment_end = endOff,
      segment_text = get_segment_text(
        pool,
        project_id = active_project,
        doc_id,
        startOff,
        endOff
      )
    )

    res <- DBI::dbWriteTable(
      pool,
      "segments",
      segment_df,
      append = TRUE,
      row.names = FALSE
    )
  }

  if (res) {
    written_segment_id <- dplyr::tbl(pool, "segments") %>%
      dplyr::filter(
        .data$project_id == !!segment_df$project_id,
        .data$doc_id == !!segment_df$doc_id,
        .data$code_id == !!segment_df$code_id,
        .data$segment_start == !!segment_df$segment_start,
        .data$segment_end == !!segment_df$segment_end,
        .data$user_id == !!segment_df$user_id
      ) %>%
      dplyr::pull(segment_id)

    log_add_segment_record(
      pool,
      project_id = active_project,
      segment_df %>%
        dplyr::mutate(segment_id = written_segment_id),
      user_id = user_id
    )
  } else {
    warning("segment not added")
  }
}

write_memo_segment_db <- function(
  pool,
  active_project,
  user_id,
  doc_id,
  code_id,
  startOff,
  endOff
) {
  segment_df <- data.frame(
    project_id = active_project,
    user_id = user_id,
    doc_id = doc_id,
    code_id = code_id,
    segment_start = startOff,
    segment_end = endOff,
    segment_text = get_segment_text(
      pool,
      project_id = active_project,
      doc_id,
      startOff,
      endOff
    )
  )

  res <- DBI::dbWriteTable(
    pool,
    "segments",
    segment_df,
    append = TRUE,
    row.names = FALSE
  )

  if (res) {
    written_segment_id <- dplyr::tbl(pool, "segments") %>%
      dplyr::filter(
        .data$project_id == !!segment_df$project_id,
        .data$doc_id == !!segment_df$doc_id,
        is.na(.data$code_id),
        .data$segment_start == !!segment_df$segment_start,
        .data$segment_end == !!segment_df$segment_end,
        .data$user_id == !!segment_df$user_id
      ) %>%
      dplyr::pull(segment_id)

    #TODO edit for memo purposes
    # log_add_segment_record(pool, project_id = active_project, segment_df %>%
    #                            dplyr::mutate(segment_id = written_segment_id),
    #                        user_id = user_id)
  } else {
    rql_message("Segment memo not added")
  }

  return(max(written_segment_id))
}

# calculate code overlap for doc display ----
calculate_code_overlap <- function(segments, paragraphs) {
  starts <- sort(unique(c(
    segments$segment_start,
    segments$segment_end + 1,
    paragraphs$segment_start
  )))
  ends <- sort(unique(c(
    segments$segment_end,
    segments$segment_start - 1,
    paragraphs$segment_end
  )))
  min_value <- min(paragraphs$segment_start)
  max_value <- max(paragraphs$segment_end)
  starts <- starts[starts >= min_value & starts <= max_value]
  ends <- ends[ends >= min_value & ends <= max_value]

  events_df <- tibble::tibble(
    segment_start = starts,
    segment_end = ends
  ) %>%
    tibble::rownames_to_column("span_id")

  named_starts <- stats::setNames(events_df$segment_start, events_df$span_id)
  named_ends <- stats::setNames(events_df$segment_start, events_df$span_id)

  long_codes <- segments %>%
    dplyr::mutate(
      span_id = purrr::map2(segment_start, segment_end, .f = function(x, y) {
        intersect(
          names(named_starts[named_starts >= x]),
          names(named_ends[named_ends <= y])
        )
      })
    ) %>%
    tidyr::unnest(cols = c(span_id)) %>%
    dplyr::select(-segment_end, -segment_start)

  if (!("memo_id" %in% colnames(long_codes))) {
    long_codes <- long_codes %>%
      dplyr::mutate(memo_id = NA)
  }
  res <- events_df %>%
    dplyr::left_join(long_codes, by = "span_id") %>%
    dplyr::summarise(
      segment_start = unique(segment_start),
      segment_end = unique(segment_end),
      segment_id = format_class_id(segment_id, "segment"),
      code_id = format_class_id(code_id, "code"),
      memo_id = format_class_id(memo_id, "memo"),
      .by = span_id
    ) %>%
    dplyr::mutate(dplyr::across(ends_with("id"), dplyr::na_if, "")) %>%
    dplyr::left_join(
      dplyr::select(paragraphs, par_id, segment_start),
      by = "segment_start"
    ) %>%
    tidyr::fill(par_id, .direction = "down") %>%
    dplyr::mutate(span_id = paste0(par_id, "-span-", span_id))
  return(res)
}

# Generate text to be displayed -----
load_doc_to_display <- function(
  pool,
  active_project,
  user,
  doc_selector,
  raw_text,
  paragraphs,
  codebook,
  highlight
) {
  min_start <- min(paragraphs$segment_start)
  max_end <- max(paragraphs$segment_end)
  segments <- load_segments_db(pool, active_project, user, doc_selector) %>%
    dplyr::filter(segment_start <= max_end, segment_end >= min_start)

  memos_index <- segments$segment_id[is.na(segments$code_id)]
  memos_segments_map <- dplyr::tbl(pool, "memos_segments_map") %>%
    dplyr::filter(segment_id %in% memos_index) %>%
    dplyr::collect()

  if (nrow(memos_segments_map) > 0) {
    segments <- segments %>%
      dplyr::left_join(memos_segments_map, by = "segment_id")
  }

  spans_data <- calculate_code_overlap(segments, paragraphs)
  code_names <- codebook %>%
    dplyr::select(code_id, code_name, code_color) %>%
    dplyr::mutate(code_id = as.character(code_id))

  distinct_code_ids <- spans_data %>%
    dplyr::filter(!is.na(code_id)) %>%
    dplyr::pull(code_id) %>%
    unique()

  code_names_lookup <- tibble::tibble(
    code_id = distinct_code_ids,
    code_name = sapply(distinct_code_ids, blend_codes, code_names),
    code_color = sapply(distinct_code_ids, blend_colors, code_names)
  )
  if (nrow(code_names_lookup) > 0) {
    spans <- spans_data %>%
      dplyr::left_join(code_names_lookup, by = "code_id")
  } else {
    spans <- spans_data %>%
      dplyr::mutate(code_name = NA)
  }
  return(spans)
}

# Generate memos to be displayed -----
load_memos_to_display <- function(pool, active_project, user, doc_selector) {
  # Get info about segments with memos
  memoed_segments <- load_segments_db(
    pool = pool,
    active_project = active_project,
    user = user,
    doc_selector = doc_selector
  ) %>%
    dplyr::filter(is.na(code_id)) %>%
    dplyr::pull(segment_id)
  memo_res <- NULL
  if (length(memoed_segments) > 0) {
    # Map segments to memos
    memo_seg <- dplyr::tbl(pool, "memos_segments_map") %>%
      dplyr::filter(segment_id %in% memoed_segments) %>%
      dplyr::collect()
    # Get info about memo content
    memo_data <- dplyr::tbl(pool, "memos") %>%
      dplyr::filter(memo_id %in% !!unique(memo_seg$memo_id)) %>%
      dplyr::collect()
    # Bind memo details to mapped memos
    memo_res <- memo_seg %>%
      dplyr::left_join(memo_data, by = "memo_id")
  }
  memo_res
}

# Load codes for a segment -------------------------------------------

load_segment_codes_db <- function(
  pool,
  active_project,
  user_id,
  active_doc,
  marked_codes
) {
  segment_codes_df <- dplyr::tbl(pool, "segments") %>%
    dplyr::inner_join(
      dplyr::tbl(pool, "codes") %>%
        dplyr::select(code_id, code_name),
      by = "code_id",
      suffix = c(".x", ".y")
    ) %>%
    dplyr::filter(
      .data$project_id == as.integer(active_project),
      .data$doc_id == as.integer(active_doc)
    ) %>%
    dplyr::filter(dplyr::between(
      marked_codes,
      .data$segment_start,
      .data$segment_end
    )) %>%
    dplyr::collect()

  if (!is.null(user_id)) {
    segment_codes_df <- segment_codes_df %>%
      dplyr::filter(.data$user_id == as.integer(.env$user_id))
  }

  segment_codes_df
}

# Parse tag position -----------

parse_tag_pos <- function(tag_postion, which_part) {
  startOff <- as.integer(unlist(strsplit(tag_postion, "-")))[1]
  endOff <- as.integer(unlist(strsplit(tag_postion, "-")))[2]

  switch(
    which_part,
    "start" = startOff,
    "end" = endOff,
    "both" = c(startOff, endOff)
  )
}


# Remove codes from a document ----------------

delete_segment_codes_db <- function(
  pool,
  active_project,
  user_id,
  doc_id = NULL,
  segment_id
) {
  # delete code from a segment
  if (is.null(doc_id)) {
    query <- glue::glue_sql(
      "DELETE FROM segments
                       WHERE project_id = {active_project}
                       AND segment_id = {segment_id}",
      .con = pool
    )
  } else {
    query <- glue::glue_sql(
      "DELETE FROM segments
                       WHERE project_id = {active_project}
                       AND doc_id = {doc_id}
                       AND segment_id = {segment_id}",
      .con = pool
    )
  }
  purrr::walk(query, function(x) {
    DBI::dbExecute(pool, x)
  })

  log_delete_segment_record(
    pool,
    project_id = active_project,
    segment_id,
    user_id
  )
}


# Generate coding tools -------------------------------------------------

generate_coding_tools <- function(
  ns,
  code_id,
  code_name,
  code_color,
  code_desc
) {
  div(
    actionButton(
      inputId = ns(code_id),
      label = code_name,
      name = code_id,
      class = "code-button",
      title = paste(code_desc),
      style = paste0(
        "background: none;
                       width: 100%;
                       border-left: 5px solid ",
        code_color,
        ";"
      ),
      onclick = paste0(
        "Shiny.setInputValue('",
        ns("selected_code"),
        "', this.name, {priority: 'event'});"
      )
    ),
    actionButton(
      inputId = ns(paste0("more-", code_id)),
      label = "",
      name = code_id,
      title = "Code info",
      icon = icon("ellipsis-vertical"),
      class = "code-menu-extra",
      onclick = paste0(
        "Shiny.setInputValue('",
        ns("selected_code_extra"),
        "', this.name, {priority: 'event'});"
      )
    )
  )
}

generate_coding_tools_tree <- function(
  ns,
  code_id,
  code_name,
  code_color,
  code_desc
) {
  # Create the main button
  main_button <- div(
    div(
      id = paste0("box_wrap_", ns(code_id)),
      class = "box_wrap",
      style = "display: flex;",

      actionButton(
        inputId = ns(code_id),
        label = code_name,
        name = code_id,
        class = "code-button hierarchy",
        title = paste(code_desc),
        style = paste0("--code-color: ", code_color, ";"),
        onclick = paste0(
          "Shiny.setInputValue('",
          ns("selected_code"),
          "', this.name, {priority: 'event'});"
        )
      ),
      actionButton(
        inputId = ns(paste0("more-", code_id)),
        label = "",
        name = code_id,
        title = "Code info",
        icon = icon("ellipsis-vertical"),
        class = "code-menu-extra hierarchy",
        onclick = paste0(
          "Shiny.setInputValue('",
          ns("selected_code_extra"),
          "', this.name, {priority: 'event'});"
        )
      )
    )
  )
}

# Tooltip helper for overlaying codes ----

blend_codes <- function(string_id, code_names_df) {
  ids <- stringr::str_extract_all(string_id, "\\d+", simplify = TRUE)

  names <- code_names_df %>%
    dplyr::filter(code_id %in% ids) %>%
    dplyr::pull(code_name)

  paste(names, collapse = "\n")
}

# Color helper for overlaying codes ----
blend_colors <- function(string_id, code_names_df) {
  ids <- stringr::str_extract_all(string_id, "\\d+", simplify = TRUE)

  colors_multiple <- code_names_df %>%
    dplyr::filter(code_id %in% ids) %>%
    dplyr::pull(code_color)
  color_mean <- colors_multiple %>%
    stringr::str_extract_all("\\d+") %>%
    purrr::map(as.numeric) %>%
    as.data.frame() %>%
    rowMeans() %>%
    round()

  color_mean_string <- paste0(color_mean, collapse = ",")
  paste0("rgb(", color_mean_string, ")")
}


# make span  ----
make_span <- function(
  segment_start,
  segment_end,
  span_id = NULL,
  segment_id = NULL,
  code_id = NULL,
  code_name = NULL,
  code_color = NULL,
  raw_text,
  highlight = NULL,
  memo_id = NULL
) {
  # Extract the text segment and remove newlines

  span_text <- substr(raw_text, segment_start, segment_end)
  span_text <- stringr::str_replace_all(span_text, "\n|\r", "")
  # Check if a code_id is assigned
  code_assigned <- !is.na(code_id)
  memo_assigned <- !is.na(memo_id)

  span_class <- if (code_assigned && memo_assigned) {
    # Both code and memo are assigned
    paste(segment_id, code_id, highlight, memo_id)
  } else if (code_assigned) {
    # Only code is assigned
    paste(segment_id, code_id, highlight)
  } else if (memo_assigned) {
    # Only memo is assigned
    paste(segment_id, memo_id)
  } else {
    # Neither code nor memo is assigned
    NULL
  }
  # Create a span element with attributes and data
  htmltools::span(
    span_text,
    id = span_id,
    title = if (code_assigned) code_name else NULL,
    class = span_class,
    style = if (code_assigned) {
      paste("--code-color:", na.omit(code_color))
    } else {
      NULL
    },
    `data-startend` = paste(segment_start, segment_end),
    .noWS = "outside"
  )
}
