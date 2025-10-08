utils::globalVariables(c("sql", "is_new_quickcode"))

# ============================================================================
# Database Schema Definition (DBML-inspired)
# ============================================================================

schema <- list(
  # Base tables (no dependencies - level auto-calculated)
  projects = list(
    columns = list(
      project_id = list(type = "serial_pk"),
      project_name = list(type = "text"),
      project_description = list(type = "text"),
      created_at = list(type = "timestamp", default = "CURRENT_TIMESTAMP")
    )
  ),

  users = list(
    columns = list(
      user_id = list(type = "integer_pk"),
      user_login = list(type = "text", unique = TRUE),
      user_name = list(type = "text"),
      user_mail = list(type = "text"),
      created_at = list(type = "timestamp", default = "CURRENT_TIMESTAMP")
    )
  ),

  # Tables with dependencies (levels auto-calculated from foreign keys)
  documents = list(
    columns = list(
      doc_id = list(type = "serial_pk"),
      project_id = list(type = "integer"),
      user_id = list(type = "integer"),
      doc_name = list(type = "text"),
      doc_description = list(type = "text"),
      doc_text = list(type = "text"),
      created_at = list(type = "timestamp", default = "CURRENT_TIMESTAMP")
    ),
    foreign_keys = list(
      list(
        column = "project_id",
        ref_table = "projects",
        ref_column = "project_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "user_id",
        ref_table = "users",
        ref_column = "user_id",
        on_delete = "CASCADE"
      )
    )
  ),

  codes = list(
    columns = list(
      code_id = list(type = "serial_pk"),
      project_id = list(type = "integer"),
      user_id = list(type = "integer"),
      code_name = list(type = "text"),
      code_description = list(type = "text"),
      code_color = list(type = "text")
    ),
    foreign_keys = list(
      list(
        column = "project_id",
        ref_table = "projects",
        ref_column = "project_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "user_id",
        ref_table = "users",
        ref_column = "user_id",
        on_delete = "CASCADE"
      )
    )
  ),

  categories = list(
    columns = list(
      category_id = list(type = "serial_pk"),
      project_id = list(type = "integer"),
      user_id = list(type = "integer"),
      category_name = list(type = "text"),
      category_description = list(type = "text")
    ),
    foreign_keys = list(
      list(
        column = "project_id",
        ref_table = "projects",
        ref_column = "project_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "user_id",
        ref_table = "users",
        ref_column = "user_id",
        on_delete = "CASCADE"
      )
    )
  ),

  cases = list(
    columns = list(
      case_id = list(type = "serial_pk"),
      project_id = list(type = "integer"),
      case_name = list(type = "text"),
      case_description = list(type = "text")
    ),
    foreign_keys = list(
      list(
        column = "project_id",
        ref_table = "projects",
        ref_column = "project_id",
        on_delete = "CASCADE"
      )
    )
  ),

  attributes = list(
    columns = list(
      attribute_id = list(type = "serial_pk"),
      attribute_name = list(type = "text"),
      attribute_object = list(type = "text"),
      attribute_type = list(type = "text"),
      project_id = list(type = "integer"),
      user_id = list(type = "integer")
    ),
    foreign_keys = list(
      list(
        column = "user_id",
        ref_table = "users",
        ref_column = "user_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "project_id",
        ref_table = "projects",
        ref_column = "project_id",
        on_delete = "CASCADE"
      )
    )
  ),

  memos = list(
    columns = list(
      memo_id = list(type = "serial_pk"),
      project_id = list(type = "integer"),
      user_id = list(type = "integer"),
      text = list(type = "text")
    ),
    foreign_keys = list(
      list(
        column = "project_id",
        ref_table = "projects",
        ref_column = "project_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "user_id",
        ref_table = "users",
        ref_column = "user_id",
        on_delete = "CASCADE"
      )
    )
  ),

  requal_logs = list(
    columns = list(
      user_id = list(type = "integer"),
      project_id = list(type = "integer"),
      action = list(type = "text"),
      payload = list(type = "json"),
      created_at = list(type = "timestamp", default = "CURRENT_TIMESTAMP")
    ),
    foreign_keys = list(
      list(
        column = "project_id",
        ref_table = "projects",
        ref_column = "project_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "user_id",
        ref_table = "users",
        ref_column = "user_id",
        on_delete = "CASCADE"
      )
    )
  ),

  requal_version = list(
    columns = list(
      project_id = list(type = "integer"),
      version = list(type = "text")
    ),
    foreign_keys = list(
      list(
        column = "project_id",
        ref_table = "projects",
        ref_column = "project_id",
        on_delete = "CASCADE"
      )
    )
  ),

  user_permissions = list(
    columns = list(
      user_id = list(type = "integer"),
      project_id = list(type = "integer"),
      data_modify = list(type = "integer"),
      data_other_modify = list(type = "integer"),
      data_other_view = list(type = "integer"),
      attributes_modify = list(type = "integer"),
      attributes_other_modify = list(type = "integer"),
      attributes_other_view = list(type = "integer"),
      codebook_modify = list(type = "integer"),
      codebook_other_modify = list(type = "integer"),
      codebook_other_view = list(type = "integer"),
      annotation_modify = list(type = "integer"),
      annotation_other_modify = list(type = "integer"),
      annotation_other_view = list(type = "integer"),
      analysis_other_view = list(type = "integer"),
      report_other_view = list(type = "integer"),
      permissions_modify = list(type = "integer"),
      memo_modify = list(type = "integer"),
      memo_other_modify = list(type = "integer"),
      memo_other_view = list(type = "integer"),
      project_owner = list(type = "integer")
    ),
    foreign_keys = list(
      list(
        column = "user_id",
        ref_table = "users",
        ref_column = "user_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "project_id",
        ref_table = "projects",
        ref_column = "project_id",
        on_delete = "CASCADE"
      )
    )
  ),

  # Level 2 tables
  attribute_values = list(
    columns = list(
      attribute_value_id = list(type = "serial_pk"),
      attribute_id = list(type = "integer"),
      value = list(type = "text")
    ),
    foreign_keys = list(
      list(
        column = "attribute_id",
        ref_table = "attributes",
        ref_column = "attribute_id",
        on_delete = "CASCADE"
      )
    )
  ),

  segments = list(
    columns = list(
      segment_id = list(type = "serial_pk"),
      project_id = list(type = "integer"),
      user_id = list(type = "integer"),
      doc_id = list(type = "integer"),
      code_id = list(type = "integer"),
      segment_start = list(type = "integer"),
      segment_end = list(type = "integer"),
      segment_text = list(type = "text")
    ),
    foreign_keys = list(
      list(
        column = "project_id",
        ref_table = "projects",
        ref_column = "project_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "doc_id",
        ref_table = "documents",
        ref_column = "doc_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "code_id",
        ref_table = "codes",
        ref_column = "code_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "user_id",
        ref_table = "users",
        ref_column = "user_id",
        on_delete = "CASCADE"
      )
    )
  ),

  categories_codes_map = list(
    columns = list(
      project_id = list(type = "integer"),
      category_id = list(type = "integer"),
      code_id = list(type = "integer")
    ),
    foreign_keys = list(
      list(
        column = "project_id",
        ref_table = "projects",
        ref_column = "project_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "category_id",
        ref_table = "categories",
        ref_column = "category_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "code_id",
        ref_table = "codes",
        ref_column = "code_id",
        on_delete = "CASCADE"
      )
    )
  ),

  cases_documents_map = list(
    columns = list(
      project_id = list(type = "integer"),
      case_id = list(type = "integer"),
      doc_id = list(type = "integer")
    ),
    foreign_keys = list(
      list(
        column = "project_id",
        ref_table = "projects",
        ref_column = "project_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "case_id",
        ref_table = "cases",
        ref_column = "case_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "doc_id",
        ref_table = "documents",
        ref_column = "doc_id",
        on_delete = "CASCADE"
      )
    )
  ),

  memos_codes_map = list(
    columns = list(
      memo_id = list(type = "integer"),
      code_id = list(type = "integer")
    ),
    foreign_keys = list(
      list(
        column = "code_id",
        ref_table = "codes",
        ref_column = "code_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "memo_id",
        ref_table = "memos",
        ref_column = "memo_id",
        on_delete = "CASCADE"
      )
    )
  ),

  memos_documents_map = list(
    columns = list(
      memo_id = list(type = "integer"),
      doc_id = list(type = "integer"),
      memo_start = list(type = "integer"),
      memo_end = list(type = "integer")
    ),
    foreign_keys = list(
      list(
        column = "doc_id",
        ref_table = "documents",
        ref_column = "doc_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "memo_id",
        ref_table = "memos",
        ref_column = "memo_id",
        on_delete = "CASCADE"
      )
    )
  ),

  memos_segments_map = list(
    columns = list(
      memo_id = list(type = "integer"),
      segment_id = list(type = "integer")
    ),
    foreign_keys = list(
      list(
        column = "segment_id",
        ref_table = "segments",
        ref_column = "segment_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "memo_id",
        ref_table = "memos",
        ref_column = "memo_id",
        on_delete = "CASCADE"
      )
    )
  ),

  attributes_users_map = list(
    columns = list(
      user_id = list(type = "integer"),
      attribute_id = list(type = "integer"),
      attribute_value_id = list(type = "integer"),
      project_id = list(type = "integer")
    ),
    foreign_keys = list(
      list(
        column = "user_id",
        ref_table = "users",
        ref_column = "user_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "attribute_id",
        ref_table = "attributes",
        ref_column = "attribute_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "attribute_value_id",
        ref_table = "attribute_values",
        ref_column = "attribute_value_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "project_id",
        ref_table = "projects",
        ref_column = "project_id",
        on_delete = "CASCADE"
      )
    )
  ),

  attributes_documents_map = list(
    columns = list(
      doc_id = list(type = "integer"),
      attribute_id = list(type = "integer"),
      attribute_value_id = list(type = "integer"),
      project_id = list(type = "integer")
    ),
    foreign_keys = list(
      list(
        column = "doc_id",
        ref_table = "documents",
        ref_column = "doc_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "attribute_id",
        ref_table = "attributes",
        ref_column = "attribute_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "attribute_value_id",
        ref_table = "attribute_values",
        ref_column = "attribute_value_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "project_id",
        ref_table = "projects",
        ref_column = "project_id",
        on_delete = "CASCADE"
      )
    )
  ),

  attributes_cases_map = list(
    columns = list(
      case_id = list(type = "integer"),
      attribute_id = list(type = "integer"),
      attribute_value_id = list(type = "integer"),
      project_id = list(type = "integer")
    ),
    foreign_keys = list(
      list(
        column = "case_id",
        ref_table = "cases",
        ref_column = "case_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "attribute_id",
        ref_table = "attributes",
        ref_column = "attribute_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "attribute_value_id",
        ref_table = "attribute_values",
        ref_column = "attribute_value_id",
        on_delete = "CASCADE"
      ),
      list(
        column = "project_id",
        ref_table = "projects",
        ref_column = "project_id",
        on_delete = "CASCADE"
      )
    )
  )
)

# ============================================================================
# Type Mappings (DBML-inspired)
# ============================================================================

type_map <- list(
  sqlite = list(
    serial_pk = "INTEGER PRIMARY KEY AUTOINCREMENT",
    integer_pk = "INTEGER PRIMARY KEY",
    integer = "INTEGER",
    text = "TEXT",
    timestamp = "TEXT",
    json = "TEXT"
  ),
  postgres = list(
    serial_pk = "SERIAL PRIMARY KEY",
    integer_pk = "INTEGER PRIMARY KEY",
    integer = "INTEGER",
    text = "TEXT",
    timestamp = "TIMESTAMP",
    json = "JSON" # Throws warnings
  )
)

# ============================================================================
# Core Functions
# ============================================================================

# Auto-calculate dependency levels from foreign keys
calculate_dependency_levels <- function(schema) {
  all_tables <- names(schema)
  levels <- setNames(rep(NA_integer_, length(all_tables)), all_tables)

  # Iteratively assign levels
  max_iterations <- length(all_tables) + 1
  for (iteration in 1:max_iterations) {
    changed <- FALSE

    for (tbl in all_tables) {
      if (!is.na(levels[tbl])) {
        next
      }

      fks <- schema[[tbl]]$foreign_keys

      # No foreign keys = level 0
      if (is.null(fks) || length(fks) == 0) {
        levels[tbl] <- 0
        changed <- TRUE
        next
      }

      # Get all referenced tables
      ref_tables <- purrr::map_chr(fks, ~ .x$ref_table)
      ref_levels <- levels[ref_tables]

      # Can only assign level if all dependencies are resolved
      if (all(!is.na(ref_levels))) {
        levels[tbl] <- max(ref_levels) + 1
        changed <- TRUE
      }
    }

    if (!changed) break
  }

  # Check for unresolved (circular dependencies or missing tables)
  if (any(is.na(levels))) {
    unresolved <- names(levels)[is.na(levels)]
    stop(
      "Cannot resolve dependency order for tables: ",
      paste(unresolved, collapse = ", ")
    )
  }

  levels
}

# Get tables in correct dependency order
get_ordered_tables <- function(schema) {
  levels <- calculate_dependency_levels(schema)

  tibble::tibble(
    table = names(levels),
    level = levels
  ) %>%
    dplyr::arrange(level, table) %>%
    dplyr::pull(table)
}

# Build column definition SQL
build_column_sql <- function(col_name, col_def, db_type, type_map) {
  base_type <- type_map[[db_type]][[col_def$type]]

  parts <- c(col_name, base_type)

  # Add constraints
  if (!is.null(col_def$unique) && col_def$unique) {
    parts <- c(parts, "UNIQUE")
  }

  if (!is.null(col_def$default)) {
    parts <- c(parts, paste0("DEFAULT ", col_def$default))
  }

  paste(parts, collapse = " ")
}

# Build foreign key constraint SQL
build_fk_sql <- function(fk) {
  sprintf(
    "FOREIGN KEY(%s) REFERENCES %s(%s) ON DELETE %s",
    fk$column,
    fk$ref_table,
    fk$ref_column,
    fk$on_delete
  )
}

# Build CREATE TABLE statement
build_create_table_sql <- function(table_name, table_def, db_type, type_map) {
  # Build column definitions
  col_sqls <- purrr::imap_chr(
    table_def$columns,
    ~ build_column_sql(.y, .x, db_type, type_map)
  )

  # Build foreign key constraints
  fk_sqls <- if (!is.null(table_def$foreign_keys)) {
    purrr::map_chr(table_def$foreign_keys, build_fk_sql)
  } else {
    character(0)
  }

  # Combine all SQL parts
  all_parts <- c(col_sqls, fk_sqls)

  sprintf(
    "CREATE TABLE IF NOT EXISTS %s (\n    %s\n);",
    table_name,
    paste(all_parts, collapse = "\n,   ")
  )
}

# Detect database type from pool
get_db_type <- function(pool) {
  if (pool::dbGetInfo(pool)$pooledObjectClass == "SQLiteConnection") {
    "sqlite"
  } else {
    "postgres"
  }
}

# Get existing columns for a table
get_existing_columns <- function(pool, table_name) {
  tryCatch(
    {
      # Use dplyr::tbl to get table structure
      result <- dplyr::tbl(pool, table_name) %>%
        utils::head(0) %>%
        dplyr::collect()
      names(result)
    },
    error = function(e) {
      character(0) # Table doesn't exist
    }
  )
}

# Add a column to existing table
add_column_sql <- function(table_name, col_name, col_def, db_type, type_map) {
  col_sql <- build_column_sql(col_name, col_def, db_type, type_map)
  sprintf("ALTER TABLE %s ADD COLUMN %s;", table_name, col_sql)
}

# ============================================================================
# Main Functions
# ============================================================================

# Create entire database schema
create_db_schema <- function(pool) {
  db_type <- get_db_type(pool)
  ordered_tables <- get_ordered_tables(schema)

  purrr::walk(ordered_tables, function(tbl) {
    sql <- build_create_table_sql(tbl, schema[[tbl]], db_type, type_map)
    DBI::dbExecute(pool, sql)
  })

  message("Created database schema with ", length(ordered_tables), " tables.")
}

# Update database schema (add missing tables and columns)
update_db_schema <- function(pool) {
  db_type <- get_db_type(pool)
  ordered_tables <- get_ordered_tables(schema)
  existing_tables <- pool::dbListTables(pool)
  existing_tables <- existing_tables[!grepl("sqlite", existing_tables)]

  tables_created <- 0
  columns_added <- 0

  purrr::walk(ordered_tables, function(tbl) {
    if (!tbl %in% existing_tables) {
      sql <- build_create_table_sql(tbl, schema[[tbl]], db_type, type_map)
      DBI::dbExecute(pool, sql)
      tables_created <<- tables_created + 1
    } else {
      existing_cols <- get_existing_columns(pool, tbl)
      schema_cols <- names(schema[[tbl]]$columns)
      missing_cols <- setdiff(schema_cols, existing_cols)

      if (length(missing_cols) > 0) {
        purrr::walk(missing_cols, function(col) {
          sql <- add_column_sql(
            tbl,
            col,
            schema[[tbl]]$columns[[col]],
            db_type,
            type_map
          )
          DBI::dbExecute(pool, sql)
          columns_added <<- columns_added + 1
        })
      }
    }
  })

  if (tables_created > 0 || columns_added > 0) {
    message(sprintf(
      "Updated schema: %d tables created, %d columns added.",
      tables_created,
      columns_added
    ))
  } else {
    message("Schema is up to date.")
  }
}


# Database functions -----------------

create_default_user <- function(pool, project_id, user_id) {
  default_user_permission_df <- tibble::tibble(
    data_modify = 1,
    data_other_modify = 1,
    data_other_view = 1,
    attributes_modify = 1,
    attributes_other_modify = 1,
    attributes_other_view = 1,
    codebook_modify = 1,
    codebook_other_modify = 1,
    codebook_other_view = 1,
    annotation_modify = 1,
    annotation_other_modify = 1,
    annotation_other_view = 1,
    analysis_other_view = 1,
    report_other_view = 1,
    permissions_modify = 1,
    memo_modify = 1,
    memo_other_modify = 1,
    memo_other_view = 1,
    project_owner = 1
  )

  if (golem::get_golem_options("mode") %in% c("local", "local_test")) {
    user_df <- tibble::tibble(
      user_name = Sys.info()["user"]
    )
    DBI::dbWriteTable(pool, "users", user_df, append = TRUE, row.names = FALSE)

    user_df_stored <- dplyr::tbl(pool, "users") %>%
      dplyr::filter(.data$user_name == !!user_df$user_name) %>%
      dplyr::collect()
    user_permission_df <- dplyr::bind_cols(
      tibble::tibble(
        user_id = user_df_stored$user_id,
        project_id = project_id
      ),
      default_user_permission_df
    )
  } else {
    user_permission_df <- dplyr::bind_cols(
      tibble::tibble(
        user_id = user_id,
        project_id = project_id
      ),
      default_user_permission_df
    )
  }

  DBI::dbWriteTable(
    pool,
    "user_permissions",
    user_permission_df,
    append = TRUE,
    row.names = FALSE
  )
}

create_project_record <- function(pool, project_df, user_id) {
  res <- pool::dbWriteTable(
    pool,
    "projects",
    project_df,
    append = TRUE,
    row.names = FALSE
  )

  project_id <- dplyr::tbl(pool, "projects") %>%
    dplyr::filter(project_name == !!project_df$project_name) %>%
    dplyr::pull(project_id)

  # to delete later after we check for unique project names
  project_id <- max(project_id)

  if (res) {
    create_default_user(pool, project_id, user_id = user_id)

    log_create_project_record(pool, project_id, project_df, user_id)
  }

  requal_version_df <- data.frame(
    project_id = project_id,
    version = as.character(packageVersion("requal"))
  )
  res_v <- DBI::dbWriteTable(
    pool,
    "requal_version",
    requal_version_df,
    append = TRUE,
    row.names = FALSE
  )
}

add_documents_record <- function(pool, project_id, document_df, user_id) {
  res <- DBI::dbWriteTable(
    pool,
    "documents",
    document_df,
    append = TRUE,
    row.names = FALSE
  )
  if (res) {
    project_id <- as.integer(project_id)
    written_document_id <- dplyr::tbl(pool, "documents") %>%
      dplyr::filter(
        .data$doc_name == !!document_df$doc_name,
        .data$doc_text == !!document_df$doc_text,
        .data$project_id == !!as.numeric(project_id),
        .data$user_id == !!user_id
      ) %>%
      dplyr::pull(doc_id)

    written_document_id <- written_document_id[
      written_document_id == max(written_document_id)
    ]
    log_add_document_record(
      pool,
      project_id,
      document_df %>%
        dplyr::mutate(
          doc_id = written_document_id,
          doc_text = substr(doc_text, 1, 140)
        ),
      user_id = user_id
    )
  } else {
    warning("document not added")
  }
}

add_cases_record <- function(pool, project_id, case_df, user_id) {
  res <- DBI::dbWriteTable(
    pool,
    "cases",
    case_df,
    append = TRUE,
    row.names = FALSE
  )
  if (res) {
    written_case_id <- dplyr::tbl(pool, "cases") %>%
      dplyr::filter(
        .data$case_name == !!case_df$case_name,
        .data$project_id == !!as.numeric(project_id)
      ) %>%
      dplyr::pull(.data$case_id)

    written_case_id <- written_case_id[written_case_id == max(written_case_id)]
    log_add_case_record(
      pool,
      project_id,
      case_df %>%
        dplyr::mutate(case_id = written_case_id),
      user_id = user_id
    )
  } else {
    warning("case not added")
  }
}

add_codes_record <- function(pool, project_id, codes_df, user_id) {
  res <- DBI::dbWriteTable(
    pool,
    "codes",
    codes_df,
    append = TRUE,
    row.names = FALSE
  )
  if (res) {
    written_code_id <- dplyr::tbl(pool, "codes") %>%
      dplyr::filter(
        .data$code_name == !!codes_df$code_name,
        .data$project_id == !!as.integer(project_id),
        .data$user_id == !!user_id
      ) %>%
      dplyr::pull(code_id)

    written_code_id <- written_code_id[written_code_id == max(written_code_id)]
    log_add_code_record(
      pool,
      project_id,
      codes_df %>%
        dplyr::mutate(
          code_id = written_code_id
        ),
      user_id
    )
  } else {
    warning("code not added")
  }
}

add_quickcode_record <- function(pool, project_id, codes_df, user_id) {
  # Make sure column exists to identify new quickcode
  db_helper_column(pool, "codes", "is_new_quickcode", "add")
  # temporarily write into DB with original code_id
  codes_df$is_new_quickcode <- 1

  res <- DBI::dbWriteTable(
    pool,
    "codes",
    codes_df,
    append = TRUE,
    row.names = FALSE
  )
  if (res) {
    written_code_id <- dplyr::tbl(pool, "codes") %>%
      dplyr::filter(
        .data$project_id == !!as.integer(project_id),
        .data$user_id == !!as.integer(user_id),
        is_new_quickcode == 1
      ) %>%
      dplyr::pull(code_id)
    # remove helper column from DB
    db_helper_column(pool, "codes", "is_new_quickcode", "drop")
    # just a check we are getting the latest id
    written_code_id <- written_code_id[written_code_id == max(written_code_id)]
    log_add_code_record(
      pool,
      project_id,
      codes_df %>%
        dplyr::mutate(
          is_new_quickcode = NULL,
          code_id = written_code_id
        ),
      user_id
    )
    return(written_code_id)
  } else {
    warning("code not added")
  }
}

add_case_doc_record <- function(pool, project_id, case_doc_df, user_id) {
  res <- DBI::dbWriteTable(
    pool,
    "cases_documents_map",
    case_doc_df,
    append = TRUE,
    row.names = FALSE
  )
  if (res) {
    log_add_case_doc_record(pool, project_id, case_doc_df, user_id)
  } else {
    warning("code document map not added")
  }
}

# Globals ####

make_globals <- quote({
  if (golem::get_golem_options(which = "mode") == "server") {
    pool <- pool::dbPool(
      drv = RPostgreSQL::PostgreSQL(),
      host = golem::get_golem_options(which = "dbhost"),
      port = golem::get_golem_options(which = "dbport"),
      dbname = golem::get_golem_options(which = "dbname"),
      user = golem::get_golem_options(which = "dbusername"),
      password = golem::get_golem_options(which = "dbpassword")
    )

    onStop(function() {
      print("closing pool")
      pool::poolClose(pool)
    })

    if ("projects" %in% pool::dbListTables(pool)) {
      existing_projects <- dplyr::pull(dplyr::tbl(pool, "projects"), project_id)
      names(existing_projects) <- dplyr::pull(
        dplyr::tbl(pool, "projects"),
        project_name
      )
    } else {
      existing_projects <- data.frame()
    }
  } else if (golem::get_golem_options(which = "mode") == "local_test") {
    pool <- pool::dbPool(
      drv = RSQLite::SQLite(),
      dbname = golem::get_golem_options(which = "dbname")
    )

    onStop(function() {
      print("closing pool")
      pool::poolClose(pool)
    })

    if ("projects" %in% pool::dbListTables(pool)) {
      existing_projects <- dplyr::pull(dplyr::tbl(pool, "projects"), project_id)
      names(existing_projects) <- dplyr::pull(
        dplyr::tbl(pool, "projects"),
        project_name
      )
    } else {
      existing_projects <- data.frame()
    }
  }
})
