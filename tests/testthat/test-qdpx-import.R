# Helper to create a minimal valid QDPX archive for testing
create_test_qdpx <- function(base_name = "test_project") {
  # Create temp directory structure
  tmp_dir <- tempfile("qdpx_test")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)

  # Write project.qde with proper GUID format
  project_xml <- '<?xml version="1.0" encoding="utf-8"?>
<Project xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="urn:QDA-XML:project:1.0 refi.xsd"
         name="Test Project"
         origin="RE-QDA"
         createdDateTime="2024-01-01T10:00:00"
         modifiedDateTime="2024-01-02T12:00:00"
         xmlns="urn:QDA-XML:project:1.0">
  <Description>A test project for QDPX import.</Description>
  <Users>
    <User guid="a1b2c3d4-e5f6-7890-abcd-ef1234567890" id="admin" name="Admin User"/>
  </Users>
  <CodeBook>
    <Codes>
      <Code guid="code-001-aaaa-bbbb-cccc-ddddeeeeffff" name="Theme A" color="#FFFF00" isCodable="true">
        <Description>First theme</Description>
      </Code>
      <Code guid="code-002-aaaa-bbbb-cccc-ddddeeeeffff" name="Theme B" color="#00FF00" isCodable="true">
        <Description>Second theme</Description>
      </Code>
    </Codes>
    <Sets/>
  </CodeBook>
  <Sources>
    <TextSource guid="source-001-aaaa-bbbb-cccc-ddddeeeeffff" name="Document 1" plainTextPath="doc1.txt">
      <Description>First document</Description>
      <PlainTextContent>This is the content of the first document. It contains some text that will be coded.</PlainTextContent>
      <PlainTextSelection guid="sel-001-aaaa-bbbb-cccc-ddddeeeeffff" startPosition="0" endPosition="31">
        <Coding guid="coding-001-aaaa-bbbb-cccc-ddddeeeeffff" creatingUser="a1b2c3d4-e5f6-7890-abcd-ef1234567890">
          <CodeRef targetGUID="code-001-aaaa-bbbb-cccc-ddddeeeeffff"/>
        </Coding>
      </PlainTextSelection>
      <PlainTextSelection guid="sel-002-aaaa-bbbb-cccc-ddddeeeeffff" startPosition="32" endPosition="55">
        <Coding guid="coding-002-aaaa-bbbb-cccc-ddddeeeeffff" creatingUser="a1b2c3d4-e5f6-7890-abcd-ef1234567890">
          <CodeRef targetGUID="code-002-aaaa-bbbb-cccc-ddddeeeeffff"/>
        </Coding>
      </PlainTextSelection>
      <!-- Memo segment: selection with only NoteRef, no Coding -->
      <PlainTextSelection guid="sel-003-aaaa-bbbb-cccc-ddddeeeeffff" startPosition="65" endPosition="83">
        <NoteRef targetGUID="note-001-aaaa-bbbb-cccc-ddddeeeeffff"/>
      </PlainTextSelection>
    </TextSource>
  </Sources>
  <Notes>
    <Note guid="note-001-aaaa-bbbb-cccc-ddddeeeeffff" creatingUser="a1b2c3d4-e5f6-7890-abcd-ef1234567890">
      <PlainTextContent>This is a test note.</PlainTextContent>
    </Note>
    <Note guid="note-002-aaaa-bbbb-cccc-ddddeeeeffff" creatingUser="a1b2c3d4-e5f6-7890-abcd-ef1234567890">
      <PlainTextContent>Another test note with more content.</PlainTextContent>
    </Note>
  </Notes>
  <Sets>
    <Set guid="set-001-aaaa-bbbb-cccc-ddddeeeeffff" name="Test Category">
      <Description>A test category for grouping codes</Description>
      <MemberCode targetGUID="code-001-aaaa-bbbb-cccc-ddddeeeeffff"/>
      <MemberCode targetGUID="code-002-aaaa-bbbb-cccc-ddddeeeeffff"/>
    </Set>
  </Sets>
  <Links/>
</Project>'
  writeLines(project_xml, file.path(tmp_dir, "project.qde"))

  # Write source document
  writeLines(
    "This is the content of the first document. It contains some text that will be coded.",
    file.path(tmp_dir, "doc1.txt")
  )

  # Zip it up as .qdpx
  qdpx_path <- tempfile(base_name, fileext = ".qdpx")
  utils::zip(qdpx_path, files = list.files(tmp_dir, recursive = TRUE, full.names = TRUE),
             extras = "-j")
  unlink(tmp_dir, recursive = TRUE)

  return(qdpx_path)
}

valid_qdpx_path <- create_test_qdpx()

test_that("parse_qdpx returns expected structure", {
  result <- parse_qdpx(valid_qdpx_path)

  expect_type(result, "list")
  expect_named(result, c("project", "users", "codebook", "sources", "selections", "notes", "sets"))

  # Check project
  expect_s3_class(result$project, "data.frame")
  expect_equal(nrow(result$project), 1)
  expect_equal(result$project$project_name, "Test Project")

  # Check users
  expect_s3_class(result$users, "data.frame")
  expect_equal(nrow(result$users), 1)

  # Check codebook
  expect_s3_class(result$codebook, "data.frame")
  expect_equal(nrow(result$codebook), 2)
  expect_true("code_name" %in% names(result$codebook))
  expect_true("guid" %in% names(result$codebook))

  # Check sources
  expect_s3_class(result$sources, "data.frame")
  expect_equal(nrow(result$sources), 1)
  expect_true("source_guid" %in% names(result$sources))

  # Check selections
  expect_s3_class(result$selections, "data.frame")
  # Now includes 2 coded segments + 1 memo segment (selection with only NoteRef)
  expect_equal(nrow(result$selections), 3)
  expect_true(all(c("source_guid", "code_guid", "note_guid", "startPosition", "endPosition") %in% names(result$selections)))

  # Check that memo segment has note_guid but no code_guid
  memo_sel <- result$selections[is.na(result$selections$code_guid), ]
  expect_equal(nrow(memo_sel), 1)
  expect_equal(memo_sel$note_guid[1], "note-001-aaaa-bbbb-cccc-ddddeeeeffff")

  # Check notes
  expect_s3_class(result$notes, "data.frame")
  expect_true(all(c("note_guid", "creating_user", "note_name", "text") %in% names(result$notes)))

  # Check sets (categories)
  expect_s3_class(result$sets, "data.frame")
  expect_true(all(c("set_guid", "set_name", "set_description", "member_code_guids") %in% names(result$sets)))
})

test_that("segment offsets are correctly converted on import", {
  # Create an in-memory SQLite database for testing
  test_pool <- pool::dbPool(drv = RSQLite::SQLite(), dbname = ":memory:")
  DBI::dbExecute(test_pool, "PRAGMA foreign_keys = ON;")
  create_db_schema(test_pool)

  # Create project (bypass golem mode check by calling lower-level function)
  project_df <- tibble::tibble(
    project_name = "Test Import",
    project_description = "Testing segment offsets",
    created_at = as.character(Sys.time())
  )
  DBI::dbWriteTable(test_pool, "projects", project_df, append = TRUE, row.names = FALSE)
  active_project <- dplyr::tbl(test_pool, "projects") %>%
    dplyr::filter(project_name == "Test Import") %>%
    dplyr::pull(project_id)

  # Create default user and permissions
  user_df <- tibble::tibble(
    user_id = 1L,
    user_login = "test_user",
    user_name = "Test User"
  )
  DBI::dbWriteTable(test_pool, "users", user_df, append = TRUE, row.names = FALSE)

  # Re-parse to get fresh result (since we're in a new test context)
  result <- parse_qdpx(valid_qdpx_path)

  # Import codebook and get mapping
  code_map <- .import_codebook(
    codebook = result$codebook,
    user_id = 1L,
    active_project = active_project,
    pool = test_pool
  )

  # Import sources and get mapping
  doc_map <- .import_sources(
    sources = result$sources,
    user_id = 1L,
    active_project = active_project,
    pool = test_pool
  )

  # Import segments
  .import_segments(
    selections = result$selections,
    code_map = code_map,
    doc_map = doc_map,
    user_id = 1L,
    active_project = active_project,
    pool = test_pool
  )

  # Verify segments were imported with correct offsets
  # Now includes 2 coded segments + 1 memo segment
  segments <- dplyr::tbl(test_pool, "segments") %>%
    dplyr::filter(project_id == !!active_project) %>%
    dplyr::collect()

  expect_equal(nrow(segments), 3)

  # First selection: startPosition=0, endPosition=31
  # Should become: segment_start = 0 + 1 = 1, segment_end = 31
  seg1 <- segments %>%
    dplyr::filter(segment_start == 1, segment_end == 31)
  expect_equal(nrow(seg1), 1)

  # Second selection: startPosition=32, endPosition=55
  # Should become: segment_start = 32 + 1 = 33, segment_end = 55
  seg2 <- segments %>%
    dplyr::filter(segment_start == 33, segment_end == 55)
  expect_equal(nrow(seg2), 1)

  pool::poolClose(test_pool)
})

test_that("memo segments (selections with only NoteRef) are imported correctly", {
  # Create an in-memory SQLite database for testing
  test_pool <- pool::dbPool(drv = RSQLite::SQLite(), dbname = ":memory:")
  DBI::dbExecute(test_pool, "PRAGMA foreign_keys = ON;")
  create_db_schema(test_pool)

  # Create project
  project_df <- tibble::tibble(
    project_name = "Test Memo Segments",
    project_description = "Testing memo segment import",
    created_at = as.character(Sys.time())
  )
  DBI::dbWriteTable(test_pool, "projects", project_df, append = TRUE, row.names = FALSE)
  active_project <- dplyr::tbl(test_pool, "projects") %>%
    dplyr::filter(project_name == "Test Memo Segments") %>%
    dplyr::pull(project_id)

  # Create user
  user_df <- tibble::tibble(
    user_id = 1L,
    user_login = "test_user",
    user_name = "Test User"
  )
  DBI::dbWriteTable(test_pool, "users", user_df, append = TRUE, row.names = FALSE)

  # Re-parse to get fresh result
  result <- parse_qdpx(valid_qdpx_path)

  # Import codebook
  code_map <- .import_codebook(
    codebook = result$codebook,
    user_id = 1L,
    active_project = active_project,
    pool = test_pool
  )

  # Import sources
  doc_map <- .import_sources(
    sources = result$sources,
    user_id = 1L,
    active_project = active_project,
    pool = test_pool
  )

  # Import segments (including memo segments)
  segment_result <- .import_segments(
    selections = result$selections,
    code_map = code_map,
    doc_map = doc_map,
    user_id = 1L,
    active_project = active_project,
    pool = test_pool
  )

  # Verify coded segments were imported
  coded_segments <- dplyr::tbl(test_pool, "segments") %>%
    dplyr::filter(project_id == !!active_project, !is.na(code_id)) %>%
    dplyr::collect()
  expect_equal(nrow(coded_segments), 2)

  # Verify memo segment was imported (code_id IS NULL)
  memo_segments <- dplyr::tbl(test_pool, "segments") %>%
    dplyr::filter(project_id == !!active_project, is.na(code_id)) %>%
    dplyr::collect()
  expect_equal(nrow(memo_segments), 1)

  # Verify memo segment has correct position
  expect_equal(nrow(memo_segments), 1)
  expect_equal(memo_segments$segment_start[1], 66)  # startPosition 65 + 1
  expect_equal(memo_segments$segment_end[1], 83)
  expect_equal(memo_segments$segment_text[1], "that will be coded")

  # Import notes (with memo_mappings to create memos_segments_map)
  .import_notes(
    notes = result$notes,
    user_id = 1L,
    active_project = active_project,
    pool = test_pool,
    memo_mappings = segment_result$memo_mappings
  )

  # Verify memos_segments_map entry was created
  memo_segments_map <- dplyr::tbl(test_pool, "memos_segments_map") %>%
    dplyr::collect()
  expect_equal(nrow(memo_segments_map), 1)

  # Verify the mapping is correct
  expect_true(memo_segments_map$memo_id[1] > 0)
  expect_true(memo_segments_map$segment_id[1] > 0)

  pool::poolClose(test_pool)
})

test_that("corrupted QDPX file returns error message", {
  # Create a corrupted zip (not valid XML)
  tmp_corrupt <- tempfile("corrupt")
  dir.create(tmp_corrupt, showWarnings = FALSE)
  writeLines("not valid xml", file.path(tmp_corrupt, "project.qde"))
  corrupted_path <- tempfile("corrupted", fileext = ".qdpx")
  utils::zip(corrupted_path, files = list.files(tmp_corrupt, recursive = TRUE, full.names = TRUE),
             extras = "-j")
  unlink(tmp_corrupt, recursive = TRUE)

  # Parse should return empty results with an error message
  result <- tryCatch(
    parse_qdpx(corrupted_path),
    error = function(e) {
      list(codebook = data.frame(), sources = data.frame(), message = e$message)
    }
  )

  expect_true(!is.null(result$message))
  expect_equal(nrow(result$codebook), 0)
  expect_equal(nrow(result$sources), 0)

  unlink(corrupted_path)
})

# Cleanup
unlink(valid_qdpx_path)
