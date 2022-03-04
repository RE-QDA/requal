
import_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

import_rqda("", import_con)

test_that("Import from RQDA works", {
    expect_true(DBI::dbExistsTable(import_con, "projects"))
    expect_true(DBI::dbExistsTable(import_con, "logs"))
    expect_true(DBI::dbExistsTable(import_con, "documents"))
    expect_true(DBI::dbExistsTable(import_con, "segments"))
})

