
import_rqda_test_file <- "test_rqda_import.rqda"
import_requal_test_file <- "test_rqda_import.requal"

rqda_con <- DBI::dbConnect(RSQLite::SQLite(), import_rqda_test_file)
DBI::dbWriteTable(rqda_con, "project", data.frame(
    databaseversion = "0.2.2", 
    date = "", 
    memo = "test RQDA file", 
    about = "RQDA"
))

DBI::dbWriteTable(rqda_con, "source", data.frame(
    name = "doc_name", 
    id = 1, 
    file = "text text text", 
    memo = "document 1", 
    status = 1
))

DBI::dbWriteTable(rqda_con, "caselinkage", data.frame(
    status = 1, 
    caseid = 1, 
    fid = 1
))

DBI::dbWriteTable(rqda_con, "cases", data.frame(
    name = "case 1", 
    id = 1, 
    memo = "description", 
    status = 1
))

DBI::dbWriteTable(rqda_con, "codecat", data.frame(
    name = "Category 1", 
    catid = 1, 
    cid = NA, 
    status = 1, 
    memo = "category description"
))

DBI::dbWriteTable(rqda_con, "coding", data.frame(
    cid = 1, 
    fid = 1, 
    seltext = "text", 
    selfirst = 1, 
    selend = 5, 
    status = 1, 
    memo = "coding memo"
))

DBI::dbWriteTable(rqda_con, "freecode", data.frame(
    name = "code 1", 
    memo = "code description", 
    status = 1, 
    id = 1, 
    color = NA
))

DBI::dbWriteTable(rqda_con, "treecode", data.frame(
    cid = 1, 
    catid = 1, 
    status = 1
))


rql_import_rqda(import_rqda_test_file, import_requal_test_file)
import_con <- DBI::dbConnect(RSQLite::SQLite(), import_requal_test_file)

test_that("Import from RQDA works", {
    expect_true(DBI::dbExistsTable(import_con, "projects"))
    expect_true(DBI::dbExistsTable(import_con, "logs"))
    expect_true(DBI::dbExistsTable(import_con, "documents"))
    expect_true(DBI::dbExistsTable(import_con, "segments"))
    expect_true(dplyr::tbl(import_con, "documents") %>% dplyr::collect() %>% nrow() == 
                    dplyr::tbl(rqda_con, "source") %>% dplyr::collect() %>% nrow())
})

DBI::dbDisconnect(import_con)
DBI::dbDisconnect(rqda_con)

unlink(import_rqda_test_file)
unlink(import_requal_test_file)