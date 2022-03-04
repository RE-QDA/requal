con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
create_db_schema(con)

test_that("DB Schema was created", {
    expect_true(DBI::dbExistsTable(con, "projects"))
    expect_true(DBI::dbExistsTable(con, "logs"))
    expect_true(DBI::dbExistsTable(con, "documents"))
    expect_true(DBI::dbExistsTable(con, "segments"))
})

project_df <- dplyr::tibble(
    project_name = "test", 
    project_description = "test",
    created_at = "2021-01-01 00:00:00"
)
create_project_record(con, project_df)

test_that("Test project is recorded", {
    expect_true((dplyr::tbl(con, "projects") %>% 
                     dplyr::collect() %>% 
                     nrow()) == 1)
    expect_true(dplyr::tbl(con, "projects") %>% 
                    dplyr::collect() %>% 
                    dplyr::pull(project_name) == "test")
    
    expect_true((dplyr::tbl(con, "logs") %>% 
                     dplyr::collect() %>% 
                     nrow()) == 1)
    expect_true(dplyr::tbl(con, "logs") %>% 
                    dplyr::collect() %>% 
                    dplyr::pull(action) == "Create project")
})

# create_project_record()