pool <- pool::dbPool(RSQLite::SQLite(), dbname = ":memory:")
create_db_schema(pool)

test_that("DB Schema was created", {
    expect_true(DBI::dbExistsTable(pool, "projects"))
    expect_true(DBI::dbExistsTable(pool, "logs"))
    expect_true(DBI::dbExistsTable(pool, "documents"))
    expect_true(DBI::dbExistsTable(pool, "segments"))
})

project_df <- dplyr::tibble(
    project_name = "test", 
    project_description = "test",
    created_at = "2021-01-01 00:00:00"
)
# create_project_record(pool, project_df, user_id = 1)
# 
# test_that("Test project is recorded", {
#     expect_true((dplyr::tbl(pool, "projects") %>% 
#                      dplyr::collect() %>% 
#                      nrow()) == 1)
#     expect_true(dplyr::tbl(pool, "projects") %>% 
#                     dplyr::collect() %>% 
#                     dplyr::pull(project_name) == "test")
#     
#     expect_true((dplyr::tbl(pool, "logs") %>% 
#                      dplyr::collect() %>% 
#                      nrow()) == 1)
#     expect_true(dplyr::tbl(pool, "logs") %>% 
#                     dplyr::collect() %>% 
#                     dplyr::pull(action) == "Create project")
# })
