pool <- pool::dbPool(
    drv = RPostgreSQL::PostgreSQL(),
    dbname = "requal",
    user = "radimhladik",
    password = "test"
)

pool::dbGetInfo(pool)

DBI::dbListTables(pool)

dplyr::tbl(pool, "projects")
