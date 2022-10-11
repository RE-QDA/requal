pool <- pool::dbPool(
    drv = RPostgreSQL::PostgreSQL(),
    dbname = "requal",
    user = "radimhladik",
    password = "test"
)
