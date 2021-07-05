# library(RSQLite)
# library(RPostgres)
# 
# con <- dbConnect(SQLite(), "test.sqlite")
# con_post <- dbConnect(Postgres(), 
#                       dbname = "uniq", 
#                       host = "localhost", 
#                       user = "uniq", 
#                       password = "janhusdidnothingwrong")
# 

create_db <- function(con){
    UseMethod("create_db")
}

create_db.SQLiteConnection <- function(con){
    r <- dbSendStatement(con, 
    'create table projects 
    (   project_id INTEGER PRIMARY KEY
    ,   project_name TEXT
    ,   project_description TEXT
    ,   created_at TEXT
    )')
    dbClearResult(r)
}


create_db.PqConnection <- function(con){
    r <- dbSendStatement(con, "create table projects (project_id INTEGER PRIMARY KEY
                         ,  project_name TEXT
                         ,  project_description TEXT
                         ,  created_at TEXT)")
    dbClearResult(r)
}
