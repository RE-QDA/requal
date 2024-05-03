## DEV notes

Instruction for local development of `requal` server version.
  
- Run `create_db_users.R` to create mock users. 
- Run `docker-compose up` to start up DB. 
- Run `run_dev.R`. 

## Testing

- `shinytest2::record_test("inst/test_app")` - starts the test version of the app 
(defined in `app.R` which uses a database with empty project `tests/test_basic.requal`)
- tests need to be run using `devtools::test()` (or Build > Test in RStudio) 

- running test app interactively can be done using `app <- AppDriver$new(app_dir = "inst/test_app", name = "requaltest", seed = 123, height = 789, width = 1139)` and `app$view()`

## Test coverage

### Project
- [ ] create new project
- [x] load project

### Documents
- [x] create document
- [ ] delete document
- [ ] upload document

### Codebook
#### Codes
- [x] add code
- [x] merge codes
- [x] delete code
- [x] export codebook

#### Categories
- [x] add category
- [x] remove category
- [ ] assign code to category
- [ ] remove code from category

### Workdesk
- [x] select document to code
- [x] assign code to segment
- [x] remove code from segment
- [ ] removing code using the modal window when there are multiple codes overlapping 

### Analysis
- [ ] filter 
- [ ] export

### Report

### About
- [ ] loads

### Memos
- [x] create
- [x] edit
- [x] delete
- [x] export

### Users
