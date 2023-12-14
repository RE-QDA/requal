## DEV notes

Instruction for local development of `reQual` server version.
  
- Run `create_db_users.R` to create mock users. 
- Run `docker-compose up` to start up DB. 
- Run `run_dev.R`. 

## Testing

- `shinytest2::record_test("inst/test_app")` - starts the test version of the app 
(defined in `app.R` which uses a database with empty project `tests/test_basic.requal`)

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
- [ ] export codebook

#### Categories
- [x] add category
- [x] remove category
- [ ] assign code to category
- [ ] remove code from category

### Workdesk
- [ ] select document to code
- [ ] assign code to segment
- [ ] remove code from segment

### Analysis
- [ ] filter 
- [ ] export

### Report

### About
- [ ] loads

### Memos
- [ ] create
- [ ] edit
- [ ] delete

### Users
