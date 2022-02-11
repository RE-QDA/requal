# Define project_name as global variable to pass R CMD Check without notes
utils::globalVariables(c("project_name", 
                         "project_id", 
                         "doc_id",
                         "code_name",
                         "code_id",
                         "segment_id",
                         "code_color",
                         "text"))
