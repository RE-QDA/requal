# delete documents from project
delete_db_documents <- function(project_db, 
                                active_project, 
                                delete_doc_id) {
    
    con <- DBI::dbConnect(RSQLite::SQLite(), project_db)
    on.exit(DBI::dbDisconnect(con))
    
    
    DBI::dbExecute(con,
                   "DELETE from documents
                   WHERE doc_id IN (?)",
                   params = list(delete_doc_id))
    if(length(delete_doc_id)){
        log_delete_document_record(con, active_project, delete_doc_id)   
    }
}


# add input document ----
add_input_document <- function(connection, project, doc_name, doc_text, doc_description) {
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          connection)
    
    on.exit(DBI::dbDisconnect(con))

        text_df <- tibble::tibble(
        project_id = project,
        doc_name = .env$doc_name,
        doc_description = .env$doc_description,
        doc_text = .env$doc_text
    )
    
    add_documents_record(con, project, text_df)
    
}

# encodings -----

support_encodings <- function() {
    
    encodings_df <- tibble::enframe(c("all languages (utf_16)", "all languages (utf_32)", "all languages (utf_8)", "Arabic (cp1256)", "Arabic (cp720)", "Arabic (cp864)", "Arabic (iso8859_6)", "Baltic languages (cp1257)", "Baltic languages (cp775)", "Baltic languages (iso8859_13)", "Baltic languages (iso8859_4)", "Bulgarian, Byelorussian, Macedonian, Russian, Serbian (cp1251)", "Bulgarian, Byelorussian, Macedonian, Russian, Serbian (cp855)", "Bulgarian, Byelorussian, Macedonian, Russian, Serbian (iso8859_5)", "Bulgarian, Byelorussian, Macedonian, Russian, Serbian (mac_cyrillic)", "Canadian (cp863)", "Celtic languages (iso8859_14)", "Danish, Norwegian (cp865)", "English (ascii)", "English (cp037)", "English (cp437)", "Esperanto, Maltese (iso8859_3)", "Europe Central and Eastern (cp1250)", "Europe Central and Eastern (cp852)", "Europe Central and Eastern (iso8859_2)", "Europe Central and Eastern (mac_latin2)", "Europe South-Eastern (iso8859_16)", "Europe Western (cp1140)", "Europe Western (cp1252)", "Europe Western (cp500)", "Europe Western (cp850)", "Europe Western (cp858)", "Europe Western (iso8859_15)", "Europe Western (latin_1)", "Europe Western (mac_roman)", "German (cp273)", "Greek (cp1253)", "Greek (cp737)", "Greek (cp869)", "Greek (cp875)", "Greek (iso8859_7)", "Greek (mac_greek)", "Hebrew (cp1255)", "Hebrew (cp424)", "Hebrew (cp856)", "Hebrew (cp862)", "Hebrew (iso8859_8)", "Chinese Simplified (gb2312)", "Chinese Simplified (hz)", "Chinese Traditional (big5)", "Chinese Traditional (big5hkscs)", "Chinese Traditional (cp950)", "Chinese Unified (gb18030)", "Chinese Unified (gbk)", "Icelandic (cp861)", "Icelandic (mac_iceland)", "Japanese (cp932)", "Japanese (euc_jp)", "Japanese (iso2022_jp)", "Japanese (shift_jis)", "Japanese, Korean, Simplified Chinese, Western Europe, Greek (iso2022_jp_2)", "Kazakh (ptcp154)", "Kazakh (kz1048)", "Korean (cp949)", "Korean (euc_kr)", "Korean (iso2022_kr)", "Korean (johab)", "Nordic languages (iso8859_10)", "Portuguese (cp860)", "Russian (cp866)", "Russian (koi8_r)", "Tajik (koi8_t)", "Thai (cp874)", "Thai languages (iso8859_11)", "Turkish (cp1026)", "Turkish (cp1254)", "Turkish (cp857)", "Turkish (iso8859_9)", "Turkish (mac_turkish)", "Ukrainian (cp1125)", "Ukrainian (koi8_u)", "Urdu (cp1006)", "Vietnamese (cp1258)"))  %>% 
        dplyr::mutate(encoding = toupper(stringr::str_replace(value, ".*\\(([^\\)]+)\\)", "\\1")),
    encoding = stringr::str_replace(encoding, "_", "-"))
    
    
    values <- encodings_df$encoding
        
    names <- encodings_df$value
        
    names(values) <- names
    
    return(values)
}
