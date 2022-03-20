coded_segments_test <- data.frame(
    project_id = 1, 
    doc_id = 1, 
    code_id = 1, 
    segment_id = 1, 
    segment_start = 1, 
    segment_end = 4, 
    segment_text = "ahoj"
)

test_that("code_overlap works", {
    expect_true(nrow(check_overlap(coded_segments_test, 6, 10)) == 0)
    expect_true(nrow(check_overlap(coded_segments_test, 2, 10)) == 1)
})
