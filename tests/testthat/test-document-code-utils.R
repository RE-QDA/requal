coded_segments_df <- dplyr::tibble(
    project_id = 1,
    code_id = 1, 
    doc_id = 1,
    segment_id = c(1, 2), 
    segment_start = c(10, 30), 
    segment_end = c(20, 40)
)

coded_segments_overlap <- dplyr::tibble(
    project_id = 1, 
    code_id = c(1, 2), 
    doc_id = 1, 
    segment_id = c(1, 2), 
    segment_start = c(10, 20), 
    segment_end = c(30, 40)
)

paragraphs <- NULL
test_that("Test overlap checking returns correct nrows", {
    expect_true(nrow(check_overlap(coded_segments_df, 0, 8)) == 0)
    expect_true(nrow(check_overlap(coded_segments_df, 20, 25)) == 1)
    expect_true(nrow(check_overlap(coded_segments_df, 25, 35)) == 1)
    expect_true(nrow(check_overlap(coded_segments_df, 15, 35)) == 2)
})

test_that("Test that calculating new segment range works", {
    expect_true(
        check_overlap(coded_segments_df, 20, 25) %>%
            summarise_new_segment_range(., 20, 25) %>%
            dplyr::pull(segment_start) == 10
    )
    expect_true(
        check_overlap(coded_segments_df, 20, 25) %>%
            summarise_new_segment_range(., 20, 25) %>%
            dplyr::pull(segment_end) == 25
    )
    
    expect_true(
        check_overlap(coded_segments_df, 15, 35) %>%
            summarise_new_segment_range(., 15, 35) %>%
            dplyr::pull(segment_start) == 10
    )
    expect_true(
        check_overlap(coded_segments_df, 15, 35) %>%
            summarise_new_segment_range(., 15, 35) %>%
            dplyr::pull(segment_end) == 40
    )
})

test_that("Test that calculating code overlap works", {
    expect_true(nrow(calculate_code_overlap(coded_segments_df, paragraphs)) == 2)
    expect_true(nrow(calculate_code_overlap(coded_segments_overlap, paragraphs)) == 3)
    expect_true(all(calculate_code_overlap(coded_segments_overlap, paragraphs) %>% 
                        dplyr::pull(code_id) %in% c("1", "1-2", "2")))
    
})

library(testthat)
