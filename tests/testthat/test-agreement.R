segments <- dplyr::tribble(
    ~user_id, ~doc_id, ~code_id, ~segment_id, ~segment_start, ~segment_end, 
    1, 1, 1, 1, 1, 10, 
    2, 1, 1, 2, 6, 10
)

segments2 <- dplyr::tribble(
    ~user_id, ~doc_id, ~code_id, ~segment_id, ~segment_start, ~segment_end, 
    1, 1, 1, 1, 1, 10, 
    2, 1, 1, 2, 15, 20
)

segments3 <- dplyr::tribble(
    ~user_id, ~doc_id, ~code_id, ~segment_id, ~segment_start, ~segment_end, 
    1, 1, 1, 1, 1, 10, 
    2, 1, 1, 2, 6, 10, 
    1, 1, 1, 3, 20, 30
)

segments4 <- dplyr::tribble(
    ~user_id, ~doc_id, ~code_id, ~segment_id, ~segment_start, ~segment_end,
    1, 1, 1, 1, 1, 20,
    2, 1, 1, 2, 1, 10,
    2, 1, 1, 3, 15, 20,
    1, 1, 1, 4, 30, 40,
    2, 1, 1, 5, 50, 60
)

test_that("calculate segment overlap works", {
    expect_equal(
        mean(calculate_segment_overlap_by_users(segments)$is_overlap), 1
    ) 
    expect_equal(
        mean(calculate_segment_overlap_by_users(segments2)$is_overlap), 0
    )
    expect_equal(
        mean(calculate_segment_overlap_by_users(segments3)$is_overlap), 0.5
    )
    expect_equal(
        round(mean(calculate_segment_overlap_by_users(segments4)$is_overlap), 2), 0.33
    )
})

segments1b <- dplyr::tribble(
    ~user_id, ~doc_id, ~code_id, ~segment_id, ~segment_start, ~segment_end, 
    1, 1, 1, 1, 1, 10, 
    2, 1, 1, 2, 1, 10
)

segments2b <- dplyr::tribble(
    ~user_id, ~doc_id, ~code_id, ~segment_id, ~segment_start, ~segment_end, 
    1, 1, 1, 1, 1, 10, 
    2, 1, 1, 2, 20, 30
)

segments3b <- dplyr::tribble(
    ~user_id, ~doc_id, ~code_id, ~segment_id, ~segment_start, ~segment_end, 
    1, 1, 1, 1, 1, 10, 
    2, 1, 1, 2, 1, 5
)

test_that("calculate code overlap works", {
    expect_equal(
        calculate_code_overlap_by_users(segments1b)$total_overlap, 1
    )
    expect_equal(
        calculate_code_overlap_by_users(segments2b)$total_overlap, 0
    )
    expect_equal(
        calculate_code_overlap_by_users(segments3b)$total_overlap, 0.5
    )
})
