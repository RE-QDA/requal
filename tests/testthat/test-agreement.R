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
    1, 1, 1, 3, 20, 30, 
    2, 1, 1, 4, 50, 60
)

segments4 <- dplyr::tribble(
    ~user_id, ~doc_id, ~code_id, ~segment_id, ~segment_start, ~segment_end,
    1, 1, 1, 1, 1, 20,
    2, 1, 1, 2, 1, 10,
    2, 1, 1, 3, 15, 20,
    1, 1, 1, 4, 30, 40,
    2, 1, 1, 5, 50, 60
)
# 
# segments5 <- dplyr::tribble(
#     ~user_id, ~doc_id, ~code_id, ~segment_id, ~segment_start, ~segment_end, 
#     1, 1, 1, 1, 1, 10, 
#     2, 1, 1, 2, 6, 10, 
#     3, 1, 1, 3, 1, 5
# )

# calculate_segment_overlap_by_users(segments)
# calculate_segment_overlap_by_users(segments2)
# calculate_segment_overlap_by_users(segments3)
# calculate_segment_overlap_by_users(segments4)
# calculate_segment_overlap_by_users(segments5)

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
})
