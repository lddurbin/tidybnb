test_that("rvest can read the BNB webpage", {
  testthat::expect_gt(length(rvest::read_html("https://www.bl.uk/collection-metadata/new-bnb-records")), 0)
})
