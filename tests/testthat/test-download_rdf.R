test_that("rvest can read the BNB webpage", {
  testthat::expect_gt(length(rvest::read_html("https://www.bl.uk/collection-metadata/new-bnb-records")), 0)
})

test_that("rvest retrieves details for 25 RDF files", {
  testthat::expect_equal(length(get_rdf_details(rvest::read_html("https://www.bl.uk/collection-metadata/new-bnb-records"))), 25)
})

test_that("rvest retrieves URLs for 25 RDF files", {
  testthat::expect_equal(length(get_rdf_urls(rvest::read_html("https://www.bl.uk/collection-metadata/new-bnb-records"))), 25)
})
