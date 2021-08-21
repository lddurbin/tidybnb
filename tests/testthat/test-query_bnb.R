test_that("Virtuoso is running when called", {
  testthat::expect_match(virtuoso_start(), "running")
})

test_that("A single RDF file is loaded into Virtuoso for querying", {
  testthat::expect_match(virtuoso_load(testthat::test_path("rdf", "bnbrdf_n3658.rdf.gz")) %>% stringr::word(-3, -1), "Loader has finished,")
})

test_that("Multiple RDF files are loaded into Virtuoso for querying", {
  rdf_files <- fs::dir_ls(testthat::test_path("rdf"))

  testthat::expect_s3_class(query_bnb(rdf_files), "character")
})
