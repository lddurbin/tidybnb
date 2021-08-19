test_that("rvest can read the BNB webpage", {
  length(rvest::read_html("https://www.bl.uk/collection-metadata/new-bnb-records")) > 0
})

test_that("rvest can extract the relevant data from the BNB webpage", {
  BNB_page_rdfs <- rvest::read_html("https://www.bl.uk/collection-metadata/new-bnb-records") %>%
    rvest::html_elements(".grid_39") %>%
    rvest::html_elements("ul:last-child") %>%
    rvest::html_elements("li") %>%
    rvest::html_text2()

  length(BNB_page_rdfs) > 1
})
