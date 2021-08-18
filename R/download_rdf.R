#' Download new British National Bibliography records in RDF format
#'
#' @param rdf_dates user-defined dates in the format d/m/y for which RDF files they want to download. If no argument is supplied, the most recent 25 records are downloaded
#' @param file_location character string of path to directory where RDF file(s) will be stored
#'
#' @return RDF files in the user-defined directory
#' @export
#'
#' @examples
#' file_dates <- c("28/7/2021", "4/8/2021")
#' download_rdf("raw data/rdf", file_dates)
download_rdf <- function(file_location, rdf_dates = c(TRUE)) {
  BNB_page <- rvest::read_html("https://www.bl.uk/collection-metadata/new-bnb-records")

  BNB_page_rdfs <- BNB_page %>%
    rvest::html_elements(".grid_39") %>%
    rvest::html_elements("ul:last-child") %>%
    rvest::html_elements("li") %>%
    rvest::html_text2()

  BNB_rdf_dates <- BNB_page_rdfs %>%
    stringr::word(1) %>%
    lubridate::dmy()

  BNB_urls_hashed <- BNB_page %>%
    rvest::html_elements(".grid_39") %>%
    rvest::html_elements("ul:last-child") %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href")

  BNB_urls_hashed <- paste0("https://www.bl.uk/collection-metadata/", BNB_urls_hashed)

  BNB_rdf_filenames <- BNB_page_rdfs %>%
    stringr::word(2) %>%
    stringr::str_sub(2) %>%
    stringr::str_to_lower()

  if(length(rdf_dates) > 0) {
    rdf_dates <- lubridate::dmy(rdf_dates)
    files_to_download <- which(BNB_rdf_dates %in% rdf_dates)
  }

  target_slugs <- BNB_urls_hashed[files_to_download]
  target_urls <- lapply(target_slugs, utils::URLencode) %>% unlist()
  target_filenames <- BNB_rdf_filenames[files_to_download]

  utils::download.file(target_urls, destfile = paste0(file_location, "/", target_filenames), method = "libcurl")
}
