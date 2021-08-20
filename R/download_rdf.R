#' Download new British National Bibliography records in RDF format
#'
#' @param files_to_download user-defined dates in the format d/m/y for which RDF files they want to download. If no argument is supplied, the most recent 25 records are downloaded
#' @param file_location character string of path to directory where RDF file(s) will be stored
#' @param file_format file type of downloaded files, either "zip" (original format), "rdf" (decompressed format) or "gzip" (re-compressed and suitable for uploading to Virtuoso)
#'
#' @return RDF files in the user-defined directory
#' @export
#'
#' @examples
#' file_dates <- c("28/7/2021", "4/8/2021")
#' download_rdf("raw data/rdf", file_dates)
download_rdf <- function(file_location, files_to_download = c(TRUE), file_format = "gzip") {
  BNB_page <- rvest::read_html("https://www.bl.uk/collection-metadata/new-bnb-records")

  BNB_page_rdfs <- get_rdf_details(BNB_page)

  BNB_rdf_dates <- BNB_page_rdfs %>%
    stringr::word(1) %>%
    lubridate::dmy()

  BNB_rdf_urls <- paste0("https://www.bl.uk/collection-metadata/", get_rdf_urls(BNB_page))

  BNB_rdf_filenames <- BNB_page_rdfs %>%
    stringr::word(2) %>%
    stringr::str_sub(2) %>%
    stringr::str_to_lower()

  if(!is.logical(files_to_download)) {
    files_to_download <- which(BNB_rdf_dates %in% lubridate::dmy(files_to_download))
  }

  target_urls <- lapply(BNB_rdf_urls[files_to_download], utils::URLencode) %>% unlist()
  target_file_locations <- paste0(file_location, "/", BNB_rdf_filenames[files_to_download])

  utils::download.file(target_urls, destfile = target_file_locations, method = "libcurl")

  format_files(file_format, target_file_locations, file_location)
}


# Helpers -----------------------------------------------------------------

get_rdf_details <- function(BNB_page) {
  BNB_page %>%
    rvest::html_elements(".grid_39") %>%
    rvest::html_elements("ul:last-child") %>%
    rvest::html_elements("li") %>%
    rvest::html_text2()
}

get_rdf_urls <- function(BNB_page) {
  BNB_page %>%
    rvest::html_elements(".grid_39") %>%
    rvest::html_elements("ul:last-child") %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href")
}

format_files <- function(file_format, target_file_locations, file_location) {
  if(file_format == "rdf") {
    purrr::walk(target_file_locations, utils::unzip, exdir = file_location)
    unlink(target_file_locations)
    if(file_format == "gzip") {
      lapply(target_file_locations %>% stringr::str_replace(".zip", ".rdf"), R.utils::gzip, ext = "gz", remove = TRUE)
    }
  }
}
