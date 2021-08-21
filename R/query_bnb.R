#' Query the RDF data and return as a tibble
#'
#' @param rdf_data character vector containing path(s) to rdf file(s)
#'
#' @return tibble containing query result
#' @export
#'
query_bnb <- function(rdf_data) {
  purrr::walk(rdf_data, virtuoso_load)
}


# Helpers -----------------------------------------------------------------

virtuoso_start <- function() {
  if(!virtuoso::has_virtuoso()){
    virtuoso::vos_install()
  }
  virtuoso::vos_start()
  virtuoso::vos_status()
}

virtuoso_load <- function(rdf_data) {
  virtuoso_start()
  virtuoso::vos_import(virtuoso::vos_connect(), rdf_data)
  virtuoso::vos_log() %>% utils::tail(n=1)
}
