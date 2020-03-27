
api_url <- "https://colorado.rstudio.com/rsc/satrdays_bike_api/"

#' Get Feeds
#'
#' @return data frame of feed names and URLs
#' @export
#'
#' @examples
#' get_feeds_list()
get_feeds_list <- function() {
  file.path(api_url, "feeds") %>%
    httr::GET() %>%
    httr::content() %>%
    purrr::map_df(tibble::as.tibble)
}

#' Get URL of a Feed
#'
#' @param feed name of a feed
#'
#' @return list of data and update time
#'
#' @examples
#' get_feed_url("station_information")
get_feed_url <- function(feed) {
  feeds <- get_feeds_list()

  stopifnot(feed %in% feeds$name)

  dplyr::filter(feeds, name == feed) %>%
    dplyr::pull(url)
}

#' Get data from a feed
#'
#' @param feed name of a feed
#'
#' @return data from feed
#' @export
#'
#' @examples
#' get_feed_dat("station_status")
get_feed_dat <- function(feed) {
  url <- get_feed_url(feed)

  dat <- httr::GET(
    file.path(api_url, "feed_dat"),
    query = list(
      feed_url = url
    )
  ) %>%
    httr::content()

  dat$dat <- dat$dat %>%
    purrr::map_df(tibble::as.tibble)
  dat$last_updated <- dat$last_updated[[1]]

  dat
}

#' Make Plot Data from Station and Status
#'
#' @param station station df
#' @param status status df
#'
#' @return combined plot data
#' @export
#'
#' @examples
#' make_plot_dat(get_feed_dat("station_information")$dat, get_feed_dat("station_status")$dat)
make_plot_dat <- function(station, status) {
  dplyr::inner_join(station, status) %>%
    dplyr::mutate(
      plot_val = glue::glue(
        paste0(
          "<b>{name}</b><br>",
          "Bikes: {num_bikes_available}<br>",
          "Empty Docks: {num_docks_available}"
        ))
    )
}
