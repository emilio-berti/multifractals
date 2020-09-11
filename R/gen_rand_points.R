#' Generates @param n random points in the bi-dimensional plane x = [0, 1], y =
#' [0, 1].
#' @param n integer, number of random points to generate.
#' @return sf object with only the geometry column.
gen_rand_points <- function(x) {
  sp <- tibble::tibble(x = runif(n),
                       y = runif(n),
                       geometry = purrr::map2(x, y, function(x, y) {
                         sf::st_point(c(x, y))
                       }))
  sp <- sf::st_as_sf(sp)
  sp <- dplyr::select(sp, geometry)
  return(sp)
}

a <- function(p, A) {
  sp <- tibble::tibble(x = runif(n),
                       y = runif(n),
                       geometry = purrr::map2(x, y, function(x, y) {
                         sf::st_point(c(x, y))
                       }))
  sp <- sf::st_as_sf(sp)
  sp <- dplyr::select(sp, geometry)
  return(sp)
}
