#' Title
#'
#' @param cells
#'
#' @returns
#' @export
#'
#' @examples
get_accounts <- function(cells) {

  out <- cells |>
    dplyr::filter(row != 2) |>
    dplyr::filter(row <= 536) |>
    unpivotr::behead("up", "year") |>
    unpivotr::behead("left", "id") |>
    unpivotr::behead("left", "item") |>
    dplyr::select(.data$year, .data$id, .data$item, value = numeric)

  return(out)

}
