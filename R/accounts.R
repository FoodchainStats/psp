#' Title
#'
#' @param cells a tidyxl::xlsx_cells output
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
    dplyr::select("year", "id", "item", value = "numeric")

  return(out)

}
