#' Read an agricultural accounts table
#'
#' Extract data from a tidyxl::xlsx_cells output
#'
#' @param cells a tidyxl::xlsx_cells output
#'
#' @returns a dataframe in tidy format
#' @export
#'
#' @examples
#' \dontrun{
#' ac <- system.file("extdata/accounts.xlsx", package = "psp") |>
#' tidyxl::xlsx_cells()
#'
#' read_accounts(ac)
#' }
read_accounts <- function(cells) {

  check_tidyxl(cells)

  test <- c("ID", "Item")
  a1b1 <- cells |>
    dplyr::filter(row == 1, col %in% c(1, 2)) |>
    dplyr::pull(character)

  if(!identical(test, a1b1)) {
    cli::cli_abort(c("{.arg cells} does not appear to be an accounts table",
                     "i" = "Expecting cells A1 and B1 to contain 'ID' and 'Item'"))
  }

  out <- cells |>
    dplyr::filter(row != 2, row <= 536, !.data$is_blank) |>
    dplyr::filter(row <= 536) |>
    unpivotr::behead("up", "year") |>
    unpivotr::behead("left", "id") |>
    unpivotr::behead("left", "item") |>
    dplyr::select("year", "id", "item", value = "numeric")

  return(out)

}
