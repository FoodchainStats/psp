
#' Title
#'
#' @param cells a tidyxl::xlsx_cells output
#'
#' @returns
#' @export
#'
#' @examples
sut_tableno <- function(cells) {

b3 <- cells |>
  dplyr::filter(row == 3, col == 2) |>
  dplyr::pull(character)

c3 <- cells |>
  dplyr::filter(row == 3, col == 3) |>
  dplyr::pull(character)

t1 <- stringr::str_detect(b3, "Product")
t2a <- stringr::str_detect(c3, "intermediate")
t2b <- stringr::str_detect(c3, "[dD]emand")
t3 <- stringr::str_detect(c3, "01.1")
t4 <- stringr::str_detect(c3, "capital")

t <- which(c(t1, t2a, t2b, t3, t4))

out <- dplyr::case_when(t == 1 ~ "supply",
                 t == 2 ~ "intcon",
                 t == 3 ~ "demand",
                 t == 4 ~ "hhfce",
                 t == 5 ~ "gfcf",
                 .default = NA)

return(out)

}


#' Title
#'
#' @param cells a tidyxl::xlsx_cells output
#'
#' @returns
#' @export
#'
#' @examples
get_sut <- function(cells){

  t_no <- sut_tableno(cells)

  if(t_no == "supply") {

    out <- cells |>
      dplyr::filter(row >=3, row <=107) |>
      unpivotr::behead("up", "category") |>
      unpivotr::behead("left", "code") |>
      unpivotr::behead("left", "product") |>
      dplyr::select("category", "code", "product", value = "numeric") |>
      dplyr::mutate(category = stringr::str_replace_all(.data$category, " \\r\\n(?=[a-zA-z])", " ")) |>
      dplyr::mutate(category = stringr::str_replace_all(.data$category, "\\r\\n", " ")) |>
      dplyr::mutate(category = stringr::str_replace_all(.data$category, "'", "")) |>
      dplyr::mutate(product = stringr::str_trim(.data$product))

    return(out)
  }

  if(t_no == "intcon") {
    out <- cells |>
      dplyr::filter(row >=4, row <=109) |>
      unpivotr::behead("up", "sic") |>
      unpivotr::behead("up", "industry") |>
      unpivotr::behead("left", "code") |>
      unpivotr::behead("left", "product") |>
      dplyr::select("sic", "industry", "code", "product", value = "numeric") |>
      dplyr::mutate(product = stringr::str_trim(.data$product))

    return(out)
  }

}


verify_sut <- function(cells) {

  is_sut <- TRUE

  cols <- c("sheet", "address", "row", "col")

  if(!identical(colnames(cells)[1:4], cols)) {
    is_sut <- FALSE
    message("not a sut spreadsheet")
    stop()
    }


  return(is_sut)
}




