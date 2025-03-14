
#' Identify a supply use table
#'
#' Given a dataframe derived from a SUT spreadsheet, identify which type of SUT
#' it is.
#'
#' @param cells a tidyxl::xlsx_cells output
#'
#' @returns a value identifying the type of table
#' @export
#'
#' @examples
#' \dontrun{
#' sut <- acquire_sut()
#'
#' x |>
#'   tidyxl::xlsx_cells(sut) |>
#'   dplyr::filter(sheet == "Table 1 - Supply 2017") |>
#'   sut_tabletype()
#'
#' }
sut_tabletype <- function(cells) {

  # if(verify_sut(cells) == FALSE) {
  #   # stop("Object is not the output from a tidyxl::xlsx_cells function")
  #   cli::cli_abort(message = "{.arg cells} is not the output from a {.fn tidyxl::xlsx_cells} function")
  # }

  # check_sut(cells)

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
                   .default = "x")

  if(rlang::is_empty(out)) out <- "fail"

  return(out)

}


#' Read a supply use table
#'
#' Extract a SUT from a tidyxl::xlsx_cells output
#'
#' @param cells a tidyxl::xlsx_cells output
#'
#' @returns a dataframe in tidy format
#' @export
#'
#' @examples
#' \dontrun{
#' sut <- acquire_sut()
#'
#' x |>
#'   tidyxl::xlsx_cells(sut) |>
#'   dplyr::filter(sheet == "Table 1 - Supply 2017") |>
#'   read_sut()
#'
#' }
read_sut <- function(cells){


  check_sut(cells)

  t_no <- sut_tabletype(cells)

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

  if(t_no == "demand") {

    other <-  c("Exports of goods to EU",
                "Exports of goods to rest of the world",
                "Total exports of Goods",
                "Exports of Services",
                "Total exports of goods and services",
                "Total final demand",
                "Total demand for products")

    out <- cells |>
      dplyr::filter(!.data$is_blank) |>
      dplyr::filter(row >=4, row <= 109) |>
      unpivotr::behead(direction = "up-left", "category") |>
      unpivotr::behead(direction = "up", "type") |>
      unpivotr::behead("left", "code") |>
      unpivotr::behead("left", "product") |>
      dplyr::select("category", "type", "code", "product", value = "numeric") |>
      dplyr::mutate(category = dplyr::case_when(tolower(type) %in% tolower(other) ~ "Other",
                                                .default = .data$category),
                    type = stringr::str_replace_all(.data$type, "\\r\\n", " "),
                    product = stringr::str_trim(.data$product))


    return(out)

  }

  if(t_no == "hhfce") {

    out <- cells |>
      dplyr::filter(row >=3, row <= 107, !.data$is_blank) |>
      unpivotr::behead("up", "sic") |>
      unpivotr::behead("up", "category") |>
      unpivotr::behead("left", "code") |>
      unpivotr::behead("left", "product") |>
      dplyr::select("sic", "category", "code", "product", value = "numeric") |>
      dplyr::mutate(sic = dplyr::case_when(is.na(sic) ~ "Total",
                                           .default = .data$sic),
                    product = stringr::str_trim(.data$product))

    return(out)
  }

  if(t_no == "gfcf") {
    out <- cells |>
      dplyr::filter(row >=4, row <= 31, !.data$is_blank) |>
      unpivotr::behead("up", "sic") |>
      unpivotr::behead("up", "category") |>
      unpivotr::behead("left", "code") |>
      unpivotr::behead("left", "product") |>
      dplyr::select("sic", "category", "code", "product", value = "numeric") |>
      dplyr::mutate(sic = dplyr::case_when(is.na(sic) ~ "Total",
                                           .default = .data$sic),
                    product = stringr::str_trim(.data$product))


    return(out)
  }

  if(t_no == "fail") {
  cli::cli_abort(message = "{.arg cells} does not appear to be a supply and use table")
  }

}


verify_sut <- function(cells) {

  is_sut <- TRUE

  cols <- c("sheet", "address", "row", "col")

  if(!identical(colnames(cells)[1:4], cols)) {
    is_sut <- FALSE
    }

  return(is_sut)
}


check_sut <- function(cells, arg = rlang::caller_arg(cells), call = rlang::caller_env()) {
  # is_sut <- TRUE
  cols <- c("sheet", "address", "row", "col")
  if(!identical(colnames(cells)[1:4], cols)) {
    cli::cli_abort("{.arg {arg}} is not the output from a {.fn tidyxl::xlsx_cells} function", call = call)
  }
}



