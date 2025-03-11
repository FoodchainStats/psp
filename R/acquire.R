#' Title
#'
#'
#' @param path Path to a supply and use spreadsheet
#'
#' @returns
#' @export
#'
#' @examples
acquire_sut <- function(path){

  if (!missing(path)) {
    if (!dir.exists(path))
      stop(paste(path, "does not exist"))
  }

  url <- url_sut()

  if (missing(path)) {
    tmp <- tempfile()
    utils::download.file(url, tmp)
    sutfile <- tmp
  } else {
    utils::download.file(url, destfile = paste0(path, "/",
                                                "sut.xlsx"))
    sutfile <- paste0(path, "/", "sut.xlsx")
  }

  return(sutfile)


}
