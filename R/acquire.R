#' Download an ONS supply use spreadsheet
#'
#' Retrieves a supply and use table from the ONS (see [url_sut()]) and stores it
#' in the specified folder, or tempfile if not specified.
#'
#' @param path Path to a folder store a supply and use spreadsheet. Downloaded
#'   file will be named 'sut.xlsx'.If not specified a tempfile will be used.
#'
#' @returns A file location
#' @export
#'
#' @examples
#' \dontrun{
#'
#' sut <- acquire_sut()
#'
#' # or specify a folder
#' sut <- acquire_sut("~/downloads")
#'
#' }
acquire_sut <- function(path){

  if (!missing(path)) {
    if (!dir.exists(path))
      cli::cli_abort(message = "Path {.arg {path}} does not exist.")
      # stop(paste(path, "does not exist"))
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
    message(sutfile)
  }

  return(sutfile)


}
