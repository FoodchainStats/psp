#' URL for Supply and Use tables
#'
#' @returns The location of ONS Supply and Use tables
#' @export
#'
#' @examples
#' url <- url_sut()
url_sut <- function() {
  url <- "https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables"

  html <- rvest::read_html(url)

  links <- html |> rvest::html_elements("a")
  linktext <- links |> rvest::html_text2()
  linkurls <- links |> rvest::html_attr("href")
  datalink <- which(stringr::str_detect(linktext, "xlsx \\("))
  link <- linkurls[datalink]

  file <- paste0("https://www.ons.gov.uk", link)
  return(file)
}
