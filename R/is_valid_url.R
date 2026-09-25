#' Title
#'
#' @param url
#' @param timeout_sec
#'
#' @returns
#' @export
#'
#' @details
#' Proposed by Gemini under request.
#'
#'
#' @examples
is_valid_url <- function(url, timeout_sec = 5) {

  # Common browser User-Agent to bypass bot-blocking (fixes many 403s)
  ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"

  # Helper to perform the request safely
  perform_check <- function(method) {
    tryCatch({
      resp <- httr2::request(url) |>
        httr2::req_method(method) |>
        httr2::req_user_agent(ua) |>
        httr2::req_options(followlocation = TRUE) |> # Automatically follow redirects (301/302)
        httr2::req_timeout(timeout_sec) |>
        httr2::req_error(is_error = \(resp) FALSE) |> # Prevent httr2 from throwing R errors on HTTP 4xx/5xx
        httr2::req_perform()

      httr2::resp_status(resp)
    }, error = function(e) {
      NA_integer_
    })
  }

  # 1. Try lightweight HEAD request first
  status <- perform_check("HEAD")

  # 2. If HEAD failed, returned 403, or 405 (Method Not Allowed), fall back to GET
  if (is.na(status) || status %in% c(403, 405, 400)) {
    status <- perform_check("GET")
  }

  if (is.na(status)) return(FALSE)

  # Consider valid if:
  # - 2xx (Success)
  # - 3xx (Redirect reached if followlocation is off, but with followlocation TRUE it lands on 2xx)
  # - 403 (Server exists and answered, even if blocking access)
  return(status >= 200 && status < 400 || status == 403)
}
