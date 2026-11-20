#' @title Resolve a Neotoma contact record
#' @description
#' Decide what one response of the Neotoma API endpoint `contacts/<contactid>`
#' yields: a contact record, an empty record, or a failed request.
#' @param status_code
#' A single number, the HTTP status of the response, or `NA` when the request
#' got no response.
#' @param response_content
#' The parsed body of the response (a list), or `NULL` when the request got
#' no response.
#' @return
#' `NULL` when the request failed: the status is not 200, or the body has no
#' `data` list. An empty list when the status is 200 and `data` is empty,
#' which means Neotoma holds no contact record for this ID. Otherwise the
#' first contact record of `data`, a list.
#' @details
#' Some contact IDs listed as dataset authors have no contact record; the API
#' then returns status 200 with an empty `data` list. Such an author is kept
#' with an empty record, as `RFossilpol` did, rather than treated as failed.
resolve_neotoma_contact <- function(
  status_code,
  response_content
) {
  assertthat::assert_that(
    base::is.numeric(status_code) || base::identical(status_code, NA),
    base::length(status_code) == 1L,
    msg = "'status_code' must be a single number or NA."
  )

  assertthat::assert_that(
    base::is.list(response_content) || base::is.null(response_content),
    msg = "'response_content' must be a list or NULL."
  )

  list_data <-
    purrr::pluck(response_content, "data")

  is_success <-
    base::isTRUE(status_code == 200) && base::is.list(list_data)

  if (
    !is_success
  ) {
    return(NULL)
  }

  res <-
    purrr::pluck(
      list_data,
      1,
      .default = base::list()
    )

  return(res)
}
