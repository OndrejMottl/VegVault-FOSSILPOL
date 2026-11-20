#' @title Build a Neotoma author table
#' @description
#' Build the one-row author table that `RFossilpol` saves for each dataset
#' author, from a contact record of the Neotoma API.
#' @param author_id
#' A single author identifier of the form `"<contactid>_neotoma"`.
#' @param contact_info
#' A list with one contact record, as parsed from the `data` element of the
#' Neotoma API response `contacts/<contactid>`. The fields used are
#' `givennames`, `familyname`, `email`, `address` and `url`.
#' @return
#' A tibble with one row and the character columns `author_id`,
#' `first_name`, `last_name`, `email`, `Department` and `url`, in this order.
#' @details
#' The columns match those of `RFossilpol::proc_neo_get_authors()`, so its
#' compilation step can read the saved table. `first_name` comes from
#' `givennames`, `last_name` from `familyname` and `Department` from
#' `address`. A missing or `NULL` field becomes `NA`.
build_neotoma_author_table <- function(
  author_id,
  contact_info
) {
  assertthat::assert_that(
    base::is.character(author_id),
    base::length(author_id) == 1L,
    base::isTRUE(
      stringr::str_detect(
        string = author_id,
        pattern = "^[0-9]+_neotoma$"
      )
    ),
    msg = stringr::str_c(
      "'author_id' must be a single value of the form ",
      "'<contactid>_neotoma'."
    )
  )

  assertthat::assert_that(
    base::is.list(contact_info),
    msg = "'contact_info' must be a list with one Neotoma contact record."
  )

  # Output column -> field of the Neotoma contact record
  vec_fields <-
    c(
      first_name = "givennames",
      last_name = "familyname",
      email = "email",
      Department = "address",
      url = "url"
    )

  list_values <-
    purrr::map(
      .x = vec_fields,
      .f = ~ purrr::pluck(
        contact_info,
        .x,
        .default = NA_character_
      ) |>
        base::as.character()
    )

  res <-
    tibble::tibble(
      author_id = author_id,
      !!!list_values
    )

  return(res)
}
