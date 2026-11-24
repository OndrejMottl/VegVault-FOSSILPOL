#' @title Save Neotoma author files
#' @description
#' Download the contact records of Neotoma dataset authors that have no file
#' yet and save each as an author table that `RFossilpol` can read.
#' @param vec_author_ids
#' A non-empty character vector of author identifiers of the form
#' `"<contactid>_neotoma"`, without missing values.
#' @param dir
#' A single path to an existing directory that holds one file per author.
#' Use forward slashes: `RUtilpol::save_latest_file()` cannot save to a path
#' written with backslashes.
#' @param url_contacts
#' A single URL of the Neotoma contacts endpoint, ending with `/`. The
#' numeric contact ID is appended to it.
#' @param verbose
#' Logical. If `TRUE` (default), progress messages are printed to
#' the console via `cli`.
#' @return
#' Invisibly, a character vector of the author identifiers that were
#' downloaded and saved. It is empty when every author already has a file.
#' @details
#' An author has a file when a file name in `dir`, without its date and hash
#' (`RUtilpol::get_clean_name()`), equals its identifier. Only authors without
#' a file are requested, with the numeric `contactid` alone, because the
#' Neotoma API rejects the `"_neotoma"` suffix that
#' `RFossilpol::proc_neo_get_authors()` sends. `resolve_neotoma_contact()`
#' decides what each response yields. A contact record is turned into a table
#' by `build_neotoma_author_table()` and saved with
#' `RUtilpol::save_latest_file()` as `.rds` under the author identifier. When
#' Neotoma holds no contact record for the ID (status 200 with empty `data`),
#' the saved table has the identifier and `NA` in every other column, as
#' `RFossilpol` saved it. Requests that fail save nothing. After all
#' requests, the function aborts and names the authors that could not be
#' saved; the files already saved are kept, so a rerun requests only the
#' rest.
save_neotoma_authors <- function(
  vec_author_ids,
  dir,
  url_contacts = "https://api.neotomadb.org/v2.0/data/contacts/",
  verbose = TRUE
) {
  assertthat::assert_that(
    base::is.character(vec_author_ids),
    base::length(vec_author_ids) > 0L,
    !base::anyNA(vec_author_ids),
    base::all(
      stringr::str_detect(
        string = vec_author_ids,
        pattern = "^[0-9]+_neotoma$"
      )
    ),
    msg = stringr::str_c(
      "'vec_author_ids' must be a non-empty character vector of values ",
      "of the form '<contactid>_neotoma', without missing values."
    )
  )

  assertthat::assert_that(
    base::is.character(dir),
    base::length(dir) == 1L,
    !base::is.na(dir),
    base::isTRUE(base::dir.exists(dir)),
    msg = "'dir' must be a single path to an existing directory."
  )

  assertthat::assert_that(
    base::is.character(url_contacts),
    base::length(url_contacts) == 1L,
    !base::is.na(url_contacts),
    msg = "'url_contacts' must be a single character value."
  )

  assertthat::assert_that(
    base::is.logical(verbose),
    base::length(verbose) == 1L,
    !base::is.na(verbose),
    msg = "'verbose' must be TRUE or FALSE."
  )

  vec_file_names <-
    base::list.files(path = dir)

  vec_author_ids_present <-
    if (
      base::length(vec_file_names) == 0L
    ) {
      base::character(0)
    } else {
      RUtilpol::get_clean_name(vec_file_names)
    }

  vec_author_ids_missing <-
    base::setdiff(
      base::unique(vec_author_ids),
      vec_author_ids_present
    )

  if (
    base::length(vec_author_ids_missing) == 0L
  ) {
    if (
      isTRUE(verbose)
    ) {
      cli::cli_inform(
        "All {base::length(base::unique(vec_author_ids))} authors have a file."
      )
    }

    res <-
      base::character(0)

    return(base::invisible(res))
  }

  if (
    isTRUE(verbose)
  ) {
    cli::cli_inform(
      "Requesting {base::length(vec_author_ids_missing)} authors from Neotoma."
    )
  }

  # The API accepts only the numeric contact ID, without the "_neotoma"
  #   suffix of the author identifier
  vec_urls <-
    stringr::str_c(
      url_contacts,
      stringr::str_remove(
        string = vec_author_ids_missing,
        pattern = "_neotoma$"
      )
    )

  # A request that cannot connect counts as failed, like a non-200 status
  request_contact <-
    purrr::possibly(
      .f = httr::GET,
      otherwise = NULL
    )

  list_responses <-
    purrr::map(
      .x = vec_urls,
      .f = ~ request_contact(
        url = .x,
        config = httr::timeout(60)
      ),
      .progress = verbose
    )

  list_contacts <-
    purrr::map(
      .x = list_responses,
      .f = ~ if (
        base::is.null(.x)
      ) {
        resolve_neotoma_contact(
          status_code = NA_real_,
          response_content = NULL
        )
      } else {
        resolve_neotoma_contact(
          status_code = httr::status_code(.x),
          response_content = httr::content(.x)
        )
      }
    )

  vec_is_received <-
    purrr::map_lgl(
      .x = list_contacts,
      .f = ~ base::is.list(.x)
    )

  vec_author_ids_without_record <-
    vec_author_ids_missing[
      purrr::map_lgl(
        .x = list_contacts,
        .f = ~ base::is.list(.x) && base::length(.x) == 0L
      )
    ]

  if (
    isTRUE(verbose) && base::length(vec_author_ids_without_record) > 0L
  ) {
    cli::cli_inform(
      "No Neotoma contact record, saved with the identifier only:
      {.val {vec_author_ids_without_record}}."
    )
  }

  vec_author_ids_received <-
    vec_author_ids_missing[vec_is_received]

  purrr::walk2(
    .x = vec_author_ids_received,
    .y = list_contacts[vec_is_received],
    .f = ~ RUtilpol::save_latest_file(
      object_to_save = build_neotoma_author_table(
        author_id = .x,
        contact_info = .y
      ),
      file_name = .x,
      dir = dir,
      prefered_format = "rds",
      use_sha = TRUE,
      verbose = FALSE
    )
  )

  vec_author_ids_failed <-
    vec_author_ids_missing[!vec_is_received]

  if (
    base::length(vec_author_ids_failed) > 0L
  ) {
    cli::cli_abort(
      c(
        "Authors not saved: {.val {vec_author_ids_failed}}.",
        "i" = "Saved {base::length(vec_author_ids_received)} other authors;
        rerun to request only the authors still missing."
      )
    )
  }

  if (
    isTRUE(verbose)
  ) {
    cli::cli_inform(
      "Saved {base::length(vec_author_ids_received)} authors."
    )
  }

  res <- vec_author_ids_received

  return(base::invisible(res))
}
