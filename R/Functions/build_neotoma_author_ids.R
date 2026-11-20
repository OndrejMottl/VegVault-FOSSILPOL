#' @title Build Neotoma author identifiers
#' @description
#' Collect the identifiers of the dataset authors (principal investigators)
#' of selected Neotoma records, in the form used by `RFossilpol`.
#' @param neotoma_download
#' A named list of downloaded Neotoma records, as returned by
#' `RFossilpol::proc_neo_download_records()`. Names are dataset IDs and each
#' record holds its authors in `site$collectionunit$dataset$datasetpi`.
#' @param dataset_ids
#' A character vector of dataset IDs whose authors are collected.
#' @return
#' A sorted character vector of unique author identifiers of the form
#' `"<contactid>_neotoma"`. It is empty when no selected record has an author.
#' @details
#' The identifiers are those that `RFossilpol::proc_neo_get_authors()` builds
#' and uses as author file names. Dataset IDs that are not names of
#' `neotoma_download` and authors without a `contactid` contribute nothing.
build_neotoma_author_ids <- function(
  neotoma_download,
  dataset_ids
) {
  assertthat::assert_that(
    base::is.list(neotoma_download),
    !base::is.null(base::names(neotoma_download)),
    msg = "'neotoma_download' must be a named list of Neotoma records."
  )

  assertthat::assert_that(
    base::is.character(dataset_ids),
    !base::anyNA(dataset_ids),
    msg = "'dataset_ids' must be a character vector without missing values."
  )

  vec_dataset_ids_present <-
    base::intersect(
      dataset_ids,
      base::names(neotoma_download)
    )

  list_authors <-
    purrr::map(
      .x = vec_dataset_ids_present,
      .f = ~ purrr::pluck(
        neotoma_download,
        .x,
        "site",
        "collectionunit",
        "dataset",
        "datasetpi"
      )
    ) |>
    purrr::list_flatten()

  vec_contact_ids <-
    purrr::map(
      .x = list_authors,
      .f = ~ purrr::pluck(.x, "contactid")
    ) |>
    purrr::compact() |>
    purrr::map_chr(
      .f = ~ base::as.character(.x)
    )

  res <-
    stringr::str_c(vec_contact_ids, "_neotoma") |>
    base::unique() |>
    base::sort()

  return(res)
}
