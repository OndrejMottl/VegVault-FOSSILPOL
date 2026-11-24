#' @title Validate export parts against their source
#' @description
#' Check that a set of parts adds up exactly to the data frame they were
#' split from, and abort when it does not.
#' @param list_parts
#' A non-empty list of data frames, in the order in which they are bound.
#' @param data_reference
#' A data frame. The object the parts were split from.
#' @param id_column
#' A single character value. The name of the identifier column, which must
#' be present in `data_reference` and in every part.
#' @return
#' `list_parts`, invisibly, when every check passes.
#' @details
#' The checks run in this order, and the first one that fails aborts with
#' an informative error:
#' 1. The number of rows over all parts equals the number of rows of
#'    `data_reference`.
#' 2. No identifier appears more than once over all parts.
#' 3. The set of identifiers over all parts equals the set in
#'    `data_reference`.
#' 4. The identifiers over all parts, in part order, are in the same order
#'    as in `data_reference`.
#' 5. The parts bound in order are identical to `data_reference`, including
#'    nested list-columns.
validate_export_parts <- function(
  list_parts,
  data_reference,
  id_column = "dataset_id"
) {
  assertthat::assert_that(
    base::is.list(list_parts),
    !base::is.data.frame(list_parts),
    base::length(list_parts) > 0L,
    base::all(purrr::map_lgl(list_parts, base::is.data.frame)),
    msg = "'list_parts' must be a non-empty list of data frames."
  )

  assertthat::assert_that(
    base::is.data.frame(data_reference),
    msg = "'data_reference' must be a data frame."
  )

  assertthat::assert_that(
    assertthat::is.string(id_column),
    !base::is.na(id_column),
    msg = "'id_column' must be a single character value."
  )

  assertthat::assert_that(
    id_column %in% base::names(data_reference),
    base::all(
      purrr::map_lgl(
        .x = list_parts,
        .f = ~ id_column %in% base::names(.x)
      )
    ),
    msg = stringr::str_glue(
      "'{id_column}' must be a column of 'data_reference' and of every part."
    )
  )

  n_rows_parts <-
    purrr::map_int(list_parts, base::nrow) |>
    base::sum()

  n_rows_reference <-
    base::nrow(data_reference)

  if (
    n_rows_parts != n_rows_reference
  ) {
    cli::cli_abort(
      c(
        "The parts hold {n_rows_parts} rows, but the reference has
        {n_rows_reference}.",
        "i" = "Look for a missing part or a part left from an earlier run."
      )
    )
  }

  vec_ids_parts <-
    purrr::map(
      .x = list_parts,
      .f = ~ .x[[id_column]]
    ) |>
    purrr::list_c()

  vec_ids_reference <-
    data_reference[[id_column]]

  vec_ids_duplicated <-
    vec_ids_parts[base::duplicated(vec_ids_parts)] |>
    base::unique()

  if (
    base::length(vec_ids_duplicated) > 0L
  ) {
    cli::cli_abort(
      c(
        "Values of {.field {id_column}} found more than once in the parts:
        {.val {vec_ids_duplicated}}."
      )
    )
  }

  vec_ids_missing <-
    base::setdiff(vec_ids_reference, vec_ids_parts)

  vec_ids_unknown <-
    base::setdiff(vec_ids_parts, vec_ids_reference)

  if (
    base::length(vec_ids_missing) > 0L ||
      base::length(vec_ids_unknown) > 0L
  ) {
    cli::cli_abort(
      c(
        "The {.field {id_column}} values of the parts differ from the
        reference.",
        "x" = "Missing from the parts: {.val {vec_ids_missing}}.",
        "x" = "Not in the reference: {.val {vec_ids_unknown}}."
      )
    )
  }

  if (
    !base::identical(vec_ids_parts, vec_ids_reference)
  ) {
    cli::cli_abort(
      "The rows of the parts are not in the order of the reference."
    )
  }

  data_parts_bound <-
    dplyr::bind_rows(list_parts)

  if (
    !base::identical(data_parts_bound, data_reference)
  ) {
    cli::cli_abort(
      c(
        "The bound parts are not identical to the reference.",
        "i" = "Identifiers and row order agree, so values or attributes
        differ."
      )
    )
  }

  res <-
    list_parts

  return(invisible(res))
}
