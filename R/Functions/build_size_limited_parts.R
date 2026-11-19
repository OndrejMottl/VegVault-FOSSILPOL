#' @title Build size-limited parts of a data frame
#' @description
#' Split a data frame into consecutive parts whose estimated `qs2` size
#' does not exceed a limit, keeping the row order.
#' @param data_source
#' A data frame with at least one row.
#' @param max_part_bytes
#' A single positive finite number. The largest estimated size of one part,
#' in bytes.
#' @return
#' A named list of data frames. The names are consecutive capital letters
#' starting at `"A"`. Every part has the columns of `data_source`, and
#' binding the parts in name order reproduces `data_source`.
#' @details
#' The size of a row is the length of its serialisation by
#' `qs2::qs_serialize()` with the default settings, which are the settings
#' `RUtilpol::save_latest_file()` uses to write `.qs` files. Rows are
#' assigned to parts in their original order. A new part starts when the
#' next row would raise the size of the current part above
#' `max_part_bytes`, so a part of exactly `max_part_bytes` is kept whole.
#' The function aborts when `data_source` has no rows, when a single row is
#' larger than `max_part_bytes`, or when more than 26 parts are needed.
build_size_limited_parts <- function(
  data_source,
  max_part_bytes
) {
  assertthat::assert_that(
    base::is.data.frame(data_source),
    msg = "'data_source' must be a data frame."
  )

  assertthat::assert_that(
    base::nrow(data_source) > 0L,
    msg = "'data_source' must have at least one row."
  )

  assertthat::assert_that(
    base::is.numeric(max_part_bytes),
    base::length(max_part_bytes) == 1L,
    base::isTRUE(base::is.finite(max_part_bytes)),
    base::isTRUE(max_part_bytes > 0),
    msg = "'max_part_bytes' must be a single positive finite number."
  )

  vec_row_bytes <-
    purrr::map_dbl(
      .x = base::seq_len(base::nrow(data_source)),
      .f = ~ data_source[.x, , drop = FALSE] |>
        qs2::qs_serialize() |>
        base::length()
    )

  vec_rows_oversized <-
    base::which(vec_row_bytes > max_part_bytes)

  if (
    base::length(vec_rows_oversized) > 0L
  ) {
    cli::cli_abort(
      c(
        "Rows of {.arg data_source} larger than {.arg max_part_bytes}
        ({max_part_bytes} bytes): {vec_rows_oversized}.",
        "i" = "Such a row cannot fit into any part; raise the limit."
      )
    )
  }

  # Running size of the current part: it restarts at the row's own size
  #   whenever adding the row would exceed the limit. Row sizes are
  #   positive, so the running size equals the row size exactly at the
  #   first row of each part.
  vec_running_bytes <-
    purrr::accumulate(
      .x = vec_row_bytes,
      .f = ~ dplyr::if_else(
        .x + .y > max_part_bytes,
        .y,
        .x + .y
      )
    )

  vec_part_index <-
    base::cumsum(vec_running_bytes == vec_row_bytes)

  n_parts <-
    base::max(vec_part_index)

  if (
    n_parts > base::length(base::LETTERS)
  ) {
    cli::cli_abort(
      c(
        "{n_parts} parts are needed, but part names run only from A to Z.",
        "i" = "Raise {.arg max_part_bytes} or extend the part naming."
      )
    )
  }

  res <-
    purrr::map(
      .x = base::seq_len(n_parts),
      .f = ~ data_source[vec_part_index == .x, , drop = FALSE]
    ) |>
    rlang::set_names(base::LETTERS[base::seq_len(n_parts)])

  return(res)
}
