data_fixture <-
  tibble::tibble(
    dataset_id = base::as.character(base::seq_len(5L)),
    age_uncertainty = purrr::map(
      .x = base::seq_len(5L),
      .f = ~ base::matrix(
        data = base::seq_len(10L * .x),
        nrow = 10L
      )
    )
  )

list_parts_valid <-
  base::list(
    A = dplyr::slice(data_fixture, 1:3),
    B = dplyr::slice(data_fixture, 4:5)
  )


#----------------------------------------------------------#
# 1. Inputs -----
#----------------------------------------------------------#

testthat::test_that("validate_export_parts() validates list_parts", {
  list_invalid_parts <-
    base::list(
      "a",
      data_fixture,
      base::list(),
      base::list(A = 1)
    )

  purrr::walk(
    .x = list_invalid_parts,
    .f = ~ testthat::expect_error(
      validate_export_parts(
        list_parts = .x,
        data_reference = data_fixture,
        id_column = "dataset_id"
      ),
      regexp = "'list_parts' must be a non-empty list of data frames"
    )
  )
})

testthat::test_that("validate_export_parts() validates data_reference", {
  testthat::expect_error(
    validate_export_parts(
      list_parts = list_parts_valid,
      data_reference = base::list(dataset_id = "1"),
      id_column = "dataset_id"
    ),
    regexp = "'data_reference' must be a data frame"
  )
})

testthat::test_that("validate_export_parts() validates id_column", {
  list_invalid_ids <-
    base::list(1, c("dataset_id", "age_uncertainty"), NA_character_)

  purrr::walk(
    .x = list_invalid_ids,
    .f = ~ testthat::expect_error(
      validate_export_parts(
        list_parts = list_parts_valid,
        data_reference = data_fixture,
        id_column = .x
      ),
      regexp = "'id_column' must be a single character value"
    )
  )

  testthat::expect_error(
    validate_export_parts(
      list_parts = list_parts_valid,
      data_reference = data_fixture,
      id_column = "site_id"
    ),
    regexp = "'site_id' must be a column"
  )
})


#----------------------------------------------------------#
# 2. Outputs -----
#----------------------------------------------------------#

testthat::test_that("validate_export_parts() returns valid parts invisibly", {
  testthat::expect_invisible(
    validate_export_parts(
      list_parts = list_parts_valid,
      data_reference = data_fixture,
      id_column = "dataset_id"
    )
  )

  res <-
    validate_export_parts(
      list_parts = list_parts_valid,
      data_reference = data_fixture,
      id_column = "dataset_id"
    )

  testthat::expect_identical(res, list_parts_valid)
})

testthat::test_that("validate_export_parts() accepts a single part", {
  res <-
    validate_export_parts(
      list_parts = base::list(A = data_fixture),
      data_reference = data_fixture,
      id_column = "dataset_id"
    )

  testthat::expect_identical(res, base::list(A = data_fixture))
})


#----------------------------------------------------------#
# 3. Functionality -----
#----------------------------------------------------------#

testthat::test_that("validate_export_parts() aborts on a missing row", {
  testthat::expect_error(
    validate_export_parts(
      list_parts = base::list(
        A = dplyr::slice(data_fixture, 1:3),
        B = dplyr::slice(data_fixture, 4L)
      ),
      data_reference = data_fixture,
      id_column = "dataset_id"
    ),
    regexp = "The parts hold 4 rows, but the reference has 5"
  )
})

testthat::test_that("validate_export_parts() aborts on a stale extra part", {
  testthat::expect_error(
    validate_export_parts(
      list_parts = base::list(
        A = dplyr::slice(data_fixture, 1:3),
        B = dplyr::slice(data_fixture, 4:5),
        C = dplyr::slice(data_fixture, 1L)
      ),
      data_reference = data_fixture,
      id_column = "dataset_id"
    ),
    regexp = "The parts hold 6 rows, but the reference has 5"
  )
})

testthat::test_that("validate_export_parts() aborts on an id in two parts", {
  testthat::expect_error(
    validate_export_parts(
      list_parts = base::list(
        A = dplyr::slice(data_fixture, 1:3),
        B = dplyr::slice(data_fixture, c(3L, 5L))
      ),
      data_reference = data_fixture,
      id_column = "dataset_id"
    ),
    regexp = "more than once"
  )
})

testthat::test_that("validate_export_parts() aborts on an unknown id", {
  data_row_unknown <-
    dplyr::slice(data_fixture, 5L) |>
    dplyr::mutate(dataset_id = "99")

  testthat::expect_error(
    validate_export_parts(
      list_parts = base::list(
        A = dplyr::slice(data_fixture, 1:3),
        B = dplyr::bind_rows(
          dplyr::slice(data_fixture, 4L),
          data_row_unknown
        )
      ),
      data_reference = data_fixture,
      id_column = "dataset_id"
    ),
    regexp = "differ from the reference"
  )
})

testthat::test_that("validate_export_parts() aborts on reordered rows", {
  testthat::expect_error(
    validate_export_parts(
      list_parts = base::list(
        A = dplyr::slice(data_fixture, c(2L, 1L, 3L)),
        B = dplyr::slice(data_fixture, 4:5)
      ),
      data_reference = data_fixture,
      id_column = "dataset_id"
    ),
    regexp = "not in the order"
  )
})

testthat::test_that("validate_export_parts() aborts on changed values", {
  data_row_changed <-
    dplyr::slice(data_fixture, 5L) |>
    dplyr::mutate(
      age_uncertainty = base::list(base::matrix(0L))
    )

  testthat::expect_error(
    validate_export_parts(
      list_parts = base::list(
        A = dplyr::slice(data_fixture, 1:3),
        B = dplyr::bind_rows(
          dplyr::slice(data_fixture, 4L),
          data_row_changed
        )
      ),
      data_reference = data_fixture,
      id_column = "dataset_id"
    ),
    regexp = "not identical"
  )
})
