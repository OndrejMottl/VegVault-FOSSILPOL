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

vec_row_bytes <-
  purrr::map_dbl(
    .x = base::seq_len(base::nrow(data_fixture)),
    .f = ~ data_fixture[.x, ] |>
      qs2::qs_serialize() |>
      base::length()
  )


#----------------------------------------------------------#
# 1. Inputs -----
#----------------------------------------------------------#

testthat::test_that("build_size_limited_parts() validates data_source", {
  testthat::expect_error(
    build_size_limited_parts(
      data_source = base::list(dataset_id = "1"),
      max_part_bytes = 1e6
    ),
    regexp = "'data_source' must be a data frame"
  )
})

testthat::test_that("build_size_limited_parts() validates max_part_bytes", {
  list_invalid_limits <-
    base::list("a", -1, 0, c(1e6, 2e6), NA_real_, Inf, NULL)

  purrr::walk(
    .x = list_invalid_limits,
    .f = ~ testthat::expect_error(
      build_size_limited_parts(
        data_source = data_fixture,
        max_part_bytes = .x
      ),
      regexp = "'max_part_bytes' must be a single positive finite number"
    )
  )
})

testthat::test_that("build_size_limited_parts() aborts on zero rows", {
  testthat::expect_error(
    build_size_limited_parts(
      data_source = dplyr::slice(data_fixture, 0L),
      max_part_bytes = 1e6
    ),
    regexp = "'data_source' must have at least one row"
  )
})


#----------------------------------------------------------#
# 2. Outputs -----
#----------------------------------------------------------#

testthat::test_that("build_size_limited_parts() keeps a small input whole", {
  res <-
    build_size_limited_parts(
      data_source = data_fixture,
      max_part_bytes = base::sum(vec_row_bytes) * 2
    )

  testthat::expect_type(res, "list")
  testthat::expect_named(res, "A")
  testthat::expect_identical(res[["A"]], data_fixture)
})

testthat::test_that("build_size_limited_parts() keeps a full part whole", {
  res <-
    build_size_limited_parts(
      data_source = data_fixture,
      max_part_bytes = base::sum(vec_row_bytes)
    )

  testthat::expect_named(res, "A")
  testthat::expect_identical(res[["A"]], data_fixture)
})

testthat::test_that("build_size_limited_parts() splits off one extra row", {
  res <-
    build_size_limited_parts(
      data_source = data_fixture,
      max_part_bytes = base::sum(vec_row_bytes[1:4])
    )

  testthat::expect_named(res, c("A", "B"))
  testthat::expect_identical(res[["A"]], dplyr::slice(data_fixture, 1:4))
  testthat::expect_identical(res[["B"]], dplyr::slice(data_fixture, 5L))
})

testthat::test_that("build_size_limited_parts() names parts by letter", {
  testthat::expect_true(
    base::min(vec_row_bytes) * 2 > base::max(vec_row_bytes)
  )

  res <-
    build_size_limited_parts(
      data_source = data_fixture,
      max_part_bytes = base::max(vec_row_bytes)
    )

  vec_part_ids <-
    purrr::map_chr(
      .x = res,
      .f = ~ dplyr::pull(.x, dataset_id)
    )

  testthat::expect_named(res, c("A", "B", "C", "D", "E"))
  testthat::expect_identical(
    base::unname(vec_part_ids),
    dplyr::pull(data_fixture, dataset_id)
  )
})


#----------------------------------------------------------#
# 3. Functionality -----
#----------------------------------------------------------#

testthat::test_that("build_size_limited_parts() keeps rows past 10,000", {
  data_large <-
    tibble::tibble(
      dataset_id = base::as.character(base::seq_len(10050L)),
      age_uncertainty = purrr::map(
        .x = base::seq_len(10050L),
        .f = ~ base::matrix(
          data = .x,
          nrow = 2L,
          ncol = 2L
        )
      )
    )

  n_first_row_bytes <-
    data_large[1, ] |>
    qs2::qs_serialize() |>
    base::length()

  res <-
    build_size_limited_parts(
      data_source = data_large,
      max_part_bytes = n_first_row_bytes * 3000
    )

  data_bound <-
    dplyr::bind_rows(res)

  testthat::expect_gt(base::length(res), 1L)
  testthat::expect_equal(base::nrow(data_bound), 10050L)
  testthat::expect_identical(data_bound, data_large)
})

testthat::test_that("build_size_limited_parts() parts round-trip through qs2", {
  path_dir <-
    withr::local_tempdir()

  res <-
    build_size_limited_parts(
      data_source = data_fixture,
      max_part_bytes = base::sum(vec_row_bytes[1:2])
    )

  vec_paths <-
    base::file.path(
      path_dir,
      stringr::str_c("part_", base::names(res), ".qs")
    )

  purrr::walk2(
    .x = res,
    .y = vec_paths,
    .f = ~ qs2::qs_save(
      object = .x,
      file = .y
    )
  )

  data_read <-
    purrr::map(
      .x = vec_paths,
      .f = ~ qs2::qs_read(file = .x)
    ) |>
    dplyr::bind_rows()

  testthat::expect_gt(base::length(res), 1L)
  testthat::expect_identical(data_read, data_fixture)
})


#----------------------------------------------------------#
# 4. Edge cases -----
#----------------------------------------------------------#

testthat::test_that("build_size_limited_parts() aborts on an oversized row", {
  testthat::expect_error(
    build_size_limited_parts(
      data_source = data_fixture,
      max_part_bytes = base::max(vec_row_bytes) - 1
    ),
    regexp = "larger than"
  )
})

testthat::test_that("build_size_limited_parts() aborts beyond 26 parts", {
  data_many <-
    tibble::tibble(
      dataset_id = base::as.character(base::seq_len(27L)),
      age_uncertainty = purrr::map(
        .x = base::seq_len(27L),
        .f = ~ base::matrix(
          data = .x,
          nrow = 2L,
          ncol = 2L
        )
      )
    )

  vec_many_row_bytes <-
    purrr::map_dbl(
      .x = base::seq_len(27L),
      .f = ~ data_many[.x, ] |>
        qs2::qs_serialize() |>
        base::length()
    )

  testthat::expect_true(
    base::min(vec_many_row_bytes) * 2 > base::max(vec_many_row_bytes)
  )

  testthat::expect_error(
    build_size_limited_parts(
      data_source = data_many,
      max_part_bytes = base::max(vec_many_row_bytes)
    ),
    regexp = "A to Z"
  )
})
