url_unreachable <- "http://127.0.0.1:9/"

testthat::test_that("save_neotoma_authors() validates vec_author_ids", {
  path_dir <-
    withr::local_tempdir() |>
    base::normalizePath(winslash = "/")

  testthat::expect_error(
    save_neotoma_authors(
      vec_author_ids = 101L,
      dir = path_dir,
      url_contacts = url_unreachable
    ),
    regexp = "'vec_author_ids' must be a non-empty character vector"
  )

  testthat::expect_error(
    save_neotoma_authors(
      vec_author_ids = base::character(0),
      dir = path_dir,
      url_contacts = url_unreachable
    ),
    regexp = "'vec_author_ids' must be a non-empty character vector"
  )

  testthat::expect_error(
    save_neotoma_authors(
      vec_author_ids = c("101_neotoma", NA_character_),
      dir = path_dir,
      url_contacts = url_unreachable
    ),
    regexp = "'vec_author_ids' must be a non-empty character vector"
  )

  testthat::expect_error(
    save_neotoma_authors(
      vec_author_ids = c("101_neotoma", "102"),
      dir = path_dir,
      url_contacts = url_unreachable
    ),
    regexp = "'vec_author_ids' must be a non-empty character vector"
  )
})

testthat::test_that("save_neotoma_authors() validates dir", {
  path_dir <-
    withr::local_tempdir() |>
    base::normalizePath(winslash = "/")

  testthat::expect_error(
    save_neotoma_authors(
      vec_author_ids = "101_neotoma",
      dir = base::file.path(path_dir, "missing"),
      url_contacts = url_unreachable
    ),
    regexp = "'dir' must be a single path to an existing directory"
  )

  testthat::expect_error(
    save_neotoma_authors(
      vec_author_ids = "101_neotoma",
      dir = c(path_dir, path_dir),
      url_contacts = url_unreachable
    ),
    regexp = "'dir' must be a single path to an existing directory"
  )
})

testthat::test_that("save_neotoma_authors() validates url and verbose", {
  path_dir <-
    withr::local_tempdir() |>
    base::normalizePath(winslash = "/")

  testthat::expect_error(
    save_neotoma_authors(
      vec_author_ids = "101_neotoma",
      dir = path_dir,
      url_contacts = 1
    ),
    regexp = "'url_contacts' must be a single character value"
  )

  testthat::expect_error(
    save_neotoma_authors(
      vec_author_ids = "101_neotoma",
      dir = path_dir,
      url_contacts = url_unreachable,
      verbose = "yes"
    ),
    regexp = "'verbose' must be TRUE or FALSE"
  )
})

testthat::test_that("save_neotoma_authors() skips authors with a file", {
  path_dir <-
    withr::local_tempdir() |>
    base::normalizePath(winslash = "/")

  RUtilpol::save_latest_file(
    object_to_save = base::data.frame(author_id = "101_neotoma"),
    file_name = "101_neotoma",
    dir = path_dir,
    prefered_format = "rds",
    use_sha = TRUE,
    verbose = FALSE
  )

  testthat::expect_invisible(
    save_neotoma_authors(
      vec_author_ids = "101_neotoma",
      dir = path_dir,
      url_contacts = url_unreachable,
      verbose = FALSE
    )
  )

  res <-
    save_neotoma_authors(
      vec_author_ids = "101_neotoma",
      dir = path_dir,
      url_contacts = url_unreachable,
      verbose = FALSE
    )

  testthat::expect_type(res, "character")
  testthat::expect_length(res, 0L)
  testthat::expect_length(base::list.files(path_dir), 1L)
})

testthat::test_that("save_neotoma_authors() names authors it cannot save", {
  path_dir <-
    withr::local_tempdir() |>
    base::normalizePath(winslash = "/")

  testthat::expect_error(
    save_neotoma_authors(
      vec_author_ids = c("101_neotoma", "102_neotoma"),
      dir = path_dir,
      url_contacts = url_unreachable,
      verbose = FALSE
    ),
    regexp = "101_neotoma.*102_neotoma"
  )

  testthat::expect_length(base::list.files(path_dir), 0L)
})

testthat::test_that("save_neotoma_authors() requests only missing authors", {
  path_dir <-
    withr::local_tempdir() |>
    base::normalizePath(winslash = "/")

  RUtilpol::save_latest_file(
    object_to_save = base::data.frame(author_id = "101_neotoma"),
    file_name = "101_neotoma",
    dir = path_dir,
    prefered_format = "rds",
    use_sha = TRUE,
    verbose = FALSE
  )

  msg_error <-
    base::tryCatch(
      expr = save_neotoma_authors(
        vec_author_ids = c("101_neotoma", "102_neotoma"),
        dir = path_dir,
        url_contacts = url_unreachable,
        verbose = FALSE
      ),
      error = function(e) base::conditionMessage(e)
    )

  testthat::expect_match(msg_error, "102_neotoma")
  testthat::expect_false(base::grepl("101_neotoma", msg_error))
})

testthat::test_that("save_neotoma_authors() reports progress when verbose", {
  path_dir <-
    withr::local_tempdir() |>
    base::normalizePath(winslash = "/")

  RUtilpol::save_latest_file(
    object_to_save = base::data.frame(author_id = "101_neotoma"),
    file_name = "101_neotoma",
    dir = path_dir,
    prefered_format = "rds",
    use_sha = TRUE,
    verbose = FALSE
  )

  testthat::expect_message(
    save_neotoma_authors(
      vec_author_ids = "101_neotoma",
      dir = path_dir,
      url_contacts = url_unreachable,
      verbose = TRUE
    )
  )

  testthat::expect_silent(
    save_neotoma_authors(
      vec_author_ids = "101_neotoma",
      dir = path_dir,
      url_contacts = url_unreachable,
      verbose = FALSE
    )
  )
})
