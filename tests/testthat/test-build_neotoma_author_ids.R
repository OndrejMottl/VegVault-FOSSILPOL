list_download_fixture <-
  list(
    "11" = list(
      site = list(
        collectionunit = list(
          dataset = list(
            datasetpi = list(
              list(contactid = 86L),
              list(contactid = 5L)
            )
          )
        )
      )
    ),
    "12" = list(
      site = list(
        collectionunit = list(
          dataset = list(
            datasetpi = list(
              list(contactid = 86L)
            )
          )
        )
      )
    ),
    "13" = list(
      site = list(
        collectionunit = list(
          dataset = list(
            datasetpi = list(
              list(contactid = 7L),
              list(familyname = "Without identifier")
            )
          )
        )
      )
    ),
    "14" = list(
      site = list(
        collectionunit = list(
          dataset = list(
            datasetpi = list()
          )
        )
      )
    )
  )

testthat::test_that("build_neotoma_author_ids() validates neotoma_download", {
  testthat::expect_error(
    build_neotoma_author_ids(
      neotoma_download = "11",
      dataset_ids = "11"
    ),
    regexp = "'neotoma_download' must be a named list"
  )

  testthat::expect_error(
    build_neotoma_author_ids(
      neotoma_download = base::unname(list_download_fixture),
      dataset_ids = "11"
    ),
    regexp = "'neotoma_download' must be a named list"
  )
})

testthat::test_that("build_neotoma_author_ids() validates dataset_ids", {
  testthat::expect_error(
    build_neotoma_author_ids(
      neotoma_download = list_download_fixture,
      dataset_ids = 11
    ),
    regexp = "'dataset_ids' must be a character vector"
  )

  testthat::expect_error(
    build_neotoma_author_ids(
      neotoma_download = list_download_fixture,
      dataset_ids = c("11", NA_character_)
    ),
    regexp = "'dataset_ids' must be a character vector"
  )
})

testthat::test_that("build_neotoma_author_ids() returns unique sorted ids", {
  res <-
    build_neotoma_author_ids(
      neotoma_download = list_download_fixture,
      dataset_ids = c("12", "11")
    )

  testthat::expect_type(res, "character")
  testthat::expect_equal(res, c("5_neotoma", "86_neotoma"))
})

testthat::test_that("build_neotoma_author_ids() uses only selected records", {
  res <-
    build_neotoma_author_ids(
      neotoma_download = list_download_fixture,
      dataset_ids = "12"
    )

  testthat::expect_equal(res, "86_neotoma")
})

testthat::test_that("build_neotoma_author_ids() ignores absent dataset ids", {
  res <-
    build_neotoma_author_ids(
      neotoma_download = list_download_fixture,
      dataset_ids = c("12", "99")
    )

  testthat::expect_equal(res, "86_neotoma")
})

testthat::test_that("build_neotoma_author_ids() skips authors without id", {
  res <-
    build_neotoma_author_ids(
      neotoma_download = list_download_fixture,
      dataset_ids = "13"
    )

  testthat::expect_equal(res, "7_neotoma")
})

testthat::test_that("build_neotoma_author_ids() returns empty if no authors", {
  res <-
    build_neotoma_author_ids(
      neotoma_download = list_download_fixture,
      dataset_ids = "14"
    )

  testthat::expect_type(res, "character")
  testthat::expect_length(res, 0L)
})
