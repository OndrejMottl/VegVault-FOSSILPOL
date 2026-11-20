list_contact_fixture <-
  list(
    contactid = 101L,
    contactname = "Example, Ada",
    familyname = "Example",
    givennames = "Ada",
    email = "ada@example.org",
    address = "Department of Examples",
    url = "https://example.org"
  )

testthat::test_that("build_neotoma_author_table() validates author_id", {
  testthat::expect_error(
    build_neotoma_author_table(
      author_id = 101L,
      contact_info = list_contact_fixture
    ),
    regexp = "'author_id' must be a single value of the form"
  )

  testthat::expect_error(
    build_neotoma_author_table(
      author_id = c("101_neotoma", "102_neotoma"),
      contact_info = list_contact_fixture
    ),
    regexp = "'author_id' must be a single value of the form"
  )

  testthat::expect_error(
    build_neotoma_author_table(
      author_id = "101",
      contact_info = list_contact_fixture
    ),
    regexp = "'author_id' must be a single value of the form"
  )
})

testthat::test_that("build_neotoma_author_table() validates contact_info", {
  testthat::expect_error(
    build_neotoma_author_table(
      author_id = "101_neotoma",
      contact_info = "Example, Ada"
    ),
    regexp = "'contact_info' must be a list"
  )
})

testthat::test_that("build_neotoma_author_table() returns one-row tibble", {
  res <-
    build_neotoma_author_table(
      author_id = "101_neotoma",
      contact_info = list_contact_fixture
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_named(
    res,
    c("author_id", "first_name", "last_name", "email", "Department", "url")
  )
})

testthat::test_that("build_neotoma_author_table() maps the contact fields", {
  res <-
    build_neotoma_author_table(
      author_id = "101_neotoma",
      contact_info = list_contact_fixture
    )

  testthat::expect_equal(dplyr::pull(res, author_id), "101_neotoma")
  testthat::expect_equal(dplyr::pull(res, first_name), "Ada")
  testthat::expect_equal(dplyr::pull(res, last_name), "Example")
  testthat::expect_equal(dplyr::pull(res, email), "ada@example.org")
  testthat::expect_equal(
    dplyr::pull(res, Department),
    "Department of Examples"
  )
  testthat::expect_equal(dplyr::pull(res, url), "https://example.org")
})

testthat::test_that("build_neotoma_author_table() turns NULL fields into NA", {
  list_contact_sparse <-
    list(
      contactid = 101L,
      familyname = "Example",
      givennames = NULL,
      email = NULL
    )

  res <-
    build_neotoma_author_table(
      author_id = "101_neotoma",
      contact_info = list_contact_sparse
    )

  testthat::expect_equal(dplyr::pull(res, last_name), "Example")
  testthat::expect_equal(dplyr::pull(res, first_name), NA_character_)
  testthat::expect_equal(dplyr::pull(res, email), NA_character_)
  testthat::expect_equal(dplyr::pull(res, Department), NA_character_)
  testthat::expect_equal(dplyr::pull(res, url), NA_character_)
})

testthat::test_that("build_neotoma_author_table() accepts an empty record", {
  res <-
    build_neotoma_author_table(
      author_id = "101_neotoma",
      contact_info = base::list()
    )

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, author_id), "101_neotoma")
  testthat::expect_equal(dplyr::pull(res, first_name), NA_character_)
  testthat::expect_equal(dplyr::pull(res, last_name), NA_character_)
  testthat::expect_equal(dplyr::pull(res, email), NA_character_)
})
