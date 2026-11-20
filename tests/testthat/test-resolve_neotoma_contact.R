list_contact_fixture <-
  list(
    contactid = 101L,
    familyname = "Example",
    givennames = "Ada"
  )

list_content_with_record <-
  list(
    status = "success",
    data = list(list_contact_fixture),
    message = "Retrieved all tables"
  )

list_content_empty <-
  list(
    status = "success",
    data = list(),
    message = "Retrieved all tables"
  )

testthat::test_that("resolve_neotoma_contact() validates status_code", {
  testthat::expect_error(
    resolve_neotoma_contact(
      status_code = "200",
      response_content = list_content_with_record
    ),
    regexp = "'status_code' must be a single number"
  )

  testthat::expect_error(
    resolve_neotoma_contact(
      status_code = c(200, 200),
      response_content = list_content_with_record
    ),
    regexp = "'status_code' must be a single number"
  )
})

testthat::test_that("resolve_neotoma_contact() validates response_content", {
  testthat::expect_error(
    resolve_neotoma_contact(
      status_code = 200,
      response_content = "success"
    ),
    regexp = "'response_content' must be a list or NULL"
  )
})

testthat::test_that("resolve_neotoma_contact() returns the contact record", {
  res <-
    resolve_neotoma_contact(
      status_code = 200,
      response_content = list_content_with_record
    )

  testthat::expect_equal(res, list_contact_fixture)
})

testthat::test_that("resolve_neotoma_contact() keeps an ID without a record", {
  res <-
    resolve_neotoma_contact(
      status_code = 200,
      response_content = list_content_empty
    )

  testthat::expect_type(res, "list")
  testthat::expect_length(res, 0L)
})

testthat::test_that("resolve_neotoma_contact() fails on a non-200 status", {
  res <-
    resolve_neotoma_contact(
      status_code = 400,
      response_content = list(
        status = "failure",
        data = NULL,
        message = "contactid must be an integer."
      )
    )

  testthat::expect_null(res)
})

testthat::test_that("resolve_neotoma_contact() fails without a response", {
  res <-
    resolve_neotoma_contact(
      status_code = NA_real_,
      response_content = NULL
    )

  testthat::expect_null(res)
})

testthat::test_that("resolve_neotoma_contact() fails without a data list", {
  res <-
    resolve_neotoma_contact(
      status_code = 200,
      response_content = list(status = "success")
    )

  testthat::expect_null(res)
})
