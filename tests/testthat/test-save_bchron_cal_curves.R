vec_curve_names <-
  c(
    "calmixed",
    "nh_zone_1",
    "nh_zone_2",
    "nh_zone_3",
    "sh_zone_1_2",
    "sh_zone_3"
  )

testthat::test_that("save_bchron_cal_curves() validates dir", {
  dir_curves <-
    withr::local_tempdir() |>
    base::normalizePath(winslash = "/")

  testthat::expect_error(
    save_bchron_cal_curves(
      dir = 1,
      verbose = FALSE
    ),
    regexp = "'dir' must be a single path to an existing directory"
  )

  testthat::expect_error(
    save_bchron_cal_curves(
      dir = c(dir_curves, dir_curves),
      verbose = FALSE
    ),
    regexp = "'dir' must be a single path to an existing directory"
  )

  testthat::expect_error(
    save_bchron_cal_curves(
      dir = NA_character_,
      verbose = FALSE
    ),
    regexp = "'dir' must be a single path to an existing directory"
  )

  testthat::expect_error(
    save_bchron_cal_curves(
      dir = base::file.path(dir_curves, "missing"),
      verbose = FALSE
    ),
    regexp = "'dir' must be a single path to an existing directory"
  )
})

testthat::test_that("save_bchron_cal_curves() validates the proportion", {
  dir_curves <-
    withr::local_tempdir() |>
    base::normalizePath(winslash = "/")

  for (value in base::list("0.5", c(0.2, 0.8), 1.5, -0.1, NA_real_)) {
    testthat::expect_error(
      save_bchron_cal_curves(
        dir = dir_curves,
        first_curve_proportion = value,
        verbose = FALSE
      ),
      regexp = "'first_curve_proportion' must be a single number from 0 to 1"
    )
  }
})

testthat::test_that("save_bchron_cal_curves() validates verbose", {
  dir_curves <-
    withr::local_tempdir() |>
    base::normalizePath(winslash = "/")

  testthat::expect_error(
    save_bchron_cal_curves(
      dir = dir_curves,
      verbose = "yes"
    ),
    regexp = "'verbose' must be TRUE or FALSE"
  )

  testthat::expect_error(
    save_bchron_cal_curves(
      dir = dir_curves,
      verbose = NA
    ),
    regexp = "'verbose' must be TRUE or FALSE"
  )
})

testthat::test_that("save_bchron_cal_curves() saves the six curve files", {
  dir_curves <-
    withr::local_tempdir() |>
    base::normalizePath(winslash = "/")

  save_bchron_cal_curves(
    dir = dir_curves,
    verbose = FALSE
  )

  testthat::expect_equal(
    base::sort(base::list.files(dir_curves)),
    base::sort(base::paste0(vec_curve_names, ".rda"))
  )
})

testthat::test_that("save_bchron_cal_curves() returns the paths invisibly", {
  dir_curves <-
    withr::local_tempdir() |>
    base::normalizePath(winslash = "/")

  testthat::expect_invisible(
    save_bchron_cal_curves(
      dir = dir_curves,
      verbose = FALSE
    )
  )

  res <-
    save_bchron_cal_curves(
      dir = dir_curves,
      verbose = FALSE
    )

  testthat::expect_type(res, "character")
  testthat::expect_named(res, vec_curve_names)
  testthat::expect_equal(
    base::unname(res),
    base::file.path(dir_curves, base::paste0(vec_curve_names, ".rda"))
  )
})

testthat::test_that("save_bchron_cal_curves() saves curves Bchron can read", {
  dir_curves <-
    withr::local_tempdir() |>
    base::normalizePath(winslash = "/")

  save_bchron_cal_curves(
    dir = dir_curves,
    verbose = FALSE
  )

  res_mixed <-
    Bchron::BchronCalibrate(
      ages = 3000,
      ageSds = 50,
      calCurves = "calmixed",
      pathToCalCurves = dir_curves
    )

  testthat::expect_s3_class(res_mixed, "BchronCalibratedDates")

  vec_postbomb_names <- vec_curve_names[-1]

  res_postbomb <-
    Bchron::BchronCalibrate(
      ages = base::rep(-500, base::length(vec_postbomb_names)),
      ageSds = base::rep(20, base::length(vec_postbomb_names)),
      calCurves = vec_postbomb_names,
      ids = vec_postbomb_names,
      pathToCalCurves = dir_curves,
      allowOutside = TRUE
    )

  testthat::expect_s3_class(res_postbomb, "BchronCalibratedDates")
  testthat::expect_named(res_postbomb, vec_postbomb_names)
})

testthat::test_that("save_bchron_cal_curves() uses the proportion", {
  dir_low <-
    withr::local_tempdir() |>
    base::normalizePath(winslash = "/")

  dir_high <-
    withr::local_tempdir() |>
    base::normalizePath(winslash = "/")

  save_bchron_cal_curves(
    dir = dir_low,
    first_curve_proportion = 0.2,
    verbose = FALSE
  )

  save_bchron_cal_curves(
    dir = dir_high,
    first_curve_proportion = 0.8,
    verbose = FALSE
  )

  env_low <-
    base::new.env()

  env_high <-
    base::new.env()

  vec_names_low <-
    base::load(
      file = base::file.path(dir_low, "calmixed.rda"),
      envir = env_low
    )

  vec_names_high <-
    base::load(
      file = base::file.path(dir_high, "calmixed.rda"),
      envir = env_high
    )

  mat_low <-
    base::get(vec_names_low, envir = env_low)

  mat_high <-
    base::get(vec_names_high, envir = env_high)

  testthat::expect_equal(base::ncol(mat_low), 3L)
  testthat::expect_false(base::isTRUE(base::all.equal(mat_low, mat_high)))
})

testthat::test_that("save_bchron_cal_curves() replaces existing files", {
  dir_curves <-
    withr::local_tempdir() |>
    base::normalizePath(winslash = "/")

  path_calmixed <-
    base::file.path(dir_curves, "calmixed.rda")

  base::writeLines(
    text = "not a curve",
    con = path_calmixed
  )

  save_bchron_cal_curves(
    dir = dir_curves,
    verbose = FALSE
  )

  env_curve <-
    base::new.env()

  vec_names_curve <-
    base::load(
      file = path_calmixed,
      envir = env_curve
    )

  mat_curve <-
    base::get(vec_names_curve, envir = env_curve)

  testthat::expect_equal(base::ncol(mat_curve), 3L)
})

testthat::test_that("save_bchron_cal_curves() reports only when verbose", {
  dir_curves <-
    withr::local_tempdir() |>
    base::normalizePath(winslash = "/")

  testthat::expect_message(
    save_bchron_cal_curves(
      dir = dir_curves,
      verbose = TRUE
    ),
    regexp = "Saved 6 calibration curves"
  )

  testthat::expect_no_message(
    save_bchron_cal_curves(
      dir = dir_curves,
      verbose = FALSE
    )
  )
})
