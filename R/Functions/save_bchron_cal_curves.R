#' @title Save calibration curves for Bchron
#' @description
#' Build the mixed IntCal20 and SHCal20 calibration curve and the five
#' post-bomb calibration curves that the workflow uses, and save them as
#' Bchron curve files in `dir`.
#' @param dir
#' A single path to an existing directory. For age-depth modelling, this is
#' the `data` folder of the installed Bchron package
#' (`base::system.file("data", package = "Bchron")`), from which
#' `Bchron::Bchronology()` reads calibration curves by default.
#' @param first_curve_proportion
#' A single number from 0 to 1, the proportion of IntCal20 in the mixed
#' curve (default 0.5). It is passed to `rcarbon::mixCurves()` as `p`.
#' @param verbose
#' Logical. If `TRUE` (default), progress messages are printed to
#' the console via `cli`.
#' @return
#' Invisibly, a character vector of the paths of the six saved files, named
#' by curve: `calmixed`, `nh_zone_1`, `nh_zone_2`, `nh_zone_3`,
#' `sh_zone_1_2` and `sh_zone_3`.
#' @details
#' The curves are those of `RFossilpol::chron_prepare_cal_curves()`: the
#' mixed curve comes from `rcarbon::mixCurves()` without reservoir offsets,
#' and the post-bomb curves from `IntCal::ccurve()` with `cc` 1 to 5 and
#' `postbomb = TRUE`. Each curve is written by `Bchron::createCalCurve()`,
#' so the file has the format Bchron reads, and then copied to `dir`,
#' replacing a file of the same name. Bchron 4.7.8 writes new curves to
#' `base::tempdir()` whatever `pathToCalCurves` says, which is why
#' `RFossilpol::chron_prepare_cal_curves()` no longer finds them in the
#' Bchron package folder.
save_bchron_cal_curves <- function(
  dir,
  first_curve_proportion = 0.5,
  verbose = TRUE
) {
  assertthat::assert_that(
    base::is.character(dir),
    base::length(dir) == 1L,
    !base::is.na(dir),
    base::isTRUE(base::dir.exists(dir)),
    msg = "'dir' must be a single path to an existing directory."
  )

  assertthat::assert_that(
    base::is.numeric(first_curve_proportion),
    base::length(first_curve_proportion) == 1L,
    !base::is.na(first_curve_proportion),
    first_curve_proportion >= 0,
    first_curve_proportion <= 1,
    msg = "'first_curve_proportion' must be a single number from 0 to 1."
  )

  assertthat::assert_that(
    base::is.logical(verbose),
    base::length(verbose) == 1L,
    !base::is.na(verbose),
    msg = "'verbose' must be TRUE or FALSE."
  )

  mat_calmixed <-
    rcarbon::mixCurves(
      calCurve1 = "intcal20",
      calCurve2 = "shcal20",
      p = first_curve_proportion,
      resOffsets = 0,
      resErrors = 0
    )

  # The post-bomb zones follow the 'cc' codes of IntCal::ccurve(): 1 to 3
  #   for the northern and 4 and 5 for the southern hemisphere
  vec_postbomb_names <-
    c(
      "nh_zone_1",
      "nh_zone_2",
      "nh_zone_3",
      "sh_zone_1_2",
      "sh_zone_3"
    )

  list_postbomb <-
    purrr::map(
      .x = base::seq_along(vec_postbomb_names),
      .f = ~ IntCal::ccurve(
        cc = .x,
        postbomb = TRUE
      )
    ) |>
    purrr::set_names(nm = vec_postbomb_names)

  list_curves <-
    c(
      base::list(calmixed = mat_calmixed),
      list_postbomb
    )

  vec_curve_names <-
    base::names(list_curves)

  # Bchron 4.7.8 writes to tempdir() and older versions to 'pathToCalCurves';
  #   pointing both to tempdir() finds the files with either version. Old
  #   files are removed first, so that a stale curve is never copied.
  path_temp <- base::tempdir()

  vec_files_temp <-
    base::file.path(
      path_temp,
      stringr::str_c(vec_curve_names, ".rda")
    )

  base::unlink(vec_files_temp)

  purrr::iwalk(
    .x = list_curves,
    .f = ~ base::suppressMessages(
      Bchron::createCalCurve(
        name = .y,
        calAges = .x[, 1],
        uncalAges = .x[, 2],
        oneSigma = .x[, 3],
        pathToCalCurves = path_temp
      )
    )
  )

  vec_is_created <-
    base::file.exists(vec_files_temp)

  if (
    !base::all(vec_is_created)
  ) {
    cli::cli_abort(
      c(
        "Bchron did not create the curves
        {.val {vec_curve_names[!vec_is_created]}}.",
        "i" = "Expected the files in {.path {path_temp}}."
      )
    )
  }

  res <-
    base::file.path(
      dir,
      stringr::str_c(vec_curve_names, ".rda")
    ) |>
    purrr::set_names(nm = vec_curve_names)

  vec_is_copied <-
    base::file.copy(
      from = vec_files_temp,
      to = res,
      overwrite = TRUE
    )

  base::unlink(vec_files_temp)

  if (
    !base::all(vec_is_copied)
  ) {
    cli::cli_abort(
      c(
        "Calibration curves not saved:
        {.val {vec_curve_names[!vec_is_copied]}}.",
        "i" = "Check that {.path {dir}} can be written to."
      )
    )
  }

  if (
    isTRUE(verbose)
  ) {
    cli::cli_inform(
      "Saved {base::length(res)} calibration curves to {.path {dir}}."
    )
  }

  return(base::invisible(res))
}
