environment_tests <-
  new.env(parent = globalenv())

vec_function_files <-
  list.files(
    path = file.path("R", "Functions"),
    pattern = "[.]R$",
    recursive = TRUE,
    full.names = TRUE
  )

invisible(
  lapply(
    vec_function_files,
    sys.source,
    envir = environment_tests
  )
)

testthat::test_dir(
  path = file.path("tests", "testthat"),
  reporter = "summary",
  env = environment_tests,
  load_helpers = FALSE
)
