# Package-level environment for once-per-session state (created when the
# namespace loads). Used, for example, to show the check_normality() reading
# guide only once per session.
.ds_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # which_test() is held back (internal, not exported); register its print
  # method so it still dispatches for internal use and tests.
  registerS3method("print", "ds_test_suggestion", print.ds_test_suggestion,
    envir = asNamespace(pkgname))
}
