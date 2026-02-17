#' @title Package startup
#' @description Actions to perform when the package is loaded.
#' @param libname library name
#' @param pkgname package name
#' @keywords internal
#' @details This function is called when the package is loaded. It is used to
#'   set up package options using the `potions` package.
#' @noRd
.onLoad <- function(libname, pkgname) {
  op <- options()
  # Default options for gaseous
  op.gaseous <- list(
    gaseous.gasmet_txt_dir = "data/00_raw/gas_concentration"
  )
  # Remove options already set by user
  op.set <- !names(op.gaseous) %in% names(op)
  # Set default options if not yet specified
  if (any(op.set)) {
    options(op.gaseous[op.set])
  }
  invisible()
}
