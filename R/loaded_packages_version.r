#' Get a list of currently loaded packages and their versions
#'
#' This function retrieves the names and versions of all currently loaded R packages.
#' It returns a data frame with the package names in the first column and their respective
#' versions in the second column.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{package}{The name of the loaded package.}
#'   \item{version}{The version of the loaded package.}
#' }
#' @examples
#' # Get the list of loaded packages with versions
#' loaded_packages_version()
#'
#' @export

loaded_packages_version <- function() {
  # Get the names of currently loaded namespaces
  loaded_packages <- loadedNamespaces()

  # Create a data frame with package names and their respective versions
  package_versions <- data.frame(
    package = loaded_packages,
    version = sapply(loaded_packages, function(pkg) as.character(packageVersion(pkg)))
  )

  # Return the result
  return(package_versions)
}
