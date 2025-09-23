#' Compute step-down critical alpha values using Holm's method
#'
#' Given a vector of p-values, this function computes the per-hypothesis
#' critical alpha thresholds following Holm's step-down procedure
#' (1979). Unlike \code{\link[stats]{p.adjust}}, which returns adjusted
#' p-values, this function returns the maximum allowable alpha for each
#' test under Holm's method.
#'
#' @param p A numeric vector of p-values. Must be within \eqn{[0, 1]}.
#' @param sig.level Overall significance level (family-wise error rate),
#'   a single numeric value between 0 and 1. Default is 0.05.
#' @param na.rm Logical. If \code{TRUE}, missing values in \code{p} are removed
#'   with a warning (unless \code{warn = FALSE}); if \code{FALSE}, the function
#'   stops when missing values are detected. Default is \code{FALSE}.
#' @param warn Logical. If \code{TRUE} (default), warnings are issued for
#'   duplicated p-values or removed NAs. If \code{FALSE}, warnings are
#'   suppressed.
#'
#' @details Holm's method sorts p-values in ascending order, compares them to
#' sequentially adjusted thresholds \eqn{\alpha / (K - i + 1)}, and fixes
#' the subsequent thresholds once the first non-significant result is
#' encountered. This function implements that procedure and restores the
#' results to the original order of \code{p}.
#'
#' @return A numeric vector of the same length as \code{p}, containing the
#' per-hypothesis critical alpha thresholds.
#'
#' @seealso \code{\link[stats]{p.adjust}} for adjusted p-values.
#'
#' @references
#' Holm, S. (1979). A simple sequentially rejective multiple test procedure.
#' \emph{Scandinavian Journal of Statistics}, 6(2), 65–70.
#'
#' @examples
#' p <- c(0.01, 0.04, 0.03, 0.20)
#' p_to_holmalpha(p)
#'
#' @export

p_to_holmalpha <- function(p, sig.level = 0.05, na.rm = FALSE, warn = TRUE) {
  ## ---- Basic checks ----
  if (missing(p)) stop("'p' is missing.")
  if (!is.numeric(p)) stop("'p' must be numeric.")
  if (!is.numeric(sig.level) || length(sig.level) != 1L)
    stop("'sig.level' must be a single numeric value.")
  if (!(sig.level > 0 && sig.level < 1))
    stop("'sig.level' must be in (0, 1).")

  ## Handling of NA
  if (anyNA(p)) {
    if (!na.rm) {
      na_idx <- which(is.na(p))
      stop(sprintf("NA detected in 'p' at positions: %s. Set na.rm=TRUE to drop them.",
                   paste(na_idx, collapse = ", ")))
    } else {
      if (warn) warning("NA values in 'p' were removed (na.rm = TRUE).")
      keep <- !is.na(p)
      p_in  <- p[keep]
      res   <- p_to_holmalpha(p_in, sig.level = sig.level, na.rm = FALSE, warn = warn)
      # Restore positions
      out <- rep(NA_real_, length(p))
      out[keep] <- res
      return(out)
    }
  }

  K <- length(p)
  if (K == 0L) stop("'p' must have length >= 1.")
  if (any(!is.finite(p))) stop("Non-finite values detected in 'p'.")
  if (any(p < 0 | p > 1))
    stop("All 'p' must be in [0, 1].")

  ## ---- Sorting (Holm sorts p in ascending order and applies step-down) ----
  ord <- order(p, decreasing = FALSE)     # Use order(), not rank()
  p_sorted <- p[ord]
  divider  <- rev(seq_len(K))             # K, K-1, ..., 1
  alpha_seq <- sig.level / divider        # Threshold at each step

  ## ---- Determine where the first non-significant result occurs (fix alpha afterward) ----
  sig <- p_sorted < alpha_seq
  first_nonsig_pos <- match(FALSE, sig)   # Position of the first non-significant result (NA if none)

  alpha_out_sorted <- alpha_seq
  if (!is.na(first_nonsig_pos)) {
    # Fix subsequent thresholds at the first non-significant alpha
    alpha_out_sorted[first_nonsig_pos:K] <- alpha_seq[first_nonsig_pos]
  }
  # Restore original order
  alpha_out <- numeric(K)
  alpha_out[ord] <- alpha_out_sorted

  ## Additional warning
  if (warn && any(duplicated(p))) {
    warning("Duplicated p-values detected. Ties were handled by 'order()' (stable).")
  }

  return(alpha_out)
}
