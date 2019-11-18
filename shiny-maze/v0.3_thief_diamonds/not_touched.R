# Copyright 2018 Chris Campbell

#' @title set FALSE when landing point is at coordinate
#' @description touched values should no longer be shown
#' @param pos length 2 numeric vector
#' @param sizep length 1 proportion of plotting area that
#' is length of landing point
#' @param x numeric vector of x coordinates to check 
#' @param y numeric vector of y coordinates to check
#' @param s logical vector of positions to update with FALSE
#' @return logical vector of length s
#' @examples
#' not_touched(pos = c(0.5, 0.5), sizep = 0.1, x = 0.1, y = 0.5, s = TRUE)
#' not_touched(pos = c(0, 1), sizep = 0.1, x = 0.1, y = 0.5, s = TRUE)
#' not_touched(pos = c(0.2, 0.2), sizep = 0.1, x = 0.2, y = 0.25, s = TRUE) # FALSE
#' not_touched(pos = c(0.2, 0.8), sizep = 0.1, 
#'     x = c(0.2, 0.2), y = c(0.5, 0.8), s = c(TRUE, TRUE)) # TRUE FALSE

not_touched <- function(pos, sizep, x, y, s) {
    # check inputs
    if (!is.numeric(pos) || length(pos) != 2L) { stop("pos must be length 2 numeric") }
    if (any(pos < 0L) || any(pos > 1L)) { warning("pos expected in range 0, 1") }
    if (!is.numeric(sizep) || length(sizep) != 1L) { stop("sizep must be single numeric") }
    if (any(sizep < 0L) || any(sizep > 1L)) { warning("sizep expected in range 0, 1") }
    if (length(x) != length(y)) { stop("x and y must be equal length") }
    if (length(x) != length(s)) { stop("x and s must be equal length") }
    ind <- x - sizep <= pos[1L] & x + sizep >= pos[1L] &
        y - sizep <= pos[2L] & y + sizep >= pos[2L]
    s[ind] <- FALSE
    s
}
