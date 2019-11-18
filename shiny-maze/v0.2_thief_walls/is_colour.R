# Copyright 2018 Chris Campbell

# @title is landing point named colour?
# @description check RGB values of 3D array against
# specified colour in Cartesian coordinates
# @param pos length 2 numeric vector
# @param size length 1 pixel length of landing point
# @param bgarray numeric array with dim c(x, y, 3)
# corresponding to RGB colour channels
# @param col single character colour to compare
# @return single logical
# @examples
# bg <- array(0.5, dim = c(8, 12, 3))
# bg[2, 2, ] <- c(1, 0, 0) # mat (0.2, 0.2) == plot (0.2, 0.8)
# bg[2, 9, ] <- c(0.5, 1, 0)
# is_colour(pos = c(0.5, 0.5), size = 1, bgarray = bg)
# is_colour(pos = c(0, 1), size = 2, bgarray = bg)
# is_colour(pos = c(0.2, 0.2), size = 1, bgarray = bg) # FALSE
# is_colour(pos = c(0.2, 0.8), size = 1, bgarray = bg) # TRUE
# is_colour(pos = c(0.2, 0.2), size = 10, bgarray = bgd)
# positions <- expand.grid(x = seq_len(wd) / wd, y = seq_len(ht) / ht)
# positions$is_red <- apply(positions, MARGIN = 1, FUN = is_colour, 
#     size = size, bgarray = bgd)
# positions$is_red <- positions$is_red * 3L + 1L
# background_plot +
#     geom_point(data = positions, mapping = aes(x = x, y = y, shape = is_red)) +
#     scale_shape_identity()

is_colour <- function(pos, size, bgarray, col = "red") {
    # check inputs
    if (!is.numeric(pos) || length(pos) != 2L) { stop("pos must be length 2 numeric") }
    if (any(pos < 0L) || any(pos > 1L)) { 
        warning("pos expected in range 0, 1") 
        return(FALSE)
    }
    if (!is.numeric(size) || length(size) != 1L) { stop("size must be single numeric") }
    if (!is.array(bgarray)) { stop("bgarray must be an array") }
    # width and height
    bgdim <- dim(bgarray)[1:2]
    # rows/cols are counted from top left to bottom right
    # x/y are counted from bottom left to top right
    spot <- bgarray[
        # rows = y position
        seq.int(
            from = max(1L, floor((1 - pos[2L]) * bgdim[1L] - size / 2)), 
            to = min(bgdim[1L], ceiling((1 - pos[2L]) * bgdim[1L] + size / 2))),
        # cols = x position
        seq.int(
            # max/min to check selected region falls within array boundary
            from = max(1L, floor(pos[1L] * bgdim[2L] - size / 2)), 
            to = min(bgdim[2L], ceiling(pos[1L] * bgdim[2L] + size / 2))), 
        , drop = FALSE]
    any(apply(
        X = spot, 
        MARGIN = 1:2, 
        FUN = binarise_colour, 
        col = col))
}


# @title binarise colour
# @description are these pixels the declared colour?
# check whether RGB channels are within a proportion
# of col.
# @param x matrix with 3 rows, or vector that can be 
# coerced to 3 rows containing RGB colour channel values
# in range 0:1
# @param col single character colour to compare
# @param dist single numeric distance each channel can 
# vary from col to be classified 
# @return logical vector
# @examples
# binarise_colour(x = c(0.95, 0.05, 0))
# binarise_colour(x = c(0.95, 0.05, 0.9))
# binarise_colour(x = matrix(c(0.95, 0.05, 0, 1, 0, 0, 0.5, 0, 0), nrow = 3L))

binarise_colour <- function(x, col = "red", dist = 0.1) {
    x <- matrix(x, nrow = 3L, byrow = FALSE)
    if (any(x > 1L)) { x <- x / 255 }
    col_rgb <- c(col2rgb(col) / 255)
    apply(
        X = x, 
        MARGIN = 2L, 
        FUN = function(xi) { all(abs(col_rgb - xi) < dist) })
}
