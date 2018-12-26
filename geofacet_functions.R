# two functions from original geofacet package by Ryan Hafen <rhafen@gmail.com>
# application uses filtering to insert logo on gtable


gf_gtable_filter <- function (x, keep, trim = FALSE) {
  matches <- x$layout$name %in% keep
  x$layout <- x$layout[matches, , drop = FALSE] # nolint
  x$grobs <- x$grobs[matches]
  if (trim)
    x <- gtable::gtable_trim(x)
  x
}


print.facet_geo <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  if (newpage) grid::grid.newpage()
  
  attrs <- attr(x, "geofacet")
  grd <- attrs$grid
  
  g <- ggplot2::ggplotGrob(x)
  
  extra_rgx <- NULL
  
  if (attrs$move_axes) {
    scls <- attrs$scales
    if (is.null(scls))
      scls <- "same"
    if (!scls %in% c("free", "free_x")) {
      # do x-axis stuff
      nc <- max(grd$col)
      nr <- max(grd$row)
      for (ii in seq_len(nc)) {
        idx <- which(!is.na(grd$label[grd$col == ii]))
        l1 <- paste0("axis-b-", ii, "-", nr)
        if (length(idx) > 0) {
          last <- max(idx)
          l2 <- paste0("axis-b-", ii, "-", last)
          g$layout[g$layout$name == l1, c("t", "b")] <-
            g$layout[g$layout$name == l2, c("t", "b")]
        } else {
          extra_rgx <- c(extra_rgx, l1)
        }
      }
    }
    if (!scls %in% c("free", "free_y")) {
      # do y-axis stuff
      for (ii in seq_len(max(grd$row))) {
        idx <- which(!is.na(grd$label[grd$row == ii]))
        l1 <- paste0("axis-l-", ii, "-1")
        if (length(idx) > 0) {
          first <- min(idx)
          l2 <- paste0("axis-l-", ii, "-", first)
          g$layout[g$layout$name == l1, c("l", "r")] <-
            g$layout[g$layout$name == l2, c("l", "r")]
        } else {
          extra_rgx <- c(extra_rgx, l1)
        }
      }
    }
  }
  
  idx <- which(is.na(grd$label))
  tmp <- setdiff(g$layout$name, c(grd$strip[idx], grd$panel[idx], extra_rgx))
  # rgx <- paste0("(^", paste(tmp, collapse = "$|^"), "$)")
  
  # TODO: look into using extra grid space to draw cartographic map
  # https://github.com/baptiste/gridextra/wiki/gtable
  # https://stackoverflow.com/questions/30532889/ggplot-overlay-two-plots
  
  g <- gf_gtable_filter(g, tmp, trim = FALSE)
  # g <- gtable::gtable_filter(g, rgx, trim = FALSE)
  # commented line: we do not want drawing, we want filtered gtable
  #grid::grid.draw(g)
  g
}