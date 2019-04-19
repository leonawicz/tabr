#' Fretboard plot
#'
#' Create a fretboard diagram.
#'
#' Create a fretboard diagram ggplot object. Number of strings is dervied from \code{tuning}.
#' See \code{\link{tunings}} for pre-defined tunings and examples of explicit tunings.
#' \code{tuning} affects point labels when \code{labels = "notes"}.
#'
#' @param string integer or as \code{tabr}-style character string, string numbers.
#' @param fret integer or as \code{tabr}-style character string, fret numbers.
#' @param labels character, optional text labels, must be one for every point.
#' @param mute logical, whether to mute notes, typically a vector corresponding to \code{string} and \code{fret}.
#' @param label_size numeric, size of fretted note labels.
#' @param label_color character, label color.
#' @param point_size numeric, size of fretted note points.
#' @param point_color character, point color.
#' @param point_fill character, point fill color.
#' @param group optional vector to facet by.
#' @param horizontal logical, directional orientation.
#' @param left_handed logical, handedness orientation.
#' @param fret_range fret limits, if not \code{NULL}, overrides limits derived from \code{fret}.
#' @param key character, key signature, used to enforce type of accidentals when \code{labels = "notes"}.
#' @param tuning explicit tuning, e.g., \code{"e, a, d g b e'"}, or a pre-defined tuning. See details.
#' @param show_tuning logical, show tuning of each string.
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' # open chord
#' am_frets <- c(0, 0, 2, 2, 1, 0) # first note will be muted; 'x' is drawn at 0
#' idx <- c(1, 1, 2, 2, 2, 1)
#' fill <- c("white", "black")[idx]
#' lab_col <- c("black", "white")[idx]
#' mute <- c(TRUE, rep(FALSE, 5))
#' fretboard_plot(6:1, am_frets, "notes", mute, label_color = lab_col, point_fill = fill)
#'
#' # moveable chord
#' fretboard_plot(6:1, am_frets, mute = mute, point_fill = fill, fret_range = c(0, 4),
#'   horizontal = TRUE, show_tuning = TRUE)
#'
#' # scale shifting exercise
#' string <- c(6, 6, 6, 5, 5, 5, 4, 4, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1)
#' fret <- c(2, 4, 5, 2, 4, 5, 2, 4, 6, 7, 9, 6, 7, 9, 7, 9, 10, 7, 9, 10)
#' fretboard_plot(string, fret, labels = "notes")
fretboard_plot <- function(string, fret, labels = NULL, mute = FALSE, label_size = 4, label_color = "white",
                           point_size = 10, point_color = "black", point_fill = "black", group = NULL,
                           horizontal = FALSE, left_handed = FALSE,
                           fret_range = NULL, key = "c", tuning = "standard", show_tuning = FALSE){
  string <- as.integer(.uncollapse(string))
  fret <- as.integer(.uncollapse(fret))
  o <- strsplit(.map_tuning(tuning), " ")[[1]]
  if(is.null(fret_range)){
    fret_range <- range(fret, na.rm = TRUE)
    if(!any(fret == 0, na.rm = TRUE))  fret_range <- fret_range - c(1, 0)
  }
  if(any(fret_range < 0) | any(fret < 0, na.rm = TRUE)) stop("Fret numbers must be >= 0.")
  if(any(string < 1)) stop("String numbers must be >= 1.")
  n <- length(o)
  d <- data.frame(x = string, y = fret - 0.5, label_color = label_color,
                  point_color = point_color, point_fill = point_fill)
  if(!is.null(group)) d$group <- group
  if(!is.null(labels)){
    if(length(labels) == 1 && labels == "notes"){
      fret2 <- fret
      fret2[fret2 >= 10] <- paste0("(", fret2[fret2 >= 10], ")")
      d$lab <- sf_phrase(paste0(string, collapse = " "), paste0(fret2, collapse = " "), 1, key, tuning, TRUE) %>%
        pretty_notes() %>% .uncollapse()
    } else {
      d$lab <- labels
    }
  }
  d2 <- data.frame(string_1 = 1, string_n = n, fret = fret_range[1]:fret_range[2])
  d3 <- data.frame(string = 1:n, fret_1 = fret_range[1], fret_n = fret_range[2])
  g <- ggplot2::ggplot(d, ggplot2::aes(.data[["x"]], .data[["y"]])) +
    ggplot2::geom_segment(data = d2, ggplot2::aes(x = .data[["string_1"]], xend = .data[["string_n"]],
                                                  y = .data[["fret"]], yend = .data[["fret"]]), size = 1) +
    ggplot2::geom_segment(data = d3, ggplot2::aes(x = .data[["string"]], xend = .data[["string"]],
                                                  y = .data[["fret_1"]], yend = .data[["fret_n"]]), size = 1)
  y <- x <- ggplot2::element_text(size = 18, face = 2)
  if(0 %in% fret_range){
    fret_range2 <- if(any(fret == 0)) fret_range - c(1, 0) else fret_range
    if(horizontal) x <- ggplot2::element_blank() else y <- ggplot2::element_blank()
    d4 <- data.frame(string_1 = 1, string_n = n, fret = 0)
    g <- g + ggplot2::geom_segment(
      data = d4, ggplot2::aes(x = .data[["string_1"]], xend = .data[["string_n"]],
                              y = .data[["fret"]], yend = .data[["fret"]]), size = 4) +
      if(horizontal & !left_handed) ggplot2::scale_y_continuous(limits = fret_range2) else
        ggplot2::scale_y_reverse(limits = rev(fret_range2))
  } else {
    g <- g + if(horizontal & !left_handed)
      ggplot2::scale_y_continuous(limits = fret_range, labels = fret_range[1], breaks = fret_range[1]) else
        ggplot2::scale_y_reverse(limits = rev(fret_range), labels = fret_range[1], breaks = fret_range[1])
  }
  if(show_tuning){
    xbreaks <- 1:n
    xlabels <- rev(pretty_notes(o))
  } else {
    xbreaks <- NULL
    xlabels <- NULL
  }
  xlab_pos <- if(horizontal & !left_handed) "bottom" else "top"
  if(left_handed & !horizontal){
    xscale <- ggplot2::scale_x_continuous(breaks = xbreaks, labels = xlabels, position = xlab_pos,
                                          expand = c(0, 0), limits = c(1, n) + c(-0.5, 0.5))
  } else {
    xscale <- ggplot2::scale_x_reverse(breaks = xbreaks, labels = xlabels, position = xlab_pos,
                                       expand = c(0, 0), limits = c(n, 1) + c(0.5, -0.5))
  }
  if(any(mute))
    g <- g + ggplot2::geom_point(data = d[mute, ], shape = "X", color = d$point_color[mute], size = point_size)
  g <- g + ggplot2::geom_point(data = d[!mute, ], shape = 21,
                               color = d$point_color[!mute], fill = d$point_fill[!mute], size = point_size) +
    xscale + ggplot2::theme_void()
  if(!is.null(labels))
    g <- g + ggplot2::geom_text(data = d[!mute, ], ggplot2::aes(label = .data[["lab"]]), size = label_size,
                                fontface = 2, color = d$label_color[!mute])
  if(horizontal){
    asp <- n / (diff(fret_range) + 1)
    g <- g + ggplot2::coord_flip()
  } else {
    asp <- ((diff(fret_range) + 1) / n)
  }
  if(!is.null(group)) g <- g + ggplot2::facet_wrap(stats::as.formula(paste("~group")), scales = "free") # nolint
  g + ggplot2::theme(axis.text.x = x, axis.text.y = y, aspect.ratio = asp)
}
