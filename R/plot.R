#' Chord and fretboard diagram plots
#'
#' Create a fretboard diagram for a single chord or a general progression.
#'
#' These functions are under development and subject to change. They each
#' return a ggplot object.
#'
#' Use \code{plot_chord} to create a fretboard diagram of a specific chord.
#' \code{plot_chord} accepts a character string in simple fretboard format,
#' e.g., \code{chord = "xo221o"}.
#' Zero is allowed in place of \code{"o"}. This only works when no spaces or
#' semicolons are detected. The function checks for spaces first, then
#' semicolons, to split fret numbers.
#' Do not mix formats. For example, you can use \code{chord = "xo221o"},
#' \code{chord = "x 8 10 10 9 8"} or \code{chord = "x;8;10;10;9;8"}.
#' Trailing delimiters are ignored (Lilypond format: \code{"x;8;10;10;9;8;"}).
#' If there are fewer fret values than there are strings on the instrument, as
#' inferred from \code{tuning}, then muted strings, \code{x}, are inferred for
#' the remaining lower-pitch strings.
#'
#' \code{plot_fretboard} produces a more general fretboard diagram plot. It is
#' intended for scales, arpeggios and other patterns along the fretboard. For
#' this function, provide vectors of string and fret numbers. \code{mute} is
#' available but not as applicable for this function. For single chord diagrams,
#' use \code{plot_chord}.
#'
#' Number of strings is derived from \code{tuning}. See \code{\link{tunings}}
#' for pre-defined tunings and examples of explicit tunings.
#' \code{tuning} affects point labels when \code{labels = "notes"}.
#'
#' @param string integer or as a space-delimited character string; instrument
#' string numbers.
#' @param fret integer or as a space-delimited character string; fret numbers.
#' @param chord character, a single chord given in fret notation. See details.
#' @param labels character, optional text labels, must be one for every point.
#' @param mute logical vector or specific integer indices, which notes to mute.
#' @param label_size numeric, size of fretted note labels.
#' @param label_color character, label color.
#' @param point_size numeric, size of fretted note points.
#' @param point_color character, point color.
#' @param point_fill character, point fill color.
#' @param group optional vector to facet by.
#' @param horizontal logical, directional orientation.
#' @param left_handed logical, handedness orientation.
#' @param fret_range fret limits, if not \code{NULL}, overrides limits derived
#' from \code{fret}.
#' @param accidentals character, when \code{labels = "notes"} represent
#' accidentals: \code{"flat"} or \code{"sharp"}.
#' @param tuning explicit tuning, e.g., \code{"e, a, d g b e'"}, or a
#' pre-defined tuning. See details.
#' @param show_tuning logical, show tuning of each string.
#' @param asp numeric, aspect ratio, overrides default aspect ratio derived
#' from number of strings and frets.
#' @param base_size base size for \code{ggplot2::theme_void}.
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' # General patterns: scale shifting exercise
#' string <- c(6, 6, 6, 5, 5, 5, 4, 4, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1)
#' fret <- "2 4 5 2 4 5 2 4 6 7 9 6 7 9 7 9 10 7 9 10" # string input accepted
#' plot_fretboard(string, fret, labels = "notes")
#'
#' # Single chord diagrams
#' # open chord
#' idx <- c(1, 1, 2, 2, 2, 1)
#' fill <- c("white", "black")[idx]
#' lab_col <- c("black", "white")[idx]
#' plot_chord("xo221o", "notes", label_color = lab_col, point_fill = fill)
#'
#' # moveable chord
#' plot_chord("355433", horizontal = TRUE, show_tuning = TRUE)
#'
#' plot_chord("0231") # leading x inferred; same as plot_chord("xxo321")
#'
#' plot_chord("10 12 13 11", fret_range = c(10, 14))
plot_fretboard <- function(string, fret, labels = NULL, mute = FALSE,
                           label_size = 10, label_color = "white",
                           point_size = 10, point_color = "black",
                           point_fill = "black", group = NULL,
                           horizontal = FALSE, left_handed = FALSE,
                           fret_range = NULL, accidentals = c("flat", "sharp"),
                           tuning = "standard", show_tuning = FALSE,
                           asp = NULL, base_size = 20){
  accidentals <- match.arg(accidentals)
  label_size <- label_size / 2.5
  key <- if(accidentals == "flat") "f" else "g"
  string <- as.integer(.uncollapse(string))
  fret <- as.integer(.uncollapse(fret))
  if(length(string) != length(fret))
    stop("`string` and `fret` must have equal number of entries.",
         call. = FALSE)
  o <- strsplit(.map_tuning(tuning), " ")[[1]]
  if(is.logical(mute) & length(mute) == 1) mute <- rep(mute, length(string))
  if(is.numeric(mute)){
    mute0 <- mute
    mute <- rep(FALSE, length(string))
    mute[mute0] <- TRUE
  }
  if(!is.null(fret_range)){
    idx <- fret == 0 & fret < fret_range[1] & mute
    if(any(idx)) fret[idx] <- fret_range[1] - 1
    if(!fret_range[1] == 0) fret_range <- fret_range - c(1, 0)
  } else {
    fret_range <- range(fret, na.rm = TRUE)
    if(!any(fret == 0, na.rm = TRUE) & !fret_range[1] == 0)
      fret_range <- fret_range - c(1, 0)
  }

  if(any(fret_range < 0) | any(fret < 0, na.rm = TRUE))
    stop("Fret numbers must be >= 0.", call. = FALSE)
  if(any(string < 1)) stop("String numbers must be >= 1.", call. = FALSE)
  n <- length(o)
  d <- data.frame(x = string, y = fret - 0.5, label_color = label_color,
                  point_color = point_color, point_fill = point_fill)
  if(!is.null(group)) d$group <- group
  if(!is.null(labels)){
    if(length(labels) == 1 && labels == "notes"){
      fret2 <- fret
      fret2[fret2 >= 10] <- paste0("(", fret2[fret2 >= 10], ")")
      d$lab <- sf_phrase(
        paste0(string, collapse = " "),
        paste0(fret2, collapse = " "), 1, key, tuning, TRUE) %>%
        pretty_notes() %>%
        .uncollapse()
    } else {
      d$lab <- labels
    }
  }
  d2 <- data.frame(string_1 = 1, string_n = n,
                   fret = fret_range[1]:fret_range[2])
  d3 <- data.frame(string = 1:n, fret_1 = fret_range[1], fret_n = fret_range[2])
  g <- ggplot2::ggplot(d, ggplot2::aes(.data[["x"]], .data[["y"]])) +
    ggplot2::geom_segment(
      data = d2,
      ggplot2::aes(x = .data[["string_1"]], xend = .data[["string_n"]],
                   y = .data[["fret"]], yend = .data[["fret"]]),
      size = 1) +
    ggplot2::geom_segment(
      data = d3,
      ggplot2::aes(x = .data[["string"]], xend = .data[["string"]],
                   y = .data[["fret_1"]], yend = .data[["fret_n"]]),
      size = 1)
  y <- x <- ggplot2::element_text(size = 18, face = 2)

  if(0 %in% fret_range){
    fr2 <- if(any(fret == 0)) fret_range - c(1, 0) else fret_range
    if(horizontal){
      x <- ggplot2::element_blank()
    } else {
      y <- ggplot2::element_blank()
    }
    d4 <- data.frame(string_1 = 1, string_n = n, fret = 0)
    g <- g + ggplot2::geom_segment(
      data = d4,
      ggplot2::aes(
        x = .data[["string_1"]], xend = .data[["string_n"]],
        y = .data[["fret"]], yend = .data[["fret"]]), size = 4) +
      if(horizontal & !left_handed){
        ggplot2::scale_y_continuous(limits = fr2)
      } else {
        ggplot2::scale_y_reverse(limits = rev(fr2))
      }
  } else {
    if(any(mute | fret == 0)){
      fr2 <- fret_range - c(1, 0)
      bump <- 1
    } else {
      fr2 <- fret_range
      bump <- 0
    }
    if(horizontal & !left_handed){
      g <- g + ggplot2::scale_y_continuous(
        limits = fr2, labels = fr2[1] + bump + 1, breaks = fr2[1] + bump + 0.5)
    } else {
      g <- g + ggplot2::scale_y_reverse(
        limits = rev(fr2), labels = fr2[1] + bump + 1,
        breaks = fr2[1] + bump + 0.5)
    }
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
    xscale <- ggplot2::scale_x_continuous(
      breaks = xbreaks, labels = xlabels, position = xlab_pos,
      expand = c(0, 0), limits = c(1, n) + c(-0.5, 0.5))
  } else {
    xscale <- ggplot2::scale_x_reverse(
      breaks = xbreaks, labels = xlabels, position = xlab_pos, expand = c(0, 0),
      limits = c(n, 1) + c(0.5, -0.5))
  }

  if(any(mute)) g <- g + ggplot2::geom_point(
    data = d[mute, ],
    shape = "X", color = d$point_color[mute], size = point_size
  )

  g <- g + ggplot2::geom_point(
    data = d[!mute, ], shape = 21, color = d$point_color[!mute],
    fill = d$point_fill[!mute], size = point_size) +
    xscale + ggplot2::theme_void(base_size = base_size)

  if(!is.null(labels))
    g <- g +
    ggplot2::geom_text(data = d[!mute, ], ggplot2::aes(label = .data[["lab"]]),
                       size = label_size, fontface = 2,
                       color = d$label_color[!mute])
  if(is.null(asp)){
    gap <- diff(fret_range) + 1
    asp <- if(horizontal) n / gap else gap / n
  }
  if(horizontal) g <- g + ggplot2::coord_flip()
  if(!is.null(group)) g <- g +
    ggplot2::facet_wrap(stats::as.formula(paste("~group")), scales = "free")
  g + ggplot2::theme(
    plot.margin = ggplot2::unit(c(5, 5, 5, 5), "mm"),
    axis.text.x = x, axis.text.y = y, aspect.ratio = asp)
}

#' @export
#' @rdname plot_fretboard
plot_chord <- function(chord, labels = NULL, label_size = 10,
                       label_color = "white", point_size = 10,
                       point_color = "black", point_fill = "black",
                       group = NULL, horizontal = FALSE, left_handed = FALSE,
                       fret_range = NULL, accidentals = c("flat", "sharp"),
                       tuning = "standard", show_tuning = FALSE, asp = NULL,
                       base_size = 20){
  accidentals <- match.arg(accidentals)
  if(length(chord) > 1) stop("Length of chord must be one.", call. = FALSE)
  if(grepl(";", chord)){
    chord <- gsub(";$", "", chord)
    chord <- strsplit(chord, ";")[[1]]
  } else if(grepl(" ", chord)){
    chord <- strsplit(trimws(chord), " ")[[1]]
  } else {
    chord <- strsplit(chord, "")[[1]]
  }
  n <- length(chord)
  n_strings <- length(strsplit(.map_tuning(tuning), " ")[[1]])
  if(n > n_strings)
    stop("Cannot have more fret values than number of instrument strings.",
         call. = FALSE)
  if(n < n_strings) chord <- c(rep("x", n_strings - n), chord)
  chord <- gsub("o", "0", chord)
  mute <- chord == "x"
  if(any(mute)){
    chord[mute] <- 0
  }
  plot_fretboard(n_strings:1, chord, labels, mute, label_size, label_color,
                 point_size, point_color, point_fill, group, horizontal,
                 left_handed, fret_range, accidentals, tuning, show_tuning,
                 asp, base_size)
}
