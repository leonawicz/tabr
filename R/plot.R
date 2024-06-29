#' Chord and fretboard diagram plots
#'
#' Create a fretboard diagram for a single chord or a general progression.
#'
#' These functions are under development and subject to change. They each return
#' a ggplot object.
#'
#' Use `plot_chord()` to create a fretboard diagram of a specific chord.
#' `plot_chord()` accepts a character string in simple fretboard format, e.g.,
#' `chord = "xo221o"`. Zero is allowed in place of `"o"`. This only works when
#' no spaces or semicolons are detected. The function checks for spaces first,
#' then semicolons, to split fret numbers. Do not mix formats. For example, you
#' can use `chord = "xo221o"`, `chord = "x 8 10 10 9 8"` or
#' `chord = "x;8;10;10;9;8"`. Trailing delimiters are ignored (LilyPond format:
#' `"x;8;10;10;9;8;"`). If there are fewer fret values than there are strings on
#' the instrument, as inferred from `tuning`, then muted strings, `x`, are
#' inferred for the remaining lower-pitch strings.
#'
#' `plot_fretboard()` produces a more general fretboard diagram plot. It is
#' intended for scales, arpeggios and other patterns along the fretboard. For
#' this function, provide vectors of string and fret numbers. `mute` is
#' available but not as applicable for this function; it is a pass-through from
#' `plot_chord()`. For single chord diagrams, use `plot_chord()`. The letter
#' `"o"` is also allowed in `fret` for open strings and will display below the
#' lowest fret plotted. The number `0` is treated with the intent of displaying
#' the corresponding position on the instrument neck.
#'
#' Number of strings is derived from `tuning`. See [tunings()] for pre-defined
#' tunings and examples of explicit tunings. `tuning` affects point labels when
#' `labels = "notes"`.
#'
#' Providing `fret_labels` overrides the default (minimal) fret numbering
#' behavior for the fret axis. These are only intended to be integers. The
#' vector of integers given is sorted and subset if needed to the range of frets
#' that appear in the plot. See example.
#'
#' @param string integer or as a space-delimited character string; instrument
#' string numbers.
#' @param fret integer or as a space-delimited character string; fret numbers.
#' @param chord character, a single chord given in fret notation. See details.
#' @param labels `NULL` or character, optional vector of text labels, must be
#' one for every point; or just the special value `"notes"`.
#' @param mute logical vector or specific integer indices, which notes to mute.
#' See details.
#' @param label_size numeric, size of fretted note labels.
#' @param label_color character, label color.
#' @param point_size numeric, size of fretted note points.
#' @param point_color character, point color.
#' @param point_fill character, point fill color.
#' @param group optional vector to facet by.
#' @param horizontal logical, directional orientation.
#' @param left_handed logical, handedness orientation.
#' @param fret_range fret limits, if not `NULL`, overrides limits derived
#' from `fret`.
#' @param fret_labels integer, vector of fret number labels for fret axis. See
#' details.
#' @param fret_offset logical set to `TRUE` to shift the fret axis number labels
#' (if present) from being directly next to the fret to being aligned with the
#' circles behind the fret.
#' @param accidentals character, when `labels = "notes"` represent accidentals:
#' `"flat"` or `"sharp"`.
#' @param tuning explicit tuning, e.g., `"e, a, d g b e'"`, or a pre-defined
#' tuning. See details.
#' @param show_tuning logical, show tuning of each string on string axis.
#' @param asp numeric, aspect ratio, overrides default aspect ratio derived
#' from number of strings and frets.
#' @param base_size base size for `ggplot2::theme_void()`.
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' # General patterns: scale shifting exercise
#' string <- c(6, 6, 6, 5, 5, 5, 4, 4, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1)
#' fret <- "2 4 5 2 4 5 2 4 6 7 9 6 7 9 7 9 10 7 9 10" # string input accepted
#' plot_fretboard(string, fret, labels = "notes", fret_offset = TRUE)
#' plot_fretboard(string, fret, fret_labels = c(3, 5, 7, 9, 12), show_tuning = TRUE)
#'
#' # open and muted strings on shifted general fretboard layout
#' # try to use plot_chord() if more suitable
#' plot_fretboard("6 5 4 3", "o 9 10 12", mute = 2, show_tuning = TRUE)
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
#' # leading x inferred; same as plot_chord("xxo321")
#' plot_chord("o231", fret_labels = 3)
#' plot_chord("10 12 13 11", show_tuning = TRUE)
#' plot_chord("o x 10 12 13 11", fret_range = c(9, 14), fret_labels = c(9, 12))
plot_fretboard <- function(string, fret, labels = NULL, mute = FALSE,
                           label_size = 10, label_color = "white",
                           point_size = 10, point_color = "black",
                           point_fill = "black", group = NULL,
                           horizontal = FALSE, left_handed = FALSE,
                           fret_range = NULL, fret_labels = NULL,
                           fret_offset = FALSE, accidentals = c("flat", "sharp"),
                           tuning = "standard", show_tuning = FALSE,
                           asp = NULL, base_size = 20){
  accidentals <- match.arg(accidentals)
  label_size <- label_size / 2.5
  key <- if(accidentals == "flat") "f" else "g"
  string <- as.integer(.uncollapse(string))

  if(is.logical(mute) & length(mute) == 1) mute <- rep(mute, length(string))
  if(is.numeric(mute)){
    mute0 <- mute
    mute <- rep(FALSE, length(string))
    mute[mute0] <- TRUE
  }

  fret <- .uncollapse(fret)
  open_string <- fret == "o"
  nut <- 0

  if(!is.null(fret_range)){
    if(length(fret_range) != 2)
      stop("Explicit fret range must be a vector of two numbers.", call. = FALSE)
  }

  if(any(open_string)){
    fret[open_string] <- min(as.integer(fret[!open_string & !mute])) - 1
    fret <- as.integer(fret)
    if(!is.null(fret_range)){
      if(fret_range[1] < fret[open_string][1])
        stop("Explicit fret range must be greater than implicit fret number of explicit open strings.", call. = FALSE)
    }
    nut <- fret[open_string][1]
  } else {
    fret <- as.integer(fret)
  }

  if(any(mute)){
    if(nut > 0) fret[mute] <- nut else nut <- fret[mute]
  }

  if(length(string) != length(fret))
    stop("`string` and `fret` must have equal number of entries.", call. = FALSE)

  if(any(string < 1)) stop("String numbers must be >= 1.", call. = FALSE)

  if(!is.null(fret_range)){
    if(any(fret < fret_range[1]) | any(fret > fret_range[2]))
      stop("Explicit fret range will cut off notes.", call. = FALSE)
  }
  if(is.null(fret_range)) fret_range <- range(fret, na.rm = TRUE)
  fr2 <- fret_range - c(1, 0)
  if(any(fret_range < 0) | any(fret < 0, na.rm = TRUE))
    stop("Fret numbers must be >= 0.", call. = FALSE)

  fret_pos <- NULL
  if(is.null(fret_labels)){
    if(fret_range[1] > 1) fret_pos <- fret_labels <- fret_range[1] + any(open_string | mute)
  } else {
    if(!is.numeric(fret_labels))
      stop("Fret labels must be integers.", call. = FALSE)
    fret_labels <- as.integer(fret_labels)
    fret_labels <- fret_labels[fret_labels %in% seq(max(fret_range[1] - 1, 1), fret_range[2], by = 1)]
    fret_pos <- fret_labels
  }
  if(is.numeric(fret_labels) && fret_offset){
    fret_pos <- fret_labels - 0.5
  }

  o <- .split_chords(.map_tuning(tuning))
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
        paste0(fret2, collapse = " "), 1, key, tuning, TRUE) |>
        pretty_notes() |>
        .uncollapse()
    } else {
      d$lab <- labels
    }
  }

  d2 <- data.frame(string_1 = 1, string_n = n, fret = max(fr2[1], nut):fret_range[2])
  d3 <- data.frame(string = 1:n, fret_1 = max(fr2[1], nut), fret_n = fret_range[2])
  g <- ggplot2::ggplot(d, ggplot2::aes(.data[["x"]], .data[["y"]])) +
    ggplot2::geom_segment(
      data = d2,
      ggplot2::aes(x = .data[["string_1"]], xend = .data[["string_n"]],
                   y = .data[["fret"]], yend = .data[["fret"]]),
      linewidth = 1) +
    ggplot2::geom_segment(
      data = d3,
      ggplot2::aes(x = .data[["string"]], xend = .data[["string"]],
                   y = .data[["fret_1"]], yend = .data[["fret_n"]]),
      linewidth = 1)

  y <- x <- ggplot2::element_text(size = 18, face = 2)
  if(fret_range[1] < 2){
    g <- g + ggplot2::geom_segment(
      data = data.frame(string_1 = 1, string_n = n, fret = nut),
      ggplot2::aes(
        x = .data[["string_1"]], xend = .data[["string_n"]],
        y = .data[["fret"]], yend = .data[["fret"]]
      ), linewidth = 4
    )
  }

  if(is.null(fret_labels)){
    if(horizontal){
      x <- ggplot2::element_blank()
    } else {
      y <- ggplot2::element_blank()
    }
  }

  if(fret_range[1] == 0){
    lims <- if(all(fret > 0)) fret_range else fr2
    if(horizontal & !left_handed){
      g <- g + ggplot2::scale_y_continuous(limits = lims, labels = fret_labels, breaks = fret_pos)
    } else {
      g <- g + ggplot2::scale_y_reverse(limits = rev(lims), labels = fret_labels, breaks = fret_pos)
    }
  } else {
    if(horizontal & !left_handed){
      g <- g + ggplot2::scale_y_continuous(
        limits = fr2, labels = fret_labels, breaks = fret_pos)
    } else {
      g <- g + ggplot2::scale_y_reverse(
        limits = rev(fr2), labels = fret_labels, breaks = fret_pos)
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
                       point_color = "black", point_fill = "black", group = NULL,
                       horizontal = FALSE, left_handed = FALSE,
                       fret_range = NULL, fret_labels = NULL,
                       fret_offset = FALSE, accidentals = c("flat", "sharp"),
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
  n_strings <- length(.split_chords(.map_tuning(tuning)))
  if(n > n_strings)
    stop("Cannot have more fret values than number of instrument strings.",
         call. = FALSE)
  if(n < n_strings) chord <- c(rep("x", n_strings - n), chord)
  explicit_open_string <- chord == "o"
  chord <- gsub("o", "0", chord)
  mute <- chord == "x"

  if(any(mute)){
    chord[mute] <- 0
    fret <- as.integer(chord)
    zero_fret <- max(min(fret[!mute]) - 1, 0)
    chord[mute] <- zero_fret
  }
  if(any(explicit_open_string)) chord[explicit_open_string] <- "o"

  plot_fretboard(n_strings:1, chord, labels, mute, label_size, label_color,
                 point_size, point_color, point_fill, group, horizontal,
                 left_handed, fret_range, fret_labels, fret_offset, accidentals,
                 tuning, show_tuning, asp, base_size)
}
