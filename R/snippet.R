#' Render sheet music snippet with LilyPond
#'
#' Render a sheet music/tablature snippet from a music object with LilyPond.
#'
#' These functions allow you to render short, simple snippets of sheet music
#' directly from a \code{music} object. This is useful when you do not need to
#' build up from phrases to tracks to a full score. They treat \code{music} as
#' a single voice for a single track. This simplifies the possible output but
#' is very convenient when this is all you need.
#'
#' These functions abstract the following pipeline,
#'
#' \code{music \%>\% phrase() \%>\% track() \%>\% score() \%>\% render_*()}
#'
#' for this simple edge case and directly expose the most relevant arguments.
#'
#' All \code{header} list elements are character strings. The options for
#' \code{header} include:
#' \itemize{
#'   \item \code{title}
#'   \item \code{subtitle}
#'   \item \code{composer}
#'   \item \code{album}
#'   \item \code{arranger}
#'   \item \code{instrument}
#'   \item \code{meter}
#'   \item \code{opus}
#'   \item \code{piece}
#'   \item \code{poet}
#'   \item \code{copyright}
#'   \item \code{tagline}
#' }
#'
#' All \code{paper} list elements are numeric except \code{page_numbers} and
#' \code{print_first_page_number},
#' which are logical. \code{page_numbers = FALSE} suppresses all page numbering.
#' When \code{page_numbers = TRUE}, you can set
#' \code{print_first_page_number = FALSE} to suppress printing of only the
#' first page number. \code{first_page_number} is the number of the first page,
#' defaulting to 1, and determines all subsequent page numbers. These arguments
#' correspond to LilyPond paper block variables.
#'
#' The options for \code{paper} include the following and have the following
#' default values if not provided:
#' \itemize{
#'   \item \code{textheight = 220}
#'   \item \code{linewidth = 150}
#'   \item \code{indent = 0}
#'   \item \code{fontsize = 20}
#'   \item \code{page_numbers = FALSE}
#'   \item \code{print_first_page_number = TRUE}
#'   \item \code{first_page_number = 1}
#' }
#'
#' \code{textheight = 150} is the default, but for music snippet rendering,
#' a value must be provided explicitly via \code{paper} when rendering to png.
#' Otherwise for png outputs the height is cropped automatically rather than
#' remaining a full page. See \code{lilypond} for details.
#'
#' Passing arguments to \code{header} can completely or partially prevent
#' cropping in both direction, which must then be done manually with
#' \code{linewidth} and \code{textheight}. This is all based on underlying
#' LilyPond behavior.
#'
#' @param music a music object.
#' @param file character, output file ending in .pdf or .png for sheet music or
#' tablature for \code{score}. May include an absolute or relative path.
#' For \code{render_midi}, a file ending in .mid.
#' @param header a named list of arguments passed to the header of the
#' LilyPond file. See \code{lilypond} details.
#' @param paper a named list of arguments for the LilyPond file page layout.
#' See \code{lilypond} details.
#' @param staff character, music staff setting. See \code{track} for details.
#' @param tuning character, string tuning, only applies to tablature. See
#' \code{track}.
#' @param no_tab logical, suppress tablature staff. See \code{track}.
#' @param string_names label strings at beginning of tab staff. \code{NULL}
#' (default) for non-standard tunings only, \code{TRUE} or \code{FALSE} for
#' force on or off completely.
#' @param midi logical, also output an corresponding MIDI file.
#' @param colors a named list of LilyPond element color global overrides. See
#' \code{lilypond} for details.
#' @param transparent logical, transparent background, png only.
#' @param res numeric, resolution, png only. \code{transparent = TRUE} may fail
#' when \code{res} exceeds ~150.
#' @param keep_ly logical, keep the intermediary LilyPond file.
#' @param simplify logical, uses \code{simplify_phrase} to convert to simpler,
#' more efficient LilyPond syntax.
#'
#' @return nothing returned; a file is written.
#' @export
#' @seealso \code{\link{phrase}}, \code{\link{track}}, \code{\link{score}},
#' \code{\link{lilypond}}, \code{\link{tab}}
#'
#' @examples
#' x <- "a,4;5*5 b,4- c4 cgc'e'~4 cgc'e'1 e'4;2 c';3 g;4 c;5 ce'1;51"
#' x <- as_music(x)
#'
#' y <- "a,,4;3*5 b,,4- c,4 c,g,c~4 c,g,c1 c4;1 g,;2 c,;3 g,;2 c,c1;31"
#' y <- as_music(y)
#'
#' \dontrun{
#' if(tabr_options()$lilypond != ""){ # requires LilyPond installation
#'   outfile <- file.path(tempdir(), "out.pdf")
#'   render_music(x, outfile)
#'
#'   outfile <- file.path(tempdir(), "out.png")
#'   render_music(x, outfile, "treble_8", no_tab = FALSE)
#'
#'   render_music_tc(x, outfile)
#'   render_music_bc(x, outfile)
#'
#'   render_music_tab(x, outfile)
#'   render_music_guitar(x, outfile)
#'   render_music_bass(y, outfile)
#' }
#' }
render_music <- function(music, file, staff = "treble", tuning = "standard",
                         no_tab = TRUE, string_names = NULL, header = NULL,
                         paper = NULL, midi = FALSE, colors = NULL,
                         transparent = FALSE, res = 150, keep_ly = FALSE,
                         simplify = TRUE){
  ktt <- .ktt(music)
  paper <- .paper_snippet(paper)
  phrase(music) %>%
    track(tuning, music_staff = staff, no_tab = no_tab) %>%
    score() %>%
    tab(file, ktt[1], ktt[2], ktt[3], header, paper, string_names, TRUE, midi,
        colors, TRUE, transparent, res, keep_ly, simplify, FALSE)
}

#' @export
#' @rdname render_music
render_music_tc <- function(music, file, header = NULL, paper = NULL,
                            midi = FALSE, colors = NULL, transparent = FALSE,
                            res = 150, keep_ly = FALSE, simplify = TRUE){
  ktt <- .ktt(music)
  paper <- .paper_snippet(paper)
  phrase(music) %>%
    track(music_staff = "treble", no_tab = TRUE) %>%
    score() %>%
    tab(file, ktt[1], ktt[2], ktt[3], header, paper, FALSE, TRUE, midi, colors,
        TRUE, transparent, res, keep_ly, simplify, FALSE)
}

#' @export
#' @rdname render_music
render_music_bc <- function(music, file, header = NULL, paper = NULL,
                            midi = FALSE, colors = NULL, transparent = FALSE,
                            res = 150, keep_ly = FALSE, simplify = TRUE){
  ktt <- .ktt(music)
  paper <- .paper_snippet(paper)
  phrase(music) %>%
    track(music_staff = "bass", no_tab = TRUE) %>%
    score() %>%
    tab(file, ktt[1], ktt[2], ktt[3], header, paper, FALSE, TRUE, midi, colors,
        TRUE, transparent, res, keep_ly, simplify, FALSE)
}

#' @export
#' @rdname render_music
render_music_tab <- function(music, file, staff = NA, tuning = "standard",
                             string_names = NULL, header = NULL, paper = NULL,
                             midi = FALSE, colors = NULL, transparent = FALSE,
                             res = 150, keep_ly = FALSE, simplify = TRUE){
  ktt <- .ktt(music)
  paper <- .paper_snippet(paper)
  phrase(music) %>%
    track(tuning, music_staff = staff) %>%
    score() %>%
    tab(file, ktt[1], ktt[2], ktt[3], header, paper, string_names, TRUE, midi,
        colors, TRUE, transparent, res, keep_ly, simplify, FALSE)
}

#' @export
#' @rdname render_music
render_music_guitar <- function(music, file, tuning = "standard",
                                string_names = NULL, header = NULL,
                                paper = NULL, midi = FALSE,
                                colors = NULL, transparent = FALSE, res = 150,
                                keep_ly = FALSE, simplify = TRUE){
  ktt <- .ktt(music)
  paper <- .paper_snippet(paper)
  phrase(music) %>%
    track(tuning, music_staff = "treble_8") %>%
    score() %>%
    tab(file, ktt[1], ktt[2], ktt[3], header, paper, string_names, TRUE, midi,
        colors, TRUE, transparent, res, keep_ly, simplify, FALSE)
}

#' @export
#' @rdname render_music
render_music_bass <- function(music, file, tuning = "bass",
                              string_names = NULL, header = NULL, paper = NULL,
                              midi = FALSE, colors = NULL, transparent = FALSE,
                              res = 150, keep_ly = FALSE, simplify = TRUE){
  ktt <- .ktt(music)
  paper <- .paper_snippet(paper)
  phrase(music) %>%
    track(tuning, music_staff = "bass_8") %>%
    score() %>%
    tab(file, ktt[1], ktt[2], ktt[3], header, paper, string_names, TRUE, midi,
        colors, TRUE, transparent, res, keep_ly, simplify, FALSE)
}

.paper_snippet <- function(x){
  if(is.null(x$fontsize)) x$fontsize <- 20
  if(is.null(x$page_numbers)) x$page_numbers <- FALSE
  x
}

.ktt <- function(x){
  c(music_key(x), music_time(x), music_tempo(x))
}
