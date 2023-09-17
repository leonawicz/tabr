#' Create a musical phrase from string/fret combinations
#'
#' Create a musical phrase from character strings that define string numbers,
#' fret numbers and note metadata. This function is a wrapper around
#' [phrase()].
#'
#' Note: This alternate specification wrapper is not receiving further support
#' and will be removed in a future version of `tabr`.
#'
#' This alternate syntax allows for specifying string/fret combinations instead
#' of unambiguous pitch as is used by `phrase()`. In order to remove ambiguity,
#' it is critical to specify the instrument string tuning and key signature. It
#' essentially uses `string` and `fret` in combination with a known tuning and
#' key signature to generate `notes` for [phrase()]. `info` is passed straight
#' through to `phrase()`, as is `string` once it is done being used to help
#' derive `notes`.
#'
#' @details
#' See the main function `phrase` for general details on phrase
#' construction.
#'
#' @section Comparison with `phrase()`:
#' This function is a wrapper function for users not working with musical notes
#' (what to play), but rather just position on the guitar neck (where to play).
#' This approach has conveniences, but is more limiting.
#' In order to remove ambiguity, it is necessary to specify the instrument
#' `tuning` and the `key` signature.
#'
#' In the standard approach with `phrase()` you specify what to play;
#' specifying exactly where to play is optional, but highly recommended (by
#' providing `string`).
#' With `sf_phrase()`, the `string` argument is of course required along with
#' `fret`.
#' But any time the tuning changes, this "where to play" method breaks down and
#' must be redone. It is more robust to provide the string and pitch rather
#' than the string and fret.
#' The `key` is additionally important because it is the only way to
#' indicate if accidentals should be notated as sharps or flats.
#'
#' This wrapper also increases redundancy and typing. In order to specify rests
#' `r`, silent rests `s`, and tied notes `~`, these must now be providing in
#' parallel in both the `string` and `fret` arguments, whereas in the standard
#' method using `phrase()`, they need only be provided once to `notes`.
#' A mismatch will throw an error. Despite the redundancy, this is helpful for
#' ensuring proper match up between `string` and `fret`, which is essentially a
#' dual entry method that aims to reduce itself inside `sf_phrase()` to a single
#' `notes` string that is passed internally to `phrase()`.
#'
#' The important thing to keep in mind is that by its nature, this method of
#' writing out music does not lend itself well to high detail.
#' Tabs that are informed by nothing but string and fret number remove a lot of
#' important information, and those that attempt to compensate with additional
#' symbols in say, an ascii tab, are difficult to read.
#' This wrapper function providing this alternative input method to
#' `phrase()` does its job of allowing users to create phrase objects that
#' are equivalent to standard `phrase()`-generated objects, including rests
#' and ties.
#' But practice and comfort working with `phrase()` is is highly recommended
#' for greater control of development support.
#'
#' The function `sfp()` is a convenient shorthand wrapper for `sf_phrase()`.
#' `sf_note()` and the alias `sfn()` are wrappers around `sf_phrase()` that
#' force `to_notes = TRUE`.
#'
#' @section Single-string input:
#' Another way to use `sf_phrase()` is to provide all musical input to
#' `string` and ignore `fret` and `info` as explicit arguments.
#' Providing all three explicit arguments more closely mimics the inputs of
#' `phrase()` and is useful when you have this information as three
#' independent sources.
#' However, in some cases the single-argument input method can reduce typing,
#' though this depends on the phrase.
#' More importantly, it allow you to reason about your musical inputs by time
#' step rather than by argument.
#' If you provide all three components as a single character string to the
#' `string` argument, leaving both `fret` and `info` as `NULL`, then
#' `sf_phrase()` will decompose `string` into its three component parts
#' internally.
#'
#' There are some rules for single-argument input. The three components are
#' separated by semicolons as `"string;fret;info"`.
#' For example, `"3;7x7;4"` means begin on the third string
#' (infer higher number strings muted).
#' The frets are 7th and 7th, meaning two notes are played. When an `x` is
#' present in the second entry it means a string is not played.
#' This is how it is inferred that the string numbers starting from the third
#' string are strings 3 and 1 rather than 3 and 2 in this example.
#' The 4 indicates a quarter note since it is part of the third entry where the
#' additional `info` is specified. This is contextual. For example, an
#' `x` here would still indicate a dead note, rather than an unplayed
#' string in the second entry, so this is contextual.
#'
#' A bonus when using this input method is that explicit `string` and
#' `info` values persist from one timestep to the next.
#' Neither needs to be provided again until there is a change in value.
#' For example, `"3;7x7;4 7x7 ;7x7;1"` repeats the string and info values
#' from timestep one for timestep two.
#' In timestep three, string numbers repeat again, but the duration changes
#' from quarter note to whole note.
#'
#' Note that except when both `string` and `info` are repeating and
#' only fret numbers are provided (see timestep two above), two semicolons
#' must be present so that it is unambiguous whether the sole missing component
#' is a `string` or `info` (see timestep three).
#'
#' Ambiguity would arise from a case like `"4;4"` without the second semicolon.
#' This type of indexing was chosen over using two different delimiters.
#'
#' If a rest, `r` or `s`, is provided for the `fret` entry, then the `string`
#' entry is ignored. When using this input method, ties `~` are given in the
#' info entry.
#'
#' See the examples for a comparison of two identical phrases specified using
#' both input methods for `sf_phrase()`.
#'
#' @param string character, space-delimited or vector. String numbers
#' associated with notes. Alternatively, provide all information here in a
#' single space-delimited string and ignore `fret` and `info`. See details.
#' @param fret character, space-delimited or vector (or integer vector) of fret
#' numbers associated with notes. Same number of timesteps as `string`.
#' @param info character, space-delimited or vector (or integer vector if simple
#' durations) giving metadata associated with notes. Same number of timesteps as
#' `string`.
#' @param key character, key signature or just specify `"sharp"` or `"flat"`.
#' @param tuning character, instrument tuning.
#' @param to_notes logical, return only the mapped notes character string
#' rather than the entire phrase object.
#' @param bar character or `NULL` (default). Terminates the phrase with a
#' bar or bar check. See details for [phrase()]. Also see the LilyPond help
#' documentation on bar notation for all the valid options.
#' @param ... arguments passed to `sf_phrase()`.
#'
#' @return a phrase.
#' @seealso [phrase()]
#' @export
#'
#' @examples
#' sf_phrase("5 4 3 2 1", "1 3 3 3 1", "8*4 1", key = "b_")
#' sf_phrase("6 6 12 1 21", "133211 355333 11 (13) (13)(13)", "4 4 8 8 4",
#'           key = "f")
#' sfp("6*2 1*4", "000232*2 2*4", "4 4 8*4", tuning = "dropD", key = "d")
#'
#' # compare with single-argument input
#' s <- "3*5 53~*3 543*2 643"
#' f <- "987*2 775 553 335 77~*3 545 325 210"
#' i <- "2*3 4. 16 4.*3 4*3"
#' p1 <- sfp(s, f, i)
#'
#' # Nominally shorter syntax, but potentially much easier to reason about
#' p2 <- sfp("3;987;2*2 775 ;553;4. ;335;16 5;7x7;4.~*3 ;545;4 325 6;2x10;")
#'
#' identical(p1, p2)
sf_phrase <- function(string, fret = NULL, info = NULL, key = "c",
                      tuning = "standard", to_notes = FALSE, bar = NULL){
  if((is.null(fret) & !is.null(info)) | (!is.null(fret) & is.null(info)))
    stop("`fret` and `info` must both be provided or both be NULL.",
         call. = FALSE)
  if(is.null(fret)){
    sfi <- .split_sfp_input(string)
    string <- sfi$string
    fret <- sfi$fret
    info <- sfi$info
  } else {
    string <- .uncollapse(string)
    n <- length(string)
    fret <- .uncollapse(fret)
    info <- .uncollapse(as_noteinfo(info))
    if(length(fret) == 1) fret <- rep(fret, n)
    if(length(fret) != n)
      stop(paste("`fret` must have the same number of timesteps as `string`",
                 "or a single value to repeat."), call. = FALSE)
    if(length(info) == 1) info <- rep(info, n)
    if(length(info) != n)
      stop(paste("`info` must have the same number of timesteps as `string`",
                 "or a single value to repeat."), call. = FALSE)
  }
  string <- .sfp_infer_strings(fret, string)
  tuning <- .map_tuning(tuning)
  open_notes <- rev(.split_chords(tuning))
  str_num <- rev(seq_along(open_notes))
  notes <- purrr::map2(string, fret, ~({
    string_tie <- grepl("~", .x)
    fret_tie <- grepl("~", .y)
    if(!identical(string_tie, fret_tie))
      stop("Tied note mismatch.", call. = FALSE)
    x <- if(any(string_tie)) gsub("~", "", .x) else .x
    y <- if(any(fret_tie)) gsub("~", "", .y) else .y
    rests <- c("r", "s")
    if(x %in% rests | y %in% rests){
      if(x == y) return(x) else stop("Rest mismatch.", call. = FALSE)
    }
    x <- as.integer(strsplit(x, "")[[1]])
    if(any(!x %in% str_num))
      stop("String number outside range inferred by tuning.", call. = FALSE)
    y <- trimws(gsub("(\\(\\d{1,2}\\))", " \\1 ", y))
    y <- strsplit(y, " ")[[1]]
    y <- lapply(y, function(x){
      if(substr(x, 1, 1) == "("){
        gsub("\\(|\\)", "", x)
      } else {
        strsplit(x, "")[[1]]
      }
      }) |>
      unlist() |>
      as.integer()
    if(length(x) != length(y)) stop("String/fret mismatch.", call. = FALSE)
    x <- sapply(seq_along(x), function(i, x, y){
      transpose(open_notes[x[i]], y[i], "tick", key = key)
    }, x = x, y = y)
    if(any(string_tie)) x[string_tie] <- paste0(x[string_tie], "~")
    paste(x, collapse = "")
  })) |>
    unlist() |>
    paste(collapse = " ")
  if(to_notes) return(as_noteworthy(notes))
  phrase(notes, info, gsub("~", "", string), bar)
}

#' @export
#' @rdname sf_phrase
sfp <- sf_phrase

#' @export
#' @rdname sf_phrase
sf_note <- function(...){
  o <- list(...)
  o$to_notes <- TRUE
  do.call(sf_phrase, o)
}

#' @export
#' @rdname sf_phrase
sfn <- sf_note

.sfp_infer_strings <- function(x, s){
  size <- sapply(strsplit(.strsub(gsub("~", "", x)), "_"), length)
  rests <- note_is_rest(x)
  s_tie <- grepl("~", s)
  if(any(s_tie)) s[s_tie] <- gsub("~", "", s[s_tie])
  idx <- which(nchar(s) == 1 & size > 1)
  if(length(idx)) s[idx] <- purrr::map2_chr(as.integer(s[idx]), size[idx], ~{
    x <- paste(seq(.x, by = -1, length.out = .y), collapse = "")
    if(grepl("[-0]", x)) stop("Invalid string number < 1.", call. = FALSE)
    x
  })
  idx <- !rests & nchar(s) != 0
  if(any(idx)){
    if(any(nchar(s[idx]) != size[idx]))
      stop("Number of strings and frets must match at each non-rest timestep.",
           call. = FALSE)
  }
  if(any(s_tie)) s[s_tie] <- paste0(s[s_tie], "~")
  s
}

.split_sfp_input <- function(x){
  x <- strsplit(x, " ")[[1]]
  mult <- sapply(x, .get_mult, USE.NAMES = FALSE)
  x <- sapply(x, .strip_mult, USE.NAMES = FALSE)
  if(any(!sapply(gregexpr(";", x), function(x) length(x[x != -1])) %in%
         c(0, 2)))
    stop("Must have 2 or 0 `;` present for a timestep to avoid ambiguity.",
         call. = FALSE)
  no_sep <- !grepl(";", x)
  if(any(no_sep)) x[no_sep] <- paste0(";", x[no_sep], ";")
  x <- gsub(";$", "; ", x)
  x <- lapply(strsplit(x, ";"), function(x) ifelse(x %in% c("", " "),
                                                   NA_character_, x))
  if(any(is.na(sapply(x, "[", 2))))
    stop("Fret values cannot be missing from `string;fret;info`", call. = FALSE)
  if(is.na(x[[1]][1]) | is.na(x[[1]][3]))
    stop("First timestep must include all three values as `string;fret;info`.",
         call. = FALSE)
  x <- do.call(rbind, x) |>
    as.data.frame(stringsAsFactors = FALSE) |>
    stats::setNames(c("string", "fret", "info")) |>
    tibble::as_tibble() |>
    tidyr::fill("string", "info") |>
    dplyr::mutate(
      string = .infer_strings(.data[["string"]], .data[["fret"]]),
      fret = gsub("x", "", .data[["fret"]])) |>
    dplyr::slice(rep(seq_len(length(x)), times = mult))
  idx <- grep("~", x$info)
  if(length(idx)){
    x$string[idx] <- paste0(x$string[idx], "~")
    x$fret[idx] <- paste0(x$fret[idx], "~")
    x$info[idx] <- gsub("~", "", x$info[idx])
  }
  x
}

.n_frets <- function(x) nchar(gsub("(\\(\\d{1,2}\\))", ".", x))

.muted_index <- function(x) gregexpr("x", gsub("(\\(\\d{1,2}\\))", ".", x))

.infer_strings <- function(string, fret){
  n <- .n_frets(fret)
  muted <- .muted_index(fret)
  f <- function(string, fret, n, muted){
    if(n == 1 & fret %in% c("r", "s")) return(fret)
    string <- seq(as.integer(string), by = -1, length.out = n)
    if(!(-1 %in% muted)) string <- string[-muted]
    paste0(string, collapse = "")
  }
  purrr::map_chr(seq_along(string), ~{
    f(string[.x], fret[.x], n[.x], muted[[.x]])
  })
}

.get_mult <- function(x){
  y <- !grepl("\\*", x)
  if(any(y)) x[y] <- paste0(x[y], "*1")
  as.integer(gsub(".*\\*(\\d+)$", "\\1", x))
}

.strip_mult <- function(x) gsub("(.*)\\*\\d+$", "\\1", x)
