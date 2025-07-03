#' Convert MIDI to LilyPond file
#'
#' Convert a MIDI file (`.mid`) to a LilyPond format (`.ly`) text file.
#'
#' Under development/testing. See warning and details below.
#'
#' This function is a wrapper around the `midi2ly()` command line utility
#' provided by LilyPond. It inherits all the limitations thereof.
#' LilyPond is not intended to be used to produce meaningful sheet music from
#' arbitrary MIDI files.
#' While [lilypond()] converts R code `score()` objects to LilyPond markup
#' directly, MIDI conversion to LilyPond markup by `midily()` requires LilyPond.
#'
#' WARNING: Even though the purpose of the command line utility is to convert
#' an existing MIDI file to a LilyPond file, it nevertheless generates a
#' LilyPond file that *specifies inclusion of MIDI output*.
#' This means when you subsequently process the LilyPond file with LilyPond or
#' if you use `miditab()` to go straight from your MIDI file to pdf output,
#' the command line tool will also produce a MIDI file output. It will
#' overwrite your original MIDI file if it has the same file name and location!
#'
#' `allow_tuplets = NULL` to disallow all tuplets. Fourth, eighth and sixteenth
#' note triplets are allowed. The format is a character vector where each
#' element is `duration*numerator/denominator`, no spaces. See default argument.
#'
#' On Windows systems, it may be necessary to specify a path in [tabr_options()]
#' to both `midi2ly` and `python` if they are not already added to the system
#' PATH variable.
#'
#' @param midi_file character, MIDI file (`.mid`). May include an absolute or
#' relative path.
#' @param file LilyPond output file ending in `.ly`.
#' @param key key signature, defaults to `"c"`.
#' @param absolute logical, print absolute pitches (unavailable in current
#' package version).
#' @param quantize integer, duration, quantize notes on duration.
#' @param explicit logical, print explicit durations.
#' @param start_quant integer, duration, quantize note starts on the duration.
#' @param allow_tuplet character vector, allow tuplet durations. See details.
#' @param details logical, print additional information to console.
#' @param lyric logical, treat all text as lyrics.
#'
#' @return nothing returned; a file is written.
#' @export
#' @seealso [miditab()], [tab()], [lilypond()]
#'
#' @examples
#' \dontrun{
#' if(tabr_options()$midi2ly != ""){
#'   midi <- system.file("example.mid", package = "tabr")
#'   outfile <- file.path(tempdir(), "out.ly")
#'   midily(midi, outfile) # requires LilyPond installation
#' }
#' }
midily <- function(midi_file, file, key = "c", absolute = FALSE,
                   quantize = NULL, explicit = FALSE, start_quant = NULL,
                   allow_tuplet = c("4*2/3", "8*2/3", "16*2/3"),
                   details = FALSE, lyric = FALSE){
  .check_lilypond()
  x <- paste0("--key=", .midily_key(key))
  # if(absolute) x <- paste(x, "--absolute-pitches")
  if(absolute) message("--absolute-pitches CL arg is ignored in current tabr version.")
  if(!is.null(quantize)) x <- paste(x, paste0("--duration-quant=", quantize))
  if(explicit) x <- paste(x, "--explicit-durations")
  if(!is.null(start_quant)) x <- paste(x, paste0("--start-quant=", start_quant))
  if(!is.null(allow_tuplet)){
    for(i in seq_along(allow_tuplet))
      x <- paste(x, paste0("--allow-tuplet=", allow_tuplet[i]))
  }
  if(details) x <- paste(x, "--verbose")
  if(lyric) x <- paste(x, "--text-lyrics")
  is_windows <- Sys.info()[["sysname"]] == "Windows"
  py_path <- tabr_options()$python
  if(py_path == ""){
    py_path <- if(is_windows) "python.exe" else "python"
  }
  midi2ly_path <- tabr_options()$midi2ly
  if(midi2ly_path == "") midi2ly_path <- "midi2ly"
  call_string <- paste0(
    "\"", py_path, "\" ", "\"", midi2ly_path, "\" ", x, " --output=\"",
    .adjust_file_path(file)$lp, "\" \"", midi_file, "\""
  )
  system(call_string)
  invisible()
}

.midily_key <- function(x){
  .keycheck(x)
  idx <- which(.keydata$key == x)
  n <- .keydata$nsf[idx]
  if(key_is_flat(x)) n <- -n
  if(.keydata$major[idx]) paste0(n, ":0") else paste0(n, ":1")
}

#' Convert MIDI to tablature
#'
#' Convert a MIDI file to  sheet music/guitar tablature.
#'
#' Under development/testing. See warning and details below.
#'
#' Convert a MIDI file to a pdf or png music score using the LilyPond music
#' engraving program.
#' Output format is inferred from `file` extension. This function is a wrapper
#' around [midily()], the function that converts the MIDI file to a LilyPond
#' (`.ly`) file using a LilyPond command line utility.
#'
#' WARNING: Even though the purpose of the command line utility is to convert
#' an existing MIDI file to a LilyPond file, it nevertheless generates a
#' LilyPond file that *specifies inclusion of MIDI output*.
#' This means when you subsequently process the LilyPond file with LilyPond or
#' if you use `miditab()` to go straight from your MIDI file to pdf output,
#' the command line tool will also produce a MIDI file output. It will
#' overwrite your original MIDI file if it has the same file name and location!
#'
#' On Windows systems, it may be necessary to specify a path in [tabr_options()]
#' to both `midi2ly` and `python` if they are not already added to the system
#' PATH variable.
#'
#' @param midi_file character, MIDI file (`.mid`). May include an absolute
#' or relative path.
#' @param file character, output file ending in .pdf or .png.
#' @param keep_ly logical, keep LilyPond file.
#' @param details logical, set to `TRUE` to print LilyPond log output to
#' console. Windows only.
#' @param ... additional arguments passed to [midily()].
#'
#' @return nothing returned; a file is written.
#' @export
#' @seealso [midily()], [tab()], [lilypond()]
#'
#' @examples
#' \dontrun{
#' if(tabr_options()$midi2ly != ""){
#'   midi <- system.file("example.mid", package = "tabr")
#'   outfile <- file.path(tempdir(), "out.pdf")
#'   miditab(midi, outfile, details = FALSE) # requires LilyPond installation
#' }
#' }
miditab <- function(midi_file, file, keep_ly = FALSE, details = FALSE, ...){
  fp <- .adjust_file_path(file)
  if(details) cat("#### Engraving midi to", fp$tp, "####\n")
  do.call(midily, c(list(midi_file = midi_file, file = fp$lp), list(...)))
  lp_path <- tabr_options()$lilypond
  is_windows <- Sys.info()[["sysname"]] == "Windows"
  if(lp_path == ""){
    lp_path <- if(is_windows) "lilypond.exe" else "lilypond"
  }
  call_string <- paste0("\"", lp_path, "\" --", fp$ext,
                        " -dstrip-output-dir=#f \"", fp$lp, "\"")
  if(is_windows){
    system(call_string, show.output.on.console = details)
  } else {
    system(call_string)
  }
  if(!keep_ly) unlink(fp$lp)
  invisible()
}

#' Read, inspect and convert MIDI file contents
#'
#' Read MIDI file into a data frame and inspect the music data with supporting
#' functions.
#'
#' The `read_midi()` function wraps around `tuneR::readMidi()` by Uwe Ligges and
#' Johanna Mielke. `midi_notes()` is a work in progress, but converts MIDI data
#' to noteworthy strings and note info formats. This makes it easy to analyze,
#' transform and edit the music data as well as render it to sheet music and a
#' new MIDI file.
#'
#' `read_midi()` does not parse the ticks per quarter note from the MIDI file
#' input at this time. It must be specified with `ticks_per_qtr`.
#'
#' @param file character, path to MIDI file.
#' @param x a data frame returned by `read_midi()`. An integer vector for
#' `ticks_to_duration()`; a character vector (may be a space-delimited string)
#' for `duration_to_ticks()`.
#' @param ticks_per_qtr ticks per quarter note. Used to compute durations from
#' MIDI file ticks.
#' @param channel,track integer, filter rows on channel or track.
#' @param noteworthy logical, convert to `noteworthy` and `noteinfo` data.
#'
#' @return a tibble data frame
#' @export
#'
#' @examples
#' ticks_to_duration(c(120, 160))
#' ticks_to_duration(c(128, 192, 512), ticks_per_qtr = 384)
#' duration_to_ticks(c("t8", "8", "8.", "8.."))
#' duration_to_ticks(c("t8 8 8. 8.."), ticks_per_qtr = 384)
#'
#' \dontrun{
#' file <- system.file("example2.mid", package = "tabr")
#' if(require("tuneR")){
#'   x <- read_midi(file, ticks_per_qtr = 384)
#'   midi_metadata(x)
#'   midi_time(x)
#'   midi_key(x)
#'   midi_notes(x, channel = 0, noteworthy = FALSE)
#'
#'   (x <- midi_notes(x, channel = 0))
#'   (x <- as_music(x$pitch, x$duration))
#'
#'   # requires LilyPond installation
#'   if(tabr_options()$lilypond != ""){
#'     out <- file.path(tempdir(), "out.pdf")
#'     phrase(x) |> track_bc() |> score() |> tab(out, details = FALSE)
#'   }
#' }
#' }
read_midi <- function(file, ticks_per_qtr = 480){
  if(!requireNamespace("tuneR")){
    message("Please install the `tuneR` package to read MIDI files.")
    return(invisible())
  }
  mx <- duration_to_ticks(1, ticks_per_qtr)
  x <- tibble::as_tibble(tuneR::readMidi(file))
  y <- tuneR::getMidiNotes(x)
  bycols <- c("channel", "track", "time", parameter1 = "note")
  d <- dplyr::left_join(x, y, by = bycols) |>
    dplyr::select(-dplyr::all_of(c("notename"))) |>
    dplyr::mutate(
      pitch = semitone_pitch(.data[["parameter1"]]),
      pitch = ifelse(.data[["event"]] == "Note On", .data[["pitch"]], NA),
      duration = ticks_to_duration(.data[["length"]], ticks_per_qtr)) |>
    dplyr::select(dplyr::all_of(
      c("time", "length", "duration", "event", "type", "channel", "parameter1",
        "parameter2", "parameterMetaSystem", "track", "pitch", "velocity"
      )
    ))
  idx <- which(is.na(d$duration) & d$event == "Note On")
  if(length(idx)){
    d$duration[idx] <- purrr::map_chr(d$length[idx], ~{
      y <- x <- .x
      while(x > mx){
        x <- x - mx
        y <- c(x, y)
      }
      paste(ticks_to_duration(y, ticks_per_qtr), collapse = ";")
    })
  }
  d
}

#' @export
#' @rdname read_midi
midi_metadata <- function(x){
  dplyr::filter(x, !.data[["event"]] %in% c("Note On", "Note Off"))
}

#' @export
#' @rdname read_midi
midi_notes <- function(x, channel = NULL, track = NULL, noteworthy = TRUE){
  x <- dplyr::filter(x, .data[["event"]] == "Note On") |>
    dplyr::rename(semitone = dplyr::all_of("parameter1")) |>
    dplyr::select(dplyr::all_of(
      c("time", "length", "duration", "pitch", "semitone","velocity", "channel", "track")
    ))

  if(!is.null(channel))
    x <- dplyr::filter(x, .data[["channel"]] %in% !! channel)
  if(!is.null(track))
    x <- dplyr::filter(x, .data[["track"]] %in% !! track)
  if(noteworthy){
    x <- dplyr::group_by(x, .data[["time"]]) |>
      dplyr::summarize(
        duration = unique(.data[["duration"]]),
        pitch = paste(.data[["pitch"]][order(.data[["semitone"]])],
                      collapse = "")) |>
      dplyr::select(-dplyr::all_of("time"))
    idx <- grep(";", x$duration)
    if(length(idx)){
      while(length(idx)){
        info <- strsplit(x$duration[idx[1]], ";")[[1]]
        n <- length(info)
        notes <- rep(x$pitch[idx[1]], n)
        notes[-n] <- tie(notes[-n])
        x$duration[idx[1]] <- info[1]
        x$pitch[idx[1]] <- notes[1]
        x <- dplyr::add_row(
          x, "duration" := info[-1], "pitch" := notes[-1], .after = idx[1])
        idx <- grep(";", x$duration)
      }
    }
    x <- dplyr::mutate(
      x, duration = as_noteinfo(.data[["duration"]]),
      pitch = as_noteworthy(.data[["pitch"]])
    )
  }
  x
}

#' @export
#' @rdname read_midi
midi_time <- function(x){
  unique(x$parameterMetaSystem[x$event == "Time Signature"])
}

#' @export
#' @rdname read_midi
midi_key <- function(x){
  unique(x$parameterMetaSystem[x$event == "Key Signature"])
}

#' @export
#' @rdname read_midi
ticks_to_duration <- function(x, ticks_per_qtr = 480){
  y <- .tick_table(ticks_per_qtr)
  z <- y[match(x, y)]
  idx <- which(is.na(z))
  if(length(idx)){
    z[idx] <- sapply(x[idx], function(x) y[which.min(abs(y - x))])
    names(z)[idx] <- names(y)[match(z[idx], y)]
  }
  names(z)
}

#' @export
#' @rdname read_midi
duration_to_ticks <- function(x, ticks_per_qtr = 480){
  x <- .uncollapse(x)
  y <- .tick_table(ticks_per_qtr)
  z <- as.integer(y[match(x, names(y))])
  if(any(is.na(z))) stop("Invalid durations found.", call. = FALSE)
  z
}

.tick_table <- function(r = 480){
  x <- r * 2 ^ (-3:2)
  dot1 <- x[-6] + r * 2 ^ (-4:0)
  dot2 <- dot1[-6] + r * 2 ^ (-5:-1)
  trp <- (2 / 3) * x[-6]
  d <- c(32, 16, 8, 4, 2, 1)
  x <- c(x, dot1, dot2, trp)
  names(x) <- c(d, paste0(d[-6], "."), paste0(d[-6], ".."), paste0("t", d[-6]))
  sort(x)
}
