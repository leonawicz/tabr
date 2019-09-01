#' Convert MIDI to LilyPond file
#'
#' Convert a MIDI file (\code{.mid}) to a LilyPond format (\code{.ly}) text file.
#'
#' Under development/testing. See warning and details below.
#'
#' This function is a wrapper around the \code{midi2ly} command line utility provided by LilyPond. It inherits all the limitations thereof. LilyPond is not intended to be used to produce meaningful sheet music from arbitrary MIDI files.
#' \emph{A future version will offer additional arguments that use \code{tabr} to subsequently edit the generated LilyPond file as a second step, allowing the user to make some nominal substitutions or additions to the default output.}
#' While \code{\link{lilypond}} converts R code \code{score} objects to LilyPond markup directly, MIDI conversion to LilyPond markup by \code{midily} requires LilyPond.
#'
#' WARNING: Even though the purpose of the command line utility is to convert an existing MIDI file to a LilyPond file, it nevertheless generates a LilyPond file that \emph{specifies inclusion of MIDI output}.
#' This means when you subsequently process the LilyPond file with LilyPond or if you use \code{miditab} to go straight from your MIDI file to pdf output, the command line tool will also produce a MIDI file output. It will overwrite your original MIDI file if it has the same file name and location!
#' The next version of this function will add an default argument \code{midi_out = FALSE} to remove this from the generated LilyPond file. If \code{TRUE} and the basename of \code{midi_file} matches the basename of \code{file}, then \code{file} will be renamed, the basename appended with a \code{-1}.
#'
#' \code{allow_tuplets = NULL} to disallow all tuplets. Fourth, eighth and sixteenth note triplets are allowed. The format is a character vector where each element is \code{duration*numerator/denominator}, no spaces. See default argument.
#'
#' On Windows systems, it may be necessary to specify a path in tabr_options to both \code{midi2ly} and \code{python} if they are not already successfully set as follows.
#' On package load, \code{tabr} will attempt to check for \code{midi2ly.exe} at \code{C:/Program Files (x86)/LilyPond/usr/bin/midi2ly.py} and similarly for the \code{python.exe} that ships with LilyPond at \code{C:/Program Files (x86)/LilyPond/usr/bin/python.exe}.
#' If this is not where LilyPond is installed, then LilyPond and Python need to be provided to \code{tabr_options} or added to the system PATH variable.
#'
#' @param midi_file character, MIDI file (\code{.mid}). May include an absolute or relative path.
#' @param file LilyPond output file ending in \code{.ly}.
#' @param key key signature, defaults to \code{"c"}.
#' @param absolute logical, print absolute pitches.
#' @param quantize integer, duration, quantize notes on duration.
#' @param explicit logical, print explicit durations.
#' @param start_quant integer, duration, quantize note starts on the duration.
#' @param allow_tuplet character vector, allow tuplet durations. See details.
#' @param details logical, verbose detail.
#' @param lyric logical, treat all text as lyrics.
#' @param path character, optional output directory prefixed to \code{file}, may be an absolute or relative path. If \code{NULL} (default), only \code{file} is used.
#'
#' @return nothing returned; a file is written.
#' @export
#' @seealso \code{\link{miditab}}, \code{\link{tab}}, \code{\link{lilypond}}
#'
#' @examples
#' \dontrun{
#' if(tabr_options()$midi2ly != ""){
#'   midi <- system.file("example.mid", package = "tabr")
#'   outfile <- file.path(tempdir(), "out.ly")
#'   midily(midi, outfile) # requires LilyPond installation
#' }
#' }
midily <- function(midi_file, file, key = "c", absolute = FALSE, quantize = NULL, explicit = FALSE,
                   start_quant = NULL, allow_tuplet = c("4*2/3", "8*2/3", "16*2/3"), details = FALSE,
                   lyric = FALSE, path = NULL){
  x <- paste0("--key=", .midily_key(key))
  if(absolute) x <- paste(x, "--absolute-pitches")
  if(!is.null(quantize)) x <- paste(x, paste0("--duration-quant=", quantize))
  if(explicit) x <- paste(x, "--explicit-durations")
  if(!is.null(start_quant)) x <- paste(x, paste0("--start-quant=", start_quant))
  if(!is.null(allow_tuplet)){
    for(i in seq_along(allow_tuplet)) x <- paste(x, paste0("--allow-tuplet=", allow_tuplet[i]))
  }
  if(explicit) x <- paste(x, "--verbose")
  if(explicit) x <- paste(x, "--text-lyrics")
  system(paste0("\"", tabr_options()$python, "\" ", "\"", tabr_options()$midi2ly, "\" ", x,
                " --output=\"", .adjust_file_path(file, path)$lp, "\" \"", midi_file, "\""))
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
#' Convert a MIDI file to a pdf or png music score using the LilyPond music engraving program.
#' Output format is inferred from \code{file} extension. This function is a wrapper around \code{\link{midily}}, the function that converts the MIDI file to a LilyPond (\code{.ly}) file using a LilyPond command line utility.
#'
#' WARNING: Even though the purpose of the command line utility is to convert an existing MIDI file to a LilyPond file, it nevertheless generates a LilyPond file that \emph{specifies inclusion of MIDI output}.
#' This means when you subsequently process the LilyPond file with LilyPond or if you use \code{miditab} to go straight from your MIDI file to pdf output, the command line tool will also produce a MIDI file output. It will overwrite your original MIDI file if it has the same file name and location!
#' The next version of this function will add an default argument \code{midi_out = FALSE} to remove this from the generated LilyPond file. If \code{TRUE} and the basename of \code{midi_file} matches the basename of \code{file}, then \code{file} will be renamed, the basename appended with a \code{-1}.
#'
#' On Windows systems, it may be necessary to specify a path in tabr_options to both \code{midi2ly} and \code{python} if they are not already successfully set as follows.
#' On package load, \code{tabr} will attempt to check for \code{midi2ly.exe} at \code{C:/Program Files (x86)/LilyPond/usr/bin/midi2ly.py} and similarly for the \code{python.exe} that ships with LilyPond at \code{C:/Program Files (x86)/LilyPond/usr/bin/python.exe}.
#' If this is not where LilyPond is installed, then LilyPond and Python need to be provided to \code{tabr_options} or added to the system PATH variable.
#'
#' @param midi_file character, MIDI file (\code{.mid}). May include an absolute or relative path.
#' @param file character, output file ending in .pdf or .png.
#' @param keep_ly logical, keep LilyPond file.
#' @param path character, optional output directory prefixed to \code{file}, may be an absolute or relative path. If \code{NULL} (default), only \code{file} is used.
#' @param details logical, set to \code{FALSE} to disable printing of log output to console.
#' @param ... additional arguments passed to \code{\link{midily}}.
#'
#' @return nothing returned; a file is written.
#' @export
#' @seealso \code{\link{midily}}, \code{\link{tab}}, \code{\link{lilypond}}
#'
#' @examples
#' \dontrun{
#' if(tabr_options()$midi2ly != ""){
#'   midi <- system.file("example.mid", package = "tabr")
#'   outfile <- file.path(tempdir(), "out.pdf")
#'   miditab(midi, outfile, details = FALSE) # requires LilyPond installation
#' }
#' }
miditab <- function(midi_file, file, keep_ly = FALSE, path = NULL, details = TRUE, ...){
  fp <- .adjust_file_path(file, path)
  if(details) cat("#### Engraving midi to", fp$tp, "####\n")
  do.call(midily, c(list(midi_file = midi_file, file = basename(fp$lp), path = dirname(fp$lp)), list(...)))
  lp_path <- tabr_options()$lilypond
  is_windows <- Sys.info()[["sysname"]] == "Windows"
  if(lp_path == "" && is_windows) lp_path <- "lilypond.exe"
  call_string <- paste0("\"", lp_path, "\" --", fp$ext, " -dstrip-output-dir=#f \"", fp$lp, "\"")
  if(is_windows){
    system(call_string, show.output.on.console = details)
  } else {
    system(call_string)
  }
  if(!keep_ly) unlink(fp$lp)
  invisible()
}
