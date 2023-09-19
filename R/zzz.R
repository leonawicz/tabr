.onAttach <- function(lib, pkg){
  x <- c("lilypond", "midi2ly", "python")
  lp_path <- as.character(Sys.which(x[1]))
  ml_path <- as.character(Sys.which(x[2]))
  py_path <- as.character(Sys.which(x[3]))
  is_win <- .Platform$OS.type == "windows"
  if(lp_path != "" & ml_path == ""){
    ml_path2 <- gsub("lilypond(\\.exe|)", "midi2ly", lp_path)
    if(file.exists(ml_path2)){
      ml_path <- ml_path2
    } else {
      ml_path2 <- gsub("lilypond(\\.exe|)", "midi2ly.py", lp_path)
      if(file.exists(ml_path2)) ml_path <- ml_path2
    }
  }

  tabr_options(lilypond = lp_path, midi2ly = ml_path, python = py_path)

  msg <- paste0(utils::capture.output(tabr_lilypond_api(), type = "message"), "\n")

  no_lp <- gsub("\\.$", " (only required for transcription).", .lp_not_found)
  if(lp_path == ""){
    msg <- paste0(msg, no_lp)
  } else {
    x <- tryCatch(
      system(paste(lp_path, "--version"), intern = TRUE),
      error = function(e) .lp_not_found
    )
    x <- x[1]
    if(x == .lp_not_found){
      msg <- paste0(msg, no_lp)
    } else {
      x <- gsub("^GNU LilyPond (\\d+.\\d+.\\d+).*", "\\1", x, perl = TRUE)
      msg <- paste0(msg, "Local installation detected: LilyPond ", x)
    }
  }

  packageStartupMessage(msg)
}
