# nolint start

.onLoad <- function(lib, pkg){
  .tabr_env$opts <- list(dev = "pdf", midi = TRUE)
  x <- c("lilypond", "midi2ly.py", "python")
  win_paths <- c("C:/Program Files (x86)/LilyPond/usr/bin", "C:/Program Files/LilyPond/usr/bin")
  lp_path <- as.character(Sys.which(x[1]))
  ml_path <- as.character(Sys.which(x[2]))
  py_path <- as.character(Sys.which(x[3]))
  is_win <- .Platform$OS.type == "windows"
  if(is_win){
    if(lp_path == ""){
      opts <- file.path(win_paths, "lilypond.exe")
      if(file.exists(opts[1])) lp_path <- opts[1]
      if(file.exists(opts[2])) lp_path <- opts[2]
    }
    if(ml_path == "" ){
      opts <- file.path(win_paths, "midi2ly.py")
      if(file.exists(opts[1])) ml_path <- opts[1]
      if(file.exists(opts[2])) ml_path <- opts[2]
    }
    if(py_path == ""){
      opts <- file.path(win_paths, "python.exe")
      if(file.exists(opts[1])) py_path <- opts[1]
      if(file.exists(opts[2])) py_path <- opts[2]
    }
  } else {
    if(lp_path != "" & ml_path == ""){
      ml_path2 <- gsub("lilypond", "midi2ly.py", ml_path)
      if(file.exists(ml_path2)) ml_path <- ml_path2
    }
  }
  tabr_options(lilypond = lp_path, midi2ly = ml_path, python = py_path)
}

# nolint end
