# nolint start

.onLoad <- function(lib, pkg){
  .tabr_env$opts <- list(dev = "pdf", midi = TRUE)
  x <- c("lilypond", "midi2ly.py", "python")
  win_paths <- c("C:/Program Files (x86)/LilyPond/usr/bin", "C:/Program Files/LilyPond/usr/bin")
  lp_path <- Sys.which(x[1])
  ml_path <- Sys.which(x[2])
  py_path <- Sys.which(x[3])
  if(lp_path == ""){
    lp_path <- x[1]
    if(.Platform$OS.type == "windows"){
      opts <- file.path(win_paths, "lilypond.exe")
      if(file.exists(opts[1])) lp_path <- opts[1]
      if(file.exists(opts[2])) lp_path <- opts[2]
    }
  }
  if(ml_path == ""){
    ml_path <- x[2]
    if(.Platform$OS.type == "windows"){
      opts <- file.path(win_paths, "midi2ly.py")
      if(file.exists(opts[1])) ml_path <- opts[1]
      if(file.exists(opts[2])) ml_path <- opts[2]
    }
  }
  if(py_path == ""){
    py_path <- x[3]
    if(.Platform$OS.type == "windows"){
      opts <- file.path(win_paths, "python.exe")
      if(file.exists(opts[1])) py_path <- opts[1]
      if(file.exists(opts[2])) py_path <- opts[2]
    }
  }
  tabr_options(lilypond = lp_path, midi2ly = ml_path, python = py_path)
}

# nolint end
