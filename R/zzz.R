.onLoad <- function(lib, pkg){
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
}
