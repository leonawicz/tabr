# nolint start

.onLoad <- function(lib, pkg){
  .tabr_env$opts <- list(dev = "pdf", midi = TRUE)
  lp_path <- Sys.which("lilypond")
  if(lp_path == ""){
    lp_path <- "lilypond"
    if(.Platform$OS.type == "windows"){
      opts <- c("C:/Program Files (x86)/LilyPond/usr/bin/lilypond.exe",
                "C:/Program Files/LilyPond/usr/bin/lilypond.exe")
      if(file.exists(opts[1])) lp_path <- opts[1]
      if(file.exists(opts[2])) lp_path <- opts[2]
    }
  }
  tabr_options(lilypond = lp_path)
}

# nolint end
