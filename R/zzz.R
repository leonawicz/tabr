.onLoad <- function(lib, pkg){
  .tabr_env$opts <- list(dev = "pdf", midi = TRUE)
  lp_path <- Sys.which("lilypond")
  if(lp_path == ""){
    tabr_options(lilypond = "lilypond")
  } else {
    tabr_options(lilypond = lp_path)
  }
}
