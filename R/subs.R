.tunelab <- function(x){
  x <- gsub("[,']", "", x)
  x <- toupper(x)
  x <- gsub("#", "is", x)
  x <- gsub("_", "b", x)
  x
}

.octave_to_tick <- function(x){
  x <- gsub("0", ",,,", x)
  x <- gsub("1", ",,", x)
  x <- gsub("2", ",", x)
  x <- gsub("3", "", x)
  x <- gsub("4", "'", x)
  x <- gsub("5", "''", x)
  x <- gsub("6", "'''", x)
  x <- gsub("7", "''''", x)
  x <- gsub("8", "'''''", x)
  x <- gsub("9", "''''''", x)
  x
}

.octave_to_int <- function(x){
  x <- gsub(",,,", "0", x)
  x <- gsub(",,", "1", x)
  x <- gsub(",", "2", x)
  x <- gsub("''''''", "9", x)
  x <- gsub("'''''", "8", x)
  x <- gsub("''''", "7", x)
  x <- gsub("'''", "6", x)
  x <- gsub("''", "5", x)
  x <- gsub("'", "4", x)
  x
}

.notesub <- function(x, sharp = "#", flat = "_", simplify = FALSE){
  x <- gsub(sharp[1], "is", x)
  x <- gsub(flat[1], "es", x)
  if(simplify) x <- gsub("ees", "es", gsub("aes", "as", x))
  x
}

.tabsub <- function(x){
  x <- strsplit(x, ";")[[1]]
  x[1] <- gsub("-", "\\\\glissando", x[1])
  x[1] <- gsub("x", "xDEADNOTEx", x[1])
  x[1] <- .notesub(x[1])
  x[1] <- gsub("\\]", "\\\\staccato", x[1])
  if(length(x) == 2){
    x[2] <- paste0(";", substr(x[2], 1, 1), gsub("_", " ", substring(x[2], 2)))
    x <- paste0(x, collapse = "")
  }
  x
}

.strsub <- function(x){
  x <- strsplit(as.character(x), " ")[[1]] %>%
    purrr::map_chr(.star_expand) %>%
    paste0(collapse = " ")
  x <- gsub("7s", "7654321", x)
  x <- gsub("6s", "654321", x)
  x <- gsub("5s", "54321", x)
  x <- gsub("4s", "4321", x)
  x <- gsub("3s", "321", x)
  f <- function(x){
    strsplit(gsub("\\(", " \\(", gsub("\\)", " ", x)), " ")[[1]] %>%
      purrr::map(~({
        if(substr(.x, 1, 1) == "(") substring(.x, 2) else strsplit(.x, "")[[1]]
      })) %>%
      unlist() %>%
      paste0(collapse = "_")
  }
  purrr::map_chr(strsplit(x, " ")[[1]], f)
}

.noterev <- function(x){
  purrr::map(strsplit(x, " ")[[1]], ~({
    x <- gsub("as", "aes", .x)
    x <- gsub("^es| es", "ees", x)
    x <- .split_chord(x)
    x <- gsub("is", "#", x)
    x <- gsub("^as|^aes", "a_", x)
    x <- gsub("^ees|^es", "e_", x)
    x <- gsub("es", "_", x)
    paste(x, collapse = "")
  })) %>%
    paste(collapse = " ")
}
