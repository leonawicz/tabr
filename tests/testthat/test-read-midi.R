context("read midi")

test_that("Read midi files as expected", {
  if(require(tuneR)){
    file <- system.file("example.mid", package = "tabr")

    x <- read_midi(file)
    expect_equal(dim(x), c(40, 12))
    expect_equal(dim(midi_metadata(x)), c(16, 12))
    expect_equal(midi_time(x), "4/4, 18 clocks/tick, 8 1/32 notes / 24 clocks")
    expect_equal(midi_key(x), "C major")

    x <- midi_notes(x, channel = 1, track = 3, noteworthy = FALSE)
    expect_equal(dim(x), c(6, 8))
    expect_equal(
      names(x),
      c("time", "length", "duration", "pitch", "semitone", "velocity",
        "channel", "track")
    )

    file <- system.file("example2.mid", package = "tabr")
    x <- read_midi(file)
    x <- midi_notes(x, channel = 0, track = 2)
    expect_equal(dim(x), c(14, 2))
    expect_equal(names(x), c("duration", "pitch"))
  }
})
