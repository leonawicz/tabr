context("read midi")

test_that("Read midi files as expected", {
  file <- system.file("example.mid", package = "tabr")
  if(require(tuneR)){
    x <- read_midi(file)
    expect_equal(dim(x), c(40, 12))
    expect_equal(dim(midi_metadata(x)), c(16, 12))
    expect_equal(midi_time(x), "4/4, 18 clocks/tick, 8 1/32 notes / 24 clocks")
    expect_equal(midi_key(x), "C major")

    x <- midi_notes(x, channel = 1, track = 3)
    expect_equal(dim(x), c(6, 7))
    expect_equal(
      names(x),
      c("time", "duration", "note", "semitone", "velocity", "channel", "track")
    )
  }
})
