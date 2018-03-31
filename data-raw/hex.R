#install.packages("hexSticker")
library(hexSticker)
library(ggplot2)
img <- "data-raw/hexsubplot.png"
out <- "data-raw/tabr.png"
mult <- 4 # multiplier for larger sticker size, prevents pixelated subplot image text.

sticker(img, 1, 0.8, 0.8, 1, "tabr", p_size = mult * 48, h_size = mult * 1.2, h_fill = "firebrick1",
        h_color = "gray20", url = "leonawicz.github.io/tabr", u_color = "white", u_size = mult * 3,
        filename = out)

# overwrite file for larger size
ggsave(out, width = mult*43.9, height = mult*50.8, bg = "transparent", units = "mm")

# store a smaller version (400px width) of this higher quality image output at inst/tabr.png
