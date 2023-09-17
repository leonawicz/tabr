#install.packages("hexSticker")
library(hexSticker)
library(ggplot2)
img <- "data-raw/hexsubplot.png"
out <- "data-raw/tabr.png"
mult <- 4 # multiplier for larger sticker size, prevents pixelated subplot image text.

sticker(img, 1, 0.75, 0.65, 0.85, "tabr", p_y = 1.4, p_size = mult * 48, h_size = mult * 1.2, h_fill = "firebrick1",
        h_color = "gray20", url = "leonawicz.github.io/tabr", u_color = "white", u_size = mult * 3,
        filename = out)

# overwrite file for larger size
ggsave(out, width = mult * 43.9, height = mult * 50.8, bg = "transparent", units = "mm")

# store a smaller version (600px width) at man/figures/logo.png
