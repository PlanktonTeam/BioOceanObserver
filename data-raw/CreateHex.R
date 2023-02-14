# Some code to create a hex sticker for the IMOS BOO project
#
# Last updated: Monday 17th November 2022
#
# Jason D. Everett (UQ/CSIRO/UNSW)

# For installation instructions see here:
# https://github.com/GuangchuangYu/hexSticker

# devtools::install_github("GuangchuangYu/hexSticker")

library(hexSticker)
# 
# 
# sticker("IMOS_Crop.png", 
#         package="", 
#         p_size=5, 
#         p_y = 1.7,
#         s_x=1, 
#         s_y=1, 
#         s_width=0.92,
#         s_height=0.4,
#         h_fill = "#3B6E8F",
#         h_color = "white",
#         url = "www.imos.org.au",
#         u_color = "white",
#         u_family = "sans",
#         dpi = 800,
#         asp = 1,
#         filename="IMOS_Hex.png")


hexSticker::sticker("data-raw/Sapphirina_m_NSI_20161116_5x_jup_8st_ED.JPG", 
                    package="Biological\n Ocean Observer", 
                    p_y = 1.5,
                    p_color = "white",
                    p_size = 45,
                    s_x = 1, 
                    s_y = 0.85, 
                    s_width = 0.9,
                    h_fill = "black",
                    h_color = "white",
                    url = "https://shiny.csiro.au/BioOceanObserver",
                    u_color = "white",
                    u_family = "sans",
                    u_size = 12,
                    u_x = 1.04,
                    u_y = 0.1,
                    dpi = 1000,
                    asp = 1,
                    filename="inst/app/www/BOO_HexRaw.png", 
                    white_around_sticker = TRUE,
                    lineheight = 0.1)


# Code from https://github.com/GuangchuangYu/hexSticker/issues/39
library(magick)
p <- image_read("inst/app/www/BOO_HexRaw.png")

pp <- p %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = 50, point = "+0+0") %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = 50, point = "+517+0") %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = 50, point = "+0+599") #%>%
  # image_fill(color = "transparent", refcolor = "white", fuzz = 4, point = "+517+599") # This one seems to find some text as well

image_write(image = pp, path = "inst/app/www/BOO_Hex.png")

