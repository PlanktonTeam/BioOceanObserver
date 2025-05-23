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

# sticker("data-raw/IMOS_Cropped.png", #"data-raw/IMOS_logo-stacked-reversed-01.png"
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
#         u_size = 15,
#         u_x = 0.999,
#         u_y = 0.1,
#         # u_family = "sans",
#         dpi = 800,
#         asp = 1,
#         filename="data-raw/IMOS_Hex.png")


hexSticker::sticker("data-raw/Sapphirina_m_NSI_20161116_5x_jup_8st_ED_crop.JPG", 
                    package="Biological\nOcean Observer", 
                    p_y = 1.44,
                    p_x = 1,
                    p_color = "white",
                    p_size = 58,
                    p_fontface = "bold",
                    s_x = 1, 
                    s_y = 0.78, 
                    s_width = 0.80,
                    s_height = 0.80,
                    h_fill = "black",
                    h_color = "#191919", # "#171717",# "#aaa9ad", #"#404c50",# "#29338a", "#36c3e2",# "#048bb8",# "#0f1838", #"#4e3d52", #"#36444d",
                    url = "shiny.csiro.au/BioOceanObserver",
                    u_color = "white",
                    # u_family = "sans",
                    u_size = 17.2,
                    u_x = 0.965,
                    u_y = 0.07,
                    dpi = 1000,
                    asp = 1,
                    filename="inst/app/www/BOO_Hex.png", 
                    # white_around_sticker = TRUE,
                    lineheight = 0.1)

# 
# # Code from https://github.com/GuangchuangYu/hexSticker/issues/39
# library(magick)
# p <- image_read("inst/app/www/BOO_HexRaw.png")
# 
# pp <- p %>%
#   image_fill(color = "transparent", refcolor = "white", fuzz = 50, point = "+0+1") %>%
#   image_fill(color = "transparent", refcolor = "white", fuzz = 50, point = "+517+0") %>%
#   image_fill(color = "transparent", refcolor = "white", fuzz = 50, point = "+0+599") #%>%
#   # image_fill(color = "transparent", refcolor = "white", fuzz = 4, point = "+517+599") # This one seems to find some text as well
# 
# image_write(image = pp, path = "inst/app/www/BOO_Hex.png")

