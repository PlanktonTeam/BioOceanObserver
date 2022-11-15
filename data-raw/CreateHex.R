# Some code to create a hex sticker for the IMOS BOO project
#
# Last updated: Monday 14th November 2022
#
# Jason D. Everett (UQ/CSIRO/UNSW)


# For installation instructions see here:
# https://github.com/GuangchuangYu/hexSticker

# devtools::install_github("GuangchuangYu/hexSticker")

## Settings:
col_bg_1 <- "#1E8BC3"      ## Summer Sky
col_bg_2 <- "#2574A9"      ## Jelly Bean
col_bg_3 <- "#4B77BE"      ## Steel Blue
col_border_1 <- "#6BB9F0"  ## Malibu
col_border_2 <- "#5C97BF"  ## Fountain Blue
col_border_3 <- "#89C4F4"  ## Jordy Blue


library(hexSticker)

sticker("20170908_163800_removal.png", 
        package="BOO", 
        p_size=5, 
        p_y = 1.7,
        s_x=1, 
        s_y=.75, 
        s_width=.8,
        h_fill = col_bg_3, 
        h_color = col_border_3,
        url = "Biological Ocean Observer",
        u_color = "white",
        filename="BOO_Hex.png")



sticker("IMOS_Crop.png", 
        package="", 
        p_size=5, 
        p_y = 1.7,
        s_x=1, 
        s_y=1, 
        s_width=0.92,
        s_height=0.4,
        h_fill = "#3B6E8F",
        h_color = "white",
        url = "www.imos.org.au",
        u_color = "white",
        u_family = "sans",
        dpi = 800,
        asp = 1,
        filename="IMOS_Hex.png")


sticker("copepod2_rotate22.svg", 
        package="planktonr", 
        p_y = 0.95,
        p_color = "white",
        p_size = 8,
        s_x=0.81, 
        s_y=1.05, 
        s_width=0.75,
        # s_height=2.5,
        h_fill = "#54bceb", # "#3B6E8F",
        h_color = "white",
        url = "https://github.com/PlanktonTeam/planktonr",
        u_color = "white",
        u_family = "sans",
        u_size = 1.2,
        dpi = 1000,
        asp = 1,
        filename="planktonr_Hex.png")



hexSticker::sticker("copepod2_rotate22.svg", 
                    package="planktonr", 
                    p_y = 0.95,
                    p_color = "white",
                    p_size = 8,
                    s_x=0.81, 
                    s_y=1.05, 
                    s_width=0.75,
                    # s_height=2.5,
                    h_fill = "#54bceb", # "#3B6E8F",
                    h_color = "white",
                    url = "https://github.com/PlanktonTeam/planktonr",
                    u_color = "white",
                    u_family = "sans",
                    u_size = 1.2,
                    dpi = 1000,
                    asp = 1,
                    filename="planktonr_Hex.png")
