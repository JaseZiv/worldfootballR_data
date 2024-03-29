
# install.packages("ggsoccer")
library(hexSticker)
library(ggplot2)
library(ggsoccer)

setwd(here::here("man"))
sysfonts::font_add_google(name = "Chivo", family = "chivo")
sysfonts::font_add_google(name = "Play", family = "play")

pitch <- ggplot() +
  annotate_pitch(fill = "#538032", colour = "grey30") +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "#538032"))


pitch <- pitch + theme_void() + theme_transparent()


sticker(pitch,
        package="worldfootballR data",
        p_family = "play", p_size=11, p_color = "grey30",
        s_x=1, s_y=.8, s_width=1.3, s_height=0.85,
        h_fill = "#538032", h_color = "grey30",
        url = "https://github.com/JaseZiv/worldfootballR_data", u_y = 0.07, u_x = 1.0, u_size = 3, u_color = "white", u_family = "play",
        filename="figures/logo.png")

# smaller size hex logo:
sticker(pitch,
        package="worldfootballR_data",
        p_family = "play", p_size=11, p_color = "grey30",
        s_x=1, s_y=.8, s_width=1.3, s_height=0.85,
        h_fill = "#538032",
        url = "hhttps://github.com/JaseZiv/worldfootballR_data", u_y = 0.07, u_x = 1.0, u_size = 3, u_color = "white", u_family = "play",
        filename="figures/logo_small_size.png") # modify size in viewer to dimensions 181x209 as a png


###########################################################################
# Different Options: ------------------------------------------------------

# sticker(pitch,
#         package="worldfootballR",
#         p_size=6, p_color = "white",
#         s_x=1, s_y=.8, s_width=1.3, s_height=0.85,
#         h_fill = "#538032",
#         url = "https://jaseziv.github.io/worldfootballR/", u_y = 0.09, u_x = 1.05, u_size = 1.2, u_color = "white",
#         filename="man/figures/logo_standard.png")
#
#
# sticker(pitch,
#         package="worldfootballR",
#         p_family = "chivo",
#         p_size=6, p_color = "white",
#         s_x=1, s_y=.8, s_width=1.3, s_height=0.85,
#         h_fill = "#538032",
#         url = "https://jaseziv.github.io/worldfootballR/", u_y = 0.07, u_x = 1.0, u_size = 1.2, u_color = "white", u_family = "chivo",
#         filename="man/figures/logo_chivo.png")
#
#
# sticker(pitch,
#         package="worldfootballR",
#         p_family = "play",
#         p_size=6, p_color = "white",
#         s_x=1, s_y=.8, s_width=1.3, s_height=0.85,
#         h_fill = "#538032",
#         h_color = "black",
#         spotlight = T, l_y = 0.83,
#         url = "https://jaseziv.github.io/worldfootballR/", u_y = 0.07, u_x = 1.0, u_size = 1.2, u_color = "white", u_family = "play",
#         filename="man/figures/logo_play_black_border.png")


