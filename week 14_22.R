##%######################################################%##
#                                                          #
####                 My 4th tidy tuesday                ####
#                       05.04.2022                         #
#                   Author: Rebecca Rau                    #
#                                                          #
##%######################################################%##

# LIBRARY ----
library(ggplot2)
library(tidyr)
library(dplyr)
library(sysfonts)
library(showtext)
library(ggtext)


# DATA ----
tuesdata <- tidytuesdayR::tt_load('2022-04-05')
news_orgs <- tuesdata$news_orgs


# INFO ABOUT DATA ----
summary(news_orgs)


# MY IDEAS ----
# 1) Plot the number of news outlets founded per year
# 2) Plot the number of news outlets per country
# 3) Plot the primary language per country
# 4) Plot how many founders are still the owners



# number of news outlets per year ----

# Font
font_add_google("Lora")
showtext_auto(enable = TRUE)


# Titles
plot_titles <- list(
  title = "Founding years of current digital-native local\nnews organizations in Canada and the USA", 
  subtitle = "Platzhalter",
  caption = "**Source:** Project Oasis | **Visualization:** Rebecca Rau @itsrebeccarau"
)


# plot
news_orgs %>% 
  count(year_founded) %>% 
  ggplot(aes(year_founded, n)) +
  geom_area() +
  
  # add an arrow highlighting 2014
  geom_curve(aes(x = 2002, y = 64, xend = 2013.7, yend = 71),
             curvature = -0.5, 
             arrow = arrow(length = unit(0.02, "npc"))) +
  
  # change the limits and the displayed tick marks of the y-axis
  scale_y_continuous(limits = c(0,80), breaks = c(0, 20, 40, 60)) +
  labs(title = plot_titles$title, 
       subtitle = plot_titles$subtitle, 
       caption = plot_titles$caption) +
  
  # add comment to the arrow
  annotate(geom = "text", 
           label = "71 new digital-native local news\norganizations were founded in 2014",
           x = 1996,
           y = 61) +
  
  # remove axis titles and gain a bit more space on the left and bottom
  xlab("") +
  ylab("") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#FFFDF2D3"),
    panel.grid = element_line(size = 0.2),
    plot.title = element_text(family = "Lora", face = "bold", size = 23, 
                              lineheight = 1.3, hjust = 0.4,
                              margin = margin(t = 20, b = -42)),
    plot.subtitle = element_blank(),
    plot.caption = element_markdown(hjust = 1)
  )


# save plot
ggsave("pictures/week-14.png", width = 23.5, height = 22, units = "cm", dpi = 300)


