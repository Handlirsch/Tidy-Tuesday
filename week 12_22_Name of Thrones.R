##%######################################################%##
#                                                          #
####               My third tidy tuesday                ####
#                       23.03.2022                         #
#                   Author: Rebecca Rau                    #
#                                                          #
##%######################################################%##

# LIBRARY ----
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(jpeg)
library(ggtext)
library(showtext)


# DATA ----
tuesdata <- tidytuesdayR::tt_load(2022, week = 12)
babynames <- tuesdata$babynames


# NAME OF THRONES ----

# names I want to look at:
got <- tribble(
  ~name, ~colourr, 
  "Arya", "#CCCCCC",
  "Maisie", "#CCCCCC", 
  "Sansa", "#CCCCCC",
  "Bran", "#CCCCCC", 
  "Kit", "#CCCCCC", 
  "Lyanna", "#CCCCCC", 
  "Tyrion", "#C29BOC",
  "Daenerys", "#B62203",
  "Khaleesi", "#B62203",
  "Theon", "#FEE227",
  "Yara", "#FEE227",
  "Jorah", "#B62203")

# colour codes
gotcol <- c("#CCCCCC", "#CCCCCC", "#B62203", "#B62203", "#B62203", "#CCCCCC", "#CCCCCC",
            "#CCCCCC", "#CCCCCC", "#FEE227", "#C29BOC", "#FEE227")
gotcol2 <- c("gray77", "gray77", "darkred", "darkred", "darkred", "gray77", "gray77",
             "gray77", "gray77", "lightgoldenrod1", "red", "lightgoldenrod1")


# Annotations
plot_titles <- list(
  title = "Name  of  Thrones", 
  subtitle = "Overview of the popularity of some names (and a title) in the series, 
  respectively the name of the actors in the USA between 1880 and 2017. 
  The coloured area marks the period during which the series was aired. 
  The dotted line marks the date of the first publication of the first book entitled 
  \"A Game of Thrones\" from the series \"Song of Ice and Fire\", on which the 
  television series is based.
  Colours based on the colours of the associated royal house of the characters in the series.
  \nPlease note the different scaling of the y-axis.",
  caption = "**Source:** {babynames} R package, Hadley Wickham | **Visualization:** Rebecca Rau @itsrebeccarau"
)

# get font
#font_add(family = "GOT-font", regular = "/Library/Fonts/Game-of-Thrones.ttf")

babynames %>% 
  filter(name %in% got$name) %>% 
  summary()

showtext_auto()

babynames %>% 
  filter(name %in% got$name) %>%
  ggplot(aes(year, n, colour = name)) +
  # rectangle for TV airing
  geom_rect(aes(xmin = 2011, xmax = 2019, ymin = -Inf, ymax = Inf), 
            inherit.aes = FALSE, 
            stat = "unique", fill = "white", alpha = 0.2) +
  # line for publishing of first book
  geom_vline(xintercept = 1996, colour = "white", linetype = "dotted") +
  # line: name popularity
  geom_line(size = 0.8) +
  scale_colour_manual(values = gotcol2) +
  scale_y_continuous() +
  coord_cartesian(xlim = c(NA, max(babynames$year))) +
  facet_wrap(vars(name), nrow = 4, scales = "free_y") +
  labs(
    title = plot_titles$title,
    subtitle = plot_titles$subtitle,
    caption = plot_titles$caption
  ) +
  theme_minimal()+
  theme(
    plot.background = element_rect(colour = NA, fill = "dimgray"),
    panel.grid.major = element_line(colour = "gray70", size = 0.1),
    panel.grid.minor.x = element_line(colour = "gray70", size = 0.075),
    panel.grid.minor.y = element_line(colour = "gray70", size = 0.075),
    strip.text = element_markdown( colour = "white",
                                   size = 15, hjust = 0, lineheight = 0.8,
                                   margin = margin(t = 12, b = 4)),
    plot.title = element_text(family = "GOT-font", face = "bold", size = 30, colour = "white", hjust = 0.5),
    plot.subtitle = element_textbox_simple(size = 15, margin = margin(t = 30, b = 30), colour = "white", hjust = 0.5),
    plot.caption = element_markdown(colour = "gray84", hjust = 1, margin = margin(t = 25), size = 12),
    legend.position = "none",
    plot.margin = margin(t = 8, b = 25, l = 20, r = 40),
    axis.title = element_blank(),
    axis.text = element_text(colour = "gray73"),
    text = element_text(colour = "red" )
  )


# save the plot
ggsave("pictures/name-of-thrones.png")
