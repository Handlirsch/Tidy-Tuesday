##%######################################################%##
#                                                          #
####               My third tidy tuesday                ####
#                       22.03.2022                         #
#                   Author: Rebecca Rau                    #
#                                                          #
##%######################################################%##

# LOBRARY ----
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
maori <- tuesdata$maorinames
births <- tuesdata$births
nz_names <- tuesdata$nz_names
nz_births <- tuesdata$nz_births


# INFO ABOUT DATA ----
summary(nz_names)    # from 1900 until 2020
summary(maori)
summary(babynames)
summary(nz_births)    # from 1935 until 2020


# MY IDEA ----
# I want to plot the number of births vs the number of unique Babynames in NZ
# (maybe separated by sex?)
# plot from 1971 onwards, since there's differentiation between M and F
# maybe show the 10 all time favorites for m and f?
# and the 10 fewest names?


# DATAWRANGLING ----
# filter only births after 1971
nz_births_new <- nz_births %>% filter(Year >= 1971) %>% glimpse()
nz_names_new <- nz_names %>% filter(Year >= 1971) %>% glimpse()

# summarise the overall unique names
nz_distinct_year <- nz_names_new %>% group_by(Year) %>% summarise(n_distinct(Name))
names(nz_distinct_year)[2] <- "Unique_Names"

# summarise the unique names per sex
nz_distinct_sex <- nz_names_new %>% group_by(Year, Sex) %>% summarise(n_distinct(Name))
names(nz_distinct_sex)[3] <- "Unique_Names"

# merge nz_births_new with Uniqnames_m and Uniquenames_f and total Unique names
nz_births_new$Uniqnames_m <- nz_distinct_sex %>% group_by(Year) %>% filter(Sex == "Male") %>% pull(Unique_Names) 
nz_births_new$Uniqnames_f <- nz_distinct_sex %>% group_by(Year) %>% filter(Sex == "Female") %>% pull(Unique_Names)
nz_births_new$Total_Unique <- nz_distinct_year$Unique_Names

# Number of Names used for both Sexes
sumfm <- nz_births_new$Uniqnames_f + nz_births_new$Uniqnames_m
nz_births_new$Both_sex_names <- sumfm - nz_births_new$Total_Unique


# PLOTTING ----
# Births in NZ from 1971 onwards
ggplot(data = nz_births_new, aes(Year, Total)) +
  geom_line() +
  geom_line(data = nz_births_new, aes(Year, Male), colour = "blue") +
  geom_line(data = nz_births_new, aes(Year, Female), colour = "red")
 
# Unique Names from 1971 onwards
ggplot(data = nz_distinct_sex, aes(Year, Unique_Names, fill = Sex)) +
  geom_bar(position = "dodge", stat = "identity")

# combination
ggplot(data = nz_births_new) +
  geom_line(aes(Year, Total)) +
  geom_line(aes(Year, Male), colour = "blue") +
  geom_line(aes(Year, Female), colour = "red") +
  geom_col(aes(Year, 40*Uniqnames_f), fill = "red", alpha = 0.5) +    # 40*... is added so the size in comparison with the lines is different
  geom_col(aes(Year, 40*Uniqnames_m), fill = "blue", alpha = 0.5) +
 
  
  scale_x_continuous(n.breaks = 12) +
  scale_y_continuous(sec.axis = sec_axis(~./40, name = "Number of unique Names\n")) +    # added a second y-axis for the columns
  labs(title = "Births and unique baby names in Aotearoa/New Zealand since 1971\n", 
       x = "", y = "Number of Births\n", caption = "\nData: nzbabynames package from Emily Kothe | Plot: @itsrebeccarau") +
  

  theme_bw() +
  theme(plot.title = element_text(size = 22, hjust = 0.5),
        plot.caption = element_text(size = 13, colour = "grey42", family = "GOT-font"),
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 14)
  )      
  


# inspired by Topenomics
# Rebecca & Lena over the years in NZ
nz_names %>% filter(Name == "Rebecca") %>% 
  ggplot(aes(x = Year, y = Count)) +
  geom_line()

nz_names %>% filter(Name == "Lena") %>% 
  ggplot(aes(x = Year, y = Count)) +
  geom_line()



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
