##%######################################################%##
#                                                          #
####          My first tidy tuesday - week 8            ####
####                      22.02.2022                    ####
####                 Author: Rebecca Rau                ####
#                                                          #
##%######################################################%##

# LIBRARIES ----

# load requires libraries
library(tidytuesdayR)
library(tidyr)
library(dplyr)
library(ggplot2)
library(viridis)
library(ggtext)
library(patchwork)


# DATA ----

# load data
tuesdata <- tidytuesdayR::tt_load(2022, week = 8)
freedom <- tuesdata$freedom

# understand data
str(freedom)
summary(freedom)
unique(freedom$Region_Name)

# divide the americas
americas <- subset(freedom, Region_Name == "Americas", select = country)
americas <- unique(americas)
south_america <- c("Argentina", "Bolivia (Plurinational State of)", "Brazil", "Chile",
                   "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", 
                   "Uruguay", "Venezuela (Bolivarian Republic of)")
freedom_renamed <- mutate(freedom, Region_Name = ifelse(country == south_america, "south america", Region_Name))
freedom_renamed <- mutate(freedom_renamed, Region_Name = ifelse(Region_Name == "Americas", "north america", Region_Name))
unique(freedom_renamed$Region_Name)

# prep data
region <- freedom_renamed %>%
  group_by(Region_Name, year) %>%
  summarise(
    CL = mean(CL),
    PR = mean(PR)
  )

freedom_renamed %>% filter(Region_Name == "south america") %>%  summary()


# VISUALISATION ---- 

# colours
magma <- magma(7)
magma[2] <- "white"
magma[7] <- "#2D1160FF"
magma

# civil liberties
cl_reg <- ggplot(region, aes(year, CL, colour = Region_Name)) +
   geom_jitter(data = freedom, size = 0.5, alpha = 0.3) +
   geom_line(size = 1.5) +
   scale_y_reverse() +
   scale_color_manual(values = magma) +
   ylab("CIVIL LIBERTIES") +
  
   annotate(geom = "text", label = "Asia", x = 2021.7, y = 4.8, colour = "#721F81FF", size = 7 ) +    # maybe read the y value from the table?
   annotate(geom = "text", label = "Africa", x = 2021.9, y = 4.4, colour = "#000004FF", size = 7) +
   annotate(geom = "text", label = "South America", x = 2024.1, y = 3, colour = "#2D1160FF", size = 7) +
   annotate(geom = "text", label = "North America", x = 2024.1, y = 2.6, colour = "#F1605DFF", size = 7) +
   annotate(geom = "text", label = "Europe", x = 2022.2, y = 2.1, colour = "#B63679FF", size = 7) +
   annotate(geom = "text", label = "Oceania", x = 2022.4, y = 1.5, colour = "#FEAF77FF", size = 7) +
   annotate(geom = "text", label = "high", x = 1996, y = 1, size = 9, fontface = "bold") +
   annotate(geom = "text", label = "low", x = 1996, y = 7, size = 9, fontface = "bold") +
   
   theme_minimal() +
   theme(panel.grid = element_blank(),
         axis.title.y = element_text(size = 30, colour = "gray50"),
         axis.text.y = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         legend.position = "None")

# political rights
pr_reg <- ggplot(region, aes(year, PR, colour = Region_Name)) +
  geom_jitter(data = freedom, size = 0.5, alpha = 0.3) +
  geom_line(size = 1.5) +
  scale_y_reverse() +
  scale_color_manual(values = magma) +
  ylab("POLITICAL RIGHTS") +
  labs(caption = expression(paste("\n", bold("Data: "), "Freedom House  ", bold("|  Plot: "), "@itsrebeccarau"))) +
  
  annotate(geom = "text", label = "dots represent the scores per county and year\nlines indicate the mean per continent", 
           x = 2014, y = 6.6, fontface = "bold.italic") +
  
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(size = 30, colour = "gray50"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_blank(),
        legend.position = "NONE",
        plot.caption = element_text(size = 20))


# combine plots
combination <- cl_reg/pr_reg

ggsave("week-8.png", width = 10, height = 15, dpi = 300)

