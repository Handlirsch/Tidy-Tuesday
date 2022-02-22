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
# civil liberties
(cl_reg <- ggplot(region, aes(year, CL, colour = Region_Name)) +
   geom_jitter(data = freedom, size = 0.5, alpha = 0.2) +
   geom_line(size = 1.5) +
   scale_y_reverse() +
   scale_colour_grey() +
   # change the colours. Make them colourblind-friendly!
   
   annotate(geom = "text", label = "Asia", x = 2021.8, y = 4.8) +    # maybe read the y value from the table?
   annotate(geom = "text", label = "Africa", x = 2022, y = 4.4) +
   annotate(geom = "text", label = "South America", x = 2024, y = 3) +
   annotate(geom = "text", label = "North America", x = 2024, y = 2.6) +
   annotate(geom = "text", label = "Europe", x = 2022.3, y = 1.9) +
   annotate(geom = "text", label = "Oceania", x = 2022.4, y = 1.7) +    # add the right colours to the labels!
   annotate(geom = "text", label = "high", x = 1996, y = 1, size = 9, fontface = "bold") +    # make them bold!
   annotate(geom = "text", label = "low", x = 1996, y = 7, size = 9, fontface = "bold") +
   
   theme_minimal() +
   theme(panel.grid = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.text.x = element_blank()
         # get rid of the legend all togehter!
         )
 )

# political rights
(pr_reg <- ggplot(region, aes(year, PR, colour = Region_Name)) +
    geom_line(size = 1.5)
  )

