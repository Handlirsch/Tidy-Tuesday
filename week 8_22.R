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
freedom_southamerica <- filter(freedom, Region_Name == "Americas", country %in% south_america)

# prep data
region <- freedom %>%
  group_by(Region_Name, year) %>%
  summarise(
    CL = mean(CL),
    PR = mean(PR)
  )


# VISUALISATION ---- 
# civil liberties
(cl_reg <- ggplot(region, aes(year, CL, colour = Region_Name)) +
   geom_jitter(data = freedom, size = 0.5, alpha = 0.2) +
   geom_line(size = 1.5) +
   scale_y_reverse() +
   
   annotate(geom = "text", label = "Asia", x = 2019, y = 5.2) +
   annotate(geom = "text", label = "Africa", x = 2019, y = 4) +
   annotate(geom = "text", label = "Americas", x = 2019, y = 2.9) +
   
   theme_minimal() +
   theme(panel.grid = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank()
         )
 )

# political rights
(pr_reg <- ggplot(region, aes(year, PR, colour = Region_Name)) +
    geom_line(size = 1.5)
  )

