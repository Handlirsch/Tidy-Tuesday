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

# prep data
region <- freedom %>%
  group_by(Region_Name, year) %>%
  summarise(
    CL = mean(CL),
    PR = mean(PR)
  )

countries <- freedom %>% 
  group_by(country, year) #%>% 
summarise(
  CL = mean(na.omit(CL)),
  PR = mean(na.omit(PR))
)

asia <- filter(freedom, Region_Name == "Asia")
oceania <- filter(freedom, Region_Name == "Oceania")
unique(oceania$country)    # 14 countries





# VISUALISATION ---- 
(cl_reg <- ggplot(regio, aes(year, CL, colour = Region_Name)) +
   geom_point())

(pr_reg <- ggplot(regio, aes(year, PR, colour = Region_Name)) +
    geom_line())

(cl_ctr <- ggplot(countries, aes(year, CL, colour = country)) +
  geom_line())

(cl_asia <- ggplot(asia, aes(year, CL, colour = country)) +
  geom_jitter())

# oceania
# I want to plot all and highlight Tonga
(cl_oz <- ggplot(oceania, aes(year, CL)) +
    geom_jitter() +
    geom_smooth(method = "lm"))

(pr_oz <- ggplot(oceania, aes(year, PR, colour = country)) +
    geom_jitter() +
    geom_smooth(method = "lm"))
