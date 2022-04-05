##%######################################################%##
#                                                          #
####                 My 4th tidy tuesday                ####
#                       05.04.2022                         #
#                   Author: Rebecca Rau                    #
#                                                          #
##%######################################################%##

# LOBRARY ----
library(ggplot2)
library(tidyr)
library(dplyr)



# DATA ----
tuesdata <- tidytuesdayR::tt_load('2022-04-05')
news_orgs <- tuesdata$news_orgs


# INFO ABOUT DATA ----
summary(news_orgs)    # from 1900 until 2020


# MY IDEA ----
# 1) Plot the number of journals founded per year
# 2) Plot the number of publications per country
# 3) Plot the primary language per country
# 4) Plot how many founders are still the owners




