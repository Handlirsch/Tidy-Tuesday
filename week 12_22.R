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
        plot.caption = element_text(size = 13, colour = "grey42"),
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 14)
  )      
  




# inspired by Topenomics
# Rebecca over the years
nz_names %>% filter(Name == "Rebecca") %>% 
  ggplot(aes(x = Year, y = Count)) +
  geom_line()

nz_names %>% filter(Name == "Lena") %>% 
  ggplot(aes(x = Year, y = Count)) +
  geom_line()


# NAME OF THRONES ----

babynames %>% filter(name == "Arya") %>% 
  ggplot(aes(year, n)) +
  geom_line()

babynames %>% filter(name == "Sansa") %>% 
  ggplot(aes(year, n)) +
  geom_line()

babynames %>% filter(name == "Tyrion") %>% 
  ggplot(aes(year, n)) +
  geom_line()

babynames %>% filter(name == "Khaleesi") %>% 
  ggplot(aes(year, n)) +
  geom_line()

babynames %>% filter(name == "Daenerys") %>% 
  ggplot(aes(year, n)) +
  geom_line()

babynames %>% filter(name == "Bran") %>% 
  ggplot(aes(year, n)) +
  geom_line()

babynames %>% filter(name == "Theon") %>% 
  ggplot(aes(year, n)) +
  geom_line()

babynames %>% filter(name == "Maisie") %>% 
  ggplot(aes(year, n)) +
  geom_line()


babynames %>% filter(name == "Kit") %>% 
  ggplot(aes(year, n)) +
  geom_line()

babynames %>% filter(name == "Lyanna") %>% 
  ggplot(aes(year, n)) +
  geom_line()




