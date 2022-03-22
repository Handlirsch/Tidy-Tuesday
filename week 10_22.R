##%######################################################%##
#                                                          #
####              My second tidy tuesday                ####
#                       08.03.2022                         #
#                   Author: Rebecca Rau                    #
#                                                          #
##%######################################################%##

# EU Student mobility
# Who comes to Germany? Top 10


# LIBRARY ----
library(tidyverse)

# IMPORT DATASET ----
erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')


# DATA WRANGLING ----

erasmus$sending_country_code <- as.factor(erasmus$sending_country_code)
erasmus$receiving_country_code <- as.factor(erasmus$receiving_country_code)

unique(erasmus$receiving_country_code)

df <- erasmus %>% filter(receiving_country_code == "DE",    # keep data for in-comings to Germany
                   sending_country_code != "DE") %>%     # only use people from abroad
  select(sending_country_code, participants, participant_age) %>%     # select only sending countries, age and no. of participants
  group_by(sending_country_code) %>%    # group data by sending country name
  mutate(total = sum(participants)) %>%     # add column with total numbers of participants per sending country
  mutate(mean_age = mean(participant_age)) %>%    # add column with mean age
  filter(row_number() == 1) %>%     # keep the unique countries only once
  select(-participants) %>%     # delete participants column
  select(-participant_age) %>%     # delete the age column
  arrange(desc(total)) %>%     # arrange by descending numbers of incoming students
  ungroup() %>%     # ungroup data
  mutate(percent = 100 * total / sum(total)) %>%    # calculate ratio
  head(10) %>%     # keep top 10
  glimpse()



# DATA VISUALISATION ----

# labels
label_data <- df
label_data <- mutate(label_data, id = seq(10, 1))
## calculate the ANGLE of the labels
angle <-  90 - 360 * (label_data$id - 0.5)/nrow(df)
# calculate the alignment of labels: right or left
label_data$hjust < -ifelse(angle < -90, 1, 0)
# flip angle BY to make them readable
label_data$angle <- ifelse(angle < -90, angle+180, angle)



ggplot(data = df, aes(x = reorder(sending_country_code, total), y = total, fill = mean_age)) +
  geom_bar(stat = "identity", width = 0.9) +
  # Limits, to make the inner circle
  ylim(-100, 150) +
  coord_polar(start = 0) +
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data = label_data, aes(x = id, y = percent + 110, label = sending_country_code, 
                                   hjust = hjust), colour = "black", fontface = "bold", 
            alpha = 0.6, size = 2.5, angle = label_data$angle, inherit.aes = FALSE ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
  )
  



## TIDYING ----
# as factor
erasmus$participant_gender <- as.factor(erasmus$participant_gender)
erasmus$participant_profile <- as.factor(erasmus$participant_profile)
erasmus$participant_nationality <- as.factor(erasmus$participant_nationality)
erasmus$special_needs <- as.factor(erasmus$special_needs) 
erasmus$fewer_opportunities <- as.factor(erasmus$fewer_opportunities)
erasmus$sending_country_code <- as.factor(erasmus$sending_country_code)
erasmus$receiving_country_code <- as.factor(erasmus$receiving_country_code)
erasmus$field_of_education <- as.factor(erasmus$field_of_education)
erasmus$academic_year <- as.factor(erasmus$academic_year)
erasmus$mobility_start_month <- as.factor(erasmus$mobility_start_month)
erasmus$mobility_end_month <- as.factor(erasmus$mobility_end_month)
erasmus$activity_mob <- as.factor(erasmus$activity_mob)
erasmus$education_level <- as.factor(erasmus$education_level)
erasmus$sending_city <- as.factor(erasmus$sending_city)
erasmus$receiving_city <- as.factor(erasmus$receiving_city)
erasmus$sending_organization <- as.factor(erasmus$sending_organization)
erasmus$receiving_organization <- as.factor(erasmus$receiving_organization)

