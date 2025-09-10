
# ----------------------------------------------------------------------------#

# Who are the five biggest individual donors for each representative, and how do their giving patterns compare over time?

# ----------------------------------------------------------------------------#

#### Load in packages ####
library(tidyverse)
library(paletteer)

#### Read in data ####

# Simpson
simpson_receipts <- read_delim("data/original/simpson_fec_17_26.csv", delim = ",")
summary(simpson_receipts)
glimpse(simpson_receipts)

# Fulcher 
fulcher_receipts <- read_delim("data/original/fulcher_fec_17_26.csv", delim = ",")
problems(fulcher_receipts)
names(fulcher_receipts)[14]
summary(fulcher_receipts)
glimpse(fulcher_receipts)

# Risch
risch_receipts <- read_delim("data/original/risch_fec_17_26.csv", delim = ",")
problems()
names(risch_receipts)[17]
summary(risch_receipts)
glimpse(risch_receipts)

# Crapo
crapo_receipts <- read_delim("data/original/crapo_fec_17_26.csv", delim = ",")
summary(crapo_receipts)
glimpse(crapo_receipts)

#### Wrangle datasets ####

# These datasets have a lot of information that we don't need to answer the question. We are going to remove most of these columns so it's easier to focus on the data we do need. 

# Create a vector with the column names that we want to keep in our datasets
select_vars <- c("is_individual", "contributor_name",  "contribution_receipt_amount", "committee_name", "report_year")

# Create a new dataframe for each congressman by calling the full dataset, then retaining all columns that match the names specified in our "select_vars" vector, then filter the rows to only retain donations made by individuals (vs. PACs, etc.). Here we are running four seperate pieces of code that does the same thing - this will be improved in future lessons
simpson_subset <- simpson_receipts %>% 
  select(all_of(select_vars)) %>% 
  filter(is_individual == TRUE)

fulcher_subset <- fulcher_receipts %>% 
  select(all_of(select_vars)) %>% 
  filter(is_individual == TRUE)

crapo_subset <- crapo_receipts %>% 
  select(all_of(select_vars)) %>% 
  filter(is_individual == TRUE)

risch_subset <- risch_receipts %>% 
  select(all_of(select_vars)) %>% 
  filter(is_individual == TRUE)

# Take a look at one of the new subsetted dataframes
glimpse(simpson_subset)

#### ID top 5 donors ####

# We want to identify the top 5 donors for each of the four congressmen. We are going to create four new dataframes by calling our subsetted dataframe, group all donations made by each individual together, add them up to get a total amount given by each unique individual, pull out the top 5 donors ordered by amount given, and create a new column denotating who the donations were made to (this will be important when we combine the 4 datasets later)
simpson_top5 <- simpson_subset %>% 
  group_by(contributor_name) %>% 
  summarise(total_given = sum(contribution_receipt_amount, 
                              na.rm = TRUE)) %>% 
  slice_max(order_by = total_given, n = 5) %>% 
  mutate(recipient = "Simpson")

fulcher_top5 <- fulcher_subset %>% 
  group_by(contributor_name) %>% 
  summarise(total_given = sum(contribution_receipt_amount, 
                              na.rm = TRUE)) %>% 
  slice_max(order_by = total_given, n = 5) %>% 
  mutate(recipient = "Fulcher")

crapo_top5 <- crapo_subset %>% 
  group_by(contributor_name) %>% 
  summarise(total_given = sum(contribution_receipt_amount, 
                              na.rm = TRUE)) %>% 
  slice_max(order_by = total_given, n = 5) %>% 
  mutate(recipient = "Crapo")

risch_top5 <- risch_subset %>% 
  group_by(contributor_name) %>% 
  summarise(total_given = sum(contribution_receipt_amount, 
                              na.rm = TRUE)) %>% 
  slice_max(order_by = total_given, n = 5) %>% 
  mutate(recipient = "Risch")

# Combine all the top donor dataframes into a single dataframe. Since they all have the same columns and data types, they can be bond with the function bind_rows()
top5_donors <- bind_rows(simpson_top5, fulcher_top5, crapo_top5, risch_top5)
top5_donors

#### Relational Data ####

# Not going to annotate as well here....

all_contributors <- bind_rows(
  select(simpson_receipts, contains("contributor")),
  select(fulcher_receipts, contains("contributor")),
  select(crapo_receipts, contains("contributor")),
  select(risch_receipts, contains("contributor"))) %>% 
  select(all_of(c("contributor_name","contributor_city", "contributor_state", "contributor_employer" ))) %>% 
  distinct()

# Why is this in ()?
(top5_donors_location <- top5_donors %>% 
    left_join(all_contributors, by = join_by(contributor_name)))

crapo_risch_shared <- crapo_top5 %>% 
  inner_join(risch_top5, by = "contributor_name")

crapo_risch_shared

crapo_risch_notshared <- crapo_top5 %>% 
  anti_join(risch_top5, by = "contributor_name")

crapo_risch_notshared

overall_top10_names <- top5_donors %>%
  arrange(desc(total_given)) %>%
  slice_max(order_by = total_given, n = 10) %>%
  pull(contributor_name)

overall_top10_names

individual_receipts <- bind_rows(
  simpson_subset %>% mutate(recipient = "Simpson"),
  fulcher_subset %>% mutate(recipient = "Fulcher"),
  crapo_subset %>% mutate(recipient = "Crapo"),
  risch_subset %>% mutate(recipient = "Risch")
)

# Calculate annual donations made by the top 10 donors 
top10_receipts <- individual_receipts %>%
  filter(contributor_name %in% overall_top10_names) %>% 
  group_by(report_year, recipient, contributor_name) %>%
  summarize(total_given = sum(contribution_receipt_amount, na.rm = TRUE)) %>%
  ungroup()
top10_receipts

# Create a figure showing each of the top 10 donors' donations through time
ggplot(data = top10_receipts, 
       mapping = aes(x = report_year, 
                     y = total_given, 
                     color = contributor_name)) +
  geom_line(linewidth = 2) + 
  geom_point(size = 4)+
  scale_color_paletteer_d("ggsci::aussie_flatui") +
  facet_wrap(vars(recipient), scales = "free_y") +
  labs(title = "Top 10 Donors Over Time by Representative",
    x = "Year",
    y = "Total Contributions ($)",
    color = "Contributor's Name") +
  theme_minimal()
