# Time to do some web scraping! 
# Purpose: scrape data about laws/policies that impact LGBTQ individuals from the Movement Advancement Project at https://www.lgbtmap.org/equality-maps
# Output: a tibble (policy_scrape_table) with each row representing a state and each column representing a policy tally related to sexual orientation (SO) or gender identity (GI)
# Last updated: April 22, 2024 by CW

# Load packages
library(rvest)
library(dplyr)
library(tidyr)

# URL of page containing table we want to scrape
url <- "https://www.lgbtmap.org/equality-maps/index/policies?sortdir=asc&sort1=state&sort2=name"

# Scoop the table
page <- read_html(url)
data_table <- page %>%
  html_node("#map-4 > div > table") %>%
  html_table(fill = TRUE)

# Convert table to tibble
data_tibble <- as_tibble(data_table)

# Clean it up
cleaned_table <- select(data_tibble, -1)
column_names <- as.character(unlist(slice(cleaned_table, 1)))
cleaned_table <- cleaned_table[-1, ]
names(cleaned_table) <- column_names
new_colnames <- c("state", "sex_gender_score", "relationship_parent_recog", 
                      "nondiscrimination", "relig_exemption", "lgbt_youth", 
                      "healthcare", "crim_justice", "identity_docs", 
                      "SO_policy_tally", "GI_policy_tally", "all_tally")
cleaned_table <- cleaned_table %>%
  setNames(new_colnames)

# Pivot and create SO and GI variables for each policy type so there is one row per state
cleaned_table <- cleaned_table %>%
  group_by(state) %>%
  mutate(row_id = row_number()) %>%
  ungroup()

cleaned_table <- cleaned_table %>%
  pivot_wider(
    names_from = row_id,
    names_glue = "{.value}_{ifelse(row_id == 1, 'SO', 'GI')}",
    values_from = c(sex_gender_score, relationship_parent_recog, nondiscrimination, 
                    relig_exemption, lgbt_youth, healthcare, crim_justice, identity_docs, 
                    SO_policy_tally, GI_policy_tally, all_tally)
  )

# Get rid of extraneous columns and give table a cuter name
policy_scrape_table <- cleaned_table %>%
  select(
    -c(sex_gender_score_SO, sex_gender_score_GI, identity_docs_SO, SO_policy_tally_GI, GI_policy_tally_SO, all_tally_GI)
  ) %>%
  rename(
    policy_tally_SO = SO_policy_tally_SO,
    policy_tally_GI = GI_policy_tally_GI,
    all_tally = all_tally_SO
  )

# Check out our table!
print(policy_scrape_table)


###under construction

# Define regions and divisions
region_mapping <- data.frame(
  state = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont",
            "New Jersey", "New York", "Pennsylvania",
            "Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin",
            "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota",
            "Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia",
            "Alabama", "Kentucky", "Mississippi", "Tennessee",
            "Arkansas", "Louisiana", "Oklahoma", "Texas",
            "Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming",
            "Alaska", "California", "Hawaii", "Oregon", "Washington"),
  Region = c(rep("Northeast", 9),
             rep("Midwest", 12),
             rep("South", 17),
             rep("West", 12))
)

# Clean the data to remove non-states and merge with region info
cleaned_table2 <- policy_scrape_table %>%
  filter(state %in% region_mapping$state) %>%
  left_join(region_mapping, by = "state")

# Convert potential character columns that should be numeric to numeric
numeric_columns <- c("policy_tally_SO","policy_tally_GI","all_tally")

cleaned_table2 <- cleaned_table2 %>%
  mutate(across(all_of(numeric_columns), ~as.numeric(as.character(.)), .names = "num_{.col}"))

# Group by Region and calculate average scores
grouped_by_region <- cleaned_table2 %>%
  group_by(Region) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Print the results
print(grouped_by_region)