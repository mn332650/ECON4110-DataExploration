# load up packages
library(rio)
library(tidyverse)
library(lubridate)

# Read in the Google Trends Data
## filenames of all the trends_up_to files
files <- list.files(path = "../DataExploration Data",
                    pattern = "trends_up_to.*\\.csv", 
                    full.names = TRUE)

gg_trends <- import_list(files, rbind = TRUE)

# Aggregate the GG Trends Data
## convert string to date and extract the first date (aka first 10 characters)
gg_trends$date <- ymd(str_sub(gg_trends$monthorweek, 1, 10))

## round all dates in the same month to be the first of the month
gg_trends$month <- floor_date(gg_trends$date, unit = "month")

## standardize the index variable 
gg_trends <- gg_trends %>%
  group_by(schname, keyword) %>%
  mutate(index_z = (index - mean(index, na.rm = TRUE)) / sd(index, na.rm = TRUE)) %>%
  ungroup()

## aggregate standardized index to the keyword=month level
agg_data <- gg_trends %>%
  group_by(keyword, month) %>%
  summarize(avg_index = mean(index_z, na.rm = TRUE), .groups = "drop")

# Read in the Scorecard data
scorecard_data <- import("../DataExploration Data/Most+Recent+Cohorts+(Scorecard+Elements).csv")

# Read in id_name_link file
id_name_link <- import("../DataExploration Data/id_name_link.csv")

# Merge in the Scorecard data
## Count how many times each school name pops up and filter to get rid of duplicates
id_name_link=id_name_link %>% 
  group_by(schname)%>%
  mutate(n=n())%>%
  filter(n==1)%>%
  ungroup()

# Join data
## Join GG Trends to id_name_link by schname
ggTrends_with_ids <- inner_join(gg_trends, id_name_link, by = "schname")

## Then, join ggTrends_with_ids to scorecard_data by unitid
### Since unitid in scorecard_data are capitalized, I will first convert it to lower case
scorecard_data = scorecard_data%>%
  rename(unitid = UNITID)

## join ggTrends_with_ids to scorecard_data by unitid
final_data = inner_join(ggTrends_with_ids,scorecard_data,by="unitid")
final_data=final_data%>%
  select(-`_file`)
