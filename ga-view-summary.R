# This script simply cycles through multiple Google Analytics views,
# pulls back a handful of metrics, and then consolidates them and
# pushes them into a .csv.

# Load the packages we'll use
library(googleAnalyticsR)
library(tidyverse)

# Settings
start_date <- as.character(Sys.Date()-31)
end_date <- as.character(Sys.Date()-1)
metrics <- c("sessions","pageviews","totalEvents")
dimensions <- "year"

# Authorize Google Analytics
ga_auth()

# Pull a lit of all available accounts and views and then filter it
# down to the subset of interest. This will need to be adjusted. Currently,
# it's assuming a single GA account is of interest (but grepl() or
# contains() could be used to grab multiple ones), and it's then
# getting all the views that start with "PROD", which also will
# likely need to be adjusted.
account_summary <- google_analytics_account_list() %>%
  filter(accountName == "[name of the Google Analytics account]", 
  grepl("^PROD.*", viewName))

# Add the start and end date to the data frame, as well as some
# columns to use to populate the metrics
account_summary$start_date <- start_date
account_summary$end_date <- end_date

# Function to pull the data from Google Analytics
get_data <- function(view_id){
  # Pull the data. The query might return multiple rows (if it spans
  # a year boundary), so collapse the results just in case.
  ga_data <- google_analytics_4(viewId = view_id,
                                date_range = c(start_date,end_date),
                                metrics = metrics,
                                dimensions = dimensions) %>%
    summarise(sessions = sum(sessions),
              pageviews = sum(pageviews),
              total_events = sum(totalEvents))
}

# Get the data for each view ID. The do() function is a little confusing,
# but it's a bit more efficient than using a for() loop.
result_metrics <- group_by(account_summary, viewId) %>% 
  do(get_data(.$viewId))

# Add the metrics back to the summary
account_summary <- left_join(account_summary, result_metrics, 
                             by = c("viewId","viewId"))

# Make a more compact set of data
clean_summary <- select(account_summary,
                        Account = accountName,
                        Property = webPropertyName,
                        View = viewName,
                        Type = type,
                        Level = level,
                        'Start Date' = start_date,
                        'End Date' = end_date,
                        Sessions = sessions,
                        Pageview = pageviews,
                        'Total Events' = total_events)

# Output the results to a .csv file. Another function can be used if
# comma-delimited isn't ideal.
write.csv(clean_summary, "summary_results.csv", row.names = FALSE)
