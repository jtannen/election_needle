library(testthat)

source("../load_data.R", chdir=TRUE)

if(!dir.exists("tmp")) dir.create("tmp")

################################
## Test that download works
################################

with_mock(
  get_destination_file = function() return("tmp/test_download.csv"),
  download_election_results()
)

################################
## Test processing
################################

source("../configs/config_20191105.R")
file <- "../raw_data/results_20191105_213752.csv"

df <- load_data(file)
head(df)

expect_equal(length(unique(df$warddiv)), 371)

expect_equal(
  names(df),
  c("warddiv", "office", "candidate", "party", "votes", "pvote")
)

expect_equal(
  length(unique(df$office)),
  length(CONFIG$offices)
)

kenney_votes <- df %>% 
  filter(office == "MAYOR", candidate == "Jim Kenney") %>%
  with(sum(votes))

expect_equal(kenney_votes, 36657)
