source("../../admin_scripts/util.R")

get_config <- function(election) {
  source(sprintf("config_%s.R", ELECTION))
  return(CONFIG)
}