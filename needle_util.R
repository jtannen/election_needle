source("../../admin_scripts/util.R")

get_config <- function(election) {
  source(sprintf("configs/config_%s.R", ELECTION))
  return(CONFIG)
}