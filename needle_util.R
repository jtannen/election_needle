source("../../admin_scripts/util.R")

get_config <- function(
  election, 
  config_dir = "C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/election_night_needle/configs"
) {
  source(sprintf("%s/config_%s.R", config_dir, ELECTION))
  return(CONFIG)
}
