library(rmarkdown)
setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/election_night_needle/")
source("needle_util.R")

ELECTION <- "20191105"
CONFIG <- get_config(ELECTION)
USE_LOG <- CONFIG$use_log

needle_params <- readRDS(sprintf("outputs/needleparams_%s_log%s.Rds", ELECTION, USE_LOG))

USE_MAPS <- TRUE
IS_TEST <- FALSE

source("needle.R")
source("tweets.R")

SHOULD_TWEET <- TRUE
if(SHOULD_TWEET){
  last_tweet <- Tweet(id=1191884860144672770, time="2019-11-05 12:00:00")
}

while(TRUE){
  print(sprintf("New Loop: %s", Sys.time()))
  
  downloaded <- download_election_results()
  
  if(is.null(downloaded)){
    Sys.sleep(60)
  } else {
    print("rmarkdown")
    rmarkdown::render( 
      "needle.Rmd", 
      output_file="election_needle.html"
    )
    
    if(!IS_TEST){
      print("copy and git")
      file.copy(
        "election_needle.html",
        "C:/Users/Jonathan Tannen/Dropbox/github_page/jtannen.github.io/election_needle.html",
        overwrite=TRUE
      )
      
      system("upload_git.bat")
    }
    
    ## BROKEN    
    # if(SHOULD_TWEET){
    #   office_maps <- list(
    #     "Council At Large"=council_at_large_plot,
    #     "District 10"=district_10_plot,
    #     "Mayor"=mayor_plot
    #   )
    #   
    #   last_tweet <- maybe_tweet_all_maps(office_maps,last_tweet = last_tweet)
    # }
  }
}
