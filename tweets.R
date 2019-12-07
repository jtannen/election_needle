library(rtweet)
library(lubridate)

source("generate_twitter_token.R")

setClass(
  "Tweet",
  slots=c(
    id="numeric",
    time="character"
  )
)

Tweet <- function(id, time){
  new("Tweet", id=id, time=time)
}

get_needle_status <- function(
  needle_list, 
  office, 
  current_time, 
  n_reporting
){
  winner_text <- needle_list$winners_sim %>% 
    arrange(desc(pwin), desc(pvote)) %>%
    head(10) %>%
    with(
      sprintf(
        "%s: %s%% (%s%%)",
        candidate,
        round(pwin*100),
        round(pvote*100)
      )
    )
  formatted_predictions <- paste(winner_text, collapse="\\n")
  
  sprintf(
    "Projection for %s at %s:\n\nProb of winning (%% of vote)\n%s\n\n(%s/%s divs reporting)\n%s",
    office,
    strip_leading_zero(format(current_time, "%I:%M")),
    formatted_predictions,
    n_reporting,
    1703,
    "https://jtannen.github.io/election_needle.html"
  )
}

tweet_update <- function(reply_tweet_id, current_time, needle_result, office, n_reporting){
  if(is.na(reply_tweet_id)) stop("reply_tweet_id is NA")
  
  tmp <- tempfile(fileext = ".png")
  if(office=="Council At Large"){width <- 8; asp=1.2} else {width<- 6; asp=2/3}
  ggsave(
    plot=needle_result$map + theme(strip.text = element_text(size=10)), 
    file=tmp, 
    width=width, 
    height=asp*width
  )
  
  status <- get_needle_status(needle_result, office, current_time, n_reporting)
  post_tweet(
    status,
    media=tmp,
    in_reply_to_status_id=reply_tweet_id
  )
  my_timeline <- get_timeline(rtweet:::home_user())
  last_id <- my_timeline$status_id[1]
  return(last_id)
}

maybe_tweet_all_maps <- function(needle_result_list, last_tweet, wait=10){
  time_of_last_tweet <- last_tweet@time
  reply_tweet_id <- last_tweet@id
  
  is_time_to_tweet <- is.na(time_of_last_tweet) | (
    (ymd_hms(current_time) - ymd_hms(time_of_last_tweet)) >= minutes(wait)
  )
  
  if(is_time_to_tweet){
    for(office in names(needle_result_list)){
      reply_tweet_id <- tweet_update(
        reply_tweet_id, current_time, needle_result_list[[office]], office, n_reporting
      )
    }
    time_of_last_tweet <- Sys.time()
  }
  
  return(
    Tweet(
      id=reply_tweet_id,
      time=time_of_last_tweet
    )
  )  
}

