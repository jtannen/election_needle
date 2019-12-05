library(tidyverse)
devtools::load_all("C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/svdcov/")
source("C:/Users/Jonathan Tannen/Dropbox/sixty_six/admin_scripts/theme_sixtysix.R")

get_turnout_svd <- function(result_df, election_type, party_grep=NULL, verbose=TRUE, use_log=USE_LOG){
  if(!is.null(party_grep)) result_df <- result_df %>% filter(grepl(party_grep, party))

  result_df <- result_df %>% filter(grepl(paste0(election_type, "$"), election))

  turnout <- result_df %>%
    filter(is_topline_office) %>%
    group_by(warddiv, election) %>%
    summarise(votes = sum(votes)) %>%
    group_by() %>%
    mutate(target = {if(use_log) log(votes + 1) else votes})

  turnout_wide <- turnout %>%
    select(warddiv, election, target) %>%
    spread(election, target, fill = 0)

  turnout_wide_mat <- as.matrix(turnout_wide %>% select(-warddiv))
  row.names(turnout_wide_mat) <- turnout_wide$warddiv

  svd <- get_svd(turnout_wide_mat, verbose=TRUE, method=SVD_METHOD)
  svd@log <- use_log
  return(svd)
}

get_pvote_svd <- function(
  df_past,
  primary_party_regex,
  use_primary=TRUE,
  use_general=TRUE,
  use_log=USE_LOG
){

  df_pvote <- df_past %>%
    filter(candidate != "Write In") %>%
    filter(election == "general" | use_primary) %>%
    filter(election == "primary" | use_general) %>%
    filter(grepl(primary_party_regex, party, ignore.case=TRUE) | election=="general")

  df_pvote <- df_pvote %>%
    group_by(election, office, district, warddiv) %>%
    mutate(pvote = votes / sum(votes)) %>%
    mutate(target = {if(use_log) log(pvote + 0.001) else pvote}) %>%
    group_by()

  n_cand <- df_pvote %>%
    select(election, office, district, candidate) %>%
    unique() %>%
    group_by(election, office, district) %>%
    summarise(n_cand = n()) %>%
    mutate(
      prior_mean = {if(use_log) log(1/n_cand) else 1/n_cand}
    ) %>%
    ungroup()

  df_pvote <- df_pvote %>%
    left_join(n_cand) %>%
    mutate(target_demean = target - prior_mean)

  pvote_wide <- df_pvote %>%
    mutate(office=paste0(office, ifelse(is.na(district), "", district))) %>%
    unite("key", candidate, office, election) %>%
    select(warddiv, key, target_demean) %>%
    spread(key, target_demean, fill=0)

  pvote_mat <- as.matrix(pvote_wide %>% select(-warddiv))

  rownames(pvote_mat) <- pvote_wide$warddiv

  svd <- get_svd(
    pvote_mat,
    n_svd=5,
    known_column_means=0,
    verbose=TRUE,
    method=SVD_METHOD
  )

  svd@log <- use_log
  return(svd)
}

#######################
## PLOTS
#######################

map_precinct_score <- function(svd, col, precinct_sf, adj_area=TRUE){
  if(!is(svd, "SVDParams")) stop("params must be of class SVDParams")

  precinct_sf$area <- as.numeric(st_area(precinct_sf))

  if(adj_area){
    if(svd@log){
      adj_fe <- function(fe, area) fe - log(area)
    } else {
      adj_fe <- function(fe, area) fe / area
    }
  } else {
    adj_fe <- function(x, ...) x
  }

  ggplot(
    precinct_sf %>%
      left_join(svd@row_scores, by=c("warddiv"="row"))
  ) +
    geom_sf(
      aes(fill = adj_fe(!!sym(col), area)),
      color= NA
    ) +
    scale_fill_viridis_c("Score")+
    theme_map_sixtysix()
}

map_precinct_fe <- function(svd, precinct_sf, adj_area) {
  map_precinct_score(svd, "mean", precinct_sf, adj_area) +
    scale_fill_viridis_c("Mean")
}

map_precinct_dim <- function(svd, k, precinct_sf){
  map_precinct_score(svd, paste0("score.",k), precinct_sf, adj_area=FALSE) +
    scale_fill_gradient2(
      paste("Score, Dimension", k),
      midpoint = 0
    )
}

plot_election_score <- function(svd, col){
  if(!is(svd, "SVDParams")) stop("svd must be of class SVDParams")

  election_df <- svd@col_scores %>%
    mutate(
      year = asnum(substr(col, 1, 4)),
      etype = substr(col, 6, nchar(as.character(col)))
    )

  ggplot(
    election_df,
    aes(x=year, y=!!sym(col))
  ) +
    geom_line(
      aes(group=year %% 4),
      color= strong_green
    ) +
    geom_point(
      color = strong_green,
      size = 2
    ) +
    facet_grid(etype ~ .) +
    xlab("") +
    theme_sixtysix() +
    ggtitle("election scores", "Grouped by 4 election cycle")
}

plot_election_fe <- function(svd) plot_election_score(svd, "mean")
plot_election_dim <- function(svd, k) plot_election_score(svd, paste0("score.", k))

pause <- function() invisible(readline(prompt = "Press <Enter> to continue..."))

pvote_diagnostics <- function(svd, precinct_sf){
  print(
    map_precinct_fe(svd, precinct_sf, adj_area=FALSE) +
      ggtitle("Precinct means of pvote")
  )
  pause()

  for(k in 1:(ncol(svd@row_scores)-2)){
    print(
      map_precinct_dim(svd, k, precinct_sf) +
        ggtitle(sprintf("pvote Dimension %s", k))
    )
    pause()
  }
}

turnout_diagnostics <- function(svd, precinct_sf){
  print(
    map_precinct_fe(svd, precinct_sf, adj_area=FALSE) +
      ggtitle("Precinct means of turnout")
  )
  pause()
  print(
    plot_election_fe(svd) +
      ggtitle("Turnout FE")
  )
  pause()

  for(k in 1:(ncol(svd@row_scores)-2)){
    print(
      map_precinct_dim(svd, k, precinct_sf) +
        ggtitle(sprintf("Turnout Dim %s", k))
    )
    pause()
    print(
      plot_election_dim(svd, k) +
        ggtitle(sprintf("Turnout Dim %s", k))
    )
    pause()
  }
}

diagnostics <- function(needle_params, precinct_sf){
  print("Plotting Diagnostics...")

  pvote_diagnostics(needle_params@pvote_svd, precinct_sf)
  turnout_diagnostics(needle_params@turnout_svd, precinct_sf)
}


if(FALSE){
  pvote_svd <- get_pvote_svd(df_past)

  if(CONFIG$is_primary){
    turnout_svds=list(
      "rep" = get_turnout_svd("primary", "^REP"),
      "dem" = get_turnout_svd("primary", "^DEM")
    )
  } else {
    turnout_svds=list(
      "general" = get_turnout_svd("general")
    )
  }

  needle_params <- needleSVDs(
    pvote_svd=pvote_svd,
    turnout_svds=turnout_svds,
    log=USE_LOG
  )

  diagnostics(needle_params, divs)
}
