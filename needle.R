library(tidyverse)
library(MASS)
library(sf)
select <- dplyr::select

# setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/election_night_needle/")
source("needle_util.R")
source("svd_for_turnout_and_pvote.R")
source("load_data.R")

if(!exists("USE_MAPS")) USE_MAPS <- TRUE
if(!exists("should_download")) should_download <- FALSE

set.seed(215)

# ELECTION <- "20191105"
# CONFIG <- get_config(ELECTION)
# USE_LOG <- CONFIG$use_log

###############################
## Load Data
###############################

## Load the most recent data
if(FALSE){
  files <- list.files("raw_data")
  file <- max(files)
  df <- load_data(file)
}

## Load a specific past download
if(FALSE){
  df <- load_data("raw_data/results_20191105_215542.csv")
}

## create a fake download from df_all
if(FALSE){
  use_past_offices <- c(
    "CITY COMMISSIONERS",
    "COUNCIL AT LARGE",
    "JUDGE OF THE COURT OF COMMON PLEAS",
    "MAYOR",
    "SHERIFF"
  )
  
  df <- readRDS("../../data/processed_data/df_all_2019_09_19.Rds") %>%
    group_by() %>%
    filter(
      election == "general",
      year == '2015',
      !grepl("WRITE IN", candidate, ignore.case = TRUE)
    ) %>%
    unite("election", year, election) %>%
    mutate(
      office = paste0(
        office, 
        ifelse(!is.na(district), paste0("-",district),"")
      )
    ) %>%
    filter(office %in% use_past_offices) %>%
    group_by(warddiv, office, candidate) %>%
    summarise(votes=sum(votes)) %>%
    group_by(warddiv, office) %>%
    mutate(pvote = votes / sum(votes))  %>%
    ungroup() %>%
    mutate(
      warddiv=pretty_div(warddiv),
      candidate = format_name(candidate)
    )
  
  
  sample_divs <- sample(unique(df$warddiv), 10)
  df <- df %>% filter(warddiv %in% sample_divs)
}

####################################
## Load shapefiles
####################################

pretty_div <- function(warddiv) sprintf("%s-%s", substr(warddiv, 1, 2), substr(warddiv, 3, 4))

divs <- st_read("../../data/gis/201911/Political_Divisions.shp")
divs <- st_transform(divs, 2272) %>%
  rename(
    warddiv = DIVISION_N
  ) %>%
  arrange(warddiv) %>%
  mutate(warddiv=pretty_div(warddiv))

wards <- st_read("../../data/gis/2019/Political_Wards.shp") %>%
  mutate(ward=sprintf("%02d", asnum(WARD_NUM))) %>%
  st_transform(2272)

phila_whole <- st_union(wards)


## Get Council Districts
if(FALSE){
  print("Getting Council Districts")
  council <- st_read("../../data/gis/city_council/Council_Districts_2016.shp")
  council <- st_transform(council, 2272)
  div_centroids <- st_centroid(divs) 
  divs_to_council <- st_covered_by(div_centroids, council)
  divs_to_council <- unlist(divs_to_council)
  if(length(divs_to_council) != nrow(divs)) stop("Too many matches")
  divs_to_council <- data.frame(
    warddiv = divs$warddiv,
    council = as.character(council$DISTRICT)[divs_to_council]
  ) 
  saveRDS(divs_to_council, file="outputs/divs_to_council.Rds")
} else {
  divs_to_council <- readRDS("outputs/divs_to_council.Rds")
}


#####################
## predict turnout
#####################

if(USE_LOG){
  transform_pvote <- function(x) log(x+0.001)
  transform_turnout <- function(x) log(x + 1)
  transform_pvote_inv <- function(x) exp(x) - 0.001
  transform_turnout_inv <- function(x) exp(x) - 1
} else {
  transform_pvote <- identity
  transform_turnout <- identity
  transform_pvote_inv <- identity
  transform_turnout_inv <- identity
}
N_BOOT <- 400


simulate_turnout <- function(df, turnout_office, turnout_svd, verbose=TRUE){
  vprint <- function(x) if(verbose) print(x)
  
  vprint("Calculating Observed Turnout")
  observed_turnout <- df %>% 
    filter(office == turnout_office) %>%
    group_by(warddiv) %>%
    summarise(turnout = sum(votes)) %>%
    mutate(target_obs = transform_turnout(turnout))
  
  election_mean <- observed_turnout %>%
    left_join(turnout_svd@row_scores, by=c("warddiv"="row")) %>%
    with(mean(target_obs - mean))
  
  vprint("Sampling from posterior")
  sim <- sample_from_posterior(
    svd=turnout_svd, 
    obs=observed_turnout$target_obs, 
    obs_id=observed_turnout$warddiv, 
    column_mean=election_mean,
    n_sim=N_BOOT,
    verbose=verbose
  ) %>%
    rename(warddiv=row) %>%
    mutate(turnout = transform_turnout_inv(value))
  
  return(sim)
}

simulate_pvote <- function(
  df,
  use_office,
  office_name,
  turnout_sim,
  pvote_svd,
  n_winners,
  consider_divs=NULL,
  verbose=TRUE
){

  vprint <- function(x) if(verbose) print(x)
  
  vprint("Processing df")
  if(!is.null(consider_divs)) df <- df %>% filter(warddiv %in% consider_divs)
  
  df_office <- df %>% 
    filter(office == use_office) %>%
    group_by() %>%
    mutate(target = transform_pvote(pvote))

  candidates <- unique(df_office$candidate)
  ncand <- length(candidates)
  
  if(ncand == 0){ print(paste0(office_name, ": NO RETURNS YET")); return()}

  vprint("Calculating SVD")
  prior_mean <- transform_pvote(1/ncand)
  
  df_list <- vector(mode="list", length=ncand)
  
  for(cand in candidates){
    vprint("####################")
    vprint(sprintf("Candidate: %s", cand))
    vprint("####################")
    observed_pvote <- df_office %>%
      filter(candidate == cand)
    
    pvote_cand_sample <- sample_from_posterior(
      pvote_svd, 
      observed_pvote$target, 
      observed_pvote$warddiv, 
      filter_to_ids=consider_divs,
      column_mean=prior_mean,
      n_sim=400,
      verbose=verbose
    ) 
    
    pvote_cand_sample <- pvote_cand_sample %>%
      rename(
        warddiv=row,
        target=value
      )
    
    df_list[[cand]] <- pvote_cand_sample
  }
    
  vprint("Post-processing Simulations")
  office_sim <- bind_rows(df_list, .id="candidate") %>%
    mutate(pvote = transform_pvote_inv(target)) %>%
    group_by(warddiv, sim) %>%
    mutate(pvote = pvote / sum(pvote)) %>%
    ungroup()
    
  office_sim_total <- office_sim %>%
    left_join(turnout_sim, by=c("sim", "warddiv")) %>%
    group_by(candidate, sim) %>%
    summarise(votes_sim = sum(pvote * turnout)) %>%
    group_by(sim) %>%
    mutate(
      pvote = votes_sim / sum(votes_sim),
      is_winner = rank(desc(pvote)) <= n_winners
    ) %>%
    ungroup()
  
  winners_sim <- office_sim_total %>% 
    group_by(candidate) %>%
    summarise(
      pvote = mean(pvote),
      pwin = mean(is_winner)
    ) 
  
  candidate_order <- get_candidate_order(winners_sim)

  vprint("Creating Needle")
  needle_facet <- get_needle_plot(winners_sim, candidate_order)
  
  if(USE_MAPS){
    vprint("Creating Maps")
    map_facet <- get_sim_map(df_office, consider_divs, candidate_order)
  } else {
    map_facet <- ggplot()
  }  
  
  return(list(
    needle=needle_facet,
    map=map_facet,
    office_sim=office_sim,
    office_sim_total=office_sim_total,
    winners_sim=winners_sim
  ))
}

get_candidate_order <- function(winners_sim){
  winners_sim %>% 
    arrange(desc(pvote)) %>%
    with(candidate)
}

get_needle_plot <- function(winners_sim, candidate_order){
  winners_sim <- winners_sim %>%
    mutate(x = -cos(pwin * pi), y = sin(pwin*pi))
  
  winners_sim$candidate <- factor(
    winners_sim$candidate,
    levels = candidate_order
  )
  
  needle_facet <- ggplot(
    winners_sim
  ) +
    geom_segment(
      x=0, y=0,
      aes(xend=x, yend=y), 
      arrow = arrow(length = unit(0.1, "inches")),
      size=2, color="grey30"
    ) +
    geom_point(x=0,y=0,color="grey30", size=4) +
    facet_wrap(~candidate, ncol=3) +
    scale_x_continuous(
      "", 
      breaks = c(0), 
      labels =c(""), 
      limits=c(-1,1)
    ) + 
    scale_y_continuous("", breaks = FALSE, minor_breaks = FALSE, labels = "", limits=c(0,1)) + 
    geom_text(
      aes(label = paste0(round(100*pwin),"%")), 
      size = 8,
      x=-1, y=1, hjust=0, vjust=1,
      fontface="bold",
      color=strong_purple
    ) +
    geom_text(
      aes(label = paste0("(", round(100*pvote, 1),"% of vote)")), 
      x=-1, y=0.5, hjust=0, vjust=0,
      # fontface="bold",
      color="grey50"
    ) +
    coord_fixed() +
    theme_sixtysix() %+replace% 
    theme(
      strip.text = element_text(face="bold", size = 20)
    )
}

get_sim_map <- function(df_office, consider_divs, candidate_order){
  div_sim <- df_office %>%
    group_by(warddiv) %>%
    mutate(
      candidate = factor(
        candidate, 
        levels = candidate_order
      )
    )
  
  if(is.null(consider_divs)) consider_divs <- unique(divs$warddiv)
  limits <- st_bbox(divs %>% filter(warddiv %in% consider_divs))
  
  ndivs <- length(unique(df_office$warddiv))
  if(ndivs > 200){
    ggmap <- wards %>%
      inner_join(
        div_sim %>% mutate(ward = substr(warddiv,1,2)) %>%
          group_by(warddiv) %>%
          mutate(turnout = sum(votes)) %>%
          group_by(ward, candidate) %>%
          summarise(pvote = weighted.mean(pvote, w=turnout))
      )
  } else {
    ggmap <- divs %>% 
      inner_join(div_sim)
  }
  
  ggplot(ggmap) + 
    geom_sf(data=phila_whole, fill="grey70", color=NA) +
    geom_sf(aes(fill = 100 * pvote), color = NA) +
    facet_wrap(~candidate, ncol=3) +
    expand_limits(fill=0) +
    scale_fill_viridis_c("% of Vote") +
    theme_map_sixtysix() %+replace%
    theme(
      legend.position = "right",
      strip.text = element_text(face="bold", size = 20)
    )+
    xlim(limits[c("xmin", "xmax")]) +
    ylim(limits[c("ymin", "ymax")])
}

if(FALSE){
  needle_params <- readRDS(sprintf("outputs/needleparams_%s_log%s.Rds", ELECTION, USE_LOG))
  
  if(CONFIG$is_primary){
    mayor_office <- "MAYOR-DEM" 
    pred_office <- "COUNCIL AT LARGE-DEM"
    turnout_svd <- needle_params@turnout_svds$dem
  } else {
    mayor_office <- "MAYOR"
    pred_office <- "COUNCIL AT LARGE"
    turnout_svd <- needle_params@turnout_svds$general
  }
  pvote_svd <- needle_params@pvote_svd
  
  print("Simulating Turnout")
  turnout_sim <- simulate_turnout(df, mayor_office, turnout_svd)
  
  if(CONFIG$is_primary) n_council_winners <- 5 else n_council_winners <- 7
  
  council_at_large_plot <- simulate_pvote(
    df,
    use_office=pred_office,
    office_name="Council At Large",
    turnout_sim=turnout_sim,
    pvote_svd=pvote_svd,
    n_winners=n_council_winners,
    verbose=TRUE
  )
  council_at_large_plot[["needle"]] %>% print()
}
