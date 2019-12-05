library(tidyverse)
library(sf)

setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/election_night_needle/")
source("../../admin_scripts/util.R")

should_download <- FALSE
use_maps <- FALSE
use_saved_covariance <- TRUE
use_log <- TRUE

use_latest_file <- FALSE

source("needle.R")

df_838 <- load_data("PRECINCT_2019521_H20_M38_S25.txt")
df_938 <- load_data("PRECINCT_2019521_H21_M38_S25.txt")
df_full <- load_data("PRECINCT_2019525_H08_M09_S54.TXT")

length(table(df_838$warddiv))

ggplot(
  phila_whole
) + geom_sf(fill = "grey50", color=NA) +
  geom_sf(
    data=divs %>% inner_join(df_838 %>% select(warddiv) %>% unique),
    fill = "black", color = NA
  )

turnout_dem <- get_turnout(df_838, "MAYOR-DEM", turnout_cov_dem)
council_at_large_dem <- get_needle_for_office(
  df_838,
  use_office="COUNCIL AT LARGE-DEM",
  office_name="Council At Large (D)",
  turnout_sim = turnout_dem,
  n_winners=5
)

council_at_large_dem$needle

color_min <- 0
color_max <- 0.2

ggplot(
  divs %>% 
    left_join(
      turnout_dem %>%
        group_by(warddiv) %>%
        summarise(turnout = mean(turnout)) 
    )%>%
    mutate(turnout_per_mile = pmin(turnout / Shape__Are * 5280^2))
) + 
  geom_sf(aes(fill = pmin(turnout_per_mile, 10e3)), color=NA) +
  geom_sf(
    data = divs %>% inner_join(
      df_838 %>%
        filter(OFFICE == "MAYOR-DEM") %>%
        group_by(warddiv) %>%
        summarise(turnout = sum(Vote_Count)) 
    ) %>%
      mutate(turnout_per_mile = pmin(turnout / Shape__Are * 5280^2)),
    aes(fill = pmin(turnout_per_mile, 10e3))
  ) +
  scale_fill_viridis_c("Votes per sq. mile") +
  theme_map_sixtysix()

ggplot(
  divs %>% 
    left_join(
      df_full %>%
        filter(OFFICE == "MAYOR-DEM") %>%
        group_by(warddiv) %>%
        summarise(turnout = sum(Vote_Count)) 
    )%>%
    mutate(turnout_per_mile = pmin(turnout / Shape__Are * 5280^2))
) + 
  geom_sf(aes(fill = pmin(turnout_per_mile, 10e3)), color=NA) +
  scale_fill_viridis_c("Votes per sq. mile") +
  theme_map_sixtysix()


map_pred <- function(
  df_obs, 
  df_sim, 
  candidate, 
  consider_divs=divs_to_council$warddiv,
  color_max = 1
){

  df_obs <- df_obs[df_obs$candidate == candidate,] 
  df_sim <- df_sim[df_sim$candidate == candidate,] %>%
    group_by(warddiv) %>%
    summarise(pvote = mean(pvote))
  
  df_sim$pvote <- pmin(df_sim$pvote, color_max)
  df_sim$pvote <- pmax(df_sim$pvote, 0)
  
  ggplot(
    divs %>% filter(warddiv %in% consider_divs)
  ) +
    geom_sf(
      data=divs %>% inner_join(df_sim),
      aes(fill=pvote), color = NA
    ) +
    geom_sf(
      data=divs %>% inner_join(df_obs),
      aes(fill=pvote), color=NA#"black"
    ) +
    scale_fill_viridis_c() +
    expand_limits(fill = c(0, color_max))+ 
    theme_map_sixtysix()
}


map_sim <- function(
  df_obs, 
  df_sim, 
  candidate, 
  sim=1, 
  consider_divs=divs_to_council$warddiv
){
  df_obs <- df_obs[df_obs$candidate == candidate,]
  df_sim <- df_sim[df_sim$candidate == candidate & df_sim$sim == sim,] 
  
  ggplot(
    divs %>% filter(warddiv %in% c(df_obs$warddiv, df_sim$warddiv))
  ) +
    geom_sf(
      data=divs %>% inner_join(df_sim),
      aes(fill=pmin(pvote, 0.2)), color = NA
    ) +
    geom_sf(
      data=divs %>% inner_join(df_obs),
      aes(fill=pvote), color=strong_orange
    ) +
    scale_fill_viridis_c() +
    theme_map_sixtysix()
}

map_pred(
  df_838, 
  council_at_large_dem$office_sim, 
  "Gym", 
  nwinners=5, 
  color_max=0.33
) + ggtitle("Simulated Results for Gym at 8:38")

map_pred(
  df_full, 
  data.frame(warddiv=character(0), candidate=character(0)), 
  "Gym", 
  nwinners=5,
  color_max=0.33
) + ggtitle("Actual Final Gym Results")

council_at_large_dem$office_sim_total %>% filter(candidate == "DiBerardinis" & is_winner)
map_sim(df_838, council_at_large_dem$office_sim, "DiBerardinis", sim = 53)

cov_sim <- council_at_large_dem$office_sim %>% 
  filter(candidate == "DiBerardinis") %>%
  select(sim, warddiv, pvote) %>%
  spread(warddiv, pvote) %>%
  select(-sim) %>%
  as.matrix %>% 
  svd()

ggplot(
  divs %>% 
    inner_join(
      as.data.frame(cov_sim$v[,2]) %>%
        rename(cov = `cov_sim$v[, 2]`) %>%
        mutate(warddiv = unique(council_at_large_dem$office_sim$warddiv))
    )
) +
  geom_sf(aes(fill = cov), color=NA) +
  scale_fill_viridis_c()


council_3 <- get_needle_for_office(
  df,
  use_office="DISTRICT COUNCIL-3RD DISTRICT-DEM",
  office_name="Council District 3",
  turnout_sim = turnout_dem,
  n_winners=1,
  consider_divs = divs_to_council$warddiv[divs_to_council$council == "3"]
)
council_3$needle

map_pred(
  df_838, 
  council_3$office_sim, 
  "Gauthier", 
  divs_to_council$warddiv[divs_to_council$council == "3"],
  nwinners=1
)

map_pred(
  df_full, 
  data.frame(
    warddiv=character(0), 
    candidate=character(0)
  ), 
  "Gauthier", 
  divs_to_council$warddiv[divs_to_council$council == "3"]
)


final_council_3 <- df_full %>% 
  filter(OFFICE == "DISTRICT COUNCIL-3RD DISTRICT-DEM") %>%
  group_by(candidate) %>%
  summarise(votes = sum(Vote_Count)) %>%
  group_by() %>%
  mutate(prop = votes/sum(votes))



histogram_simulation <- function(
  office_sim_total, 
  candidate, 
  df_final, 
  time, 
  win_bar,
  ndigits=0
){
  final_p <- 100 * df_final$prop[df_final$candidate==candidate]
  
  binwidth = 10^-ndigits
  ggplot(
    office_sim_total[office_sim_total$candidate == candidate,]
  ) + 
    geom_histogram(
      aes(x=pvote * 100, y=stat(density)*100),
      boundary=0,
      binwidth=binwidth,
      fill = strong_blue
    ) +
    geom_vline(xintercept = win_bar, linetype="dashed") +
    annotate(
      "text",
      label="Win Percent",
      x=win_bar,
      y=5,
      hjust=0,
      vjust=1.2,
      angle=90
    ) +
    geom_vline(
      xintercept = final_p,
      color = "grey20",
      size=1
    ) +
    annotate(
      "text", 
      label=sprintf("Actual Final Result = %s%%", round(final_p, ndigits)),
      x=final_p,
      y=5,
      angle=90,
      vjust=1.2,
      hjust=0
    ) +
    xlab("Proportion of Vote") +
    ylab("Proportion of Simulations") +
    theme_sixtysix() +
    ggtitle(sprintf("%s's Simulated Results at %s", candidate, time))
}

histogram_simulation(council_3$office_sim_total, "Gauthier", final_council_3, "9:38", 50)

final_at_large <- df_full %>% 
  filter(OFFICE == "COUNCIL AT LARGE-DEM") %>%
  group_by(candidate) %>%
  summarise(votes = sum(Vote_Count)) %>%
  group_by() %>%
  mutate(prop = votes/sum(votes))

histogram_simulation(
  council_at_large_dem$office_sim_total, 
  "Gym",
  final_at_large, 
  "9:38", 
  6.61,
  ndigits=1
)

