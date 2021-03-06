library(tidyverse)

test_offices <- tribble(
  ~year, ~election, ~office, ~n_winners,
  2019, "primary", "COUNCIL AT LARGE", 5,
  2017, "primary", "DISTRICT ATTORNEY", 1,
  2016, "primary", "PRESIDENT OF THE UNITED STATES", 1,
  2015, "primary", "MAYOR", 1,
  2015, "primary", "COUNCIL AT LARGE", 5,
  2015, "general", "COUNCIL AT LARGE", 7,
  2014, "primary", "GOVERNOR", 1,
  2019, "primary", "JUDGE OF THE COURT OF COMMON PLEAS", 6,
  2017, "primary", "JUDGE OF THE COURT OF COMMON PLEAS", 9,
  2015, "primary", "JUDGE OF THE COURT OF COMMON PLEAS", 12,
  2019, "primary", "DISTRICT COUNCIL: 2", 1,
  2019, "primary", "DISTRICT COUNCIL: 3", 1,
  2019, "primary", "DISTRICT COUNCIL: 7", 1
) %>% mutate(election = ifelse(election == "primary", "primary-DEM", election))

SAMPLE_FRAC <- 0.05

METHOD <- "svd"

source("../prep_data.R", chdir=TRUE)
source("../needle.R", chdir=TRUE)

print("DONE SOURCING")

primary_needle_params <- calc_params(
  method=METHOD,
  is_primary=TRUE,
  use_log=TRUE,
  show_diagnostics=FALSE,
  df_past_path = most_recent_file("../../../data/processed_data/df_major_", ext = "Rds")
)

general_needle_params <- calc_params(
  method=METHOD,
  is_primary=FALSE,
  use_log=TRUE,
  show_diagnostics=FALSE
)

df_major <- readRDS("../../../data/processed_data/df_major_2019-11-07.Rds")
df_major %<>%
  filter(candidate != "Write In") %>%
  mutate(
    warddiv = pretty_div(warddiv),
    office = ifelse(
      office == "DISTRICT COUNCIL", 
      sprintf("DISTRICT COUNCIL: %s", district), 
      office
    ),
    election = ifelse(
      election=="primary",
      sprintf("primary-%s", substr(party,1,3)),
      election
    )
  ) %>%
  group_by(office, candidate, election, year, warddiv) %>%
  summarise(votes = sum(votes)) %>%
  group_by(office, election, year, warddiv) %>%
  mutate(pvote = votes / sum(votes)) %>%
  ungroup()

turnout_offices <- data.frame(
  year = as.character(seq(2002, 2021)),
  office = c("GOVERNOR", "MAYOR", "PRESIDENT OF THE UNITED STATES", "DISTRICT ATTORNEY")
)

set.seed(215)

turnout_results <- data.frame()
results <- data.frame()

N_SIMS <- 4
for(i in 1:nrow(test_offices)) for(j in 1:N_SIMS) {
  year <- test_offices$year[i]
  election <- test_offices$election[i]
  office <- test_offices$office[i]
  n_winners <- test_offices$n_winners[i]

  print("#########################################")
  print(sprintf("%s %s %s %s", year, election, office, j))
  print("#########################################")

  ##################################
  ## SAMPLE DATA
  ##################################
  
  if(grepl("^DISTRICT COUNCIL", office)){
    district <- gsub("^DISTRICT COUNCIL: ([0-9]+)$", "\\1", office)
    consider_divs <- divs_to_council$warddiv[
      divs_to_council$council == district
    ]
    ward_sample <- unique(substr(consider_divs, 1, 2))
    ward_sample <- sample(ward_sample, 1)
    warddiv_sample <- consider_divs[substr(consider_divs, 1, 2) %in% ward_sample]
  } else {
    consider_divs <- unique(df_major$warddiv)
    ward_sample <- sprintf("%02d", sample.int(66, round(66*SAMPLE_FRAC)))
    warddiv_sample <- consider_divs[substr(consider_divs, 1, 2) %in% ward_sample]
  }
  
  turnout_office <- (turnout_offices %>% filter(year == !!year))$office
  
  n_reporting <- length(warddiv_sample)

  if(election == "primary-DEM"){
    needle_params <- primary_needle_params$dem
  } else if (election == "general"){
    needle_params <- general_needle_params$general
  } else stop("Bad election value")

  df_samp <- df_major %>% 
    filter(year == !!year, election==!!election) %>%
    filter(warddiv %in% warddiv_sample)
  
  turnout_svd <- needle_params@turnout_svd
  pvote_svd <- needle_params@pvote_svd
  
  ####################
  ## TURNOUT
  ####################
  
  turnout_sim <- simulate_turnout(
    df=df_samp, 
    turnout_office=turnout_office, 
    turnout_svd=turnout_svd, 
    verbose=FALSE
  ) 
  
  true_turnout <- df_major %>%
    filter(office == !!turnout_office, year==!!year, election==!!election) %>%
    group_by(warddiv) %>%
    summarise(true_turnout=sum(votes))
  
  turnout_sim_stats <- turnout_sim %>% 
    group_by(warddiv, simulated) %>%
    summarise(
      mean = mean(turnout),
      p025 = quantile(turnout, 0.025),
      p975 = quantile(turnout, 0.975)
    ) %>%
    ungroup() %>%
    left_join(true_turnout) %>%
    mutate(ci_covers = (true_turnout >= p025) & (true_turnout <= p975))
  
  turnout_sim_overall <- turnout_sim %>%
    group_by(sim, simulated) %>%
    summarise(turnout = sum(turnout)) %>%
    group_by(simulated) %>%
    summarise(
      mean = mean(turnout),
      p025 = quantile(turnout, 0.025),
      p975 = quantile(turnout, 0.975)
    ) %>%
    ungroup() %>%
    left_join(
      true_turnout %>%
        mutate(simulated = !warddiv %in% warddiv_sample) %>%
        group_by(simulated) %>% 
        summarise(true_turnout = sum(true_turnout))
    ) %>%
    mutate(ci_covers = (true_turnout >= p025) & (true_turnout <= p975))
  
  print(turnout_sim_overall)
  
  turnout_results <- bind_rows(
    turnout_results, 
    turnout_sim_overall %>% mutate(year=!!year, election=!!election, sim=!!j)
  )
  
  close_to <- function(x, y, tol){
    return(
      x >= y - tol & x <= y + tol
    )
  }
  
  expect_close <- function(x, y, tol){
    expect_true(close_to(x, y, tol))
  }
  
  expect_all <- function(x) expect_true(all(x))
  
  ## Exact tests among reporting
  with(
    turnout_sim_stats %>% filter(!simulated),
    {
      print(sprintf("N(divs): %s", length(warddiv)))
      expect_equal(length(warddiv), n_reporting)
      expect_all(close_to(true_turnout, mean, tol = 1e-10))
      expect_all(close_to(true_turnout, p025, tol = 1e-10))
      expect_all(close_to(true_turnout, p975, tol = 1e-10))
    }
  )
  
  if(FALSE){  
    ggplot(
      turnout_sim_stats %>% filter(simulated),
      aes(y = true_turnout, x=mean)
    ) +
      geom_point() +
      geom_abline(slope=1, intercept=0)+
      coord_fixed()
  }
  
  simulated_summary <- turnout_sim_stats %>% 
    filter(simulated) %>%
    summarise(
      pred_cor = cor(true_turnout, mean),
      true_turnout = sum(true_turnout),
      mean = sum(mean)
    ) %>% 
    left_join(turnout_sim_overall %>% filter(simulated))
  
  # Broad tests checking that the results aren't insane
  with(
    turnout_sim_stats %>% filter(simulated),
    {
      print(sprintf("Pred Votes: %0.0f. True Votes: %0.0f", sum(mean), sum(true_turnout)))
      print(sprintf("Cor:  %0.3f", cor(mean, true_turnout)))
      print(sprintf("CI Coverage: %0.3f", mean(ci_covers)))
      # stop("THIS IS BAD")
      # expect_close(sum(mean), sum(true_turnout), sum(true_turnout)/2)
      expect_close(cor(true_turnout, mean), 1.0, tol=0.15)
      # expect_gt(mean(ci_covers), 0.8)
    }
  )
  
  ############################
  ## OFFICE
  ############################
  
  pvote_sim <- simulate_pvote(
    df=df_samp, 
    use_office=office, 
    pvote_svd=pvote_svd,
    n_winners=n_winners,
    turnout_sim=turnout_sim, 
    verbose=TRUE,
    consider_divs=consider_divs
  )
  
  true_pvote <- df_major %>%
    filter(year==!!year, election==!!election, office==!!office) %>%
    rename(true_pvote = pvote, true_votes=votes)
  
  pvote_sim_stats <- pvote_sim$office_sim %>% 
    group_by(warddiv, candidate, simulated) %>%
    summarise(
      mean_target = mean(target),
      mean = mean(pvote),
      p025 = quantile(pvote, 0.025),
      p975 = quantile(pvote, 0.975)
    ) %>%
    ungroup() %>%
    left_join(true_pvote) %>%
    mutate(ci_covers = (true_pvote >= p025) & (true_pvote <= p975))
  
  # Exact tests to check sanity among reporting
  with(
    pvote_sim_stats %>% filter(!simulated),
    {
      ## mean(pvote) is adjusted for total pvote. Only target is exact
      print(sprintf("N(divs): %s", length(unique(warddiv))))
      print(cbind(true_pvote, mean) %>% head)
      # print(sprintf("Cor: %s", cor(transformed_target, mean)))
      
      expect_equal(length(unique(warddiv)), n_reporting)
      expect_all(close_to(mean, true_pvote, tol = 0.01))
      # expect_close(cor(transformed_target, mean), 1.0, tol=0.10)
      expect_all(close_to(mean, p025, tol = 1e-10))
      expect_all(close_to(mean, p975, tol = 1e-10))
    }
  )
  
  within_cand_coefs <- pvote_sim_stats %>% 
    filter(simulated) %>%
    group_by(candidate) %>%
    do(lm(true_pvote ~ mean, data=., weights = true_votes/true_pvote) %>% broom::tidy()) %>%
    filter(term == "mean")
  
  print(sprintf(
    "Mean Coefficient of pvote on prediction: %0.3f", 
    mean(within_cand_coefs$estimate)
  ))
  # expect_gt(
  #   mean(within_cand_coefs$estimate), 0.0
  # )
  
  if(FALSE){
    ## CIs are often too high. This is probably the pvote correction.
    pvote_sim_stats %>% 
      mutate(
        ci_too_low = true_pvote > p975,
        ci_too_high = true_pvote < p025,
        ci_covers = !ci_too_high & !ci_too_low
      ) %>%
      group_by(candidate) %>%
      summarise(
        ci_too_low = mean(ci_too_low),
        ci_too_high = mean(ci_too_high),
        ci_covers = mean(ci_covers)
      ) 
    
    pvote_sim_stats %>% 
      mutate(
        ci_covers = (true_pvote <= p975) & (true_pvote >= p025)
      ) %>%
      summarise(
        ci_covers = mean(ci_covers)
      ) 
    
    print(pvote_sim$needle)
    print(pvote_sim$map)
    
    ggplot(
      pvote_sim_stats %>% 
        filter(simulated) %>% 
        # filter(candidate=="ALLAN DOMB") %>%
        mutate(pred_rounded = round(mean, 2)) %>%
        group_by(candidate, pred_rounded) %>%
        summarise(
          total_votes = sum(true_votes/true_pvote),
          count = n(),
          true_pvote = weighted.mean(true_pvote, w=true_votes/true_pvote)
        ),
      aes(y = true_pvote, x=pred_rounded)
    ) +
      geom_point(aes(size=total_votes), alpha = 0.2) +
      geom_abline(slope=1, intercept=0)+
      coord_fixed() 
      # xlim(0, 0.1)
    
    hist(within_cand_coefs$estimate)
  }
  
  winner_results <- pvote_sim$winners_sim %>%
    left_join(
      true_pvote %>% 
        group_by(candidate) %>% 
        summarise(votes=sum(true_votes)) %>%
        ungroup() %>%
        mutate(
          winner = rank(desc(votes)) <= n_winners,
          true_pvote = votes / sum(votes)
        )
    )
  
  print(winner_results %>% arrange(desc(pvote)))
  
  expect_true(all(winner_results$winner[winner_results$pwin == 1]))
  expect_true(all(!winner_results$winner[winner_results$pwin == 0]))
  
  results <- bind_rows(
    results, 
    winner_results %>% mutate(year=!!year, election=!!election, office=!!office, sim=!!j)
  )
}

print(turnout_results)
turnout_results %>% filter(simulated) %>% with(mean(ci_covers))

print(results)

with(
  results %>% filter(pwin > 0 & pwin < 1),
  hist(pwin)
)

ggplot(
  results %>% filter(pwin > 0 & pwin < 1) %>%
    mutate(pred_bin = 0.05 + 0.1 * floor(pwin * 10)) %>%
    group_by(pred_bin) %>%
    summarise(mean_pred = mean(pwin), mean_win = mean(winner), n=n()),
  aes(x=mean_pred, y=mean_win)
) + 
  geom_point(aes(size=n)) +
  scale_size_area() +
  geom_abline(slope=1, intercept=0) +
  coord_fixed() +
  ylim(0,1)

setwd(olddir)
