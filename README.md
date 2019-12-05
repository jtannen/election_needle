# The Needle

The needle is a script that downloads live, partial election results, projects out the remaining precincts, and predicts the results.

# Running the Needle

## Tests
The easiest, minimal demo of running the needle is in `tests/test_past_elections.R`. That script 
- uses `calc_params()` to precompute the covariance among precincts.
- loads the historic election results and samples from it.
- uses `simulate_turnout()` to simulate turnout given the sampled divisions.
- uses `simulate_pvote()` to simulate the votes for a given race, given the sampled divisions.
- aggregates the results and tests the calibration of the needle.

## Election Night
Before election night, 
- prep the config file similar to files in `configs`.
- run `calc_params()` at the bottom of `prep_data.R`, and save the params.

On election night,
- use the script `run_needle.R` to 
  - download the results, 
  - run the analysis, and 
  - generate the markdown.
  
## Interested in using the needle?
Please reach out to me at jonathan dot tannen at gmail dot com!