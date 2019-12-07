library(testthat)

test_folder <- "C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/election_night_needle/tests/"

test_files <- list.files(
  pattern = "^test_", 
  path = test_folder,
  full.names = FALSE
)

header_text <- function(text){
  banner <- "################################"
  sprintf("%s\n## %s\n%s\n", banner, text, banner)
}

for(test_file in test_files){
  cat(header_text(sprintf("RUNNING TEST %s", test_file)))
  source(sprintf("%s/%s", test_folder, test_file), chdir=TRUE)
  print(sprintf("DONE %s", test_file))
}
