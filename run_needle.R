library(rmarkdown)
setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/election_night_needle/")

while(TRUE){
  print(Sys.time())
  print("rmarkdown")
  rmarkdown::render( 
    "needle.Rmd", 
    output_file = "election_needle.html"
  )
  
  print("copy and git")
  file.copy(
    "election_needle.html",
    "C:/Users/Jonathan Tannen/Dropbox/github_page/jtannen.github.io/election_needle.html",
    overwrite=TRUE
  )
  
  system("upload_git.bat")
}
