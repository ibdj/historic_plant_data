
install.packages("pacman")
pacman::p_load(usethis, tidyverse,googlesheets4,rgbif,janitor,ids)


#### setting up git and github ####
use_git_config(
  user.name = "ibdj", 
  user.email = "ibdjacobsen@gmail.com"
)

# see tokens https://github.com/settings/tokens
# usethis::create_github_token()
# gitcreds::gitcreds_set()

git_vaccinate() 

usethis::use_git()

use_github()



