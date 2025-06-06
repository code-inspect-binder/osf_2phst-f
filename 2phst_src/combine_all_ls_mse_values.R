# Combines diffusion results into a single dataset
# Author: Jake Fisher
# Edited 23 Aug. 2017 -- commented lines are original code

# Set up workspace
rm(list = ls())
setwd("~/lanhome/DissertationShare/social_space_diffusion_r_and_r")
library(dplyr)
library(dtplyr)

loadFiles <- function(filename) {
  # Modified "load" that returns the object loaded, instead of the object's name
  #
  # Args:
  #   filename: character string indicating what file should be loaded
  #
  # Returns:
  #   the object contained in filename
  obj.name <- load(filename)
  return(get(obj.name))
}

# Find all of the files to combine
# prediction.files <- list.files(pattern = glob2rx("ls_mses*Rdata"))
prediction.files <- list.files("~/dscr_social_space_diffusion", full.names = T)

tmp <- lapply(prediction.files, loadFiles)
# ls.mses <- bind_rows(tmp) %>% tbl_dt
full.post.mses <- bind_rows(tmp) %>% tbl_dt

# save(ls.mses, file = "ls_mses_combined.Rdata")
save(full.post.mses, file = "full_post_mses_combined.Rdata")
