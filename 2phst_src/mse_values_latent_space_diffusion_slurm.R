# Calculates next-wave MSE values for latent space diffusion model specification
# Author: Jake Fisher

# Notes: Combines 3 files: run_one_model.R, save_diffusion_results_test.R, and
# calculate_one_mse.R.  These three files used to:
# (1) run a latent space model on a given school, 
# (2) get latent space diffusion predictions from that model, and
# (3) calculate the MSE from the predictions.
# Saving each of the intermediate files took up an extraordinary amount of
# space, so I'm going to combine them here, and save the MSE on the fly.
#
# This is designed to be run from batch by _____.sh on a SLURM cluster.  A key
# goal of this script is minimizing intermediate memory use, because if you req-
# uest less memory, your job gets processed faster.  That's why this has been
# retooled to get predicted values and calculate MSEs one prediction at a time,
# because that way we don't consume like 1.5+GB of memory.

# Set up workspace
rm(list = ls())
# setwd("/hpchome/ssri/jcf26/social_space_diffusion")
set.seed(919)
library(prosperHelper2)
library(statnet)
library(latentnet)
library(latentnetDiffusion)
library(dplyr)
library(dtplyr)
library(tidyr)
library(magrittr)
library(hydroGOF)

meanImputation <- function(x) {
  # Fills in missing values of x with the mean of x
  #
  # Args:
  #   x: a numeric vector
  #
  # Returns:
  #   x with missing values filled in
  return(replace(x, is.na(x), mean(x, na.rm = T)))
}

mseOfMatchingColumns <- function(nm, mat, dt) {
  # Given a column name, a matrix, and a data.table, calculates the MSE between
  # all of the columns of the matrix and the data.table whose columns have that
  # column name
  #
  # Args:
  #   nm: column name to extract
  #   mat: matrix with at least one column named nm
  #   dt: data.table with one variable named nm
  #
  # Returns:
  #   A data.table with 3 columns: attitude, which is just nm, iteration, which
  #   indexes the number of columns in mat that are named nm, and next.wave.mse
  #   which is the mse of the column of mat with the variable in dt
  stopifnot(is.character(nm))
  stopifnot(length(nm) == 1)
  stopifnot(is.matrix(mat))
  stopifnot(is.data.table(dt))
  stopifnot(nm %in% names(dt))
  
  mat.names <- colnames(mat)
  stopifnot(nm %in% mat.names)
  
  return(
    data.table(
      attitude = nm,
      iteration = as.integer(0:(sum(mat.names == nm) - 1)),
      next.wave.mse = apply(mat[, which(colnames(mat) == nm)], 2, mse,
                            obs = unlist(dt[, nm, with = F]))
    )
  )
}

ergmmMSE <- function(self.weight, next.wave, simulate, model.obj = fit,
                     Y.init = Y, all.iter = T, draws = 100, iterations = 5,
                     row.normalize = T) {
  # Runs the ergmmDegroot function, then calculates the MSE for predicting
  # "next.wave"
  #
  # Args:
  #   self.weight: passed to ergmmDegroot
  #   next.wave: data.table with variables to match the column names in Y
  #   simulate: passed to ergmmDegroot
  #   model.obj: passed to ergmmDegroot as ergmm
  #   Y.init: passed to ergmmDegroot as Y
  #   all.iter: passed to ergmmDegroot as is
  #   draws: passed to ergmmDegroot as is
  #   iterations: passed to ergmmDegroot as is
  #   row.normalize: passed to ergmmDegroot as is
  #
  # Note: assumes that next.wave and model.obj ID values are in the same order
  # getPROSPERVariableByNetwork should do that automatically, but this function
  # does not verify that that is the case.  Use with caution.
  #
  # Returns:
  #   data.table with columns for the next wave MSE, and _____!?
  
  stopifnot(is.data.table(next.wave))
  
  unique.attitudes <- unique(colnames(Y.init))
  stopifnot(all(unique.attitudes %in% names(next.wave)))
  
  # Get predicted values from ergmmDegroot
  pred <- ergmmDegroot(ergmm = model.obj, Y = Y.init, draws = draws,
                       simulate = simulate, iterations = iterations,
                       all.iter = all.iter, self.weight = self.weight)
  
  # Compare with the observed values from next wave
  # The output is in a nested list, so I use several loops to unpack it.
  # Loops are not super slow, so don't worry about fixing this. Seriously.
  # First, loop through the draws:
  out.draws <- vector(mode = "list", length = draws)
  for (d in 1:draws) {
    
    # Then loop through the attitude values (via lapply) and get the set of 
    # For each attitude value:
    out.draws[[d]] <- lapply(unique(colnames(Y.init)), mseOfMatchingColumns,
                             mat = pred[[d]], dt = next.wave) %>%
      rbindlist
  }
  
  
  # Combine the whole thing and return
  return(rbindlist(out.draws,
                   idcol = "prediction.number")[, simulate := simulate])
}


# Get the environment variable for the loop iteration number
# array.id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

# Get command line arguments
model.spec <- commandArgs(trailingOnly = TRUE)[1]
fit.type <- commandArgs(trailingOnly = TRUE)[2]

# Samples:
# array.id <- 400  # success
# array.id <- 45  # failure
array.id <- 418  # re-run on charisma because DCC ran out of memory
# model.spec <- as.character("net ~ euclidean(d = 2)")
# fit.type <- "fit2d"

# Load the appropriate file
# load(paste0("w68_", array.id, ".Rdata"))
load(paste0("data/w17_", array.id, ".Rdata"))

# Save various network attributes for later
net.id <- net %n% "title"
ids <- network.vertex.names(net)
ch <- getCohort(net.id)
wv <- getWave(net.id)

# Run the latent space model
fit <- try(ergmm(as.formula(model.spec)))

if (class(fit) != "try-error") {
  
  # Attitude values to calculate the MSE for
  attitude.vars <- c("csumscab", "cattus", "cexpt", "csuatt_r", "cavgdev",
                     "cfmre")
  
  # Load the data
  data(PROSPER.survey)
  
  # Only save the relevant subset of the data (uses less memory)
  PROSPER.survey %<>% filter(id %in% ids, cohort == ch)
  
  # Get estimates of diffusion of an attitude
  Y <- getPROSPERVariableByNetwork(net, attitude.vars)
  
  # Mean imputation, if necessary
  Y <- apply(Y, 2, meanImputation)
  
  # Required for matrix multiplication in latentnetDiffusion::degroot
  Y <- as.matrix(Y)
  
  # Get next wave, "true" values for mse comparison
  Y.next.wave <- getPROSPERVariableByNetwork(
    net,
    attitude.vars, 
    mutate(PROSPER.survey, wave = wave - 1) %>% setkey(id, cohort, wave)
    )
  
  # Get latent space diffusion outcomes across each value of self.weight
  self.weights <- seq(from = 0, to = 1, by = .1)
  ls.mses <- bind_rows(
    
    # Calculate predicted probabilities result
    bind_rows(lapply(self.weights, ergmmMSE, next.wave = Y.next.wave,
                     simulate = F), .id = "sw"),
    
    # Calculate posterior predictive dist. result
    bind_rows(lapply(self.weights, ergmmMSE, next.wave = Y.next.wave,
                     simulate = T), .id = "sw")
      ) %>%
    
    # Some clean up after row-binding them together
    mutate(diag.weights = self.weights[as.integer(sw)],
           school = getSchool(net.id), cohort = ch, wave = wv,
           model.type = fit.type) %>%
    select(-sw) %>%
    mutate_each(funs(as.integer), school, cohort, wave, prediction.number, 
                iteration) 
  
  # Save the results as a dataset, which will then be bound together using a diff-
  # erent script.
  save(ls.mses, file = paste0("ls_mses_", fit.type, "_", array.id, ".Rdata"))
}
