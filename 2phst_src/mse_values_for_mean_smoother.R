# Calculates MSE for predictions of the next wave using other smoothers
# TODO(jcf26@): compute these in one file
# Author: Jake Fisher

# Set up workspace
rm(list = ls())
setwd("/misc/utopia3/jcf26/lanhome/DissertationShare/social_space_diffusion_r_and_r")
library(prosperHelper2)
library(hydroGOF)  # for mse
library(latentnet)
library(latentnetDiffusion)
library(dplyr)
library(dtplyr)
library(tidyr)
library(parallel)

meanImputation <- function(x) {
  # Mean-imputes missing values for a vector x
  #
  # NOTE: see http://www.mail-archive.com/r-help@r-project.org/msg58289.html
  #
  # Args:
  #   x: numeric vector
  #
  # Returns:
  #   x, with missing values replaced by mean(x)
  return(replace(x, is.na(x), mean(x, na.rm = T)))
}

makeConstructMatrix <- function(net, a) {
  # Calculates Friedkin's construct matrix (C) from pg. 40 of his 2011 book
  #
  # Args:
  #   net: statnet network object
  #   a: numeric scalar between 0 and 1, indicates how heavily to weight network
  #      vs. individual (a = 1 is all individual, a = 0 is all network)
  # 
  # TODO(jcf26@): allow a to be a vector, such that each person gets his or her
  #               own weight
  #
  # Returns:
  #   A reweighted square matrix
  
  # Check a
  stopifnot(length(a) == 1)
  stopifnot(is.numeric(a))
  stopifnot(a <= 1 | a >= 0)
  
  # Check net, and convert it to an adjacency matrix if necessary
  if (is.network(net)) {
    mat <- as.matrix(net)
  } else if (is.matrix(net)){
    stopifnot(nrow(net) == ncol(net))
    mat <- net
  } else {
    stop("net must be a statnet network or a square matrix object")
  }
  
  diag(mat) <- 0
  C <- rowNormalize(mat)
  
  rounded.rowsums <- round(rowSums(C), floor(-log10(.Machine$double.eps)))
  
  if (!all(rounded.rowsums %in% c(0, 1)))
    warning("C matrix values do not sum to 0 or 1")
  
  # Create diagonal matrix A and identity matrix I
  I <- diag(nrow(C))
  A <- I * a
  
  # Fix isolated dyads -- definitionally (my defn.) assigned to weight the last
  # observation at 1
  isos <- which(rounded.rowsums == 0)
  A[isos, isos] <- 0
  
  # Convert to a weight matrix and return
  return(A %*% C + I - A)
}

getMSE <- function(net, attitude, diag.weight, iter = 5, dat = wv.minus.1) {
  # Calculates the MSE value for 5 iterations on a given network and attitude,
  # with a given diagonal weight
  #
  # Args:
  #   net: a statnet network object, with attitude as a vertex attribute
  #   attitude: a character variable denoting a vertex attribute on net, which 
  #             should be a numeric vector (i.e., the v. attr. should be num.)
  #   diag.weight: parameter passed as "a" to makeConstructMatrix
  #   iter: number of iterations to run
  #   dat: data where the "true" values of the attitude should be looked up
  #        (expectation is that we have PROSPER.survey[, wave := wave + 1])
  #   smoother: character string indicating which smoother we should apply to 
  #             the network
  #
  # TODO(jcf26@): break this into two smaller functions
  #
  # NOTE: none of these parameters are tested; use with care
  # 
  # Returns:
  #   a scalar value
  

  # Make sure we're not pulling more than one attitude, or else this will break
  stopifnot(length(attitude) == 1)
  
  # Get "mean smoothed" matrix (i.e., matrix of "1"'s)
  mat <- matrix(1, ncol = network.size(net), nrow = network.size(net))
  
  # First calculate predicted diffusion values using net and attitude
  # Note that we have to "try" because nets[[45]] is 2 vertices, and as.matrix
  # fails on it, inexplicably
  # TODO(jcf26@): create a statnet bug report for nets[[45]]
  pred <- try(degroot(
    makeConstructMatrix(mat, diag.weight),
    matrix(meanImputation(net %v% attitude), ncol = 1),
    iterations = iter,
    all.iter = T,
    row.normalize = F  # NOTE: will warn, that's okay
  ))
  
  # Next pull the "correct" data using prosperHelper2
  Y.obs <- unlist(getPROSPERVariableByNetwork(net, attitude, dat))
  
  # Return NA if degroot failed earlier, otherwise calculate MSE values
  # NOTE: do this here, rather than with ifelse, because ifelse returns a value
  # whose type matches the "test" (i.e., if test is length 1, return length 1
  # object)
  if (class(pred) == "try-error") {
    next.wave.mses <- NA
  } else {
    next.wave.mses <- apply(pred, 2, mse, obs = Y.obs)
  }
  
  # Finally, return the MSE's -- return them in a format that matches other
  # approaches
  net.name <- net %n% "title"
  out <- data.frame(
    school = getSchool(net.name),
    cohort = getCohort(net.name),
    wave = getWave(net.name),
    attitude = attitude,
    prediction.number = NA,
    iteration = 0:iter,
    model.type = "locf",
    simulated = NA,
    diag.weights = diag.weight,
    next.wave.mse = next.wave.mses
  )
  
  return(out)
}

# Load data
data(PROSPER.networks)
data(PROSPER.survey)

# Construct a network object that we can loop over
attitudes <- c("cattus", "cavgdev", "cexpt", "cfmre", "csuatt_r", "csumscab")
nets <- subsetPROSPERNetworks(wave = 1:7) %>%
  lapply(FUN = addPROSPERVertexAttribute.dt, attribute = attitudes)

# Adjust PROSPER.survey so that we can use prosperHelper2 functions to subset it
wv.minus.1 <- PROSPER.survey
wv.minus.1[, wave := wave - 1]
setkey(wv.minus.1, id, cohort, wave)

# Now loop through all of the possible attitudes and diagonal weightings
permutations <- expand.grid(attitudes, seq(from = 0, to = 1, by = .1),
                            stringsAsFactors = F) %>%
  setNames(nm = c("attitudes", "diag.weights")) 

locf.mse <- with(permutations, mcMap(
  function(i, j) bind_rows(lapply(nets, getMSE, attitude = i, 
                                  diag.weight = j)),
  i = attitudes,
  j = diag.weights,
  mc.cores = 24
)
) %>%
  bind_rows %>%
  tbl_dt

# Save the output
save(locf.mse, file = "locf_smoother_mses.Rdata")



# # Comparison with manual approach -- make sure that the "1 matrix" is correct!
# indPlusMean <- function(gam, attitude) {
#   # Calculates expected attitude change vs. a weighted combination of the
#   # individual and the class mean
#   #
#   # Args:
#   #   gam: the weight given to an individual (numeric in [0, 1])
#   #   attitude: values for the attitude for everyone in the class (numeric)
#   #
#   # Returns:
#   #   Weighted combinations of attitude.  Specifically gam * attitude + 
#   #   (1 - gam) * mean(attitude)
#   
#   # Version removing individual i's value from the calculation of the mean
#   mean.vec <- vapply(
#     seq_along(attitude),
#     FUN = function(i) mean(attitude[-i], na.rm = T),
#     FUN.VALUE = 1,
#     USE.NAMES = F
#     )
#   return(gam * attitude + (1 - gam) * mean.vec)
#   # original version:
#   # return(gam * attitude + (1 - gam) * mean(attitude, na.rm = T))
# }
# 
# # Note that values are reversed in parameter:
# pred.manual <- indPlusMean(.1, meanImputation(net1 %v% attitude))
# mse.manual <- mse(pred.manual, Y.obs)
# 
# out
# mse.manual  # not the same... because we're including i's weight in it...