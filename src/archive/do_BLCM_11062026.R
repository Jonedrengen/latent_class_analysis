#libs
library(ggplot2)
library(rjags)
library(R2jags)
load.module("glm")
library(label.switching)
library(optparse)
library(grDevices)
library(tidyverse)
library(coda)


#command line arguments
opt_lst <- list(
  make_option(c("-i", "--input"), help = "please provide input"),
  make_option(c("-o", "--output"), help = "please provide output location")
)
#parsing
parser <- OptionParser(option_list = opt_lst)
args <- parse_args(parser, positional_arguments = T)
options <- args$options

#opts
input <- options$input
output <- options$output

#testing
input_dat <- read.csv(
  "/Users/B328695/Desktop/Latent_class_analysis/Test8Isolates_blcm_input.csv"
)

BLCM_input <- input_dat %>%
  select(
    Sample_Name,
    training,
    Human_CL1,
    Human_CL2,
    Chicken_CL1,
    Chicken_CL2,
    Pork,
    c(9:ncol(input_dat))
  )

#results_folder path
result_folder <- "/Users/B328695/Desktop/Latent_class_analysis/results_test"

set.seed(67)
class_label <- rep(NA, nrow(BLCM_input))
head(class_label)
for (i in 1:nrow(BLCM_input)) {
  if (BLCM_input$training[i] == 1) {
    if (!is.na(BLCM_input$Human_CL1[i]) && BLCM_input$Human_CL1[i] == 1) {
      class_label[i] = 1
    }
    if (!is.na(BLCM_input$Human_CL2[i]) && BLCM_input$Human_CL2[i] == 1) {
      class_label[i] = 2
    }
    if (!is.na(BLCM_input$Chicken_CL1[i]) && BLCM_input$Chicken_CL1[i] == 1) {
      class_label[i] = 3
    }
    if (!is.na(BLCM_input$Chicken_CL2[i]) && BLCM_input$Chicken_CL2[i] == 1) {
      class_label[i] = 4
    }
    if (!is.na(BLCM_input$Pork[i]) && BLCM_input$Pork[i] == 1) {
      class_label[i] = 5
    }
  }
}
ntrain = nrow(BLCM_input)
test_id <- which(BLCM_input[1:ntrain, ]$training == 0)

#remove all cols which are not features
feature_cols <- setdiff(
  names(BLCM_input),
  c(
    "Sample_Name",
    "training",
    "MLST",
    "Human_CL1",
    "Human_CL2",
    "Chicken_CL1",
    "Chicken_CL2",
    "Pork"
  )
)
Y <- as.matrix(BLCM_input[, feature_cols])
storage.mode(Y) <- "numeric"
#remove col- and rownames
dimnames(Y) <- NULL

# Fit Bayesian model:
#Note: iterations and burnin are 10,000 and 5,000 by default, but 1,000 and 500 for this course
mcmc_options <- list(
  debugstatus = TRUE,
  n.chains = 1,
  n.itermcmc = 1000,
  n.burnin = 500,
  n.thin = 1,
  result.folder = result_folder,
  bugsmodel.dir = result_folder
)
# write .bug model file:
model_bugfile_name <- "model.bug"
filename <- file.path(mcmc_options$bugsmodel.dir, model_bugfile_name)
#The math, written in jags language.
model_text <- "model{
  # Likelihood
  for (i in 1:N){
    for (k in 1:K){
      Y[i,k] ~ dbern(p[eta[i],k])
    }
    eta[i] ~ dcat(pi[1:M_fit])
  }
  
  # --- Prior for Class Proportions (pi) using Softmax ---
  
  # 1. Calculate exponential of 'a' element-wise 
  for (j in 1:M_fit){
    a[j] ~ dnorm(0, 4/9)
    exp_a[j] <- exp(a[j])
  }
  
  # 2. Sum the exponentials (Scalar sum of the vector)
  sum_exp_a <- sum(exp_a[1:M_fit])
  
  # 3. Calculate pi and define Feature Probabilities
  for (j in 1:M_fit){
    pi[j] <- exp_a[j] / sum_exp_a
    
    # Feature probabilities (Logistic transformation)
    for (k in 1:K){
      p[j,k] <- 1/(1+exp(-g[j,k]))
      g[j,k] ~ dnorm(0, 4/9)
    }
  }
} #END OF MODEL."
writeLines(model_text, filename)


#classes
M_fit <- 5
N <- nrow(Y)
K <- ncol(Y)
# eta represents the latent classes for each sample. Actual classes for training and NAs for testing.
eta <- as.numeric(class_label)
eta[test_id] <- NA

# ensuring training labels are valid
Y <- as.matrix(Y)
storage.mode(Y) <- "numeric"

# drop dimnames
dimnames(Y) <- NULL


# Create a named list for the data
jags_data <- list(Y = Y, M_fit = M_fit, N = N, K = K, eta = eta)

# pi: the estimated class proportions (mixture weights)
# p: the probability of each feature in each latent class
# eta: sampled class assignments for each sample
out_parameter <- c("pi", "p", "eta")

# Initialize 'a' with length M_fit
in_init <- function() {
  list(a = rep(0, M_fit))
}

#Running the model!
out <- R2jags::jags(
  data = jags_data,
  inits = in_init,
  parameters.to.save = out_parameter,
  model.file = filename,
  n.iter = as.integer(mcmc_options$n.itermcmc),
  n.burnin = as.integer(mcmc_options$n.burnin),
  n.thin = as.integer(mcmc_options$n.thin),
  n.chains = as.integer(mcmc_options$n.chains),
  DIC = FALSE
)

#Obtain the chain histories:
print_res <- function(x, coda_res) plot(coda_res[, grep(x, varnames(coda_res))])
get_res <- function(x, coda_res) coda_res[, grep(x, varnames(coda_res))]


res <- as.mcmc(out)
raw_out <- res[[1]]
row1 <- data.frame(row1 = raw_out[1,])
class(row1)
unique(colnames(raw_out))





