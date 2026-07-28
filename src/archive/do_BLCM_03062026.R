
#load
library(R2jags)
library(rjags)
load.module("glm")
library(label.switching)
library(ggplot2)
library(optparse)
library(grDevices)

opt_lst <- list(
    make_option(c("-i","--input"),
              help = "please provide input"),
    make_option(c("-o","--output"),
              help = "please provide output location") 
)
parser <- OptionParser(option_list = opt_lst)
args <- parse_args(parser,positional_arguments = T)
options <- args$options
input <- options$input
output <- options$output

#reproduce
set.seed(67)

#input
dat <- read.csv(input)
#results folder is output
result_folder <- output

#__create_input_checks__

#TODO

#__create_input_checks__

#create train
class_label <- rep(NA,nrow(dat))
head(class_label)
for (i in 1:nrow(dat)){
    if (dat$training[i] == 1){
        if (!is.na(dat$Human_CL1[i]) && dat$Human_CL1[i] ==1) class_label[i]=1
        if (!is.na(dat$Human_CL2[i]) && dat$Human_CL2[i] ==1) class_label[i]=2
        if (!is.na(dat$Chicken_CL1[i]) && dat$Chicken_CL1[i] ==1) class_label[i]=3
        if (!is.na(dat$Chicken_CL2[i]) && dat$Chicken_CL2[i] ==1) class_label[i]=4
        if (!is.na(dat$Pork[i]) && dat$Pork[i] ==1) class_label[i]=5}
}
ntrain = nrow(dat)
test_id <- which(dat[1:ntrain,]$training==0)


#remove all cols which are not features
feature_cols <- setdiff(
  names(dat),
  c("Sample_Name","training", "MLST",
    "Human_CL1","Human_CL2",
    "Chicken_CL1","Chicken_CL2",
    "Pork")
)
Y <- as.matrix(dat[, feature_cols])
storage.mode(Y) <- "numeric"
#remove col- and rownames
dimnames(Y) <- NULL



# Fit Bayesian model:
#Note: iterations and burnin are 10,000 and 5,000 by default, but 1,000 and 500 for this course
mcmc_options <- list(debugstatus= TRUE,
                     n.chains   = 1,
                     n.itermcmc = 1000, 
                     n.burnin   = 500, 
                     n.thin     = 1,
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
eta <- as.numeric(class_label)
eta[test_id] <- NA

# ensuring training labels are valid
Y <- as.matrix(Y)
storage.mode(Y) <- "numeric"

# drop dimnames 
dimnames(Y) <- NULL

# Create a named list for the data
jags_data <- list(Y = Y, 
                  M_fit = M_fit, 
                  N = N, 
                  K = K, 
                  eta = eta)


# pi: the estimated class proportions (mixture weights)
# p: the probability of each feature in each latent class
# eta: sampled class assignments for each sample
out_parameter <- c("pi","p","eta")

# Initialize 'a' with length M_fit
in_init <- function(){
  list(a=rep(0, M_fit)) 
}

#Running the model!
out <- R2jags::jags(data   = jags_data,
                    inits  = in_init,
                    parameters.to.save = out_parameter,
                    model.file = filename,
                    n.iter         = as.integer(mcmc_options$n.itermcmc),
                    n.burnin       = as.integer(mcmc_options$n.burnin),
                    n.thin         = as.integer(mcmc_options$n.thin),
                    n.chains       = as.integer(mcmc_options$n.chains),
                    DIC            = FALSE)

#Obtain the chain histories:
print_res <- function(x,coda_res) plot(coda_res[,grep(x,colnames(coda_res))])
get_res   <- function(x,coda_res) coda_res[,grep(x,colnames(coda_res))]

#
res <- as.mcmc(out) 

#write a traceplot
# png("pre_steph_trace.png", width = 800, height = 600)
# traceplot(res, varname = "pi", ylim = c(0,1))
# dev.off()

p_samples <- as.matrix(get_res("p", res))

# dimensions: iterations x (M_fit * K)
n_iter <- nrow(p_samples)
p_array <- out$BUGSoutput$sims.list$p

#Stephens relabeling
#Run the algorithm to detect if the model swapped class names during the run
steph <- stephens(p_array)

eta_samples <- out$BUGSoutput$sims.list$eta
eta_relab <- eta_samples

for (i in 1:n_iter) {
  perm <- steph$permutations[i, ]
  eta_relab[i, ] <- perm[eta_samples[i, ]]
}

#extract the model's classification guesses for the test samples
mat_test <- eta_relab[, test_id]

#calculate the average probability (0.0 to 1.0) for each of the classes
v1 <- apply(mat_test,2,function(v) mean(v==1))
v2 <- apply(mat_test,2,function(v) mean(v==2))
v3 <- apply(mat_test,2,function(v) mean(v==3))
v4 <- apply(mat_test,2,function(v) mean(v==4))
v5 <- apply(mat_test,2,function(v) mean(v==5))
#Combine the 10 probability scores with the original sample info into a single table called res_dat
res_dat <- cbind(v1,v2,v3,v4,v5,dat[test_id,1:10])

colnames(res_dat)[1:8] <- c("Human_pred_CL1","Human_pred_CL2", "Chicken_pred_CL1", "Chicken_pred_CL2","Pork", "Sample_Name", "training", "MLST")

#define human and animal classes
human_classes  <- 1:2
animal_classes <- 3:5

#adding a text label for the true origin (based on BLCM predictions)
isolate_source <- rep(NA_character_, length(test_id))
#filling isolate_source with labels, if 1 in main dat
isolate_source[dat$Human_CL1[test_id] == 1 |
                 dat$Human_CL2[test_id] == 1] <- "human"

isolate_source[dat$Chicken_CL1[test_id] == 1 |
                 dat$Chicken_CL2[test_id] == 1] <- "chicken"

isolate_source[dat$Pork[test_id] == 1] <- "pork"

#facor
isolate_source <- factor(
  isolate_source,
  levels = c("human", "chicken", "pork")
)


#calculate the total combined probability for "Human" vs "Animal" origin
mat_test <- eta_relab[, test_id]
post_prob <- apply(mat_test, 2, function(v)
  tabulate(v, nbins = M_fit) / length(v))
human_total  <- colSums(post_prob[human_classes, , drop = FALSE])
animal_total <- colSums(post_prob[animal_classes, , drop = FALSE])
test_is_human <- isolate_source == "human"


# Define FZEC (predictions that cross 0.8 for animal-origin)
fzec_status <- ifelse(
  test_is_human,
  animal_total > 0.80,
  NA
)

#creating the final table to be written as output
results_out <- data.frame(
  sample_id      = dat$Sample_Name[test_id],
  isolate_source = isolate_source,
  human_total    = human_total,
  animal_total   = animal_total,
  is_human       = test_is_human,
  FZEC           = fzec_status,
  stringsAsFactors = FALSE
)
results_out$isolate_source <- factor(
  results_out$isolate_source,
  levels = c("human", "chicken", "turkey", "pork")
)

# Calculate percentage of human samples that are Zoonotic 
fzec_prop <- mean(results_out$FZEC == TRUE, na.rm = TRUE)

# res_dat = pred_scores.csv
pred_scores_results <- file.path(output, "pred_scores.csv")
logger(res_dat)
write.csv(res_dat, pred_scores_results)

# results_out = FZEC_summary.csv
FZEC_sum_results <- file.path(output, "FZEC_summary.csv")
write.csv(results_out,
          file = FZEC_sum_results,
          row.names = FALSE)





# __________________visualization__________________




# copy the model's classification results
mat_all <- eta_relab

# calculate the source probabilities (0 to 1) for every sample based on the 10 defined classes
post_prob_all <- apply(mat_all, 2, function(v)
  tabulate(v, nbins = M_fit) / length(v))

# Group the 10 classes into two big buckets: 'Total Human' and 'Total Animal' probability
human_total_all  <- colSums(post_prob_all[human_classes, , drop = FALSE])
animal_total_all <- colSums(post_prob_all[animal_classes, , drop = FALSE])

# Which samples were labeled as 'Human'
true_human <- rowSums(dat[, c("Human_CL1","Human_CL2")], na.rm = TRUE) > 0
# Which samples were labeled as 'Animal' ((Chicken, Turkey, or Pork)
true_animal <- rowSums(dat[, c(
  "Chicken_CL1","Chicken_CL2",
  "Pork"
)], na.rm = TRUE) > 0

# Combine the IDs, training status, true sources, and model predictions into one plot df
plot_df <- data.frame(
  sample_id    = dat$Sample_Name, 
  training     = dat$training,
  true_source  = ifelse(true_human, "Human", "Animal"),
  human_total  = human_total_all,
  animal_total = animal_total_all
)




#________________Create plots_________________



#create plot to show animal probablity in human samples
plot_human <- ggplot(subset(plot_df, true_source == "Human" & training == 0),
       aes(x = animal_total)) +
  geom_histogram(binwidth = 0.02, fill = "gold", color = "black", alpha = 0.6) +
  geom_vline(xintercept = 0.8, linetype = "dashed", linewidth = 1) +
  labs(
    x = "Animal posterior probability",
    y = "Number of samples",
    title = "Animal posterior probability in Human Samples"
  ) +
  theme_classic()

png(filename = "population_posteriors.png", height = 600, width = 800)
plot_human
dev.off()
