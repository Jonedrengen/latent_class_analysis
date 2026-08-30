# config file for behaviour control

#Do not modify unless you know what you are doing
mcmc_jags_configuration <- list(
    #(NULL for random)
    seed = NULL,
    chains = 4L,
    iterations = 2000L,
    burn_in = 1000L,
    thin = 1L,
    save_params = c("theta", "pi", "z")
)


data_column_configuration <- list(
    id_column = "Sample_Name",
    training_column = "training",
    class_columns_prefix = "^CLA_", # leave ^ at the beginning of the prefix
    feature_columns_prefix = "^FEA_"
)

plot_configuration <- list(
)