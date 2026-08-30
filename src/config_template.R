# config file for behaviour control


config <- list(
    mcmc_jags_configuration = list(
    #(NULL for random)
    seed = 1,
    chains = 8L,
    iterations = 2000L,
    burn_in = 1000L,
    thin = 1L,
    save_params = c("pi","p","eta")
    ),

    data_column_configuration = list(
    id_column = "Sample_Name",
    training_column = "training",
    class_columns_prefix = "^CLA_", # leave ^ at the beginning of the prefix
    feature_columns_prefix = "^FEA_"
    ),

    plot_configuration = list()
)