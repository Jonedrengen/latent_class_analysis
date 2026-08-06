# This is our config file for behaviour control

fitting_configuration <- list(
    # random seed for reproducibility (leave empty unless testing)
    default_seed = 67L,
    # The default number of chains to run in the BLCM model
    default_chains = 4L,
    # The default number of iterations to run in the BLCM model
    default_iterations = 2000L,
    # The default number of burn-in iterations to discard in the BLCM model
    default_burn_in = 1000L,
    # The default thinning interval for the BLCM model
    default_thin = 1L
)