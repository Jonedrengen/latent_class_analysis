blcm_sampling_options <- function(
  seed = 67L,
  chains = 4L,
  iterations = 2000L,
  burn_in = 1000L,
  thin = 1L
) {
  values <- list(
    seed = seed,
    chains = chains,
    iterations = iterations,
    burn_in = burn_in,
    thin = thin
  )

  structure(lapply(values, as.integer), class = "blcm_sampling_options")
}

fit_blcm_model <- function(blcm_data, sampling, model_file) {
  set.seed(sampling$seed)
  rjags::load.module("glm", quiet = TRUE)

  model <- rjags::jags.model(
    model_file,
    data = blcm_data$jags_data,
    n.chains = sampling$chains,
    n.adapt = 100,
    inits = function() {
      list(
        .RNG.name = "base::Wichmann-Hill",
        .RNG.seed = sample.int(.Machine$integer.max, 1)
      )
    }
  )

  stats::update(model, n.iter = sampling$burn_in)

  draws <- coda::as.mcmc.list(rjags::coda.samples(
    model,
    variable.names = "eta",
    n.iter = sampling$iterations - sampling$burn_in,
    thin = sampling$thin
  ))

  structure(
    list(
      draws = draws,
      sampling = sampling,
      class_columns = blcm_data$class_columns,
      prediction_rows = blcm_data$prediction_rows
    ),
    class = "blcm_fit"
  )
}
