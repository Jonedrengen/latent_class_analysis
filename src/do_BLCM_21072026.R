# This is the main script for running and generating reports for the BLCM model.

# author: Jon Sztuk Slotved
# maintainer: Jon Sztuk Slotved 
# model design: Daniel Park

##############################################################
############ setup, sourcing and libraries ###################


# get "--file" default argument from commandArgs, replace it with "" so the path is not --file=path/to/script.R. Then normalize. 
blcm_script_dir <- function() {
  arguments <- commandArgs(trailingOnly = FALSE)
  file_arg <- arguments[grepl("^--file=", arguments)] #returns --file=path/to/script.R
  raw_path <- sub("^--file=", "", file_arg)
  dirname(normalizePath(raw_path))
}

# do not touch! this is src dir
.blcm_dir <- blcm_script_dir()

#sourcing other scripts
source(file.path(.blcm_dir, "config_template.R"))
source(file.path(.blcm_dir, "validation_logging_utils", "utils.R"))
source(file.path(.blcm_dir, "validation_logging_utils", "logging.R"))
source(file.path(.blcm_dir, "prepare_blcm_input_data.R"))
source(file.path(.blcm_dir, "validation_logging_utils", "Input_validator.R"))
source(file.path(.blcm_dir, "report_scripts", "blcm_summary.R"))
source(file.path(.blcm_dir, "report_scripts", "blcm_report.R"))
source(file.path(.blcm_dir, "plots", "generate_histogram.R"))
source(file.path(.blcm_dir, "plots", "blcm_report_plots.R"))


##############################################
############  functions #####################

run_jags_model <- function(prepared_blcm_data,
                           jags_mcmc_configuration,
                           model_file) {
                            
  set.seed(jags_mcmc_configuration$seed)

    jags_output <- R2jags::jags(
        model.file = model_file,
        data = prepared_blcm_data$jags_data,
        inits = prepared_blcm_data$inits, #later pass ini
        parameters.to.save = jags_mcmc_configuration$save_params,
        n.chains = jags_mcmc_configuration$chains,
        n.iter = jags_mcmc_configuration$iterations,
        n.burnin = jags_mcmc_configuration$burn_in,
        n.thin = jags_mcmc_configuration$thin,
        DIC = FALSE
      )
    jags_output
}
convert_to_mcmc <- function(jags_model_out) {
  mcmc_object <- coda::as.mcmc(jags_model_out)
  mcmc_object
}

apply_stephens <- function(jags_model_mcmc_object) {
}

#input parser for command line arguments
blcm_input_parser <- function(argv = commandArgs(trailingOnly = TRUE)) {
  options <- list(
    optparse::make_option(
      c("-i", "--input"),
      type = "character",
      required = TRUE
    ),
    optparse::make_option(
      c("-o", "--output"),
      type = "character",
      required = TRUE
    )
  )
  parsed <- optparse::parse_args(
    optparse::OptionParser(option_list = options),
    args = argv
  )
  parsed
}

# big orchestration function for running the BLCM model
run_blcm_main <- function(input_options,
                          data,
                          config) {
  output_dir <- input_options$output
  logger("input data read",
         data = data, 
         log_file_path = file.path(output_dir, "blcm_log.txt"))

  #TODO: should split options data validation and config into separate funcs
  valdiated_input <- validate_input(input_options, data, config)
  logger("input data validated", 
         data = valdiated_input,
         log_file_path = file.path(output_dir, "blcm_log.txt"))

  #preparing input data for BLCM
  prepared_blcm_data <- prepare_blcm_data(data, config = config)
  logger("blcm input data prepared",
         data = prepared_blcm_data,
         log_file_path = file.path(output_dir, "blcm_log.txt"))

  #run jags model
  jags_model_out <- run_jags_model(prepared_blcm_data,
                               config$mcmc_jags_configuration, 
                               model_file = file.path(.blcm_dir, "models", "model_blcm.bug"))
  logger("jags model run completed",
         data = jags_model_out,
         log_file_path = file.path(output_dir, "blcm_log.txt"))

  #convert jags model output to mcmc object
  jags_model_mcmc_object <- convert_to_mcmc(jags_model_out)
  logger("jags model results converted to mcmc object",
         data = jags_model_mcmc_object,
         log_file_path = file.path(output_dir, "blcm_log.txt"),
         summary_function = mcmc_object_summary)

}

##############################################
############ parse input ###################

input_options <- blcm_input_parser()


##############################################
############ main function ###################

#read data (move this to a func, when more input formats are supported)
data <- read.csv(
  input_options$input,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
run_blcm_main(input_options, data, config)
