
############# custom logging utilities ############

mcmc_object_summary <- function(mcmc_object) {

  #get special parameters
  pi_ <- grep("^pi\\[", coda::varnames(mcmc_object[[1]]))
  p_ <- grep("^p\\[", coda::varnames(mcmc_object[[1]]))
  eta_ <- grep("^eta\\[", coda::varnames(mcmc_object[[1]]))

  mcmc_summary <- cat(
    sprintf("class of mcmc_object: %s",     class(mcmc_object)),
    sprintf("amount of chains: %d",         coda::nchain(mcmc_object)),
    sprintf("iterations per chain: %d",     coda::niter(mcmc_object)),
    sprintf("number of paramters: %d",      coda::nvar(mcmc_object)),
    sprintf("pi parameters: %s",coda::varnames(mcmc_object[[1]])[pi_]),
    sprintf("p parameters: %s",coda::varnames(mcmc_object[[1]])[p_]),
    sprintf("eta parameters: %s",coda::varnames(mcmc_object[[1]])[eta_]),
    sep = "\n"
  )
  mcmc_summary
}


############# loggin functionality ############

#basic logging functions for BLCM
log_message <- function(message) {
  if (!is.null(message)) {
    message <- sprintf("[%s] MESSAGE: %s\n", Sys.time(), message)
  } else {
    message <- sprintf("[%s] MESSAGE: No message provided for logging.\n", Sys.time())
  }
  message
}

# capture data summary to log
log_data <- function(data, summary_function = str) {
  if (!is.null(data)) {
    data_summary <- capture.output(summary_function(data))
  } else {
    data_summary <- "No data found for logging."
  }
  data_summary
}

#formatting str() default
format_logs <- function(message, data, summary_function = str) {
  formatted_logs <- c(log_message(message),
                      log_data(data, summary_function),
                      strrep("-", 60))
  formatted_logs
}

#write formatted_logs to file
logs_to_file <- function(formatted_logs, file_path) {
  output_file <- cat(formatted_logs,
                     file = file_path,
                     sep = "\n",
                     append = TRUE)
  output_file
}

#logging func wrapper
logger <- function(message = "default message",
                   data = c("default:", "no", "data", "found"),
                   log_file_path = "default_log.txt",
                   summary_function = str) {
  formatted_logs <- format_logs(message,
                                data,
                                summary_function)
  logs_to_file(formatted_logs, log_file_path)

}
