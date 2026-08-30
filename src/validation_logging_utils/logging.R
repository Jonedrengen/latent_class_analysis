############# loggin func ############

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
log_data <- function(data) {
  if (!is.null(data)) {
    data_summary <- capture.output(str(data))
  } else {
    data_summary <- "No data found for logging."
  }
  data_summary
}

#formatting
format_logs <- function(message, data) {
  formatted_logs <- c(log_message(message),
                      log_data(data),
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
                   log_file_path = "default_log.txt") {
  formatted_logs <- format_logs(message, data)
  logs_to_file(formatted_logs, log_file_path)

}
