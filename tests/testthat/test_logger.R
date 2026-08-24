

#testthat for logger


#test a file is generated
test_that("logger generates a log file", {
    #set up vars for test
    test_message <- "Test message"
    temp_dir <- tempdir()
    log_file_name <- "test_log.txt"
    test_data <- mtcars

    #run logger
    logger(test_message,
           data = test_data,
           log_file_path = file.path(temp_dir, log_file_name))

    #check if log file exists
    expect_true(file.exists(file.path(temp_dir, log_file_name)))

    #clean up
    unlink(file.path(temp_dir, log_file_name))
})

#test that the log file contains the expected message and data summary
test_that("logger writes expected content to log file", {
    # set up vars for test
    tmp_dir <- tempdir()
    test_message <- "This is a test log message."
    log_file_name <- "test_log.txt"
    test_data <- mtcars

    #run logger
    logger(test_message,
           data = test_data,
           log_file_path = file.path(tmp_dir, log_file_name))

    #get log content
    log_content <- readLines(file.path(tmp_dir, log_file_name))

    #checks
    expect_true(any(grepl(test_message, log_content)))
    expect_true(any(grepl("mpg", log_content))) # Check column name from mtcars

    #clean up
    unlink(file.path(tmp_dir, log_file_name))
})