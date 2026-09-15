# Test setup: configure logging for test environment
# This file runs before all tests in the testthat directory
box::use(
  logger,
)

# Disable file logging during tests to avoid "cannot open connection" errors
# when the logs directory doesn't exist in the test working directory
logger$log_appender(logger$appender_stderr)
