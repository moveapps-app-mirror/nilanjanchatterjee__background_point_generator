library(here)
library(testthat)
setwd(here("tests/testthat"))
source(file.path("..", "..", "src", "common", "logger.R"))
source(file.path("..", "..", "src", "common", "runtime_configuration.R"))
source(file.path("..", "..", "src", "io", "app_files.R"))
source(file.path("..", "..", "src", "io", "io_handler.R"))

clearRecentOutput()
# the system under test (sut)
source(file.path("..", "..", "./RFunction.R"))

setwd(here())

