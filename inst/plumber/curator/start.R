r <- plumber::plumb("plumber.R")
# port is defined in plumber.R
r$run(host = "0.0.0.0")
