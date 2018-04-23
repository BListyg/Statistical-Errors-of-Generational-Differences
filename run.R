source_https <- function(u, unlink.tmp.certs = FALSE) {
  # load package
  require(RCurl)
  
  # read script lines from website using a security certificate
  if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
  script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
  if(unlink.tmp.certs) unlink("cacert.pem")
  
  # parase lines and evealuate in the global environement
  eval(parse(text = script), envir= .GlobalEnv)
}

source_https("https://raw.githubusercontent.com/BListyg/Statistical-Errors-of-Generational-Differences/master/normal_age_dist.R")

source_https("https://raw.githubusercontent.com/BListyg/Statistical-Errors-of-Generational-Differences/master/skewed_older.R")

source_https("https://raw.githubusercontent.com/BListyg/Statistical-Errors-of-Generational-Differences/master/skewed_younger.R")
