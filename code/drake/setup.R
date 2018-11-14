library(rio)
library(yaml)
library(dplyr)
library(drake)
library(readxl)
library(survey)
library(data.table)

options(encoding = "utf8")
pkgconfig::set_config("drake::strings_in_dots" = "literals")

datadir <- read_yaml("config.yaml")[[paste(Sys.info(),collapse="|")]]

# ENDES subdir
if(length(datadir) == 1 & any(dir.exists(datadir))){
  ineidir <- file.path(datadir, "inei")
  minedudir <- file.path(datadir, "minedu")
} else {
  stop("data dir not found")
}

# Apply labels to categorical vars imported
# from other data formats
putlabel <- function(x) {
  if (is.null(attr(x, "labels"))) {
    stop("No 'labels' attribute")
  } else {
    return(factor(
      x,
      levels = attr(x, "labels"),
      labels = names(attr(x, "labels"))
    ))
  }
}

# Transform svy mean and proportion CI estimate objects
# into a simple 3-vector
svy2pci <- function(x) {
  xclass <- attr(x, "class")
  if (xclass == "svyciprop") {
    xci <- attr(x, "ci")
    c(as.vector(x), xci[1], xci[2])
  } else if (xclass == "svystat") {
    xsd <- sqrt(attr(x, "var"))
    z <- qnorm(0.975)
    as.vector(x) + c(0, -z*xsd, z*xsd)
  } else {
    stop("only works on svyciprop or svystat objects")
  }
}

# Fixing issue with svyciprop
body(svyciprop)[[6]] <- substitute(names(rval) <- paste(deparse(formula[[2]]),collapse=""))

# Summing omitted questions
`%+rmna%` <- function(x, y) {
  x <- ifelse(is.na(x),0,x)
  y <- ifelse(is.na(y),0,y)
  x+y
}