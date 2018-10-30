library(rio)
library(dplyr)
library(survey)
library(data.table)

# Load the appropriate folder paths depending
# on where code is being run from
data.table(places = c("Linux|bmfazio-pc|x86_64|bmfazio",
                      "Windows|DESKTOP-5CGB5S0|x86-64|Personal"),
           path = c("/home/bmfazio/Documents/datasets",
                    "D:/datasets"))[
  places == paste(Sys.info()[c(1,4:6)], collapse = "|"), path] -> datadir

  # ENDES subdir
if(length(datadir) == 1 & any(dir.exists(datadir))){
  ineidir <- paste0(datadir, "/inei/")
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
  if (is.null(xclass)) {
    stop("WRONG!")
  } else if (xclass == "svyciprop") {
    xci <- attr(x, "ci")
    c(as.vector(x), xci[1], xci[2])
  } else if (xclass == "svystat") {
    xsd <- sqrt(attr(x, "var"))
    z <- qnorm(0.975)
    as.vector(x) + c(0, -z*xsd, z*xsd)
  } else {
    stop("o_O")
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