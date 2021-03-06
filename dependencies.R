
# check that package dependencies are installed.
# if any dependencies are missing then attempt to install them.
install_dependencies <- function(deps=c(), repos=c("https://cran.revolutionanalytics.com")) {
  missing <- deps[!(deps %in% installed.packages()[,"Package"])]
  if (length(missing) > 0) {
    sapply(missing, function(m) {
      print(paste("Installing missing package", m))
      install.packages(m, repos=repos)
    })
  }
}

# Load supplied dependency list.
require_dependencies <- function(deps=c()) {
  sapply(deps, function(p) require(p, character.only=TRUE))
}

deps <- c("devtools",
          "MASS", 
          "MVN", 
          "caret",
          "scatterD3", 
          "crosstalk",
          "stringr",
          "shiny",
          "threejs",
          "ks",
          "lfda")

install_dependencies (deps)

##devtools::install_github("bwlewis/rthreejs")

