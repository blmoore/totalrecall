# load test packages
library("ggplot2")
library("dplyr")
library("reshape2")
library("gridExtra")

dump_sessionInfo <- function(filename="sessionInfo.txt"){
  message(paste0("Writing sessionInfo to ", filename))
  capture.output(sessionInfo(), file=filename)
}

parse_sessionInfo <- function(filename=""){
  # debugging
  filename="sessionInfo.txt"
  f <- paste(readLines(filename), collapse=" ")
  # build char vectors
  l_pkgs <- gsub("^.*?other attached packages:(.*?)loaded via.*?$", "\\1", f)
  l_pkgs <- unlist(strsplit(loaded, " "))
  l_pkgs <- strsplit(l_pkgs[!l_pkgs == ""][-1], "_")
  l_pkgs <- do.call(rbind, l_pkgs)
  colnames(l_pkgs) <- c("pkg", "vers")

  # compare with installed versions:
  ip <- installed.packages()
  # case 1: installed
  inst <- l_pkgs[,"pkg"] %in% ip[,"Package"]
  # named matrices a pain for extract, dfs?
  if( all(inst) ){
    # all pkgs loaded in sessionInfo are installed, now check vers
    i_vers <- ip[ip[,"Package"] %in% l_pkgs[,"pkg"],c("Package","Version")]
    l_pkgs <- l_pkgs[order(l_pkgs[,"pkg"], i_vers[,"Package"]),]
    stopifnot(nrow(l_pkgs) == nrow(i_vers))
    if( all(l_pkgs[,"vers"] == i_vers[,"Version"])){
      return(message("Currently installed packages match those in sessionInfo()"))
    }
  }

  # cran mirror:
  cran <- getOption("repos")

  # return
  list("loaded"=l_pkgs)
}

dump_sessionInfo()
parse_sessionInfo()

