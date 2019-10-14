library(tesseract)
library(tidyverse)


# read in tif docs
doc <- list.files(path = '.', pattern = ".tif$", full.names = TRUE)

# create df for results
outdf <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(outdf) <- c("tif", "site", "AMG_easting", "AMG_northing", "pfc", "veg_group", 
                     "veg_condition")


# Helper Functions --------------------------------------------------------


# finds site number in list - l is type 1 list
site_finder <- function(l){
  res <- lapply(l, function(ch) grep("Site|Sife", ch))
  ind <- sapply(res, function(x) length(x) > 0)
  
  if(sum(grepl("Siteno|Sifeno", unlist(l[ind]))) > 0){
    site <- grep("Siteno|Sifeno", unlist(l[ind]))
    siten <- ifelse(identical(unlist(l[ind])[site + 1], ""),
                   unlist(l[ind])[site + 2],
                   unlist(l[ind])[site + 1])
  } else {
    site <- grep("Site", unlist(l[ind]))
    siten <- unlist(l[ind])[site[1] + 2]
  }
  
  return(siten)
}


# finds amg eastings and northings - l is type 1 list
coord_finder <- function(l){
  res <- lapply(l, function(ch) grep("AMG", ch))
  ind <- sapply(res, function(x) length(x) > 0)
  amg <-  grep("AMG", unlist(l[ind]))
  amgline <- gsub("_", "", unlist(l[ind]))
  if(sum(ind) == 0){
    coords <- c("999", "999")
  } else {
    if(is.na(as.numeric(str_sub(amgline[amg + 1], start = 1, end = 1)))){
      coords <- amgline[c(amg + 2, amg + 3)]
    } else {
      coords <- amgline[c(amg + 1, amg + 2)]
    }
  }
  return(coords)
}

# finds pfc returns NA if blank - l is type 1 list
pfc_finder <- function(l){
  res <- lapply(l, function(ch) grep("Projected", ch))
  ind <- sapply(res, function(x) length(x) > 0)
  proj <- grep("Projected", unlist(l[ind]))
  pfc <- ifelse(length(unlist(l[ind])) == proj + 2, NA, 
                unlist(l[ind])[proj + 3])
  return(pfc)
}

# finds veg group - l is type 2 list
veg_group_finder <- function(l){
  res <- lapply(l, function(ch) grep("group", ch))
  ind <- sapply(res, function(x) length(x) > 0)
  if(sum(ind) == 0){
    v_gp <- "999"
  } else {
    gp <- grep("group", unlist(l[ind]))
    proj_loc <- grep("Projected", unlist(str_split(unlist(l[ind])[gp + 1], " ")))
    v_gp <-  paste(unlist(str_split(unlist(l[ind])[gp + 1], " "))[1:(proj_loc - 1)],
                   collapse = ' ')
  }
  return(v_gp)
}

#l=t2
# finds veg cond - l is type 2 list
veg_cond_finder <- function(l){
  res <- lapply(l, function(ch) grep(" rating", ch))
  ind <- sapply(res, function(x) length(x) > 0)
  if(sum(ind) == 0){
    v_cond <- "999"
  } else {
    cp <- grep(" rating", unlist(l[ind]))
    v_cond <- unlist(l[ind])[cp + 1]
  }
  return(v_cond)
}



# Read Find Extract -------------------------------------------------------


for(i in seq_along(doc)){
  t <- tesseract::ocr(doc[i])
  
  # make without returns and spaces
  t1 <- t %>%
    stringr::str_split(pattern = "\n") %>%
    unlist() %>%
    stringr::str_split(pattern = " ")
  
  # make without returns and colons
  t2 <- t %>%
    stringr::str_split(pattern = "\n") %>%
    unlist() %>%
    stringr::str_split(pattern = ":")
  
  # site
  outdf[i, 2] <- site_finder(t1)
  
  # coords
  coords <- coord_finder(t1) 
  outdf[i, 3] <- coords[1]
  outdf[i, 4] <- coords[2]
  
  #pfc
  outdf[i, 5] <- pfc_finder(t1)
  
  # veg group
  outdf[i, 6] <- veg_group_finder(t2)
  
  # veg cond
  outdf[i, 7] <- veg_cond_finder(t2)
  
  #tif number
  outdf[i, 1] <- doc[i]
  
  cat(paste0(doc[i], "\n"))
}


# Final Clean Export ------------------------------------------------------

outdf2 <- outdf %>%
  dplyr::mutate(AMG_easting = gsub("E,", "", AMG_easting)) %>%
  dplyr::mutate(AMG_northing = gsub("_", "", AMG_northing)) %>%
  dplyr::mutate(AMG_northing = gsub("N", "", AMG_northing)) %>%
  dplyr::mutate(veg_condition = gsub("Carrying capacity", "", veg_condition)) %>%
  dplyr::mutate(veg_condition = gsub("\\| ", "", veg_condition)) %>%
  dplyr::mutate(veg_condition = gsub(";", "", veg_condition))


write_csv(outdf2, paste0(p, "/Marrilla_station_site_info.csv"))

