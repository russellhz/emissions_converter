library(dplyr)
library(tidyr)
library(stringr)
library(readr)

### eppa_to_tibble: Master function to convert eppa input to one tibble ---------
eppa_to_tibble <- function(input){
  eppa <- read_lines(paste0('../input/',input))
  
  # Read in emissions categories -- start with spaces
  cat_loc <- which(str_detect(eppa, '^\\s+[\\w]+'))
  
  # Identify empty lines, which indicate start or end of data chunk
  empty_lines <- which(nchar(eppa) == 0)
  # Remove empty lines before the first category
  empty_lines <- empty_lines[empty_lines > cat_loc[1]]
  
  # Start lines don't have empty lines/cat_loc immediately following them
  start_lines <- empty_lines[!((empty_lines + 1) %in% c(empty_lines, cat_loc, length(eppa)))] + 1
  # End lines don't have empty lines/cat_loc immediately preceding them
  end_lines <- empty_lines[!((empty_lines - 1) %in% c(empty_lines, cat_loc, length(eppa)))] - 1
  # Identify lines that indicate start of new gas
  gas_lines <- which(nchar(eppa) >= 1 & nchar(eppa) <= 4)
  
  data_list <- list()
  for (i in 1:length(start_lines)){
    data_list[[i]] <- chunk_readr(eppa, start = i, starters = start_lines, enders = end_lines, 
                cats = cat_loc, gasses = gas_lines)
  }
  data <- bind_rows(data_list) %>%
    mutate(gas = if_else(is.na(gas), str_match(category, ".+(?= EMISSIONS)"), gas)) %>%
    mutate(value = as.numeric(value),
           year = as.integer(year))
  
  return(data)
}

### chunk_readr: Creates standardized chunk from eppa data ---------
chunk_readr <- function(data, start, starters = start_lines, enders = end_lines, 
                        cats = cat_loc, gasses = gas_lines){
  start_line <- starters[start]
  
  # end_line is either corresponding empty line or end of file
  if (start > length(enders)){
    end_line <- length(data)
  } else {
    end_line <- enders[start]
  }
  
  # Identify category
  cat_line <- cats[max(which(cats < start_line))]
  category <- str_trim(data[cat_line])
  
  # Separate out chunk
  chunk <- data[start_line:end_line]
  
  # identify length of chunk based on whether a gas line exists between it and next empty line
  if (any(gasses %in% seq(start_line, end_line))){
    data <- gas_chunk2tibble(chunk, category)
  } else {
    data <- chunk2tibble(chunk, category)
  }
  
}

### chunk2tibble: Converts standardized data chunk into tibble ---------
chunk2tibble <- function(chunk, category, gas = NULL){
  # Split chunk by tabs/spaces
  x <- lapply(chunk, str_split, pattern = '\\s+')
  # Take first row length
  row_length <- length(x[[1]][[1]])
  # Convert to matrix
  mat <- matrix(unlist(x), ncol = row_length, byrow = TRUE)
  # Identify variable (agr/n_ag) and replace with "year"
  var <- mat[1,1]
  mat[1,1] <- "year"
  # Set first row as column name and remove as row
  colnames(mat) <- mat[1,]
  mat <- mat[-1,]
  # Convert to tibble
  data <- as_tibble(mat) %>%
    gather(region, value, -year) %>%
    mutate(category = category, variable = var)
  
  if (!is.null(gas)){data <- mutate(data, gas_type = gas)}
  
  return(data)
}

### gas_chunk2tibble: Converts standardized data chunk into tibble ---------
gas_chunk2tibble <- function(chunk, category){
  # separate header row
  header <- chunk[1]
  
  # for each gas row, remove gas and create one chunk
  gasses <- which(nchar(chunk) >= 1 & nchar(chunk) <= 4)
  chunk_list <- list()
  for (i in 1:length(gasses)){
    gas <- chunk[gasses[i]]
    start <- gasses[i] + 1
    end <- ifelse(i < length(gasses), gasses[i+1]-1, length(chunk))
    x <- c(header, chunk[start:end])
    chunk_list[[gas]] <- x
  }
  
  data <- lapply(chunk_list, chunk2tibble, category = category)
  for (name in names(data)){
    data[[name]] <- mutate(data[[name]], gas = name)
  }
  data <- bind_rows(data)
  
  return(data)
}

