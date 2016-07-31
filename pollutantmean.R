pollutantmean <- function (directory, pollutant, id=1:332) {
  ## 'directory' is a character vector of length 1 indicating the location of the CSV files.
  
  ## 'pollutant' is a character vector of length 1 indicating the name of the pollutant for which
  ## we will calculate the mean; either 'sulphate' or 'nitrate'
  
  ## 'id' is a integer vector indicating the monitor ID numbers to be used
  
  ## Return the mean of the pollutant across all monitors list in the 'id' vector 
  ## (ignoring NA values)
  ## Note : Do not round the result
  
  ## Merge all files data into one file
  fn_1 <- read.csv(paste('./',directory,'/',sprintf("%03s", 1),'.csv', sep="")) 
  fn_2 <- read.csv(paste('./',directory,'/',sprintf("%03s", 2),'.csv', sep="")) 
  f_tmp_data <- rbind.data.frame(fn_1,fn_2)
  for (i in 3:332) {
   t_f <- read.csv(paste('./',directory,'/',sprintf("%03s", i),'.csv', sep="")) 
   f_tmp_data <- rbind.data.frame(f_tmp_data,t_f) 
  }
  ## Subset of the given id
  ss_d <- subset.data.frame(f_tmp_data,f_tmp_data$ID %in% id)
  mn_val <- mean(ss_d[[pollutant]],na.rm=TRUE)
  mn_val
}