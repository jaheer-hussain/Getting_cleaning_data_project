  pollutantmean <- function (directory, pollutant, id=1:332) {
    ## 'directory' is a character vector of length 1 indicating the location of the CSV files.
    
    ## 'pollutant' is a character vector of length 1 indicating the name of the pollutant for which
    ## we will calculate the mean; either 'sulphate' or 'nitrate'
    
    ## 'id' is a integer vector indicating the monitor ID numbers to be used
    
    ## Return the mean of the pollutant across all monitors list in the 'id' vector 
    ## (ignoring NA values)
    ## Note : Do not round the result
    
    ## Define empty data frame 
    f_tmp_data <- data.frame(Date=factor(),
                             sulfate=double(),
                             nitrate=double(),
                             ID=integer()
                             )
    ## Merge all files data into one data frame
    for (i in 1:length(id)) {
      t_f <- read.csv(paste('./',directory,'/',sprintf("%03s", id[i]),'.csv', sep="")) 
      f_tmp_data <- rbind.data.frame(f_tmp_data,t_f) 
    }
    ## Subset of the given id
    ss_d <- subset.data.frame(f_tmp_data,f_tmp_data$ID %in% id)
    mn_val <- mean(ss_d[[pollutant]],na.rm=TRUE)
    mn_val
  }