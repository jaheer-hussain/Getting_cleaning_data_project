complete <- function (directory, id=1:332) {
  ## 'directory' is a character vector of length 1 indicating the location of the CSV files.
  
  ## 'id' is a integer vector indicating the monitor ID numbers to be used
  
  ## Return a data form for number of completely observed cases in each data file

  ## Define the empty vector to hold complete cases count
  r_ve <- vector()
  for (i in 1:length(id)) {
    c_f <- read.csv(paste("./",directory,"/",sprintf("%03s", id[i]),".csv",sep=""))
    r_ve <- c(r_ve,sum(complete.cases(c_f)))
  }
  r_df <- data.frame(id,r_ve)
  colnames(r_df) <- c("id","nobs")
  return(r_df)
}
