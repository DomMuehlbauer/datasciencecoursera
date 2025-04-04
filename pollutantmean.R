pollutantmean <- function(directory, pollutant, id = 1:332){
        pollutant_vec<- numeric()
        for(i in id) {
                if(i<10){
                        data_i <- read.csv(paste0(directory, "/00", i, ".csv"))
                }
                else if(i>=10 && i<100){
                        data_i <- read.csv(paste0(directory, "/0", i, ".csv"))
                }
                else {
                        data_i <- read.csv(paste0(directory, "/", i, ".csv"))
                }
                data_no_na <- na.omit(data_i)
                pollutant_vec <- c(pollutant_vec, data_no_na[,pollutant])
        }
        return(mean(pollutant_vec))
} 