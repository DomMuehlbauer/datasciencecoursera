corr <- function(directory, threshold=0){
        cc_df <- complete(directory)
        id <- integer()
        correlation <- numeric()
        for(i in 1:nrow(cc_df)){
                relevant <- cc_df[i,]
                if(relevant[,2] > threshold){
                        id <- c(id, i)
                }
        }
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
                sulfate <- data_no_na[,"sulfate"]
                nitrate <- data_no_na[,"nitrate"]
                correlation <- c(correlation, cor(sulfate, nitrate))
        }
        return(correlation)
} 