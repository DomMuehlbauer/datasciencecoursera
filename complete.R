complete <- function(directory, id = 1:332){
        cc_df<- data.frame(matrix(nrow=0, ncol=2))
        colnames(cc_df) <- c("id", "nobs")
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
                cc <- sum(complete.cases(data_no_na))
                cc_df <- rbind(cc_df, data.frame(id=i, nobs=cc))
        }
        return(cc_df)
} 