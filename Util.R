
# if one data.frame is shorter than the other (less rows) then this function
# fills up cols with NAs and cbinds them together. 
cbindPad <- function(data1, data2){
      na.df <- matrix(NA, ncol=ncol(data1), 
                      nrow=abs(dim(data1)[1] - dim(data2)[1]))
      na.df <- data.frame(na.df)
      
      if(dim(data1)[1] < dim(data2)[1]){
            colnames(na.df) <- colnames(data1)
            data1 <- rbind(data1, na.df)
      } else {
            colnames(na.df) <- colnames(data2)
            data2 <- rbind(data2, na.df)
      }
      return(cbind(data1, data2))
}