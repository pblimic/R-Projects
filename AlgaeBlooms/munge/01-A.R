# Example preprocessing script.

#remove those data with too many NAs
data(algae)
algae<-algae[-manyNAs(algae),]

#filling in NAs base on correlations
symnum(cor(algae[,4:18],use='complete.obs'))

lm(PO4 ~ oPO4, data=algae)

fillPO4 <- function(oP) {
  if(is.na(oP))
    return(NA)
  else return(42.897 + 1.293 * oP)
}

algae[is.na(algae$PO4), 'PO4'] <- sapply(algae[is.na(algae$PO4), 'oPO4'], fillPO4)

#visualization on data set
histogram(~mxPH | season, data = algae)
algae$season<-factor(algae$season, levels = c('spring', 'summer', 'autumn', 'winter'))
histogram(~mxPH | size * speed, data = algae)
stripplot(size ~ mxPH | speed, data = algae, jitter = T)

#filling in NAs base on similarties between cases
algae <- knnImputation(algae, k=10, meth = 'median')
