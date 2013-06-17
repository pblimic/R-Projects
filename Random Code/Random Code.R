#getting random number
n<-1
charac<-as.matrix(1:10000, 10000, 1)
number<-as.matrix(1:2000, 2000, 1)
for(n in 1:2000){
  number[n, 1]<-as.matrix(sample(1:9, 1)*10000+sample(1:9, 1)*1000+sample(1:9, 1)*100+sample(1:9, 1)*10+sample(1:9, 1))
  n<-n+1
}

#getting code directory with charactor and number
n<-1
Code<-as.matrix(1:10000, 10000, 1)
for(n in 1:10000){
MHmakeRandomString <- function(n=1, lenght=12)
{
  randomString <- c(1:n)                  # initialize vector
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                             lenght, replace=TRUE),
                             collapse="")
  }
  return(randomString)
}
Code[n,1]<-MHmakeRandomString()
n<-n+1
}

dimnames(Code) = list(c(1:10000), c('Code'))

write.csv(Code, 'C:\\Users\\Limic\\Documents\\GitHub\\R-Projects\\Random Code\\Code.csv')