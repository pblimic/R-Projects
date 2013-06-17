library('ProjectTemplate')
load.project()

#for (dataset in project.info$data)
#{
#  message(paste('Showing top 5 rows of', dataset))
#  print(head(get(dataset)))
#}

#summarize the clean data
summary(clean.algae)

#Prediction Models

#Linear Regression - Multiple

lm.a1 <- lm(a1 ~., data = clean.algae[, 1:12])

summary <- summary(lm.a1)

capture.output(summary, file='C:\\Users\\Limic\\Documents\\GitHub\\R-Projects\\AlgaeBlooms\\doc\\summary.txt')

#backward elimination

#identifying a sequential analysis of variance of the model fit
anova <- anova(lm.a1)

capture.output(anova, file='C:\\Users\\Limic\\Documents\\GitHub\\R-Projects\\AlgaeBlooms\\doc\\anova.txt')

#make the change to existed model -> lm.a1
lm2.a1 <- update(lm.a1, . ~ . -season)

summary2 <- summary(lm2.a1)

capture.output(summary, file='C:\\Users\\Limic\\Documents\\GitHub\\R-Projects\\AlgaeBlooms\\doc\\summary2.txt')

#formal comparison between the two models
anova2 <- anova(lm.a1, lm2.a1)

capture.output(anova2, file='C:\\Users\\Limic\\Documents\\GitHub\\R-Projects\\AlgaeBlooms\\doc\\anova2.txt')

#backward elimination to perform all process

final.lm <- step(lm.a1)

final.summary <- summary(final.lm)

capture.output(final.summary, file='C:\\Users\\Limic\\Documents\\GitHub\\R-Projects\\AlgaeBlooms\\doc\\final.summary.txt')

#Regression Trees

#using the data with NAs
rt.a1 <- rpart(a1 ~ ., data = algae[,1:12])

rt.a1

prettyTree(rt.a1)

rt.summary <- summary(rt.a1)

capture.output(rt.summary, file='C:\\Users\\Limic\\Documents\\GitHub\\R-Projects\\AlgaeBlooms\\doc\\rt.summary.txt')

printcp(rt.a1)

rt2.a1 <- prune(rt.a1, cp = 0.08)

rt2.a1

first.tree <- rpart(a1~ ., data = algae[, 1:12])

#pruning the tree on the specific nodes
snip.rpart(first.tree, c(4, 7))

#pruning the tree in a graphical way
prettyTree(first.tree)

snip.rpart(first.tree)

#collecting the predictions of models
lm.prediction.a1 <- predict(final.lm, clean.algae)

rt.prediction.a1 <- predict(rt.a1, algae)

#calculating the mean absolute error
(mae.a1.lm <- mean(abs(lm.prediction.a1 - algae[, 'a1'])))

(mae.a1.rt <- mean(abs(rt.prediction.a1 - algae[, 'a1'])))

#calculating the mean squared error (MSE)
(mse.a1.lm <- mean((lm.prediction.a1 - algae[, 'a1'])^2))

(mse.a1.rt <- mean((rt.prediction.a1 - algae[, 'a1'])^2))


#comparison between models
cv.rpart <- function(form, train, test, ...){
  m<-rpartXse(form, train, ...)
  p<-predict(m,test)
  mse<-mean((p-resp(form, test))^2)
  c(nmse=mse/mean((mean(resp(form, train))-resp(form, test))^2))
}

cv.lm <- function(form, train, test, ...){
  m<-lm(form, train, ...)
  p<-predict(m, test)
  p<-ifelse(p<0,0,p)
  mse<-mean((p-resp(form, test))^2)
  c(nmse=mse/mean((mean(resp(form, train))-resp(form, test))^2))
}

res <- experimentalComparison{
  c(dataset(a1~., clean.algae[, 1:12], 'a1'),
  c(variants('cv.lm'),
  variants('cv.rpart', se=c(0,0.5,1))),
  cvSettings(3,10,1234))

#getting summary of result
summary(res)
  
#plot
plot(res)
  
getVariant('cv.rpart.v1', res)
  
  
#comparative experiment for all seven prediction tasks at the same time
DSs<-sapply(names(clean.algae)[12:18],
            function(x,names.attrs){
              f<-as.formula(paste(x,'~.'))
              dataset(f,clean.algae[,c(names.attrs,x)],x)
            },
            names(clean.algae[1:11])
)

res.all<-experimentalComparison(
  DSs,
  c(variants('cv.lm'),
    variants('cv.rpart',se=c(0,0.5,1))
    ),
  cvSettings(4,10,1234))
  
#randomForest
library(randomForest)
cv.rf<-function(form,train,test,...){
  m<-randomForest(form,train, ...)
  p<-predict(m,test)
  mse<-mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}
  

res.all<-experimentalComparison(
  DSs,
  c(variants('cv.lm'),
    variants('cv.rpart',se=c(0,0.5,1)),
    variants('cv.rf',ntree=c(200,500,700))
    ),
  cvSettings(5,10,1234)
  )

#getting the best ones
bestScores(res.all)
  
compAnalysis(res.all,against='cv.rf.v3',
               datasets=c('a1','a2','a4','a6'))
  
  
  
  
  
  
  
  
  
  
  