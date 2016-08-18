library(ggplot2)
load("C:/Users/colander1/Downloads/roster.Rda")


load("C:/Users/colander1/Downloads/pbp20152016.Rda")

shots.all <- pbp[which(pbp$Event %in% c("SHOT","GOAL","MISS","BLOCKED")), ]

##remove ENG
shots.all$ENG <- ifelse(shots.all$Event =="GOAL" & shots.all$ev.team == shots.all$Away.Team & is.na(shots.all$Home.Goalie), 
                        1,
                          ifelse(shots.all$Event =="GOAL" & shots.all$ev.team == shots.all$Home.Team & is.na(shots.all$Away.Goalie),
                          1,0))
shots.all <- shots.all[which(shots.all$ENG == 0), ]

##remove SO
shots.all <- shots.all[which(shots.all$Period %in% c(1:4)), ]


##flag goals
shots.all$goal <- as.factor(ifelse(shots.all$Event =="GOAL",1,0))

##dummies
shots.all$is.Rebound <- as.factor(shots.all$is.Rebound)
shots.all$is.Rush <- as.factor(shots.all$is.Rush)
shots.all$Detail <- as.factor(shots.all$Detail)
shots.all$Strength <- ifelse(shots.all$Strength.State %in% c("5v5","EvE"), "5v5",
                          ifelse(shots.all$Strength.State=="5v4", "5v4",   
                              ifelse(shots.all$Strength.State=="4v4", "4v4",  
                                  ifelse(shots.all$Strength.State %in% c("4v5"),"4v5",  
                                      ifelse(shots.all$Strength.State=="5v3", "5v3",  
                                          ifelse(shots.all$Strength.State=="3v3", "3v3",  
                                              ifelse(shots.all$Strength.State=="4v5", "4v5","")))))))


###shooter performance
shooter.shots <- aggregate(goal ~ p1, FUN=length, data=shots.all) 
shooter.goals <- aggregate(goal ~ p1, FUN=length, data=shots.all[which(shots.all$Event %in% c("GOAL")),]) 





goals.only <- shots.all[which(shots.all$Event %in% c("GOAL")),
                        c("ENG","Game.ID","Period","p1","Event","ev.team","ev.zone","Detail","Seconds","a6.num","a6.pos","h6.num","h6.pos",
                          "Angle","XC","YC","Distance","xG","is.Rebound","is.Rush",
                          "Away.Team","Home.Team","Away.Goalie","Home.Goalie","Score.State","Strength.State","Season","Season.Type")]

##########################################
####Linear Model Cross-Validation
##########################################

sog.only <- shots.all[which(shots.all$Event %in% c("SHOT","GOAL")), ]

input <- shots.all
input <- input[names(input) %in% c("goal",model.vars)]
input <- input[complete.cases(input),]

require(glmnet)
set.seed(seed)

classes <- input[, "goal"]

train_set <- createDataPartition(classes, p = 0.8, list = FALSE)

cs_data_train <- input[train_set, ]
cs_data_test <- input[-train_set, ]

glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                           lambda = seq(.01, .2, length = 20))
glmnet_ctrl <- trainControl(method = "cv", number = 10)
model.output <- train(goal ~ ., data = cs_data_train,
                      method = "glmnet",
                      preProcess = c("center", "scale"),
                      family="binomial",
                      tuneGrid = glmnet_grid,
                      trControl = glmnet_ctrl)

model.output

trellis.par.set(caretTheme())
plot(model.output, scales = list(x = list(log = 2)))

pred_classes <- predict(model.output, newdata = cs_data_test)
table(pred_classes)
## pred_classes
##  bad good 
##  172  716
pred_probs <- predict(model.output, newdata = cs_data_test, type = "prob")
head(pred_probs)





lm.cross.v10 <- function(input, model.vars) {
  
  library(caret)
  library(Rcmdr)
  library(glmnet)
  # load the dataset
  input
  input <- input[names(input) %in% c("goal",model.vars)]
  input <- input[complete.cases(input),]
  

  
  
  # define training control
  set.seed(1234)
  train_control <- trainControl(method="cv", number=10)
  
  # train the model
  model.output <- train( goal ~ . , data=input, trControl=train_control,  method = "glm", family="binomial", tuneLength=10)
  
  # predict
  #xGoal.raw <- predict(model.output, newdata=input, type="raw")
  xGoal.prob <- predict(model.output, newdata=input, type="prob")
  colnames(xGoal.prob) <- c("Prob.Save","Prob.Goal")
  
  # var importance
  var.imp <- varImp(model.output)
  
  return(list(cbind(xGoal.prob,input),model.output,var.imp))
  
}

model.vars <- c("Angle","Distance","is.Rebound","is.Rush","Detail")

goal.model <- lm.cross.v10(sog.only, model.vars)

shots.scored <- goal.model[[1]]
goal.model[[2]]

################
##ANOVA
################

fm = goal.model[[2]]$finalModel
anova(fm, test="Chisq")

##############################
##create roc curve
##############################

######cut off rates
cutoff.roc.scores <- function(cut, prob, y) {

    yhat <- ifelse(prob > cut,1,0)
    sensitivity <- length(which(yhat == 1 & y == 1)) / length(which(y == 1)) ##total true positives / total positives
    specificity <- length(which(yhat == 0 & y == 0)) / length(which(y == 0)) ##total true negatives / total negatives
    class.rate <- length(which(yhat == y)) / length(y)                      ## total true / total obs
    distance <- sqrt( (sensitivity - 1)^2 + (specificity - 1)^2 )
    out = t(as.matrix(c(cut,sensitivity, specificity, class.rate,distance)))
    colnames(out) = c("cutoff","sensitivity", "specificity", "c.rate", "distance")
    return(out)
}

###sample
cutoff.roc.scores(.1,shots.scored$Prob.Goal,shots.scored$goal)


###create accuracy matrix
s = seq(.01,.99,length=1000)
accuracy.mat = matrix(0,1000,5)
    for(i in 1:1000) {
      accuracy.mat[i,]=cutoff.roc.scores(s[i],shots.scored$Prob.Goal,shots.scored$goal)
    }
accuracy.df <- as.data.frame(accuracy.mat)
colnames(accuracy.df) = c("cutoff","sensitivity", "specificity", "class.rate", "distance")
accuracy.df$false.pos.rate <- 1 - accuracy.df$specificity

##############
###ROC Curve
##############

ggplot(data=accuracy.df, aes(x=false.pos.rate,y=sensitivity)) +
    geom_line() 


height = (accuracy.df$sensitivity[-1]+accuracy.df$sensitivity[-length(accuracy.df$sensitivity)])/2
width = -diff(accuracy.df$false.pos.rate) # = diff(rev(omspec))
sum(height*width)
##0.7219591


ggplot(data=accuracy.df, aes(x=cutoff)) +
  geom_line(aes(y=distance,color="blue")) +
  geom_line(aes(y=specificity)) +
  geom_line(aes(y=sensitivity)) +
  geom_line(aes(y=class.rate)) 


