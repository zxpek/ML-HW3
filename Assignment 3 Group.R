
### 1
df<-read.csv("Loans_processed.csv")

set.seed(2017)
sampling <- sample(nrow(df))
df <- df[sampling,]
train <- df[1:20000,]
val <- df[20001:28000,]
test <- df[28001:nrow(df),]

train.x <- train[,1:7]
train.y <- train[,8]
sum(train.y == "Charged Off")/length(train.y)

val.x <- val[,1:7]
val.y <- val[,8]

test.x <- test[,1:7]
test.y <- test[,8]

### 2 (No rules, just majority split)
require(C50)

loanTree <- C5.0(x = train.x, y = train.y, size = 20)
loanTree
summary(loanTree)

###
LoanTreeCost <- function (train.x, train.y, val.x, val.y, stepwise=0.1, Max=5) {
  s <- c()
  p <- c()
  for (i in seq(1,Max,stepwise)) {
    loanTree <- C5.0(x = train.x, y = train.y, costs = matrix(c(1,i,1,1),2,2))
    pred <- predict(loanTree,val.x)
    n11 <- sum(pred == "Charged Off" & val.y == "Charged Off")
    n12 <- sum(pred == "Fully Paid" & val.y == "Charged Off")
    sensitivity <- n11/(n11+n12)
    s <- append(s,sensitivity)
    
    n21 <- sum(pred == "Charged Off" & val.y == "Fully Paid")
    precision <- n11/(n11+n21)
    p <- append(p,precision)
  }
  return(list(s,p))
}

params <- LoanTreeCost(train.x,train.y,val.x,val.y,Max=10)
sensitivity <- params[[1]]
precision <- params[[2]]

which.min(abs(sensitivity-0.25)) #21, so 1+0.1*(21-1) = 3.0
sensitivity[21] #25.24
  
which.min(abs(sensitivity-0.4)) #29, so 1+0.1*(29-1) = 3.8
sensitivity[29] #40.01%
precision[29] #24.20%

which.min(abs(sensitivity-0.5)) #33, so 1+0.1*(33-1) = 4.2
sensitivity[33] #49.34%
precision[33] #23.20%


loanTree25 <- C5.0(x = train.x, y = train.y, costs = matrix(c(1,3,1,1),2,2))
summary(loanTree25)
loanTree40 <- C5.0(x = train.x, y = train.y, costs = matrix(c(1,3.8,1,1),2,2))
summary(loanTree40)
loanTree50 <- C5.0(x = train.x, y = train.y, costs = matrix(c(1,4.2,1,1),2,2))
summary(loanTree50)

#### 4 ????????????????????????
#Not sure which one to pick


#### 5
predict25 <- predict(loanTree25,val.x)
errors25 <- mean(predict25 != val.y)

predict40 <- predict(loanTree40,val.x)
errors40 <- mean(predict40 != val.y)

predict50 <- predict(loanTree50,val.x)
errors50 <- mean(predict50 != val.y)

