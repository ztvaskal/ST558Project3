## Read in dataset from UCI:
dataurl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv"
hfcrRAW <- read_csv(dataurl)

## Rename Variables
names(hfcrRAW)[1]<-"Age"
names(hfcrRAW)[2]<-"Anemia"
names(hfcrRAW)[3]<-"CPK"
names(hfcrRAW)[4]<-"Diabetes"
names(hfcrRAW)[5]<-"EF"
names(hfcrRAW)[6]<-"HighBP"
names(hfcrRAW)[7]<-"Platelet"
names(hfcrRAW)[8]<-"SCr"
names(hfcrRAW)[9]<-"SNa"
names(hfcrRAW)[10]<-"Sex"
names(hfcrRAW)[11]<-"Smoke"
names(hfcrRAW)[12]<-"Time"
names(hfcrRAW)[13]<-"Target"

## Reorder Variables
hfcrDATA <- hfcrRAW[,c(1,3,5,7,8,9,12,2,4,6,10,11,13)]
varLIST <- select(hfcrDATA, Age:Time)
varLISTX <- select(hfcrDATA, Age:Time)
varLISTY <- select(hfcrDATA, Age:Time)

set.seed(1)
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(hfcrDATA), alpha * nrow(hfcrDATA))
trainSet <- hfcrDATA[inTrain,]
testSet  <- hfcrDATA[-inTrain,]

mtry <- c(2,3,sqrt(dim(trainSet)[2]-1),4,5,6)
trCtrl1 <- trainControl(method = "cv", number = 10)
fitRF <- train(Target ~ ., data = trainSet, method = "rf",
                         trControl = trCtrl1, preProcess = c("center", "scale"),
                         ntree = 500, tuneGrid = expand.grid(.mtry = mtry))

