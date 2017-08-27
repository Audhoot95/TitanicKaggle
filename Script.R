#Set working directory 
setwd("C:/Users/Audhoot/Downloads/test/Final")

#Load all the required libraries
library(rattle)
library(dplyr)
library(mice)
library(randomForest)
library(ggplot2)
library(ggthemes) # visualization
library(scales) # visualization
library(party)

#Read train and test data
train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)

#Binding both datasets to full
full <- bind_rows(train, test )

#View full structure and summary 

str(full)


#Converting some variables into factors to view missing variables 
full$Sex <- as.factor(full$Sex)
full$Embarked <- as.factor(full$Embarked)
full$Pclass <- as.factor(full$Pclass)
full$Cabin <- as.factor(full$Cabin)
full$Ticket <- as.factor(full$Ticket)
full$Survived <- as.factor(full$Survived)



summary(full)



#Dealing with missing values of Embarked and Fare 


#Viewing missing values in Embarked
full[full$Embarked == '',]

#Table of class and Embarked
table(full$Embarked, full$Pclass)

#Assigning missing values to 'S'. Most boarded by first class passengers
full$Embarked[full$Embarked == ''] <- 'S'



#Viewing missing values in Fare
full[is.na(full$Fare),]

#Viewing summary of fare for 3rd class passengers
summary(full$Fare[full$Pclass == 3])

#Setting missing value as median 
full$Fare[1044] <- 8.05


#Checking summary again 
summary(full)


#Feature Engineering Part 1 --> Title, Subclass & Family Size 


#Title Variable 

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

#Visualizing Survival rate with respect to title
ggplot(full[1:891,], aes(x = Title, fill = factor(Survived))) + geom_bar(stat='count', position='dodge', color = 'black') + labs(x = 'Title vs Survival Rate ') 


#Saving plot as pdf
pdf('1.Titlevssurvived.pdf', width=10, height=10)
ggplot(full[1:891,], aes(x = Title, fill = factor(Survived))) + geom_bar(stat='count', position='dodge', color = 'black') + labs(x = 'Title vs Survived') 
dev.off()




#Family Size 

#Adding Siblings ,Spouse, Parents, Children and Self 

full$Fsize <- full$SibSp + full$Parch + 1

#Visualizing Family size with respect to surviva rate 
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) + geom_bar(stat='count', position='dodge', color = 'black') + scale_x_continuous(breaks=c(1:11))+ labs(x = 'Family Size vs Survival Rate') 

#Saving plot as pdf
pdf('2.FsizevsSurvived.pdf', width=10, height=10)
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) + geom_bar(stat='count', position='dodge', color = 'black') + scale_x_continuous(breaks=c(1:11))+ labs(x = 'Family Size vs Survival Rate') 
dev.off()





#Subclass : Creating sublclasses with respect to passenger fares
# Royalty: R , Higher upper class  HU, Lower Upper Class : LU
# Higher Middle Class : HM, Lower Middle Class: LM
# Higher Lower class : HL and Lower Lower class : LL

#visualizing Passenger fare 
ggplot(full, aes(x = PassengerId, y = Fare, color = Pclass, shape = Survived)) + geom_point() + xlim(0,891)


#First class subclasses
table(train$Survived, train$Pclass, train$Fare > 200)
table(train$Survived, train$Pclass, train$Fare <= 200 & train$Fare > 50 )
table(train$Survived, train$Pclass, train$Fare <= 50)

ggplot(full, aes(x = PassengerId, y = Fare, color = Pclass, shape = Survived)) + geom_point() + xlim(0,891)+ geom_hline(yintercept = 200) + geom_hline(yintercept = 50)


full$Subclass[full$Pclass == 1 & full$Fare > 200] <- 'R'
full$Subclass[full$Pclass == 1 & full$Fare <= 200 & full$Fare > 50] <- 'HU'
full$Subclass[full$Pclass == 1 & full$Fare <= 50] <- 'LU'



#Second class subclasses

summary(full$Fare[full$Pclass == 2])

ggplot(full, aes(x = PassengerId, y = Fare, color = Pclass, shape = Survived)) + geom_point() + xlim(0,891)+ geom_hline(yintercept = 21) 


full$Subclass[full$Pclass == 2 & full$Fare > 21] <- 'HM'
full$Subclass[full$Pclass == 2 & full$Fare <= 21] <- 'LM'

#Lower Class subclasses

summary(full$Fare[full$Pclass == 3])

ggplot(full, aes(x = PassengerId, y = Fare, color = Pclass, shape = Survived)) + geom_point() + xlim(0,891)+ geom_hline(yintercept = 13) 


full$Subclass[full$Pclass == 3 & full$Fare > 13] <- 'HL'
full$Subclass[full$Pclass == 3 & full$Fare <= 13] <- 'LL'

full$Subclass <- as.factor(full$Subclass)
summary(full)



ggplot(full, aes(x = PassengerId, y = Fare, color = Subclass, shape = Survived)) + geom_point() + xlim(0,891)

ggplot(full[1:891,], aes(x = Subclass, fill = Survived)) + geom_bar(stat='count', position='dodge', color = 'black') + labs(x = 'Subclass vs Survival Rate')   



#Saving plot as pdf
pdf('3.SubclassvsFare.pdf', width=10, height=10)
ggplot(full, aes(x = PassengerId, y = Fare, color = Subclass, shape = Survived)) + geom_point() + xlim(0,891) + labs(x = 'Visualizing Fare wrt Subclasses')
dev.off()

#Saving plot as pdf
pdf('4.SubclassvsSurvived.pdf', width=10, height=10)
ggplot(full[1:891,], aes(x = Subclass, fill = Survived)) + geom_bar(stat='count', position='dodge', color = 'black') + labs(x = 'Subclass vs Survival Rate')   
dev.off()



#Dealing with missing values of Age using Imputation --> mice package

set.seed(1)
simple <- full[c('Fare', 'Age','Sex','Title')]
imputed <- complete(mice(simple))
full$Age <- imputed$Age

#Viewing summary again 
summary(full)



#Feature Engineering Part 2 --> Deck Variable and Family ID

#Extracting surname of all passengers and famiy size. Assigning each passenger an ID with respect to surname and family size 


full$Surname <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
full$FamilyID <- paste(full$Surname, as.character(full$Fsize))
full$FamilyID <- factor(full$FamilyID)


#Getting Deck Initials(2) of all passengers 

full$DeckInitial <- substr(full$Cabin, 1 ,2)
full$DeckInitial <- as.factor(full$DeckInitial)

#Setting missing values as NA
full$DeckInitial[full$DeckInitial == ''] <- NA

#Setting SurnameDeck as a matrix of all family IDs and respective Deck initials 
SurnameDeck <- as.matrix(table(full$DeckInitial, full$FamilyID))


#Viewing all surnames with corresponding Deck intials occupied by maximum number of family members 
colnames(SurnameDeck)
rownames(SurnameDeck)[apply(SurnameDeck, 2, which.max)]


#Creating a dataframe of FamilyID and Deck 

Deckdata <- data.frame( FamilyID = colnames(SurnameDeck), Deck = rownames(SurnameDeck)[apply(SurnameDeck, 2, which.max)] )

#Viewing NA values
summary(full$DeckInitial)


#Creating a new variable to set familyids with their corresponding decks from Deckdata
#this is done using the bind function 


full$DeckInitial <- as.character(full$DeckInitial)
Deckdata$FamilyID <- as.character(Deckdata$FamilyID)
Deckdata$Deck <- as.character(Deckdata$Deck)

r <- data.frame(y = '')
r$y <- as.character(r$y)

for(x in 1:1309){
  if(is.na(full$DeckInitial[x])){
    r <- rbind(r,Deckdata$Deck[full$FamilyID[x]])
  }else{
    r <- rbind(r, full$DeckInitial[x])
  }
}


#Updating the new deck value and viewing it 

full$Deck <- as.factor(r$y[2:1310])
summary(as.factor(full$Deck))
#There are still 987 missing values 

#Deleting Deckinitial Surname and FamilyID
full$DeckInitial <- NULL
full$FamilyID <- NULL
full$Surname <- NULL


#Predicting the rest of the missing values using random forest 


# Family IDs Again -> Random forests cannot handle many categories so we need to reduce family IDs
#This is done by Setting family ids to small for passengers with family size less than 3 and which occur not more than twice

full$Surname <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
full$FamilyID <- paste(full$Surname, as.character(full$Fsize))

full$FamilyID[full$Fsize <= 3] <- 'Small'
famIDs <- data.frame(table(full$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
full$FamilyID[full$FamilyID %in% famIDs$Var1] <- 'Small'




#Converting variables into factors for random forests

full$Deck <- as.factor(full$Deck)
full$Title <- as.factor(full$Title)
full$FamilyID <- factor(full$FamilyID)

# Seperating test data by extracting passengers with NA Deck values 

full$Deck[full$Deck == ''] <- NA
train <- full[!(is.na(full$Deck)),]
test <- full[is.na(full$Deck),]

#Deleting Deck variable from test data
test$Deck <- NULL

#Check for missing values in training data
sapply(train, function(x) sum(is.na(x)))


#Random Forest Model 

DeckModel <- randomForest( factor(Deck) ~  Subclass + FamilyID  + Fsize + Fare , data = train)

#Test predictions 
testprediction <- predict(DeckModel, newdata = test)


#Updating test dataset with new predicted deck values 
test$Deck <- testprediction


#Assessing variable importance
importance    <- importance(DeckModel)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
my_plot <- ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat='identity') + geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust=0, vjust=0.55, size = 4, colour = 'red') + coord_flip() +  labs(x = 'Variables') + theme_few()

my_plot

#Saving plot as pdf
pdf('5.VariableImportanceDeckVariable.pdf', width=10, height=10)
my_plot
dev.off()


#Binding train and test data and ordering by passengerId
full <- bind_rows(train, test )
full <- full[order(full$PassengerId),]


#Viewing summary of Deck variable 
summary(as.factor(full$Deck))


#Deleting Surname, FamilyID and famID 
full$Surname < NULL
full$FamilyID <- NULL
rm(famIDs)


full$Deck <- as.factor(full$Deck)

#View full
summary(full)


#Creating New Family ID that will be used for final model building

full$Surname <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
full$FamilyID <- paste(full$Surname, as.character(full$Fsize))
full$FamilyID[full$Fsize < 2] <- 'Small'
famIDs <- data.frame(table(full$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 3,]
full$FamilyID[ full$Fsize <= 2 & full$FamilyID %in% famIDs$Var1] <- 'Small'


full$FamilyID <- factor(full$FamilyID)





#Building Model 

#Seperating test and train dataset
train <- full[1:891,]
test <- full[892:1309,]

#Deleting Survived variable from test
test$Survived <- NULL


#Checking missing values in training data 
sapply(train, function(x) sum(is.na(x)))



#Conditional Inference Tress

set.seed(1)

testmodel <- cforest(as.factor(Survived) ~ Sex + Deck + Fare + Age + Embarked + Subclass + Fsize +  Title + FamilyID, data = train, controls=cforest_unbiased(ntree=2500, mtry=3))

#Best model so far --> testmodel <- cforest(as.factor(Survived) ~ Sex + Age + DeckInitial + Parch +  SibSp + Embarked + Subclass + Title + Fsize  + FamilyID, data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

trainprediction <- predict(testmodel, train , OOB=TRUE, type = "response")

testprediction <- predict(testmodel, test , OOB=TRUE, type = "response")


#Checking model accuracy using Confusion matrix
table(train$Survived, trainprediction)



#Creating a dataframe and writing solution to a file
solution <- data.frame(PassengerId = test$PassengerId, Survived = testprediction)
 
#Check if rows are equal to 418
nrow(solution)

write.csv( solution, file = "solution.csv", row.names = FALSE)

