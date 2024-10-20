## DATA MINING PROJECT
# SARVESH KRISHNAN RAJENDRAN (U86908171)
# ANJANA N (U45094792)

library(pROC)
library(caret)
library(rsample)
library(Boruta)
library(FSelector)
library(e1071)
library(doParallel)
library(caret)
library(foreach)
library(dplyr)
library(caret)
library(kknn)
#setting the working directory


#####################
# PREPROCESSING
df=read.csv("project_dataset_5K.csv")

str(df)

dim(df)

summary(df)


##missing values in the whole dataset
missing_values=sum(is.na(df))
missing_values

##missing values in each colmn
missing_val_each_col=sapply(df,function(x) sum(is.na(x)))
missing_val_each_col

##number of columns with missing values
number_of_columns_with_na=sum(sapply(df, function(x) any(is.na(x))))
number_of_columns_with_na

#name of the columns with all values as blank
columns_with_all_na = c(names(df)[sapply(df, function(x) all(is.na(x)))])
columns_with_all_na

#removing the columns that have all values as blank
df=df[, !names(df) %in% columns_with_all_na]


##removed all columns with all na
columns_with_all_na=names(df)[sapply(df, function(x) all(is.na(x)))]
columns_with_all_na

##number of columns with missing values
columns_with_missing_values=names(df)[sapply(df, function(x) any(is.na(x)))]
columns_with_missing_values



##number of columns in the df afer dropping columns that have all null values
ncol(df)


##finding the percent missing values in each column and adding the columns to a 
##vector that have percent missiung more that >=40%
count=0
col_to_drop=c()
for(i in columns_with_missing_values){
  count_missing_val=sum(is.na(df[[i]]))
  total_val=5000
  percent_missing=(count_missing_val/5000)*100
  if(percent_missing>=40){
    count=count+1
    col_to_drop=c(col_to_drop,i)
  }
}
print(paste("Columns that have 40% or more null values ",count))

col_to_drop
## dropping the columns that have more than 40% or more null values
df = df[, !names(df) %in% col_to_drop]

##number of columns after dropping the columns that have 40% or more null values
ncol(df)

##number of columns with missing values
columns_with_missing_values=names(df)[sapply(df, function(x) any(is.na(x)))]
columns_with_missing_values

length(columns_with_missing_values)

#dropping columns "SAFETIME"  "CTELNUM1"  "CELLFON5"  "CADULT1" because it is 
#not relevant to the class label
df = df[, !names(df) %in% c("SAFETIME","CTELNUM1","CELLFON5","CADULT1")]

#CELLSEX
sum(is.na(df$CELLSEX))
boxplot(df$CELLSEX)
#filling column CELLSEX using mfv od male or femal
library(modeest)
df$CELLSEX[is.na(df$CELLSEX)]=mfv(df$CELLSEX[!is.na(df$CELLSEX)])
sum(is.na(df$CELLSEX))

#PVTRESD3
sum(is.na(df$PVTRESD3))
boxplot(df$PVTRESD3)
df$PVTRESD3[is.na(df$PVTRESD3)]=mfv(df$PVTRESD3[!is.na(df$PVTRESD3)])
sum(is.na(df$PVTRESD3))

#LANDLINE
sum(is.na(df$LANDLINE))
boxplot(df$LANDLINE)

df$LANDLINE[is.na(df$LANDLINE)]=mfv(df$LANDLINE[!is.na(df$LANDLINE)])
sum(is.na(df$LANDLINE))

#CSTATE1
sum(is.na(df$CSTATE1))
boxplot(df$CSTATE1)
df$CSTATE1[is.na(df$CSTATE1)]=mfv(df$CSTATE1[!is.na(df$CSTATE1)])
sum(is.na(df$CSTATE1))

#HHADULT
sum(is.na(df$HHADULT))
boxplot(df$HHADULT)
df$HHADULT[is.na(df$HHADULT)]=mean(df$HHADULT[!is.na(df$HHADULT)])
sum(is.na(df$HHADULT))

##CPDEMO1B
sum(is.na(df$CPDEMO1B))
boxplot(df$CPDEMO1B)
df$CPDEMO1B[is.na(df$CPDEMO1B)]=mfv(df$CPDEMO1B[!is.na(df$CPDEMO1B)])
sum(is.na(df$CPDEMO1B))   

#VETERAN3
##as per code book the column VETERAN3 has a value of 7 if it is unknown or notsure
sum(is.na(df$VETERAN3))
boxplot(df$VETERAN3)
df$VETERAN3[is.na(df$VETERAN3)]=mfv(df$VETERAN3[!is.na(df$VETERAN3)])
sum(is.na(df$VETERAN3)) 

#EMPLOY1
sum(is.na(df$EMPLOY1))
boxplot(df$EMPLOY1)
df$EMPLOY1[is.na(df$EMPLOY1)]=mfv(df$EMPLOY1[!is.na(df$EMPLOY1)])
sum(is.na(df$EMPLOY1))



#CHILDREN
#
sum(is.na(df$CHILDREN))
boxplot(df$CHILDREN)
df$CHILDREN[is.na(df$CHILDREN)]=median(!is.na(df$CHILDREN))
sum(is.na(df$CHILDREN))

#INCOME2
sum(is.na(df$INCOME2))
boxplot(df$INCOME2)
df$INCOME2[is.na(df$INCOME2)]=mfv(df$INCOME2[!is.na(df$INCOME2)])
sum(is.na(df$INCOME2))

#WEIGHT2
sum(is.na(df$WEIGHT2))
boxplot(df$WEIGHT2)
df$WEIGHT2[247]

#Writing a for loop to converr the weights in kg to pounds
for (i in 1:nrow(df)) {

  if (!is.na(df$WEIGHT2[i])) {
    if (df$WEIGHT2[i] >= 9023 && df$WEIGHT2[i] <= 9352) {
     
      kg=as.integer(substr(df$WEIGHT2[i], 2, nchar(as.character(df$WEIGHT2[i]))))
      
      df$HEIGHT3[i] <- kg*2.20462
    } 
  }
}
df$WEIGHT2[247]
sum(is.na(df$WEIGHT2))
mean_WEIGHT2=mean(df$WEIGHT2[df$WEIGHT2!=9999 & df$WEIGHT2!=7777 &!is.na(df$WEIGHT2)])
df$WEIGHT2[is.na(df$WEIGHT2)]=floor(mean_WEIGHT2)
sum(is.na(df$WEIGHT2))

#HEIGHT3
sum(is.na(df$HEIGHT3))
boxplot(df$HEIGHT3)
max(df$HEIGHT3[df$HEIGHT3<=9900& df$HEIGHT3>7777],na.rm = TRUE)
df$HEIGHT3[1192]

#Writing a for loop for converting height in inches
for (i in 1:nrow(df)) {
  # Check the value at the current index, not the entire vector
  if (!is.na(df$HEIGHT3[i])) {
    if (df$HEIGHT3[i] >= 200 && df$HEIGHT3[i] <= 711) {
      # Extract feet and inches and convert to inches
      feet=as.integer(substr(df$HEIGHT3[i], 1, 1))
      inches=as.integer(substr(df$HEIGHT3[i], 2, 3))
      df$HEIGHT3[i] = (feet * 12) + inches
    } else if (startsWith(as.character(df$HEIGHT3[i]), "9")) {
      # Convert from centimeters to inches
      cm=as.numeric(substr(as.character(df$HEIGHT3[i]), 2, nchar(as.character(df$HEIGHT3[i]))))
      df$HEIGHT3[i] = cm * 0.393701
    }
  }
}
mean_HEIGHT3=mean(df$HEIGHT3[df$HEIGHT3!=9999 & df$HEIGHT3!=7777],na.rm = TRUE)
mean_HEIGHT3
df$HEIGHT3[is.na(df$HEIGHT3)]=floor(mean_HEIGHT3)

#DEAF
sum(is.na(df$DEAF))
boxplot(df$DEAF)
df$DEAF[is.na(df$DEAF)]=mfv(df$DEAF[!is.na(df$DEAF)])
sum(is.na(df$DEAF))

#BLIND
sum(is.na(df$BLIND))
boxplot(df$BLIND)
df$BLIND[is.na(df$BLIND)]=mfv(df$BLIND[!is.na(df$BLIND)])
sum(is.na(df$BLIND))

#DECIDE
sum(is.na(df$DECIDE))
boxplot(df$DECIDE)
df$DECIDE[is.na(df$DECIDE)]=mfv(df$DECIDE[!is.na(df$DECIDE)])
sum(is.na(df$DECIDE))

#DIFFWALK
sum(is.na(df$DIFFWALK))
boxplot(df$DIFFWALK)
df$DIFFWALK[is.na(df$DIFFWALK)]=mfv(df$DIFFWALK[!is.na(df$DIFFWALK)])
sum(is.na(df$DIFFWALK))

#DIFFDRES
sum(is.na(df$DIFFDRES))
boxplot(df$DIFFDRES)
df$DIFFDRES[is.na(df$DIFFDRES)]=mfv(df$DIFFDRES[!is.na(df$DIFFDRES)])
sum(is.na(df$DIFFDRES))

#DIFFALON
sum(is.na(df$DIFFALON))
boxplot(df$DIFFALON)
df$DIFFALON[is.na(df$DIFFALON)]=mfv(df$DIFFALON[!is.na(df$DIFFALON)])
sum(is.na(df$DIFFALON))

#SMOKE100
sum(is.na(df$SMOKE100))
boxplot(df$SMOKE100)
df$SMOKE100[is.na(df$SMOKE100)]=mfv(df$SMOKE100[!is.na(df$SMOKE100)])
sum(is.na(df$SMOKE100))

#USENOW3
sum(is.na(df$USENOW3))
boxplot(df$USENOW3)
df$USENOW3[is.na(df$USENOW3)]=mfv(df$USENOW3[!is.na(df$USENOW3)])
sum(is.na(df$USENOW3))

#ALCDAY5
sum(is.na(df$ALCDAY5))
boxplot(df$ALCDAY5)
df$ALCDAY5
for (i in 1:nrow(df)){
  if (!is.na(df$ALCDAY5[i]) && df$ALCDAY5[i] >= 101 && df$ALCDAY5[i] <= 107) {
    # Extract the second digit (which represents days per week)
    weekly_str = substr(as.character(df$ALCDAY5[i]), 2, nchar(as.character(df$ALCDAY5[i])))
    weekly=as.numeric(weekly_str)
    monthly = weekly*4
    newcode = as.integer(paste0('2', sprintf("%02d", monthly)))
    # Convert weekly frequency to monthly frequency
    df$ALCDAY5[i]=newcode
}
}
df$ALCDAY5
boxplot(df$ALCDAY5[!is.na(df$ALCDAY5)&df$ALCDAY5 != 777 & df$ALCDAY5 != 888 &df$ALCDAY5!=999])
mean_ALCDAY5 = mean( df$ALCDAY5[df$ALCDAY5 != 777 & df$ALCDAY5 != 888&df$ALCDAY5!=999], na.rm = TRUE)
mean_ALCDAY5=floor(mean_ALCDAY5)
df$ALCDAY5[is.na(df$ALCDAY5)]=mean_ALCDAY5

#FLUSHOT7
sum(is.na(df$FLUSHOT7))
boxplot(df$FLUSHOT7)
df$FLUSHOT7[is.na(df$FLUSHOT7)]=mfv(df$FLUSHOT7[!is.na(df$FLUSHOT7)])
sum(is.na(df$FLUSHOT7))

#PNEUVAC4
sum(is.na(df$PNEUVAC4))
boxplot(df$PNEUVAC4)
df$PNEUVAC4[is.na(df$PNEUVAC4)]=mfv(df$PNEUVAC4[!is.na(df$PNEUVAC4)])
sum(is.na(df$PNEUVAC4))

#FALL12MN
sum(is.na(df$FALL12MN))
boxplot(df$FALL12MN)
df$FALL12MN[is.na(df$FALL12MN)]=median(df$FALL12MN[!is.na(df$FALL12MN)])
sum(is.na(df$FALL12MN))

#SEATBELT
sum(is.na(df$SEATBELT))
boxplot(df$SEATBELT)
df$SEATBELT[is.na(df$SEATBELT)]=mfv(df$SEATBELT[!is.na(df$SEATBELT)])
sum(is.na(df$SEATBELT))

#COLNSCPY
sum(is.na(df$COLNSCPY))
boxplot(df$COLNSCPY)
df$COLNSCPY[is.na(df$COLNSCPY)&(df$X_AGE_G<4)]=2
sum(is.na(df$COLNSCPY))
df$COLNSCPY[is.na(df$COLNSCPY)&(df$X_AGE_G>=4)]=mfv(df$COLNSCPY[!is.na(df$COLNSCPY)&(df$X_AGE_G>=4)])
sum(is.na(df$COLNSCPY))

#SIGMSCPY
sum(is.na(df$SIGMSCPY))
boxplot(df$SIGMSCPY)
df$SIGMSCPY[is.na(df$SIGMSCPY)&(df$X_AGE_G<4)]=2
df$SIGMSCPY[is.na(df$SIGMSCPY)&(df$X_AGE_G>=4)]=mfv(df$SIGMSCPY[!is.na(df$SIGMSCPY)&(df$X_AGE_G>=4)])
sum(is.na(df$SIGMSCPY))

#BLDSTOL1
sum(is.na(df$BLDSTOL1))
boxplot(df$BLDSTOL1)
df$BLDSTOL1[is.na(df$BLDSTOL1)&(df$X_AGE_G<4)]=2
df$BLDSTOL1[is.na(df$BLDSTOL1)&(df$X_AGE_G>=4)]=mfv(df$BLDSTOL1[!is.na(df$BLDSTOL1)&(df$X_AGE_G>=4)])
sum(is.na(df$BLDSTOL1))


# "STOOLDNA" using "Median" method to replace the missing values
sum(is.na(df$STOOLDNA))
boxplot(df$STOOLDNA)
df$STOOLDNA[is.na(df$STOOLDNA)] = median(df$STOOLDNA, na.rm=TRUE)
sum(is.na(df$STOOLDNA))

# "VIRCOLON" using "Median" method to replace the missing values
sum(is.na(df$VIRCOLON))
boxplot(df$VIRCOLON)
df$VIRCOLON[is.na(df$VIRCOLON)] = median(df$VIRCOLON, na.rm=TRUE)
sum(is.na(df$VIRCOLON))

# "HIVTST7" using "Mode" method to replace the missing values
sum(is.na(df$HIVTST7))
boxplot(df$HIVTST7)
df$HIVTST7[is.na(df$HIVTST7)] = mfv(df$HIVTST7)
sum(is.na(df$HIVTST7))

# "HIVRISK5" using "Mode" method to replace the missing values
sum(is.na(df$HIVRISK5))
boxplot(df$HIVRISK5)
df$HIVRISK5[is.na(df$HIVRISK5)] = mfv(df$HIVRISK5)
sum(is.na(df$HIVRISK5))


# "ECIGARET" using "Mode" method to replace the missing values
sum(is.na(df$ECIGARET))
boxplot(df$ECIGARET)
df$ECIGARET[is.na(df$ECIGARET)] = mfv(df$ECIGARET)
sum(is.na(df$ECIGARET))


#"X_METSTAT" using "Mode" method to replace the missing values
sum(is.na(df$X_METSTAT))
boxplot(df$X_METSTAT)
df$X_METSTAT[is.na(df$X_METSTAT)] = mfv(df$X_METSTAT)
sum(is.na(df$X_METSTAT))

#"X_URBSTAT" using "Mode" method to replace the missing values
sum(is.na(df$X_URBSTAT))
boxplot(df$X_URBSTAT)
df$X_URBSTAT[is.na(df$X_URBSTAT)] = mfv(df$X_URBSTAT)
sum(is.na(df$X_URBSTAT))

#"X_CHISPNC" using "Mode" method to replace the missing values
sum(is.na(df$X_CHISPNC))
boxplot(df$X_CHISPNC)
df$X_CHISPNC[is.na(df$X_CHISPNC)] = mfv(df$X_CHISPNC)
sum(is.na(df$X_CHISPNC))

#"X_MICHD" using "Mode" method to replace the missing values
sum(is.na(df$X_MICHD))
boxplot(df$X_MICHD)
df$X_MICHD[is.na(df$X_MICHD)] = mfv(df$X_MICHD)
sum(is.na(df$X_MICHD))

# "X_DRDXAR2" using "Mode" method to replace the missing values
sum(is.na(df$X_DRDXAR2))
boxplot(df$X_DRDXAR2)
df$X_DRDXAR2[is.na(df$X_DRDXAR2)]= mfv(df$X_DRDXAR2)
sum(is.na(df$X_DRDXAR2))

# "HTIN4"using "Median" method to replace the missing values
sum(is.na(df$HTIN4))
boxplot(df$HTIN4)
df$HTIN4[is.na(df$HTIN4)] = median(df$HTIN4, na.rm=TRUE)
sum(is.na(df$HTIN4))

# "HTM4" using "Median" method to replace the missing values
sum(is.na(df$HTM4))
boxplot(df$HTM4)
df$HTM4[is.na(df$HTM4)] = median(df$HTM4, na.rm=TRUE)
sum(is.na(df$HTM4))

# "WTKG3" using "Median" method to replace the missing values
sum(is.na(df$WTKG3))
boxplot(df$WTKG3)
df$WTKG3[is.na(df$WTKG3)] = median(df$WTKG3, na.rm=TRUE)
sum(is.na(df$WTKG3))

# "X_BMI5" using "Median" method to replace the missing values
sum(is.na(df$X_BMI5))
boxplot(df$X_BMI5)
df$X_BMI5[is.na(df$X_BMI5)] =  median(df$X_BMI5, na.rm=TRUE)

#"X_BMI5CAT" using "Median" method to replace the missing values
sum(is.na(df$X_BMI5CAT))
boxplot(df$X_BMI5CAT)
df$X_BMI5CAT[is.na(df$X_BMI5CAT)] = median(df$X_BMI5CAT, na.rm=TRUE)
sum(df$X_BMI5CAT)

# "X_AIDTST4" using "Median" method to replace the missing values
sum(is.na(df$X_AIDTST4))
boxplot(df$X_AIDTST4)
df$X_AIDTST4[is.na(df$X_AIDTST4)] = median(df$X_AIDTST4, na.rm=TRUE)
sum(is.na(df$X_AIDTST4))

##missing values in the whole dataset
missing_values=sum(is.na(df))
missing_values

##missing values in each colmn
missing_val_each_col=sapply(df,function(x) sum(is.na(x)))
missing_val_each_col

##number of columns with missing values
number_of_columns_with_na = sum(sapply(df, function(x) any(is.na(x))))
number_of_columns_with_na

columns_with_missing_values = names(df)[sapply(df, function(x) any(is.na(x)))]
columns_with_missing_values

ncol(df)
#total columns=276
# -143 columns that we dropped bechause thre were more than 40% missing data
# -6 columns that had all values as missing
# -4 columns that we ound to have no help in finding the class
# there fore remaining columns is 

#DATA REDUCTION
#NEARZEROVARIANCE
library(caret)
dim(df)
head(df)
near_0_varicance=c(nearZeroVar(df, names = TRUE))
near_0_varicance

df = df[, !names(df) %in% near_0_varicance]
ncol(df)


non_numeric_columns = sapply(df, function(x) !is.numeric(x))
non_numeric_columns

# To get the names of the non-numeric columns
non_numeric_column_names = names(df)[non_numeric_columns]
non_numeric_column_names

df$Class[df$Class=='Y']=1
df$Class[df$Class=='N']=0

df$Class=as.numeric(df$Class)


#Correlation analysis
str(df$Class)

threshold = 0.8 # Define your threshold for significance
cor_matrix = cor(df)
cor_pairs = which(abs(cor_matrix) > threshold, arr.ind = TRUE)

cor_pairs

# Filter to remove self-correlation and NA values
cor_pairs = cor_pairs[cor_pairs[, 1] != cor_pairs[, 2], ]
cor_pairs
cor_values = cor_matrix[cor_pairs]

cor_values
# Create a data frame for the significant pairs
high_cor_df = data.frame(
  Variable1 = rownames(cor_matrix)[cor_pairs[, 1]],
  Variable2 = colnames(cor_matrix)[cor_pairs[, 2]],
  Correlation = cor_values
)
high_cor_df

# Unique variables to remove from "high_cor_df"
variables_to_remove = unique(high_cor_df$Variable2)
variables_to_remove
length(variables_to_remove)

df = df[, !names(df) %in% variables_to_remove]

#attributes remaining after removing highly correlated variables
#class attributed is included
ncol(df)

write.csv(df,'preprocessed.csv',row.names = FALSE)

#############
#SPLITTING THE DATASET INTO TEST AND TRAIN AND BALANCING THE TRAIN DATA USING 2 SAMPLING METHODS

df=read.csv("Preprocessed.csv")
df$Class[df$Class==1]="Y"
df$Class[df$Class==0]="N"

##checking percentage of 0 and 1 in Class column in the whole dataset
class_table <- table(df$Class)
class_proportions <- prop.table(class_table)
class_percentages <- class_proportions * 100
print(class_percentages)


##Splitting the dataset into test and train 66% training and 34% testing
set.seed(7)
split = initial_split(df,prop = 0.66,strata = "Class")
train = training(split)
test = testing(split)
write.csv(test,"initial_test.csv",row.names = FALSE)
write.csv(train,"initial_train.csv",row.names = FALSE)

##checking percentage of 0 and 1 in Class column in train dataset
class_train <- table(train$Class)
train_proportions <- prop.table(class_train)
train_percentages <- train_proportions * 100
print(train_percentages)


number_0=sum(train$Class=="N")
number_1=sum(train$Class=="Y")


index_0s=which(train$Class=="N")


index_1s=which(train$Class=="Y")


#undersampling using simple random sampling without replacement
set.seed(7)
srswor=sample(index_0s,number_1,replace = FALSE)


undersample_index=c(srswor,index_1s)
undersample_index

undersample=train[undersample_index,]
prop.table(table(undersample$Class))

#oversampling using simple random sampling with replacement
set.seed(7)
srswr=sample(index_1s,number_0,replace = TRUE)


oversample_index=c(srswr,index_0s)
oversample_index

oversample=train[oversample_index,]
prop.table(table(oversample$Class))

#saving the underbalanced and overbalanced data sets 

#saving the underbalanced dataset
write.csv(undersample,"underbalanced.csv",row.names=FALSE)

#saving the overbalanced dataset
write.csv(oversample,"overbalanced.csv",row.names=FALSE)

###############
# USING 3 FEATURE SELECTION METHODS ON THE 2 BALANCED TRAIN DATA
#Feature Selection

underbalanced=read.csv("underbalanced.csv")
overbalanced=read.csv("overbalanced.csv")

#Underbalanced 
underbalanced$Class=factor(underbalanced$Class)

#boruta
set.seed(1001)
underbalanced.boruta=Boruta(Class~.,data=underbalanced)
underbalanced.boruta

features.boruta.underbalanced= getSelectedAttributes(underbalanced.boruta, withTentative = FALSE)
underbalanced_boruta= underbalanced[,c(features.boruta.underbalanced,"Class")]
write.csv(underbalanced_boruta,"underbalanced_boruta.csv",row.names = FALSE)

test_underbalanced_boruta= test[,c(features.boruta.underbalanced,"Class")]
write.csv(test_underbalanced_boruta,"test_underbalanced_boruta.csv",row.names = FALSE)

#information gain

underbalanced.infogain=information.gain(Class~.,data=underbalanced)
underbalanced.infogain=cbind(rownames(underbalanced.infogain),data.frame(underbalanced.infogain,row.names = NULL))
names(underbalanced.infogain)=c("Attribute","Info Gain")
sorted.underbalanced.infogain=underbalanced.infogain[order(-underbalanced.infogain$`Info Gain`),]
sorted.underbalanced.infogain$Attribute[1:5]

features.infogain.underbalanced=c(sorted.underbalanced.infogain$Attribute[1:5])
underbalanced_infogain=underbalanced[,c(features.infogain.underbalanced,"Class")]
write.csv(underbalanced_infogain,"underbalanced_infogain.csv",row.names = FALSE)

test_underbalanced_infogain=test[,c(features.infogain.underbalanced,"Class")]
write.csv(test_underbalanced_infogain,"test_underbalanced_infogain.csv",row.names = FALSE)
#CFS
underbalanced.cfs=cfs(Class~.,data=underbalanced)
underbalanced.cfs

underbalanced_cfs=underbalanced[,c(underbalanced.cfs,"Class")]
write.csv(underbalanced_cfs,"underbalanced_cfs.csv",row.names = FALSE)

test_underbalanced_cfs=test[,c(underbalanced.cfs,"Class")]
write.csv(test_underbalanced_cfs,"test_underbalanced_cfs.csv",row.names = FALSE)
#Overbalanced
overbalanced$Class=factor(overbalanced$Class)

#boruta
set.seed(1001)
overbalanced.boruta=Boruta(Class~.,data=overbalanced)
overbalanced.boruta

features.boruta.overbalanced= getSelectedAttributes(overbalanced.boruta, withTentative = FALSE)
overbalanced_boruta= overbalanced[,c(features.boruta.overbalanced,"Class")]
write.csv(overbalanced_boruta,"overbalanced_boruta.csv",row.names = FALSE)

test_overbalanced_boruta= test[,c(features.boruta.overbalanced,"Class")]
write.csv(test_overbalanced_boruta,"test_overbalanced_boruta.csv",row.names = FALSE)
#information gain

overbalanced.infogain=information.gain(Class~.,data=overbalanced)
overbalanced.infogain=cbind(rownames(overbalanced.infogain),data.frame(overbalanced.infogain,row.names = NULL))
names(overbalanced.infogain)=c("Attribute","Info Gain")
sorted.overbalanced.infogain=overbalanced.infogain[order(-overbalanced.infogain$`Info Gain`),]
sorted.overbalanced.infogain[1:5,]

features.infogain.overbalanced=c(sorted.overbalanced.infogain$Attribute[1:5])
overbalanced_infogain=overbalanced[,c(features.infogain.overbalanced,"Class")]
write.csv(overbalanced_infogain,"overbalanced_infogain.csv",row.names = FALSE)

test_overbalanced_infogain=test[,c(features.infogain.overbalanced,"Class")]
write.csv(test_overbalanced_infogain,"test_overbalanced_infogain.csv",row.names = FALSE)
#CFS

overbalanced.cfs=cfs(Class~.,data=overbalanced)
overbalanced.cfs

overbalanced_cfs=overbalanced[,c(overbalanced.cfs,"Class")]
write.csv(overbalanced_cfs,"overbalanced_cfs.csv",row.names = FALSE)

test_overbalanced_cfs=test[,c(overbalanced.cfs,"Class")]
write.csv(test_overbalanced_cfs,"test_overbalanced_cfs.csv",row.names = FALSE)
#############
##Running classification algorithms on the 6 Datasets that we created

#Creating functions to calculate the performance measures for class Y class N and avg of both
# Performance measures For the Y class
class_Y_performance_measure=function(predicted,reference,pred_prob){
  cf=confusionMatrix(data = predicted, reference= reference,positive="Y")
  TP = as.numeric(cf$table[2, 2])
  FN = as.numeric(cf$table[1, 2])
  FP = as.numeric(cf$table[2, 1])
  TN = as.numeric(cf$table[1, 1])
  
  TPR = cf$byClass["Sensitivity"]
  
  FPR=1 - cf$byClass["Specificity"]
  
  Precision = cf$byClass["Precision"]
  
  Recall=cf$byClass["Recall"]
  
  F1=  cf$byClass["F1"]
  
  #roc
  actual_binary <- ifelse(test$Class == 'Y', 1, 0)
  roc_obj <- roc(actual_binary, pred_prob)
  auc=auc(roc_obj)
  
  MCC = (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  
  kappa=cf$overall["Kappa"]
  
  cat("TPR: ", TPR)
  cat(" FPR : ", FPR)
  cat(" Precision : ", Precision)
  cat(" Recall : ", Recall)
  cat(" Fscore : ", F1)
  cat(" ROC: ",auc)
  cat(" MCC : ", MCC)
  cat(" Kappa : ", kappa)
  
  return(list(TPR = TPR, FPR = FPR,Precision = Precision, Recall = Recall, Fscore = F1,ROC=auc ,MCC = MCC,  kappa = kappa))
  
}

# Performance measures For the N class
class_N_performance_measure=function(predicted,reference,pred_prob){
  cf=confusionMatrix(data = predicted, reference= reference,positive="N")
  TP = as.numeric(cf$table[1, 1])
  FN = as.numeric(cf$table[2, 1])
  FP = as.numeric(cf$table[1, 2])
  TN = as.numeric(cf$table[2, 2])
  
  TPR = cf$byClass["Sensitivity"]
  
  FPR=1 - cf$byClass["Specificity"]
  
  Precision = cf$byClass["Precision"]
  
  Recall=cf$byClass["Recall"]
  
  F1=  cf$byClass["F1"]
  
  #roc
  actual_binary <- ifelse(test$Class == 'N', 1, 0)
  roc_obj <- roc(actual_binary, pred_prob)
  auc=auc(roc_obj)
  
  
  MCC = (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  
  kappa=cf$overall["Kappa"]
  
  cat("TPR: ", TPR)
  cat(" FPR : ", FPR)
  cat(" Precision : ", Precision)
  cat(" Recall : ", Recall)
  cat(" Fscore : ", F1)
  cat(" ROC: ",auc)
  cat(" MCC : ", MCC)
  cat(" Kappa : ", kappa)
  return(list(TPR = TPR, FPR = FPR,Precision = Precision, Recall = Recall, Fscore = F1,ROC=auc ,MCC = MCC,  kappa = kappa))
  
}

#wt.avg of class Y and N
avg_YandN <- function(class_Y, class_N){
  TPR_avg = (class_Y$TPR + class_N$TPR) / 2
  FPR_avg = (class_Y$FPR + class_N$FPR) / 2
  Precision_avg = (class_Y$Precision + class_N$Precision) / 2
  Recall_avg = (class_Y$Recall + class_N$Recall) / 2
  Fscore_avg = (class_Y$Fscore + class_N$Fscore) / 2
  MCC_avg = (class_Y$MCC + class_N$MCC) / 2
  ROC_avg = (class_Y$ROC + class_N$ROC)/2
  Kappa_avg= (class_Y$kappa + class_N$kappa)/2
  cat("TPR Avg: ", TPR_avg)
  cat(" FPR Avg: ", FPR_avg)
  cat(" Precision Avg: ", Precision_avg)
  cat(" Recall Avg: ", Recall_avg)
  cat(" Fscore Avg: ", Fscore_avg)
  cat(" ROC: ",ROC_avg)
  cat(" MCC Avg: ", MCC_avg)
  cat(" Kappa Avg: ", Kappa_avg)
  return(list(TPR = TPR_avg, FPR = FPR_avg,Precision = Precision_avg, Recall = Recall_avg, Fscore = Fscore_avg,ROC=ROC_avg ,MCC = MCC_avg,  kappa = Kappa_avg))
}




#Factoring all the dataframes

#underbalanced
underbalanced_boruta$Class=factor(underbalanced_boruta$Class)
underbalanced_infogain$Class=factor(underbalanced_infogain$Class)
underbalanced_cfs$Class=factor(underbalanced_cfs$Class)

#overbalanced
overbalanced_boruta$Class=factor(overbalanced_boruta$Class)
overbalanced_infogain$Class=factor(overbalanced_infogain$Class)
overbalanced_cfs$Class=factor(overbalanced_cfs$Class)

#test
test_overbalanced_boruta$Class=factor(test_overbalanced_boruta$Class)
test_overbalanced_cfs$Class=factor(test_overbalanced_cfs$Class)
test_overbalanced_infogain$Class=factor(test_overbalanced_infogain$Class)

test_underbalanced_boruta$Class=factor(test_underbalanced_boruta$Class)
test_underbalanced_cfs$Class=factor(test_underbalanced_cfs$Class)
test_underbalanced_infogain$Class=factor(test_underbalanced_infogain$Class)
#######################
## Classification Algorithm 1 : Logistic regression
#MODEL 1 underbalanced_boruta

model1_lr=glm(Class ~.,data=underbalanced_boruta,family=binomial)
pred_model1_prob_lr=predict(model1_lr,newdata = test_underbalanced_boruta,type="response")
pred_model1_lr = factor(ifelse(pred_model1_prob_lr > 0.5, "Y", "N"))
cm_model1_lr=confusionMatrix(pred_model1_lr,test_underbalanced_boruta$Class,positive = "Y")

#printing performance measures for class Y
class_Y_model1_lr=class_Y_performance_measure(predicted = pred_model1_lr,reference = test_underbalanced_boruta$Class,pred_prob=pred_model1_prob_lr)
class_Y_model1_lr

#printing performance measure for class N
class_N_model1_lr=class_N_performance_measure(predicted = pred_model1_lr,reference = test_underbalanced_boruta$Class,pred_prob = pred_model1_prob_lr)
class_N_model1_lr

#avg for class Y and N
avg_model1_lr=avg_YandN(class_Y = class_Y_model1_lr,class_N = class_N_model1_lr)


#MODEL 2 underbalanced_cfs
model2_lr=glm(Class ~.,data=underbalanced_cfs,family=binomial)
pred_model2_prob_lr=predict(model2_lr,newdata = test_underbalanced_cfs,type="response")
pred_model2_lr = factor(ifelse(pred_model2_prob_lr > 0.5, "Y", "N"))
cm_model2_lr=confusionMatrix(pred_model2_lr,test_underbalanced_cfs$Class)

#printing performance measures for class Y
class_Y_model2_lr=class_Y_performance_measure(predicted = pred_model2_lr,reference = test_underbalanced_cfs$Class,pred_prob = pred_model2_prob_lr)
class_Y_model2_lr

#printing performance measure for class N
class_N_model2_lr=class_N_performance_measure(predicted = pred_model2_lr,reference = test_underbalanced_cfs$Class,pred_prob=pred_model2_prob_lr)
class_N_model2_lr

#avg for class Y and N
avg_model2_lr=avg_YandN(class_Y = class_Y_model2_lr,class_N = class_N_model2_lr)

#MODEL 3 underbalanced_infogain
model3_lr=glm(Class ~.,data=underbalanced_infogain,family=binomial)
pred_model3_prob_lr=predict(model3_lr,newdata = test_underbalanced_infogain,type="response")
pred_model3_lr = factor(ifelse(pred_model3_prob_lr > 0.5, "Y", "N"))
cm_model3_lr=confusionMatrix(pred_model3_lr,test_underbalanced_infogain$Class)

#printing performance measures for class Y
class_Y_model3_lr=class_Y_performance_measure(predicted = pred_model3_lr,reference = test_underbalanced_infogain$Class,pred_prob = pred_model3_prob_lr)
class_Y_model3_lr

#printing performance measure for class N
class_N_model3_lr=class_N_performance_measure(predicted = pred_model3_lr,reference = test_underbalanced_infogain$Class,pred_prob = pred_model3_prob_lr)
class_N_model3_lr

#avg for class Y and N
avg_model3_lr=avg_YandN(class_Y = class_Y_model3_lr,class_N = class_N_model3_lr)


#MODEL 4 overbalanced_boruta
model4_lr=glm(Class ~.,data=overbalanced_boruta,family=binomial)
pred_model4_prob_lr=predict(model4_lr,newdata = test_overbalanced_boruta,type="response")
pred_model4_lr = factor(ifelse(pred_model4_prob_lr > 0.5, "Y", "N"))
cm_model4_lr=confusionMatrix(pred_model4_lr,test_overbalanced_boruta$Class)

#printing performance measures for class Y
class_Y_model4_lr=class_Y_performance_measure(predicted = pred_model4_lr,reference = test_overbalanced_boruta$Class,pred_prob = pred_model4_prob_lr)
class_Y_model4_lr

#printing performance measure for class N
class_N_model4_lr=class_N_performance_measure(predicted = pred_model4_lr,reference = test_overbalanced_boruta$Class,pred_prob = pred_model4_prob_lr)
class_N_model4_lr

#avg for class Y and N
avg_model4_lr=avg_YandN(class_Y = class_Y_model4_lr,class_N = class_N_model4_lr)

#MODEL 5 overbalanced_cfs
model5_lr=glm(Class ~.,data=overbalanced_cfs,family=binomial)
pred_model5_prob_lr=predict(model5_lr,newdata = test_overbalanced_cfs,type="response")
pred_model5_lr = factor(ifelse(pred_model5_prob_lr > 0.5, "Y", "N"))
cm_model5_lr=confusionMatrix(pred_model5_lr,test_overbalanced_cfs$Class)

#printing performance measures for class Y
class_Y_model5_lr=class_Y_performance_measure(predicted = pred_model5_lr,reference = test_overbalanced_cfs$Class,pred_prob=pred_model5_prob_lr)
class_Y_model5_lr

#printing performance measure for class N
class_N_model5_lr=class_N_performance_measure(predicted = pred_model5_lr,reference = test_overbalanced_cfs$Class,pred_prob = pred_model5_prob_lr)
class_N_model5_lr

#avg for class Y and N
avg_model5_lr=avg_YandN(class_Y = class_Y_model5_lr,class_N = class_N_model5_lr)

#MODEL 6 overerbalanced_infogain
model6_lr=glm(Class ~.,data=overbalanced_infogain,family=binomial)
pred_model6_prob_lr=predict(model6_lr,newdata = test_overbalanced_infogain,type="response")
pred_model6_lr = factor(ifelse(pred_model6_prob_lr > 0.5, "Y", "N"))
cm_model6_lr=confusionMatrix(pred_model6_lr,test_overbalanced_infogain$Class)
#printing performance measures for class Y
class_Y_model6_lr=class_Y_performance_measure(predicted = pred_model6_lr,reference = test_overbalanced_infogain$Class,pred_prob = pred_model6_prob_lr)
class_Y_model6_lr

#printing performance measure for class N
class_N_model6_lr=class_N_performance_measure(predicted = pred_model6_lr,reference = test_overbalanced_infogain$Class,pred_prob = pred_model6_prob_lr)
class_N_model6_lr

#avg for class Y and N
avg_model6_lr=avg_YandN(class_Y = class_Y_model6_lr,class_N = class_N_model6_lr)


#############
##CLASSIFICATION 2 ; NAIVE BAYES


#train control and tune grid
trainControl_nb <- trainControl(method = "repeatedcv", number = 5,repeats = 5,summaryFunction = defaultSummary)
tuning_grid_nb <- expand.grid(laplace = c(0, 0.5, 1), usekernel = c(TRUE, FALSE), adjust = 1)



#MODEL 1 overbalanced_boruta
set.seed(1001)  # for reproducibility
model1_nb <- train(Class ~ ., data = overbalanced_boruta,
                   method = "naive_bayes",
                   trControl = trainControl_nb,
                   tuneGrid = tuning_grid_nb)
pred_model1_nb=predict(model1_nb,newdata = test_overbalanced_boruta)
cm_model1_nb=confusionMatrix(pred_model1_nb,test_overbalanced_boruta$Class)

#printing performance measures for class Y
class_Y_model1_nb=class_Y_performance_measure(predicted = pred_model1_nb,reference = test_overbalanced_boruta$Class,pred_prob=predict(model1_nb, newdata = test_overbalanced_boruta, type = "prob")$Y)
class_Y_model1_nb

#printing performance measure for class N
class_N_model1_nb=class_N_performance_measure(predicted = pred_model1_nb,reference = test_overbalanced_boruta$Class,pred_prob=predict(model1_nb, newdata = test_overbalanced_boruta, type = "prob")$N)
class_N_model1_nb

#avg for class Y and N
avg_model1_nb=avg_YandN(class_Y = class_Y_model1_nb,class_N = class_N_model1_nb)

#MODEL 2 overbalanced_cfs
set.seed(1001)  # for reproducibility
model2_nb <- train(Class ~ ., data = overbalanced_cfs,
                   method = "naive_bayes",
                   trControl = trainControl_nb,
                   tuneGrid = tuning_grid_nb)
pred_model2_nb=predict(model2_nb,newdata = test_overbalanced_cfs)
cm_model2_nb=confusionMatrix(pred_model2_nb,test_overbalanced_cfs$Class)

#printing performance measures for class Y
class_Y_model2_nb=class_Y_performance_measure(predicted = pred_model2_nb,reference = test_overbalanced_cfs$Class,pred_prob=predict(model2_nb, newdata = test_overbalanced_cfs, type = "prob")$Y)
class_Y_model2_nb

#printing performance measure for class N
class_N_model2_nb=class_N_performance_measure(predicted = pred_model2_nb,reference = test_overbalanced_cfs$Class,pred_prob=predict(model2_nb, newdata = test_overbalanced_cfs, type = "prob")$N)
class_N_model2_nb

#avg for class Y and N
avg_model2_nb=avg_YandN(class_Y = class_Y_model2_nb,class_N = class_N_model2_nb)

#MODEL 3 overbalanced_infogain
set.seed(1001)  # for reproducibility
model3_nb <- train(Class ~ ., data = overbalanced_infogain,
                   method = "naive_bayes",
                   trControl = trainControl_nb,
                   tuneGrid = tuning_grid_nb)
pred_model3_nb=predict(model3_nb,newdata = test_overbalanced_infogain)
cm_model3_nb=confusionMatrix(pred_model3_nb,test_overbalanced_infogain$Class)

#printing performance measures for class Y
class_Y_model3_nb=class_Y_performance_measure(predicted = pred_model3_nb,reference = test_overbalanced_infogain$Class,pred_prob=predict(model3_nb, newdata = test_overbalanced_infogain, type = "prob")$Y)
class_Y_model3_nb

#printing performance measure for class N
class_N_model3_nb=class_N_performance_measure(predicted = pred_model3_nb,reference = test_overbalanced_infogain$Class,pred_prob=predict(model3_nb, newdata = test_overbalanced_infogain, type = "prob")$N)
class_N_model3_nb

#avg for class Y and N
avg_model3_nb=avg_YandN(class_Y = class_Y_model3_nb,class_N = class_N_model3_nb)

#MODEL 4 underbalanced_boruta
set.seed(1001)  # for reproducibility
model4_nb <- train(Class ~ ., data = underbalanced_boruta,
                   method = "naive_bayes",
                   trControl = trainControl_nb,
                   tuneGrid = tuning_grid_nb)
pred_model4_nb=predict(model4_nb,newdata = test_underbalanced_boruta)
cm_model4_nb=confusionMatrix(pred_model4_nb,test_underbalanced_boruta$Class)

#printing performance measures for class Y
class_Y_model4_nb=class_Y_performance_measure(predicted = pred_model4_nb,reference = test_underbalanced_boruta$Class,pred_prob=predict(model4_nb, newdata = test_underbalanced_boruta, type = "prob")$Y)
class_Y_model4_nb

#printing performance measure for class N
class_N_model4_nb=class_N_performance_measure(predicted = pred_model4_nb,reference = test_underbalanced_boruta$Class,pred_prob=predict(model4_nb, newdata = test_underbalanced_boruta, type = "prob")$N)
class_N_model4_nb

#avg for class Y and N
avg_model4_nb=avg_YandN(class_Y = class_Y_model4_nb,class_N = class_N_model4_nb)


#MODEL 5 underbalanced_cfs
set.seed(1001)  # for reproducibility
model5_nb <- train(Class ~ ., data = underbalanced_cfs,
                   method = "naive_bayes",
                   trControl = trainControl_nb,
                   tuneGrid = tuning_grid_nb)
pred_model5_nb=predict(model5_nb,newdata = test_underbalanced_cfs)
cm_model5_nb=confusionMatrix(pred_model5_nb,test_underbalanced_cfs$Class)

#printing performance measures for class Y
class_Y_model5_nb=class_Y_performance_measure(predicted = pred_model5_nb,reference = test_underbalanced_cfs$Class,pred_prob=predict(model5_nb, newdata = test_underbalanced_cfs, type = "prob")$Y)
class_Y_model5_nb

#printing performance measure for class N
class_N_model5_nb=class_N_performance_measure(predicted = pred_model5_nb,reference = test_underbalanced_cfs$Class,pred_prob=predict(model5_nb, newdata = test_underbalanced_cfs, type = "prob")$N)
class_N_model5_nb

#avg for class Y and N
avg_model5_nb=avg_YandN(class_Y = class_Y_model5_nb,class_N = class_N_model5_nb)


#MODEL 6 underbalanced_infogain
set.seed(1001)  # for reproducibility
model6_nb <- train(Class ~ ., data = underbalanced_infogain,
                   method = "naive_bayes",
                   trControl = trainControl_nb,
                   tuneGrid = tuning_grid_nb)
pred_model6_nb=predict(model6_nb,newdata = test_underbalanced_infogain)
cm_model6_nb=confusionMatrix(pred_model6_nb,test_underbalanced_infogain$Class)

#printing performance measures for class Y
class_Y_model6_nb=class_Y_performance_measure(predicted = pred_model6_nb,reference = test_underbalanced_infogain$Class,pred_prob=predict(model6_nb, newdata = test_underbalanced_infogain, type = "prob")$Y)
class_Y_model6_nb

#printing performance measure for class N
class_N_model6_nb=class_N_performance_measure(predicted = pred_model6_nb,reference = test_underbalanced_infogain$Class,pred_prob=predict(model6_nb, newdata = test_underbalanced_infogain, type = "prob")$N)
class_N_model6_nb

#avg for class Y and N
avg_model6_nb=avg_YandN(class_Y = class_Y_model6_nb,class_N = class_N_model6_nb)


##############
##MODEL 3 RANDOM FOREST
# Load necessary libraries


# Register the parallel backend to use
# Detect the number of cores on your machine
numCores <- detectCores()

# Register the doParallel backend to use multiple cores (leave one core free for system processes)
registerDoParallel(cores = numCores - 1)

#Random Forest



ctrl_rf = trainControl(method = "CV",classProbs = TRUE,summaryFunction = twoClassSummary,savePredictions = TRUE) 

#MODEL 1 overbalanced_boruta
mtryValues_model1_rf = seq(2, ncol(overbalanced_boruta)-1, by = 1)
model1_rf = train(x = select(overbalanced_boruta, -Class), y = overbalanced_boruta$Class, method = "rf",
                  ntree = 250, tuneGrid = data.frame(mtry = mtryValues_model1_rf), importance = TRUE, metric = "ROC", trControl = ctrl_rf)

pred_model1_rf=predict(model1_rf,newdata = test_overbalanced_boruta)
cm_model1_rf=confusionMatrix(pred_model1_rf,test_overbalanced_boruta$Class)

#printing performance measures for class Y
class_Y_model1_rf=class_Y_performance_measure(predicted = pred_model1_rf,reference = test_overbalanced_boruta$Class,pred_prob=predict(model1_rf, newdata = test_overbalanced_boruta, type = "prob")$Y)
class_Y_model1_rf

#printing performance measure for class N
class_N_model1_rf=class_N_performance_measure(predicted = pred_model1_rf,reference = test_overbalanced_boruta$Class,pred_prob=predict(model1_rf, newdata = test_overbalanced_boruta, type = "prob")$N)
class_N_model1_rf

#avg for class Y and N
avg_model1_rf=avg_YandN(class_Y = class_Y_model1_rf,class_N = class_N_model1_rf)

#MODEL 2 overbalanced_cfs
mtryValues_model2_rf = seq(2, ncol(overbalanced_cfs)-1, by = 1)
model2_rf = train(x = select(overbalanced_cfs, -Class), y = overbalanced_cfs$Class, method = "rf",
                  ntree = 250, tuneGrid = data.frame(mtry = mtryValues_model2_rf), importance = TRUE, metric = "ROC", trControl = ctrl_rf)

pred_model2_rf=predict(model2_rf,newdata = test_overbalanced_cfs)
cm_model2_rf=confusionMatrix(pred_model2_rf,test_overbalanced_cfs$Class)

#printing performance measures for class Y
class_Y_model2_rf=class_Y_performance_measure(predicted = pred_model2_rf,reference = test_overbalanced_cfs$Class,pred_prob=predict(model2_rf, newdata = test_overbalanced_cfs, type = "prob")$Y)
class_Y_model2_rf

#printing performance measure for class N
class_N_model2_rf=class_N_performance_measure(predicted = pred_model2_rf,reference = test_overbalanced_cfs$Class,pred_prob=predict(model2_rf, newdata = test_overbalanced_cfs, type = "prob")$N)
class_N_model2_rf

#avg for class Y and N
avg_model2_rf=avg_YandN(class_Y = class_Y_model2_rf,class_N = class_N_model2_rf)



#MODEL 3 overbalanced_infogain
mtryValues_model3_rf = seq(2, ncol(overbalanced_infogain)-1, by = 1)
model3_rf = train(x = select(overbalanced_infogain, -Class), y = overbalanced_infogain$Class, method = "rf",
                  ntree =250, tuneGrid = data.frame(mtry = mtryValues_model3_rf), importance = TRUE, metric = "ROC", trControl = ctrl_rf)
pred_model3_rf=predict(model3_rf,newdata = test_overbalanced_infogain)
cm_model3_rf=confusionMatrix(pred_model3_rf,test_overbalanced_infogain$Class)

#printing performance measures for class Y
class_Y_model3_rf=class_Y_performance_measure(predicted = pred_model3_rf,reference = test_overbalanced_infogain$Class,pred_prob=predict(model3_rf, newdata = test_overbalanced_infogain, type = "prob")$Y)
class_Y_model3_rf

#printing performance measure for class N
class_N_model3_rf=class_N_performance_measure(predicted = pred_model3_rf,reference = test_overbalanced_infogain$Class,pred_prob=predict(model3_rf, newdata = test_overbalanced_infogain, type = "prob")$N)
class_N_model3_rf

#avg for class Y and N
avg_model3_rf=avg_YandN(class_Y = class_Y_model3_rf,class_N = class_N_model3_rf)

#MODEL4 underbalanced_boruta
mtryValues_model4_rf = seq(2, ncol(underbalanced_boruta)-1, by = 1)
model4_rf = train(x = select(underbalanced_boruta, -Class), y = underbalanced_boruta$Class, method = "rf",
                  ntree = 250, tuneGrid = data.frame(mtry = mtryValues_model4_rf), importance = TRUE, metric = "ROC", trControl = ctrl_rf)
pred_model4_rf=predict(model4_rf,newdata = test_underbalanced_boruta)
cm_model4_rf=confusionMatrix(pred_model4_rf,test_underbalanced_boruta$Class)

#printing performance measures for class Y
class_Y_model4_rf=class_Y_performance_measure(predicted = pred_model4_rf,reference = test_underbalanced_boruta$Class,pred_prob=predict(model4_rf, newdata = test_underbalanced_boruta, type = "prob")$Y)
class_Y_model4_rf

#printing performance measure for class N
class_N_model4_rf=class_N_performance_measure(predicted = pred_model4_rf,reference = test_underbalanced_boruta$Class,pred_prob=predict(model4_rf, newdata = test_underbalanced_boruta, type = "prob")$N)
class_N_model4_rf

#avg for class Y and N
avg_model4_rf=avg_YandN(class_Y = class_Y_model4_rf,class_N = class_N_model4_rf)



#MODEL5 underbalanced_cfs
mtryValues_model5_rf = seq(2, ncol(underbalanced_cfs)-1, by = 1)
model5_rf = train(x = select(underbalanced_cfs, -Class), y = underbalanced_cfs$Class, method = "rf",
                  ntree = 250, tuneGrid = data.frame(mtry = mtryValues_model5_rf), importance = TRUE, metric = "ROC", trControl = ctrl_rf)
pred_model5_rf=predict(model5_rf,newdata = test_underbalanced_cfs)
cm_model5_rf=confusionMatrix(pred_model5_rf,test_underbalanced_cfs$Class)
#printing performance measures for class Y
class_Y_model5_rf=class_Y_performance_measure(predicted = pred_model5_rf,reference = test_underbalanced_cfs$Class,pred_prob=predict(model5_rf, newdata = test_underbalanced_cfs, type = "prob")$Y)
class_Y_model5_rf

#printing performance measure for class N
class_N_model5_rf=class_N_performance_measure(predicted = pred_model5_rf,reference = test_underbalanced_cfs$Class,pred_prob=predict(model5_rf, newdata = test_underbalanced_cfs, type = "prob")$N)
class_N_model5_rf

#avg for class Y and N
avg_model5_rf=avg_YandN(class_Y = class_Y_model5_rf,class_N = class_N_model5_rf)


#MODEL6 underbalanced_infogain
mtryValues_model6_rf = seq(2, ncol(underbalanced_infogain)-1, by = 1)
model6_rf = train(x = select(underbalanced_infogain, -Class), y = underbalanced_cfs$Class, method = "rf",
                  ntree = 250, tuneGrid = data.frame(mtry = mtryValues_model6_rf), importance = TRUE, metric = "ROC", trControl = ctrl_rf)
pred_model6_rf=predict(model6_rf,newdata = test_underbalanced_infogain)
cm_model6_rf=confusionMatrix(pred_model6_rf,test_underbalanced_infogain$Class)

#printing performance measures for class Y
class_Y_model6_rf=class_Y_performance_measure(predicted = pred_model6_rf,reference = test_underbalanced_infogain$Class,pred_prob=predict(model6_rf, newdata = test_underbalanced_infogain, type = "prob")$Y)
class_Y_model6_rf

#printing performance measure for class N
class_N_model6_rf=class_N_performance_measure(predicted = pred_model6_rf,reference = test_underbalanced_infogain$Class,pred_prob=predict(model6_rf, newdata = test_underbalanced_infogain, type = "prob")$N)
class_N_model6_rf

#avg for class Y and N
avg_model6_rf=avg_YandN(class_Y = class_Y_model6_rf,class_N = class_N_model6_rf)

stopImplicitCluster()


#############
#Classification Algorithm 4 : XGB
#Train Control 
xgb_control <- caret::trainControl(
  method = "cv",
  number = 3,
  verboseIter = TRUE,
  returnData = FALSE,
  summaryFunction = twoClassSummary,  # For binary classification
  classProbs = TRUE  # Required for AUC, specificity, sensitivity etc.
)

xgbGrid <- expand.grid(
  nrounds = c(100, 150),  # Reduced number of rounds
  max_depth = c(3, 6),    # Fewer depth values
  eta = c(0.1, 0.3),      # Fewer learning rate values
  gamma = c(0, 1),        # Fewer gamma values
  colsample_bytree = c(0.5, 0.8),  # Fewer values for colsample_bytree
  min_child_weight = c(1, 3),      # Fewer values for min_child_weight
  subsample = c(0.5, 0.8)
)

#Model 1 overbalanced_boruta

set.seed(123)
xgbModel1 <- caret::train(
  x = overbalanced_boruta[, setdiff(names(overbalanced_boruta), "Class")], 
  y = overbalanced_boruta$Class,
  method = "xgbTree",
  tuneGrid = xgbGrid,
  metric = "ROC",
  trControl = xgb_control
)

plot(xgbModel1)

pred_model1_xgb=predict(xgbModel1, newdata = test_overbalanced_boruta)
cm_model1_xgb=confusionMatrix(pred_model1_xgb,test_overbalanced_boruta$Class,positive = 'Y')


prob_model1_xgb=predict(xgbModel1, newdata = test_overbalanced_boruta, type = "prob")

#printing performance measures for class Y
class_Y_model1_xgb=class_Y_performance_measure(predicted = pred_model1_xgb,reference = test_overbalanced_boruta$Class, pred_prob= prob_model1_xgb$Y)
class_Y_model1_xgb

#printing performance measure for class N
class_N_model1_xgb=class_N_performance_measure(predicted = pred_model1_xgb,reference = test_overbalanced_boruta$Class, pred_prob= prob_model1_xgb$N)
class_N_model1_xgb

#avg for class Y and N
avg_model1_xgb=avg_YandN(class_Y = class_Y_model1_xgb,class_N = class_N_model1_xgb)

#Model 2 overbalanced_infogain
x_model2_xgb <- overbalanced_infogain[, !(names(overbalanced_infogain) %in% "Class")]
y_model2_xgb <- overbalanced_infogain$Class
set.seed(123)
xgbModel2 <- caret::train(
  x = x_model2_xgb, 
  y = y_model2_xgb,
  method = "xgbTree",
  tuneGrid = xgbGrid,
  metric = "ROC",
  trControl = xgb_control
)

plot(xgbModel2)

pred_model2_xgb=predict(xgbModel2, newdata = test_overbalanced_infogain)
cm_model2_xgb=confusionMatrix(pred_model2_xgb,test_overbalanced_infogain$Class,positive = 'Y')


prob_model2_xgb=predict(xgbModel2, newdata = test_overbalanced_infogain, type = "prob")

#printing performance measures for class Y
class_Y_model2_xgb=class_Y_performance_measure(predicted = pred_model2_xgb,reference = test_overbalanced_infogain$Class,pred_prob = prob_model2_xgb$Y)
class_Y_model2_xgb

#printing performance measure for class N
class_N_model2_xgb=class_N_performance_measure(predicted = pred_model2_xgb,reference = test_overbalanced_infogain$Class,pred_prob = prob_model2_xgb$N)
class_N_model2_xgb

#avg for class Y and N
avg_model2_xgb=avg_YandN(class_Y = class_Y_model2_xgb,class_N = class_N_model2_xgb)

#Model 3 overbalanced_cfs

set.seed(123)
xgbModel3 <- train(
  x = overbalanced_cfs[, !(names(overbalanced_cfs) %in% "Class")], 
  y = overbalanced_cfs$Class,
  method = "xgbTree",
  tuneGrid = xgbGrid,
  metric = "ROC",
  trControl = xgb_control
)
plot(xgbModel3)

pred_model3_xgb=predict(xgbModel3, newdata = test_overbalanced_cfs)
cm_model3_xgb=confusionMatrix(pred_model3_xgb,test_overbalanced_cfs$Class,positive = 'Y')

prob_model3_xgb=predict(xgbModel3, newdata = test_overbalanced_cfs, type = "prob")

#printing performance measures for class Y
class_Y_model3_xgb=class_Y_performance_measure(predicted = pred_model3_xgb,reference = test_overbalanced_cfs$Class,pred_prob = prob_model3_xgb$Y)
class_Y_model3_xgb

#printing performance measure for class N
class_N_model3_xgb=class_N_performance_measure(predicted = pred_model3_xgb,reference = test_overbalanced_cfs$Class,pred_prob= prob_model3_xgb$N)
class_N_model3_xgb

#avg for class Y and N
avg_model2_xgb=avg_YandN(class_Y = class_Y_model3_xgb,class_N = class_N_model3_xgb)

#Model 4 underbalanced_boruta

set.seed(123)
xgbModel4 <- train(
  x = underbalanced_boruta[, !(names(underbalanced_boruta) %in% "Class")], 
  y = underbalanced_boruta$Class,
  method = "xgbTree",
  tuneGrid = xgbGrid,
  metric = "ROC",
  trControl = xgb_control
)
plot(xgbModel4)


pred_model4_xgb=predict(xgbModel4, newdata = test_underbalanced_boruta)
cm_model4_xgb=confusionMatrix(pred_model4_xgb,test_underbalanced_boruta$Class,positive = 'Y')

prob_model4_xgb=predict(xgbModel4, newdata = test_underbalanced_boruta, type = "prob")

#printing performance measures for class Y
class_Y_model4_xgb=class_Y_performance_measure(predicted = pred_model4_xgb,reference = test_underbalanced_boruta$Class,pred_prob = prob_model4_xgb$Y)
class_Y_model4_xgb

#printing performance measure for class N
class_N_model4_xgb=class_N_performance_measure(predicted = pred_model4_xgb,reference = test_underbalanced_boruta$Class,pred_prob = prob_model4_xgb$N)
class_N_model4_xgb

#avg for class Y and N
avg_model4_xgb=avg_YandN(class_Y = class_Y_model4_xgb,class_N = class_N_model4_xgb)

#Model 5 underbalanced_infogain

set.seed(123)
xgbModel5 <- train(
  x = underbalanced_infogain[, !(names(underbalanced_infogain) %in% "Class")], 
  y = underbalanced_infogain$Class,
  method = "xgbTree",
  tuneGrid = xgbGrid,
  metric = "ROC",
  trControl = xgb_control
)
plot(xgbModel5)

pred_model5_xgb=predict(xgbModel5, newdata = test_underbalanced_infogain)

cm_model5_xgb=confusionMatrix(pred_model5_xgb,test_underbalanced_infogain$Class,positive = 'Y')

prob_model5_xgb=predict(xgbModel5, newdata = test_underbalanced_infogain, type = "prob")

#printing performance measures for class Y
class_Y_model5_xgb=class_Y_performance_measure(predicted = pred_model5_xgb,reference = test_underbalanced_infogain$Class,pred_prob = prob_model5_xgb$Y)
class_Y_model5_xgb

#printing performance measure for class N
class_N_model5_xgb=class_N_performance_measure(predicted = pred_model5_xgb,reference = test_underbalanced_infogain$Class,pred_prob = prob_model5_xgb$N)
class_N_model5_xgb

#avg for class Y and N
avg_model5_xgb=avg_YandN(class_Y = class_Y_model5_xgb,class_N = class_N_model5_xgb)

#Model 6 underbalanced_cfs

set.seed(123)
xgbModel6 <- caret::train(
  x = underbalanced_cfs[, !(names(underbalanced_cfs) %in% "Class")], 
  y = underbalanced_cfs$Class,
  method = "xgbTree",
  tuneGrid = xgbGrid,
  metric = "ROC",
  trControl = xgb_control
)

plot(xgbModel6)

pred_model6_xgb=predict(xgbModel6, newdata = test_underbalanced_cfs)
cm_model6_xgb=confusionMatrix(pred_model6_xgb,test_underbalanced_cfs$Class,positive = 'Y')

prob_model6_xgb=predict(xgbModel6, newdata = test_underbalanced_cfs, type = "prob")

#printing performance measures for class Y
class_Y_model6_xgb=class_Y_performance_measure(predicted = pred_model6_xgb,reference = test_underbalanced_cfs$Class,pred_prob = prob_model6_xgb$Y)
class_Y_model6_xgb

#printing performance measure for class N
class_N_model6_xgb=class_N_performance_measure(predicted = pred_model6_xgb,reference = test_underbalanced_cfs$Class,pred_prob = prob_model6_xgb$N)
class_N_model6_xgb

#avg for class Y and N
avg_model6_xgb=avg_YandN(class_Y = class_Y_model6_xgb,class_N = class_N_model6_xgb)



#############
#Classification MODEL 5 : SVM


#Model 1 Overbalanced boruta

set.seed(31)
model1_svm <- svm(Class ~ ., data = overbalanced_boruta, method = "C-classification",
                    kernel="radial",
                     probability= TRUE)





pred_model1_svm <- predict(model1_svm, newdata = test_overbalanced_boruta)

cm_model1_svm <- table(pred_model1_svm, test_overbalanced_boruta$Class)

prob_model1_svm = attr(predict(model1_svm, newdata = test_overbalanced_boruta,probability = TRUE), "probabilities")

#printing performance measures for class Y
class_Y_model1_svm=class_Y_performance_measure(predicted = pred_model1_svm,reference = test_overbalanced_boruta$Class,pred_prob=prob_model1_svm[,1])
class_Y_model1_svm

#printing performance measure for class N
class_N_model1_svm=class_N_performance_measure(predicted = pred_model1_svm,reference = test_overbalanced_boruta$Class,pred_prob=prob_model1_svm[,2])
class_N_model1_svm

#avg for class Y and N
avg_model1_svm=avg_YandN(class_Y = class_Y_model1_svm,class_N = class_N_model1_svm)


#Model 2 overbalanced infogain

set.seed(31)
model2_svm <- svm(Class ~ ., data = overbalanced_infogain, method = "C-classification",kernel="linear",
                     probability=TRUE)



pred_model2_svm <- predict(model2_svm, newdata = test)

cm_model2_svm <- table(pred_model2_svm, test_overbalanced_infogain$Class)

prob_model2_svm = attr(predict(model2_svm, newdata = test_overbalanced_infogain, probability = TRUE),"probabilities")


#printing performance measures for class Y
class_Y_model2_svm=class_Y_performance_measure(predicted = pred_model2_svm,reference = test_overbalanced_infogain$Class,pred_prob=prob_model2_svm[,1])
class_Y_model2_svm

#printing performance measure for class N
class_N_model2_svm=class_N_performance_measure(predicted = pred_model2_svm,reference = test_overbalanced_infogain$Class,pred_prob=prob_model2_svm[,2])
class_N_model2_svm

#avg for class Y and N
avg_model2_svm=avg_YandN(class_Y = class_Y_model2_svm,class_N = class_N_model2_svm)


#Model 3 overbalanced cfs
set.seed(31)
model3_svm <- svm(Class ~ ., data = overbalanced_cfs, method = "C-classification",
                    kernel="linear",
                     probability=TRUE)



pred_model3_svm <- predict(model3_svm, newdata = test_overbalanced_cfs)

cm_model3_svm <- table(pred_model3_svm, test_overbalanced_cfs$Class)

prob_model3_svm = attr(predict(model3_svm, newdata = test_overbalanced_cfs, probability = TRUE),"probabilities")


#printing performance measures for class Y
class_Y_model3_svm=class_Y_performance_measure(predicted = pred_model3_svm,reference = test_overbalanced_cfs$Class,pred_prob=prob_model3_svm[,1])
class_Y_model3_svm

#printing performance measure for class N
class_N_model3_svm=class_N_performance_measure(predicted = pred_model3_svm,reference = test_overbalanced_cfs$Class,pred_prob=prob_model3_svm[,2])
class_N_model3_svm

#avg for class Y and N
avg_model3_svm=avg_YandN(class_Y = class_Y_model3_svm,class_N = class_N_model3_svm)


#Model4 underbalanced boruta
set.seed(31)
model4_svm <- svm(Class ~ ., data = underbalanced_boruta, method = "C-Classification",
                    kernel="linear",
                     probability=TRUE)



pred_model4_svm <- predict(model4_svm, newdata = test_underbalanced_boruta)

cm_model4_svm <- table(pred_model4_svm, test_underbalanced_boruta$Class)

prob_model4_svm = attr(predict(model4_svm, newdata = test_underbalanced_boruta, probability = TRUE),"probabilities")


#printing performance measures for class Y
class_Y_model4_svm=class_Y_performance_measure(predicted = pred_model4_svm,reference = test_underbalanced_boruta$Class,pred_prob=prob_model4_svm[,1])
class_Y_model4_svm

#printing performance measure for class N
class_N_model4_svm=class_N_performance_measure(predicted = pred_model4_svm,reference = test_underbalanced_boruta$Class,pred_prob=prob_model4_svm[,2])
class_N_model4_svm

#avg for class Y and N
avg_model4_svm=avg_YandN(class_Y = class_Y_model4_svm,class_N = class_N_model4_svm)


#Model5 Underbalanced infogain
set.seed(31)
model5_svm <- svm(Class ~ ., data = underbalanced_infogain, method = "C-classification",
                    kernel="linear",
                    probability=TRUE)





pred_model5_svm <- predict(model5_svm, newdata = test_underbalanced_infogain)

cm_model5_svm <- table(pred_model5_svm, test_underbalanced_infogain$Class)

prob_model5_svm = attr(predict(model5_svm, newdata = test_underbalanced_infogain, probability = TRUE),"probabilities")


#printing performance measures for class Y
class_Y_model5_svm=class_Y_performance_measure(predicted = pred_model5_svm,reference = test_underbalanced_infogain$Class,pred_prob=prob_model5_svm[,1])
class_Y_model5_svm

#printing performance measure for class N
class_N_model5_svm=class_N_performance_measure(predicted = pred_model5_svm,reference = test_underbalanced_infogain$Class,pred_prob=prob_model5_svm[,2])
class_N_model5_svm

#avg for class Y and N
avg_model5_nb=avg_YandN(class_Y = class_Y_model5_svm,class_N = class_N_model5_svm)


#Model6 underbalanced cfs

set.seed(31)
model6_svm <- svm(Class ~ ., data = underbalanced_cfs, method = "C-classification",
                    kernel="linear",
                    probability=TRUE)





pred_model6_svm <- predict(model6_svm, newdata = test_underbalanced_cfs)

cm_model6_svm <- table(pred_model6_svm, test_underbalanced_cfs$Class)

prob_model6_svm = attr(predict(model6_svm, newdata = test_underbalanced_cfs, probability=TRUE),"probabilities")


#printing performance measures for class Y
class_Y_model6_svm=class_Y_performance_measure(predicted = pred_model6_svm,reference = test_underbalanced_cfs$Class,pred_prob=prob_model6_svm[,1])
class_Y_model6_svm

#printing performance measure for class N
class_N_model6_svm=class_N_performance_measure(predicted = pred_model6_svm,reference = test_underbalanced_cfs$Class,pred_prob=prob_model6_svm[,2])
class_N_model6_svm

#avg for class Y and N
avg_model6_svm=avg_YandN(class_Y = class_Y_model6_svm,class_N = class_N_model6_svm)

##################
# Classification Algorithm 6: K-Nearest Neighbors

#Train control and tune grid functions
train_control <- trainControl(method = "cv", number = 5, summaryFunction = defaultSummary)
knnGrid <-  expand.grid(kmax = seq(1, 20, 2), # Example: trying k from 1 to 20
                        distance =c(1,2), # 0 for Euclidean, 1 for Manhattan
                        kernel = "optimal")

#MODEL 1 overbalanced_boruta
set.seed(31)
model1_knn <- train(Class ~., data = overbalanced_boruta, method = "kknn",
                    trControl=train_control,
                    preProcess = c("center", "scale"),
                    tuneGrid = knnGrid)

pred_model1_knn=predict(model1_knn, newdata = test_overbalanced_boruta)




cm_model1_knn=confusionMatrix(pred_model1_knn,test_overbalanced_boruta$Class,positive = 'Y')
#Plot model
plot(model1_knn)

#printing performance measures for class Y
class_Y_model1_knn=class_Y_performance_measure(predicted = pred_model1_knn,reference = test_overbalanced_boruta$Class,pred_prob=predict(model1_knn, newdata = test_overbalanced_boruta, type = "prob")$Y)
class_Y_model1_knn

#printing performance measure for class N
class_N_model1_knn=class_N_performance_measure(predicted = pred_model1_knn,reference = test_overbalanced_boruta$Class,pred_prob=predict(model1_knn, newdata = test_overbalanced_boruta, type = "prob")$N)
class_N_model1_knn

#avg for class Y and N
avg_model1_knn=avg_YandN(class_Y = class_Y_model1_knn,class_N = class_N_model1_knn)

#MODEL 2 overbalanced_infogain


set.seed(31)
model2_knn <- train(Class ~., data = overbalanced_infogain, method = "kknn",
                    trControl=train_control,
                    preProcess = c("center", "scale"),
                    tuneGrid = knnGrid)

pred_model2_knn=predict(model2_knn, newdata = test_overbalanced_infogain)


cm_model2_knn=confusionMatrix(pred_model2_knn,test_overbalanced_infogain$Class,positive = 'Y')

#Plot model
plot(model2_knn)
#printing performance measures for class Y
class_Y_model2_knn=class_Y_performance_measure(predicted = pred_model2_knn,reference = test_overbalanced_infogain$Class,pred_prob=predict(model2_knn, newdata = test_overbalanced_infogain, type = "prob")$Y)
class_Y_model2_knn

#printing performance measure for class N
class_N_model2_knn=class_N_performance_measure(predicted = pred_model2_knn,reference = test_overbalanced_infogain$Class,pred_prob=predict(model2_knn, newdata = test_overbalanced_infogain, type = "prob")$N)
class_N_model2_knn

#avg for class Y and N
avg_model2_knn=avg_YandN(class_Y = class_Y_model2_knn,class_N = class_N_model2_knn)

#MODEL 3 overbalanced_cfs

set.seed(31)
model3_knn <- train(Class ~., data = overbalanced_cfs, method = "kknn",
                    trControl=train_control,
                    preProcess = c("center", "scale"),
                    tuneGrid = knnGrid)


pred_model3_knn=predict(model3_knn, newdata = test_overbalanced_cfs)

cm_model3_knn=confusionMatrix(pred_model3_knn,test_overbalanced_cfs$Class,positive = 'Y')
#Plot model
plot(model3_knn)

#printing performance measures for class Y
class_Y_model3_knn=class_Y_performance_measure(predicted = pred_model3_knn,reference = test_overbalanced_cfs$Class,pred_prob=predict(model3_knn, newdata = test_overbalanced_cfs, type = "prob")$Y)
class_Y_model3_knn

#printing performance measure for class N
class_N_model3_knn=class_N_performance_measure(predicted = pred_model3_knn,reference = test_overbalanced_cfs$Class,pred_prob=predict(model3_knn, newdata = test_overbalanced_cfs, type = "prob")$N)
class_N_model3_knn

#avg for class Y and N
avg_model3_knn=avg_YandN(class_Y = class_Y_model3_knn,class_N = class_N_model3_knn)

#MODEL 4 underbalanced_boruta

set.seed(31)
model4_knn <- train(Class ~., data = underbalanced_boruta, method = "kknn",
                    trControl=train_control,
                    preProcess = c("center", "scale"),
                    tuneGrid = knnGrid)


pred_model4_knn=predict(model4_knn, newdata = test_underbalanced_boruta)

cm_model4_knn=confusionMatrix(pred_model4_knn,test_underbalanced_boruta$Class,positive = 'Y')
#Plot model
plot(model4_knn)

#printing performance measures for class Y
class_Y_model4_knn=class_Y_performance_measure(predicted = pred_model4_knn,reference = test_underbalanced_boruta$Class,pred_prob=predict(model4_knn, newdata = test_underbalanced_boruta, type = "prob")$Y)
class_Y_model4_knn

#printing performance measure for class N
class_N_model4_knn=class_N_performance_measure(predicted = pred_model4_knn,reference = test_underbalanced_boruta$Class,pred_prob=predict(model4_knn, newdata = test_underbalanced_boruta, type = "prob")$N)
class_N_model4_knn

#avg for class Y and N
avg_model4_knn=avg_YandN(class_Y = class_Y_model4_knn,class_N = class_N_model4_knn)


#MODEL 5 underbalanced_infogain

set.seed(31)
model5_knn <- train(Class ~., data = underbalanced_infogain, method = "kknn",
                    trControl=train_control,
                    preProcess = c("center", "scale"),
                    tuneGrid = knnGrid)

pred_model5_knn=predict(model5_knn, newdata = test_underbalanced_infogain)


cm_model5_knn=confusionMatrix(pred_model5_knn,test_underbalanced_infogain$Class,positive = 'Y')
#Plot model
plot(model5_knn)

#printing performance measures for class Y
class_Y_model5_knn=class_Y_performance_measure(predicted = pred_model5_knn,reference = test_underbalanced_infogain$Class,pred_prob=predict(model5_knn, newdata = test_underbalanced_infogain, type = "prob")$Y)
class_Y_model5_knn

#printing performance measure for class N
class_N_model5_knn=class_N_performance_measure(predicted = pred_model5_knn,reference = test_underbalanced_infogain$Class,pred_prob=predict(model5_knn, newdata = test_underbalanced_infogain, type = "prob")$N)
class_N_model5_knn

#avg for class Y and N
avg_model5_knn=avg_YandN(class_Y = class_Y_model5_knn,class_N = class_N_model5_knn)


#MODEL 6 underbalanced_cfs

set.seed(31)
model6_knn <- train(Class ~., data = underbalanced_cfs, method = "kknn",
                    trControl=train_control,
                    preProcess = c("center", "scale"),
                    tuneGrid = knnGrid)


pred_model6_knn=predict(model6_knn, newdata = test_underbalanced_cfs)

cm_model6_knn=confusionMatrix(pred_model6_knn,test_underbalanced_cfs$Class,positive = 'Y')
#Plot model
plot(model6_knn)

#printing performance measures for class Y
class_Y_model6_knn=class_Y_performance_measure(predicted = pred_model6_knn,reference = test_underbalanced_cfs$Class,pred_prob=predict(model6_knn, newdata = test_underbalanced_cfs, type = "prob")$Y)
class_Y_model6_knn

#printing performance measure for class N
class_N_model6_knn=class_N_performance_measure(predicted = pred_model6_knn,reference = test_underbalanced_cfs$Class,pred_prob=predict(model6_knn, newdata = test_underbalanced_cfs, type = "prob")$N)
class_N_model6_knn

#avg for class Y and N
avg_model6_knn=avg_YandN(class_Y = class_Y_model6_knn,class_N = class_N_model6_knn)




