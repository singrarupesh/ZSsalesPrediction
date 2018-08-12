#Change the files path according to your directories path


library(lubridate)
library(sqldf)
library(caret)
set.seed(998)

holidayData <- read.csv("D:\\Development\\R\\Mekktronics\\holidays.csv", as.is = T)
holidayData$Date <- gsub(", ", "/", holidayData$Date)
holidayData$Year <- as.Date(holidayData$Date, "%Y")
holidayData$Month <- format(as.Date(holidayData$Date), "%m")
holidayData$Year <- year(as.Date(holidayData$Year, format = "%m/%d/%Y"))
holidayData$Total <- 1
finalHolidayData1 = holidayData[,c("Year", "Month", "Country", "Total")]
finalHolidayData = with(finalHolidayData1 , aggregate(list(Total = Total), list(Year = Year, 
                          Month = Month,  Country = Country), sum))

trainData <- read.csv("D:\\Development\\R\\Mekktronics\\yds_train2018.csv", as.is = T)
keepTrainData = trainData[,c("Year", "Month", "Product_ID", "Country", "Sales")]
keepTrainDataMonthly = with(keepTrainData , aggregate(list(Sales = Sales), list(Year = Year, 
    Month = Month, Product_ID = Product_ID, Country = Country), sum))
expenseData <- read.csv("D:\\Development\\R\\Mekktronics\\promotional_expense.csv", as.is = T)
keepExpensiveColumns = expenseData[,c("Year", "Month", "Product_Type", "Country", "Expense_Price")]
expenseMonthly = with(keepExpensiveColumns , aggregate(list(Expense_Price = Expense_Price), list(Year = Year, Month = Month, 
    Product_Type = Product_Type, Country = Country), sum))

trainWithExpense = sqldf("
  SELECT d1.Year, d1.Month, d1.Country, d1.Product_ID, d2.Expense_Price, d1.Sales
                     FROM keepTrainDataMonthly d1 LEFT JOIN expenseMonthly d2
                     ON d1.Year = d2.Year
                     AND d1.Month = d2.Month
                     AND d1.Country = d2.Country
                     AND d1.Product_ID = d2.Product_Type
                     ")

ydsTest = read.csv("D:\\Development\\R\\Mekktronics\\ydsnaivesubmission.csv", as.is = T)
ydsTestMonthly = ydsTest[,c("Year", "Month", "Product_ID", "Country", "Sales")]

ydsFinal = sqldf("
  SELECT d1.Year, d1.Month, d1.Country, d1.Product_ID, d2.Expense_Price, d1.Sales
                 FROM ydsTestMonthly d1 LEFT JOIN expenseMonthly d2
                 ON d1.Year = d2.Year
                 AND d1.Month = d2.Month
                 AND d1.Country = d2.Country
                 AND d1.Product_ID = d2.Product_Type
                 ")
ydsFinal[is.na(ydsFinal)] <- 0

trainFinal <- sqldf("
  SELECT d1.Year, d1.Month, d1.Country, d1.Product_ID, d1.Expense_Price, d1.Sales, d2.Total
                 FROM trainWithExpense d1 LEFT JOIN finalHolidayData d2
                 ON d1.Year = d2.Year
                 AND d1.Month = d2.Month
                 AND d1.Country = d2.Country
                 ")
trainFinal[is.na(trainFinal)] <- 0

testFinal = sqldf("
  SELECT d1.Year, d1.Month, d1.Country, d1.Product_ID, d1.Expense_Price, d1.Sales, d2.Total
                 FROM ydsFinal d1 LEFT JOIN finalHolidayData d2
                 ON d1.Year = d2.Year
                 AND d1.Month = d2.Month
                 AND d1.Country = d2.Country
                 ")
testFinal[is.na(testFinal)] <- 0

#testFinal[complete.cases(testFinal), ]

#test <- with(testFinal , aggregate.data.frame(list(Holiday, Holiday), list(Year = Year, 
        #Month = Month, Product_ID = Product_ID, Expense_Price = Expense_Price, Sales = Sales, Country = Country), sum))

#inTraining <- createDataPartition(trainFinal$Sales, p = .7, list=FALSE)
training <- trainFinal
testing <- testFinal

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


system.time(
  rfFit1 <- train(Sales~., data = training, method = "rf", trControl=fitControl)
)

pred <- predict(rfFit1, testing)

df <- data.frame(ydsTest,pred)

write.csv(file="D:\\Development\\R\\Mekktronics\\predResults.csv", row.names=FALSE, x = df)

result<-df[,c("S_No","Year", "Month", "Country","Product_ID", "pred")]

names(result)[6]<-"Sales"

write.csv(file="D:\\Development\\R\\Mekktronics\\yds_submission2018.csv", row.names=FALSE, x=result)

