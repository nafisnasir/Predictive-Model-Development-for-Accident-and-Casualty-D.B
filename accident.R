setwd("E:/2nd Semester/advanced analytis/dataset")
accident= read.csv("E:/2nd Semester/advanced analytis/dataset/fatalaccidentdata.csv",header=TRUE)
casualty= read.csv("E:/2nd Semester/advanced analytis/dataset/fatalcasualtydata.csv",header=TRUE)
View (accident)
summary(accident)
summary(casualty)
summary(dataframe)

#checking if any missing values - no missing values found so data preprocessing not done
colSums(is.na(dataframe))
sum(is.na(dataframe))


# load the libraries
library(mlbench)
# load the dataset
data(PimaIndiansDiabetes)
# display the dimensions of the dataset
dim(dataframe)

# list types for each attribute
sapply(dataframe, class)

# Merging dataset

dataframe = merge(accident, casualty, by.x = "Fatal_Accident_Index", by.y = "Fatal_Accident_Index")

write.csv(dataframe,file = "accident and casualty 2006 to 2018.csv")

#Converting integer to nurmeric (Pedestrain casualty to total casualty)
dataframe[6:18] <- lapply(dataframe[6:18], as.numeric)
dataframe[21] <- lapply(dataframe[21], as.numeric) 
class(dataframe$Total_Number_of_Casualties) # checking  

# distribution of class variable




# calculate standard deviation for all attributes
sapply(dataframe[,1:21], sd)

# calculate a correlation matrix for numeric variables
correlations <- cor(dataframe[,6:18,21])
# display the correlation matrix
print(correlations)

# calculate skewness for each variable
skew <- apply(dataframe[,1:21], 2, skewness)
# display skewness, larger/smaller deviations from 0 show more skew
print(skew)


mydata <- dataframe[, c(6:18,21)]
head(mydata)

cormat <- round(cor(mydata),2)
head(cormat)

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

# Give the chart file a name.
png(file = "boxplot.png")

# Plot the chart.
boxplot(dataframe$Total_Number_of_Casualties~ dataframe$Slight_Casualties, data = mtcars, xlab = "car",
        ylab = "Total", main = "Mileage Data")

# Save the file.
dev.off()


# Give the chart file a name.
png(file = "line_chart_label_colored.jpg")

# Plot the bar chart.
plot(dataframe$Total_Vehicles_Involved,type = "o", col = "red", xlab = "Month", ylab = "Rain fall",
     main = "Rain fall chart")

# Save the file.
dev.off()


barplot(dataframe$Total_Number_of_Casualties, names.arg=dataframe$Month_of_Accident, xlab="A", ylab="B", col="red",border = "blue")

barplot(dataframe$Total_Number_of_Casualties,
        main = "Accident",
        xlab = "slight Casualties",
        ylab = "Total Casualties",
        names.arg = dataframe$Slight_Casualties,
        col = "green",
        horiz = TRUE)

#pie
pie(table(dataframe$Fatal_Casualties+dataframe$Serious_Casualties+dataframe$Slight_Casualties))

#Histrogram
hist(dataframe$Hour_of_Accident, 
     main="Histogram for Hour of Accident ", 
     xlab="hours", 
     border="blue", 
     col="green")

#plotting
plot(dataframe$Slight_Casualties, dataframe$Total_Number_of_Casualties, main="Scatterplot")
cor(dataframe$Slight_Casualties, dataframe$Total_Number_of_Casualties)
# train data 
set.seed(123)
split <- sample(seq_len(nrow(dataframe)), size = floor(0.75 * nrow(dataframe)))
trainData <- dataframe[16:19 ]
testData <- dataframe[16:19 ]
head(trainData)
head(testData)


trainingOutcomes <- dataframe[16:19]
testOutcomes <- dataframe[16:19]







# linear regression
model1<- lm(dataframe$Total_Number_of_Casualties~dataframe$Slight_Casualties,data=trainData)
abline(model1,col=10, lwd=3)
summary(model1)
attributes(model1)
anova(model1)
fit(model1)
sqrt(1.7)
prediction <- predict(model1, newdata = testData)
head(prediction)
head(testData$Slight_Casualties)
plot(model1)

SSE <- sum((testData$Slight_Casualties - prediction) ^ 2)
SST <- sum((testData$Slight_Casualties - mean(testData$Slight_Casualties)) ^ 2)
1 - SSE/SST
p <- predict(model1, new_data)

# Multiple regression
model2<- lm(dataframe$Total_Number_of_Casualties~dataframe$Slight_Casualties+dataframe$Month_of_Accident)
abline(model2,col=2, lwd=3)
summary(model2)
plot(model2)
attributes(model2)
anova(model2)
sqrt(4.3)


# Multiple Linear Regression Example 
fit <- lm(dataframe$Total_Number_of_Casualties~ dataframe$Slight_Casualties + dataframe$Month_of_Accident, data=mydata)
summary(fit) # show results
# Anova
anova(model1,model2)






#Slope


# get a Google map
library(ggmap)
map<-get_map(location='great britain', zoom=4, maptype = "terrain",
             source='google',color='color')

# plot it with ggplot2
require("ggplot2")
ggmap(map) + geom_point(
  aes(x=ongitude, y=latitude, show_guide = TRUE, colour=Median), 
  data=census, alpha=.8, na.rm = T)  + 
  scale_color_gradient(low="beige", high="blue")

# plotting dot in Map
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-20, 59), ylim = c(35, 71), asp = 1)
 points(dataframe$Longitude, dataframe$Latitude, col = "blue", cex = .6)
 
 
 library(party)
 
 # Create the input data frame.
 input.dat <- dataframe[c(2,3,6:14,16:19,20:21),]
 
 # Give the chart file a name.
 png(file = "decision_tree.png")
 
 # Create the tree.
 output.tree <- ctree(
   dataframe$Month_of_Accident ~ dataframe$Hour_of_Accident +dataframe$Pedestrian_Casualties + dataframe$Pedal_Cycles+dataframe$Motor_Cycles+dataframe$Cars+dataframe$Buses_or_Coaches+dataframe$Vans+dataframe$HGVs+dataframe$Other_Vehicles+dataframe$Fatal_Casualties+dataframe$Serious_Casualties+dataframe$Slight_Casualties+dataframe$Fatal_Casualty_Type+dataframe$Fatal_Casualty_Age, 
   data = input.dat)
 
 # Plot the tree.
 plot(output.tree)
 