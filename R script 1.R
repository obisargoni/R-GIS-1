getwd()

# Variable assignment
A<-1
B<-2  
C <- A + B
C

# List variables inuse, remove variable
ls()
rm(A)
ls()

# Create and plot some data
Data1 <- c(1:100) # () concatenates a string of numbers into a vector, smae as range() in py?
Data2 <- c(101:200) 
plot(Data1, Data2, col= 'red')

# Normally distributed data
Data3 <- rnorm(100, mean = 53, sd=34)
Data4 <- rnorm(100, mean= 64, sd=14)
plot(Data3, Data4, col='gold')

# Hold data in a data frame
df <- data.frame(Data1,Data2)
plot(df, col='green')
head(df)

# Indexing a dataframe
df[1:10,1]
df[1,]
df[,2]
df[c(2,3,6),2]

# change col names
?colnames
colnames(df)
colnames(df) <- c('column 1', 'column 2')
colnames(df)

# can now refer to data by column names rather than indices
df$`column 1`
df[['column 1']][1:10]

# install tidyverse package
library(tidyverse)

LdnData <- read_csv("https://files.datapress.com/london/dataset/ward-profiles-and-atlas/2015-09-24T14:21:24/ward-profiles-excel-version.csv", na = 'n/a')

# get data types of ldn tibble as dataframe
datatypelist <- data.frame(cbind(lapply(LdnData,class)))
head(datatypelist)

# Try without cleaning
LdnData2 <- read_csv("https://files.datapress.com/london/dataset/ward-profiles-and-atlas/2015-09-24T14:21:24/ward-profiles-excel-version.csv")
datatypelist2 <- data.frame(cbind(lapply(LdnData2,class)))


?edit
LdnData2 <- edit(LdnData2)
LdnData <-edit(LdnData)

summary(df) # descriptive statistics
names(LdnData) # column names
colnames(LdnData) # also column names

tail(LdnData)

LdnBurs <-  LdnData[626:658,]
summary(LdnBurs)
LdnBurs[30]

# Get Ldn Boroighs using grep() function
?grepl()
grep("E09", LdnData[,3])
LdnBurs <- LdnData[grep("^E09", LdnData[,3], fixed = FALSE),]

# drop CoL since it's repeated
LdnBurs <- LdnBurs[2:34,]

# select subset of columns
LdnBurs <- LdnBurs[,c(1,19,20,21)]

# Rename cols
names(LdnBurs)[1] <- c("Borough Name")

# plot thedata
plot(LdnBurs$`Male life expectancy -2009-13`, LdnBurs$`% children in reception year who are obese - 2011/12 to 2013/14`)

#install.packages("plotly")
library(plotly)
plot_ly(LdnBurs, x = ~`Male life expectancy -2009-13`, y = ~`% children in reception year who are obese - 2011/12 to 2013/14`, 
        text = LdnBurs$`Borough Name`, type = 'scatter', mode = 'markers')


