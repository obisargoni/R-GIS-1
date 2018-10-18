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
plot(Data3, Data4, col='red')
