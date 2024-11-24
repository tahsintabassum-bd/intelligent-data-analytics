x= c(3, 12, 6, -5, 0, 8, 15, 1, -10, 7)
x

y=seq(from=min(x),to=max(x),along.with=x)  ## Seq command for minimum to maximum range of Y
y

min(y)    ## Minimum value of Y
max(y)    ## maximum value of Y
sort(x)   ## Minimum to maximum rearrangement of x components 

sum(x)
mean(x)
sd(x)
var(x)
mad(x)
quantile(x)
quintiles_x <- quantile(x, probs = seq(0, 1, by = 0.2))
quintiles_x
sum(y)
mean(y)
sd(y)
var(y)
mad(y)
quantile(y)
quintiles_y <- quantile(y, probs = seq(0, 1, by = 0.2))
quintiles_y
x
z= sample(x, size=7,replace=TRUE)
z

t.test(x,y, conf=0.95, var.eq=T, paired=F)


s = order (x)
s

t.test(s,y, conf=0.95, var.eq=T, paired=F)

x
a=x[x<0]
a

x= c(3, 12, 6, -5, 0, 8, 15, 1, -10, 7)
x[-x<=0]
length(x[-x<=0])

col1 <- c(1,2,3,NA,5)
col2 <- c(4,5,6,89,101)
col3 <- c(45,NA,66,121,201)
col4 <- c(14,NA,13,NA,27)
X <- rbind (col1,col2,col3,col4)

X

is.na(X)  ## All TRUE elements indicateing missing values in the dataframe 

Y <- c(3,12,99,99,7,99,21)
Y

Y[Y==99]=NA  ## replacing 99 with NA 
Y

sum(is.na(Y))

college=read.csv("college-1.csv")  
college

rownames(college) = college [ ,1]  ## The first columns containing university names are added as raw heading. 
View(college)

college = college[ ,-1]  ## We do not need university names as variables. So the first colmnn is made private.  
college

summary(college)
# Check the structure of the data to identify non-numeric columns
str(college)

# Select only the numeric columns for the scatterplot matrix
numeric_cols <- sapply(college, is.numeric)

# Apply pairs() to the first ten numeric columns
pairs(college[, numeric_cols][, 1:10])

A=college[ ,1:10]   ## first ten columns of a matrix A 
?pairs
pairs(A)  ## scatterplot matrix of the first ten columns 

## side-by-side boxplots of Outstate versus Private
# Ensure that Private is treated as a factor
college$Private <- as.factor(college$Private)
plot(college$Outstate~college$Private, xlab= 'Private', ylab= 'Outstate fee', col='grey', main='Outstate tuition fee at different types of schools')




Elite <- rep ("No", nrow(college ))      ## Make all the elements blank and homogeneous  
Elite [college$Top10perc >50] <- "Yes"   ## Only the values more than 50 at 'top 10 per' column became 'yes' and selected.
Elite <- as.factor (Elite)               ## Convert the numeric numbers to ordered factor.
college <- data.frame(college ,Elite)    ## New variable 'Elite' created 


summary(Elite)  ## Use of the summary() function to see how many elite universities there are. 78 universities found elite

summary(college)

plot(college$Outstate~college$Elite, xlab= 'Elite', ylab= 'Outstate fee', col='gray', main='How the outstate tuition fee differs in Elite and Normal School')

par(mfrow=c(2,2))
hist(college$Apps, col='gray')
hist(college$Accept, col='gray')
hist(college$PhD, col='gray')
hist(college$S.F.Ratio, col='gray')

install.packages("plyr")

library(plyr)

df = baseball
?baseball 

df["sf"][df["year"] <= 1954] = 0    ## sf variable before 1954 are set to 0

index=is.na(df$hbp)    ## Missing value of hbp is set to 0. 
df$hbp[index] = 0

a <- df[df$ab>50,]  ## player records with fewer than 50 (ab<50) at bats are excluded. The data frame assigned at 'a'. 


a$hbp <- as.numeric(as.character(a$hbp))   ## Convert the 'hbp' to numeric for the equation 
class(df$hbp)

obp=(a$h + a$bb + a$hbp)/(a$ab + a$bb + a$hbp +a$sf)  ## base percentage  

b=cbind(a,obp)      ## Combine the OBP with data=frame 'a' where player records with are than 50 (ab<50)
newdata <- b[order(obp),]  ## rearranging the full data frame with ascending order of 'obp' column & assign to 'new data'

x=newdata[ , c('id','year','obp')]   ## Slicing the 'player name', 'year'  and 'obp' column only from the data frame 'new data'
x[1:5,]  ## Extract the top five records only 





quakes ## load the quakes 

plot(quakes$mag,quakes$depth,xlab='Magnitude level',ylab='Depth', main='Plot of "Magnitude level vs Depth"',col='black')

quakeAvgDepth= aggregate(depth~mag,quakes,mean)
quakeAvgDepth

colnames(quakeAvgDepth)=c('Magnitude Level','Average Depth')
colnames(quakeAvgDepth)

plot(quakeAvgDepth$`Magnitude Level`,quakeAvgDepth$`Average Depth`,xlab='Magnitude level',ylab='Average Depth', main='Plot of "Magnitude level vs Average Depth"',col='black')


