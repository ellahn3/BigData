
install.packages("dplyr")
install.packages("tidyr")

library(dplyr)
library(tidyr)
electric <- read.delim("C:/Users/mordi/OneDrive/Desktop/big data/week3/table.tsv")
electric$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )#time obyect
summary(electric)
print (head(electric))
print(names(electric))

#clean data
electric<-distinct(electric)
print (head(electric))

# Build a cube
revenue_cube <-
  tapply(electric$megawatthours,#data
         electric[,c("Day.ahead.demand.forecast","Net.generation")],  #dimensions that i want to calculate with the func
         FUN = sum )
print (revenue_cube)

#roll up 
rollup <-
  apply(revenue_cube, c("year", "prod"),
        FUN = function(x) sum(x, na.rm = TRUE) )
print (rollup)


# Slice
# cube data in Jan, 2012
print (revenue_cube[ , "1", "2012", ])


# create an empty plot
plot(1, type="n", xlab="", ylab="", xlim = c(0,12), ylim=c(-2, 2))


# calculate means and stdev
demands <- seq(10) # [ -c(1,4,8) ]
for ( i in demands ) {
  M[[ i ]] <- mean( C[ !is.na(C[, i]) ,i ] )
  S[[ i ]] <- sd( C[ !is.na(C[, i]), i ] )
}


days <- seq(10)[ -c(7,8,9,10,11,12,13,14) ]
for ( i in days ) {
  # rearrange in a new, temporary dataframe
  DF <- data.frame ( Time = rng - min(rng), Demand = norm.C[ , i ] )
  # plot
  lines( DF, col = i, type = 'b' )
  # linear fit
  LM[[ i ]] <- lm( Demand ~ Time, data = DF)
  a <- coef(LM[[ i ]])[1]
  b <- coef(LM[[ i ]])[2]
  abline(a, b, col = i, lw =2)
}





print (revenue_cube[ "2", "2021" ])

