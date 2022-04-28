# Task 1----
# Write a loop which iterates over all the numbers from 1 to 10 and prints
# them multiplied by 3
for (i in 1:10) {
  print(i*3)
}

# Task 2----
# Write a loop which chooses 10 random numbers, one at a time from a normal
# distribution (use rnorm and see the help ?rnorm) and prints the number
# if it is bigger than 1.

for (i in rnorm(1:10)) if (i>1) {print (i)}


# Task 3----
# What is the probability that out of a group of 6 men and 8 women, if we pick
# 5 people at random, exactly 3 will be men?
# Use a for loop, which simulates the picking.


people <- c(rep("men", 6), rep("women",8))
ResultVector <- NULL
for (a in 1:1000){
    choose <- sample(people, size = 5, replace = FALSE)
    if(sum(choose=="men")==3){
    ResultVector<-c(ResultVector,1)
    } else{
    ResultVector<-c(ResultVector,0)
    }
   
}
 sum(ResultVector)/1000
 dhyper(3,6,8,5,log=TRUE)
 
 
   for (a in choose){
   if(a=="men")
     print(a)
   else
     print("women")
 
 table (c(ResultVector))


}
# Task 4----
# Calculate the price of a european style option with strike price of 120, with an
# expiration date in 100 days.
# The underlying stock has a starting price of 100, which will change every 
# day based on a random number coming from a normal distribution with 
# mean = 0 and standard deviation = 7. This is rnorm(1, mean = 0, sd = 5) 
# The starting price was 100 on day 0.
# On day 1 it is 100 + rnorm(1, mean = 0, sd = 7). 
# On day 2 it is price from day 1 + rnorm(1, mean = 0, sd = 7) etc.
# On day 100 it is price from day 99 + rnorm(1, mean = 0, sd = 7).
# 
# European style options can only be exercised at expiry date. 
# So you must calculate the price of the underlying asset at day 100 and 
# compare it to the strike price. Then calculate the potential profit. 
# Because it is a call option the profit will range from 0 to infinity.
# Make 1000 simulations of the profit and the price of the call option,
# will be equal to the sum of all the profits divided by the number of 
# simulations(in this case 1000).

 
Price = 100 
ResultVector2 <- NULL
for(k in 1:10000) {
 Price = 100
 for(x in 1:10){
  Price = Price + sample(-1:1, 1)
}
  ResultVector2 <- c(ResultVector2, Price) 
}
 
#Problem 4

profit <- 0

for(k in 1:1000){
  
  price <- 100
  
  for (x in 1:100){
    price <- price + rnorm(1, mean = 0, sd = 7)
  }
  
  if(price >= 120){
    profit <- profit + (price - 120)
  }
}
print(profit/1000)
 


 



 