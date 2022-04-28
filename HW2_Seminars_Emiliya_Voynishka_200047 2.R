# Problem 1----
outcomes <- c("win", "lose")
counter <- 0
for(k in 1:1000){
  
  budget <- 100
  bet <- 1
  
  while(budget > 0) {
    counter <- counter + 1
    
    biased_coin <- sample(outcomes, size = 1, prob = c(0.486, 0.514))
    
    if(biased_coin == "win"){
      budget <- budget+bet
      bet <- 1
    }
    else{
      budget <- budget-bet
      
      if(bet*2 > budget){
        bet <- budget
      }
      else{
        bet <- bet*2
      }
    }
  }
}
average <- counter/1000
print(average)











# Problem 2----
# 5.2.4 Exercises
# I. Find all flights that
# 1. Had an arrival delay of two or more hours----
filter(flights, arr_delay >= 120)


# 2. Flew to Houston (IAH or HOU)----
filter(flights, dest == "IAH" | dest == "HOU")


# 3. Were operated by United, American, or Delta----
filter(flights, carrier %in% c("AA", "DL", "UA"))


# 4. Departed in summer (July, August, and September)----
filter(flights, month >= 7, month <= 9)


# 5. Arrived more than two hours late, but didn’t leave late----
filter(flights, arr_delay > 120, dep_delay <= 0)


# 6. Were delayed by at least an hour, but made up over 30 minutes in flight----
filter(flights, dep_delay >= 60, dep_delay - arr_delay > 30)


# 7. Departed between midnight and 6am (inclusive)----
filter(flights, dep_time <= 600 | dep_time == 2400)


# II. Another useful dplyr filtering helper is between(). What does it do? Can you use it to simplify the code needed to answer the previous challenges?----
filter(flights, between(month, 7, 9))


# III. How many flights have a missing dep_time? What other variables are missing? What might these rows represent?----
filter(flights, is.na(dep_time))
summary(flights)


# IV. Why is NA ^ 0 not missing? Why is NA | TRUE not missing? Why is FALSE & NA not missing? Can you figure out the general rule? (NA * 0 is a tricky counterexample!)----
# NA ^ 0 == 1 since for all numeric values
# x ^ 0 = 1 
NA | TRUE
NA & FALSE
NA | FALSE
NA & TRUE
NA * 0

Inf * 0
-Inf * 0




# 5.3.1 Exercises
# I. How could you use arrange() to sort all missing values to the start? (Hint: use is.na()).----
arrange(flights, desc(dep_time))

# II. Sort flights to find the most delayed flights. Find the flights that left earliest.----
arrange(flights, desc(dep_delay))
arrange(flights, dep_delay)

# III. Sort flights to find the fastest (highest speed) flights.----
head(arrange(flights, air_time))

# IV. Which flights travelled the farthest? Which travelled the shortest?----
arrange(flights, desc(distance))

# 5.4.1 Exercises----
# I. Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights.----
select(flights, dep_time, dep_delay, arr_time, arr_delay)

# II. What happens if you include the name of a variable multiple times in a select() call?----
select(flights, year, month, day, year, year)
  
# III. What does the any_of() function do? Why might it be helpful in conjunction with this vector?----
# vars <- c("year", "month", "day", "dep_delay", "arr_delay")
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))

# IV. Does the result of running the following code surprise you? How do the select helpers deal with case by default? How can you change that default?----
# select(flights, contains("TIME"))
select(flights, contains("TIME"))

# 5.5.2 Exercises----
# I. Currently dep_time and sched_dep_time are convenient to look at, but hard to compute with because they’re not really continuous numbers. Convert them to a more convenient representation of number of minutes since midnight.----
1504 %/% 100
1504 %% 100
1504 %/% 100 * 60 + 1504 %% 100
flights_times <- mutate(flights,
                        dep_time_mins = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                        sched_dep_time_mins = (sched_dep_time %/% 100 * 60 +
                                                 sched_dep_time %% 100) %% 1440
)
select(
  flights_times, dep_time, dep_time_mins, sched_dep_time,
  sched_dep_time_mins
)

# II. Compare air_time with arr_time - dep_time. What do you expect to see? What do you see? What do you need to do to fix it?----
nrow(filter(flights_airtime, air_time_diff != 0))

# III. Compare dep_time, sched_dep_time, and dep_delay. How would you expect those three numbers to be related?----
filter(flights_deptime, dep_delay_diff != 0)

# IV. Find the 10 most delayed flights using a ranking function. How do you want to handle ties? Carefully read the documentation for min_rank().----
rankme <- tibble(
  x = c(10, 5, 1, 5, 5)
)

# V. What does 1:3 + 1:10 return? Why?----
1:3 + 1:10

# VI. What trigonometric functions does R provide?----
x <- seq(-3, 7, by = 1 / 2)
sin(pi * x)
cos(pi * x)
tan(pi * x)
pi
sinpi(x)
cospi(x)
tanpi(x)

