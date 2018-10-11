# load the packages

library(dplyr)
library(hflights)

# explore data
data(hflights)
head(hflights)

# assign to local dataframe
flights <- tbl_df(hflights)
flights

# look at specific no. of rows
print(flights, n=2)

# take a look at the data
data.frame(head(flights))


# 'filter' coma represents AND operatotr
filter(flights, Month==1, DayofMonth==1)

# 'filter' use OR operator
filter(flights, UniqueCarrier=="AA" | UniqueCarrier=="UA")

filter(flights, UniqueCarrier %in% c("AA", "UA"))

# 'select' : Pick column by name
select(flights, DepTime, ArrTime, FlightNum)

select(flights, Year:DayofMonth, contains("Taxi"), contains("Delay"))

# 'chaining' or "Pipelining'
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  filter(DepDelay > 60)

# 'arrange' : Reorder rows
# sort by 'DepDelay'
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(DepDelay)
# sort by descending
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(desc(DepDelay))


# 'mutate' :  add new variable
flights %>% 
  select(Distance, AirTime) %>%
  mutate(Speed = Distance/AirTime*60)
flights <- flights %>%
             mutate(Speed = Distance/AirTime*60)
flights$Speed

# 'summarise' : Reduce variables to values
# create a table grouped by Dest, and then summarise
# each group by taking the mean of ArrDelay
flights %>%
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay, na.rm=TRUE))

#  'summarise_each' : allow you to apply the same summary 
# function to multiple columns at once

# for each carrier, claculate the percentage of flights
# cancelled or diverted
flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(mean), Cancelled, Diverted)

# for each carrier, calculate the minimum and maximum 
# arrival and departure delays
flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(min(.,na.rm = TRUE), max(.,na.rm=TRUE)),
                  matches("Delay"))


# Helper function 'n()' counts the number of rows in
# a group
# Helper function 'n_distinct(vector)' counts the number
# of unique items in that vector

# for each day of the year(365 days), count the total 
# number of flights and sort in descending order.
flights %>%
  group_by(Month, DayofMonth) %>%
  summarise(flight_count = n()) %>%
  arrange(desc(flight_count))

# rewrite using 'tally' function
flights %>%
  group_by(Month, DayofMonth) %>%
  tally(sort = TRUE)

# for each destination, count the total number of 
# flights nd the number of distinct planes that flew there

flights %>%
  group_by(Dest) %>%
  summarise(flight_count = n(), plane_count = n_distinct(TailNum))

# for each destination, show the number of cancelled 
# and not cancelled flights
flights %>%
  group_by(Dest) %>%
  select(Cancelled) %>%
  table() %>%
  head()

# 'Aggregation function' (like 'mean') takes n inputs and returns 1 value
# 'Window function' takes n inputs and returns n values
# (like 'min_rank')

# for each carrier, calculate which two days of the year
# they had their longest departure delays
# note: smallest (not largest) value is ranked as 1, so you 
# have to use 'desc' to rank by largest value

flights %>%
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay) %>%
  filter(min_rank(desc(DepDelay)) <= 2) %>%
  arrange(UniqueCarrier, desc(DepDelay))

# rewrite
flights %>%
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay) %>%
  top_n(2) %>%
  arrange(UniqueCarrier, desc(DepDelay))


# for each month, calculate the number of flights
# and the change from the previous month
flights %>%
  group_by(Month) %>%
  summarise(flight_count = n()) %>%
  mutate(change = flight_count - lag(flight_count))

# rewrite
flights %>%
  group_by(Month) %>%
  tally() %>%
  mutate(change = n - lag(n))

## Other Useful Convenience Functions

# randomly sample a fixed number of rows, without 
# replacement
flights %>% sample_n(5)

# randomly sample a fraction of rows, with replacement
flights %>% sample_frac(0.25, replace = TRUE)

# structure of object
str(flights)
# rewrite structure of object 
glimpse(flights)





