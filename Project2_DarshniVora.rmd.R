#Explore Bike Share Data
library(ggplot2)
library(lubridate) 
library(anytime) 
library(data.table)

#Loading data into R

ny1 = read.csv('new_york_city.csv')
wash1 = read.csv('washington.csv')
chi1 = read.csv('chicago.csv')

#We will remove gender and birthyear from Chicago and New York so that there is consistency in all records

ny2 = ny1 [,c(1,2,3,4,5,6,7)]
chi2 = chi1 [,c(1,2,3,4,5,6,7)]

#We will now remove all the null values

ny = na.omit(ny2)
chi = na.omit(chi2)
wash = na.omit(wash1)

#We observe that there was only one row that had null values in New York and Washington. Whereas, Chicago did not have any null values.


#Data Wrangling
#We will now make changes to the data and join the 3 dataframes to ensure that we can easily run our code

#We are adding the city field so that we do not lose that data when we combine it to one dataframe.

wash$City = 'Washington'
ny$City = 'New York'
chi$City = 'Chicago'

#Next, we need to correct data types because Start.Time and End.Time are not in proper datetime objects. 

correctDates = function(df_) {df_$Start.Time = anytime(df_$Start.Time)
df_$End.Time = anytime(df_$End.Time)
df_
}

wash = correctDates(wash)
chi = correctDates(chi)
ny = correctDates(ny)

#We added the month field
wash$Month = month(wash$Start.Time)
chi$Month = month(chi$Start.Time)
ny$Month = month(ny$Start.Time)

#Lastly, we will combine the 3 dataframes together. Since R doesn't allow us combine more than 2 at a time, we have to do it twice.

df <- rbind(ny, wash)
df <- rbind(df, chi)

##Question 1: What is the most common month?

options(repr.plot.width=10, repr.plot.height=5)


ggplot(aes(x=Month, fill=City), data=df) +
  geom_bar(position='dodge') +
  scale_x_continuous(breaks=c(1,2,3,4,5,6), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun")) +
  theme(text = element_text(size = 10)) +
  ggtitle("Number of rides for each month")

#The most number of rides are taken in the month of June, for all 3 cities.

#We can calculate the mean number of trips for a city in the following way:
#Total number of rows (observations) in one dataframe/Number of months we have data for (6)
#Chicago:
Mean_number_of_rides_Chicago = nrow(chi)/6
print(Mean_number_of_rides_Chicago)
#Washington:
Mean_number_of_rides_Washington = nrow(wash)/6
print(Mean_number_of_rides_Washington)
#New York
Mean_number_of_rides_NewYork = nrow(ny)/6
print(Mean_number_of_rides_NewYork)

#Therefore we observe that for the month of June the number of rides is substantially higher than the average number of rides for a month.

#Question 2: What is the total travel time for users in different cities?

#We convert the Total travel duration to hours for a more meaningful and manageable number
Total_Duration_Chi = sum(chi$Trip.Duration)/3600

Total_Duration_Wash = sum(wash$Trip.Duration)/3600

Total_Duration_NY = sum(ny$Trip.Duration)/3600

#Graph to show the total travel dutaion
TD = c(Total_Duration_Chi,Total_Duration_NY,Total_Duration_Wash)
L = c("Chicago", "New York", "Washington")
barplot(TD,names.arg=L,xlab="Cities",ylab="Total Duration in hours",col="red",
        main="Total travel duration chart",border="red")

#Solely, this data does not give us any useful information because since the total number of observations is the highest in the Washington dataframe, we see the highest records there.

#Next, let us find the average travel duration so as to make a more meaningful comparison.

#Question3 : What is the average travel time for users in different cities?

#We 

Mean_Duration_Chi = mean(chi$Trip.Duration/60)
Mean_Duration_Wash = mean(wash$Trip.Duration/60)
Mean_Duration_NY = mean(ny$Trip.Duration/60)

#Graph to show the total travel dutaion
MD = c(Mean_Duration_Chi,Mean_Duration_Wash,Mean_Duration_NY)
L = c("Chicago", "New York", "Washington")
barplot(MD,names.arg=L,xlab="Cities",ylab="Average Duration in minutes",col="red",
        main="Average travel duration chart",border="red")

#Now we see a more meaningful comparison and observe that on average riders in New York rent the bike for 20 minutes whereas in Chicago and Wahington they rent it for close to 15 minutes.
