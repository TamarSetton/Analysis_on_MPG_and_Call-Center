#analysis of call center

#installation of all necessary packages
install.packages("tidyverse")
library(tidyverse)

cc <- read.csv("/Users/tamarsetton/Downloads/Call_Center.csv")

#To seee what working with
glimpse(cc)


#I wondered what was the main reason for calling
ggplot(cc, aes(x = Reason)) +
  geom_histogram(stat="count")
#Billing Questions dominate the reasons for calling almost 25000 to 5000


#What percentage of sentiments were there per each Reason? does it vary based on reason for calling?
ggplot(cc, aes(x = Reason, fill = Sentiment)) +
  geom_bar(position = "fill")
#the sentiment is the same for all reasons. 


#I can assume a better sentiment can lead to a higher csat.Score. Lets see if this is true.
#to get a list of the possible sentiments
print(unique(cc$Sentiment))
print(unique(cc$Csat.Score))


linear_model <- lm(cc$Csat.Score ~ cc$Sentiment, data = cc)
summary(linear_model)
#This model makes a lot of sense just looking at it. The coefficients align exactly with what I expected.
#Negative being the intercept, very negative is -2.07075 from there, and going in order of the positives Neutral, Positive, then very positive.

plot(linear_model)
#going through the plots I can verify the model doesnt have a trend in residuals and seems to be a pretty good model. That being said, it is important to note that the Csa,score is missing a lot of data.



#I feel like the response time probably effects the customer satisfaction, and possibly the call duration time. 
#Sometimes after I wai for a long time I want to keep them on the phone to get my times worth. If that's so, it might be more cost effective to 
#make sure the wait times are short so A) customers are more satisfied, B)the wait times stay short so they dont escilate with people keeping reps on the phone.

#first lets check how many response time variables there are
print(unique(cc$Response.Time)) #not too many

response_time_to_Csat <- lm(cc$Csat.Score ~ cc$Response.Time, data = cc)
summary(response_time_to_Csat)
#haha this is the worst model I've ever seen. The adjusted R squared is below zero. It seems the only response time that has a significant effect on the Customer satisfaction is above SLA

#I thought maybe just taking the first bit might be a decent model.
cc500 <- head(cc, 500)
response_time_to_Csat2<- lm(cc500$Csat.Score ~ cc500$Response.Time, data = cc500)
summary(response_time_to_Csat2)
#but its equally as bad

#lets see if the response time effects the duration time
response_time_to_duration <- lm(cc$Call.Duration.In.Minutes ~ cc$Response.Time, data = cc)
summary(response_time_to_duration)
#also an awful model. It seems the response time doesnt really effect the customer satisfaction or the call duration.


#I wonder if the reason effects call duration and csat
reason_to_duration <- lm(cc$Call.Duration.In.Minutes ~ cc$Reason, data = cc)
summary(reason_to_duration)
#the reason doesnt seem to effect call duration

reason_to_csat<- lm(cc$Csat.Score ~ cc$Reason, data = cc)
summary(reason_to_csat)
#reason also has no effect on customer satisfaction


#lets see if the call ceter city effects the response time, so we see which are responding faster
#first lets add a column that takes the response times and turns them into numbers 
print(unique(cc$Response.Time))
cc$Numeric.response.time<- as.numeric(factor(cc$Response.Time, levels = c("Below SLA", "Within SLA", "Above SLA")))

#just to check this worked
unique(cc$Call.Centres.City)
glimpse(cc)
#it did!

#now lets make our linear model
center_to_response <- lm(Numeric.response.time ~ Call.Centres.City, data = cc)
summary(center_to_response)
#no statistical significance between the center city and response time. although it seems they're all usually below SLA
