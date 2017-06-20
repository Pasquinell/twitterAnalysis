

################# Twitter Analysis ###############################################
##################################################################################
# By_           Pasquinell Urbani
# Mail_         purbanir@gmail.com
# Date_         2017/05/30
# Function_     This code generates mutiples plots by taking twitter publications
###################################################################################
###################################################################################

# Run only if it's the first time
#install.packages(c("httr","devtools","twitteR","tm","wordcloud","plyr","dplyr","lubdidate","plotly","ggplot2","xts","padr"))


# Load all the packages and log in to twitter developers account
library(httr)
library(devtools)
library(twitteR)
library(tm)
library(wordcloud)
library(plyr)
library(dplyr)
detach("package:plyr", unload=TRUE) 
library(lubridate)
library(plotly)
library(ggplot2)
library(xts)
library(padr) # Guide to padr https://www.r-bloggers.com/introducing-padr/

# Set API Keys
# How to create an account https://dev.twitter.com/index
api_key <- ""
api_secret <- ""
access_token <- ""
access_token_secret <- ""
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)



########### Extract the information ############################

# Choose all the delegations we want to download
list_delegations = c("DelegacionAero","DAMFMN","daetsam","Delegacion_ICCP",
                     "deleetsii_upm","daetsime","DelegacionETSIN","dat_etsit",
                     "DatopoUpm","DAETSEM","dalumetsisi","DAETSIAAB_UPM","da_etsidi",
                     "DelegacionETSIC","dalum_etsist","daetsiinf","DAInefUpm",
                     "delegacioncsdmm")


# Loop for creating df_general
for(i in list_delegations){
  
  # Load the tweets of the i delegation:
  #tweets = userTimeline(i, n=2000)
  tweets_RT = userTimeline(i, n=3500,includeRts=TRUE)
  # Transform to dataframe
  df = twListToDF(tweets_RT)
  
  # agregate by day
  df2 = df %>%
    # Cut all the dates (column created) and erase H:M:S (hour:min:sec)
    thicken(interval = "day") %>%
    # Group by this new column
    group_by(created_day) %>%
    # Summarize the 3 columns that matter
    summarize(n_tweets = n(),
              likes = sum(favoriteCount),
              # Retweets
              rt = sum(retweetCount))
  # Fill with NA's the missing days
  df2_pad = df2 %>%
    pad
  # Fill with ceros the missing days
  df2_pad_fill = df2_pad %>% 
    fill_by_value(n_tweets,likes,rt)
  # Agregate by week and index the monday of this week as the ID of he day
  # https://stackoverflow.com/questions/40554231/dplyr-lubridate-how-to-aggregate-a-dataframe-by-week
  df2_pad_fill_week = df2_pad_fill %>%
    # Group by week, name a new column first_day_of_week and place a date like year-month-day (where day is the monday)
    group_by(first_day_of_week = cut(created_day, "week", start.on.monday = TRUE)) %>%
    # sumarize the new data
    summarize(n_tweets = sum(n_tweets), #select distinct
             likes = sum(likes),
             rt = sum(rt))
  
  #transform the column week in POSIXct
  df2_pad_fill_week$first_day_of_week <- as.POSIXct(df2_pad_fill_week$first_day_of_week, "%Y-%m-%d")
  
  
  # Build rt_t (retweets/n_tweets) and likes_t (likes/n_tweets) and delegations
  df2_pad_fill_week["rt_t"] = df2_pad_fill_week["rt"]/df2_pad_fill_week["n_tweets"]
  df2_pad_fill_week["likes_t"] = df2_pad_fill_week["likes"]/df2_pad_fill_week["n_tweets"]
  df2_pad_fill_week["delegacion"]  = i
  
  
  # If not the first, rowbind (rbind) the dataframes
  if(i==list_delegations[1]){
    df_general = df2_pad_fill_week
  }else{
    df_general = rbind(df_general,df2_pad_fill_week)
  }
  
}


########### Plot the time series ############################


# Choose all the delegations we want to plot
list_delegations = c("DelegacionAero","DAMFMN",
                     "daetsam","Delegacion_ICCP",
                     "deleetsii_upm","daetsime","DelegacionETSIN","dat_etsit",
                     "DatopoUpm","DAETSEM","dalumetsisi","DAETSIAAB_UPM","da_etsidi",
                     "DelegacionETSIC","dalum_etsist","daetsiinf","DAInefUpm",
                     "delegacioncsdmm")



# Loop for ploting cantidad de tweets
for (i in list_delegations){
  minidf = df_general %>%
    filter(delegacion == i)
  
  ## Now we extract trend
  # Build xts object
  xts_obj <- as.xts(minidf$n_tweets, order.by=minidf$first_day_of_week, frequency = 4)
  # Build a ts from xts with frecuency 4 (nearly 4 weeks in one month)
  ts_obj = ts(xts_obj, frequency = 4)
  # decompose() separates your ts in to original data, trend, noise, seasonality
  # http://maths-people.anu.edu.au/~johnm/courses/r/ASC2008/pdf/Rtimeseries-ohp.pdf
  dec = decompose(ts_obj)
  # Extract only the column
  trend = dec$trend
  # Create dataframe with the original and trend data
  df_trend = data.frame(date = time(xts_obj),original = coredata(xts_obj),trend = coredata(trend))
  
  # Graph original and trend
  n_tweets_plot <- ggplot()+
    geom_line(aes(x=date, y=trend, lty="Tendencia"),df_trend) +
    geom_line(aes(x=date, y=original, lty="Datos"),df_trend,colour="#0072B2") +
    ggtitle(paste("Numero de tweets de ",i )) +
    labs(x="Fecha",y="Numero de tweets" ) +
    #xlim(as.POSIXct("2013-01-01"), as.POSIXct("2017-04-01")) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme_classic()
  print(n_tweets_plot)
}

# Loop for ploting likes
for (i in list_delegations){
  minidf = df_general %>%
    filter(delegacion == i)
  
  ## Now we extract trend
  # Build xts object
  xts_obj <- as.xts(minidf$likes, order.by=minidf$first_day_of_week, frequency = 4)
  # Build a ts from xts with frecuency 4 (nearly 4 weeks in one month)
  ts_obj = ts(xts_obj, frequency = 4)
  # decompose() separates your ts in to original data, trend, noise, seasonality
  # http://maths-people.anu.edu.au/~johnm/courses/r/ASC2008/pdf/Rtimeseries-ohp.pdf
  dec = decompose(ts_obj)
  # Extract only the column
  trend = dec$trend
  # Create dataframe with the original and trend data
  df_trend = data.frame(date = time(xts_obj),original = coredata(xts_obj),trend = coredata(trend))
  
  # Graph original and trend
  n_tweets_plot <- ggplot()+
    geom_line(aes(x=date, y=trend, lty="Tendencia"),df_trend) +
    geom_line(aes(x=date, y=original, lty="Datos"),df_trend,colour="#0072B2") +
    ggtitle(paste("Numero de likes de ",i )) +
    labs(x="Fecha",y="Numero de likes" ) +
    #xlim(as.POSIXct("2013-01-01"), as.POSIXct("2017-04-01")) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme_classic()
  print(n_tweets_plot)
}

# Loop for ploting retweets
for (i in list_delegations){
  minidf = df_general %>%
    filter(delegacion == i)
  
  ## Now we extract trend
  # Build xts object
  xts_obj <- as.xts(minidf$rt, order.by=minidf$first_day_of_week, frequency = 4)
  # Build a ts from xts with frecuency 4 (nearly 4 weeks in one month)
  ts_obj = ts(xts_obj, frequency = 4)
  # decompose() separates your ts in to original data, trend, noise, seasonality
  # http://maths-people.anu.edu.au/~johnm/courses/r/ASC2008/pdf/Rtimeseries-ohp.pdf
  dec = decompose(ts_obj)
  # Extract only the column
  trend = dec$trend
  # Create dataframe with the original and trend data
  df_trend = data.frame(date = time(xts_obj),original = coredata(xts_obj),trend = coredata(trend))
  
  # Graph original and trend
  n_tweets_plot <- ggplot()+
    geom_line(aes(x=date, y=trend, lty="Tendencia"),df_trend) +
    geom_line(aes(x=date, y=original, lty="Datos"),df_trend,colour="#0072B2") +
    ggtitle(paste("Numero de retweets de ",i )) +
    labs(x="Fecha",y="Numero de retweets" ) +
    #xlim(as.POSIXct("2013-01-01"), as.POSIXct("2017-04-01")) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme_classic()
  print(n_tweets_plot)
  
  
}

# Loop for ploting likes/cantidad de tweets
for (i in list_delegations){
  minidf = df_general %>%
    filter(delegacion == i)
  
  ## Now we extract trend
  # Build xts object
  xts_obj <- as.xts(minidf$likes_t, order.by=minidf$first_day_of_week, frequency = 4)
  # replace nan with 0
  xts_obj[is.nan(xts_obj)] <- 0
  # Build a ts from xts with frecuency 4 (nearly 4 weeks in one month)
  ts_obj = ts(xts_obj, frequency = 4)
  # decompose() separates your ts in to original data, trend, noise, seasonality
  # http://maths-people.anu.edu.au/~johnm/courses/r/ASC2008/pdf/Rtimeseries-ohp.pdf
  dec = decompose(ts_obj)
  # Extract only the column
  trend = dec$trend
  # Create dataframe with the original and trend data
  df_trend = data.frame(date = time(xts_obj),original = coredata(xts_obj),trend = coredata(trend))
  
  # Graph original and trend
  n_tweets_plot <- ggplot()+
    geom_line(aes(x=date, y=trend, lty="Tendencia"),df_trend) +
    geom_line(aes(x=date, y=original, lty="Datos"),df_trend,colour="#0072B2") +
    ggtitle(paste("Numero de likes/cantidad de tweets de ",i )) +
    labs(x="Fecha",y="Numero de likes/cantidad de tweets" ) +
    #xlim(as.POSIXct("2013-01-01"), as.POSIXct("2017-04-01")) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme_classic()
  print(n_tweets_plot)
  
  
}

# Loop for ploting rt/cantidad de tweets
for (i in list_delegations){
  minidf = df_general %>%
    filter(delegacion == i)
  
  ## Now we extract trend
  # Build xts object
  xts_obj <- as.xts(minidf$rt_t, order.by=minidf$first_day_of_week, frequency = 4)
  # replace nan with 0
  xts_obj[is.nan(xts_obj)] <- 0
  # Build a ts from xts with frecuency 4 (nearly 4 weeks in one month)
  ts_obj = ts(xts_obj, frequency = 4)
  # decompose() separates your ts in to original data, trend, noise, seasonality
  # http://maths-people.anu.edu.au/~johnm/courses/r/ASC2008/pdf/Rtimeseries-ohp.pdf
  dec = decompose(ts_obj)
  # Extract only the column
  trend = dec$trend
  # Create dataframe with the original and trend data
  df_trend = data.frame(date = time(xts_obj),original = coredata(xts_obj),trend = coredata(trend))
  
  # Graph original and trend
  n_tweets_plot <- ggplot()+
    geom_line(aes(x=date, y=trend, lty="Tendencia"),df_trend) +
    geom_line(aes(x=date, y=original, lty="Datos"),df_trend,colour="#0072B2") +
    ggtitle(paste("Numero de retweets/cantidad de tweets de ",i )) +
    labs(x="Fecha",y="Numero de retweets/cantidad de tweets" ) +
    #xlim(as.POSIXct("2013-01-01"), as.POSIXct("2017-04-01")) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme_classic()
  print(n_tweets_plot)
  
  
}



