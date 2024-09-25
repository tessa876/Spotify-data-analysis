setwd(“C:/spotify_2023”)

spotify_data <- read.csv("spotify_2023.csv")

# This is my first chart comparing Taylor Swift's streams over the years as she
# releases new music.
taylor_swift_data <- subset(spotify_data, artist == 'Taylor Swift',)

plot(taylor_swift_data$released_year  ~ taylor_swift_data$streams, 
     col = 'blue', xlab = 'Streams', ylab = "Year Released", 
     main = 'Change of Streams for Taylor Swift Over the Years')


# Calculate total streams for each artist
artist_totals <- aggregate(streams ~ artist, 
                           data = spotify_data, FUN = sum)

# Order artisits from most streams to least
top_artists <- artist_totals[order(artist_totals$streams, decreasing = TRUE), ]


# Take the top 5 artists
top_five_artists <- top_artists[-c(6:644), ]

#Change the parameters to make graph more readable 
par(mar = c(8, 9, 4, 4) + 0.05)

# Graphing the bar plot.
barplot(top_five_artists$streams, 
        names.arg = top_five_artists$artist, 
        col = "lightpink", 
        main = "Top 5 Artists by Total Streams",
        xlab = "Artist",
        ylab = "Total Streams", 
        las = 2,
        cex.names = 0.7,
        xpd = TRUE)  # Allow labels to be placed outside the plot area

# Move labels lower and more to the side
par(mgp = c(7, 1, 0)) 

# New graph describing the difference between spotify charts and apple charts

# Making new dataset with only song names and spotify charts

songs_spotify <- data.frame(track_name = c(spotify_data$track_name),
                            charts = c(spotify_data$in_spotify_charts))

# Ordering songs based on spotify charts

top_songs_spotify <- songs_spotify[order(songs_spotify$charts,
                                        decreasing = TRUE), ]

# Take the top 5 songs
top_five_spotify_songs <- top_songs_spotify[-c(6:953), ]

# Making new dataset with only song names and apple charts
songs_apple <- data.frame(track_name = c(spotify_data$track_name),
                            charts = c(spotify_data$in_apple_charts))

# Ordering songs based on apple charts
top_songs_apple <- songs_apple[order(songs_apple$charts, 
                                      decreasing = TRUE),]

# Take the top 5 songs
top_five_apple_songs <- top_songs_apple[-c(6:953), ]

par(mar = c(10, 9, 3, 5) + 0.05)

barplot( c(top_five_spotify_songs$charts, top_five_apple_songs$charts), 
        names.arg = c(top_five_spotify_songs$track_name, 
                     top_five_apple_songs$track_name),
        col = c("red","purple"), 
        main = "Top 5 Songs on Spotify V.S. Appple",
        xlab = "Songs",
        ylab = "Top Charts",
        las = 2,
        cex.names = 0.4,
        xpd = TRUE)  

legend("topleft", legend = c("Spotify", "Apple"), fill = c("red", "purple"),
       bty = "n")

# a) Test whether 50% of songs are in a major key using 𝛼 = 0.1. State the 
# hypotheses, calculate the Z- statistic and p-value using R, and state the relevant conclusion. (4 marks)


songs_in_major_key <- subset(spotify_data, mode == 'Major',)

songs_in_major_key <- 550
total_songs <- 953

p_hat <- songs_in_major_key / total_songs

mean_major <- 0.577

sd <- sqrt(total_songs * p_hat * (1 - p_hat)) 

z_stat <- (p_hat - 0.5) / (sqrt(0.5*(1-0.5)/total_songs))

z_stat <- 4.762

p_value <- 2*pnorm(-z_stat)

p_value <- 0.000001919

# this is the significance level to test the hypothesis
alpha <- 0.1

# Since our p_value is above our significance level we have reseason to 
# reject our null hypothesis and accept our alternative hypothesis

# b) Compute a 99% confidence interval for the proportion of songs that are in 
# a major key. You may use 𝑝 ≈ 0.5. Interpret the interval. (2 marks)

x_bar <- mean_major

crit_value <- qt(p=alpha/2, df=total_songs-1, lower.tail = FALSE)


x_bar - crit_value * sd / sqrt(total_songs)
x_bar + crit_value * sd / sqrt(total_songs)

# Confidence interval is (-2.363, 1.391) 


# Test whether the average beats per minute of a song is equal to 124 bpm 
#using 𝛼 = 0.05. State the hypotheses, calculate the test statistic and
# p-value using R, and state the relevant conclusion. (4 marks)



n <- total_songs 

sd <- 28.057

alpha <- 0.05

# H_0: mu = 124
# H_A: mu =/= 124

# Calculate the test statistic

alpha <- 0.05
n <- length(spotify_data$bpm)
x_bar <- mean(spotify_data$bpm)
s <- sd(spotify_data$bpm) 
mu_null <- 124

t_stat <- (x_bar - mu_null)/(s / sqrt(n))

t_stat <- -1.606

# Calculate the p-value

p_value <- 2*pt(t_stat, df=n-1)

p_value <- 0.10862

# Compute a 90% confidence interval for the average beats per minute of a song. 
# Interpret the interval. (2 marks)

crit_value <- qt(p=alpha/2, df=n-1, lower.tail = FALSE)


x_bar - crit_value * sd / sqrt(total_songs)
x_bar + crit_value * sd / sqrt(total_songs)

confidence_interval <- (120.8, 124.3)

# 90 % confidence interval is (120.8, 124.3)