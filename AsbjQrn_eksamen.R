install.packages("ggplot2")
install.packages("dplyr")

library('dplyr')
library("ggplot2")



mydata = read.csv("share-of-violent-deaths-non-state-societies.csv")  # read csv file 

mydata1 = read.csv("rate-of-violent-deaths-state-societies.csv")  # read csv file, is deaths pr 100.000
mydata2 = read.csv("share-of-violent-deaths-state-societies.csv")  # read csv file 
mydata3 = read.csv("share-of-violent-deaths-prehistoric-archeological-sites.csv")  # read csv file 

# mydata1 and mydata2 are modern, mydata and mydata3 are premodern, except mydata2[1,] which is ancient mexico

# create mean mortality premodern and modern, respectively

premodern <- merge(mydata, mydata3)

#calculating premodern average mortality

premodernsum <- mydata2[1,4] + sum(mydata[,4]) + sum(mydata3[,4])

premodern_number_rows <- nrow(mydata) + nrow(mydata3) + 1 # +1 since we include ancient mexico from mydata2

avg_premodern_mortality <- premodernsum/premodern_number_rows

#calculating modern average mortality

percentage_mortality <- mydata1[,4]/1000

modernsum <- sum(percentage_mortality) + sum(mydata2[2:5,4]) 

modern_number_rows <- length(mydata2[2:5,4]) + length(percentage_mortality) # +1 since we include ancient mexico from mydata2

avg_modern_mortality <- modernsum/modern_number_rows 

# Plotting modern vs premodern mortality rates


combined_mortality <- rbind(avg_modern_mortality,avg_premodern_mortality)
#data_frame(val = combined_mortality)

val <- tibble(combined_mortality)

names(val)[1] <- "Mortality"

library("ggplot2")

name = c("Modern","Premodern")


plot2 <- ggplot(val, aes(x=name , y=Mortality)) + geom_bar(stat = "identity") + ggtitle("Violent deaths pr 100.000") + ylab("Violent deaths (1000)") + xlab("Time Period")  # Y axis is explicit. 'stat=identity'
print(plot2)

