bus <- read.csv("Stack/bus_data.csv")
View(bus)
#1.	Import and review data   (20pts)
#Load the tab delimited bus data file ('bus_data.tab') and inspect it.

#a.	Are there any attributes in the dataset that are not useful for analysis or for making predictions?  Why?
#I think dirtag, predictable, and speedkmhr are not useful for pca analysis largely because they are not entirely numerical 
#and some have little variation.

#b.	Do any of the variables appear to be treated as discrete even though they actually represent continuous values? 

# Yes there are, lat and long appear to be descrete but are actually contious values of geography. 

#c.	Do any of the variables seem to represent the same data? 

#Epoch and Local seem to be representing the same data. I also think time and utc are representing the same time. 

#d.	What do you think the dirTag attribute represents? 
  
#Some type of directory tag that involves how the data is being collected. 

  
#2.	Inspect the distributions of attributes and selecting data (10pts)


#a.	Now inspect the distribution of route numbers. Are there any bus routes that should not be included in our 1 bus analysis? If so, remove those.
# Yes I think we should remove the number 43 and 66 from the column d.routetag.

#one.bus <- bus[-c(12356, 12352, 10799, 10559, 10556, 755, 753, 526, 19, 18, 9975,5953, 5950, 641),]

bus.one_complete <- bus.one[!(is.na(bus.one$dirTag) | bus.one$dirTag == ""),]

View(new.one.bus)
               
#b.	Is there anything else we should filter that is not a complete attribute?
# Yes,  predictable, speedkm, and dirTag should be filtered as they are not complete.   
  
#3.	Selecting attributes to analyze (10pts)


#a.	Which attributes should be excluded from analysis? Are any of them perfectly correlated? Should we include all of those?
#vpredictable, speedkm, and dirTag should be excluded. I do not see that any of them are correlated. 

#b.	If you want to 'label' points in your analysis with the direction the buses are heading, which attribute do you need?
# I would use the attribute heading, which appears to show a 360 degree heading of the direction the bus is moving. There are some negative attributes,  
#however these also appear to be misleading in other coulmns. By all accounts the heading column seems to show the direction of travel.


#4.	Select Data (10pts)
#Filter out the rows that have irrelevant or undefined values for dirTag, and those that have any routeTag other than 1.

new.one.bus <- one.bus[!(one.bus$predictable ==FALSE), ]

new.one.bus2 <- one.bus[!(is.na(one.bus$dirTag) | one.bus$dirTag == ""),]

View(new.one.bus)

#new.one.bus2 <- na.omit(new.one.bus,cols = seq_along(new.one.bus, invert=FALSE,...))

install.packages('janitor')

#new.one.bus2 <-new.one.bus[-which(apply(new.one.bus,1,function("dirTag")all(is.na("dirTag")))),]
                        
#new.one.bus2 <- new.one.bus[-c(299, 300, 472, 3359, 3766, 3994, 4212, 4325, 6524, 9291, 9595, 9619, 10277, 10875, 10899, 11192, 11275, 12048, 12463),]

view(new.one.bus2)

#5.	View the route (15pts)
#a.	Choose features that will help you plot the geographic locations of the route on a graph. Save the graph you produce.

new.one.bus2 %>% leaflet(width = '100%') %>% addTiles() %>%
  setView(-78.8310, 35.9867, zoom = 10) %>%
  addMarkers(lat = ~ lat,
             lng = ~ lon,
             popup = new.one.bus2$dirTag)

install.packages("leaflet")

#ggplot(data = new.one.bus2), aes(x = new.one.bus2$lat, y = new.one.bus2$lon)

install.packages("ggplot2")

ggplot() +  geom_point(data=new.one.bus2, aes(x=lat, y=lon), color="red")


#b.	How closely does this mesh with the actual bus route? Can you guess what is happening when there are any deviations from the actual route? 
#I think this pretty closely resembles the actual bus route. It is clear that the bus occasionally starts or ends in a different location, or goes slightly off route. 


#6.	Bus time and frequency (15pts)
#We want to understand when the buses run and their frequency throughout the 24 hour period. 
#a.	Plot out the frequency of the bus observations by two hour increments. Save the graph you produce.

plot(new.one.bus2$secondsPastMidnight, new.one.bus2$vehicleId, ylab = "Bus ID", xlab = "Seconds Past Midnight")

busfrequency <- table(new.one.bus2$secondsPastMidnight/7200)
busfreq_his<- hist(busfrequency, breaks = 12)
plot(new.one.bus2$secondsPastMidnight/7200, new.one.bus2$vehicleId)

#b.	What are the peak periods when the buses run most frequently?  
 # Hint: You can view the number of rows that appear by time interval, and then you can make the number of different time intervals 12 instead of the default value, representing 2 hour windows.

#It seems that the most number of trips on the bus routes take place between 4AM and 6AM as well as 8AM and 10AM.

#C. PCA (20pts)
#Perform PCA analysis on this data. 

bus.pca <- new.one.bus2[,-c(2,3,5,7,9,10,13,14,15)]

View(bus.pca)

bus.correlation <- cor(bus.pca)

install.packages("factoextra")
library(factoextra)

pca.bus.complete <- prcomp(bus.pca, center = TRUE, scale. = TRUE)

fviz_pca_var(pca.bus.complete)
fviz_pca_biplot(pca.bus.complete)

bus.correlation.comp <- prcomp(bus.correlation, center = TRUE, scale. = TRUE)


fviz_pca_var(bus.correlation.comp)
fviz_pca_biplot(bus.correlation.comp, label= "NONE")
fviz_pca_biplot(pca.bus.complete, label = "none")

summary(pca.bus.complete)
summary(bus.correlation.comp)


#1.	How many principal components account for almost all (about equal to 100%) of the variance? Inspect the components. Comment on the features that form the primary "ingredients" in each of these principal components. 
#I see 6 components.
#2.	In one or two words discuss what the first, second and third principal components represent.  Discuss why these features dominate the variance.
#1st component is the component with the most variance, the 2nd component is 2nd greatest variance on the second coordinate. 


#3.	Plot the data as projected onto two principal components. Find a scatter plot that well separates the two direction tags. Save the plot. You should see that most of the points cluster into two mostly rectangular blocks. How do you interpret these blocks? How do you interpret the points that lie outside of these blocks? Hint: Remember what you said these principal components represent in the previous question.
## I interpret the blocks to indicate that there are two key time-frames which have the most amount of buses running the routes. 

