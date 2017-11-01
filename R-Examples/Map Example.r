#Plotting County Data in R Using geom_map()
#Author: Blake Madden

#This script demonstrates how to plot city and county-level data
#onto a state map. In this example, we will be plotting store locations
#for a fictitious bakery company in Ohio. Our goal will be to see
#how various cities and counties compare to each other in terms of how
#many store locations they have. We will accomplish this by creating
#datasets of our bakeries and then building geographical heatmaps
#showing the density of their locations.

require(ggplot2)
require(maps)
require(mapproj)
require(dplyr)
require(ggrepel)

location.data <- data.frame(county = c('montgomery', 'miami', 'miami',
                                       'franklin', 'montgomery',
                                       'montgomery', 'miami'),
                            city = c('dayton', 'piqua', 'troy',
                                     'columbus', 'vandalia',
                                     'huber heights', 'tipp city'),
                            count = c(5, 2, 7, 13, 8, 1, 2))

location.data.by.county <- location.data %>%
  #split the data into county groups
  group_by(county) %>%
  #then tally up the number of locations for each group
  summarize(count = sum(count))

ohio.map.data <- map_data('county', 'ohio')
names(ohio.map.data)[5:6] <- c('state', 'id')

ohio.map <- ggplot() +
  #remove decorations so that we only see the county outlines
  #adjust the font on the legend
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8)) +
  #plot an outline of the counties
  geom_map(aes(map_id = ohio.map.data$id), map = ohio.map.data, fill='white', color='black') +
  #plot the (county-level) location data
  geom_map(data=location.data.by.county, aes(map_id = county, fill=count),
           map = ohio.map.data, color='black') +
  #change the color scheme of the county legend and also customize its title
  scale_fill_gradient2(low='white', high='blue', name='County level') +
  labs(title = 'Bakery Locations') + #add a title
  theme(plot.title = element_text(hjust = 0.5)) + #center the title
  coord_map() +
  #adjust the axes to fit the state outline
  expand_limits(x = ohio.map.data$long, y = ohio.map.data$lat)

print(ohio.map)

#remembering that the 'id' column in ohio.map.data is the name of the county,
#we are asking to get only the coordinates data where the county name can be
#found in our list of bakery counties.
county.map.data <- filter(ohio.map.data, id %in% location.data$county)

city.map.data <- data.frame(city = c('dayton', 'piqua', 'troy', 'columbus',
                                     'vandalia', 'huber heights', 'tipp city'),
                            long = c(-84.199632, -84.24023, -84.20422,
                                     -83.00275, -84.19298, -84.12738,
                                     -84.176),
                            lat = c(39.777394, 40.14761, 40.04003,
                                    39.96199, 39.8913, 39.86229, 39.96109))

location.data <- location.data %>% inner_join(city.map.data , by = 'city')

highest.store.density <- location.data %>% group_by(county) %>% slice(which.max(count))

county.map <- ggplot() +
  #remove decorations so that we only see the county outlines
  #adjust the font on the legend
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8)) +
  #plot the counties, where their colors are scaled to their number of stores
  geom_map(data=location.data.by.county, aes(map_id = county, fill=count),
           map = county.map.data, color='black') +
  scale_fill_gradient2(low='white', high='blue', name='County level') +
  #plot the stores by city
  geom_point(data = location.data,
             #make the city points red and translucent
             #(to make any possible overlaps easier to see)
             color='red',
             alpha=.75,
             aes(x=long, y = lat,
                 #the points on the cities will be scaled to their
                 #respective number of stores
                 size=count)) +
  #customize the city level title, color scheme, and flip the order so that the legends
  #are going in the same direction
  scale_colour_gradient(low = 'pink', high = 'red') +
  labs(size='City level', title = 'Bakery Locations',
       caption = 'Cities with the most locations in each county are labeled') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, size = 10)) + #center the title
  guides(size = guide_legend(reverse = TRUE)) +
  #place labels on cities with the most locations in each county.
  #note that we use ggrepel use to prevent any possible label overlapping.
  ggrepel::geom_label_repel(aes(x=long, y = lat, label = city),
                            data=highest.store.density ) +
  #adjust the mapping to the curvature of the earth
  coord_map() +
  #fit the axis scaling to the counties that we are plotting
  expand_limits(x = county.map.data$long, y = county.map.data$lat)

print(county.map)

indiana.map.data <- map_data('county', 'indiana')
names(indiana.map.data)[5:6] <- c('state', 'id')

kentucky.map.data <- map_data('county', 'kentucky')
names(kentucky.map.data)[5:6] <- c('state', 'id')

pennsylvania.map.data <- map_data('county', 'pennsylvania')
names(pennsylvania.map.data)[5:6] <- c('state', 'id')

ohio.map <- ggplot() +
  #remove decorations so that we only see the county outlines,
  #adjust the font on the legend
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8)) +
  geom_map(aes(map_id = ohio.map.data$id),
           map = ohio.map.data, fill='white', color='black') +
  geom_map(data=location.data.by.county, aes(map_id = county, fill=count),
           map = ohio.map.data, color='black') +
  scale_fill_gradient2(low='white', high='blue', name='County level') +
  #draw additional states (use different outline colors for each state)
  geom_map(aes(map_id = indiana.map.data$id), map = indiana.map.data,
           fill='white', color='sandybrown') +
  geom_map(aes(map_id = kentucky.map.data$id), map = kentucky.map.data,
           fill='white', color='royalblue') +
  geom_map(aes(map_id = pennsylvania.map.data$id), map = pennsylvania.map.data,
           fill='white', color='slategrey') +
  #draw titles
  labs(title = 'Bakery Locations',
       subtitle = 'Quad-state Area',
       caption = 'Note: store location data currently only available for Ohio') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, size = 10)) +
  coord_map() +
  #expand the axes to fit all of the longitude/latitude points
  #for all four states
  expand_limits(x = c(ohio.map.data$long,
                      indiana.map.data$long,
                      kentucky.map.data$long,
                      pennsylvania.map.data$long),
                y = c(ohio.map.data$lat,
                      indiana.map.data$lat,
                      kentucky.map.data$lat,
                      pennsylvania.map.data$lat))

print(ohio.map)