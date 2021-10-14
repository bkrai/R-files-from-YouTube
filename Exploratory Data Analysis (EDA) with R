# Data Manipulation & Visualization

# 7 Package 
library(dplyr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(openintro)
library(fiftystater)
library(colorplaner)

# Data
vehicle <- read.csv(file.choose(), header = T)
car <- as_data_frame(vehicle)
car

# Filter
car %>%
         filter(State=='CA' | State == 'TX' | State=='FL')

car %>%
         filter(State=='CA', ---)

# Arrange
car %>%
         filter(State=='CA' | State == 'TX' | State=='FL') %>%
         arrange(desc(Mileage))

# Summarise
car %>%
         summarise(Avg_lc = mean(lc),
                   sd_lc = sd(lc),
                   max_lc = max(lc),
                   min_lc = ---,
                   sum_lc = ---,
                   median_lc = ---,
                   total = n())

# Group by
car %>%
         group_by(State) %>%
         summarise(Avg_lc = mean(lc),
                   sd_lc = sd(lc),
                   max_lc = max(lc),
                   min_lc = ---,
                   sum_lc = ---,
                   median_lc = ---,
                   total = n()) %>%
         arrange(desc(Avg_lc))

# Mutate
car %>%
         group_by(State) %>%
         mutate(cph = sum(lc)/sum(lh)) %>%
         summarise(Avg_cph = ---,
                   Avg_mileage = ---) %>%
         arrange(desc(Avg_cph))

# Visualization 
# Histogram
car %>%
         filter(State=='CA' | State == 'TX' | State=='FL') %>%
         ggplot(aes(x=lc, fill = ---)) +
         geom_histogram(alpha=0.8, color='darkblue') +
         ggtitle('Labor cost in Top 3 states') +
         facet_wrap(~State)

# Density
car %>%
         filter(State=='CA' | State == 'TX' | State=='FL') %>%
         ggplot(aes(x=lc, fill = State)) +
         ---(alpha=0.5, color='darkblue') +
         ggtitle('Labor cost in Top 3 states') 

# Scatter
car %>%
         filter(State=='CA' | State == 'TX' | State=='FL') %>%
         ggplot(aes(x=lh, y = lc, col=State, size=mc)) +
         geom_point(alpha=0.5, color='darkblue') +
         geom_smooth(se=0) +
         facet_wrap(---)

# bar plot
new <- car %>%
         group_by(State) %>%
         mutate(cph = ---/---) %>%
         summarise(Avg_cph = mean(cph),
                   Avg_mileage = mean(Mileage)) %>%
         arrange(desc(Avg_cph))

ggplot(new, aes(x=State, y = Avg_cph, fill = State)) +
         geom_col() +
         coord_flip() +
         ggtitle('Cost per hour in 50 states')

# Box plot
car %>%
         group_by(State) %>%
         filter(n() >40) %>%
         ggplot(aes(x=---, y=---, col = State)) +
         geom_boxplot()

# Map-1
# Data preparation
new <- car %>%
         group_by(State) %>%
         summarise(total = n(),
                   Avg_mileage = mean(Mileage))
colnames(new) <- c('region', 'value', 'mileage')
new
new$region <- abbr2state(new$region)
new$region <- tolower(new$region)
new <- new[-1,]

state_choropleth(new,
                 title = "Car Failures in US",
                 legend = 'Number of Failures')

# Map-2
p <- ggplot(new, aes(map_id = ---)) +
         geom_map(aes(fill=---), map = fifty_states) +
         expand_limits(x=fifty_states$long, y=fifty_states$lat) +
         coord_map() +
         scale_x_continuous(breaks=NULL) +
         scale_y_continuous(breaks=NULL) +
         labs(x="", y="") +
         theme(legend.position = "bottom",
               panel.background = element_blank())
p + fifty_states_inset_boxes()
p + aes(fill2 = ---) + scale_fill_colourplane() +
         theme(legend.position = 'right') +
         ggtitle('Geo Map for Failures and Average Mileage')
