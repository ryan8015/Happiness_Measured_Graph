library(tidyverse)
library(sf)
library(rvest)
library(stringr)
library(scales)
library(viridis)
library(ggplot2)
library(dplyr)


happiness = read.csv("2019.csv")
happiness
head(happiness)
colnames(happiness) <- c('rank','country','score','GDP','Social','life','freedom','generosity','corruption')
head(happiness)
plot(x=happiness$Score,y=happiness$Country.or.region)

map.world <- map_data('world')

anti_join(happiness, map.world, by = c('country' = 'region'))

map.world %>%
  group_by(region) %>%
  summarise() %>%
  print(n = Inf)

happiness <- happiness %>%  mutate(country = recode(country, `United States` = 'USA'
                                              , `United Kingdom` = 'UK'
                                              , `Congo, Democratic Republic of the` = 'Democratic Republic of the Congo'
                                              , `Trinidad and Tobago` = 'Trinidad'
                                              , `Congo, Republic of the` = 'Republic of Congo'
)
)



finalHappy <- left_join( map.world, happiness, by = c('region' = 'country'))
finalHappy


ggplot(finalHappy, aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = life)) +
  scale_fill_viridis_c(option = "A", trans = "sqrt") +
  guides(fill = guide_legend(reverse = T)) +
  labs(fill = 'Life Expectancy Measure'
       ,title = 'Life Expectancy By Country'
       ,subtitle = 'Life Expectancy measured in 2019'
       ,x = NULL
       ,y = NULL) +
  theme(text = element_text(color = '#EEEEEE')
        ,plot.title = element_text(size = 28)
        ,plot.subtitle = element_text(size = 14)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = '#333333')
        ,plot.background = element_rect(fill = '#333333')
        ,legend.position = c(.18,.36)
        ,legend.background = element_blank()
        ,legend.key = element_blank()
  ) +
  annotate(geom = 'text'
           ,label = 'Information was sourced from Kaggle'
           ,x = 18, y = -55
           ,size = 3
           ,color = '#CCCCCC'
           ,hjust = 'left'
  )

ggplot(finalHappy, aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = GDP)) +
  scale_fill_viridis_c(option = "D", trans = "sqrt") +
  guides(fill = guide_legend(reverse = T)) +
  labs(fill = 'GDP Per Capita'
       ,title = 'GDP By Country'
       ,subtitle = 'GDP measured in 2019'
       ,x = NULL
       ,y = NULL) +
  theme(text = element_text(color = '#EEEEEE')
        ,plot.title = element_text(size = 28)
        ,plot.subtitle = element_text(size = 14)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = '#333333')
        ,plot.background = element_rect(fill = '#333333')
        ,legend.position = c(.18,.36)
        ,legend.background = element_blank()
        ,legend.key = element_blank()
  ) +
  annotate(geom = 'text'
           ,label = 'Information was sourced from Kaggle'
           ,x = 18, y = -55
           ,size = 3
           ,color = '#CCCCCC'
           ,hjust = 'left'
  )

ggplot(finalHappy, aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = corruption)) +
  scale_colour_gradient(
    low = muted("red"),
    mid = "white",
    high = muted("blue"),
    midpoint = 0,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  guides(fill = guide_legend(reverse = T)) +
  labs(fill = 'Percentage of Corruption'
       ,title = 'Corruption By Country'
       ,subtitle = 'Corruption measured in 2019'
       ,x = NULL
       ,y = NULL) +
  theme(text = element_text(color = '#EEEEEE')
        ,plot.title = element_text(size = 28)
        ,plot.subtitle = element_text(size = 14)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = '#333333')
        ,plot.background = element_rect(fill = '#333333')
        ,legend.position = c(.18,.36)
        ,legend.background = element_blank()
        ,legend.key = element_blank()
  ) +
  annotate(geom = 'text'
           ,label = 'Information was sourced from Kaggle'
           ,x = 18, y = -55
           ,size = 3
           ,color = '#CCCCCC'
           ,hjust = 'left'
  )