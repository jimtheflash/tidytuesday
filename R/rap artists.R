library(tidyverse)
library(formattable)
library(plotly)
library(htmlwidgets)

# get the data from github
polls <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# top 10 list
rankings %>%
  arrange(desc(points)) %>%
  transmute(Rank = row_number(),
            Song = title,
            Artist = artist,
            Year = year,
            `Total Points` = points,
            `Total Votes` = n,
            `First Place Votes` = n1) %>%
  filter(Rank <= 10) %>%
  formattable(.,
              list(align = c('r', 'l', 'l', 'r', 'r', 'r'),
                   `Total Points` = color_bar()))

# make data for plotting
plot_data <- rankings %>%
  mutate(Year = year) %>%
  group_by(Year) %>%
  summarise(`Total Points` = sum(points),
            `Total Votes` = sum(n)) %>%
  ungroup()

# which years had the most rankings points?
points_by_year <- ggplot(plot_data, aes(Year, `Total Points`)) +
  geom_bar(stat = 'identity', width = .9, 
           alpha = .8, fill = '#4E2A84') +
  scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.ticks.x = element_line(colour = 'grey'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(x = NULL, y = NULL,
       title = "Total Ranking Points By Year")
# view it as a plotly  
ggplotly(points_by_year)
# export
saveWidget(ggplotly(points_by_year), 'rap_rankings_by_year_barplot.html', 
           selfcontained = FALSE)
                        
# which years had the most votes?
votes_by_year <- ggplot(plot_data, aes(Year, `Total Votes`)) +
  geom_bar(stat = 'identity', width = .9, 
           alpha = .8, fill = '#4E2A84') +
  scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.ticks.x = element_line(colour = 'grey'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(x = NULL, y = NULL,
       title = "Total Votes By Year")
# view it as a plotly  
ggplotly(votes_by_year)
# export
saveWidget(ggplotly(votes_by_year), 'rap_votes_by_year_barplot.html', 
           selfcontained = FALSE)
