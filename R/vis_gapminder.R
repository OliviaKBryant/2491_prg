# Activity 1

# Make a plot showing how the relationship between GDP, life expectancy and population varies over time and continent

library(tidyverse)

# if gapminder isn't installed, load the data file from this repo
if (require(gapminder)){
    data(gapminder)
  } else {
    gapminder <- read_csv("../data/gapminder.csv")
  }

gapminder_example <- 
  ggplot(data = gapminder,
         aes(x = gdpPercap, y = lifeExp)) +
  geom_path(aes(group = country, color = continent)) +
  geom_point(aes(color = continent, size = pop), shape = 1) +
  scale_color_brewer(palette = "PuOr") +
  scale_x_log10() +
  annotation_logticks(sides = 'b') +
  facet_wrap( ~ continent, scales = 'free', nrow = 1) +
  scale_size_area() +
  theme_dark() +
  labs(x = "GDP", y = "LE",
       title = "GDP and Life Expectancy 1952-2007") +
  theme(legend.position = 'bottom',
        legend.box      = 'vertical',
        axis.text.x = element_text(angle = 45, hjust = 1))
gapminder_example

if (!dir.exists('./results')){dir.create('./results')}

ggsave(filename = "results/gapminder_example.pdf",
       plot = gapminder_example, width = 8, height = 4, 
       units = "in")

ggsave(filename = "results/gapminder_example.png",
       plot = gapminder_example, width = 8, height = 4, 
       units = "in", dpi = 150)

# good: colours do contrast with the background, title is present, splitting by continent is quite good

# bad: we don't need population, colours not needed, years are not represented/no concept of time, scale
# is different for every continent, axes poorly labelled, doesn't make it clear that it's per capita,
# can't differentiate between countries, background is too dark and grid lines are too prominent, could
# change the scale to per 100/per million or have them either all use e+ whatever or none of them.

# Activity 2

# Activity 2a

# Build a graph which your group believes better shows the relationship between life expectancy and GDP
library(magrittr)
gapminder %<>% filter(year>2000)

gapminder_better <- 
  ggplot(data = gapminder,
         aes(x = gdpPercap/1000, y = lifeExp)) +
  geom_path(aes(group = country), color = "red",
            size = 0.8,
            arrow = arrow(type = "open", angle = 30, 
                          length = unit(0.05, "inches"))) +
  # geom_point(aes(color = continent, size = pop), shape = 1) +
  # scale_color_brewer(palette = "") +
  scale_x_log10(labels = ~sprintf("%g",.)) +
  annotation_logticks(sides = 'b') +
  facet_wrap( ~ continent, nrow = 1) +
  theme_bw() +
  labs(x = "GDP per Capita ($1000s)", y = "Life Expectancy (LE)",
       title = "GDP and Life Expectancy in 2002 and 2007") +
  theme(legend.position = 'bottom',
        legend.box      = 'vertical')
gapminder_better



# Activity 2b

# Make a plot which is as bad as possible while still attempting to honestly show the information
# (i.e. don't add things to the plot which can't be derived from the variables in the plot)

