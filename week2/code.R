## Importing packages

library(tidyverse)
library(glue)
library(scales)
library(ggforce)
library(bbplot)
library(patchwork)

windowsFonts(Helvetica = "Product Sans")

## Defining custom theme

theme_lox <- function() {
  theme(panel.grid.major.x = element_line(size = 0.2, colour = "#cbcbcb"),
        panel.grid.major.y = element_line(size = 0.2, colour = "#cbcbcb"),
        plot.title = element_text(family = 'Helvetica', size = 22, face = "bold", color = "#222222"),
        plot.subtitle = element_text(family = 'Helvetica', size = 14, margin = margin(6,0,6,0)),
        plot.caption = element_text(family = 'Helvetica', face = 'bold'),
        axis.text = element_text(family = "Helvetica", size = 12, color = "#222222"),
        axis.text.y = element_text(face = 'bold')
  )
}

## Importing and cleaning data

rainfall <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

rainfall <- rainfall %>%
  unite(date, year, month, day, sep = "-") %>%
  mutate(date = lubridate::as_date(date))

yearly_rainfall <- rainfall %>% 
  group_by(year = lubridate::year(date), month = lubridate::month(date)) %>% 
  summarise(rainfall = mean(rainfall, na.rm = T)) %>%
  group_by(year) %>%
  summarise(rainfall = sum(rainfall, na.rm = T)) %>%
  filter(year >= 1900 & year < 2020) %>%
  ungroup()

yearly_temperature <- temperature %>%
  filter(temp_type == "max") %>%
  group_by(year = lubridate::year(date)) %>%
  summarize(temperature = mean(temperature, na.rm = T))

## Building plots

rain <- ggplot(yearly_rainfall, aes(x = year, y = rainfall)) + 
  geom_path(color = "#4c8ca8") + 
  geom_point(color = "#4c8ca8") +
  geom_mark_circle(aes(filter = year == 2019, label = glue("Total rainfall: {round(rainfall, 2)} mm"), description = "Annual rainfall is at a 60-year low"), 
                   label.buffer = unit(3, "cm"), label.fontsize = 10, label.family = 'Helvetica', con.linetype = 2) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(limits = c(1900, 2020), breaks = seq(1900, 2020, 10)) +
  scale_y_continuous(limits = c(10, 50), breaks = seq(10, 50, 5), labels = label_number_si(unit = 'mm')) +
  bbc_style() +
  theme_lox()

temp <- ggplot(yearly_temperature, aes(x = year, y = temperature)) + 
  geom_path(color = "#940713") + 
  geom_point(color = "#940713") +
  geom_mark_circle(aes(filter = year == 2019, label = glue("Avg. max temperature: {round(temperature, 1)}°C"), description = "Temperature has spiked considerably"),
                   label.buffer = unit(9, "cm"), label.fontsize = 10, label.family = 'Helvetica', con.linetype = 2) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(limits = c(1900, 2020), breaks = seq(1900, 2020, 10)) +
  scale_y_continuous(limits = c(15, 30), breaks = seq(15, 30, 2.5), labels = label_number_si(unit = '°C', sep = ''), position = 'right') +
  bbc_style() +
  theme_lox()

## Stitching plots together

plot <- (rain / temp) + 
  plot_annotation(title = "What do low rainfall and high temperatures in Australia signal?",
                  subtitle = "The raging fires in Australia are not independent of climate change",
                  caption = "Data: Australian Bureau of Meterology | Graphic: @lakshyaag",
                  theme = theme_lox())

## Exporting plot

ggsave(filename = 'week2/week2.png', plot, width = 15, height = 7, units = 'in')