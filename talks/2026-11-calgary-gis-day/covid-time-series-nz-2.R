library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(stringr)

# library(wesanderson)
# library(Manu)
library(ggpubr)
library(grid)

library(readxl)
library(readODS)

library(patchwork)

setwd("/Users/david/Documents/code/dosull.github.io/talks/2026-11-calgary-gis-day")

c19 <- read_ods("cases.ods")

c19.all <- read.csv("https://raw.githubusercontent.com/UoA-eResearch/nz-covid19-data-auto/master/data.csv")
names(c19.all) <- c("Date", "Status", "Sex", "Age", "DHB", "Overseas", "Historical")
c19.all$Date <- as.Date(parse_date_time(c19.all$Date, "ymd"))

c19.community <- c19.all |>
  filter(Overseas == "No") |>
  group_by(Date) |>
  summarise(Source.ANZ = n())

c19.overseas <- c19.all |>
  filter(Overseas == "Yes") |>
  group_by(Date) |>
  summarise(Source.overseas = n())

c19.unknown <- c19.all |>
  filter(Overseas == "Unknown") |>
  group_by(Date) |>
  summarise(Source.not.known = n())

c19$Date <- as.Date(parse_date_time(c19$Date, "ymd"))
  max.date <- max(c19$Date)
  min.date <- min(c19$Date)

c19 <- c19 |> 
  left_join(c19.community) |>
  left_join(c19.unknown) |>
  left_join(c19.overseas)

alert.levels <- c19 |>
  dplyr::select(Date, Level) |>
  pivot_longer(-Date) |>
  rename(variable = name) |>
  arrange(Date, variable) |>
  filter(Date > min.date, Date <= max.date)

# This code determines the total cases from the provided spreadsheet input
# by cumulative calculation, but leads to inconsistency with the reported daily numbers... ¯\_(ツ)_/¯
c19 <- c19 |>
mutate(Overseas = cumsum(replace_na(Source.overseas, 0)),
       Not.known = cumsum(replace_na(Source.not.known, 0)),
       In.ANZ = cumsum(replace_na(Source.ANZ, 0)),
       Total.cases =  Overseas + Not.known + In.ANZ)

grand_totals <- c19 |> 
  dplyr::select(Date, Total.cases, Active.cases) |>
  pivot_longer(-Date) |>
  dplyr::rename(variable = name) |>
  arrange(Date, variable) |>
  filter(Date >= min.date, Date <= max.date)

sub_totals <- c19 |> 
  dplyr::select(Date, In.ANZ, Not.known, Overseas) |>
  pivot_longer(-Date) |>
  dplyr::rename(variable = name) |>
  arrange(Date, variable) |>
  filter(Date >= min.date, Date <= max.date)

changes_down <- c19 |>
  mutate(Recovered = -New.recovered, 
          Dead = -New.dead) |>
  dplyr::select(Date, Recovered, Dead) |>
  pivot_longer(-Date) |>
  rename(variable = name) |>
  arrange(Date, variable) |>
  filter(Date >= min.date, Date <= max.date)

changes_up <- c19 |>
  dplyr::select(Date, Source.ANZ, Source.not.known, Source.overseas) |>
  pivot_longer(-Date) |>
  rename(variable = name) |>
  arrange(Date, variable) |>
  filter(Date >= min.date, Date <= max.date)

var.names <- c(
  "Total.cases", "Active.cases", "Source.ANZ", "Source.not.known",
  "Source.overseas", "Recovered", "Dead")
my.colours <- c(
  "red", "orange", "indianred4", "indianred2", 
  "#ffaaaa", "seagreen", "black")


totals_plot <- function(date_range, yrange, interval = "month") {
  ggplot(alert.levels) +
    geom_rect(aes(xmin = lag(Date), xmax = Date,
                  ymin = yrange[1], ymax = yrange[2],
                  alpha = value)) +
    scale_alpha(range = c(0.1, 0.6)) +
    geom_line(data = grand_totals,
              aes(x = Date, y = value, colour = variable, group = variable)) +
    scale_colour_manual(breaks = var.names, values = my.colours) +
    scale_x_date(
      date_breaks = interval, date_labels = '%B %Y', limits = date_range) + 
    scale_y_continuous(limits = yrange) +
    labs(x = "Date", y = 'People') +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    guides(alpha = guide_legend(title = "Alert levels")) + 
    labs(fill = "Alert levels", colour = "Counts")
}

diffs_plot <- function(date_range, yrange, interval = "month") {
  ggplot(alert.levels) +
    geom_rect(aes(xmin = lag(Date), xmax = Date,
                  ymin = yrange[1], ymax = yrange[2],
                  alpha = value)) +
    scale_alpha(range = c(0.1, 0.6)) +
    geom_col(data = bind_rows(changes_up, changes_down), 
             aes(x = Date, y = value, fill = variable), width = 1) +
    scale_fill_manual(breaks = var.names, values = my.colours) +
    scale_x_date(
      date_breaks = interval, date_labels = '%B %Y', limits = date_range) + 
    scale_y_continuous(limits = yrange) + 
    labs(x = "", y = expression(paste(Delta, ' People'))) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    guides(alpha = "none") + 
    labs(fill = "Details", colour = "Counts")
}

dates <- c(
  as.Date("2020-02-26"),
  as.Date("2020-06-01"),
  as.Date("2021-08-01"),
  as.Date("2021-12-31"),
  as.Date("2022-02-25")
)

g0 <- totals_plot(dates[1:2], c(0, 1750))
g0d <- diffs_plot(dates[1:2], c(-120, 100))
# g0 / g0d

g00 <- totals_plot(dates[2:3], c(0, 3000), "3 months")
g00d <- diffs_plot(dates[2:3], c(-120, 100), "3 months")
# g00 / g00d

g000 <- totals_plot(dates[3:4], c(0, 15000))
g000d <- diffs_plot(dates[3:4], c(-200, 250))
# g000 / g000d

g1 <- totals_plot(dates[c(1, 5)], c(0, 95000), "3 months")
g1d <- diffs_plot(dates[c(1, 5)], c(-300, 12500), "3 months")
# g1 / g1d


all.data <- c19 |>
  dplyr::select(Date, Active.cases, Dead, 
                Source.overseas, Source.not.known, Source.ANZ, 
                Recovered, Total.cases) |>
  pivot_longer(-Date) |>
  rename(variable = name) |>
  arrange(Date, variable) |>
  filter(Date >= min.date, Date <= max.date)

all.data$value <- replace(all.data$value, all.data$value == 0, NA)

g2 <- ggplot(alert.levels) +
  geom_rect(
    aes(xmin = lag(Date), xmax = Date,
        ymin = 1, ymax = max(all.data$value, na.rm = TRUE),
    alpha = value)) +
  scale_alpha(range = c(0.1, 0.6)) +
  geom_line(
    data = all.data,
    aes(x = Date, y = value, colour = variable, group = variable)) +
  scale_colour_manual(breaks = var.names, values = my.colours) + 
  scale_y_log10(labels = scales::comma) +
  scale_x_date(date_breaks = '3 months', date_labels = '%b %y') + 
  labs(x = "", y = 'People') +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  guides(alpha = "none", fill = "none") + 
  labs(colour = "Counts")

# g2

