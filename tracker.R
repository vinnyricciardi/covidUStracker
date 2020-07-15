library(tidyverse)
library(data.table)
library(lubridate)
library(leaflet)
library(devtools)
library(zoo)


df_case <- fread('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv', stringsAsFactors = F)
df_death <- fread('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv', stringsAsFactors = F)

df_case <- melt(df_case, 
                id.vars = names(df_case)[c(1:11)], 
                variable.name = 'dtime',
                value.name = 'case') %>%
  select(Combined_Key, dtime, case)

df_death <- melt(df_death, 
                id.vars = names(df_death)[c(1:12)], 
                variable.name = 'dtime',
                value.name = 'death')


df <- merge(df_case, df_death, by = c('Combined_Key', 'dtime'), all = T)

df$dtime <- parse_date_time(df$dtime, 'mdy')
df <- df %>% 
  group_by(Combined_Key) %>%
  arrange(dtime, .by_group = T) %>%
  mutate(dailyCases = case - lag(case),
         dailyDeaths = death - lag(death))

df$casePC <- df$dailyCases / df$Population
df$deathPC <- df$dailyDeaths / df$Population

# Trend State
df_trend_s <- df %>% 
  group_by(Province_State, dtime) %>%
  summarise(Population = sum(Population, na.rm = T),
            dailyCases = sum(dailyCases, na.rm = T),
            dailyDeaths = sum(dailyDeaths, na.rm = T),
            casePC = dailyCases / Population,
            deathPC = dailyDeaths / Population) %>%
  filter(Province_State %in% c(
    'Massachusetts', 'Maryland', 'California', 'Virginia',
    'District of Columbia', 'Washington'
  ))

plotly::ggplotly(
  ggplot(df_trend_s, aes(dtime, deathPC, color = Province_State)) +
    geom_line()
    # geom_smooth(se = F)
)


# Trend County
df_trend_c <- df %>% 
  filter(Combined_Key %in% c(
    'Orange, California, US', 
    'San Bernardino, California, US',
    'Worcester, Massachusetts, US',
    'Montgomery, Maryland, US'))

plotly::ggplotly(
  ggplot(df_trend_c, aes(dtime, deathPC, color = Combined_Key)) +
    geom_line()
    # geom_smooth(se=F)
)


# MAP
df_latest <- df %>% 
  filter(dtime == max(dtime) & deathPC > 0 & 
         !is.na(deathPC) & is.finite(deathPC))
  

leaflet(df_latest) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addCircleMarkers(lng = ~Long_,
                   lat = ~Lat, 
                   radius = df_latest$deathPC * 100000, 
                   opacity = df_latest$deathPC * 100000,
                   fill = T, 
                   popup = paste(
                     df_latest$Combined_Key, 
                     '<br>No. Deaths: ', df_latest$dailyDeaths,
                     '<br>Deaths per 100,000:', round(df_latest$deathPC * 100000, 2)),
                   color = 'red') 

