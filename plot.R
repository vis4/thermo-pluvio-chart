needs(tidyverse, directlabels, clipr, ggrepel)

# load data from DWD:

# yearly, seasonal and monthly averages for air temp



# yearly, seasonal and monthly averages for precipitation
precip <- read_delim('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/annual/precipitation/regional_averages_rr_year.txt',
                     delim=';', skip = 1) %>% 
  transmute(year=Jahr, time='Year', value=as.numeric(Deutschland)) %>% 
  bind_rows(
    read_delim('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/seasonal/precipitation/regional_averages_rr_autumn.txt',
               delim=';', skip = 1) %>% 
      transmute(year=Jahr,time='Autumn', value=as.numeric(Deutschland)),
    read_delim('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/seasonal/precipitation/regional_averages_rr_spring.txt',
               delim=';', skip = 1) %>% 
      transmute(year=Jahr,time='Spring', value=as.numeric(Deutschland)),
    read_delim('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/seasonal/precipitation/regional_averages_rr_summer.txt',
               delim=';', skip = 1) %>% 
      transmute(year=Jahr,time='Summer', value=as.numeric(Deutschland)),
    read_delim('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/seasonal/precipitation/regional_averages_rr_winter.txt',
               delim=';', skip = 1) %>% 
      transmute(year=Jahr,time='Winter', value=as.numeric(Deutschland)),
    read_delim('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/precipitation/regional_averages_rr_01.txt',
               delim=';', skip = 1) %>% 
      transmute(year=Jahr,time='January', value=as.numeric(Deutschland)),
    read_delim('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/precipitation/regional_averages_rr_02.txt',
               delim=';', skip = 1) %>% 
      transmute(year=Jahr,time='February', value=as.numeric(Deutschland)),
    read_delim('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/precipitation/regional_averages_rr_03.txt',
               delim=';', skip = 1) %>% 
      transmute(year=Jahr,time='March', value=as.numeric(Deutschland)),
    read_delim('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/precipitation/regional_averages_rr_04.txt',
               delim=';', skip = 1) %>% 
      transmute(year=Jahr,time='April', value=as.numeric(Deutschland)),
    read_delim('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/precipitation/regional_averages_rr_05.txt',
               delim=';', skip = 1) %>% 
      transmute(year=Jahr,time='May', value=as.numeric(Deutschland)),
    read_delim('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/precipitation/regional_averages_rr_06.txt',
               delim=';', skip = 1) %>% 
      transmute(year=Jahr,time='June', value=as.numeric(Deutschland)),
    read_delim('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/precipitation/regional_averages_rr_07.txt',
               delim=';', skip = 1) %>% 
      transmute(year=Jahr,time='July', value=as.numeric(Deutschland)),
    read_delim('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/precipitation/regional_averages_rr_08.txt',
               delim=';', skip = 1) %>% 
      transmute(year=Jahr,time='August', value=as.numeric(Deutschland)),
    read_delim('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/precipitation/regional_averages_rr_09.txt',
               delim=';', skip = 1) %>% 
      transmute(year=Jahr,time='September', value=as.numeric(Deutschland)),
    read_delim('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/precipitation/regional_averages_rr_10.txt',
               delim=';', skip = 1) %>% 
      transmute(year=Jahr,time='October', value=as.numeric(Deutschland)),
    read_delim('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/precipitation/regional_averages_rr_11.txt',
               delim=';', skip = 1) %>% 
      transmute(year=Jahr,time='November', value=as.numeric(Deutschland)),
    read_delim('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/precipitation/regional_averages_rr_12.txt',
               delim=';', skip = 1) %>% 
      transmute(year=Jahr,time='December', value=as.numeric(Deutschland))
  ) %>% 
  mutate(measure='precipitation')

# compute climate baseline (1961-1990)
temp.base <- temp %>% 
  filter(year>1960 & year <= 1990) %>% 
  group_by(time) %>% 
  summarise(base=mean(value))

precip.base <- precip %>% 
  filter(year>1960 & year <= 1990) %>% 
  group_by(time) %>% 
  summarise(base=mean(value))

# join temperature and precipitation data, add baseline, compute anomalies
both <- temp %>% 
  left_join(temp.base) %>% 
  bind_rows(left_join(precip, precip.base)) %>%            
  mutate(anomaly=ifelse(measure=='temperature',
                        value-base,             # difference anomaly for temp.
                        (value-base)/base*100)) # pct. anomaly for rain

# create one big dataset for Datawrapper
out <- both %>% 
  select(year, time, measure, anomaly) %>% 
  pivot_wider(names_from=measure, values_from=anomaly)

# look at all the years
out %>% 
  filter(time=='Year' & year > 1960) %>% 
  ggplot(aes(temperature, precipitation, label=year)) +
  geom_point() +
  geom_text_repel(label.size=3) +
  geom_hline(yintercept=0)  +
  geom_vline(xintercept=0)  +
  theme_minimal()


out %>% 
  filter(year==2019) %>% 
  write_csv('temp-precip-anomalies-2019.csv')

colors <- tribble(#####1976b3
  ~time, ~color,
  'Year', '#333333',
  'Summer', '#308c00',
  'June', '#308c00',
  'July', '#308c00',
  'August', '#308c00',
  #~~~~~~~~~~~~~~~~~
  'Winter', '#6ea2ff',
  'December', '#6ea2ff',
  'January', '#6ea2ff',
  'February', '#6ea2ff',
  #~~~~~~~~~~~~~~~~~
  'Autumn', '#ac2125',
  'September', '#ac2125',
  'October', '#ac2125',
  'November', '#ac2125',
  #~~~~~~~~~~~~~~~~~
  'Spring', '#fac10e',
  'March', '#fac10e',
  'April', '#fac10e',
  'May', '#fac10e',
)

out %>% 
  filter(year==2020) %>% 
  filter(!(time %in% c('Year','Summer','Winter','Spring','Autumn'))) %>% 
  left_join(colors) %>% 
  transmute(x1=0,
            y1=0,
            x2=round(temperature,2),
            y2=round(precipitation,2),
            paste0('@color:',color),
            '@width:2') %>% 
  format_csv(col_names = F) %>% 
  str_replace_all(',@', ' @') %>% 
  write_clip()

out %>% 
  filter(year==2020) %>% 
  write_csv('temp-precip-anomalies-2020.csv')

out %>% 
  filter(year==2018) %>% 
  write_csv('temp-precip-anomalies-2018.csv')

out %>% 
  filter(year==2017) %>% 
  write_csv('temp-precip-anomalies-2017.csv')


out %>% 
  filter(year>=2010) %>% 
  filter(time %in% c('Summer','Winter','Spring','Autumn')) %>% 
  write_csv('temp-precip-anomalies-2010-2020.csv')

out %>% 
  filter(year>=2010) %>% 
  filter(time %in% c('Summer','Winter','Spring','Autumn')) %>% 
  left_join(colors) %>% 
  transmute(x1=0,
            y1=0,
            x2=round(temperature,2),
            y2=round(precipitation,2),
            paste0('@color:',color),
            '@width:2',
            '@opacity:0.3') %>% 
  format_csv(col_names = F) %>% 
  str_replace_all(',@', ' @') %>% 
  write_clip()

out %>% 
  filter(year>=2010) %>% 
  filter(time %in% c('Summer','Winter','Spring','Autumn')) %>% 
  ggplot(aes(xend=temperature, yend=precipitation,color=time)) +
  geom_hline(yintercept=0)  +
  geom_vline(xintercept=0)  +
  geom_segment(x=0,y=0, arrow = arrow(length = unit(7, 'points'))) +
  geom_text(aes(temperature, precipitation, label=year)) +
  theme_minimal()
  
out %>% 
  filter(year>=2000) %>% 
  filter(time %in% c('Summer','Winter','Spring','Autumn')) %>% 
  group_by(time) %>% 
  summarise(temperature=mean(temperature), precipitation=mean(precipitation)) %>% 
  ggplot(aes(xend=temperature, yend=precipitation,color=time)) +
  geom_hline(yintercept=0)  +
  geom_vline(xintercept=0)  +
  theme_minimal() +
  geom_segment(x=0,y=0, arrow = arrow(length = unit(7, 'points')))

out %>% 
  filter(year>=2000) %>% 
  filter(time %in% c('Summer','Winter','Spring','Autumn')) %>% 
  group_by(time) %>% 
  summarise(temperature=mean(temperature), precipitation=mean(precipitation)) %>% 
  write_csv('temp-precip-mean-anomalies-2010-2020.csv')


out %>% 
  filter(year>=2000) %>% 
  filter(time %in% c('Summer','Winter','Spring','Autumn')) %>% 
  group_by(time) %>% 
  summarise(temperature=mean(temperature), precipitation=mean(precipitation)) %>% 
  left_join(colors) %>% 
  transmute(x1=0,
            y1=0,
            x2=round(temperature,2),
            y2=round(precipitation,2),
            paste0('@color:',color),
            '@width:3',
            '@opacity:1') %>% 
  format_csv(col_names = F) %>% 
  str_replace_all(',@', ' @') %>% 
  write_clip()


#   
# stations <- read_csv('https://www.vis4.net/data/dwd/stations.csv') %>% 
#   filter(to > '2020-01-01' & from < '1900-01-01')
# 
# stations %>% 
#   ggplot(aes(lon, lat)) +
#   geom_point() +
#   coord_map()
# 
# data <- bind_rows(lapply(stations$id, function(id) {
#   read_csv(paste0('https://vis4.net/data/dwd/stations/',id,'.csv')) %>% 
#     mutate(RSK=ifelse(RSK==-999, NA, RSK), station.id=id)
# })) %>% left_join(stations, by = c('station.id'='id'))
# 
# # total rain per year at this station
# data %>% 
#   filter(date >= '1900-01-01') %>% 
#   group_by(station.id, year=as.integer(format(date, '%Y'))) %>% 
#   summarise(rain=sum(RSK, na.rm = T), name=first(name)) %>%
#   filter(year < 2020) %>% 
#   ggplot(aes(year, rain, group=name)) +
#   geom_point(size=0.5) +
#   geom_smooth(method='lm') +
#   facet_wrap(. ~ name, scales='free_y')
# 
#  