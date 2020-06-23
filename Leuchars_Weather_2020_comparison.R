#########################################################################################
# Weather in lockdown - B Meier                                                         #
# Metoffice Data Leucars                                                                #
# https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/leucharsdata.txt #
#########################################################################################

library(stringr)
library(gridExtra)
library(plotly)

setwd("../Projects_2020")
#dat <- readLines("Leuchars_Weather_2020")
#head(dat, 10)


# load file
Leuchars <- read.table('Leuchars_Weather_2020_clean', skip=2, header = F, stringsAsFactors = F)
head(Leuchars)
ncol(Leuchars)
nrow(Leuchars)

colnames(Leuchars) <- c("Year", "Month", "max_Temp_in_C", "min_Temp_in_C", "airfrost_in_days", "Rainfall_in_mm", "Sunshine_in_h")

str(Leuchars)
View(Leuchars)

# remove stars from temperature entries (indicate ), change chr into num
Leuchars2 <- Leuchars %>% 
  dplyr::mutate(
    max_Temp_in_C = readr::parse_number(max_Temp_in_C),
    min_Temp_in_C = readr::parse_number(min_Temp_in_C),
    airfrost_in_days = readr::parse_number(airfrost_in_days))

str(Leuchars2)

# determine average by month
annual_ave_no_2020 <- Leuchars2 %>% 
  dplyr::filter(Year != 2020) %>% 
  dplyr::group_by(Month) %>%
  dplyr::summarise(ave_max= round(mean(max_Temp_in_C), digits=2), ave_min = round(mean(min_Temp_in_C), digits = 2),
                   ave_frost = round(mean(airfrost_in_days),digits = 2), ave_rain = round(mean(Rainfall_in_mm), digits = 2),
                   ave_sun = round(mean(Sunshine_in_h), digits = 2))

ave_jan_to_april <- as.data.frame(annual_ave_no_2020[1:4,]) # get first 4 months

year_2020 <- Leuchars2 %>% 
  dplyr::filter(Year == 2020) # select gets columns, filter by rows

ave_jan_to_april
year_2020

ave_jan_to_april$Year <- "since 1957"
all <- ave_jan_to_april %>%
  dplyr::select(Year, everything())
colnames(all) <- c("Year", "Month", "Tmax", "Tmin", "Airfrost", "Rainfall", "Sunshine")
colnames(year_2020) <- colnames(all)


compare_2020 <- rbind(all, year_2020)
str(compare_2020)
head(compare_2020)
#compare_2020$Year <- as.character(compare_2020$Year)

a <- ggplot(data=compare_2020, aes(x=Month, y=Rainfall, fill=Year)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("Comparison average monthly rainfall and sunshine for 2020 versus all years since 1957 (Leuchars)") + 
  ylab("Rainfall in mm") +
  theme_minimal()

b <- ggplot(data=compare_2020, aes(x=Month, y=Sunshine, fill=Year)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Paired") +
  ylab("Sunshine in hrs") +
  theme_minimal()

grid.arrange(a, b, nrow=1, ncol=2)

# Leuchars2
all_4months <- Leuchars2 %>% 
  dplyr::filter(Month %in% c(1:4))


z = ggplot(data=all_4months, aes(x=Month, y=Sunshine_in_h, group=Year, color=Year)) +
  geom_line()

ggplotly(z)

rain = ggplot(data=all_4months, aes(x=Month, y=Rainfall_in_mm, group=Year, color=Year)) +
  geom_line()

ggplotly(rain)


since_2005 <- Leuchars2 %>%
  dplyr::filter(Year %in% c(2005:2020)) %>% 
  dplyr::filter(Month %in% c(1:4))

rain = ggplot(data=since_2005, aes(x=Month, y=Rainfall_in_mm, group=Year, color=Year)) +
  geom_line()

sun = ggplot(data=since_2005, aes(x=Month, y=Sunshine_in_h, group=Year, color=Year)) +
  geom_line()

ggplotly(sun)


#collapseRows(x[1:2,])
#unlist(droplevels(x[1,]))
#tidyr::separate(db, col = 3, into = c("type", "number"), sep = ":")


#### When to visit Scotland ####
head(Leuchars2) 

Leuchars_US <- Leuchars2 %>% 
  mutate(max_Temp_in_F = (max_Temp_in_C * 9 / 5) + 32, min_Temp_in_F = (min_Temp_in_C * 9 / 5) + 32) %>% 
  group_by(Month) %>% 
  summarise(max_Temp = round(mean(max_Temp_in_F), digits = 1), min_Temp = round(mean(min_Temp_in_F), digits = 1),
            airfrost = round(mean(airfrost_in_days), digits = 1), rainfall = round(mean(Rainfall_in_mm), digits = 1),
            sunshine = round(mean(Sunshine_in_h), digits = 1))

Leuchars_US

Tmax <- ggplot(data=Leuchars_US, aes(x=Month, y=max_Temp)) +
  scale_x_discrete(name ="Month", limits=c(1:12)) +
  ylab("max Temperature in F") +
  geom_line()

Temp_Leuchars <- ggplot(Leuchars_US, aes(x=Month)) + 
  scale_x_discrete(name ="Month", limits=c(1:12)) +
  ggtitle("Average Temperature in Leuchars: 1957 - 2020") +
  ylab("Temperature in F") +
  ylim(30,70) +
  geom_line(aes(y = max_Temp), color = "darkred") + 
  geom_line(aes(y = min_Temp), color="steelblue") +
  theme_minimal()
  
Frost_Leuchars <- ggplot(Leuchars_US, aes(x=Month, y=airfrost)) + 
  scale_x_discrete(name ="Month", limits=c(1:12)) +
  ggtitle("Average days of airfrost in Leuchars: 1957 - 2020") +
  ylab("Days of airfrost") +
  ylim(0,30) +
  geom_line() + 
  theme_minimal()

Rain_Leuchars <- ggplot(Leuchars_US, aes(x=Month, y=rainfall)) + 
  scale_x_discrete(name ="Month", limits=c(1:12)) +
  ggtitle("Average rainfall in Leuchars: 1957 - 2020") +
  ylab("Rainfall in mm") +
  ylim(25,100) +
  geom_line() + 
  theme_minimal()

Sun_Leuchars <- ggplot(Leuchars_US, aes(x=Month, y=sunshine)) + 
  scale_x_discrete(name ="Month", limits=c(1:12)) +
  ggtitle("Average sunshine in Leuchars: 1957 - 2020") +
  ylab("Sunshine in h") +
  ylim(0,200) +
  geom_line() + 
  theme_minimal()

grid.arrange(Temp_Leuchars, Frost_Leuchars, Rain_Leuchars, Sun_Leuchars, nrow=4, ncol=1)
