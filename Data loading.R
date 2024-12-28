
setwd("/Users/jonahlubin/Desktop/Jonah Analytics/Analytics Competitions/Big data bowl 2025/Big Data Bowl 2024 2025/nfl-big-data-bowl-2025/tracking_data")
library(data.table)
library(dplyr)
tracking_week_1 <- fread("tracking_week_1.csv") %>%
  mutate(unique_id = paste(gameId, playId))
tracking_week_2 <- fread("tracking_week_2.csv")%>%
  mutate(unique_id = paste(gameId, playId))
tracking_week_3 <- fread("tracking_week_3.csv")%>%
  mutate(unique_id = paste(gameId, playId))
tracking_week_4 <- fread("tracking_week_4.csv")%>%
  mutate(unique_id = paste(gameId, playId))
tracking_week_5 <- fread("tracking_week_5.csv")%>%
  mutate(unique_id = paste(gameId, playId))
tracking_week_6 <- fread("tracking_week_6.csv")%>%
  mutate(unique_id = paste(gameId, playId))
tracking_week_7 <- fread("tracking_week_7.csv")%>%
  mutate(unique_id = paste(gameId, playId))
tracking_week_8 <- fread("tracking_week_8.csv")%>%
  mutate(unique_id = paste(gameId, playId))
tracking_week_9 <- fread("tracking_week_9.csv")%>%
  mutate(unique_id = paste(gameId, playId))
# 
# tracking_data <- list.files(path = "tracking_data/", full.names = TRUE) %>%
#   lapply(fread) %>%
#   bind_rows()
# 
# write.csv(tracking_data, "tracking_data.csv", row.names = F)

setwd("/Users/jonahlubin/Desktop/Jonah Analytics/Analytics Competitions/Big data bowl 2025/Big Data Bowl 2024 2025/nfl-big-data-bowl-2025")

tracking_data <- fread("tracking_data.csv") 
tracking_data <- tracking_data %>%
  filter(!gameId %in% c("2022091809", "2022091113"))

head(tracking_data, 10)

sort(table(tracking_data$event), decreasing = T)

tracking_week_1 %>%
  group_by(playId) %>%
  summarise(rows = n()) %>%
  arrange(desc(rows))

games <- read.csv("games.csv") %>%
  filter(!gameId %in% c("2022091809", "2022091113"))

player_play <- fread("player_play.csv") %>%
  mutate(unique_id = paste(gameId, playId)) %>%
  filter(!gameId %in% c("2022091809", "2022091113"))

sort(table(player_play$routeRan), decreasing = T)

players <- read.csv("players.csv")

plays <- read.csv("plays.csv") %>%
  filter(!gameId %in% c("2022091809", "2022091113")) %>%
  mutate(unique_id = paste(gameId, playId),
         yards_to_end_zone = absoluteYardlineNumber-10)

unique(tracking_data$frameType)

pre_snap_tracking_data <- tracking_data %>%
  filter(frameType!="AFTER_SNAP")

football_location_at_snap <- tracking_data %>%
  filter(frameType=="SNAP", displayName=="football") %>%
  mutate(football_x = x, 
         football_y = y,
         football_x = ifelse(playDirection == 'left', 120 - football_x, football_x) - 10,
         football_y = ifelse(playDirection == 'left', 160/3 - football_y, football_y)) %>%
  select(gameId, playId, football_x, football_y)

