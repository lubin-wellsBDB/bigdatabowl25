complete_data <- plays %>%
  inner_join(player_play, by = c("gameId", "playId"))

defensive_matchups <- player_play %>%
  filter(!is.na(pff_primaryDefensiveCoverageMatchupNflId)) %>%
  select(gameId, playId, pff_primaryDefensiveCoverageMatchupNflId, nflId) %>%
  rename(defender_id = nflId, receiver_id = pff_primaryDefensiveCoverageMatchupNflId)
rows <- nrow(defensive_matchups)
defensive_matchups <- defensive_matchups %>%
  mutate(defensive_matchup_id = seq(1, rows))

complete_data <- complete_data %>%
  rename(receiver_id = nflId)

mismatch_data_2 <- merge(complete_data, defensive_matchups, by = c("gameId", "playId", "receiver_id"))

updated_defensive_coverages <- mismatch_data_2 %>%
  select(gameId, playId, receiver_id, defender_id)

receiver_tracking_data_2 <- mismatch_data_2 %>%
  select(gameId, playId, receiver_id, defender_id) %>%
  left_join(tracking_data, by = c("gameId", "playId", "receiver_id" = "nflId")) %>%
  select(gameId, playId, receiver_id, displayName, frameId, club, receiver_x = x, receiver_y = y, receiver_s = s, receiver_a = a, receiver_dis = dis, receiver_o = o, receiver_dir = dir, playDirection, event, defender_id)

defender_tracking_data_2 <- mismatch_data_2 %>%
  select(gameId, playId, defender_id, receiver_id) %>%
  left_join(tracking_data, by = c("gameId", "playId", "defender_id" = "nflId")) %>%
  select(gameId, playId, defender_id, displayName, frameId, club, defender_x = x, defender_y = y, defender_s = s, defender_a = a, defender_dis = dis, defender_o = o, defender_dir = dir, receiver_id)

receiver_and_defender_tracking_data_2 <- receiver_tracking_data_2 %>%
  left_join(defender_tracking_data_2, by = c("gameId", "playId", "frameId", "receiver_id", "defender_id"))

receiver_and_defender_tracking_data_2 <- receiver_and_defender_tracking_data_2 %>%
  left_join(plays, by = c("gameId", "playId")) 

receiver_and_defender_tracking_data_3 <- receiver_and_defender_tracking_data_2 %>%
  left_join(games, by = "gameId") %>%
  mutate(YardFromOwnGoal = ifelse(possessionTeam == yardlineSide, yardlineNumber, 50 + (50 - yardlineNumber)), # yards from own goal
         TeamOffense = ifelse(possessionTeam == homeTeamAbbr, 'home', 'away'), # home or away team on offense
         receiver_x_new = ifelse(playDirection == 'left', 120 - receiver_x, receiver_x) - 10, # flip X if plays moving left
         receiver_y_new = ifelse(playDirection == 'left', 160/3 - receiver_y, receiver_y), # flip Y if plays moving left
         defender_x_new = ifelse(playDirection == 'left', 120 - defender_x, defender_x) - 10, # flip X if plays moving left
         defender_y_new = ifelse(playDirection == 'left', 160/3 - defender_y, defender_y), # flip Y if plays moving left
         YardToGo = 100 - YardFromOwnGoal, # yards to go
         receiver_dir_new = ifelse(playDirection == 'left', receiver_dir - 180, receiver_dir), # flip player directions
         receiver_dir_new = ifelse(receiver_dir_new < 0, receiver_dir_new + 360, receiver_dir_new),
         defender_dir_new = ifelse(playDirection == 'left', defender_dir - 180, defender_dir), # flip player directions
         defender_dir_new = ifelse(defender_dir_new < 0, defender_dir_new + 360, defender_dir_new),
         receiver_o_new = ifelse(playDirection == 'left', receiver_o - 180, receiver_o), # flip player directions
         receiver_o_new = ifelse(receiver_o_new < 0, receiver_o_new + 360, receiver_o_new),
         defender_o_new = ifelse(playDirection == 'left', defender_o - 180, defender_o), # flip player directions
         defender_o_new = ifelse(defender_o_new < 0, defender_o_new + 360, defender_o_new)) %>%
  left_join(football_location_at_snap, by = c("gameId", "playId")) %>%
  mutate(receiver_x_new = receiver_x_new - football_x,
         receiver_y_new = receiver_y_new - football_y,
         defender_x_new = defender_x_new - football_x,
         defender_y_new = defender_y_new - football_y,
         receiver_dist_to_defender = sqrt((receiver_x_new - defender_x_new)^2 + (receiver_y_new - defender_y_new)^2))

# write.csv(receiver_and_defender_tracking_data_3, "receiver_and_defender_tracking_data_2.csv", row.names = F)

receiver_and_defender_tracking_data_3 <- fread("receiver_and_defender_tracking_data_2.csv")

ball_snap_data <- receiver_and_defender_tracking_data_3 %>% filter(event == "ball_snap")


