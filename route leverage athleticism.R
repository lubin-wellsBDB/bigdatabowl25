head(receiver_and_defender_tracking_data_3)

head(player_play)

route_data <- player_play %>%
  left_join(players, by = "nflId") %>%
  filter(position%in%c("WR", "TE")) %>%
  filter(wasRunningRoute==1) %>%
  select(gameId, playId, receiver_id = nflId, routeRan, inMotionAtBallSnap, shiftSinceLineset, motionSinceLineset) %>%
  filter(routeRan!="SCREEN") %>%
  left_join(plays) %>%
  filter(pff_passCoverage!="Prevent")

defensive_coverage_scheme <- player_play %>%
  filter(!is.na(pff_defensiveCoverageAssignment)) %>%
  select(gameId, playId, defender_id = nflId, pff_defensiveCoverageAssignment) %>%
  mutate(coverage_assignment_simplified = case_when(pff_defensiveCoverageAssignment %in% c("HCR", "HCL") ~ "Hook/Curl",
                                                    pff_defensiveCoverageAssignment %in% c("4OR", "4OL") ~ "Quarters Outside",
                                                    pff_defensiveCoverageAssignment %in% c("FL", "FR") ~ "Flat Zone",
                                                    pff_defensiveCoverageAssignment %in% c("CFR", "CFL") ~ "Curl/Flat",
                                                    pff_defensiveCoverageAssignment %in% c("4IR", "4IL") ~ "Quarters Inside",
                                                    pff_defensiveCoverageAssignment %in% c("2L", "2R") ~ "Deep Half",
                                                    pff_defensiveCoverageAssignment %in% c("3L", "3R") ~ "Deep Third Outside",
                                                    pff_defensiveCoverageAssignment %in% c("3M") ~ "Deep Third Middle",
                                                    pff_defensiveCoverageAssignment %in% c("HOL") ~ "Hole",
                                                    pff_defensiveCoverageAssignment %in% c("DF") ~ "Deep Free",
                                                    pff_defensiveCoverageAssignment %in% c("PRE") ~ "Prevent",
                                                    pff_defensiveCoverageAssignment %in% c("MAN") ~ "Man"
  ))

route_data <- merge(route_data, updated_defensive_coverages, by = c("gameId", "playId", "receiver_id"))

route_data <- merge(route_data, defensive_coverage_scheme, by = c("gameId", "playId", "defender_id"))


athleticism_data <- tracking_data %>%
  filter(!is.na(nflId)) %>%
  group_by(nflId) %>%
  summarise(top_speed = max(s),
            top_acceleration=max(a),
            speed_acceleration_combined = top_speed + top_acceleration) %>%
  select(nflId, speed_acceleration_combined)

deceleration_data <- tracking_data %>%
  filter(!is.na(nflId)) %>% 
  left_join(players, by = "nflId") %>%
  left_join(player_play, by = c("nflId", "gameId", "playId")) %>%
  filter(position %in% c("WR", "TE"), wasTargettedReceiver==0) %>%
  group_by(nflId, gameId, playId) %>% 
  arrange(nflId, gameId, playId, frameId) %>% 
  mutate(future_acceleration = lead(a, n = 5),  
         future_playId = lead(playId, n = 5),   
         deceleration = if_else(future_playId == playId, a - future_acceleration, NA_real_)) %>% 
  summarise(max_deceleration = max(deceleration, na.rm = TRUE), 
            top_acceleration = max(a, na.rm = TRUE),
            accel_decel_combined = top_acceleration + max_deceleration) %>% 
  ungroup() %>% 
  group_by(nflId) %>% 
  summarise(
    overall_max_deceleration = max(max_deceleration, na.rm = TRUE)
  )

colnames(tracking_data)

full_athleticism_data <- merge(athleticism_data, deceleration_data, by = "nflId") %>%
  mutate(athleticism_combined = speed_acceleration_combined + overall_max_deceleration) %>%
  select(nflId, athleticism_combined)

leverage_data <- ball_snap_data %>%
  mutate(acceleration_diff = abs(receiver_a - defender_a))%>%
  select(gameId, playId, receiver_id, defender_id, receiver_dist_to_defender, acceleration_diff) 

leverage_data_with_motions <- merge(leverage_data, player_play, by.x = c("gameId", "playId", "receiver_id"), by.y = c("gameId", "playId", "nflId")) %>%
  select(gameId, playId, receiver_id, defender_id, receiver_dist_to_defender, acceleration_diff, inMotionAtBallSnap, shiftSinceLineset, motionSinceLineset)

route_leverage_athleticism <- merge(route_data, full_athleticism_data, by.x = "receiver_id", by.y = "nflId")

route_leverage_athleticism <- merge(route_leverage_athleticism, leverage_data, by = c("gameId", "playId", "receiver_id", "defender_id"))

head(route_leverage_athleticism)

openness <- read.csv("openness_by_play.csv") %>%
  filter(!gameId %in% c("2022091809", "2022091113")) %>%
  select(gameId, playId, receiver_id, defender_id, openness_after_10, 
         openness_after_20, openness_after_30, openness_after_40, openness_after_50,
         openness_after_pass_forward, openness_after_pass_arrived)


route_leverage_athleticism <- merge(route_leverage_athleticism, openness, by = c("gameId", "playId", "receiver_id", "defender_id"))


library(tidyr)
when_ball_is_thrown <- receiver_and_defender_tracking_data_3 %>%
  group_by(gameId, playId, event) %>%
  summarise(frameId = first(frameId)) %>%
  filter(event %in% c("pass_forward", "ball_snap", "qb_sack", "run")) %>%
  select(gameId, playId, event, frameId) %>%
  pivot_wider(
    names_from = event,
    values_from = frameId
  ) %>%
  mutate(
    time_to_throw = ifelse(!is.na(pass_forward), pass_forward - ball_snap, ifelse(!is.na(qb_sack), qb_sack - ball_snap, run - ball_snap))
  ) %>%
  select(gameId, playId, time_to_throw) %>%
  na.omit()

route_leverage_athleticism <- merge(route_leverage_athleticism, when_ball_is_thrown, by = c("gameId", "playId"))

route_leverage_athleticism$openness_after_pass_forward_2 <- route_leverage_athleticism$openness_after_pass_forward
route_leverage_athleticism$openness_after_pass_forward_3 <- route_leverage_athleticism$openness_after_pass_forward

route_leverage_athleticism <- route_leverage_athleticism %>%
  mutate(
    average_openness = case_when(time_to_throw<8 ~ rowMeans(
                                   select(., openness_after_pass_forward, 
                                          openness_after_pass_arrived, openness_after_pass_forward_2,
                                          openness_after_pass_forward_3), 
                                   na.rm = TRUE
                                 ),
                                 time_to_throw<20 ~ rowMeans(
                                   select(., openness_after_10, openness_after_pass_forward, 
                                          openness_after_pass_arrived, openness_after_pass_forward_2,
                                          openness_after_pass_forward_3), 
                                   na.rm = TRUE
                                 ),
                                 time_to_throw<28 ~ rowMeans(
                                   select(., openness_after_10, openness_after_20, 
                                          openness_after_pass_forward, openness_after_pass_arrived, 
                                          openness_after_pass_forward_2,
                                          openness_after_pass_forward_3), 
                                   na.rm = TRUE
                                 ),
                                 time_to_throw<38 ~ rowMeans(
                                   select(., openness_after_10, openness_after_20, 
                                          openness_after_30, openness_after_pass_forward, 
                                          openness_after_pass_arrived, openness_after_pass_forward_2,
                                          openness_after_pass_forward_3), 
                                   na.rm = TRUE
                                 ),
                                 time_to_throw<48 ~ rowMeans(
                                   select(., openness_after_10, openness_after_20, 
                                          openness_after_30, openness_after_40, 
                                          openness_after_pass_forward, openness_after_pass_arrived,
                                          openness_after_pass_forward_2,
                                          openness_after_pass_forward_3
                                          ), 
                                   na.rm = TRUE
                                 ),
                                 time_to_throw>=48 ~ rowMeans(
                                   select(., openness_after_10, openness_after_20, 
                                          openness_after_30, openness_after_40, 
                                          openness_after_50, openness_after_pass_forward, 
                                          openness_after_pass_arrived, openness_after_pass_forward_2,
                                          openness_after_pass_forward_3), 
                                   na.rm = TRUE
                                 ))
  ) %>%
  select(-openness_after_10, -openness_after_20, -openness_after_30, -openness_after_40, 
         -openness_after_50, -time_to_throw, -openness_after_pass_forward, -openness_after_pass_arrived,
         , -openness_after_pass_forward_2,
         -openness_after_pass_forward_3)


str(route_leverage_athleticism)

route_leverage_athleticism$routeRan <- as.factor(route_leverage_athleticism$routeRan)
route_leverage_athleticism$pff_defensiveCoverageAssignment <- as.factor(route_leverage_athleticism$pff_defensiveCoverageAssignment)

route_leverage_athleticism <- route_leverage_athleticism %>%
  mutate(
    std_athleticism_combined = scale(athleticism_combined),
    std_receiver_dist_to_defender = scale(receiver_dist_to_defender),
    std_acceleration_diff = scale(acceleration_diff)
  )


route_coverage_data <- route_leverage_athleticism %>%
  mutate(route_coverage_interaction = paste(routeRan, coverage_assignment_simplified, sep = " vs. "))

route_combos <- route_coverage_data %>%
  group_by(route_coverage_interaction) %>%
  summarise(count=n()) %>%
  filter(count>=50)

route_coverage_data <- route_coverage_data %>%
  filter((route_coverage_interaction %in% route_combos$route_coverage_interaction))

route_coverage_data <- route_coverage_data %>%
  mutate(
    std_receiver_dist_to_defender = scale(receiver_dist_to_defender),
    std_acceleration_diff = scale(acceleration_diff),
    std_athleticism_combined = scale(athleticism_combined)
  )

library(xgboost)

route_coverage_data <- route_coverage_data %>%
  mutate(
    inMotionAtBallSnap = ifelse(is.na(inMotionAtBallSnap), FALSE, inMotionAtBallSnap),
    shiftSinceLineset = ifelse(is.na(shiftSinceLineset), FALSE, shiftSinceLineset),
    motionSinceLineset = ifelse(is.na(motionSinceLineset), FALSE, motionSinceLineset)
  )

missing_data <- route_coverage_data %>%
  select(route_coverage_interaction, inMotionAtBallSnap, shiftSinceLineset, motionSinceLineset,
         std_receiver_dist_to_defender, std_acceleration_diff, std_athleticism_combined) %>%
  summarise_all(~ sum(is.na(.)))


X <- model.matrix(~ route_coverage_interaction + inMotionAtBallSnap + shiftSinceLineset + motionSinceLineset +
                    std_receiver_dist_to_defender + std_acceleration_diff + std_athleticism_combined,
                  data = route_coverage_data)
y <- route_coverage_data$average_openness


set.seed(117)
train_indices <- sample(seq_len(nrow(X)), size = 0.8 * nrow(X))
X_train <- X[train_indices, ]
X_test <- X[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices] 

# 
# library(caret)
# 
# param_grid <- expand.grid(
#   nrounds = c(500, 1000),
#   max_depth = c(3, 6),
#   eta = c(0.1, 0.2),
#   gamma = c(0, 0.1),
#   min_child_weight = c(1, 5),
#   subsample = c(0.8, 1),
#   colsample_bytree = c(0.8, 1)
# )
# # 
# train_control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
# # 
# xgb_tune <- train(
#   x = X_train, y = y_train,
#   method = "xgbTree",
#   trControl = train_control,
#   tuneGrid = param_grid
# )
# 
# print(xgb_tune$bestTune)


xgb_model <- xgboost(
  data = X_train, label = y_train, 
  nrounds = 500, objective = "reg:squarederror",
  gamma = 0, min_child_weight = 1,
  eta = 0.1, max_depth = 3, subsample = 1, colsample_bytree = 0.8,
  verbose = FALSE
)

library(Metrics)

y_pred <- predict(xgb_model, newdata = X_test)

mae <- mae(y_test, y_pred)
rmse <- rmse(y_test, y_pred)
r_squared <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)

cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared (R²):", r_squared, "\n")

library(ggplot2)
actual_vs_predicted <- data.frame(
  actual = y_test,
  predicted = y_pred
)

ggplot(actual_vs_predicted, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  labs(
    title = "Actual vs Predicted Openness",
    x = "Actual Openness",
    y = "Predicted Openness"
  ) +
  theme_minimal()


library(SHAPforxgboost)

expected_openness <- predict(xgb_model, newdata = X)

route_coverage_data$expected_openness <- expected_openness

shap_values <- shap.values(xgb_model, X_train)

shap_scores <- as.data.frame(shap_values$mean_shap_score)

shap_scores_df <- data.frame(
  feature = rownames(shap_scores),    # Features from row names of the shap_scores
  mean_shap_score = shap_scores[, 1] # Mean SHAP score for each feature
)

route_shap_values <- shap_scores_df
route_shap_values$feature <- gsub("^route_coverage_interaction", "", route_shap_values$feature)

route_coverage_data <- route_coverage_data %>%
  mutate(inMotionAtBallSnap = ifelse(!is.na(inMotionAtBallSnap),as.integer(inMotionAtBallSnap), 0),
         shiftSinceLineset = ifelse(!is.na(shiftSinceLineset),as.integer(shiftSinceLineset), 0),
         motionSinceLineset = ifelse(!is.na(motionSinceLineset),as.integer(motionSinceLineset), 0)) 

shap_score_data <- merge(route_coverage_data, route_shap_values, by.x = "route_coverage_interaction", by.y = "feature")

inMotionAtBallSnap_true_value <- route_shap_values %>%
  filter(feature=="inMotionAtBallSnapTRUE") %>%
  pull(mean_shap_score)

shiftSinceLineset_true_value <- route_shap_values %>%
  filter(feature=="shiftSinceLinesetTRUE") %>%
  pull(mean_shap_score)

motionSinceLineset_true_value <- route_shap_values %>%
  filter(feature=="motionSinceLinesetTRUE") %>%
  pull(mean_shap_score)

shap_score_data <- shap_score_data %>%
  mutate(inMotionAtBallSnap_value = inMotionAtBallSnap * inMotionAtBallSnap_true_value,
         shiftSinceLineset_value = shiftSinceLineset * shiftSinceLineset_true_value,
         motionSinceLineset_value = motionSinceLineset * motionSinceLineset_true_value)

std_receiver_dist_to_defender_value_pulled <- route_shap_values %>%
  filter(feature=="std_receiver_dist_to_defender") %>%
  pull(mean_shap_score)

std_athleticism_combined_value_pulled <- route_shap_values %>%
  filter(feature=="std_athleticism_combined") %>%
  pull(mean_shap_score)

std_acceleration_diff_value_pulled <- route_shap_values %>%
  filter(feature=="std_acceleration_diff") %>%
  pull(mean_shap_score)

shap_score_data <- shap_score_data %>%
  mutate(std_receiver_dist_to_defender_value = std_receiver_dist_to_defender * std_receiver_dist_to_defender_value_pulled,
         std_athleticism_combined_value = std_athleticism_combined * std_athleticism_combined_value_pulled,
         std_acceleration_diff_value = std_acceleration_diff * std_acceleration_diff_value_pulled)

shap_score_data <- shap_score_data %>%
  mutate(athleticism_importance = std_athleticism_combined,
         route_importance = mean_shap_score,
         pre_snap_importance = std_receiver_dist_to_defender_value + 
           std_acceleration_diff_value + inMotionAtBallSnap_value + shiftSinceLineset_value + 
           motionSinceLineset_value)

shap_score_data_selected <- shap_score_data %>%
  mutate(openness_over_expected = average_openness - expected_openness) %>%
  select(gameId, playId, receiver_id, average_openness, expected_openness, openness_over_expected, athleticism_importance, route_importance, pre_snap_importance)




routes_ran <- shap_score_data_selected %>%
  group_by(receiver_id) %>%
  summarise(routes_ran = n()) %>%
  filter(routes_ran>=100) %>%
  select(receiver_id, routes_ran)

receiver_importance <- shap_score_data_selected %>%
  filter(receiver_id %in% routes_ran$receiver_id) %>%
  group_by(receiver_id) %>%
  summarise(
    avg_athleticism_importance = mean(athleticism_importance, na.rm = TRUE),
    avg_route_importance = mean(route_importance, na.rm = TRUE),
    avg_pre_snap_importance = mean(pre_snap_importance, na.rm = TRUE),
    avg_openness = mean(average_openness, na.rm = TRUE),
    avg_exp_openness = mean(expected_openness, na.rm = TRUE),
    avg_openness_over_exp = mean(openness_over_expected, na.rm = TRUE),
    routes_ran = n()
  )

receiver_importance <- receiver_importance %>%
  arrange(desc(avg_openness)) %>%
  left_join(players, by = c("receiver_id" = "nflId"))

route_coverage_effectiveness <- route_coverage_data %>%
  group_by(route_coverage_interaction) %>%
  summarise(
    avg_openness = mean(average_openness, na.rm = TRUE),
    count = n()
  ) %>%
  filter(count >=100) %>%
  arrange(desc(avg_openness))
  

ggplot(route_coverage_effectiveness, aes(x = reorder(route_coverage_interaction, avg_openness), y = avg_openness)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Route/Coverage Efficiency",
    x = "Route Ran vs Coverage",
    y = "Average Separation"
  ) +
  theme_minimal()

top_5_route_coverage_effectiveness <- route_coverage_effectiveness %>%
  arrange(desc(avg_openness)) %>%
  slice_head(n = 5)

bottom_5_route_coverage_effectiveness <- route_coverage_effectiveness %>%
  arrange(avg_openness) %>%
  slice_head(n = 5)

combined_route_coverage_effectiveness <- bind_rows(
  top_5_route_coverage_effectiveness %>% mutate(group = "Top 5 Route - Coverage Interactions"),
  bottom_5_route_coverage_effectiveness %>% mutate(group = "Bottom 5 Route - Coverage Interactions")
)

library(gt)
library(gtExtras)

route_coverage_effectiveness_gt <- combined_route_coverage_effectiveness %>%
  select(group, route_coverage_interaction, avg_openness) %>%
  gt(groupname_col = "group") %>%
  tab_header(
    title = "Top 5 and Bottom 5 Route - Coverage Interactions by Average Openness") %>%
  cols_label(
    route_coverage_interaction = "Route - Coverage Interaction",
    avg_openness = "Average Openness"
  ) %>%
  data_color(
    columns = avg_openness,
    colors = scales::col_numeric(
      palette = c("firebrick", "forestgreen"),
      domain = NULL
    )
  ) %>%
  gt_theme_538() %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "gray",
        weight = px(2)
      ),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "black"), 
    locations = cells_row_groups()
  )

gtsave(route_coverage_effectiveness_gt, "route_coverage_effectiveness_gt.png")

motion_efficiency <- route_coverage_data %>%
  filter(pff_manZone!="Other") %>%
  group_by(routeRan, inMotionAtBallSnap, pff_manZone) %>%
  summarise(
    avg_openness = mean(average_openness, na.rm = TRUE),
    count = n()
  ) %>%
  filter(count > 50) %>%
  pivot_wider(
    names_from = inMotionAtBallSnap,
    values_from = avg_openness,
    names_prefix = "motion_"
  ) %>%
  ungroup() %>%
  group_by(routeRan, pff_manZone) %>%
  summarise(motion_efficiency = sum(motion_1, na.rm=T) - sum(motion_0, na.rm = T),
            count = sum(count))

ggplot(motion_efficiency, aes(x = reorder(paste(routeRan, pff_manZone, sep = "_"), motion_efficiency), y = motion_efficiency)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Motion Efficiency",
    x = "Route Ran",
    y = "Motion Efficiency (Openness Improvement)"
  ) +
  theme_minimal()


team_motion_efficiency <- route_coverage_data %>%
  group_by(possessionTeam, inMotionAtBallSnap) %>%
  summarise(
    avg_openness = mean(average_openness, na.rm = TRUE),
    count = n()
  ) %>%
  pivot_wider(
    names_from = inMotionAtBallSnap,
    values_from = avg_openness,
    names_prefix = "motion_"
  ) %>%
  ungroup() %>%
  group_by(possessionTeam) %>%
  summarise(motion_efficiency = sum(motion_1, na.rm=T) - sum(motion_0, na.rm = T))

library(nflplotR)
ggplot(team_motion_efficiency_2, aes(x = reorder(possessionTeam, motion_efficiency), y = motion_efficiency)) +
  geom_bar(stat = "identity") +
  scale_color_nfl(type = "secondary") +
  scale_fill_nfl(alpha = 0.4) +
  coord_flip() +
  labs(
    title = "Team Motion Efficiency",
    x = "Team",
    y = "Motion Efficiency (Openness Improvement)"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_nfl_logo(size = 1.25))

library(nflfastR)
teams_colors_logos_data <- teams_colors_logos %>%
  rename(possessionTeam = team_abbr)

team_motion_efficiency_2 <- team_motion_efficiency %>%
  left_join(teams_colors_logos_data)

team_motion_efficiency_plot <- ggplot(data = team_motion_efficiency_2, aes(x = motion_efficiency,
                                         y = reorder(possessionTeam, motion_efficiency))) +
  geom_col(aes(fill = team_color),
           color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  scale_fill_identity() +
  geom_image(aes(image = team_logo_espn),
             asp = 16/9, size = .02) +
  xlab("EPA Per Play") +
  ylab("") +
  theme_fivethirtyeight() +
  scale_y_discrete(expand = c(.05, 0)) +
  theme(legend.position = "none",
        axis.title.x =  element_text(size = 12, color = "black",),
        panel.grid.major.y = element_line(color = alpha("lightgray", 0.4))) +
  labs(title = "Team Motion Efficiency",
       x = "Motion Efficiency (Openness Improvement)",
       subtitle = "Analyzing Teams' Openness Improvement With Pre-Snap Movement",
       caption = "2022 NFL Season - Weeks 1-9",
       y = "Team")

ggsave("team_motion_efficiency_plot.png", team_motion_efficiency_plot)


library(tidyverse)
library(cluster)  
library(factoextra) 

receiver_data <- receiver_importance %>%
  select(receiver_id, avg_athleticism_importance, avg_route_importance, avg_pre_snap_importance) %>%
  drop_na()

receiver_data_scaled <- receiver_data %>%
  mutate(across(avg_athleticism_importance:avg_pre_snap_importance, scale))

kmeans_result <- kmeans(receiver_data_scaled %>% select(-receiver_id), centers = 3, nstart = 25)

receiver_data <- receiver_data %>%
  mutate(cluster = kmeans_result$cluster)

cluster_labels <- receiver_data %>%
  group_by(cluster) %>%
  summarize(
    avg_athleticism = mean(avg_athleticism_importance),
    avg_route = mean(avg_route_importance),
    avg_pre_snap = mean(avg_pre_snap_importance)
  ) %>%
  mutate(
    cluster_label = case_when(
      avg_athleticism > avg_route & avg_athleticism > avg_pre_snap ~ "Athletic",
      avg_route > avg_athleticism & avg_route > avg_pre_snap ~ "Route Reliant",
      avg_pre_snap > avg_athleticism & avg_pre_snap > avg_route ~ "Pre-Snap Reliant",
      TRUE ~ "Combination"
    )
  )

receiver_data <- receiver_data %>%
  left_join(cluster_labels %>% select(cluster, cluster_label), by = "cluster")
receiver_data <- merge(receiver_data, players, by.x = "receiver_id", by.y = "nflId")

fviz_cluster(kmeans_result, data = receiver_data_scaled %>% select(-receiver_id),
             geom = "point", ellipse.type = "convex", repel = TRUE) +
  labs(title = "Receiver Clustering", x = "Dimension 1", y = "Dimension 2") 


