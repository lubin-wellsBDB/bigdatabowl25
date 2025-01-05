#Plots
library(ggplot2)
library(gganimate)
library(dplyr)
library(ggthemes)

grouped_shap_values <- shap_values$shap_score %>%
  as.data.frame() %>%
  mutate(
    route_coverage_group = rowSums(select(., starts_with("route_coverage_interaction"))),
    motion_related_group = rowSums(select(., c("inMotionAtBallSnapTRUE", "shiftSinceLinesetTRUE", 
                                               "motionSinceLinesetTRUE", "std_acceleration_diff", 
                                               "std_receiver_dist_to_defender"))),
    athleticism_group = std_athleticism_combined
  ) %>%
  select(route_coverage_group, motion_related_group, athleticism_group) %>%
  filter(motion_related_group<=10)

force_plot_data <- shap.prep.stack.data(
  shap_contrib = grouped_shap_values,
  top_n = 3
)

force_plot_data <- force_plot_data %>%
  rename("Pre-Snap Movement" = motion_related_group, "Route - Coverage Interaction" = route_coverage_group, "Athleticism" = athleticism_group)

shap.plot.force_plot(force_plot_data)

force_plot_obj <- shap.plot.force_plot(force_plot_data, zoom_in = FALSE)

force_plot <- force_plot_obj +
  theme_fivethirtyeight() + 
  labs(
    title = "SHAP Value Force Plot",
    subtitle = "Visualization of Feature Importance in Separation Scores",
    caption = "2022 NFL Season - Weeks 1-9",
    y = "SHAP Values By Feature"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 14, color = "grey30", hjust = 0.5),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black"),
    panel.grid = element_blank(), 
    plot.margin = margin(20, 20, 20, 20)
  ) +
  ylim(c(-5,10)) +
  scale_fill_manual(values = c("#3B9AB2FF", "#EBCC2AFF", "#F21A00FF")) +  # Use your custom colors
  scale_color_manual(values = c("#3B9AB2FF", "#EBCC2AFF", "#F21A00FF")) +
  theme(plot.title.position = "plot", plot.caption.position = "plot") 

ggsave("shap_value_force_plot.png", force_plot)


motion_X_train <- as.data.frame(X_train)
motion_X_train <- motion_X_train %>%
  select(std_acceleration_diff, motionSinceLinesetTRUE, shiftSinceLinesetTRUE, inMotionAtBallSnapTRUE)


motion_X_train_matrix <- as.matrix(motion_X_train)

library(xgboost)
xgb_model_motion <- xgboost(
  data = motion_X_train_matrix,
  label = y_train, 
  params = xgb_model$params, 
  nrounds = xgb_model$niter
)

shap.plot.summary.wrap1(xgb_model_motion, motion_X_train_matrix, top_n = 4)


receiving_yard_leaders <- player_play %>%
  group_by(nflId) %>%
  summarise(receiving_yards = sum(receivingYards)) %>%
  arrange(desc(receiving_yards)) %>%
  left_join(players) %>%
  head(4)

density_plot_data <- shap_score_data_selected %>%
  rename(nflId = receiver_id) %>%
  left_join(players, by = "nflId") %>%
  filter(displayName %in% c("Davante Adams", "Tyreek Hill", "Deebo Samuel", "Justin Jefferson")) %>%
  select(displayName, pre_snap_importance, athleticism_importance, route_importance) %>%
  rename("Athleticism" = athleticism_importance, "Pre-Snap Movement" = pre_snap_importance, "Route - Coverage Interaction" = route_importance)


density_plot_data_long <- density_plot_data %>%
  pivot_longer(
    cols = -displayName, 
    names_to = "attribute", 
    values_to = "SHAP"
  )

density_plot_data_long_2 <- density_plot_data_long %>%
  mutate(
    SHAP = ifelse(
      attribute == "Route - Coverage Interaction",
      (SHAP - min(density_plot_data_long$SHAP[density_plot_data_long$attribute == "Route - Coverage Interaction"], na.rm = TRUE)) /
        (max(SHAP[attribute == "Route - Coverage Interaction"], na.rm = TRUE) -
           min(SHAP[attribute == "Route - Coverage Interaction"], na.rm = TRUE)) * 6 - 2,
      SHAP
    )
  )

density_plot_data_all <- shap_score_data_selected %>%
  rename(nflId = receiver_id) %>%
  left_join(players, by = "nflId") %>%
  select(displayName, pre_snap_importance, athleticism_importance, route_importance)%>%
  mutate(route_importance = (route_importance - min(route_importance, na.rm = TRUE)) /
        (max(route_importance, na.rm = TRUE) -
           min(route_importance, na.rm = TRUE)) * 6 - 2
  )
mean_athleticism_imp <- mean(density_plot_data_all$athleticism_importance)
mean_route_imp <- mean(density_plot_data_all$route_importance)
mean_presnap_imp <- mean(density_plot_data_all$pre_snap_importance)

ggplot(density_plot_data_filtered, aes(x = SHAP, y=..scaled.., fill = attribute, color = attribute)) +
  geom_density(alpha = 0.4, adjust = 1.5) + 
  scale_x_continuous(breaks = scales::pretty_breaks(), limits = c(min_value, max_value)) +
  scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, max_density)) +
  facet_wrap(~ displayName, scales = "fixed") +
  theme_fivethirtyeight() +
  xlab("Attribute Value") +
  ylab("Density") +
  labs(fill = "Attribute", color = "Attribute")

ggplot(density_plot_data_long, aes(x = SHAP, y = ..scaled.., fill = attribute, color = attribute)) +
  geom_density(alpha = 0.4, adjust = 1.5) +  
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  facet_grid(rows = vars(displayName), cols = vars(attribute), scales = "fixed") +
  theme_fivethirtyeight() +
  xlab("Attribute Value") +
  ylab("Density") +
  labs(fill = "Attribute", color = "Attribute")

density_plot <- ggplot(density_plot_data_long_2, aes(x = SHAP, y = ..scaled.., fill = attribute, color = attribute)) +
  geom_density(alpha = .5, adjust = 1.5) + 
  geom_vline(data = density_plot_data_long_2 %>% distinct(attribute), 
             aes(xintercept = 0, color = attribute), linetype = "dashed") + 
  scale_y_continuous(breaks = NULL) +
  facet_grid(rows = vars(displayName), cols = vars(attribute), scales = "fixed") +
  theme_fivethirtyeight() +
  theme(
    axis.title = element_text(size = 10, face = "bold"),
    strip.text.y = element_text(size = 7, face = "bold"), 
    strip.text.x = element_text(size = 12, face = "bold") ,
    strip.background = element_rect(
      color="black", fill="lightgray", size=1.5, linetype="solid"
    ),
    plot.title = element_text(size = 18, face = "bold", color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 14, color = "grey30", hjust = 0.5),
  ) +
  xlab("SHAP Value") +
  ylab("Frequency") +
  labs(fill = "Attribute", color = "Attribute",
       title = "SHAP Value Distribution for Individual Receivers",
       caption = "2022 NFL Season - Weeks 1-9",
       subtitle = "Frequency of SHAP Values For All Routes Run By Receiver") +
  scale_fill_manual(values = c("#F21A00FF", "#EBCC2AFF", "#3B9AB2FF")) +
  scale_color_manual(values = c("firebrick", "gold4", "darkblue"))

ggsave("density_plot.png", density_plot, width = 10, height = 6)



top_5_athleticism <- receiver_data %>%
  arrange(desc(avg_athleticism_importance)) %>%
  slice_head(n = 5) %>%
  mutate(group = "Top 5 Athleticism Reliant")

top_5_routes <- receiver_data %>%
  arrange(desc(avg_route_importance)) %>%
  slice_head(n = 5) %>%
  mutate(group = "Top 5 Route Reliant")

top_5_presnap <- receiver_data %>%
  arrange(desc(avg_pre_snap_importance)) %>%
  slice_head(n = 5) %>%
  mutate(group = "Top 5 Pre-Snap Reliant")

player_teams <- player_play %>%
  select(teamAbbr, nflId) %>%
  group_by(nflId) %>%
  summarise(team = first(teamAbbr)) %>%
  left_join(players) %>%
  select(nflId, team, displayName)

teams_colors_logos_data <- teams_colors_logos_data %>%
  rename(team = possessionTeam)

attribute_top_5s <- bind_rows(
  top_5_athleticism %>% 
    rename(Athleticism = avg_athleticism_importance, 
           PreSnap = avg_pre_snap_importance, 
           Route = avg_route_importance),
  top_5_presnap %>% 
    rename(Athleticism = avg_athleticism_importance, 
           PreSnap = avg_pre_snap_importance, 
           Route = avg_route_importance),
  top_5_routes %>% 
    rename(Athleticism = avg_athleticism_importance, 
           PreSnap = avg_pre_snap_importance, 
           Route = avg_route_importance)
) %>%
  left_join(player_teams) %>%
  left_join(teams_colors_logos_data)

head(attribute_top_5s)

library(gt)
library(scales)
library(gtExtras)

attribute_top_5s_gt <- attribute_top_5s %>%
  select(group, displayName, team_logo_espn, position, Athleticism, PreSnap, Route) %>%
  gt(groupname_col = "group") %>%
  tab_header(
    title = "Top 5 Players by Importance Attribute",
    subtitle = "Minimum 100 Routes Run in 2022 NFL Season - Weeks 1-9 "
  ) %>%
  cols_label(
    displayName = "Player Name",
    team_logo_espn = "Team",
    position = "Position",
    Athleticism = "Athleticism Value",
    PreSnap = "Pre Snap Value",
    Route = "Route Value"
  ) %>%
  gt_img_rows(columns = team_logo_espn, height = 30) %>%
  data_color(
    columns = Athleticism,
    rows = group == "Top 5 Athleticism Reliant",
    colors = scales::col_numeric(
      palette = c("#D9F2D9", "#005500"),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = PreSnap,
    rows = group == "Top 5 Pre-Snap Reliant",
    colors = scales::col_numeric(
      palette = c("#D9F2D9", "#005500"),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = Route,
    rows = group == "Top 5 Route Reliant",
    colors = scales::col_numeric(
      palette = c("#D9F2D9", "#005500"),
      domain = NULL
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#D3D3D3"),
    locations = cells_body(
      columns = vars(PreSnap, Route),
      rows = group == "Top 5 Athleticism Reliant"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#D3D3D3"),
    locations = cells_body(
      columns = vars(Athleticism, Route),
      rows = group == "Top 5 Pre-Snap Reliant"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#D3D3D3"),
    locations = cells_body(
      columns = vars(Athleticism, PreSnap),
      rows = group == "Top 5 Route Reliant"
    )
  ) %>%
  gtExtras::gt_theme_538() %>%
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
  ) %>%
  tab_footnote(
    footnote = "Athleticism Reliant: Players whose separation relies heavily on physical attributes",
    locations = cells_row_groups(groups = "Top 5 Athleticism Reliant")
  ) %>%
  tab_footnote(
    footnote = "Route Reliant: Players benefiting most from specific route and coverage interactions",
    locations = cells_row_groups(groups = "Top 5 Route Reliant")
  ) %>%
  tab_footnote(
    footnote = "Pre-Snap Reliant: Players benefiting most from pre-snap movement",
    locations = cells_row_groups(groups = "Top 5 Pre-Snap Reliant")
  )


gtsave(attribute_top_5s_gt, "attribute_top_5s_gt.png")



motion_efficiency_plot <- ggplot(motion_efficiency, aes(x = reorder(paste(routeRan, pff_manZone, sep = " vs. "), motion_efficiency), 
                              y = motion_efficiency, fill = motion_efficiency)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "firebrick", high = "forestgreen") +
  coord_flip() +
  geom_hline(yintercept = 0, color = "black") +
  theme_fivethirtyeight() +
  theme(legend.position = "none",
        axis.title.x =  element_text(size = 12, color = "black"),
        panel.grid.major.y = element_line(color = alpha("lightgray", 0.3)),
        axis.text.x = element_blank()) +
  labs(title = "Route Motion Efficiency",
       y = "Motion Efficiency (Openness Improvement)",
       subtitle = "Analyzing Routes' Openness Improvement With Pre-Snap Movement Against Defensive Coverage",
       caption = "2022 NFL Season - Weeks 1-9",
       x = "Route-Coverage Combination") +
  geom_text(aes(label = round(motion_efficiency, 2), hjust = hjust), size = 3)
ggsave("motion_efficiency_plot.png", motion_efficiency_plot, width = 11, height = 6)

            
