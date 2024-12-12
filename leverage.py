import math
import pandas as pd
import numpy as np

if __name__ == "__main__":
    data = pd.read_csv("data/receiver_and_defender_tracking_data.csv")
    snap_data = data.loc[data["event"] == "ball_snap"]
    for i in [5, 10, 15, 20]:
        snap_data[f'receiver_x_after_{i}'] = snap_data["receiver_x"] + (snap_data["receiver_s"] * np.sin(snap_data["receiver_dir"]) * (i/10.0))
        snap_data[f'receiver_y_after_{i}'] = snap_data["receiver_y"] + (snap_data["receiver_s"] * np.cos(snap_data["receiver_dir"]) * (i/10.0))
        snap_data[f'defender_x_after_{i}'] = snap_data["defender_x"] + (snap_data["defender_s"] * np.sin(snap_data["defender_dir"]) * (i/10.0))
        snap_data[f'defender_y_after_{i}'] = snap_data["defender_y"] + (snap_data["defender_s"] * np.cos(snap_data["defender_dir"]) * (i/10.0))
        snap_data[f'receiver_dist_to_defender_after_{i}'] = np.sqrt((snap_data[f"receiver_x_after_{i}"] - snap_data[f"defender_x_after_{i}"]) ** 2 + (snap_data[f"receiver_y_after_{i}"] - snap_data[f"defender_y_after_{i}"]) ** 2)
        snap_data[f'dist_diff_after_{i}'] = snap_data[f"receiver_dist_to_defender_after_{i}"] - snap_data["receiver_dist_to_defender"]
    snap_data["rate_of_delta_dist"] = snap_data["dist_diff_after_20"]/2
    normalizer = max(np.abs(snap_data["rate_of_delta_dist"]))
    snap_data["normalized_rate_of_delta_dist"] = snap_data["rate_of_delta_dist"] / normalizer
    snap_data.to_csv("data/dist_by_play.csv")