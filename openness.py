import math
import pandas as pd
import numpy as np

if __name__ == "__main__":
    data = pd.read_csv("data/whole_receiver_and_defender_tracking_data.csv")
    snap_data = data.loc[data["event"] == "ball_snap"]
    values = [(10, [], "openness_after_10"), (20, [], "openness_after_20"), (30, [], "openness_after_30"), (40, [], "openness_after_40"), (50, [], "openness_after_50")]
    openness_after_pass_forward = []
    openness_after_pass_arrived = []
    counter = 0
    for row in snap_data.itertuples():
        counter += 1
        if counter % 100 == 0:
            print(f"{(100.0 * counter)/len(snap_data)}% done")
        play = data.loc[data["gameId"] == row.gameId].loc[data["playId"] == row.playId].loc[data["receiver_id"] == row.receiver_id]
        
        for val in values:
            play_row = play.loc[play["frameId"] == row.frameId + val[0]]
            if len(play_row) != 0: 
                rec_x = play_row["receiver_x"].iat[0] + (play_row["receiver_s"].iat[0] * np.sin(play_row["receiver_dir"].iat[0] / (2 * math.pi)) * .5)
                rec_y = play_row["receiver_y"].iat[0] + (play_row["receiver_s"].iat[0] * np.cos(play_row["receiver_dir"].iat[0] / (2 * math.pi)) * .5)
                def_x = play_row["defender_x"].iat[0] + (play_row["defender_s"].iat[0] * np.sin(play_row["defender_dir"].iat[0] / (2 * math.pi)) * .5)
                def_y = play_row["defender_y"].iat[0] + (play_row["defender_s"].iat[0] * np.cos(play_row["defender_dir"].iat[0] / (2 * math.pi)) * .5)
                val[1].append(np.sqrt((rec_x - def_x) ** 2 + (rec_y - def_y) ** 2))
            else:
                val[1].append(None)
        
        pass_forward_row = play.loc[play["event"] == "pass_forward"]

        if len(pass_forward_row) != 0:
            rec_x = pass_forward_row["receiver_x"].iat[0] + (pass_forward_row["receiver_s"].iat[0] * np.sin(pass_forward_row["receiver_dir"].iat[0] / (2 * math.pi)) * .5)
            rec_y = pass_forward_row["receiver_y"].iat[0] + (pass_forward_row["receiver_s"].iat[0] * np.cos(pass_forward_row["receiver_dir"].iat[0] / (2 * math.pi)) * .5)
            def_x = pass_forward_row["defender_x"].iat[0] + (pass_forward_row["defender_s"].iat[0] * np.sin(pass_forward_row["defender_dir"].iat[0] / (2 * math.pi)) * .5)
            def_y = pass_forward_row["defender_y"].iat[0] + (pass_forward_row["defender_s"].iat[0] * np.cos(pass_forward_row["defender_dir"].iat[0] / (2 * math.pi)) * .5)
            openness_after_pass_forward.append(np.sqrt((rec_x - def_x) ** 2 + (rec_y - def_y) ** 2))
        else:
            openness_after_pass_forward.append(None)

        pass_arrived_row = play.loc[play["event"] == "pass_arrived"]

        if len(pass_arrived_row) != 0:
            rec_x = pass_arrived_row["receiver_x"].iat[0] + (pass_arrived_row["receiver_s"].iat[0] * np.sin(pass_arrived_row["receiver_dir"].iat[0] / (2 * math.pi)) * .5)
            rec_y = pass_arrived_row["receiver_y"].iat[0] + (pass_arrived_row["receiver_s"].iat[0] * np.cos(pass_arrived_row["receiver_dir"].iat[0] / (2 * math.pi)) * .5)
            def_x = pass_arrived_row["defender_x"].iat[0] + (pass_arrived_row["defender_s"].iat[0] * np.sin(pass_arrived_row["defender_dir"].iat[0] / (2 * math.pi)) * .5)
            def_y = pass_arrived_row["defender_y"].iat[0] + (pass_arrived_row["defender_s"].iat[0] * np.cos(pass_arrived_row["defender_dir"].iat[0] / (2 * math.pi)) * .5)
            openness_after_pass_arrived.append(np.sqrt((rec_x - def_x) ** 2 + (rec_y - def_y) ** 2))
        else:
            openness_after_pass_arrived.append(None)
        
    for val in values:
        snap_data[val[2]] = val[1]

    snap_data["openness_after_pass_forward"] = openness_after_pass_forward
    snap_data["openness_after_pass_arrived"] = openness_after_pass_arrived

    print(snap_data)
    snap_data.to_csv("data/openness_by_play.csv")