# Massive credit to Ben Davis and Nidiyan Rajendran's 2024 submission, Pull the Plug, for much of the basic animation work:
# https://github.com/BenDavis71/BDB/blob/main/coneAnalysis/ConeVisV2.ipynb
import plotly.graph_objects as go
import numpy as np
import pandas as pd
import plotly.graph_objects as go
import os
import imageio
import math

# for mpl animation
import matplotlib.animation as animation
from matplotlib import rc
rc('animation', html='html5')

tracking = pd.read_csv("data/crossing_without_motion_play.csv")
openness = pd.read_csv("data/openness_by_play.csv")
games = pd.read_csv("data/games.csv")
plays = pd.read_csv("data/plays.csv")


colors = {
    'ARI':["#97233F","#000000","#FFB612"], 
    'ATL':["#A71930","#000000","#A5ACAF"], 
    'BAL':["#241773","#000000"], 
    'BUF':["#00338D","#C60C30"], 
    'CAR':["#0085CA","#101820","#BFC0BF"], 
    'CHI':["#0B162A","#C83803"], 
    'CIN':["#FB4F14","#000000"], 
    'CLE':["#311D00","#FF3C00"], 
    'DAL':["#003594","#041E42","#869397"],
    'DEN':["#FB4F14","#002244"], 
    'DET':["#0076B6","#B0B7BC","#000000"], 
    'GB' :["#203731","#FFB612"], 
    'HOU':["#03202F","#A71930"], 
    'IND':["#002C5F","#A2AAAD"], 
    'JAX':["#101820","#D7A22A","#9F792C"], 
    'KC' :["#E31837","#FFB81C"], 
    'LA' :["#003594","#FFA300","#FF8200"], 
    'LAC':["#0080C6","#FFC20E","#FFFFFF"], 
    'LV' :["#000000","#A5ACAF"],
    'MIA':["#008E97","#FC4C02","#005778"], 
    'MIN':["#4F2683","#FFC62F"], 
    'NE' :["#002244","#C60C30","#B0B7BC"], 
    'NO' :["#101820","#D3BC8D"], 
    'NYG':["#0B2265","#A71930","#A5ACAF"], 
    'NYJ':["#125740","#000000","#FFFFFF"], 
    'PHI':["#004C54","#A5ACAF","#ACC0C6"], 
    'PIT':["#FFB612","#101820"], 
    'SEA':["#002244","#69BE28","#A5ACAF"], 
    'SF' :["#AA0000","#B3995D"],
    'TB' :["#D50A0A","#FF7900","#0A0A08"], 
    'TEN':["#0C2340","#4B92DB","#C8102E"], 
    'WAS':["#5A1414","#FFB612"], 
    'football':["#CBB67C","#663831"]
}

def hex_to_rgb_array(hex_color):
    '''take in hex val and return rgb np array'''
    return np.array(tuple(int(hex_color.lstrip('#')[i:i+2], 16) for i in (0, 2, 4))) 

def ColorDistance(hex1,hex2):
    '''d = {} distance between two colors(3)'''
    if hex1 == hex2:
        return 0
    rgb1 = hex_to_rgb_array(hex1)
    rgb2 = hex_to_rgb_array(hex2)
    rm = 0.5*(rgb1[0]+rgb2[0])
    d = abs(sum((2+rm,4,3-rm)*(rgb1-rgb2)**2))**0.5
    return d

def ColorPairs(team1,team2):
    color_array_1 = colors[team1]
    color_array_2 = colors[team2]

    # If color distance is small enough then flip color order
    if ColorDistance(color_array_1[0],color_array_2[0])<500:
        return {team1:[color_array_1[0],color_array_1[1]],team2:[color_array_2[1],color_array_2[0]],'football':colors['football']}
    else:
        return {team1:[color_array_1[0],color_array_1[1]],team2:[color_array_2[0],color_array_2[1]],'football':colors['football']}
    
def animate_play(games,tracking_df,play_df,openness,gameId,playId, man_in_motion):
    selected_game_df = games[games.gameId==gameId].copy()
    selected_play_df = play_df[(play_df.playId==playId)&(play_df.gameId==gameId)].copy()
    
    selected_tracking_df = tracking_df[(tracking_df.playId==playId)&(tracking_df.gameId==gameId)].copy()
    selected_tracking_df["x_new"] += 10
    selected_tracking_df["y_new"] = np.abs(53.5 - selected_tracking_df["y_new"])

    open_data = openness[(openness.gameId==gameId)&(openness.playId==playId)&(openness.receiver_id==man_in_motion)]
    mim_defender_id = open_data.defender_id.unique()[0]

    motion_frame_row = selected_tracking_df.loc[selected_tracking_df["event"] == "man_in_motion"].frameId.unique()
    shift_frame_row = selected_tracking_df.loc[selected_tracking_df["event"] == "shift"].frameId.unique()
    snap_frame = selected_tracking_df.loc[selected_tracking_df["event"] == "ball_snap"].frameId.unique()[0]
    pass_forward_frame = selected_tracking_df.loc[selected_tracking_df["event"] == "pass_forward"].frameId.unique()[0]
    pass_arrived_row = selected_tracking_df.loc[selected_tracking_df["event"] == "pass_arrived"].frameId.unique()

    pass_arrived_frame = -1
    if len(pass_arrived_row) != 0:
        pass_arrived_frame = pass_arrived_row[0]

    sorted_frame_list = list(selected_tracking_df.frameId.unique())
    max_frame = max(sorted_frame_list)

    frames_to_pause = 10
    sorted_frame_list.extend([pass_forward_frame for _ in range(frames_to_pause)])
    if pass_arrived_frame != -1:
        sorted_frame_list.extend([pass_arrived_frame for _ in range(frames_to_pause)])
    if snap_frame + 10 <= max_frame:
        sorted_frame_list.extend([snap_frame + 10 for _ in range(frames_to_pause)])
    if snap_frame + 20 <= max_frame:
        sorted_frame_list.extend([snap_frame + 20 for _ in range(frames_to_pause)])
    if snap_frame + 30 <= max_frame:
        sorted_frame_list.extend([snap_frame + 30 for _ in range(frames_to_pause)])
    if snap_frame + 40 <= max_frame:
        sorted_frame_list.extend([snap_frame + 40 for _ in range(frames_to_pause)])
    if snap_frame + 50 <= max_frame:
        sorted_frame_list.extend([snap_frame + 50 for _ in range(frames_to_pause)])
    sorted_frame_list.sort()

    motion_frame = -1

    if len(motion_frame_row) != 0:
        motion_frame = motion_frame_row[0]
    elif len(shift_frame_row) != 0:
        motion_frame = shift_frame_row[0]
    
    # get good color combos
    team_combos = list(set(selected_tracking_df.club.unique())-set(["football"]))
    
    color_orders = ColorPairs(team_combos[0],team_combos[1])
        
    # get play General information 
    line_of_scrimmage = np.where(selected_tracking_df.playDirection.values[0] == "right", selected_play_df.absoluteYardlineNumber.values[0], 120 - selected_play_df.absoluteYardlineNumber.values[0])

    frames = []

    motion_player_trajectory = {"x": [], "y": []}
    route_player_trajectory = {"x": [], "y": []}
    frames_waited = 0

    for frameId in sorted_frame_list:
        data = []

        # Add Numbers to Field 
        data.append(
            go.Scatter(
                y=np.arange(20,110,10), 
                x=[5]*len(np.arange(20,110,10)),
                mode='text',
                text=list(map(str,list(np.arange(20, 61, 10)-10)+list(np.arange(40, 9, -10)))),
                textfont_size = 30,
                textfont_family = "Courier New, monospace",
                textfont_color = "#ffffff",
                showlegend=False,
                hoverinfo='none'
            )
        )
        data.append(
            go.Scatter(
                y=np.arange(20,110,10), 
                x=[53.5-5]*len(np.arange(20,110,10)),
                mode='text',
                text=list(map(str,list(np.arange(20, 61, 10)-10)+list(np.arange(40, 9, -10)))),
                textfont_size = 30,
                textfont_family = "Courier New, monospace",
                textfont_color = "#ffffff",
                showlegend=False,
                hoverinfo='none'
            )
        )

        # Add line of scrimage 
        data.append(
            go.Scatter(
                y=[line_of_scrimmage,line_of_scrimmage], 
                x=[0,53.5],
                line_dash='dash',
                line_color='blue',
                showlegend=False,
                hoverinfo='none'
            )
        )

        # Add Endzone Colors 
        endzoneColors = {0:color_orders[selected_game_df.homeTeamAbbr.values[0]][0],
                         110:color_orders[selected_game_df.visitorTeamAbbr.values[0]][0]}
        
        for x_min in [0,110]:
            data.append(
                go.Scatter(
                    y=[x_min,x_min,x_min+10,x_min+10,x_min],
                    x=[0,53.5,53.5,0,0],
                    fill="toself",
                    fillcolor=endzoneColors[x_min],
                    mode="lines",
                    line=dict(
                        color="white",
                        width=3
                        ),
                    opacity=1,
                    showlegend= False,
                    hoverinfo ="skip"
                )
            )

        # Get the player's position at the current frame
        motion_player_df = selected_tracking_df[
            (selected_tracking_df.frameId == frameId) & 
            (selected_tracking_df.nflId == man_in_motion)
        ].copy()

        mim_defender_df = selected_tracking_df[
            (selected_tracking_df.frameId == frameId) & 
            (selected_tracking_df.nflId == mim_defender_id)
        ].copy()

        if motion_frame != -1 and frameId >= motion_frame and frameId <= snap_frame and not motion_player_df.empty:
            motion_player_trajectory["x"].append(motion_player_df["y_new"].values[0])
            motion_player_trajectory["y"].append(motion_player_df["x_new"].values[0])

        if frameId >= snap_frame and not motion_player_df.empty:
            route_player_trajectory["x"].append(motion_player_df["y_new"].values[0])
            route_player_trajectory["y"].append(motion_player_df["x_new"].values[0])
            
        data.append(go.Scatter(
            x=motion_player_trajectory["x"],  
            y=motion_player_trajectory["y"],
            mode="lines",
            line=dict(color="blue", width=2),
            name="Motion Path",
            hoverinfo="skip",
            showlegend=False
        ))         

        rec_x = motion_player_df.x.values[0] + (motion_player_df.s.values[0] * np.sin(motion_player_df.dir.values[0] / (2 * math.pi)) * .5)
        rec_y = motion_player_df.y.values[0] + (motion_player_df.s.values[0] * np.cos(motion_player_df.dir.values[0] / (2 * math.pi)) * .5)
        def_x = mim_defender_df.x.values[0] + (mim_defender_df.s.values[0] * np.sin(mim_defender_df.dir.values[0] / (2 * math.pi)) * .5)
        def_y = mim_defender_df.y.values[0] + (mim_defender_df.s.values[0] * np.cos(mim_defender_df.dir.values[0] / (2 * math.pi)) * .5)
        frame_openness = np.sqrt((rec_x - def_x) ** 2 + (rec_y - def_y) ** 2)/7 # over 14 we consider very open

        red = 255.0
        green = 255.0

        if frame_openness > 2.0:
            frame_openness = 2.0
        
        if frame_openness > 1.0:
            red *= (2 - frame_openness)
        else: green *= frame_openness

        color = f"rgba({int(red)}, {int(green)}, 0, 1)"

        data.append(go.Scatter(
            x=route_player_trajectory["x"],  
            y=route_player_trajectory["y"],
            mode="lines",
            line=dict(color=color, width=2),
            name="Route Path",
            hoverinfo="skip",
            showlegend=False
        ))    
        
        for team in selected_tracking_df.club.unique():
            plot_df = selected_tracking_df[(selected_tracking_df.club==team)&(selected_tracking_df.frameId==frameId)].copy()
            if team != "football":
                hover_text_array=[]
                for nflId in plot_df.nflId:
                    selected_player_df = plot_df[plot_df.nflId==nflId]                        
                    hover_text_array.append("nflId:{}<br>displayName:{}<br>Player Speed:{} yd/s".format(selected_player_df["nflId"].values[0],
                                                                                      selected_player_df["displayName"].values[0],
                                                                                      selected_player_df["s"].values[0]))
                data.append(go.Scatter(x=plot_df["y_new"], y=plot_df["x_new"],mode = 'markers',marker=go.scatter.Marker(
                                                                                             color=color_orders[team][0],
                                                                                             line=go.scatter.marker.Line(width=2,
                                                                                                            color=color_orders[team][1]),
                                                                                             size=10),
                                        name=team,hovertext=hover_text_array,hoverinfo="text"))
            else:
                data.append(go.Scatter(x=plot_df["y_new"], y=plot_df["x_new"],mode = 'markers',marker=go.scatter.Marker(
                                                                                             color=color_orders[team][0],
                                                                                             line=go.scatter.marker.Line(width=2,
                                                                                                            color=color_orders[team][1]),
                                                                                             size=10),
                                        name="Football",hoverinfo='none'))
        
        open_text = ""
        frames_waited += 1
        if frameId == pass_arrived_frame:        
            open_text = f"Openness: {"%.3f" % open_data.openness_after_pass_arrived.unique()[0]}"
        elif frameId == pass_forward_frame:            
            open_text = f"Openness: {"%.3f" % open_data.openness_after_pass_forward.unique()[0]}"
        elif frameId == snap_frame + 10:            
            open_text = f"Openness: {"%.3f" % open_data.openness_after_10.unique()[0]}"
        elif frameId == snap_frame + 20:            
            open_text = f"Openness: {"%.3f" % open_data.openness_after_20.unique()[0]}"  
        elif frameId == snap_frame + 30:            
            open_text = f"Openness: {"%.3f" % open_data.openness_after_30.unique()[0]}"  
        elif frameId == snap_frame + 40:            
            open_text = f"Openness: {"%.3f" % open_data.openness_after_40.unique()[0]}"
        elif frameId == snap_frame + 50: 
            open_text = f"Openness: {"%.3f" % open_data.openness_after_50.unique()[0]}" 
        else: frames_waited = 0   

        data.append(go.Scatter(
                x=[motion_player_df["y_new"].values[0]],
                y=[motion_player_df["x_new"].values[0] + 2], 
                mode="text",
                text=[open_text],
                textfont=dict(
                    color="black",
                    size=14,
                    family="Arial",
                ),
                name="Openness Text",
                hoverinfo="none",
                showlegend=False
            ))
    
        waited_text = "" if frames_waited == 0 else f"Restarting in {frames_to_pause - frames_waited} frames"
        data.append(go.Scatter(
                x=[10],
                y=[120], 
                mode="text",
                text=[waited_text],
                textfont=dict(
                    color="black",
                    size=14,
                    family="Arial",
                ),
                name="Openness Text",
                hoverinfo="none",
                showlegend=False
            ))

        frames.append(go.Frame(data=data, name=str(frameId)))

    scale=10
    layout = go.Layout(
        autosize=False,
        height=120*scale,
        width=60*scale,
        yaxis=dict(range=[0, 120], autorange=False, tickmode='array',tickvals=np.arange(10, 111, 5).tolist(),showticklabels=False),
        xaxis=dict(range=[0, 53.3], autorange=False,showgrid=False,showticklabels=False),

        plot_bgcolor='#00B140',
    )

    fig = go.Figure(
        data=frames[0]["data"],
        layout= layout,
        frames=frames[1:]
    )

    # Add Team Abbreviations in EndZone's
    for y_min in [0,110]:
        if y_min == 0:
            teamName=selected_game_df.homeTeamAbbr.values[0]
        else:
            teamName=selected_game_df.visitorTeamAbbr.values[0]
            
        fig.add_annotation(
            y=y_min+5,
            x=53.5/2,
            text=teamName,
            showarrow=False,
            font=dict(
                family="Courier New, monospace",
                size=32,
                color="White"
                ),
            textangle = 0
        )

    # Directory to save images
    os.makedirs("frames", exist_ok=True)

    # Generate and save each frame
    for i, frame in enumerate(fig.frames):
        fig.update(data=frame.data)  
        file_name = f"frames/frame_{i:03d}.png"
        fig.write_image(file_name, engine="kaleido")

    images = []
    for i in range(len(fig.frames)):
        images.append(imageio.imread(f"frames/frame_{i:03d}.png"))
    imageio.mimsave("animation.gif", images, duration=0.1)  

    print("Animation saved as animation.gif")

  
    return fig

animate_play(games, tracking, plays, openness, 2022101000, 3837, 47839)