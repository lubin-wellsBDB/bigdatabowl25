# Keep 'Em Separated: How Pre-Snap Motion Contributes to Receiver Separation

## How to Run

To run the python scripts, you first must install the requirements with `pip install -r requirements.txt`

You must also populate the `/data` directory with the output from `data loading 2.R`

With these, `openness.py` and `leverage.py` are ready to use.

The `animation.py`and `animation.ipynb` files are far from production ready code - they require tracking data which has been
normalized according to `Data loading.R`, and require that you specify the gameId, playId, and nflId of the player who is in
motion on the play you are animating in the function arguments. They also require that you have previously run `openness.py`,
along with adding the `games.csv`, `players.csv`, and `plays.csv` files to the `/data` directory.
