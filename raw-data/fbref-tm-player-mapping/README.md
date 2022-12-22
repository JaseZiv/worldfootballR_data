# Mapping FBref and Transfermarkt Players

This section creates a map of player URLs from FBref players to the relevant player's data on Transfermarkt.

Currently, the mappings are for players who have played in the top 5 European leagues since the start of the 2017-18 season.

I aim to update this fairly frequently, so that players who subsequently appear on FBref in these leagues will continue to be mapped.

***

## Show your support
Follow me on Twitter ([jaseziv](https://twitter.com/jaseziv)) for updates

If this package helps you, all I ask is that you star this repo. If you did want to show your support and contribute to server time and data storage costs, feel free to send a small donation through the link below.

<a href="https://www.buymeacoffee.com/jaseziv83A" target="_blank"><img src="https://cdn.buymeacoffee.com/buttons/default-orange.png" alt="Coffee (Server Time)" height="41" width="174"></a>

***

## Usage

To update the data, first run `prepare_working_files.R`. This will generate a list of csv outputs. There are two that will potentially need to be actioned:

* `joined_missing.csv` contains the players who haven't been able to be matched by the automated script. These need to be manually investigated and then overwrite the `joined_missing_manual_fix.csv` file
* `duplicate_players_df.csv` contains a list of players who have been joined using the automated script, however duplicates have arisen. Manually fix these duplicates by removing the spurious matches, then save to file called `duplicate_players_df_manual_fix.csv`.

Once these files have been manually fixed, run `create_final_data.R` and the final output file will be written to [`output/fbref_to_tm_mapping.csv`](https://github.com/JaseZiv/worldfootballR_data/blob/master/raw-data/fbref-tm-player-mapping/output/fbref_to_tm_mapping.csv).

### Update (2021-10-29): Write to Googlesheets

The project also writes the mapped data to a gogglesheet, found [here](https://docs.google.com/spreadsheets/d/1GjjS9IRp6FVzVX5QyfmttMk8eYBtIzuZ_YIM0VWg8OY/edit#gid=61874932).


### Update (2022-08-11) Update Player Positions

For players that were mapped originally, some of these may have changed positions since the initial map. To get current TM positions, run the file names `update_player_positions.R`.

***

## Contributing

If anyone wants to contribute mapped players for different leagues, feel free to get in touch with me on Twitter [here](https://twitter.com/jaseziv), create an issue in [`worldfootballR`](https://github.com/JaseZiv/worldfootballR) or email me on `jaseziv83@gmail.com`.
