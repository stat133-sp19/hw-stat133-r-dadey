### Data Dictionary

Variable Name | Plot Name | Type | No Obs. (Unique) | Description
--------------|-----------|------|------------------|-------------
name | Name | char | 5      | Player's full name
team_name | Team Name | factor | 1      | Name of basketball team
game_date | Date | char | 71 | Date of match
season | Season | factor | 1 | NBA season = '2016'
period | Period | int | 4 | Period of match
minutes_remaining | Minutes Remaining | int | 11 | Number of minutes remaining in period
seconds_remaining | Seconds Remaining | int | 11 | Number of seconds remaining in period; remainder of minutes
shot_made_flag | Shot successful? | chr | 2 | Indicates if shot was successful
action_type | Shot Type | factor | 33 | Type of shot
shot_type | Points | factor | 2 | Point value of shot
shot_distance | Distance | int | 56 | Distance of shot (in ft)
opponent | Opponent | factor | 28 | Opponents in match
x | X Coordinate (inches) | int | 356 | X Coordinate of shot, with respect to centreline
y | Y Coordinate (inches) | int | 225 | Y Coordinate of shot, with respect to centreline
minutes | Minutes Played | num | 48 | Number of minutes elapsed at time of shot
