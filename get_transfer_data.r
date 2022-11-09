library(toRvik)
library(tidyverse)

# load transfers
transfers <- transfer_portal()

# define high-majors schools; this list should change a bit over the years
# but for the rough draft, I am just keeping to these schools
hm <- c("ACC", "Amer", "B12", "B10", "SEC", "P12")

# join with conferences b/c i guess i never did that in the original data -- oh well
teams <- bart_teams()

transfers <- left_join(
  transfers,
  teams |> rename("from_conf" = "conf"),
  by = c("from" = "team", "year")
) |>
  left_join(
    teams |> rename("to_conf" = "conf"),
    by = c("to" = "team", "year")
  ) |>
  # indicate if a player went from hm-hm or nh-hm
  mutate(path = case_when(
    from_conf %in% hm & to_conf %in% hm ~ 0,
    !from_conf %in% hm & to_conf %in% hm ~ 1,
    from_conf %in% hm & !to_conf %in% hm ~ 2,
    !from_conf %in% hm & !to_conf %in% hm ~ 3,
    from_d1 == FALSE ~ 4,
    to_d1 == FALSE ~ 5
  ))

# PATH KEY:
# 0 = from a high-major conf. to a high-major conf.
# 1 = from a non-high major conf. to a high-major conf.
# 2 = from a high-major conf. to a non-high major conf.
# 3 = from a non-high major conf. to a non-high major conf.
# 4 = from a non-d1 school to any d1 school
# 5 = from a d1 school to a non-d1 school

# GET SEASON DATA =========================================================

# grab player season data
player_season <- bart_player_season(stat = "all", load_all = TRUE) |>
  filter(id %in% transfers$id)

# indicate if the season is the player's first since transfering
player_season_data <- player_season |>
  group_by(id) |>
  arrange(year, .by_group = TRUE) |>
  mutate(new_school = case_when(
    # first year playing or same team as prev. year
    row_number() == 1 | lag(team) == team ~ 0,
    .default = 1
  ))

# write data to .csv for gh
# write.csv(player_season, 'transfer_player_season_data.csv')
