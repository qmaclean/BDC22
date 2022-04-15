
# Packages
library(gt)
library(tidyverse)
library(purrr)
library(gifski)
library(gganimate) 


### load data ####
getwd()

# contextual event data data
nwhl<-read_csv("Src/hackathon_nwhl.csv")
womens<-read_csv("Src/hackathon_womens.csv")
pxp_womens<-read_csv("Src/pxp_womens_oly_2022_v2.csv")

## 
glimpse(nwhl)
glimpse(pxp_womens)
glimpse(womens)

#### tracking data pp level info
pp_info<-read_csv("Src/TrackingData/pp_info.csv")

pp_info<-pp_info %>%
  mutate(total_game_seconds_remaining_start = 
           if_else(start_period == 1,start_game_clock_seconds + 2400,
                   if_else(start_period == 2,start_game_clock_seconds + 1200,
                           start_game_clock_seconds)),
         total_game_seconds_remaining_end = 
           if_else(end_period == 1,end_game_clock_seconds + 2400,
                   if_else(end_period == 2,end_game_clock_seconds + 1200,
                           end_game_clock_seconds)))



### pxp womens
pxp_womens<-pxp_womens %>%
  mutate(total_clock_seconds = 
           if_else(period == 1,clock_seconds + 2400,
                   if_else(period == 2,clock_seconds + 1200,
                           clock_seconds)),
        game_name = case_when(
          #Game 1
          game_date == "8/2/2022" & team_name == "Olympic (Women) - Canada" & venue == "away" ~ "2022-02-08 Canada at USA",
          game_date == "8/2/2022" & team_name == "Olympic (Women) - United States" & venue == "home" ~ "2022-02-08 Canada at USA",
          #Game 2
          game_date == "8/2/2022" & team_name == "Olympic (Women) - Olympic Athletes from Russia" & venue == "away" ~ "2022-02-08 ROC at Finland",
          game_date == "8/2/2022" & team_name == "Olympic (Women) - Finland" & venue == "home" ~ "2022-02-08 ROC at Finland",
          #Game 3
          game_date == "12/2/2022" & team_name ==  "Olympic (Women) - Switzerland" & venue == "away" ~ "2022-02-12 Switzerland at ROC",
          game_date == "12/2/2022" & team_name == "Olympic (Women) - Olympic Athletes from Russia" & venue == "home" ~ "2022-02-12 Switzerland at ROC",
          #Game 4
          game_date == "14/2/2022" & team_name ==  "Olympic (Women) - Switzerland" & venue == "away" ~ "2022-02-14 Switzerland at Canada",
          game_date == "14/2/2022" & team_name ==  "Olympic (Women) - Canada" & venue == "home" ~ "2022-02-14 Switzerland at Canada",
          #Game 5
          game_date == "14/2/2022" & team_name ==  "Olympic (Women) - United States" & venue == "away" ~ "2022-02-14 USA at Finland",
          game_date == "14/2/2022" & team_name ==  "Olympic (Women) - Finland" & venue == "home" ~ "2022-02-14 USA at Finland",
          #Game 6
          game_date == "16/2/2022" & team_name ==  "Olympic (Women) - Switzerland" & venue == "away" ~ "2022-02-16 Switzerland at Finland",
          game_date == "16/2/2022" & team_name ==  "Olympic (Women) - Finland" & venue == "away" ~ "2022-02-16 Switzerland at Finland",
          TRUE ~ as.character(NA)
          
        ),
        team_abbr = case_when(
          team_name == "Olympic (Women) - Canada" ~ "Canada",
          team_name == "Olympic (Women) - United States" ~ "USA",
          team_name == "Olympic (Women) - Olympic Athletes from Russia" ~ "ROC",
          team_name == "Olympic (Women) - Finland" ~ "Finland",
          team_name == "Olympic (Women) - Switzerland" ~ "Switzerland",
          TRUE ~ as.character(NA)
        )
      )


##### join event & Power play info
pxp_womens_pp<-pxp_womens %>%
  left_join(pp_info,by=c("game_name"="game_name")) %>%
  filter(total_clock_seconds <= total_game_seconds_remaining_start,
         total_clock_seconds >= total_game_seconds_remaining_end)
  
# final power play event data
pxp_womens_pp<-pxp_womens_pp %>%
  dplyr::filter(situation_type %in% c("4 on 5","5 on 4","6 on 5","5 on 6",
                                      "4 on 6","6 on 4","4 on 4"))

pxp_womens_pp<-pxp_womens_pp %>%
  mutate(playId = row_number(),
         start_zone = case_when(
           x_coord < 75 ~ "Defensive Zone",
           x_coord >= 75 & x_coord <= 125 ~ "Neutral Zone",
           x_coord > 125 ~ "Offensive Zone",
           TRUE ~ as.character(NA)
         ),
         end_zone = case_when(
           x_coord_2 < 75 ~ "Defensive Zone",
           x_coord_2 >= 75 & x_coord_2 <= 125 ~ "Neutral Zone",
           x_coord_2 > 125 ~ "Offensive Zone",
           TRUE ~ as.character(NA)
         )
  )

FPS<-1/30

## tracking data to do
# 1. union all pp data
# 2. left join to video shots info 
# 3. left join to get rosters

### iterate through to combine data
######### GM1 CAN vs USA ################
# GM1_PP1
CAN_USA_PP1<-read_csv("Src/TrackingData/2022-02-08 Canada at USA/2022-02-08 Canada at USA P1 PP1.csv")
CAN_USA_PP1$game_name<-"2022-02-08 Canada at USA"
CAN_USA_PP1$penalty<-1
# GM1_PP2
CAN_USA_PP2<-read_csv("Src/TrackingData/2022-02-08 Canada at USA/2022-02-08 Canada at USA P1 PP2.csv")
CAN_USA_PP2$game_name<-"2022-02-08 Canada at USA"
CAN_USA_PP2$penalty<-2
# GM1_PP3
CAN_USA_PP3<-read_csv("Src/TrackingData/2022-02-08 Canada at USA/2022-02-08 Canada at USA P2 PP3.csv")
CAN_USA_PP3$game_name<-"2022-02-08 Canada at USA"
CAN_USA_PP3$penalty<-3
# GM1_PP4
CAN_USA_PP4<-read_csv("Src/TrackingData/2022-02-08 Canada at USA/2022-02-08 Canada at USA P2 PP5.csv")
CAN_USA_PP4$game_name<-"2022-02-08 Canada at USA"
CAN_USA_PP4$penalty<-5
# GM1_PP5
CAN_USA_PP5<-read_csv("Src/TrackingData/2022-02-08 Canada at USA/2022-02-08 Canada at USA P3 PP6.csv")
CAN_USA_PP5$game_name<-"2022-02-08 Canada at USA"
CAN_USA_PP5$penalty<-6
# GM1_PP6
CAN_USA_PP6<-read_csv("Src/TrackingData/2022-02-08 Canada at USA/2022-02-08 Canada at USA P3 PP7.csv")
CAN_USA_PP6$game_name<-"2022-02-08 Canada at USA"
CAN_USA_PP6$penalty<-7

### video data 
#game 1 shot video
CAN_USA_PP1_vsi<-read_csv("Src/TrackingData/2022-02-08 Canada at USA/videoShotsInfo_2022-02-08 Canada at USA P1 PP1.csv")
CAN_USA_PP1_vsi$game_name<-"2022-02-08 Canada at USA"
CAN_USA_PP1_vsi$penalty<-1
#game 2 shot video
CAN_USA_PP2_vsi<-read_csv("Src/TrackingData/2022-02-08 Canada at USA/videoShotsInfo_2022-02-08 Canada at USA P1 PP2.csv")
CAN_USA_PP2_vsi$game_name<-"2022-02-08 Canada at USA"
CAN_USA_PP2_vsi$penalty<-2
#game 3 shot video
CAN_USA_PP3_vsi<-read_csv("Src/TrackingData/2022-02-08 Canada at USA/videoShotsInfo_2022-02-08 Canada at USA P2 PP3.csv")
CAN_USA_PP3_vsi$game_name<-"2022-02-08 Canada at USA"
CAN_USA_PP3_vsi$penalty<-3
#game 4 shot video
CAN_USA_PP4_vsi<-read_csv("Src/TrackingData/2022-02-08 Canada at USA/videoShotsInfo_2022-02-08 Canada at USA P2 PP5.csv")
CAN_USA_PP4_vsi$game_name<-"2022-02-08 Canada at USA"
CAN_USA_PP4_vsi$penalty<-5
#game 5 shot video
CAN_USA_PP5_vsi<-read_csv("Src/TrackingData/2022-02-08 Canada at USA/videoShotsInfo_2022-02-08 Canada at USA P3 PP6.csv")
CAN_USA_PP5_vsi$game_name<-"2022-02-08 Canada at USA"
CAN_USA_PP5_vsi$penalty<-6
#game 6 shot video
CAN_USA_PP6_vsi<-read_csv("Src/TrackingData/2022-02-08 Canada at USA/videoShotsInfo_2022-02-08 Canada at USA P3 PP7.csv")
CAN_USA_PP6_vsi$game_name<-"2022-02-08 Canada at USA"
CAN_USA_PP6_vsi$penalty<-7

## roster tracking
CAN_USA_roster<-read_csv("Src/TrackingData/2022-02-08 Canada at USA/2022-02-08 Canada at USA roster.csv")

#### fix roster 
### join on frame_id & period? 


##### TRANSFORM & JOIN VIDEO FILES ######
# PP1 # 
v1<-CAN_USA_PP1_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
    do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                  time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

CAN_USA_PP1<-CAN_USA_PP1 %>%
  left_join(v1,by=c("frame_id"))

CAN_USA_PP1<-CAN_USA_PP1 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(CAN_USA_PP1_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

CAN_USA_PP1<-CAN_USA_PP1 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

CAN_USA_PP1<-CAN_USA_PP1 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
                )

### join on ppinfo on v1 -> should be able to join to event data

rm(CAN_USA_PP1_vsi)
rm(v1)

# PP2 
v2<-CAN_USA_PP2_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

CAN_USA_PP2<-CAN_USA_PP2 %>%
  left_join(v2,by=c("frame_id"))


CAN_USA_PP2<-CAN_USA_PP2 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(CAN_USA_PP2_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

CAN_USA_PP2<-CAN_USA_PP2 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

CAN_USA_PP2<-CAN_USA_PP2 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )



rm(CAN_USA_PP2_vsi)
rm(v2)

# PP3
v3<-CAN_USA_PP3_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

CAN_USA_PP3<-CAN_USA_PP3 %>%
  left_join(v3,by=c("frame_id"))

CAN_USA_PP3<-CAN_USA_PP3 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(CAN_USA_PP3_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

CAN_USA_PP3<-CAN_USA_PP3 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

CAN_USA_PP3<-CAN_USA_PP3 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )



rm(CAN_USA_PP3_vsi)
rm(v3)

# PP4 
v4<-CAN_USA_PP4_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

CAN_USA_PP4<-CAN_USA_PP4 %>%
  left_join(v4,by=c("frame_id"))

CAN_USA_PP4<-CAN_USA_PP4 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(CAN_USA_PP4_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

CAN_USA_PP4<-CAN_USA_PP4 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

CAN_USA_PP4<-CAN_USA_PP4 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(CAN_USA_PP4_vsi)
rm(v4)

# PP5
v5<-CAN_USA_PP5_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

CAN_USA_PP5<-CAN_USA_PP5 %>%
  left_join(v5,by=c("frame_id"))

CAN_USA_PP5<-CAN_USA_PP5 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(CAN_USA_PP5_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

CAN_USA_PP5<-CAN_USA_PP5 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

CAN_USA_PP5<-CAN_USA_PP5 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(CAN_USA_PP5_vsi)
rm(v5)

# PP6
v6<-CAN_USA_PP6_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

CAN_USA_PP6<-CAN_USA_PP6 %>%
  left_join(v6,by=c("frame_id"))

CAN_USA_PP6<-CAN_USA_PP6 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(CAN_USA_PP6_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

CAN_USA_PP6<-CAN_USA_PP6 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

CAN_USA_PP6<-CAN_USA_PP6 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(CAN_USA_PP6_vsi)
rm(v6)

#### join to rosters ###
CAN_USA_FINAL<-rbind(CAN_USA_PP1,CAN_USA_PP2,CAN_USA_PP3,CAN_USA_PP4,CAN_USA_PP5,CAN_USA_PP6)



######### DIAGNOSE MISSING PLAYERS #####
CAN_USA_FINAL<-CAN_USA_FINAL %>%
  mutate(team = ifelse(CAN_USA_FINAL$team_name == "Canada","away","home")) %>%
  rename(jn = jersey_number) %>%
  left_join(CAN_USA_roster,by=c("jn","team"))


missing_players_CAN_USA_FINAL<-CAN_USA_FINAL %>%
  #dplyr::filter(!complete.cases(player)) %>%
  group_by(jn,team_name,player,position) %>%
  summarize(n = n())

CAN_USA_roster_mis<-CAN_USA_roster %>%
  left_join(missing_players_CAN_USA_FINAL,by=c("player"))







#### match event data to tracking data




write.csv(CAN_USA_FINAL,"DataTransform/TrackingMerge/CAN_USA_FINAL.csv")

rm(CAN_USA_PP1)
rm(CAN_USA_PP2)
rm(CAN_USA_PP3)
rm(CAN_USA_PP4)
rm(CAN_USA_PP5)
rm(CAN_USA_PP6)
rm(CAN_USA_roster)


######################### GAME2 ROC @ Finland ###
# GM2_PP1
ROC_FIN_PP1<-read_csv("Src/TrackingData/2022-02-08 ROC at Finland/2022-02-08 ROC at Finland P1 PP1.csv")
ROC_FIN_PP1$game_name<-"2022-02-08 ROC at Finland"
ROC_FIN_PP1$penalty<-1
# GM2_PP2
ROC_FIN_PP2<-read_csv("Src/TrackingData/2022-02-08 ROC at Finland/2022-02-08 ROC at Finland P2 PP2.csv")
ROC_FIN_PP2$game_name<-"2022-02-08 ROC at Finland"
ROC_FIN_PP2$penalty<-2
# GM2_PP3
ROC_FIN_PP3<-read_csv("Src/TrackingData/2022-02-08 ROC at Finland/2022-02-08 ROC at Finland P2 PP3.csv")
ROC_FIN_PP3$game_name<-"2022-02-08 ROC at Finland"
ROC_FIN_PP3$penalty<-3
# GM2_PP4
ROC_FIN_PP4<-read_csv("Src/TrackingData/2022-02-08 ROC at Finland/2022-02-08 ROC at Finland P2 PP4.csv")
ROC_FIN_PP4$game_name<-"2022-02-08 ROC at Finland"
ROC_FIN_PP4$penalty<-4
# GM2_PP5
ROC_FIN_PP5<-read_csv("Src/TrackingData/2022-02-08 ROC at Finland/2022-02-08 ROC at Finland P2 PP5.csv")
ROC_FIN_PP5$game_name<-"2022-02-08 ROC at Finland"
ROC_FIN_PP5$penalty<-5
# GM2_PP5
ROC_FIN_PP6<-read_csv("Src/TrackingData/2022-02-08 ROC at Finland/2022-02-08 ROC at Finland P3 PP6.csv")
ROC_FIN_PP6$game_name<-"2022-02-08 ROC at Finland"
ROC_FIN_PP6$penalty<-6

### video data 
# Game 1
ROC_FIN_PP1_vsi<-read_csv("Src/TrackingData/2022-02-08 ROC at Finland/videoShotsInfo_2022-02-08 ROC at Finland P1 PP1.csv")
ROC_FIN_PP1_vsi$game_name<-"2022-02-08 ROC at Finland"
ROC_FIN_PP1_vsi$penalty<-1
# Game 2
ROC_FIN_PP2_vsi<-read_csv("Src/TrackingData/2022-02-08 ROC at Finland/videoShotsInfo_2022-02-08 ROC at Finland P2 PP2.csv")
ROC_FIN_PP2_vsi$game_name<-"2022-02-08 ROC at Finland"
ROC_FIN_PP2_vsi$penalty<-2
# Game 3
ROC_FIN_PP3_vsi<-read_csv("Src/TrackingData/2022-02-08 ROC at Finland/videoShotsInfo_2022-02-08 ROC at Finland P2 PP3.csv")
ROC_FIN_PP3_vsi$game_name<-"2022-02-08 ROC at Finland"
ROC_FIN_PP3_vsi$penalty<-3
# Game 4
ROC_FIN_PP4_vsi<-read_csv("Src/TrackingData/2022-02-08 ROC at Finland/videoShotsInfo_2022-02-08 ROC at Finland P2 PP4.csv")
ROC_FIN_PP4_vsi$game_name<-"2022-02-08 ROC at Finland"
ROC_FIN_PP4_vsi$penalty<-4
# Game 5
ROC_FIN_PP5_vsi<-read_csv("Src/TrackingData/2022-02-08 ROC at Finland/videoShotsInfo_2022-02-08 ROC at Finland P2 PP5.csv")
ROC_FIN_PP5_vsi$game_name<-"2022-02-08 ROC at Finland"
ROC_FIN_PP5_vsi$penalty<-5
# Game 6
ROC_FIN_PP6_vsi<-read_csv("Src/TrackingData/2022-02-08 ROC at Finland/videoShotsInfo_2022-02-08 ROC at Finland P3 PP6.csv")
ROC_FIN_PP6_vsi$game_name<-"2022-02-08 ROC at Finland"
ROC_FIN_PP6_vsi$penalty<-6

## roster tracking
ROC_FIN_roster<-read_csv("Src/TrackingData/2022-02-08 ROC at Finland/2022-02-08 ROC at Finland roster.csv")


##### TRANSFORM & JOIN VIDEO FILES ######
# PP1 # 
v1<-ROC_FIN_PP1_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

ROC_FIN_PP1<-ROC_FIN_PP1 %>%
  left_join(v1,by=c("frame_id"))

ROC_FIN_PP1<-ROC_FIN_PP1 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(ROC_FIN_PP1_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

ROC_FIN_PP1<-ROC_FIN_PP1 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

ROC_FIN_PP1<-ROC_FIN_PP1 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )



rm(ROC_FIN_PP1_vsi)
rm(v1)


# PP2 
v2<-ROC_FIN_PP2_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

ROC_FIN_PP2<-ROC_FIN_PP2 %>%
  left_join(v2,by=c("frame_id"))

ROC_FIN_PP2<-ROC_FIN_PP2 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(ROC_FIN_PP2_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

ROC_FIN_PP2<-ROC_FIN_PP2 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

ROC_FIN_PP2<-ROC_FIN_PP2 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )


rm(ROC_FIN_PP2_vsi)
rm(v2)

# PP3
v3<-ROC_FIN_PP3_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

ROC_FIN_PP3<-ROC_FIN_PP3 %>%
  left_join(v3,by=c("frame_id"))

ROC_FIN_PP3<-ROC_FIN_PP3 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(ROC_FIN_PP3_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

ROC_FIN_PP3<-ROC_FIN_PP3 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

ROC_FIN_PP3<-ROC_FIN_PP3 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(ROC_FIN_PP3_vsi)
rm(v3)

# PP4 
v4<-ROC_FIN_PP4_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

ROC_FIN_PP4<-ROC_FIN_PP4 %>%
  left_join(v4,by=c("frame_id"))

ROC_FIN_PP4<-ROC_FIN_PP4 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(ROC_FIN_PP4_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

ROC_FIN_PP4<-ROC_FIN_PP4 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

ROC_FIN_PP4<-ROC_FIN_PP4 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(ROC_FIN_PP4_vsi)
rm(v4)

# PP5
v5<-ROC_FIN_PP5_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

ROC_FIN_PP5<-ROC_FIN_PP5 %>%
  left_join(v5,by=c("frame_id"))

ROC_FIN_PP5<-ROC_FIN_PP5 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(ROC_FIN_PP5_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

ROC_FIN_PP5<-ROC_FIN_PP5 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

ROC_FIN_PP5<-ROC_FIN_PP5 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(ROC_FIN_PP5_vsi)
rm(v5)

# PP6
v6<-ROC_FIN_PP6_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

ROC_FIN_PP6<-ROC_FIN_PP6 %>%
  left_join(v6,by=c("frame_id"))

ROC_FIN_PP6<-ROC_FIN_PP6 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(ROC_FIN_PP6_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

ROC_FIN_PP6<-ROC_FIN_PP6 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

ROC_FIN_PP6<-ROC_FIN_PP6 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(ROC_FIN_PP6_vsi)
rm(v6)

#### join to rosters ###
ROC_FIN_FINAL<-rbind(ROC_FIN_PP1,ROC_FIN_PP2,ROC_FIN_PP3,ROC_FIN_PP4,ROC_FIN_PP5,ROC_FIN_PP6)


ROC_FIN_FINAL<-ROC_FIN_FINAL %>%
  mutate(team = ifelse(ROC_FIN_FINAL$team_name == "ROC","away","home")) %>%
  rename(jn = jersey_number) %>%
  left_join(ROC_FIN_roster,by=c("jn","team"))

write.csv(ROC_FIN_FINAL,"DataTransform/TrackingMerge/ROC_FIN_FINAL.csv")

rm(ROC_FIN_PP1)
rm(ROC_FIN_PP2)
rm(ROC_FIN_PP3)
rm(ROC_FIN_PP4)
rm(ROC_FIN_PP5)
rm(ROC_FIN_PP6)
rm(ROC_FIN_roster)

##### GM 3 SWISS @ ROC ######
# GM3_PP1
SWISS_ROC_PP1<-read_csv("Src/TrackingData/2022-02-12 Switzerland at ROC/2022-02-12 Switzerland at ROC P1 PP1.csv")
SWISS_ROC_PP1$game_name<-"2022-02-12 Switzerland at ROC"
SWISS_ROC_PP1$penalty<-1
# GM2_PP2
SWISS_ROC_PP2<-read_csv("Src/TrackingData/2022-02-12 Switzerland at ROC/2022-02-12 Switzerland at ROC P1 PP2.csv")
SWISS_ROC_PP2$game_name<-"2022-02-12 Switzerland at ROC"
SWISS_ROC_PP2$penalty<-2
# GM2_PP3
SWISS_ROC_PP3<-read_csv("Src/TrackingData/2022-02-12 Switzerland at ROC/2022-02-12 Switzerland at ROC P3 PP3.csv")
SWISS_ROC_PP3$game_name<-"2022-02-12 Switzerland at ROC"
SWISS_ROC_PP3$penalty<-3
# GM2_PP4
SWISS_ROC_PP4<-read_csv("Src/TrackingData/2022-02-12 Switzerland at ROC/2022-02-12 Switzerland at ROC P3 PP5.csv")
SWISS_ROC_PP4$game_name<-"2022-02-12 Switzerland at ROC"
SWISS_ROC_PP4$penalty<-5


### video data
# game 1
SWISS_ROC_PP1_vsi<-read_csv("Src/TrackingData/2022-02-12 Switzerland at ROC/videoShotsInfo_2022-02-12 Switzerland at ROC P1 PP1.csv")
SWISS_ROC_PP1_vsi$game_name<-"2022-02-12 Switzerland at ROC"
SWISS_ROC_PP1_vsi$penalty<-1
# game 2
SWISS_ROC_PP2_vsi<-read_csv("Src/TrackingData/2022-02-12 Switzerland at ROC/videoShotsInfo_2022-02-12 Switzerland at ROC P1 PP2.csv")
SWISS_ROC_PP2_vsi$game_name<-"2022-02-12 Switzerland at ROC"
SWISS_ROC_PP2_vsi$penalty<-2
# game 3
SWISS_ROC_PP3_vsi<-read_csv("Src/TrackingData/2022-02-12 Switzerland at ROC/videoShotsInfo_2022-02-12 Switzerland at ROC P3 PP3.csv")
SWISS_ROC_PP3_vsi$game_name<-"2022-02-12 Switzerland at ROC"
SWISS_ROC_PP3_vsi$penalty<-3
# game 4
SWISS_ROC_PP4_vsi<-read_csv("Src/TrackingData/2022-02-12 Switzerland at ROC/videoShotsInfo_2022-02-12 Switzerland at ROC P3 PP5.csv")
SWISS_ROC_PP4_vsi$game_name<-"2022-02-12 Switzerland at ROC"
SWISS_ROC_PP4_vsi$penalty<-5

## roster tracking
SWISS_ROC_roster<-read_csv("Src/TrackingData/2022-02-12 Switzerland at ROC/2022-02-12 Switzerland at ROC roster.csv")


##### TRANSFORM & JOIN VIDEO FILES ######
# PP1 # 
v1<-SWISS_ROC_PP1_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

SWISS_ROC_PP1<-SWISS_ROC_PP1 %>%
  left_join(v1,by=c("frame_id"))

SWISS_ROC_PP1<-SWISS_ROC_PP1 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(SWISS_ROC_PP1_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

SWISS_ROC_PP1 <-SWISS_ROC_PP1  %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

SWISS_ROC_PP1<-SWISS_ROC_PP1 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(SWISS_ROC_PP1_vsi)
rm(v1)

# PP2 
v2<-SWISS_ROC_PP2_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

SWISS_ROC_PP2<-SWISS_ROC_PP2 %>%
  left_join(v2,by=c("frame_id"))


SWISS_ROC_PP2<-SWISS_ROC_PP2 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(SWISS_ROC_PP2_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

SWISS_ROC_PP2 <-SWISS_ROC_PP2  %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

SWISS_ROC_PP2<-SWISS_ROC_PP2 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(SWISS_ROC_PP2_vsi)
rm(v2)

# PP3
v3<-SWISS_ROC_PP3_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

SWISS_ROC_PP3<-SWISS_ROC_PP3 %>%
  left_join(v3,by=c("frame_id"))

SWISS_ROC_PP3<-SWISS_ROC_PP3 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(SWISS_ROC_PP3_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

SWISS_ROC_PP3 <-SWISS_ROC_PP3  %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

SWISS_ROC_PP3<-SWISS_ROC_PP3 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(SWISS_ROC_PP3_vsi)
rm(v3)

# PP4 
v4<-SWISS_ROC_PP4_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

SWISS_ROC_PP4<-SWISS_ROC_PP4 %>%
  left_join(v4,by=c("frame_id"))

SWISS_ROC_PP4<-SWISS_ROC_PP4 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(SWISS_ROC_PP4_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

SWISS_ROC_PP4 <-SWISS_ROC_PP4  %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

SWISS_ROC_PP4<-SWISS_ROC_PP4 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(SWISS_ROC_PP4_vsi)
rm(v4)


#### join to rosters ###
SWISS_ROC_FINAL<-rbind(SWISS_ROC_PP1,SWISS_ROC_PP2,SWISS_ROC_PP3,SWISS_ROC_PP4)


SWISS_ROC_FINAL<-SWISS_ROC_FINAL %>%
  mutate(team = ifelse(SWISS_ROC_FINAL$team_name == "Switzerland","away","home")) %>%
  rename(jn = jersey_number) %>%
  left_join(SWISS_ROC_roster,by=c("jn","team"))

write.csv(SWISS_ROC_FINAL,"DataTransform/TrackingMerge/SWISS_ROC_FINAL.csv")

rm(SWISS_ROC_PP1)
rm(SWISS_ROC_PP2)
rm(SWISS_ROC_PP3)
rm(SWISS_ROC_PP4)
rm(SWISS_ROC_roster)

########### GAME 4 FIN USA ############
# GM4_PP1
FIN_USA_PP1<-read_csv("Src/TrackingData/2022-02-14 Finland at USA/2022-02-14 Finland at USA P2 PP1.csv")
FIN_USA_PP1$game_name<-"2022-02-14 USA at Finland"
FIN_USA_PP1$penalty<-1
# GM4_PP2
FIN_USA_PP2<-read_csv("Src/TrackingData/2022-02-14 Finland at USA/2022-02-14 Finland at USA P2 PP3.csv")
FIN_USA_PP2$game_name<-"2022-02-14 USA at Finland"
FIN_USA_PP2$penalty<-3
# GM4_PP3
FIN_USA_PP3<-read_csv("Src/TrackingData/2022-02-14 Finland at USA/2022-02-14 Finland at USA P3 PP4.csv")
FIN_USA_PP3$game_name<-"2022-02-14 USA at Finland"
FIN_USA_PP3$penalty<-4
# GM4_PP4
FIN_USA_PP4<-read_csv("Src/TrackingData/2022-02-14 Finland at USA/2022-02-14 Finland at USA P3 PP5.csv")
FIN_USA_PP4$game_name<-"2022-02-14 USA at Finland"
FIN_USA_PP4$penalty<-5
# GM4_PP5
FIN_USA_PP5<-read_csv("Src/TrackingData/2022-02-14 Finland at USA/2022-02-14 Finland at USA P3 PP6.csv")
FIN_USA_PP5$game_name<-"2022-02-14 USA at Finland"
FIN_USA_PP5$penalty<-6

### video data 
# Game 1
FIN_USA_PP1_vsi<-read_csv("Src/TrackingData/2022-02-14 Finland at USA/videoShotsInfo_2022-02-14 Finland at USA P2 PP1.csv")
FIN_USA_PP1_vsi$game_name<-"2022-02-14 USA at Finland"
FIN_USA_PP1_vsi$penalty<-1
# Game 2
FIN_USA_PP2_vsi<-read_csv("Src/TrackingData/2022-02-14 Finland at USA/videoShotsInfo_2022-02-14 Finland at USA P2 PP3.csv")
FIN_USA_PP2_vsi$game_name<-"2022-02-14 USA at Finland"
FIN_USA_PP2_vsi$penalty<-3
# Game 3
FIN_USA_PP3_vsi<-read_csv("Src/TrackingData/2022-02-14 Finland at USA/videoShotsInfo_2022-02-14 Finland at USA P3 PP4.csv")
FIN_USA_PP3_vsi$game_name<-"2022-02-14 USA at Finland"
FIN_USA_PP3_vsi$penalty<-4
# Game 4
FIN_USA_PP4_vsi<-read_csv("Src/TrackingData/2022-02-14 Finland at USA/videoShotsInfo_2022-02-14 Finland at USA P3 PP5.csv")
FIN_USA_PP4_vsi$game_name<-"2022-02-14 USA at Finland"
FIN_USA_PP4_vsi$penalty<-5
# Game 5
FIN_USA_PP5_vsi<-read_csv("Src/TrackingData/2022-02-14 Finland at USA/videoShotsInfo_2022-02-14 Finland at USA P3 PP6.csv")
FIN_USA_PP5_vsi$game_name<-"2022-02-14 USA at Finland"
FIN_USA_PP5_vsi$penalty<-6

## roster tracking
FIN_USA_roster<-read_csv("Src/TrackingData/2022-02-14 Finland at USA/2022-02-14 Finland at USA roster.csv")


##### TRANSFORM & JOIN VIDEO FILES ######
# PP1 # 
v1<-FIN_USA_PP1_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

FIN_USA_PP1<-FIN_USA_PP1 %>%
  left_join(v1,by=c("frame_id"))

FIN_USA_PP1<-FIN_USA_PP1 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(FIN_USA_PP1_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

FIN_USA_PP1 <-FIN_USA_PP1  %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

FIN_USA_PP1<-FIN_USA_PP1 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )


rm(FIN_USA_PP1_vsi)
rm(v1)

# PP2 
v2<-FIN_USA_PP2_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

FIN_USA_PP2<-FIN_USA_PP2 %>%
  left_join(v2,by=c("frame_id"))

FIN_USA_PP2<-FIN_USA_PP2 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(FIN_USA_PP2_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

FIN_USA_PP2 <-FIN_USA_PP2  %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

FIN_USA_PP2<-FIN_USA_PP2 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(FIN_USA_PP2_vsi)
rm(v2)

# PP3
v3<-FIN_USA_PP3_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

FIN_USA_PP3<-FIN_USA_PP3 %>%
  left_join(v3,by=c("frame_id"))

FIN_USA_PP3<-FIN_USA_PP3 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(FIN_USA_PP3_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

FIN_USA_PP3 <-FIN_USA_PP3  %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

FIN_USA_PP3<-FIN_USA_PP3 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(FIN_USA_PP3_vsi)
rm(v3)

# PP4 
v4<-FIN_USA_PP4_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

FIN_USA_PP4<-FIN_USA_PP4 %>%
  left_join(v4,by=c("frame_id"))

FIN_USA_PP4<-FIN_USA_PP4 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(FIN_USA_PP4_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

FIN_USA_PP4 <-FIN_USA_PP4  %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

FIN_USA_PP4<-FIN_USA_PP4 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(FIN_USA_PP4_vsi)
rm(v4)

# PP5
v5<-FIN_USA_PP5_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

FIN_USA_PP5<-FIN_USA_PP5 %>%
  left_join(v5,by=c("frame_id"))

FIN_USA_PP5<-FIN_USA_PP5 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(FIN_USA_PP5_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

FIN_USA_PP5 <-FIN_USA_PP5 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

FIN_USA_PP5<-FIN_USA_PP5 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(FIN_USA_PP5_vsi)
rm(v5)

#### join to rosters ###
FIN_USA_FINAL<-rbind(FIN_USA_PP1,FIN_USA_PP2,FIN_USA_PP3,FIN_USA_PP4,FIN_USA_PP5)


FIN_USA_FINAL<-FIN_USA_FINAL %>%
  mutate(team = ifelse(FIN_USA_FINAL$team_name == "USA","away","home")) %>%
  rename(jn = jersey_number) %>%
  left_join(FIN_USA_roster,by=c("jn","team"))

write.csv(FIN_USA_FINAL,"DataTransform/TrackingMerge/FIN_USA_FINAL.csv")

rm(FIN_USA_PP1)
rm(FIN_USA_PP2)
rm(FIN_USA_PP3)
rm(FIN_USA_PP4)
rm(FIN_USA_roster)

####### GAME 5 SWISS & Canada #####
# GM5_PP1
SWISS_CAN_PP1<-read_csv("Src/TrackingData/2022-02-14 Switzerland at Canada/2022-02-14 Switzerland at Canada P1 PP1.csv")
SWISS_CAN_PP1$game_name<-"2022-02-14 Switzerland at Canada"
SWISS_CAN_PP1$penalty<-1
# GM4_PP2
SWISS_CAN_PP2<-read_csv("Src/TrackingData/2022-02-14 Switzerland at Canada/2022-02-14 Switzerland at Canada P1 PP2.csv")
SWISS_CAN_PP2$game_name<-"2022-02-14 Switzerland at Canada"
SWISS_CAN_PP2$penalty<-2
# GM4_PP3
SWISS_CAN_PP3<-read_csv("Src/TrackingData/2022-02-14 Switzerland at Canada/2022-02-14 Switzerland at Canada P2 PP4.csv")
SWISS_CAN_PP3$game_name<-"2022-02-14 Switzerland at Canada"
SWISS_CAN_PP3$penalty<-4
# GM4_PP4
SWISS_CAN_PP4<-read_csv("Src/TrackingData/2022-02-14 Switzerland at Canada/2022-02-14 Switzerland at Canada P3 PP5.csv")
SWISS_CAN_PP4$game_name<-"2022-02-14 Switzerland at Canada"
SWISS_CAN_PP4$penalty<-5

### video data 
# pp 1
SWISS_CAN_PP1_vsi<-read_csv("Src/TrackingData/2022-02-14 Switzerland at Canada/videoShotsInfo_2022-02-14 Switzerland at Canada P1 PP1.csv")
SWISS_CAN_PP1_vsi$game_name<-"2022-02-14 Switzerland at Canada"
SWISS_CAN_PP1_vsi$penalty<-1
# pp 2
SWISS_CAN_PP2_vsi<-read_csv("Src/TrackingData/2022-02-14 Switzerland at Canada/videoShotsInfo_2022-02-14 Switzerland at Canada P1 PP2.csv")
SWISS_CAN_PP2_vsi$game_name<-"2022-02-14 Switzerland at Canada"
SWISS_CAN_PP2_vsi$penalty<-2
# pp 3
SWISS_CAN_PP3_vsi<-read_csv("Src/TrackingData/2022-02-14 Switzerland at Canada/videoShotsInfo_2022-02-14 Switzerland at Canada P2 PP3.csv")
SWISS_CAN_PP3_vsi$game_name<-"2022-02-14 Switzerland at Canada"
SWISS_CAN_PP3_vsi$penalty<-3
# pp 4
SWISS_CAN_PP4_vsi<-read_csv("Src/TrackingData/2022-02-14 Switzerland at Canada/videoShotsInfo_2022-02-14 Switzerland at Canada P2 PP4.csv")
SWISS_CAN_PP4_vsi$game_name<-"2022-02-14 Switzerland at Canada"
SWISS_CAN_PP4_vsi$penalty<-4
# pp 5
SWISS_CAN_PP5_vsi<-read_csv("Src/TrackingData/2022-02-14 Switzerland at Canada/videoShotsInfo_2022-02-14 Switzerland at Canada P3 PP5.csv")
SWISS_CAN_PP5_vsi$game_name<-"2022-02-14 Switzerland at Canada"
SWISS_CAN_PP5_vsi$penalty<-5

## roster tracking
SWISS_CAN_roster<-read_csv("Src/TrackingData/2022-02-14 Switzerland at Canada/2022-02-14 Switzerland at Canada roster.csv")


##### TRANSFORM & JOIN VIDEO FILES ######
# PP1 # 
v1<-SWISS_CAN_PP1_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

SWISS_CAN_PP1<-SWISS_CAN_PP1 %>%
  left_join(v1,by=c("frame_id"))

SWISS_CAN_PP1<-SWISS_CAN_PP1 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(SWISS_CAN_PP1_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

SWISS_CAN_PP1 <-SWISS_CAN_PP1 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

SWISS_CAN_PP1<-SWISS_CAN_PP1 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(SWISS_CAN_PP1_vsi)
rm(v1)

# PP2 
v2<-SWISS_CAN_PP2_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

SWISS_CAN_PP2<-SWISS_CAN_PP2 %>%
  left_join(v2,by=c("frame_id"))

SWISS_CAN_PP2<-SWISS_CAN_PP2 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(SWISS_CAN_PP2_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

SWISS_CAN_PP2 <-SWISS_CAN_PP2 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

SWISS_CAN_PP2<-SWISS_CAN_PP2 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(SWISS_CAN_PP2_vsi)
rm(v2)

# PP3
#v3<-SWISS_CAN_PP3_vsi %>%
#  mutate(diff = frame_end_shot - frame_start_shot) %>%
#  group_by(shot_ind) %>%
#  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
#                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

v4<-SWISS_CAN_PP4_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

SWISS_CAN_PP3<-SWISS_CAN_PP3 %>%
  left_join(v4,by=c("frame_id"))

SWISS_CAN_PP3<-SWISS_CAN_PP3 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(SWISS_CAN_PP4_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

SWISS_CAN_PP3 <-SWISS_CAN_PP3 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

SWISS_CAN_PP3<-SWISS_CAN_PP3 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(SWISS_CAN_PP3_vsi)
#rm(v3)
rm(v4)

# PP4 
v5<-SWISS_CAN_PP5_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

SWISS_CAN_PP4<-SWISS_CAN_PP4 %>%
  left_join(v5,by=c("frame_id"))

SWISS_CAN_PP4<-SWISS_CAN_PP4 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(SWISS_CAN_PP5_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

SWISS_CAN_PP4 <-SWISS_CAN_PP4 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

SWISS_CAN_PP4<-SWISS_CAN_PP4 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )

rm(SWISS_CAN_PP4_vsi)
rm(SWISS_CAN_PP5_vsi)
rm(v5)



#### join to rosters ###
SWISS_CAN_FINAL<-rbind(SWISS_CAN_PP1,SWISS_CAN_PP2,SWISS_CAN_PP3,SWISS_CAN_PP4)


SWISS_CAN_FINAL<-SWISS_CAN_FINAL %>%
  mutate(team = ifelse(SWISS_CAN_FINAL$team_name == "Switzerland","away","home")) %>%
  rename(jn = jersey_number) %>%
  left_join(SWISS_CAN_roster,by=c("jn","team"))

write.csv(SWISS_CAN_FINAL,"DataTransform/TrackingMerge/SWISS_CAN_FINAL.csv")

rm(SWISS_CAN_PP1)
rm(SWISS_CAN_PP2)
rm(SWISS_CAN_PP3)
rm(SWISS_CAN_PP4)
rm(SWISS_CAN_roster)

####### GAME 6 Switzerland at Finland ########
# GM6_PP1
SWISS_FIN_PP1<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/2022-02-16 Switzerland at Finland P1 PP1.csv")
SWISS_FIN_PP1$game_name<-"2022-02-16 Switzerland at Finland"
SWISS_FIN_PP1$penalty<-1
# GM6_PP2
SWISS_FIN_PP2<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/2022-02-16 Switzerland at Finland P1 PP2.csv")
SWISS_FIN_PP2$game_name<-"2022-02-16 Switzerland at Finland"
SWISS_FIN_PP2$penalty<-2
# GM6_PP3
SWISS_FIN_PP3<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/2022-02-16 Switzerland at Finland P2 PP3.csv")
SWISS_FIN_PP3$game_name<-"2022-02-16 Switzerland at Finland"
SWISS_FIN_PP3$penalty<-3
# GM6_PP4
SWISS_FIN_PP4<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/2022-02-16 Switzerland at Finland P2 PP4.csv")
SWISS_FIN_PP4$game_name<-"2022-02-16 Switzerland at Finland"
SWISS_FIN_PP4$penalty<-4
# GM6_PP5
SWISS_FIN_PP5<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/2022-02-16 Switzerland at Finland P2 PP5.csv")
SWISS_FIN_PP5$game_name<-"2022-02-16 Switzerland at Finland"
SWISS_FIN_PP5$penalty<-5
# GM6_PP6
SWISS_FIN_PP6<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/2022-02-16 Switzerland at Finland P3 PP6.csv")
SWISS_FIN_PP6$game_name<-"2022-02-16 Switzerland at Finland"
SWISS_FIN_PP6$penalty<-6
# GM6_PP7
SWISS_FIN_PP7<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/2022-02-16 Switzerland at Finland P3 PP7.csv")
SWISS_FIN_PP7$game_name<-"2022-02-16 Switzerland at Finland"
SWISS_FIN_PP7$penalty<-7
# GM6_PP8
SWISS_FIN_PP8<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/2022-02-16 Switzerland at Finland P3 PP8.csv")
SWISS_FIN_PP8$game_name<-"2022-02-16 Switzerland at Finland"
SWISS_FIN_PP8$penalty<-8

### video data 
# pp1
SWISS_FIN_PP1_vsi<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/videoShotsInfo_2022-02-16 Switzerland at Finland P1 PP1.csv")
SWISS_FIN_PP1_vsi$game_name<-"2022-02-16 Switzerland at Finland"
SWISS_FIN_PP1_vsi$penalty<-1
# pp2
SWISS_FIN_PP2_vsi<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/videoShotsInfo_2022-02-16 Switzerland at Finland P1 PP2.csv")
SWISS_FIN_PP2_vsi$game_name<-"2022-02-16 Switzerland at Finland"
SWISS_FIN_PP2_vsi$penalty<-2
# pp3
SWISS_FIN_PP3_vsi<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/videoShotsInfo_2022-02-16 Switzerland at Finland P2 PP3.csv")
SWISS_FIN_PP3_vsi$game_name<-"2022-02-16 Switzerland at Finland"
SWISS_FIN_PP3_vsi$penalty<-3
# pp4
SWISS_FIN_PP4_vsi<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/videoShotsInfo_2022-02-16 Switzerland at Finland P2 PP4.csv")
SWISS_FIN_PP4_vsi$game_name<-"2022-02-16 Switzerland at Finland"
SWISS_FIN_PP4_vsi$penalty<-4
# pp5
SWISS_FIN_PP5_vsi<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/videoShotsInfo_2022-02-16 Switzerland at Finland P2 PP5.csv")
SWISS_FIN_PP5_vsi$game_name<-"2022-02-16 Switzerland at Finland"
SWISS_FIN_PP5_vsi$penalty<-5
# pp6
SWISS_FIN_PP6_vsi<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/videoShotsInfo_2022-02-16 Switzerland at Finland P3 PP6.csv")
SWISS_FIN_PP6_vsi$game_name<-"2022-02-16 Switzerland at Finland"
SWISS_FIN_PP6_vsi$penalty<-6
# pp7
SWISS_FIN_PP7_vsi<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/videoShotsInfo_2022-02-16 Switzerland at Finland P3 PP7.csv")
SWISS_FIN_PP7_vsi$game_name<-"2022-02-16 Switzerland at Finland"
SWISS_FIN_PP7_vsi$penalty<-7
# pp8
SWISS_FIN_PP8_vsi<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/videoShotsInfo_2022-02-16 Switzerland at Finland P3 PP8.csv")
SWISS_FIN_PP8_vsi$game_name<-"2022-02-16 Switzerland at Finland"
SWISS_FIN_PP8_vsi$penalty<-8


## roster tracking
SWISS_FIN_roster<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/2022-02-16 Switzerland at Finland roster.csv")


##### TRANSFORM & JOIN VIDEO FILES ######
# PP1 # 
v1<-SWISS_FIN_PP1_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

SWISS_FIN_PP1<-SWISS_FIN_PP1 %>%
  left_join(v1,by=c("frame_id"))

SWISS_FIN_PP1<-SWISS_FIN_PP1 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(SWISS_FIN_PP1_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

SWISS_FIN_PP1 <-SWISS_FIN_PP1 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

SWISS_FIN_PP1<-SWISS_FIN_PP1 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )


rm(SWISS_FIN_PP1_vsi)
rm(v1)

# PP2 
v2<-SWISS_FIN_PP2_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

SWISS_FIN_PP2<-SWISS_FIN_PP2 %>%
  left_join(v2,by=c("frame_id"))

SWISS_FIN_PP2<-SWISS_FIN_PP2 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(SWISS_FIN_PP2_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

SWISS_FIN_PP2 <-SWISS_FIN_PP2 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

SWISS_FIN_PP2<-SWISS_FIN_PP2 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )


rm(SWISS_FIN_PP2_vsi)
rm(v2)

# PP3
v3<-SWISS_FIN_PP3_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

SWISS_FIN_PP3<-SWISS_FIN_PP3 %>%
  left_join(v3,by=c("frame_id"))

SWISS_FIN_PP3<-SWISS_FIN_PP3 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(SWISS_FIN_PP3_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

SWISS_FIN_PP3 <-SWISS_FIN_PP3 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

SWISS_FIN_PP3<-SWISS_FIN_PP3 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )


rm(SWISS_FIN_PP3_vsi)
rm(v3)

# PP4 
v4<-SWISS_FIN_PP4_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

SWISS_FIN_PP4<-SWISS_FIN_PP4 %>%
  left_join(v4,by=c("frame_id"))

SWISS_FIN_PP4<-SWISS_FIN_PP4 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(SWISS_FIN_PP4_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

SWISS_FIN_PP4 <-SWISS_FIN_PP4 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

SWISS_FIN_PP4<-SWISS_FIN_PP4 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )


rm(SWISS_FIN_PP4_vsi)
rm(v4)

# PP5
v5<-SWISS_FIN_PP5_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

SWISS_FIN_PP5<-SWISS_FIN_PP5 %>%
  left_join(v5,by=c("frame_id"))

SWISS_FIN_PP5<-SWISS_FIN_PP5 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(SWISS_FIN_PP5_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

SWISS_FIN_PP5 <-SWISS_FIN_PP5 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

SWISS_FIN_PP5<-SWISS_FIN_PP5 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )


rm(SWISS_FIN_PP5_vsi)
rm(v5)

# PP6
v6<-SWISS_FIN_PP6_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

SWISS_FIN_PP6<-SWISS_FIN_PP6 %>%
  left_join(v6,by=c("frame_id"))

SWISS_FIN_PP6<-SWISS_FIN_PP6 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(SWISS_FIN_PP6_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

SWISS_FIN_PP6 <-SWISS_FIN_PP6 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

SWISS_FIN_PP6<-SWISS_FIN_PP6 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )


rm(SWISS_FIN_PP6_vsi)
rm(v6)

# PP7
v7<-SWISS_FIN_PP7_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

SWISS_FIN_PP7<-SWISS_FIN_PP7 %>%
  left_join(v7,by=c("frame_id"))

SWISS_FIN_PP7<-SWISS_FIN_PP7 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(SWISS_FIN_PP7_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

SWISS_FIN_PP7 <-SWISS_FIN_PP7 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

SWISS_FIN_PP7<-SWISS_FIN_PP7 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )


rm(SWISS_FIN_PP7_vsi)
rm(v7)

# PP8
v8<-SWISS_FIN_PP8_vsi %>%
  mutate(diff = frame_end_shot - frame_start_shot) %>%
  group_by(shot_ind) %>%
  do(data.frame(shot_ind=.$shot_ind, frame_id=seq(.$frame_start_shot,.$frame_end_shot,by=1),
                time = seq(.$`time_start_shot(sec)`,.$`time_end_shot(sec)`,length.out=.$diff+1)))

SWISS_FIN_PP8<-SWISS_FIN_PP8 %>%
  left_join(v8,by=c("frame_id"))

SWISS_FIN_PP8<-SWISS_FIN_PP8 %>%
  left_join(pp_info,by=c("game_name" = "game_name","penalty" = "penalty_number")) %>%
  mutate(clock_remaining_frame = total_game_seconds_remaining_start - time,
         clock_remaining_frame_round = round(total_game_seconds_remaining_start - time,0)) %>%
  left_join(SWISS_FIN_PP8_vsi,by = c("game_name","penalty","shot_ind")) %>%
  mutate(clock_remaining_frame_round_pxp_join = round(clock_remaining_frame + `time_start_shot(sec)`,0))

SWISS_FIN_PP8 <-SWISS_FIN_PP8 %>%
  left_join(pxp_womens_pp,by=c("game_name"="game_name",
                               "penalty" = "penalty_number",
                               "clock_remaining_frame_round_pxp_join" = "total_clock_seconds")) 

SWISS_FIN_PP8<-SWISS_FIN_PP8 %>%
  dplyr::select(-start_video_clock_seconds.y,-end_video_clock_seconds.y,-start_period.y,-end_period.y,
                -start_game_clock_seconds.y,-end_game_clock_seconds.y,-total_game_seconds_remaining_start.y,
                -total_game_seconds_remaining_end.y,-period.y,-team_name.y) %>%
  dplyr::rename(period = period.x,
                team_name = team_name.x,
                start_video_clock_seconds = start_video_clock_seconds.x,
                end_video_clock_seconds = end_video_clock_seconds.x,
                start_period = start_period.x,
                end_period = end_period.x,
                start_game_clock_seconds = start_game_clock_seconds.x,
                end_game_clock_seconds = end_game_clock_seconds.x,
                total_game_seconds_remaining_start = total_game_seconds_remaining_start.x,
                total_game_seconds_remaining_end = total_game_seconds_remaining_end.x
  )


rm(SWISS_FIN_PP8_vsi)
rm(v8)

#### join to rosters ###
SWISS_FIN_FINAL<-rbind(SWISS_FIN_PP1,SWISS_FIN_PP2,SWISS_FIN_PP3,SWISS_FIN_PP4,SWISS_FIN_PP5,SWISS_FIN_PP6,SWISS_FIN_PP7,SWISS_FIN_PP8)


SWISS_FIN_FINAL<-SWISS_FIN_FINAL %>%
  mutate(team = ifelse(SWISS_FIN_FINAL$team_name == "Switzerland","away","home")) %>%
  rename(jn = jersey_number) %>%
  left_join(SWISS_FIN_roster,by=c("jn","team"))

write.csv(SWISS_FIN_FINAL,"DataTransform/TrackingMerge/SWISS_FIN_FINAL.csv")

rm(SWISS_FIN_PP1)
rm(SWISS_FIN_PP2)
rm(SWISS_FIN_PP3)
rm(SWISS_FIN_PP4)
rm(SWISS_FIN_PP5)
rm(SWISS_FIN_PP6)
rm(SWISS_FIN_PP7)
rm(SWISS_FIN_PP8)
rm(SWISS_FIN_roster)


tracking<-rbind(CAN_USA_FINAL,ROC_FIN_FINAL,SWISS_ROC_FINAL,FIN_USA_FINAL,SWISS_CAN_FINAL,SWISS_FIN_FINAL)


write.csv(tracking,"tracking_final.csv")


tracking<-read.csv("tracking_final.csv") %>%
  dplyr::select(-X) %>%
  mutate(pp_index = paste(game_name,penalty,sep= "--"))



#### join tracking data averages to events? 
tr<-tracking %>%
  dplyr::select(pp_index,playId,frame_id,period,track_id,team_id,team_name,jn,x_ft,y_ft) %>%
  dplyr::mutate(event = if_else(complete.cases(playId),"event","non_event"))

### not all players will be in frame given computer vision data
min_frame<-tr %>%
  group_by(playId) %>%
  summarize(min_frame_id = min(frame_id))


  
tr<-tr %>%
  left_join(min_frame,by=c("playId")) %>%
  mutate(min_frame = ifelse(event == "event",min_frame_id,NA)) %>%
  #dplyr::select(-min_frame_id) %>%
  left_join(pxp_womens_pp,by=c("playId")) %>%
  dplyr::select(pp_index,game_name,playId,frame_id,period.x,track_id,team_id,team_name.x,jn,x_ft,y_ft,event.x,event.y,
                event_successful,x_coord,y_coord,event_type,x_coord_2,y_coord_2,event_detail_1,
                event_detail_2,event_detail_3,team_abbr,start_zone,end_zone,min_frame) %>%
  rename(period = period.x,
         team_name = team_name.x,
         event_bool = event.x,
         event = event.y
         ) %>%
  ### if min frame equals frame indicate start of event 
  mutate(
        event = ifelse(frame_id == min_frame & complete.cases(min_frame),event,NA),
         event_successful = ifelse(frame_id == min_frame & complete.cases(min_frame),event_successful,NA),
         x_coord = ifelse(frame_id == min_frame & complete.cases(min_frame),x_coord,NA),
         y_coord = ifelse(frame_id == min_frame & complete.cases(min_frame),y_coord,NA),
         event_type = ifelse(frame_id == min_frame & complete.cases(min_frame),event_type,NA),
         x_coord_2 = ifelse(frame_id == min_frame & complete.cases(min_frame),x_coord_2,NA),
         y_coord_2 = ifelse(frame_id == min_frame & complete.cases(min_frame),y_coord_2,NA),
         event_detail_1 = ifelse(frame_id == min_frame & complete.cases(min_frame),event_detail_1,NA),
         event_detail_2 = ifelse(frame_id == min_frame & complete.cases(min_frame),event_detail_2,NA),
         event_detail_3 = ifelse(frame_id == min_frame & complete.cases(min_frame),event_detail_3,NA)) %>%
  mutate(event_bool = ifelse(event_bool == "event" & complete.cases(event),"start_event",event_bool)) %>%
  fill(playId) %>%
  ungroup() %>%
  arrange(playId,frame_id,period) %>%
  fill(start_zone,end_zone,event,x_coord,y_coord,x_coord_2,y_coord_2,event_successful,team_abbr,game_name,pp_index) %>%
  ungroup() %>%
  #arrange(playId,period,frame_id,track_id,jn)
  group_by(playId,event_bool,track_id) %>%
  mutate(frame_of_seq = row_number(),
         event_team = ifelse(team_abbr == team_name & complete.cases(team_abbr),"event_team",
                             ifelse(team_abbr != team_name & complete.cases(team_abbr),"opp_team",NA))) %>%
  ungroup() %>%
  group_by(track_id,jn,playId,frame_id) %>%
  arrange(track_id,jn,playId,frame_id) %>%
  fill(event_team,.direction = "down") %>%
  ungroup() %>%
  mutate(x_shift = x_ft - 7, ### @the bucket_less suggestion
  y_shift = y_ft,
  x_ft_new = ifelse(start_zone == "Defensive Zone",200 - x_shift,x_shift),
  y_ft_new = ifelse(start_zone == "Defensive Zone",98 - y_shift,y_shift),
  x_coord_new = x_coord,
  y_coord_new = y_coord * (98/85),
  x_coord_2_new = x_coord_2,
  y_coord_2_new = y_coord_2 * (98/85)) %>% 
  filter(x_shift >= 0,    # filtering out data irregularities
         x_ft_new >= 0) %>% ## filtering out data shift irregularities
  arrange(playId,frame_id,track_id,jn)
  

## nas remove
(nrow(tracking) - nrow(tr)) / nrow(tracking)
### create starting position for specific event in period? 


### get starting position of faceoffs
FaceOffs<-tr %>%
  dplyr::filter(event == "Faceoff Win",
                complete.cases(event_type)) %>%
  group_by(playId,frame_id,jn) %>%
  arrange(track_id) %>%
  mutate(n = row_number()) %>%
  dplyr::filter(n == 1)  %>%
  dplyr::select(playId,frame_id,jn,x_ft_new,y_ft_new) %>%
  rename(x_ft_face_off = x_ft_new,
         y_ft_face_off = y_ft_new)

### get starting position of play
play<-tr %>%
  dplyr::filter(event == "Play",
                complete.cases(event_type)) %>%
  group_by(playId,frame_id,jn) %>%
  arrange(track_id) %>%
  mutate(n = row_number()) %>%
  dplyr::filter(n == 1)  %>%
  dplyr::select(playId,frame_id,jn,x_ft_new,y_ft_new)  %>%
  rename(x_ft_play= x_ft_new,
         y_ft_play = y_ft_new)

### get starting position of Zone Entry
ZE<-tr %>%
  dplyr::filter(event == "Zone Entry",
                complete.cases(event_type)) %>%
  group_by(playId,frame_id,jn) %>%
  arrange(track_id) %>%
  mutate(n = row_number()) %>%
  dplyr::filter(n == 1)  %>%
  dplyr::select(playId,frame_id,jn,x_ft_new,y_ft_new)  %>%
  rename(x_ft_ze= x_ft_new,
         y_ft_ze = y_ft_new)


#### add stationary positions #####
tr<-tr %>%
  left_join(FaceOffs,by=c("playId","jn")) %>%
  left_join(play,by=c("playId","jn")) %>%
  left_join(ZE,by=c("playId","jn")) %>%
  dplyr::select(-frame_id.y,-frame_id.x.x,-frame_id.y.y) %>%
  dplyr::rename(frame_id = frame_id.x)


##### fill down & fill up
tr<-tr %>%
  group_by(playId,frame_id,jn) %>%
  fill(x_ft_face_off,.direction = "down") %>%
  fill(y_ft_face_off,.direction = "down") %>%
  ungroup() %>%
  group_by(playId,frame_id,jn) %>%
  fill(x_ft_play,y_ft_play,.direction = "updown") %>%
  ungroup() %>%
  group_by(playId,frame_id,jn) %>%
  fill(x_ft_ze,y_ft_ze,.direction = "updown") %>%
  ungroup()

tr<-tr %>%
  mutate(faceOff = ifelse(frame_id == frame_id[event == "Faceoff Win"][1],1,0),
         Play = ifelse(frame_id == frame_id[event == "Play"][1],1,0),
         Shot = ifelse(frame_id == frame_id[event == "Shot"][1],1,0),
         zone_entry = ifelse(frame_id == frame_id[event == "Zone Entry"][1],1,0))

  
  
write.csv(tr,"DataTransform/tracking_cleaned.csv")






