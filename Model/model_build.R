library(tidyverse)
library(merTools)
library(rstanarm)
library(lme4)
library(caret)
library(qdapRegex)
library(gt)
library(performance)
library(bayesplot)


# model building

# combine data
#CAN_USA
cu_p1<-read_csv("DataTransform/OffTracking/CAN_USA_pp1_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
cu_p2<-read_csv("DataTransform/OffTracking/CAN_USA_pp2_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
cu_p3<-read_csv("DataTransform/OffTracking/CAN_USA_pp3_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
cu_p5<-read_csv("DataTransform/OffTracking/CAN_USA_pp5_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
cu_p6<-read_csv("DataTransform/OffTracking/CAN_USA_pp6_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
cu_p7<-read_csv("DataTransform/OffTracking/CAN_USA_pp7_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
#ROC_FIN
rf_p15<-read_csv("DataTransform/OffTracking/ROC_FIN_pp1_5_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
rf_p6<-read_csv("DataTransform/OffTracking/ROC_FIN_pp6_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
#SWISS_ROC
sr1<-read_csv("DataTransform/OffTracking/SWISS_ROC_pp1_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
sr2<-read_csv("DataTransform/OffTracking/SWISS_ROC_pp2_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
sr3<-read_csv("DataTransform/OffTracking/SWISS_ROC_pp3_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
sr5<-read_csv("DataTransform/OffTracking/SWISS_ROC_pp5_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
#FIN_USA
fu1<-read_csv("DataTransform/OffTracking/USA_FIN_pp1_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
fu3<-read_csv("DataTransform/OffTracking/USA_FIN_pp3_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
fu4<-read_csv("DataTransform/OffTracking/USA_FIN_pp4_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
fu56<-read_csv("DataTransform/OffTracking/USA_FIN_pp5_6_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
#SWISS_CAN
sc1<-read_csv("DataTransform/OffTracking/SWISS_CAN_pp1_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
sc2<-read_csv("DataTransform/OffTracking/SWISS_CAN_pp2_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
sc4<-read_csv("DataTransform/OffTracking/SWISS_CAN_pp4_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
sc5<-read_csv("DataTransform/OffTracking/SWISS_CAN_pp5_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
#SWISS_FIN
sf1<-read_csv("DataTransform/OffTracking/SWISS_FIN_pp1_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
sf2<-read_csv("DataTransform/OffTracking/SWISS_FIN_pp2_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
sf3<-read_csv("DataTransform/OffTracking/SWISS_FIN_pp3_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
sf4<-read_csv("DataTransform/OffTracking/SWISS_FIN_pp4_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
sf5<-read_csv("DataTransform/OffTracking/SWISS_FIN_pp5_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
sf6<-read_csv("DataTransform/OffTracking/SWISS_FIN_pp6_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
sf7<-read_csv("DataTransform/OffTracking/SWISS_FIN_pp7_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)
sf8<-read_csv("DataTransform/OffTracking/SWISS_FIN_pp8_off_tracking_calcs.csv") %>% dplyr::select(-`...1`)




train_df<-rbind(cu_p1,cu_p2,cu_p3,cu_p5,cu_p6,cu_p7,rf_p15,rf_p6,sr1,sr2,sr3,sr5,fu1,fu3,fu4,fu56,
          sc1,sc2,sc4,sc5,sf1,sf2,sf3,sf4,sf5,sf6,sf7,sf8)

#test_df<-rbind(sf1,sf2,sf3)

#### remove data ####
rm(cu_p1)
rm(cu_p2)
rm(cu_p3)
rm(cu_p5)
rm(cu_p6)
rm(cu_p7)
rm(fu1)
rm(fu3)
rm(fu4)
rm(fu56)
rm(rf_p15)
rm(rf_p6)
rm(sc1)
rm(sc2)
rm(sc4)
rm(sc5)
rm(sr1)
rm(sr2)
rm(sr3)
rm(sr5)
rm(sf1)
rm(sf2)
rm(sf3)
rm(sf4)
rm(sf5)
rm(sf6)
rm(sf7)
rm(sf8)

pxp_womens<-read_csv("Src/pxp_womens_oly_2022_v2.csv")

## 
glimpse(pxp_womens)

#### tracking data pp level info
pp_info<-read_csv("Src/TrackingData/pp_info.csv")

pp_info<-pp_info %>%
  dplyr::mutate(total_game_seconds_remaining_start = 
           if_else(start_period == 1,start_game_clock_seconds + 2400,
                   if_else(start_period == 2,start_game_clock_seconds + 1200,
                           start_game_clock_seconds)),
         total_game_seconds_remaining_end = 
           if_else(end_period == 1,end_game_clock_seconds + 2400,
                   if_else(end_period == 2,end_game_clock_seconds + 1200,
                           end_game_clock_seconds)))



### pxp womens
pxp_womens<-pxp_womens %>%
  dplyr::mutate(total_clock_seconds = 
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
  dplyr::left_join(pp_info,by=c("game_name"="game_name")) %>%
  dplyr::filter(total_clock_seconds <= total_game_seconds_remaining_start,
         total_clock_seconds >= total_game_seconds_remaining_end)

# final power play event data
pxp_womens_pp<-pxp_womens_pp %>%
  dplyr::filter(situation_type %in% c("4 on 5","5 on 4","6 on 5","5 on 6",
                                      "4 on 6","6 on 4","4 on 4"))

pxp_womens_pp<-pxp_womens_pp %>%
  dplyr::mutate(playId = row_number(),
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

pxp_womens_pp_sel<-pxp_womens_pp %>%
  dplyr::select(game_name,playId,total_game_seconds_remaining_start,total_game_seconds_remaining_end,penalty_number,
                situation_type,event,event_successful,event_type,x_coord,y_coord,
                x_coord_2,y_coord_2,goals_for,goals_against,player_name,team_abbr,event_detail_1,event_detail_2,player_name_2) %>%
  dplyr::mutate(same_team_prev_play = ifelse(team_abbr == lag(team_abbr),1,0))

pxp_womens_pp_sel %>%
  dplyr::group_by(event,event_successful) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::arrange(desc(n))


#### Rosters ###
CAN_USA_roster<-read_csv("Src/TrackingData/2022-02-08 Canada at USA/2022-02-08 Canada at USA roster.csv") %>%
  mutate(team_name = ifelse(team == "away","Canada","USA"))
ROC_FIN_roster<-read_csv("Src/TrackingData/2022-02-08 ROC at Finland/2022-02-08 ROC at Finland roster.csv") %>%
  mutate(team_name = ifelse(team == "away","ROC","Finland"))
SWISS_ROC_roster<-read_csv("Src/TrackingData/2022-02-12 Switzerland at ROC/2022-02-12 Switzerland at ROC roster.csv") %>%
  mutate(team_name = ifelse(team == "away","Switzerland","ROC"))
FIN_USA_roster<-read_csv("Src/TrackingData/2022-02-14 Finland at USA/2022-02-14 Finland at USA roster.csv") %>%
  mutate(team_name = ifelse(team == "away","USA","Finland"))
SWISS_CAN_roster<-read_csv("Src/TrackingData/2022-02-14 Switzerland at Canada/2022-02-14 Switzerland at Canada roster.csv") %>%
  mutate(team_name = ifelse(team == "away","Switzerland","Canada"))
SWISS_FIN_roster<-read_csv("Src/TrackingData/2022-02-16 Switzerland at Finland/2022-02-16 Switzerland at Finland roster.csv") %>%
  mutate(team_name = ifelse(team == "away","Switzerland","Finland"))
roster<-rbind(CAN_USA_roster,ROC_FIN_roster,SWISS_ROC_roster,FIN_USA_roster,SWISS_CAN_roster,SWISS_FIN_roster)

roster<-roster %>%
  dplyr::select(-team) %>%
  distinct()

rm(CAN_USA_roster)
rm(FIN_USA_roster)
rm(ROC_FIN_roster)
rm(SWISS_CAN_roster)
rm(SWISS_FIN_roster)
rm(SWISS_ROC_roster)

# separate model for puck recovery
# takeaway model? 
##### join on df #####
pass<-train_df %>%
  dplyr::left_join(pxp_womens_pp_sel,by=c("playId")) %>%
  dplyr::left_join(roster,by=c("jn"="jn","team_abbr"="team_name")) %>%
  dplyr::filter(event %in% c("Play","Takeaway","Dump In/Out","Zone Entry","Shot","Puck Recovery"))%>%
  mutate(event_successful = 
           case_when(
             event == "Play" & event_successful == TRUE ~ 0,
             event == "Play" & event_successful == FALSE ~ 1,
             event == "Takeaway" ~ 0,
             event == "Dump In/Out" & event_successful == TRUE ~ 0,
             event == "Dump In/Out" & event_successful == FALSE ~ 1,
             event == "Zone Entry" & event_successful == TRUE ~ 0,
             event == "Zone Entry" & event_successful == FALSE ~ 1,
             event == "Shot" & event_successful == TRUE ~ 0,
             event == "Shot" & event_successful == FALSE ~ 1,
             event == "Puck Recovery" & same_team_prev_play == 0 ~ 1,
             event == "Puck Recovery" & same_team_prev_play == 1 ~ 0,
             TRUE ~ 1),
      pass_distance = sqrt((x_coord - x_coord_2) ^ 2 + (y_coord - y_coord_2) ^ 2),
         opp_team = case_when(
           #game 1
           team_abbr == "Canada" & game_name == "2022-02-08 Canada at USA" ~ "USA",
           team_abbr == "USA" & game_name == "2022-02-08 Canada at USA" ~ "Canada",
           # game 2
           team_abbr == "ROC" & game_name == "2022-02-08 ROC at Finland" ~ "Finland",
           team_abbr == "Finland" & game_name == "2022-02-08 ROC at Finland" ~ "ROC",
           #game 3
           team_abbr == "Switzerland" & game_name == "2022-02-12 Switzerland at ROC" ~ "ROC",
           team_abbr == "ROC" & game_name == "2022-02-12 Switzerland at ROC" ~ "Switzerland",
           #game 4
           team_abbr == "Switzerland" & game_name == "2022-02-14 Switzerland at Canada" ~ "Canada",
           team_abbr == "Canada" & game_name == "2022-02-14 Switzerland at Canada" ~ "Switzerland",  
           #game 5
           team_abbr == "USA" & game_name == "2022-02-14 USA at Finland" ~ "Finland",
           team_abbr == "Finland" & game_name == "2022-02-14 USA at Finland" ~ "USA",    
           #game 6
           team_abbr == "Switzerland" & game_name == "2022-02-16 Switzerland at Finland" ~ "Finland",
           team_abbr == "Finland" & game_name == "2022-02-16 Switzerland at Finland" ~ "Switzerland",
           TRUE ~ as.character(NA)
         )
         ) %>%
  dplyr::left_join(roster,by=c("closest_opp_player_id"="jn","opp_team"="team_name")) %>%
  dplyr::rename("opp_player_name" = player.y,
                "opp_position" = position.y,
                "event_player_name" = player.x,
                "event_position_name" = position.x
                ) %>%
  dplyr::left_join(roster,by=c("player_name"="player","team_abbr"="team_name")) %>%
  dplyr::rename("event_jn" = jn.y,
                "event_position" = position,
                "jn" = jn.x) %>%
  dplyr::left_join(roster,by=c("player_name"="player","opp_team"="team_name")) %>%
  dplyr::rename("jn" = jn.x,
                "targeted_defender_jn" = jn.y) %>%
  arrange(pp_index,jn,playId) %>%
  fill(var_x,var_y,event_start_separation,event_separation,non_event_separation) %>%
  dplyr::filter(jn != 100) %>%
  dplyr::select(-var_j100_x,-var_j100_y) %>%
  mutate(closest_event_player_separation = ifelse(is.na(closest_event_player_separation), cummean(closest_event_player_separation), closest_event_player_separation)) %>%
  tidyr::fill(closest_event_player_separation, .direction = "down") %>%
  mutate(second_closest_event_player_separation = ifelse(is.na(second_closest_event_player_separation), cummean(second_closest_event_player_separation), second_closest_event_player_separation)) %>%
  tidyr::fill(second_closest_event_player_separation, .direction = "down") %>%
  mutate(third_closest_event_player_separation = ifelse(is.na(third_closest_event_player_separation), cummean(third_closest_event_player_separation), third_closest_event_player_separation)) %>%
  tidyr::fill(third_closest_event_player_separation, .direction = "down") %>%
  mutate(closest_opp_player_separation = ifelse(is.na(closest_opp_player_separation), cummean(closest_opp_player_separation), closest_opp_player_separation)) %>%
  tidyr::fill(closest_opp_player_separation,.direction = "down") %>%
  mutate(second_closest_opp_player_separation = ifelse(is.na(second_closest_opp_player_separation), cummean(second_closest_opp_player_separation), second_closest_opp_player_separation)) %>%
  tidyr::fill(second_closest_opp_player_separation,.direction = "down") %>%
  mutate(third_closest_opp_player_separation = ifelse(is.na(third_closest_opp_player_separation), cummean(third_closest_opp_player_separation), third_closest_opp_player_separation)) %>%
  tidyr::fill(third_closest_opp_player_separation,.direction = "down") %>%
  mutate(closest_100_event_separation = ifelse(is.na(closest_100_event_separation), cummean(closest_100_event_separation), closest_100_event_separation)) %>%
  tidyr::fill(closest_100_event_separation,.direction = "down") %>%
  mutate(closest_100_opp_separation = ifelse(is.na(closest_100_opp_separation), cummean(closest_100_opp_separation), closest_100_opp_separation)) %>%
  tidyr::fill(closest_100_opp_separation,.direction = "down") %>%
  mutate(closest_event_start_separation = ifelse(is.na(closest_event_start_separation), cummean(closest_event_start_separation), closest_event_start_separation)) %>%
  tidyr::fill(closest_event_start_separation,.direction = "updown") %>%
  mutate(closest_opp_start_separation = ifelse(is.na(closest_opp_start_separation), cummean(closest_opp_start_separation), closest_opp_start_separation)) %>%
  tidyr::fill(closest_opp_start_separation,.direction = "downup") %>%
  mutate(closest_event_action_separation = ifelse(is.na(closest_event_action_separation), cummean(closest_event_action_separation), closest_event_action_separation)) %>%
  tidyr::fill(closest_event_action_separation,.direction = "downup") %>%
  mutate(closest_opp_action_separation = ifelse(is.na(closest_opp_action_separation), cummean(closest_opp_action_separation), closest_opp_action_separation)) %>%
  tidyr::fill(closest_opp_action_separation,.direction = "downup") %>%
  mutate(closest_non_event_separation = ifelse(is.na(closest_non_event_separation), cummean(closest_non_event_separation), closest_non_event_separation)) %>%
  tidyr::fill(closest_non_event_separation,.direction = "downup") %>%
  mutate(closest_non_opp_separation = ifelse(is.na(closest_non_opp_separation), cummean(closest_non_opp_separation), closest_non_opp_separation)) %>%
  tidyr::fill(closest_non_opp_separation,.direction = "downup") %>%
  mutate(goal_diff = goals_for - goals_against,
         time_elapsed = total_game_seconds_remaining_start - total_game_seconds_remaining_end) %>%
  arrange(playId) %>%
  group_by(playId) %>%
  mutate(dis_event_x_abs = abs(dis_from_event_x),
         dis_event_des_x_abs = abs(dis_from_event_des_x),
        dis_event_x_rank = rank(dis_event_x_abs),
         dis_event_des_x_rank = rank(dis_event_des_x_abs),
        defender_id = ifelse(event %in% c("Play","Dump In/Out","Shot"),
          paste(opp_team,closest_opp_player_id,sep="-"),
          ifelse(event %in% c("Takeaway","Puck Recovery"),
          paste(team_abbr,event_jn,sep="-"),
          ifelse(event %in% c("Zone Entry"),
          paste(opp_team,targeted_defender_jn,sep="-"),NA))),
        offense_id = 
          ifelse(event %in% c("Play","Dump In/Out","Shot"),
          paste(team_abbr,jn,sep="-"),
          ifelse(event %in% c("Takeaway","Puck Recovery"),
          paste(opp_team,closest_opp_player_id,sep="-"),
          ifelse(event %in% c("Zone Entry"),
          paste(team_abbr,event_jn,sep="-"),NA)))
  ) %>%
  ungroup()

a<-pass %>%
  dplyr::filter(closest_opp_player_id == "5",
                event == "Play",
                event_successful == 1)



summary(pass)
#write.csv(pass,"Summary/pass.csv")

### rescale & center variables
pass_preprocess <- pass %>%
  select_if(is_numeric) %>%  
  dplyr::select(-playId,-jn,-closest_event_player_id,-second_closest_event_player_id,-third_closest_event_player_id,
        -closest_opp_player_id,-second_closest_opp_player_id,-third_closest_opp_player_id,-total_game_seconds_remaining_start,
        -total_game_seconds_remaining_end,-penalty_number,-x_coord,-y_coord,-x_coord_2,-y_coord_2,-goals_for,-goals_against,
        -pass_distance,-goal_diff,-same_team_prev_play,-event_jn,-targeted_defender_jn,-time_elapsed,-event_successful) %>%
  preProcess(method = c("center", "scale")) 

pass_scale <- predict(pass_preprocess, newdata = pass)

#pass_scale_final<-na.omit(pass_scale)



##### EDA #####
summary(pass_scale)

pass_glm<-glm(event_successful ~ 
  var_x + var_y + dis_from_event_x + dis_from_event_y + dis_from_event_des_x + dis_from_event_des_y + 
  separation + event_start_separation + event_separation + non_event_separation + net_separation + 
  closest_event_player_separation + second_closest_event_player_separation + third_closest_event_player_separation +
  closest_opp_player_separation + second_closest_opp_player_separation + third_closest_opp_player_separation + 
  closest_100_event_separation + closest_100_opp_separation + closest_event_start_separation + closest_opp_start_separation + 
  closest_event_action_separation + closest_opp_action_separation + closest_non_event_separation + closest_non_opp_separation + 
  situation_type + x_coord + y_coord + x_coord_2 + y_coord_2 + pass_distance + goal_diff + dis_event_x_abs + dis_event_des_x_abs
    ,data = pass_scale)


summary(pass_glm)

pass_glm1<-glm(event_successful ~ 
                 separation  + net_separation + closest_event_player_separation  + 
                 closest_opp_player_separation + closest_event_start_separation  +
                 closest_opp_start_separation + dis_event_x_abs + dis_event_des_x_abs
                ,data = pass_scale)

summary(pass_glm1)

#### model mixed effects


pass_mle<-lme4::glmer(factor(event_successful) ~ 
                        var_x + var_y + dis_from_event_x  + dis_from_event_des_x + dis_from_event_des_y +
                        separation + event_separation + non_event_separation + net_separation + closest_event_player_separation + second_closest_event_player_separation +
                        third_closest_event_player_separation + closest_opp_player_separation + second_closest_opp_player_separation +
                        third_closest_opp_player_separation + closest_event_start_separation + closest_opp_start_separation + pass_distance + goal_diff + dis_event_x_abs +
                        dis_event_des_x_abs + 
                        (1|offense_id) +
                        (1|defender_id),
                      data = pass_scale,
                      family = binomial(link = "logit"),
                      control = glmerControl(optimizer ="Nelder_Mead"))



summary(pass_mle)

coef(pass_mle)$defender_id


### final mixed effects used for simulation
pass_mle2<-lme4::glmer(factor(event_successful) ~ 
                         separation + net_separation  + closest_opp_player_separation + closest_event_player_separation +
                        dis_event_x_abs + dis_event_des_x_abs +
                         (1|defender_id),
                       data = pass_scale,
                       family = binomial(link = "probit"),
                       control = glmerControl(optimizer ="Nelder_Mead"))




summary(pass_mle2)

summary(pass_scale)

# low collinearity
check_collinearity(pass_mle2)


pass_model<-pass %>%
  dplyr::select(event_successful,separation,net_separation,closest_opp_player_separation,
                closest_event_player_separation,dis_event_x_abs,dis_event_des_x_abs,defender_id,event)





summary(pass_model)

pass_scale<-pass_scale %>%
  dplyr::filter(complete.cases(dis_event_des_x_abs))

### bayesian glmer using rstan
pass.success.mod.mcmc<-stan_glmer(event_successful ~   separation + net_separation  + 
                                    closest_opp_player_separation + closest_event_player_separation +
                                    dis_event_x_abs + dis_event_des_x_abs +
                                    (1|defender_id),
                                  data = pass_scale,
                                  chains = 5,
                                  family = binomial(link = 'probit'),
                                  prior_intercept = normal(0,10),
                                  prior = normal(0,1),
                                  prior_aux = exponential(1),
                                  prior_covariance = decov(1,1,1,1),
                                  adapt_delta = .8,
                                  iter = 10000,
                                  QR = FALSE)


pp_check(pass.success.mod.mcmc)
#pp_check(pass.success.mod.mcmc, plotfun = "scatter_avg") # y vs. average yrep

summary.vals<-as.data.frame(summary(pass.success.mod.mcmc))
summary.vals$parms<-row.names(summary.vals)

defender.vals<-dplyr::filter(
  summary.vals,stringr::str_detect(
    parms,'defender_id'
  ))

### closest defender metrics
defender.vals$defender_num<-qdapRegex::ex_between(defender.vals$parms, "-", "]")
defender.vals$defender_team<-qdapRegex::ex_between(defender.vals$parms, ":", "-")
defender.vals$defender_num<-as.numeric(defender.vals$defender_num)
defender.vals$defender_team<-as.character(defender.vals$defender_team)
defender.vals$defender_id<-paste(defender.vals$defender_team,defender.vals$defender_num,sep="-")


defender.vals<-defender.vals %>%
  left_join(roster,by=c("defender_num" = "jn","defender_team" = "team_name"))


defender.vals<-defender.vals %>%
  dplyr::select(-parms) %>%
  dplyr::arrange(mean)

### query data 
pass.success.mcmc.preds<-pass.success.mod.mcmc$data

SDs<-data.frame(pass.success.mod.mcmc$ses)
SDs$parms<-row.names(SDs)

defender.SD<-dplyr::filter(SDs,str_detect(
  parms,'defender_id'
))

defender.SD$defender_num<-qdapRegex::ex_between(defender.SD$parms, "-", "]")
defender.SD$defender_team<-qdapRegex::ex_between(defender.SD$parms, ":", "-")
defender.SD$defender_num<-as.numeric(defender.SD$defender_num)
defender.SD$defender_team<-as.character(defender.SD$defender_team)
defender.SD$defender_id<-paste(defender.SD$defender_team,defender.SD$defender_num,sep="-")

defender.coefs<-ranef(pass.success.mod.mcmc)[[1]]
defender.coefs$defender<-row.names(defender.coefs)

names(defender.coefs)[1]<-"defender.eff"

defender.coefs<-defender.coefs %>%
  left_join(defender.SD,by=c("defender" = "defender_id"))

#passer.coefs<-passer.coefs %>%
#  filter(complete.cases(passer_player_name))


defender.coefs<-defender.coefs %>%
  rename(defender.SD = pass.success.mod.mcmc.ses) %>%
  dplyr::filter(complete.cases(defender.SD)) %>%
  dplyr::select(-parms)


#  passer.SD[,1]

pass.success.mcmc.preds$fitted.values<-pass.success.mod.mcmc$fitted.values
pass.success.mcmc.preds$full.lin.preds<-pass.success.mod.mcmc$linear.predictors


###### Convert group-level intercepts to probability scale ####
pass.success.mcmc.preds<-pass.success.mcmc.preds %>%
  dplyr::left_join(defender.coefs,by=c("defender_id" = "defender"))

pass.success.mcmc.preds$wo_defender<-with(pass.success.mcmc.preds,
                                        pnorm(full.lin.preds - defender.eff)  
)

pass.success.mcmc.preds$wo_defender_plus1SD<-with(pass.success.mcmc.preds,
                                                pnorm(full.lin.preds - defender.eff + defender.SD))

##### summarize predictor values ####
defender.mcmc.final<-pass.success.mcmc.preds %>%
  dplyr::group_by(defender_id,defender_team,defender_num)%>%
  dplyr::summarise(
    mcmc.mean = mean(fitted.values - wo_defender),
    mcmc.sd = mean(wo_defender_plus1SD - wo_defender),
    chances = n()
  ) %>%
  arrange(mcmc.mean)

pass.success.mcmc.preds %>%
  mutate(PDAA = fitted.values - wo_defender) %>%
  ggplot(aes(x=PDAA,fill=defender_team)) +
  geom_density(alpha=0.3)

#top defenders
defender.mcmc.final %>%
  arrange(desc(mcmc.mean)) %>%
  filter(chances > 10,    ### chances is just amount of people guarding
         complete.cases(defender_num)) %>%
  head(10L)



#### simulate posteriors ####
RE.sims<-merTools::REsim(pass_mle2,n.sims=10000,seed=1234)

defender.sims<-dplyr::filter(RE.sims,groupFctr == "defender_id")

defender.sims<-defender.sims %>%
  rename(defender = groupID) %>%
  dplyr::select(defender,mean,median,sd)

##### extra coefficients ###
pass.frame.sim.preds<-pass.success.mcmc.preds
pass.frame.sim.preds<-pass.frame.sim.preds %>%
  dplyr::left_join(defender.sims,by=c("defender_id" = "defender"))


pass.frame.sim.preds$all_frame<-predict(pass_mle2,type="response")
pass.frame.sim.preds$all_defender_mean<-pnorm(predict(pass_mle2,
                                                    type='link',
                                                    re.form = ~
                                                      (1|defender_id)) +
                                              pass.frame.sim.preds$mean)

pass.frame.sim.preds$wo_defender_plus1SD<-pnorm(predict(
  pass_mle2,
  type='link',
  re.form = ~ 
    (1|defender_id)) +
    pass.frame.sim.preds$defender.SD)



##### Compile and compare simulation results to MCMC ######
defender.sim.final<-pass.frame.sim.preds %>%
  dplyr::group_by(defender_id,defender_team,defender_num) %>%
  dplyr::summarise(
    sim.mean = mean(all_frame - wo_defender),
    sim.sd = mean(wo_defender_plus1SD - wo_defender),
    chances = n()
  ) 




defender.sim.mcmc<-defender.sim.final %>%
  dplyr::inner_join(defender.mcmc.final,
                    by=c('defender_id','chances')) %>%
  dplyr::select(defender_id,defender_team.x,defender_num.x,chances,
                mcmc.mean,sim.mean,mcmc.sd,sim.sd) %>%
  dplyr::rename(
                defender_team = defender_team.x,
                defender_num = defender_num.x) %>%
  dplyr::arrange(sim.mean)


#### visualize output of model
pass_join<-pass %>%
  dplyr::filter(complete.cases(dis_event_des_x_abs)) %>%
  dplyr::select(playId,jn,separation,net_separation,closest_opp_player_separation,
                closest_event_player_separation,dis_event_x_abs,closest_opp_action_separation,
                dis_event_des_x_abs)
  

### bring pass metrics for context
pass_metrics<-pass.frame.sim.preds %>%
  left_join(pass_join,by=c("playId","jn")) %>%
  group_by(defender_id) %>%
  summarize(n = n_distinct(playId),
            dis_event_x_abs = mean(dis_event_x_abs.y,na.rm=T),
            dis_event_des_x_abs = mean(dis_event_des_x_abs.y,na.rm = T),
            pass_distance = mean(pass_distance,na.rm = T),
            closest_opp_action_separation = mean(closest_opp_action_separation.y,na.rm = T),
            closest_opp_player_separation = mean(closest_opp_player_separation.y,na.rm = T)
            )


defender.sim.mcmc<-defender.sim.mcmc %>%
  left_join(roster,by=c("defender_num"="jn","defender_team"="team_name")) %>%
  rename("closest_defensive_player" = player)
  
#### get player headshots
defender.sim.mcmc<-defender.sim.mcmc %>%
  mutate(headshot_url = case_when(
    closest_defensive_player == "Kendall Coyne Schofield" ~ "http://d2a3o6pzho379u.cloudfront.net/128730.jpg",
  closest_defensive_player == "Ronja Savolainen" ~ "http://d2a3o6pzho379u.cloudfront.net/137120.jpg",
  closest_defensive_player == "Viivi Vainikka" ~ "https://results.eurosport.com/images/athletes/l/1005564.png",
  closest_defensive_player == "Sofianna Sundelin" ~ "https://results.eurosport.com/images/athletes/l/1013086.png",
  closest_defensive_player == "Nina Pirogova" ~ "http://d2a3o6pzho379u.cloudfront.net/137848.jpg",
  closest_defensive_player == "Oxana Bratishcheva" ~ "https://en.fhr.ru/upload/resize_cache/uf/fab/200_200_2/ROC_BRATISHEVA_Oxana.jpg",
  closest_defensive_player == "Olga Sosina" ~ "http://d2a3o6pzho379u.cloudfront.net/119329.jpg",
  closest_defensive_player == "Shannon Sigrist" ~ "http://d2a3o6pzho379u.cloudfront.net/138074.jpg",
  closest_defensive_player == "Alexandra Vafina" ~ "https://results.eurosport.com/images/athletes/l/1056252.png",
  closest_defensive_player == "Hannah Brandt" ~ "http://d2a3o6pzho379u.cloudfront.net/138311.jpg",
  closest_defensive_player == "Kaleigh Quennec" ~ "https://results.eurosport.com/images/athletes/l/1049801.png",
  closest_defensive_player == "Emily Clark" ~ "http://d2a3o6pzho379u.cloudfront.net/136863.jpg",
  closest_defensive_player == "Minnamari Tuominen" ~ "http://d2a3o6pzho379u.cloudfront.net/119313.jpg",
  closest_defensive_player == "Sanni Rantala" ~ "https://results.eurosport.com/images/athletes/l/1035631.png",
  closest_defensive_player == "Noemi Ryhner" ~ "https://girlshockeyday.schellingf.com/neu/coaches/kloten18/noemi_ryhner.png",
  closest_defensive_player == "Amanda Kessel" ~ "http://d2a3o6pzho379u.cloudfront.net/128733.jpg",
  closest_defensive_player == "Hayley Scamurra" ~ "https://assets.ngin.com/attachments/profiles/e635-169971645/USA_SCAMURRA_HAYLEY.jpg",
  closest_defensive_player == "Jenni Hiirikoski" ~ "http://d2a3o6pzho379u.cloudfront.net/119235.jpg",
  closest_defensive_player == "Emma Maltais" ~ "https://olympic.ca/wp-content/uploads/2021/12/2021NWTHockey_Maltais-9208147-e1640793471188.jpg",
  closest_defensive_player == "Yekaterina Dobrodeyeva" ~ "https://results.eurosport.com/images/athletes/l/1056259.png",
  closest_defensive_player == "Ella Viitasuo" ~ "http://d2a3o6pzho379u.cloudfront.net/137123.jpg",
  closest_defensive_player == "Fanuza Kadirova" ~ "http://d2a3o6pzho379u.cloudfront.net/137841.jpg",
  closest_defensive_player == "Megan Keller" ~ "http://d2a3o6pzho379u.cloudfront.net/138315.jpg",
  closest_defensive_player == "Dani Cameranesi" ~ "http://d2a3o6pzho379u.cloudfront.net/138312.jpg",
  closest_defensive_player == "Angelina Goncharenko" ~ "http://d2a3o6pzho379u.cloudfront.net/128360.jpg",
  closest_defensive_player == "Ashton Bell" ~ "https://olympic.ca/wp-content/uploads/2021/12/2021NWTHockey_Bell-9208041-e1640793343183.jpg",
  closest_defensive_player == "Elisa Holopainen" ~ "https://results.eurosport.com/images/athletes/l/1005528.png",
  closest_defensive_player == "Sarah Nurse" ~ "http://d2a3o6pzho379u.cloudfront.net/136867.jpg",
  closest_defensive_player == "Michelle Karvinen" ~ "http://d2a3o6pzho379u.cloudfront.net/119307.jpg",
  closest_defensive_player == "Veronika Korzhakova" ~ "https://results.eurosport.com/images/athletes/l/1056262.png",
  closest_defensive_player == "Jocelyne Larocque" ~ "http://d2a3o6pzho379u.cloudfront.net/127493.jpg",
  closest_defensive_player == "Sini Karjalainen" ~ "https://results.eurosport.com/images/athletes/l/1005530.png",
  closest_defensive_player == "Valeria Pavlova" ~ "http://d2a3o6pzho379u.cloudfront.net/137847.jpg",
  closest_defensive_player == "Anna Savonina" ~ "https://en.fhr.ru/upload/resize_cache/uf/fa3/200_200_2/ROC_SAVONINA_Anna.jpg",
  closest_defensive_player == "Noora Tulus" ~ "http://d2a3o6pzho379u.cloudfront.net/137121.jpg",
  closest_defensive_player == "Maria Batalova" ~ "http://d2a3o6pzho379u.cloudfront.net/137836.jpg",
  closest_defensive_player == "Blayre Turnbull" ~ "http://d2a3o6pzho379u.cloudfront.net/136870.jpg",
  closest_defensive_player == "Natalie Spooner" ~ "http://d2a3o6pzho379u.cloudfront.net/127495.jpg",
  closest_defensive_player == "Sinja Leemann" ~ "https://results.eurosport.com/images/athletes/l/1048702.png",
  closest_defensive_player == "Liana Ganeyeva" ~ "http://d2a3o6pzho379u.cloudfront.net/137840.jpg",
  closest_defensive_player == "Sarah Forster" ~ "http://d2a3o6pzho379u.cloudfront.net/128505.jpg",
  closest_defensive_player == "Rahel Enzler" ~ "https://results.eurosport.com/images/athletes/l/1048345.png",
  closest_defensive_player == "Maria Pechnikova" ~ "https://results.eurosport.com/images/athletes/l/1056275.png",
  closest_defensive_player == "Alex Carpenter" ~ "http://d2a3o6pzho379u.cloudfront.net/128729.jpg",
  closest_defensive_player == "Marie-Philip Poulin" ~ "http://d2a3o6pzho379u.cloudfront.net/119286.jpg",
  closest_defensive_player == "Lena Marie Lutz" ~ "https://img.chmedia.ch/2021/9/8/7ebf38f9-734b-4a79-ba53-d53c6f64dd15.jpeg",
  closest_defensive_player == "Anna Shibanova" ~ "http://d2a3o6pzho379u.cloudfront.net/128363.jpg",
  closest_defensive_player == "Yelena Provorova" ~ "https://results.eurosport.com/images/athletes/l/1056278.png",
  closest_defensive_player == "Lee Stecklein" ~ "http://d2a3o6pzho379u.cloudfront.net/128737.jpg",
  closest_defensive_player == "Jamie Lee Rattray" ~ "https://olympic.ca/wp-content/uploads/2021/12/2021NWTHockey_Rattray-9208469-e1640793521442.jpg",
  closest_defensive_player == "Jenniina Nylund" ~ "https://scsuhuskies.com/images/2021/9/6/JenniinaNylund.jpg",
  closest_defensive_player == "Sarah Fillier" ~ "https://olympic.ca/wp-content/uploads/2021/12/2021NWTHockey_Fillier-9207815-e1640793426450.jpg",
  closest_defensive_player == "Hilary Knight" ~ "http://d2a3o6pzho379u.cloudfront.net/119215.jpg",
  closest_defensive_player == "Claire Thompson" ~ "https://goprincetontigers.com/images/2017/9/26/Thompson_DSC_9061.jpg",
  closest_defensive_player == "Ella Shelton" ~ "https://olympic.ca/wp-content/uploads/2021/12/2021NWTHockey_Shelton-9208081-e1640793536729.jpg",
  closest_defensive_player == "Susanna Tapani" ~ "http://d2a3o6pzho379u.cloudfront.net/127679.jpg",
  closest_defensive_player == "Meeri Raisanen" ~ "http://d2a3o6pzho379u.cloudfront.net/1006055.jpg",
  closest_defensive_player == "Nicole Bullo" ~ "http://d2a3o6pzho379u.cloudfront.net/110282.jpg",
  closest_defensive_player == "Stefanie Wetli" ~ "http://d2a3o6pzho379u.cloudfront.net/138077.jpg",
  closest_defensive_player == "Petra Nieminen" ~ "http://d2a3o6pzho379u.cloudfront.net/137113.jpg",
  closest_defensive_player == "Megan Bozek" ~ "http://d2a3o6pzho379u.cloudfront.net/128728.jpg",
  closest_defensive_player == "Lara Christen" ~ "https://results.eurosport.com/images/athletes/l/1048326.png",
  closest_defensive_player == "Nicole Vallario" ~ "https://results.eurosport.com/images/athletes/l/1049894.png",
  closest_defensive_player == "Erin Ambrose" ~ "https://olympic.ca/wp-content/uploads/2021/12/2021NWTHockey_Ambrose-9207917-e1640793324623.jpg",
  closest_defensive_player == "Jillian Saulnier" ~ "http://d2a3o6pzho379u.cloudfront.net/136868.jpg",
  closest_defensive_player == "Rebecca Johnston" ~ "http://d2a3o6pzho379u.cloudfront.net/119353.jpg",
  closest_defensive_player == "Alina Marti" ~ "https://results.eurosport.com/images/athletes/l/1048774.png",
  closest_defensive_player == "Dominique Ruegg" ~ "http://d2a3o6pzho379u.cloudfront.net/138073.jpg",
  closest_defensive_player == "Renata Fast" ~ "http://d2a3o6pzho379u.cloudfront.net/136865.jpg",
  closest_defensive_player == "Phoebe Staenz" ~ "http://d2a3o6pzho379u.cloudfront.net/128510.jpg",
  closest_defensive_player == "Lara Stalder" ~ "http://d2a3o6pzho379u.cloudfront.net/128509.jpg",
  closest_defensive_player == "Brianne Jenner" ~ "http://d2a3o6pzho379u.cloudfront.net/127492.jpg",
  closest_defensive_player == "Yulia Smirnova" ~ "https://results.eurosport.com/images/athletes/l/1056284.png",
  closest_defensive_player == "Mich Zandee-Hart" ~ "https://olympic.ca/wp-content/uploads/2021/12/2021NWTHockey_Zandee-Hart-9208271-e1640793582191.jpg",
  closest_defensive_player == "Cayla Barnes" ~ "https://assets.ngin.com/attachments/profiles/6d5d-169970667/USA_BARNES_CAYLA.jpg",
  closest_defensive_player == "Abby Roque" ~ "https://assets.ngin.com/attachments/profiles/6b96-169970874/USA_ROQUE_ABBY.jpg",
  closest_defensive_player == "Savannah Harmon" ~ "https://assets.ngin.com/attachments/profiles/4122-169971517/USA_HARMON_SAVANNAH.jpg",
  closest_defensive_player == "Alina Muller" ~ "https://nuhuskies.com/images/2021/9/24/mueller.JPG",
  TRUE ~ as.character(NA)
  ),   ## flag urls
  flag_url = case_when(
    defender_team == "USA" ~ "https://upload.wikimedia.org/wikipedia/en/thumb/a/a4/Flag_of_the_United_States.svg/1200px-Flag_of_the_United_States.svg.png",
    defender_team == "Finland" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Flag_of_Finland.svg/640px-Flag_of_Finland.svg.png",
    defender_team == "ROC" ~ "https://upload.wikimedia.org/wikipedia/commons/4/48/ROC_flag_%282021_NWSCh%29.png",
    defender_team == "Switzerland" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f3/Flag_of_Switzerland.svg/640px-Flag_of_Switzerland.svg.png",
    defender_team == "Canada" ~ "https://upload.wikimedia.org/wikipedia/en/thumb/c/cf/Flag_of_Canada.svg/1200px-Flag_of_Canada.svg.png",
    TRUE ~ as.character(NA)
  )
  )%>%
  dplyr::filter(complete.cases(defender_num))
  
### write update defender metrics  
write.csv(defender.sim.mcmc,"Summary/defender.sim.mcmc.csv")

mean(pass_metrics$n)


#### visualize output
tab_data<- defender.sim.mcmc %>%
  left_join(pass_metrics,by=c("defender_id")) %>%
  mutate(mcmc.mean = round(mcmc.mean,3),
         sim.mean = round(sim.mean,3),
         mcmc.sd = round(mcmc.sd,3),
         sim.sd = round(sim.sd,3),
         dis_event_x_abs = round(abs(dis_event_x_abs),2),
         dis_event_des_x_abs = round(abs(dis_event_des_x_abs),2),
         closest_opp_action_separation = round(abs(closest_opp_action_separation),2),
         closest_opp_player_separation = round(abs(closest_opp_player_separation),2)) %>%
  arrange(desc(mcmc.mean)) %>%
  filter(
         mcmc.mean > 0,
         n > 5) %>%
  ungroup() %>%
  head(10L) %>%
  dplyr::select(closest_defensive_player,position,flag_url,headshot_url,n,mcmc.mean,mcmc.sd,sim.mean,sim.sd,chances,closest_opp_player_separation)
### figure out pass play event under average

##### top passing defended above average ####
tab_function <- function(data, ...){
  data %>% 
    gt() %>% 
    text_transform(
      locations = cells_body(vars(headshot_url)),
      fn = function(x){
        web_image(
          url = x,
          height = px(30)
        )
      }
    ) %>% 
    text_transform(
      locations = cells_body(vars(flag_url)),
      fn = function(x){
        web_image(
          url = x,
          height = px(30)
        )
      }
    ) %>%
    cols_label(
      headshot_url = "",
      flag_url = "",
      closest_defensive_player = "Player",
      position = "Pos.",
      chances = "Off. Players Guarding",
      n = "Plays",
      mcmc.mean = "DPAA",
      mcmc.sd = "DPAA Sdev",
      sim.mean = "DPAA (Sim Posteriors)",
      sim.sd = "DPAA Sdev (Sim Posteriors)",
      closest_opp_player_separation = "Avg. Separation to Closest Player (m)"
    ) %>% 
    data_color(
      columns = vars(mcmc.mean),
      colors = scales::col_numeric(
        palette = c("#f7f7f7","#7fbf7b"),         
        domain = NULL
      )
    ) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = vars(closest_defensive_player,position)
      )
    ) %>% 
    tab_options(
      column_labels.background.color = "white",
      column_labels.font.weight = "bold",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 12,
      heading.align = "left",
      ...
    ) %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    )  %>%
    tab_footnote(
      footnote = "Plays include passes, takeaways, dump in/outs, zone entries,shots, puck recoveries (Avg. 8 plays per defender in overall dataset)",
      locations = cells_column_labels(
        columns = n
      )) %>%
    tab_footnote(
      footnote = "DPAA uses 5 MCMC chains",
      locations = cells_column_labels(
        columns = mcmc.mean
      )) %>%
    tab_footnote(
      footnote = "standard deviations for DPAA",
      locations = cells_column_labels(
        columns = mcmc.sd
      ))  %>%
    tab_footnote(
      footnote = "Mean simulation of Posteriors using 10,000 simulations",
      locations = cells_column_labels(
        columns = sim.mean
      ))  %>%
    tab_footnote(
      footnote = "Standard deviation of simulation of Posteriors using 10,000 simulations",
      locations = cells_column_labels(
        columns = sim.sd
      ))  %>%
    tab_header(
      title = md("Top Defending Plays Above Average"),
    subtitle = md("Data: Stathletes | Int'l Games: 2022 Winter Olympics  | Min. 5 Plays defended; DPAA > 0")
  ) 
}

a<-tab_data %>% 
  dplyr::filter(complete.cases(headshot_url)) %>%
  slice(1:5) %>% 
  tab_function()

a
#a %>%
#  gtsave("Top_Defending_Plays_Above_Average.png")



##### bottom passing defended above 
bot_tab_data<- defender.sim.mcmc %>%
  left_join(pass_metrics,by=c("defender_id")) %>%
  mutate(mcmc.mean = round(mcmc.mean,3),
         sim.mean = round(sim.mean,3),
         mcmc.sd = round(mcmc.sd,3),
         sim.sd = round(sim.sd,3),
         dis_event_x_abs = round(abs(dis_event_x_abs),2),
         dis_event_des_x_abs = round(abs(dis_event_des_x_abs),2),
         closest_opp_action_separation = round(abs(closest_opp_action_separation),2),
         closest_opp_player_separation = round(abs(closest_opp_player_separation),2)) %>%
  arrange(mcmc.mean) %>%
  filter(n > 5,
         mcmc.mean < 0,
         complete.cases(closest_defensive_player)) %>%
  ungroup() %>%
  head(5L) %>%
  dplyr::select(closest_defensive_player,position,flag_url,headshot_url,n,mcmc.mean,mcmc.sd,sim.mean,sim.sd,chances,closest_opp_player_separation)


bot_tab_function <- function(data, ...){
  data %>% 
    gt() %>% 
    text_transform(
      locations = cells_body(vars(headshot_url)),
      fn = function(x){
        web_image(
          url = x,
          height = px(30)
        )
      }
    ) %>% 
    text_transform(
      locations = cells_body(vars(flag_url)),
      fn = function(x){
        web_image(
          url = x,
          height = px(30)
        )
      }
    ) %>%
    cols_label(
      headshot_url = "",
      flag_url = "",
      closest_defensive_player = "Player",
      position = "Pos.",
      chances = "Off. Players Guarding",
      n = "Passes",
      mcmc.mean = "PDAA",
      mcmc.sd = "PDAA Sdev",
      sim.mean = "PDAA (Sim Posteriors)",
      sim.sd = "PDAA Sdev (Sim Posteriors)",
      closest_opp_player_separation = "Avg. Separation to Closest Player (m)"
    ) %>% 
    data_color(
      columns = vars(mcmc.mean),
      colors = scales::col_numeric(
        palette = c("#FAD5A5","#CC5500"),         
        domain = NULL
      )
    ) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = vars(closest_defensive_player,position)
      )
    ) %>% 
    tab_options(
      column_labels.background.color = "white",
      column_labels.font.weight = "bold",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 12,
      heading.align = "left",
      ...
    ) %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    )  %>%
    tab_footnote(
      footnote = "Plays include passes, takeaways, dump in/outs, zone entries,shots, puck recoveries",
      locations = cells_column_labels(
        columns = n
      )) %>%
    tab_footnote(
      footnote = "PDAA uses 5 MCMC chains",
      locations = cells_column_labels(
        columns = mcmc.mean
      )) %>%
    tab_footnote(
      footnote = "standard deviations for PDAA",
      locations = cells_column_labels(
        columns = mcmc.sd
      ))  %>%
    tab_footnote(
      footnote = "Simulation of Posteriors using 10,000 simulations",
      locations = cells_column_labels(
        columns = sim.mean
      ))  %>%
    tab_header(
      title = md("Bottom Passing Defended Above Average"),
      subtitle = md("Data: Stathletes | Int'l Games: 2022 Winter Olympics  | Min. 5 Plays defended; DPAA < 0")
    ) 
}



bot_tab_data %>%
bot_tab_function()
##### show heatmap of separation by joining player to play data! 


write.csv(pass.frame.sim.preds,"Summary/pass_sim_preds.csv")



### show variation by play
pass.frame.sim.preds %>%
  group_by(event) %>%
  dplyr::summarize(
    n = n_distinct(playId),
    PDAA = mean(fitted.values - wo_defender,na.rm = T)) %>%
  pivot_wider(names_from = event,values_from = c(PDAA,n)) 


#### best player average for a specific play? 


saveRDS(pass.success.mod.mcmc,"Model/pass.success.mod.mcmc.rds")



#### visualize MCMC results
pass.success.mod.mcmc$coefficients
posterior<-as.array(pass.success.mod.mcmc)

#mcmc_dens_overlay(posterior, pars = c("separation", "net_separation"))

mcmc_intervals(posterior, pars = c("separation", "net_separation",
                              "closest_opp_player_separation",
                              "closest_event_player_separation",
                              "dis_event_x_abs"))


p<-plot(pass.success.mod.mcmc, pars = c("net_separation",),
     prob = 0.5, prob_outer = 0.9)
p + ggplot2::ggtitle("Posterior medians \n with 50% and 90% intervals")


mcmc_areas_ridges(posterior, pars = c("separation", "net_separation",
                              "closest_opp_player_separation",
                              "closest_event_player_separation",
                              "dis_event_x_abs"), border_size = 0.75) 


