library(tidyverse)
library(progress)
tr<-read_csv("DataTransform/tracking_cleaned.csv") %>%
  dplyr::select(-`...1`)

parallelly::availableCores()
future::plan("multisession")

#unique(tr$event_type)

###### amount of powerplays ###
#Game 1 CAN_USA = 6; Left: 0
#Game 2 ROC_FIN = 7; Left: 0
#Game 3 Swiss_ROC = 4;  Left: 0
#Game 4 Fin_USA = 5; Left: 0
#Game 5 Swiss_Can = 4; Left: 0
#Game 6 Swiss_Fin = 8; Left: 1
# Total = 34, Left: 1



#### grab separation metrics by players & return offensive players


sample<-tr %>% filter(pp_index %in% c("2022-02-16 Switzerland at Finland--8"))
max<-max(sample$playId)
min<-min(sample$playId)
diff<-max-min

#### do it by pp so you can work on analysis separately
pb <- progress_bar$new(
  format = "  downloading [:bar] :percent eta: :eta",
  total = diff+1, clear = FALSE, width= 60)

bdc_get_vars_hockey <- function(df_tracking){
  df_tracking_list <- df_tracking %>%
    group_split(pp_index,playId) # split data by week to save memory
  
  print('Starting Loop')

  
  df_tracking_vars <- {}
  for(i in 1:length(df_tracking_list)){
    start_time <- Sys.time()
    pb$tick()
    Sys.sleep(1/100)
    
    df_tracking_vars[[i]] <- df_tracking_list[[i]] %>%
      dplyr::select(pp_index,playId, game_name, frame_id, jn, track_id,team_name,event_team, x_ft_new, y_ft_new,event_bool,event,event_successful,
             x_coord_new,y_coord_new, x_coord_2_new, y_coord_2_new, 
             x_ft_face_off, y_ft_face_off, x_ft_play, y_ft_play,x_ft_ze, 
             y_ft_ze, faceOff, Play, Shot, zone_entry,event_detail_1,frame_of_seq,start_zone,event_type) %>%
      # join with play data
      inner_join(df_tracking_list[[i]] %>%
                   select(pp_index,playId, game_name, frame_id, jn, track_id,x_ft_new, y_ft_new, event_team,
                          event,x_ft_face_off,y_ft_face_off,x_ft_play,y_ft_play,x_ft_ze,y_ft_ze), by = c('playId', 'pp_index', 'frame_id')) %>% # join with tracking data to see interactions between players
      rename_at(vars(contains('.x')), function(x) gsub('\\.x', '1', x)) %>% # rename vars in df_tracking 1 to player 1
      rename_at(vars(contains('.y')), function(x) gsub('\\.y', '2', x)) %>% # rename vars in df_tracking 2 to player 2
      group_by(pp_index, playId, jn1, jn2) %>%
####### CREATE METRICS ###########
      dplyr::mutate(
                    var_x = var(x_ft_new1), # variance of x coord
                    var_y = var(y_ft_new1), # variance of y coord
                    avg_x = mean(x_ft_new1, na.rm = TRUE), # avg x position
                    avg_y = mean(y_ft_new1, na.rm = TRUE), # avg y position
                    dis_from_event_x = x_ft_new1 - x_coord_new, 
                    dis_from_event_y = y_ft_new1 - y_coord_new,
                    dis_from_event_des_x = x_ft_new1 - x_coord_2_new,
                    dis_from_event_des_y = y_ft_new1 - y_coord_2_new,
                    # event pos
                    # jersey 100
                    j100_x = x_ft_new1 - ifelse(jn2 == 100,x_ft_new1,NA),
                    j100_y = y_ft_new1 - ifelse(jn2 == 100,y_ft_new1,NA),
                    ## distance from center net
                    net_x = x_ft_new1 - 189,
                    net_y = y_ft_new1 - 49,
                    separation = sqrt((x_ft_new1 - x_ft_new2) ^ 2 + (y_ft_new1 - y_ft_new2) ^ 2),
                    avg_sep = mean(separation, na.rm = TRUE),
                    event_start_separation = ifelse(event_bool == "start_event",separation,NA),
                    event_separation = ifelse(event_bool == "event",separation,NA),
                    non_event_separation = ifelse(event_bool == "non_event",separation,NA),
                    net_separation = sqrt((x_ft_new1 - 189) ^ 2 + (y_ft_new1 - 49) ^ 2),
                    same_team = ifelse(event_team1 == event_team2,1,0),
                    same_player = ifelse(jn1 == jn2,1,0)) %>% # distance travelled
      ungroup() %>%
      group_by(pp_index,playId,jn1, frame_id) %>%
      
      ####### LEFT OFFF 
      # figure out top 3 closest players? 
      mutate(
             ##### event player ids
            closest_event_player_id = jn2[separation == min(separation[same_player == 0 & event_team2 == "event_team" & jn2!= 100],na.rm = T)][1],
             second_closest_event_player_id = jn2[separation == min(separation[same_player == 0 & event_team2 == "event_team" & jn2 != closest_event_player_id & jn2!= 100],na.rm = T)][1],
             third_closest_event_player_id = jn2[separation == min(separation[same_player == 0 & event_team2 == "event_team" & jn2 != closest_event_player_id & jn2!= 100 & jn2!= second_closest_event_player_id],na.rm = T)][1],
             ## event separation
            closest_event_player_separation = min(separation[same_player == 0 & event_team2 == "event_team" & jn2 == closest_event_player_id], na.rm = T),
            second_closest_event_player_separation = min(separation[same_player == 0 & event_team2 == "event_team" & jn2 == second_closest_event_player_id], na.rm = T),
            third_closest_event_player_separation = min(separation[same_player == 0 & event_team2 == "event_team" & jn2 == third_closest_event_player_id], na.rm = T),
            #### oppo team player ids
            closest_opp_player_id = jn2[separation == min(separation[same_player == 0 & event_team2 == "opp_team" & jn2!= 100],na.rm = T)][1],
            second_closest_opp_player_id = jn2[separation == min(separation[same_player == 0 & event_team2 == "opp_team" & jn2 != closest_opp_player_id & jn2!= 100],na.rm = T)][1],
            third_closest_opp_player_id = jn2[separation == min(separation[same_player == 0 & event_team2 == "opp_team" & jn2 != closest_opp_player_id & jn2!= 100 & jn2!= second_closest_opp_player_id],na.rm = T)][1],            
            ### oppo separation
            closest_opp_player_separation = min(separation[same_player == 0 & event_team2 == "opp_team" & jn2 == closest_opp_player_id], na.rm = T),
            second_closest_opp_player_separation = min(separation[same_player == 0 & event_team2 == "opp_team" & jn2 == second_closest_opp_player_id], na.rm = T),
            third_closest_opp_player_separation = min(separation[same_player == 0 & event_team2 == "opp_team" & jn2 == third_closest_opp_player_id], na.rm = T),            
            ### jn 100 sep
            closest_100_event_separation = min(separation[same_player == 0 & event_team2 == "event_team" & jn2 == 100], na.rm = T),
            closest_100_opp_separation = min(separation[same_player == 0 & event_team2 == "opp_team" & jn2 == 100], na.rm = T),
            #### situational separation
            closest_event_start_separation = min(event_start_separation[same_player == 0 & event_team2 == "event_team" & jn2 == 100],na.rm = T),
            closest_opp_start_separation = min(event_start_separation[same_player == 0 & event_team2 == "opp_team" & jn2 == 100],na.rm = T),
            # event separation
            closest_event_action_separation = min(event_separation[same_player == 0 & event_team2 == "event_team" & jn2 == 100],na.rm = T),
            closest_opp_action_separation = min(event_separation[same_player == 0 & event_team2 == "opp_team" & jn2 == 100],na.rm = T),
            # non event separation
            closest_non_event_separation = min(non_event_separation[same_player == 0 & event_team2 == "event_team" & jn2 == 100],na.rm = T),
            closest_non_opp_separation = min(non_event_separation[same_player == 0 & event_team2 == "opp_team" & jn2 == 100],na.rm = T)
            ) %>%
      filter(same_player == 0 & event_team1 == "event_team") %>%  ### filter for event team
      group_by(pp_index,playId,jn1) %>%
      summarise(
        ### take first metrics
        var_x = mean(var_x,na.rm = T),
        var_y = mean(var_y,na.rm = T),
        dis_from_event_x = mean(dis_from_event_x,na.rm = T),
        dis_from_event_y = mean(dis_from_event_y,na.rm = T),
        dis_from_event_des_x = mean(dis_from_event_des_x,na.rm = T),
        dis_from_event_des_y = mean(dis_from_event_des_y,na.rm = T),
        var_j100_x = var(j100_x,na.rm = T),
        var_j100_y = var(j100_y,na.rm = T),
        separation = mean(separation,na.rm = T),
        event_start_separation = mean(event_start_separation,na.rm = T),
        event_separation = mean(event_separation,na.rm = T),
        non_event_separation = mean(non_event_separation,na.rm = T),
        net_separation = mean(net_separation,na.rm = T),
        # player separation
        closest_event_player_separation = mean(closest_event_player_separation[is.finite(closest_event_player_separation)]),
        second_closest_event_player_separation = mean(second_closest_event_player_separation[is.finite(second_closest_event_player_separation)]),
        third_closest_event_player_separation = mean(third_closest_event_player_separation[is.finite(third_closest_event_player_separation)]),
        closest_opp_player_separation = mean(closest_opp_player_separation[is.finite(closest_opp_player_separation)]),
        second_closest_opp_player_separation = mean(second_closest_opp_player_separation[is.finite(second_closest_opp_player_separation)]),
        third_closest_opp_player_separation = mean(third_closest_opp_player_separation[is.finite(third_closest_opp_player_separation)]),
        # specific separation
        closest_100_event_separation = mean(closest_100_event_separation[is.finite(closest_100_event_separation)]),
        closest_100_opp_separation = mean(closest_100_opp_separation[is.finite(closest_100_opp_separation)]),
        closest_event_start_separation = mean(closest_event_start_separation[is.finite(closest_event_start_separation)]),
        closest_opp_start_separation = mean(closest_opp_start_separation[is.finite(closest_opp_start_separation)]),
        closest_event_action_separation = mean(closest_event_action_separation[is.finite(closest_event_action_separation)]),
        closest_opp_action_separation = mean(closest_opp_action_separation[is.finite(closest_opp_action_separation)]),
        closest_non_event_separation = mean(closest_non_event_separation[is.finite(closest_non_event_separation)]),
        closest_non_opp_separation = mean(closest_non_opp_separation[is.finite(closest_non_opp_separation)]),
        ## grab closest players
        closest_event_player_id = closest_event_player_id[1],
        second_closest_event_player_id = second_closest_event_player_id[1],
        third_closest_event_player_id = third_closest_event_player_id[1],
        closest_opp_player_id = closest_opp_player_id[1],
        second_closest_opp_player_id = second_closest_opp_player_id[1],
        third_closest_opp_player_id = third_closest_opp_player_id[1]
      ) %>%
      ungroup() %>%
      dplyr::rename(jn = jn1)
    
    
    ### take care of infinite numbers 
    
    end_time <- Sys.time()
    print(paste('Took', round(end_time - start_time, 2), 'minutes for penalty play', i))
  }
  
  df_tracking_vars <- do.call('rbind', df_tracking_vars)
  
  return(df_tracking_vars)
}

samp<-bdc_get_vars_hockey(sample)


#tracking_avg<-bdc_get_vars_hockey(tr)
#summary_tracking<-bdc_get_vars_punt(tr)

#write.csv(samp,"CAN_USA_pp7_off_tracking_calcs.csv")
#write.csv(samp,"SWISS_FIN_pp8_off_tracking_calcs.csv")
