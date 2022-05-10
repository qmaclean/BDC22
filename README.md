# BDC22
Big Data Cup 22

PDF file contains final paper. 

Data comes from: https://github.com/bigdatacup/Big-Data-Cup-2021

Competition background: https://www.stathletes.com/big-data-cup/


### Introduction
Currently, there are a lot of advanced analytics focusing on [shot quality, defending shots,](https://drive4five.blog/2021/01/06/hockey-advanced-statistics-stats-nhl-sports-analytics-aidan-resnick/) and play during even strength. This year’s Big Data Cup focuses on play during penalties as a way to gain more insights into various “special teams” strategies. During the Tracking Data Panel at the 2022 Ottawa Hockey Analytics Conference (OTTHAC), measuring defensive performance was posed as the current area of tracking analytics that had the biggest opportunity to add more depth to. Shawn Ferris echoed this sentiment in a 2020 hockey-graphs article where he said, [“there is still much more to learn about shorthanded defense with respect to understanding optimal in-zone formations and evaluating individual players”.](https://hockey-graphs.com/2020/04/16/using-data-to-inform-shorthanded-neutral-zone-decisions/#more-24218) This paper aims to build on that narrative by using 2022 Women’s Olympic Hockey event & tracking data to model around successful defensive plays as a way to scout team and individual defensive performance during penalty plays. A successful defensive play is based on a (1) Takeaway, (2) Unsuccessful Pass, (3), Dump In/Out resulting in change of possession, (4) Blocked Shot attempt, (5) Unsuccessful Zone Entry, or (6) Puck Recovery resulting in change of possession.

Current event metrics assign defensive play credit for takeaways, puck recoveries and blocked shots but these events don’t account for a majority of events such as passing, zone entry and dump in/out plays. To do so, we rely on tracking movement data to identify the closest opposing player to an offensive player as a way to give credit to a defender for the outcome of the play. The assumption we are using is that the closest defender or the one most prominent in the frame is the one most likely to be contributing to the outcome of a play. Each offensive player is assigned a closest defensive player so it is possible for a defender to be guarding multiple players for a given event. By merging the event data to the approximate time frame of the tracking data, we can gain valuable insight into the average location and separation at start, leading up to, during the event, and after the event. 

### Modelling Approach
Using a similar approach to [Jonathan Judge from Baseball Prospectus,](https://www.baseballprospectus.com/news/article/38289/bayesian-bagging-generate-uncertainty-intervals-catcher-framing-story/) a Generalized Linear Mixed Effects Model using Monte Carlo Markov Chains (MCMC) was created to identify, which defensive player had the greatest effect in adjusting the outcome of a defensive play. Our MCMC uses 5 chains and 10,000 iterations per chain to simulate and get an effective total sample size. The modeled dataset contains 1,968 different defensive play matchups (offensive player matched up to closest defensive player) for 507 unique events. In the mixed effects model, we fit the closest defensive player (for each 1,968 matchups) as the random effect. Through our exploratory analysis, we found that the following features (centered and scaled) determined a successful defensive play: **Separation** (Avg. offensive player separation or distance to any player on the ice during an event), **Net Separation** (Avg. offensive player separation to the net during an event), **Closest Opposition Player Separation** (Avg. offensive player separation to the closest defender during an event), **Closest Teammate Player Separation** (Avg. offensive player separation to closest teammate during an event), **Absolute x-Distance to Passer/Shooter** (Avg. distance of an offensive player to the passer or shooter), **Absolute x-Distance to Event Recipient** (Avg. distance of an offensive player to the recipient of the event).

#### Model Results & Evaluation
In viewing the model results, there is evidence (τ00) to show that wide variability (0.78) exists in a defender’s ability to alter a play’s outcome outside of the base model. This framework can help to identify those players who should be on ice to defend against power plays. Outside of an individual defender’s skill, the base model shows that an offensive players’ spacing and separation from each player tends to determine the outcome of a successful play. This tells us that the defender’s main goal is to reduce offensive player’s separation from anyone on the ice. Another observation is a fairly high Monte Carlo standard error of the variables in the model (typically want less than 5%) and this is likely derived from different event types included in the modeled data.

<img src="https://github.com/qmaclean/BDC22/blob/main/Viz/DPAA_Play.png" width="50%" />


For evaluating the model, we used posterior predictive checking, which takes draws from the MCMC and compares the Yrep (expected defended plays in the replicated simulations) to Y (actual defended play results). As a result, we saw that the MCMC observed the binomial distribution of the output (successfully defended play or not). Using the used [Gelman and Rubin’s scale reduction factor](https://mc-stan.org/docs/2_18/reference-manual/notation-for-samples-chains-and-draws.html) or Rhat, we saw the MCMC reached proper convergence towards the target distribution with each variable and intercept’s Rhat less than 1.05. We can conclude the Bayesian MCMC model can be a good tool to evaluate defensive play during power plays. 
In determining player performance relative to the average, we created a “without defender” metric, which show the probabilistic outcome of the play (not accounting for a specific defender’s involvement). The average difference between the MCMC mean and “without defender” metric helps to create our **Defending Plays Above Average (DPAA) metric**. *DPAA* shows the lift in probability a defensive player has on the outcome of a play. Any play with a *DPAA* greater than 0 can be defined as a positively defended play.  

As a result, the model tends to identify Dump In/Outs as the event that has the highest probability of a positively defended event (e.g., Dump In/Outs resulting in a turnover). Takeaways and Puck Recoveries tend to be less positively defended which may be due to a secondary defender’s ability to disrupt a play. Passes are one of the most difficult plays to defend. Given this information, we can also use the DPAA metric specifically for passing plays as a way to identify those defenders who were especially effective in disrupting offensive movement during a power play. Given the power play situation, we can see that there are more passes than zone entries, which may be the vastly different in even strength situations.  Shawn Ferris said that “We know that deploying better offensive players, controlling more entries, and passing after those entries all lead to more shorthanded goals over time.” Players with a reputation of defending the zone line (“blue line”) and disrupting passing are more likely to be on the ice during shorthanded defensive play. DPAA can also help to highlight how proper defensive formations can reduce the performance of passing in the offensive zone. 


<img src="https://github.com/qmaclean/BDC22/blob/main/Viz/DPAA_Event_Avg.png" width="50%" />

### Evaluating Team Performance

<img src="https://github.com/qmaclean/BDC22/blob/main/Viz/Team_DPAA.png" width="100%" />

USA, Finland, and Russian Olympic Committee (ROC) were able to have an above average DPAA rating, largely due to their ability to defend passes. USA is interesting case study given their low number of passes seen but saw the highest amount of Dump In/Out (18) and Puck Recoveries (36) relative to other countries. In fact, USA had a positive DPAA average during Puck Recoveries whereas Switzerland saw negative DPAA during Puck Recoveries. As we saw prior, the ability to defend against puck recoveries shows USA’s ability to preemptively disrupt passes/shots. USA also had closer separation to opposing players (14.95 ft of separation) compared to any other team. Given USA’s higher DPAA standard deviation, one conclusion could be that their individual performance led to better play rather than team defensive formation. Finland had the highest separation from the players they were guarding (16.78 ft of separation) but the offensive team they were guarding were the most spread out and generally farthest from the net. In short, Finland’s strategy was to space out the offense away from the net. ROC had a high amount of passing plays defended positively but overall had the [lowest penalty killing rate](https://www.iihf.com/en/events/2022/olympic-w/teamstats/penaltykilling#statistics) and the highest amount power plays goals against in the tournament at 8. Their low relative DPAA and standard deviation can likely mean that their team formation led to average defensive play. 

Canada and Switzerland were the lowest performing teams in defending plays. Canada had 50% of their goals against coming from power play goals despite having the [highest overall goal differential in the Olympics.](https://www.iihf.com/en/events/2022/olympic-w/teamstats/goalkeeping#statistics) This is due to their inability to defend passes during penalty play situations leading to the highest average pass distance compared to other teams (i.e. offensive teams were able to spread the Canadian defense). For example, the USA was able to out shoot Canada 53-27 in their first game yet Canada was able to able to win the game, which can likely be attributed to excellent Canadian goaltending. Overall Canada had a slighter higher quality (DPAA) of plays defended against compared to Switzerland. The reason for Switzerland having the lowest DPAA average was their inability to defend shots given they had the worst DPAA shot average and [2nd lowest penalty killing play](https://www.iihf.com/en/events/2022/olympic-w/teamstats/penaltykilling#statistics) during the tournament. Given the majority of the differences in DPAA were based on how well the team defends passing, the visual above identifies a team’s approach or strategy to defending passes during penalty killing situation. We can see above that the USA tends to get aggressive at the blue line very resulting in less passes in the offensive zone (with exception of top left boards). To the contrary, most of Finland’s great defensive play was in the offensive zone whereas they tend to not perform well in the neutral zone. ROC is an interesting case study given that they are deploying a [diamond/box defensive formation](https://blueseatblogs.com/2011/10/24/penalty-kill/) and their defensive play appears to be inconsistent as passes through the formation haven’t been defended well. Ultimately, ROC’s DPAA could have been a lot higher if their defensive formation was a lot tighter. Canada was not good at defending the blue line but defended the boards and near the net well. Canada needs to replace the top end player in their diamond/box formation or get more aggressive with defending the blue line.

<img src="https://github.com/qmaclean/BDC22/blob/main/Viz/Passing_Defense_Country.png" width="50%" />

### Individual Performance Analysis

#### Top Performers
To ensure proper scouting of individual performers, simulating the posteriors (using 10,000 iterations) helps to ensure the stability of the MCMC metrics (sim mean). We can see that players from Finland and USA were the top performers. One reason is that these players tend to be closer to the player they are guarding than the average defender (+4ft vs avg) with the exception of Megan Keller. Megan Keller was the best of these defenders in the tournament in defending against passes (at least 5 passes), which makes her a valuable specialist to the USA team. Although Viivi Vainikka had the highest DPAA, this was due to a majority of her plays coming from successfully defending Dump Ins/Outs (easier plays to defend against). Ronja Savolainen guarded nearly 43 offensive players and had an impressive passing DPAA at 0.263.

<img src="https://github.com/qmaclean/BDC22/blob/main/Viz/Top_Performers.png" width="100%" />


When simulating the posteriors (using 10,000 sims), we can see that she also had the lowest relative standard deviation to her simulated DPAA mean, which helps to show her consistent play for Finland and likely a big reason for their positive overall defense given their high team standard deviation. Here are the top 3 DPAA (MCMC mean) defenders for each country (7+ plays): \

•	Canada: Marie-Philip Poulin (0.175),  Blayre Turnbull (0.131),  Brianne Jenner (-0.009) \
•	Finland: Viivi Vainikka (0.488), Minnamari Tuominen (0.472), Ronja Savolainen (0.264) \
•	ROC: Oxana Bratishcheva (0.173), Nina Pirogova (0.108), Olga Sosina (0.068) \
•	Switzerland: Phoebe Staenz (0.075), Noemi Ryhner (0.023), Lara Stalder (-0.031) \
•	USA: Megan Keller (0.477), Abby Roque (0.375), Amanda Kessel (0.203) \

#### Bottom Performers
The bottom individual performers were primarily from Canada and Switzerland with the exception of Susanna Tapani from Finland. Higher separation from the players guarding (+4ft vs avg) and total amount of players guarding (40 was 8th highest number of players closest to) appear to be the top reason for Susanna’s performance. Her teammate Ronja Savolainen had guarded a similar number of players but played closer to defenders on average and saw a better DPAA as a result. Given Canada and Switzerland’s low team standard deviation, the bottom performers listed here are more likely a function of their team’s defensive formation than their individual play. Lastly, it is important to note that the ROC had only 1 negative DPAA player despite overall average team DPAA (near 0). This leads us to believe their overall defensive formation can be attributed as the reason for their low penalty killing rate. Here are the bottom 3 DPAA (MCMC mean) defenders for each country (7+ plays):

•	Canada: Rebecca Johnston (-0.219), Natalie Spooner (-0.187), Mich Zandee-Hart (-0.148) \
•	Finland: Susanna Tapani (-0.227), Sini Karjalainen (-0.199), Ella Viitasuo (-0.040) \
•	ROC: Fanuza Kadirova (-0.088), Maria Batalova (0.014), Angelina Goncharenko (0.017) \
•	Switzerland: Lara Christen ( -0.226), Nicole Vallario (-0.225), Alina Marti (-0.218) \
•	USA: Alex Carpenter (-0.143), Lee Stecklein (-0.060), Dani Cameranesi (0.003) \

<img src="https://github.com/qmaclean/BDC22/blob/main/Viz/bottom_performer.png" width="100%" />

#### High Volume Performers
Below shows defenders with the highest amount of involvement and helps us to provide insights into the scheme assignment given out to shorthanded defenders. As we can see, most players are fitting into a diamond/box format given the concentration of defending plays made. The exception is Jocelyne Larocque (Canada) who has been defending the boards, which may have contributed to her below average play given she had to guard a wider zone. The same goes for Mich Zandee-Hart (Canada) who was stretched to play in three different parts of the diamond/box format. Canada may want to play into a tighter box/diamond format or have players play more disciplined to their zone assignment as a way to prevent and spread out passes for future games. Ronja Savolainen played mostly towards the blue line with some plays made in the neutral zone. The difference with her play versus Mich was that Ronja played at 13ft separation vs Mich played at 21ft separation (nearly +6 vs avg). Jenni Hiirikoski, similar to Ronja Savolainen, also played 13ft to her defender and strictly to what appears to be an assigned zone. Jenni Hiirikoski, similar to Ronja Savolainen, also played 13ft to her defender and strictly to what appears to be an assigned zone. A rigid defensive zone formation would help to tighten average separation to the closest offensive player and increased positive defended plays as defenders would play to their assigned zone.

<img src="https://github.com/qmaclean/BDC22/blob/main/Viz/HighVolume.png" width="75%" />


### Conclusion
DPAA can be a tool to evaluate defensive play during penalty situations. The direct application of DPAA can be used to evaluate if overall team performance comes from excellent individual performance or a function of a poor defensive formation. One insight from the model was for defenders to aim to be within 15ft of the offensive players they are guarding. A zone assignment would install a system that ensures players are playing within 15ft of the nearest offensive player and prevent a player from being effective. Results from the model said that defending passes has the lowest probability of a positively defended play and DumpIn/Outs had the highest probability of a positively defended play. This is why taking the USA strategy of playing the blue line aggressively as a way to prevent passing plays in the offensive zone from occurring is the optimal strategy. Ideally, the players at the top of the box/diamond zone formation are your more athletic/quicker skaters that can be able to reduce offensive separation and your remaining defenders can play a tighter portion of the box/diamond to prevent play in front of the net. DPAA would be an effective tool to identify those defenders who can play at the top of the box/diamond formation. One way of monitoring individual defender’s performance is within the linked sample scouting reports, which uses DPAA to measure a player’s performance relative to all others. 

DPAA could be expanded to even strength situations given the proportion of passes but we would hypothesize that zone entry carries would likely play a greater role in the model (given less proportion of zone entries during power plays). The model would also need to be changed to take into account play situation given we would want to investigate even strength vs special teams play as a performance metric. I would want use DPAA to further classify different optimal forechecking and in-zone formations to improve team defensive formation strategies.
I would like to thanks Stathletes and the OTTHAC for hosting the Big Data Cup and providing the data for advancing the use of analytics in women’s hockey. Also, a special thanks to Josh Pohlkamp-Hartt from the Boston Bruins for providing perspective into shorthanded play/strategies.


Sample Scouting Reports: https://github.com/qmaclean/BDC22/tree/main/DPAA%20Scouting%20Reports

**Sarah Nurse (example):**

<img src="https://github.com/qmaclean/BDC22/blob/main/DPAA%20Scouting%20Reports/Sarah%20Nurse_DPAA.png" width="100%" />








