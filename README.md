# Premier-League-Analysis-and-insights
An analysis of the Premier League from 1992 through 2022

# Introduction

According to Wikipedia, The Premier League is the highest level of the men's English football league system. Contested by 20 clubs, it operates on a system of promotion and relegation with the English Football League (EFL). Seasons typically run from August to May with each team playing 38 matches (playing all 19 other teams both home and away). The competition was founded as the FA Premier League on 20 February 1992 following the decision of clubs in the Football League First Division to break away from the Football League, founded in 1888. The Premier League is the most-watched sports league in the world, broadcast in 212 territories to 643 million homes and a potential TV audience of 4.7 billion people. For the 2018–19 season, the average Premier League match attendance was 38,181, second to the German Bundesliga's 43,500.

# 1. ask phase
In this repository, we will be trying to make some season analyses (trace the PL during the years), some Teams analyses, and try to answer some questions.

**The questions that we will try to answer during this analysis:**
  
  1-Does the home stadium ground give any advantage? And if the answer is yes, what's the quantity for this advantage?

2- what is the best way to collect points, defensive or attacking play?

3 - who is the best coach in PL history?

# 2.Prepare phase

Firstly I will start by loading some packages that I will use during the analysis

```
library(tidyverse)
library(gdata)
```

the package that I will use to download the data

```
library(worldfootballR)
```

loading and read the data in a data frame called "PL"

```
PL<-fb_match_results(country = "ENG", gender = "M", season_end_year = c(1993:2022), tier = "1st")
```

looking at the name field of the data

```
names(PL)
```

```
 [1] "Competition_Name" "Gender"           "Country"          "Season_End_Year"  "Round"           
 [6] "Wk"               "Day"              "Date"             "Time"             "Home"            
[11] "HomeGoals"        "Away"             "AwayGoals"        "Attendance"       "Venue"           
[16] "Referee"          "Notes"            "MatchURL"         "Home_xG"          "Away_xG"  
```

We will exclude some columns that we will not use in our analysis and assign the new data to the data frame called "PL_r"

```
PL_r<-PL %>%
  arrange(Season_End_Year,Wk)%>%
  select(Season_End_Year,Wk,Date,Home,HomeGoals,AwayGoals,Away)
```

the structure of the data
```
str(PL_r)
```

```
'data.frame':	11646 obs. of  7 variables:
 $ Season_End_Year: int  1993 1993 1993 1993 1993 1993 1993 1993 1993 1993 ...
 $ Wk             : int  1 1 1 1 1 1 1 1 1 1 ...
 $ Date           : chr  "1992-08-15" "1992-08-15" "1992-08-15" "1992-08-15" ...
 $ Home           : chr  "Coventry City" "Leeds United" "Sheffield Utd" "Crystal Palace" ...
 $ HomeGoals      : int  2 2 2 3 2 1 1 0 1 1 ...
 $ AwayGoals      : int  1 1 1 3 4 1 1 0 1 0 ...
 $ Away           : chr  "Middlesbrough" "Wimbledon" "Manchester Utd" "Blackburn" ...
```

converting a column "date" to Date instead of a character

```
PL_r$Date<-as.Date(as.character(PL_r$Date))
```

below is the description of our data variables

**Season_End_Year:** Premier League Season End Year

**Wk:** The week number

**Date:** Match Date

**Home:** Team playing at the Home Ground

**HomeGoals:** Home Team Goals 

**AwayGoals:** AwayTeam Goals

**Away:** Team playing at the Away Ground

we will add a field "FTR" that describes the final  result by H, A, or D (H=Home Win, D=Draw, A=Away Win)

```
PL_r$FTR<- case_when(
                  PL_r$HomeGoals>PL_r$AwayGoals~"H",
                  PL_r$HomeGoals<PL_r$AwayGoals~"A",
                  PL_r$HomeGoals==PL_r$AwayGoals~"D"
                     ) 
```

# 3.Process phase


summarize the data

```
summary(PL_r)
```

```
 Season_End_Year       Wk             Date                Home             HomeGoals       AwayGoals   
 Min.   :1993    Min.   : 1.00   Min.   :1992-08-15   Length:11646       Min.   :0.000   Min.   :0.00  
 1st Qu.:2000    1st Qu.:10.00   1st Qu.:1999-08-07   Class :character   1st Qu.:1.000   1st Qu.:0.00  
 Median :2007    Median :20.00   Median :2007-02-07   Mode  :character   Median :1.000   Median :1.00  
 Mean   :2007    Mean   :19.74   Mean   :2007-03-21                      Mean   :1.521   Mean   :1.14  
 3rd Qu.:2015    3rd Qu.:29.00   3rd Qu.:2014-11-29                      3rd Qu.:2.000   3rd Qu.:2.00  
 Max.   :2022    Max.   :42.00   Max.   :2022-05-22                      Max.   :9.000   Max.   :9.00  
     Away               FTR           
 Length:11646       Length:11646      
 Class :character   Class :character  
 Mode  :character   Mode  :character  
```

As we saw, "Wk" has a maximum of 42, which means some seasons have more than 38 weeks round. Let's check how many of them appear on wk through the data.

```
table(PL_r$Wk)
```

```
1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26 
303 303 303 303 303 303 303 303 303 303 303 303 303 303 303 303 303 303 303 303 303 303 303 303 303 303 
27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42 
303 303 303 303 303 303 303 303 303 303 303 303  33  33  33  33 
```

Rounds 39,40,41 and 42 appeared 33 times, which means in some years the PL continuous to round 42, let's see how many games played in every season

```
table(PL_r$Season_End_Year)
```

```
1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 
 462  462  462  380  380  380  380  380  380  380  380  380  380  380  380  380  380  380  380  380  380 
2014 2015 2016 2017 2018 2019 2020 2021 2022 
 380  380  380  380  380  380  380  380  380
```

as we see the seasons ending in 1993,1994 and 1995 have 462 matches each.
because the first 3 seasons in the English Premier League played with 22 teams before it reduced to 20 team 


**Checking for NAs**

```
colSums(is.na(PL_r))
```

```
Season_End_Year              Wk            Date            Home       HomeGoals       AwayGoals 
              0               0               0               0               0               0 
           Away             FTR 
              0               0 
```

No missing values at this data

# 4.Analyze phase

We will come out and try to answer the first question.

**Does the home stadium ground give any advantage? And if the answer is yes, what's the quantity for this advantage? **
  
  ```
## we will create a new dataframe with percentage of the game result
## (H: for home winning , A: for away team winning, and D for draw)

home_vs_away<-count(PL_r,FTR)%>%
  arrange(desc(n))

home_vs_away$percentage<-(home_vs_away$n/sum((home_vs_away$n)))*100
```

plotting the result

```
ggplot(data = home_vs_away,aes(x=reorder(FTR,-percentage),y=percentage))+
  geom_col()+
  ggtitle("The percentage of results at Home ground")+
  xlab("who wins the Home games")
```

![1](https://user-images.githubusercontent.com/41892582/191893564-d0916d0e-0c0c-4d35-bec5-39b9ab2715f1.png)


It looks like the home ground gives a big advantage. Let's try to quantify this advantage.

```
## creating a new data frame to calculate each average points Home and Away matches
home_vs_away_points<-matrix(nrow = 2,ncol = 2)
home_vs_away_points<-as.data.frame(home_vs_away_points)
names(home_vs_away_points)=c("where_to_play","Average_points")
home_vs_away_points$`where_to_play`<-c("Home","Away")

##average points at home matches
home_vs_away_points[1,2]<-((home_vs_away[1,2])*3+(home_vs_away[3,2])*1)/sum(home_vs_away$n)

##average points at away matches
home_vs_away_points[2,2]<-((home_vs_away[2,2])*3+(home_vs_away[3,2])*1)/sum(home_vs_away$n)
```

plotting the result

```
ggplot(data = home_vs_away_points,aes(x=where_to_play,y=Average_points))+
  geom_col()+
  ggtitle("Home Vs Away, average points")+
  xlab("where to play")+ylab("Average points")
```

![2](https://user-images.githubusercontent.com/41892582/191893919-6f75c8e2-b9f6-4da4-bad0-879dbd2d2544.png)

On average, the advantage is about 0.53 points, which is taken by the team who's playing on their home ground. It's about 10 points in a whole season.

Let's track this advantage through the years.

First, we will go to see how many points the teams have collected on their home ground over the years.

```
## new dataframe counting final results by every year
point_year<-PL_r%>%
  group_by(Season_End_Year)%>%
  count(FTR)

## calculating the points that collected in a home and away ground
point_year$points<-case_when(point_year$FTR=="H"~point_year$n*3,
                             point_year$FTR=="A"~point_year$n*3,
                             point_year$FTR=="D"~point_year$n*1
)

## creating a new column"h_points" that summation points that collected in home ground either "h_points" to away points
point_year2<-point_year%>%
  group_by(Season_End_Year)%>%
  summarize(h_points=points[FTR=="H"]+points[FTR=="D"],a_points=(points[FTR=="A"])+points[FTR=="D"])

## tidying the data
point_year3<-point_year2%>%
  pivot_longer(c(`h_points`, `a_points`), names_to = "Home_vs_Away", values_to = "Points")
```

plotting the result

```
ggplot(data = point_year3,aes(x=Season_End_Year,y=Points,col=Home_vs_Away))+
  geom_line()+
  theme(legend.title=element_blank())+
  theme(legend.position=c(0.9,0.9))+
  scale_color_manual(labels = c("Away", "Home"),
                     values = c( "red", "blue"))+
  ggtitle("Total points collected by the teams Home vs Away")+
  xlab("Season end year")+
  ylab("Total points")
```

![a](https://user-images.githubusercontent.com/41892582/192075470-33916bb1-29ba-4c2b-95b7-ddc71bcadc9a.png)

the teams collecting points in home ground more than away grounds through the years except for season that end in 2021.

**The home percentage winning through the years**
  ```
## new dataframe counting the Home, Away and Draw through the years
home_vs_away_years<-PL_r %>%
  group_by(Season_End_Year,FTR)%>%
  count(FTR)

##the percentage of every case
home_vs_away_years$Season_End_Year<-as.numeric(home_vs_away_years$Season_End_Year)

home_vs_away_years$percentage<- case_when(
  home_vs_away_years$Season_End_Year==1993~home_vs_away_years$n*100/462,
  home_vs_away_years$Season_End_Year==1994~home_vs_away_years$n*100/462,
  home_vs_away_years$Season_End_Year==1995~home_vs_away_years$n*100/462,
  TRUE ~ home_vs_away_years$n*100/380
)
```

plotting the Home winning percentage through the years

```
ggplot(data = home_vs_away_years,aes(x=Season_End_Year ,y=percentage))+
  geom_line(data=subset(home_vs_away_years,FTR=="H"),col="red")+
  ggtitle("Home winning percentage through the years")+
  xlab("Season end year")
```

![3](https://user-images.githubusercontent.com/41892582/191894603-a6c8eca9-c846-4df9-8122-5399639816af.png)


**observation**
  
  The winning percentage at home continued to more than 40% over the years before dropping significantly in 2021, before returning to normal. We will return to studying that case later in our analysis after seeing if it impacts scoring goals on the home ground stadium or not.


Now we will try to see What effect does the home field have on the number of goals scored? 
  
  ```
home_goals_vs_away_goals<-PL_r %>%
  group_by(Season_End_Year)%>%
  summarise(all_home_goals=sum(HomeGoals),all_away_goals=sum(AwayGoals))
## tidying the data
home_goals_vs_away_goals2<-home_goals_vs_away_goals%>%
  pivot_longer(c(`all_home_goals`, `all_away_goals`), names_to = "Home_vs_Away", values_to = "Goals")
```

plotting the all Home and Away goals through the years

```
ggplot(data = home_goals_vs_away_goals2,aes(x=Season_End_Year,y=Goals,col=Home_vs_Away))+
  geom_line()+
  theme(legend.title=element_blank())+
  theme(legend.position=c(0.9,0.9))+
  scale_color_manual(labels = c("Away", "Home"),
                     values = c( "red", "blue"))+
  ggtitle("Home vs Away goals")+
  xlab("Season end year")+
  ylab("Goals")
```

![4](https://user-images.githubusercontent.com/41892582/192028190-aecb84f5-b3dd-4d46-aece-dcbcfa42a68d.png)

Again, the 2021 season ended the obvious advantage of the team playing at home. 

**How many goals per match have been scored over the years, both at home and away**
  
  ```
## New data frame to calculate the goals per game through the years
home_goals_vs_away_goals_match<-home_goals_vs_away_goals
## Home
home_goals_vs_away_goals_match$HG_per_match<-case_when(
  home_goals_vs_away_goals$Season_End_Year==1993~home_goals_vs_away_goals_match$all_home_goals/462,
  home_goals_vs_away_goals$Season_End_Year==1994~home_goals_vs_away_goals_match$all_home_goals/462,
  home_goals_vs_away_goals$Season_End_Year==1995~home_goals_vs_away_goals_match$all_home_goals/462,
  TRUE ~ home_goals_vs_away_goals_match$all_home_goals/380)
## Away
home_goals_vs_away_goals_match$AG_per_match<-case_when(
  home_goals_vs_away_goals$Season_End_Year==1993~home_goals_vs_away_goals_match$all_away_goals/462,
  home_goals_vs_away_goals$Season_End_Year==1994~home_goals_vs_away_goals_match$all_away_goals/462,
  home_goals_vs_away_goals$Season_End_Year==1995~home_goals_vs_away_goals_match$all_away_goals/462,
  TRUE ~ home_goals_vs_away_goals_match$all_away_goals/380)
## tidying the data
home_goals_vs_away_goals_matc2<-home_goals_vs_away_goals_match%>%
  select(Season_End_Year,HG_per_match,AG_per_match)

home_goals_vs_away_goals_matc2<-home_goals_vs_away_goals_matc2%>%
  pivot_longer(c(`HG_per_match`, `AG_per_match`), names_to = "Home_vs_Away", values_to = "Goals_match")
```

plotting the Home vs Away goal per match through the years

```
ggplot(data = home_goals_vs_away_goals_matc2,aes(x=Season_End_Year,y=Goals_match,col=Home_vs_Away))+
  geom_line()+
  theme(legend.title=element_blank())+
  theme(legend.position=c(0.95,0.95))+
  scale_color_manual(labels = c("Away", "Home"),
                     values = c( "red", "blue"))+
  ggtitle("Home vs Away goals per match")+
  xlab("Season end year")+
  ylab("Goals")
```

![5](https://user-images.githubusercontent.com/41892582/192040176-d582d976-0617-440e-a948-66e5db08f004.png)

As we saw the home ground make a clear advantage through the years, in both point collecting and goals scoring, except for the year 2021. By looking deeper into this year, we found that it was the year of the COVID-19 pandemic, which prevented the crowd from attending the matches. 
We will be looking deeper into this period of time.

According to "premierleague.com", the last 92 matches of the season 2019-2020 were played behind closed doors. Season 2020–21 started with matches behind closed doors, and this remained largely the case throughout the season, apart from a brief period in limited areas in December 2020, and at all grounds in May 2021, when clubs were able to welcome a limited number of fans back into their stadiums. 
Let's study the effect of the absence of the crowd through the 2020–2021 season (season end year 2021).

```
## new dataframe with only results of 2020-2021 season
season_2021<-subset(PL_r,Season_End_Year=="2021")
```

Home vs Away winning in 2021 season

```
point_2021<-season_2021%>%
  group_by(Season_End_Year)%>%
  count(FTR)
```
plotting the result

```
ggplot(data =point_2021,aes(FTR,n))+
  geom_col()+
  ggtitle("Home vs Away winning in 2021 season")+
  ylab("Numbers of matches")+
  xlab("The Results")
  ```
  
![6](https://user-images.githubusercontent.com/41892582/192045456-7605a0d3-09eb-4ffa-8c58-9935f67e6a6f.png)

It's clear that home advantage is gone without the crowd's attendance.

Now we will make some team analysis, starting by looking for the team that won the most at its home stadium.

```  
## new dataframe to calculate the home results for each PL team
home_point<-PL_r %>%
  group_by(Home)%>%
  count(FTR)
  
## replacing "H" by "W" for winning, "A" by "L" for losing and "D" still "D" for draw
home_point$FTR[home_point$FTR=="H"]<-"W"
home_point$FTR[home_point$FTR=="A"]<-"L"

## calculating the points collected at home ground by each team
## as we know the winning team get three points, the losing team take nothing, and 1 point for each team during draw
home_point$points<- case_when(home_point$FTR=="L"~home_point$n*0,
                               home_point$FTR=="W"~home_point$n*3,
                               home_point$FTR=="D"~home_point$n*1
                               )
                               
home_point2<-home_point%>%
  group_by(Home)%>%
  summarize(T_point=sum(points))
```  

plotting the result

```
ggplot(data = home_point2,aes(x=T_point,y=reorder(Home,T_point),fill=Home))+
  geom_col()+
  ggtitle("Points by team in Home ground")+
  ylab("Team")+
  xlab("Points")+
  theme(legend.position="none")
```

![7](https://user-images.githubusercontent.com/41892582/192052506-5c9a294e-e7fa-45f4-9cf1-778dcea408d6.png)

As we see, Manchester United is the team with the most points collected in their home stadium, followed by Arsenal, Liverpool, and Chelsea.

Let's see the points collected on the home ground per match for every team.

```
## new dataframe to count how many matches played at home ground for each team
total_matches_at_home<-PL_r%>%
  group_by(Home)%>%
  count(Home)

## bind the number of matches played with the total points collected
home_point3<-cbind(home_point2,total_matches_at_home[,2])
home_point3<-home_point3%>%
  arrange(desc(n))

##calculating the average points per match for every team
home_point3$point_per_match<-(home_point3$T_point/home_point3$n)
home_point3<-arrange(home_point3,desc(point_per_match))
```

plotting the result

```
ggplot(data = home_point3,aes(x=point_per_match,y=reorder(Home,point_per_match),fill=Home))+
  geom_col()+
  ggtitle("Average points at Home ground for every team")+
  ylab("Team")+
  xlab("Points")+
  theme(legend.position="none")
```

![8](https://user-images.githubusercontent.com/41892582/192066385-181f34a1-9646-476f-afcf-cbacef1d1a3f.png)

As we see, Manchester United has the best record on its home ground by average points per match, also except for two teams the average point on home ground is more than 1 point.

Let's move to the other side and see the teams' performances individually in away matches.

```
## new dataframe to calculate the home results for each PL team
away_point<-PL_r %>%
  group_by(Away)%>%
  count(FTR)

## replacing "H" by "L" for losing, "A" by "W" for winning and "D" still "D" for draw
away_point$FTR[away_point$FTR=="H"]<-"L"
away_point$FTR[away_point$FTR=="A"]<-"W"

## calculating the points collected at away ground by each team
## as we know the winning team get three points, the losing team take nothing, and 1 point for each team during draw
away_point$points<- case_when(away_point$FTR=="L"~away_point$n*0,
                              away_point$FTR=="W"~away_point$n*3,
                              away_point$FTR=="D"~away_point$n*1
)

away_point2<-away_point%>%
  group_by(Away)%>%
  summarize(T_point=sum(points))
```

plotting the result

```
ggplot(data = away_point2,aes(x=T_point,y=reorder(Away,T_point),fill=Away))+
  geom_col()+
  ggtitle("Points by team in Away ground")+
  ylab("Team")+
  xlab("Points")+
  theme(legend.position="none")
```

![9](https://user-images.githubusercontent.com/41892582/192067608-694e718f-16a8-4a23-9546-dd3d81aa3314.png)

Manchester United is also in first place, followed by Chelsea, Arsenal, and Liverpool.

Now we're looking at the average points collected in away games.

```
## new dataframe to count how many matches played at away ground for each team
total_matches_at_away<-PL_r%>%
  group_by(Away)%>%
  count(Away)

## bind the number of matches played with the total points collected
away_point3<-cbind(away_point2,total_matches_at_away[,2])
away_point3<-away_point3%>%
  arrange(desc(n))

##calculating the average points per match for every team
away_point3$point_per_match<-(away_point3$T_point/away_point3$n)
away_point3<-arrange(away_point3,desc(point_per_match))
```

plotting the result

```
ggplot(data = away_point3,aes(x=point_per_match,y=reorder(Away,point_per_match),fill=Away))+
  geom_col()+
  ggtitle("Average points at Away ground for every team")+
  ylab("Team")+
  xlab("Points")+
  theme(legend.position="none")
```

![10](https://user-images.githubusercontent.com/41892582/192068318-0472e724-b99b-438a-a0cc-176200d55bf6.png)

Manchester United came first followed by Chelsea, Arsenal, and Liverpool, also as we notice the average point descending remarkably Compared to home ground for each team.

now we calculate the difference between the average home point and the average away points for every team.

```
## merging the two data that count the point at home and away ground that we used before
abs<-merge(x=home_point3,y=away_point3,by.x = "Home",by.y = "Away")
abs<-abs%>%
  select(Home,point_per_match.x,point_per_match.y)

## creating new column calculate the difference between the average home point and the average away points
abs$abs<-abs$point_per_match.x-abs$point_per_match.y

```
plotting the result

```
ggplot(data = away_point3,aes(x=point_per_match,y=reorder(Away,point_per_match),fill=Away))+
  geom_col()+
  ggtitle("Average points at Away ground for every team")+
  ylab("Team")+
  xlab("Points")+
  theme(legend.position="none")
```

![11](https://user-images.githubusercontent.com/41892582/192069799-1ef67305-4f57-41dc-83e7-4b2a25510f2c.png)

We can say that Plott demonstrates how tough the team is at home versus away. There is no team that has a negative difference between their home and away average, which means every team collects more points on the home ground than the away average.


now we will be going to see the average goals at the home ground by  each team

**Home goals scored**

```
## new dataframe to count how many goals scored by the team in Home ground
total_home_goals<-PL_r%>%
  group_by(Home)%>%
  summarize(all_home_goal=sum(HomeGoals))

## bind the new data frame with data that count the number of matches played at home ground for each team
total_home_goals2<-cbind(total_home_goals,total_matches_at_home[,2])

## creating a new column that calculate the average goals for each team
total_home_goals2$goal_per_match<-(total_home_goals2$all_home_goal/total_home_goals2$n)
total_home_goals2<-arrange(total_home_goals2,desc(goal_per_match))
```

plotting the result

```
ggplot(data = total_home_goals2,aes(x=goal_per_match,y=reorder(Home,goal_per_match),fill=Home))+
  geom_col()+
  ggtitle("Average goals at Home ground for every team")+
  ylab("Team")+
  xlab("Goals")+
  theme(legend.position="none")
```

![12](https://user-images.githubusercontent.com/41892582/192072909-0ace1dd9-1986-41d2-bc0f-3d297473b25e.png)

except for Huddersfield, every team scored on average more than 1 goal on home ground

let's see what about goals receiving on home ground

**Home goals received**
  
  ```
## new dataframe to count how many goals received by the team in Home ground
total_home_goals_received<-PL_r%>%
  group_by(Home)%>%
  summarize(all_home_goal_received=sum(AwayGoals))

## bind the new data frame with data that count the number of matches played at home ground for each team
total_home_goals_received2<-cbind(total_home_goals_received,total_matches_at_home[,2])

## creating a new column that calculate the average goals for each team
total_home_goals_received2$goal_per_match<-total_home_goals_received2$all_home_goal_received/total_home_goals_received2$n
total_home_goals_received2<-arrange(total_home_goals_received2,goal_per_match)
```
plotting the result

```
ggplot(data = total_home_goals_received2,aes(x=goal_per_match,y=reorder(Home,-goal_per_match),fill=Home))+
  geom_col()+
  ggtitle("Average goals received at Home ground for every team")+
  ylab("Team")+
  xlab("Goals")+
  theme(legend.position="none")
```

![13](https://user-images.githubusercontent.com/41892582/192073466-c3dac928-e6c2-43f8-a5a6-b3fea4213b92.png)

Except for Swindon Town, the average number of goals received by each team at their home ground is less than 2 goals.

What about playing outside? Let's see by average how many goals were scored and received by each team.

**Away goals scored**

```
## new dataframe to count how many goals scored by the team in Away ground
total_away_goals<-PL_r%>%
  group_by(Away)%>%
  summarize(all_away_goal=sum(AwayGoals))

## bind the new data frame with data that count the number of matches played at away ground for each team
total_away_goals2<-cbind(total_away_goals,total_matches_at_home[,2])
total_away_goals2$goal_per_match<-(total_away_goals2$all_away_goal/total_away_goals2$n)

## creating a new column that calculate the average goals for each team
total_away_goals2<-arrange(total_away_goals2,desc(goal_per_match))
```
plotting the result

```
ggplot(data = total_away_goals2,aes(x=goal_per_match,y=reorder(Away,goal_per_match),fill=Away))+
  geom_col()+
  ggtitle("Average goals scored at Away ground for every team")+
  ylab("Team")+
  xlab("Goals")+
  theme(legend.position="none")
```
![14](https://user-images.githubusercontent.com/41892582/192073987-26382628-fe39-4967-bfdf-1c6b02b9b473.png)

On average, every team scored less than 2 goals outside the home ground. More than that, except for Manchester United and Liverpool, every team scored less than 1.5 goals on average outside the home ground.

**Away goals received**

```
## new dataframe to count how many goals received by the team in Away ground
total_away_goals_received<-PL_r%>%
  group_by(Away)%>%
  summarize(all_away_goal_received=sum(HomeGoals))

## bind the new data frame with data that count the number of matches played at Away ground for each team
total_away_goals_received2<-cbind(total_away_goals_received,total_matches_at_home[,2])
total_away_goals_received2$goal_per_match<-total_away_goals_received2$all_away_goal_received/total_away_goals_received2$n

## creating a new column that calculate the average goals for each team
total_away_goals_received2<-arrange(total_away_goals_received2,goal_per_match)
```
plotting the result

```
ggplot(data = total_away_goals_received2,aes(x=goal_per_match,y=reorder(Away,-goal_per_match),fill=Away))+
  geom_col()+
  ggtitle("Average goals received at Away ground for every team")+
  ylab("Team")+
  xlab("Goals")+
  theme(legend.position="none")
```
![15](https://user-images.githubusercontent.com/41892582/192074001-505ccebf-ca17-4dee-a138-2e14db3cb895.png)

On average, every team receives more than one goal in a match played away from home. 

**Defense or offense?**

As a football fan, I can say that that debate is always on the surface between the fans. They always call that debate between romanticism and pragmatism. 
Here we will be trying to see which technique is better for collecting points. 
In many ways, we can judge whether the technique is defensive or offensive, but here in our data, we will be judging it by goals scored and goals received. 
In this section, we will see the number of goals scored and how many points on average the team will collect. We will also see the number of goals the team received and how many average points the team collected.

```
## to see how many Home teams scored by end of the match
table(PL_r$HomeGoals)
```

```
   0    1    2    3    4    5    6    7    8    9 
2737 3773 2822 1416  593  206   62   27    7    3 
```

```
## to see how many Away teams scored by end of the match
table(PL_r$AwayGoals)
```
```
   0    1    2    3    4    5    6    7    8    9 
3982 4040 2217  982  309   83   28    3    1    1 
```

both are scored in a range from 0 to 9

```
## creating a matrix with 4 variables
## variable for goals the team scored in a match "Goal_scored" and next to it the average point they got "points_s",
## and  variable for goals the team received in a match "Goal_received" and next to it the average point they got "points_A".
## and our observations the range from 0 to 9
matrix<-matrix(nrow = 10,ncol = 4)
colnames(matrix)=c("Goal_scored","points_s","Goal_received","points_A")
matrix[,1]<-c(0:9)
matrix[,3]<-c(0:9)

## creating a loop to calculate the average point when scoring a goals in a range from 0 to 9
for(i in 0:9) 
{
## in Home ground 
  home_score_i<-subset(PL_r,PL_r$HomeGoals==i)
  
  home_score_i$points_h_i<-case_when(home_score_i$FTR=="H"~3,
                                     home_score_i$FTR=="D"~1,
                                     home_score_i$FTR=="A"~0
                                     )
  all_points_gains_at_home_when_scored_i<-sum(home_score_i$points_h_i)
 
 ## in away grounds
  away_score_i<-subset(PL_r,PL_r$AwayGoals==i)
  
  away_score_i$points_a_i<-case_when(away_score_i$FTR=="H"~0,
                                     away_score_i$FTR=="D"~1,
                                     away_score_i$FTR=="A"~3
                                     )
  all_points_gains_at_away_when_scored_i<-sum(away_score_i$points_a_i)
  
  all_points_gains_when_scored_i<-all_points_gains_at_away_when_scored_i+all_points_gains_at_home_when_scored_i
  
  average_points_when_scored_i<-all_points_gains_when_scored_i/(nrow(home_score_i)+nrow(away_score_i))
  
  matrix[i+1,2]<-(average_points_when_scored_i)
}

## creating a loop to calculate the average point when receiving a goals in a range from 0 to 9
for(j in 0:9)
  
{
## in Home ground
  home_against_j<-subset(PL_r,PL_r$AwayGoals==j)
  home_against_j$points_h_j<-case_when(home_against_j$FTR=="H"~3,
                                       home_against_j$FTR=="D"~1,
                                       home_against_j$FTR=="A"~0
                                       )
  all_points_gains_at_home_when_against_j<-sum(home_against_j$points_h_j)
  
  ## in Away grounds
  away_against_j<-subset(PL_r,PL_r$HomeGoals==j)
  away_against_j$points_a_j<-case_when(away_against_j$FTR=="H"~0,
                                       away_against_j$FTR=="D"~1,
                                       away_against_j$FTR=="A"~3
                                       )
  all_points_gains_at_away_when_against_j<-sum(away_against_j$points_a_j)
  
  all_points_gains_when_against_j<-all_points_gains_at_away_when_against_j+all_points_gains_at_home_when_against_j
  
  average_points_when_against_j<-all_points_gains_when_against_j/(nrow(home_against_j)+nrow(away_against_j))
  
  matrix[j+1,4]<-(average_points_when_against_j)
  
}

goals_points<-as.data.frame(matrix)

```

```
head(goals_points,10)

```

```
   Goal_scored  points_s Goal_received    points_A
1            0 0.2842685             0 2.431463015
2            1 1.1494944             1 1.505439652
3            2 2.1395118             2 0.635046636
4            3 2.6567973             3 0.239783153
5            4 2.9113082             4 0.057649667
6            5 2.9861592             5 0.006920415
7            6 3.0000000             6 0.000000000
8            7 3.0000000             7 0.000000000
9            8 3.0000000             8 0.000000000
10           9 3.0000000             9 0.000000000

```
```
## tidying and plotting the data

goals_points2<-goals_points%>%
  pivot_longer(c(`points_s`, `points_A`), names_to = "scored_vs_against", values_to = "Points")

ggplot(data = goals_points2,aes(x=Goal_scored,y=Points,col=scored_vs_against))+
  geom_line()+
  theme(legend.title=element_blank())+
  theme(legend.position=c(0.05,0.94))+
  scale_color_manual(labels = c("against", "scored"),
  values = c( "red", "blue"))+
  ggtitle("average points when the teams scored vs received a goals")+
  xlab("Goals")+
  ylab("points")
```

![16](https://user-images.githubusercontent.com/41892582/192079087-f73b31c0-d04b-4a71-83db-6cdeb4e73f08.png)


The result is kind of surprising to me, as we see that, on average, the team that has a clean sheet collects 2.43 points. It's more than the team who scores 2 goals, which collects on average 2.13 points. 
And the team that received just one goal collected on average more than the team that scored just one goal.


**generating a PL tables through the years**
  
  ```
## creating a list that will contain every PL table
PL_Table<-list()

## creating a loop to deal with the data year by year
for(i in 1993:2022)
{
  ## creating subset contain every year results
  PL_r_i<-subset(PL_r,PL_r$Season_End_Year==i)
  
  ## count of home goals, scored and against
  
  PL_r_hgi<-PL_r_i%>%
    group_by(Home)%>%
    summarize(goal_scored_at_home=sum(HomeGoals),
              goal_against_at_home=sum(AwayGoals))
  
  ## count of away goals, scored and against
  PL_r_agi<-PL_r_i%>%
    group_by(Away)%>%
    summarize(goal_scored_at_away=sum(AwayGoals),
              goal_against_at_away=sum(HomeGoals))
  
  ##home_and_away_goals
  
  goals_i<-cbind(PL_r_hgi,PL_r_agi)
  goals_i$GS<-(goals_i$goal_scored_at_away)+(goals_i$goal_scored_at_home)
  goals_i$GA<-(goals_i$goal_against_at_away)+(goals_i$goal_against_at_home)
  goals_i$GD<-(goals_i$GS)-(goals_i$GA)
  
  goals2_i<-goals_i%>%
    select(Home,GS,GA,GD)
  
  ##home result
  PL_r_hri<-PL_r_i%>%
    group_by(Home)%>%
    count(FTR)
  
  PL_r_hri$W<-case_when(PL_r_hri$FTR=="H"~PL_r_hri$n*1,
                        PL_r_hri$FTR=="D"~0,
                        PL_r_hri$FTR=="A"~0
  )
  
  PL_r_hri$D<-case_when(PL_r_hri$FTR=="H"~0,
                        PL_r_hri$FTR=="D"~PL_r_hri$n*1,
                        PL_r_hri$FTR=="A"~0
  )
  
  PL_r_hri$L<-case_when(PL_r_hri$FTR=="H"~0,
                        PL_r_hri$FTR=="D"~0,
                        PL_r_hri$FTR=="A"~PL_r_hri$n*1
  )
  
  PL_r2_hri<-PL_r_hri%>%
    group_by(Home)%>%
    summarize(Wh=sum(W),Dh=sum(D),Lh=sum(L))
  
  PL_r2_hri$hpoints<-(PL_r2_hri$Wh*3)+(PL_r2_hri$Dh*1)
  
  ##away result
  PL_r_ari<-PL_r_i%>%
    group_by(Away)%>%
    count(FTR)
  
  PL_r_ari$W<-case_when(PL_r_ari$FTR=="H"~0,
                        PL_r_ari$FTR=="D"~0,
                        PL_r_ari$FTR=="A"~PL_r_ari$n*1
  )
  
  PL_r_ari$D<-case_when(PL_r_ari$FTR=="H"~0,
                        PL_r_ari$FTR=="D"~PL_r_ari$n*1,
                        PL_r_ari$FTR=="A"~0
  )
  
  PL_r_ari$L<-case_when(PL_r_ari$FTR=="H"~PL_r_ari$n*1,
                        PL_r_ari$FTR=="D"~0,
                        PL_r_ari$FTR=="A"~0
  )
  
  PL_r2_ari<-PL_r_ari%>%
    group_by(Away)%>%
    summarize(Wa=sum(W),Da=sum(D),La=sum(L))
  
  PL_r2_ari$apoints<-(PL_r2_ari$Wa*3)+(PL_r2_ari$Da*1)
  
  ##home and away points
  
  points_i<-cbind(PL_r2_ari,PL_r2_hri)
  
  points_i$W<-(points_i$Wa)+(points_i$Wh)
  points_i$D<-(points_i$Da)+(points_i$Dh)
  points_i$L<-(points_i$La)+(points_i$Lh)
  points_i$points<-(points_i$apoints)+(points_i$hpoints)
  
  points2_i<-points_i%>%
    select(Away,W,D,L,points)
  
  Table_i<-cbind(goals2_i,points2_i)
  
  Table2_i<-Table_i%>%
    select(Home,W,D,L,GS,GA,GD,points)
  
  Table3_i<-arrange(Table2_i,desc(points),desc(GD),desc(GS))
  
  names(Table3_i)[names(Table3_i) == 'Home'] <- 'Team'
  
  Table3_i$Season<-paste0(i-1,"/",i)
  Table3_i$Rank<-1:case_when(i==1993~22,
                             i==1994~22,
                             i==1995~22,
                             TRUE ~ 20)
  
  Table3_i<-Table3_i[,c(9,10,1,2,3,4,5,6,7,8)]
  
  
  PL_Table[[i]]<-Table3_i
}
```

testing the result

```
PL_Table[[2017]]
```

```
Season Rank            Team  W  D  L GS GA  GD points
1  2016/2017    1         Chelsea 30  3  5 85 33  52     93
2  2016/2017    2       Tottenham 26  8  4 86 26  60     86
3  2016/2017    3 Manchester City 23  9  6 80 39  41     78
4  2016/2017    4       Liverpool 22 10  6 78 42  36     76
5  2016/2017    5         Arsenal 23  6  9 77 44  33     75
6  2016/2017    6  Manchester Utd 18 15  5 54 29  25     69
7  2016/2017    7         Everton 17 10 11 62 44  18     61
8  2016/2017    8     Southampton 12 10 16 41 48  -7     46
9  2016/2017    9     Bournemouth 12 10 16 55 67 -12     46
10 2016/2017   10       West Brom 12  9 17 43 51  -8     45
11 2016/2017   11        West Ham 12  9 17 47 64 -17     45
12 2016/2017   12  Leicester City 12  8 18 48 63 -15     44
13 2016/2017   13      Stoke City 11 11 16 41 56 -15     44
14 2016/2017   14  Crystal Palace 12  5 21 50 63 -13     41
15 2016/2017   15    Swansea City 12  5 21 45 70 -25     41
16 2016/2017   16         Burnley 11  7 20 39 55 -16     40
17 2016/2017   17         Watford 11  7 20 40 68 -28     40
18 2016/2017   18       Hull City  9  7 22 37 80 -43     34
19 2016/2017   19   Middlesbrough  5 13 20 27 53 -26     28
20 2016/2017   20      Sunderland  6  6 26 29 69 -40     24
```

**how many points collecting by the winner of the PL through the years**
  
  ```
winner_points<-matrix(nrow = 30,ncol = 2)
colnames(winner_points)=c("Season_End_Year","winner_points")
winner_points<-as.data.frame(winner_points)
winner_points$Season_End_Year<-1993:2022
for (i in 1993:2022)
{  
  wi<-PL_Table[[i]]$points[PL_Table[[i]]$Rank==1]
  
  winner_points[i-1992,2]<-wi
}
```

plotting the result

```
ggplot(data=winner_points)+
  aes(x=Season_End_Year,y=winner_points)+
  geom_line()+
  ggtitle("points of winners of the PL")+
  xlab("Season End Year")+
  ylab("Points")
```

![17](https://user-images.githubusercontent.com/41892582/192080109-2ee8d49d-7857-420a-9ad9-7c9edae1788a.png)

no obvious trend for how many points are needed to win the PL through the years, 75 is the least points collected by the winner of the PL while 100 points are the most.

how many points collecting by the runner-up of the PL through the years?
  
  ```
runner_up_points<-matrix(nrow = 30,ncol = 2)
colnames(runner_up_points)=c("Season_End_Year","runner_up_points")
runner_up_points<-as.data.frame(runner_up_points)
runner_up_points$Season_End_Year<-1993:2022
for (j in 1993:2022)
{  
  rj<-PL_Table[[j]]$points[PL_Table[[j]]$Rank==2]
  
  runner_up_points[j-1992,2]<-rj
}
```
plotting the result

```
ggplot(data=runner_up_points)+
  aes(x=Season_End_Year,y=runner_up_points)+
  geom_line()+
  ggtitle("points of runner up of the PL")+
  xlab("Season End Year")+
  ylab("Points")
```

![18](https://user-images.githubusercontent.com/41892582/192080345-c852e016-62e2-472a-b562-7ee56dee903d.png)

As we saw, the runners-up collected more points than the PL champions in another year, which leads us to wonder about the competitiveness of the PL teams over time, and whether it is increasing or decreasing. 
Of course, there is no obvious way to measure competitiveness, but we are going to use one of the statistical dispersion measures to see the variance between teams during the years. 
We are going to use standard deviation, which, according to "Wikipedia"  is the measure of the amount of variation or dispersion of a set of values, A low standard deviation indicates that the values tend to be close to the mean (also called the expected value) of the set, while a high standard deviation indicates that the values are spread out over a wider range. 
That is to say, the low standard deviation of the year tends to be more competitive than the higher standard deviation.

**The standard deviation between all the points collected by the teams in the PL over the years.**
  
  ```
sd<-matrix(nrow = 30,ncol = 2)
sd<-as.data.frame(sd)
names(sd)<-c("Season_End_Year","sd")
for(j in 1993:2022)
{
  sd[j-1992,2]<-sd(PL_Table[[j]]$points)
  sd[j-1992,1]<-paste0(j) 
}

ggplot(data = sd,aes(x=Season_End_Year,y=sd))+
  geom_col()+
  coord_flip()+
  ggtitle("standard deviation between  the teams")+
  ylab("Season End Year")

```

![19](https://user-images.githubusercontent.com/41892582/192080859-c9218b38-3f7c-4260-9084-6510d3579ea6.png)

We didn't see a trend here, and we cannot expect to see one because all the teams can not be close to each other, it must be superior teams, teams that face relegation, and teams in between them. Now we will try to break it down. We will see every 4 Ranks at a time.

Now we will try to separate the table into five groups and see the competitiveness during the years.

**1-4 Ranks**

```
sd1<-matrix(nrow = 30,ncol = 2)
sd1<-as.data.frame(sd1)
names(sd1)<-c("Season_End_Year","sd")
for(j in 1993:2022)
{
  sd1[j-1992,2]<-sd(PL_Table[[j]]$points[PL_Table[[j]]$Rank<=4])
  sd1[j-1992,1]<-paste0(j) 
}
ggplot(data = sd1,aes(x=Season_End_Year,y=sd))+
geom_col()+
coord_flip()+
ggtitle("standard deviation between  the teams in the first fourth places")+
xlab("Season End Year")
```

![20](https://user-images.githubusercontent.com/41892582/192081446-ad547fd4-381f-4133-998c-a952d667d309.png)

**5-8 Ranks**

```
sd2<-matrix(nrow = 30,ncol = 2)
sd2<-as.data.frame(sd2)
names(sd2)<-c("Season_End_Year","sd")
for(j in 1993:2022)
{
  sd2[j-1992,2]<-sd(PL_Table[[j]]$points[PL_Table[[j]]$Rank>=5&PL_Table[[j]]$Rank<=8])
  sd2[j-1992,1]<-paste0(j) 
}
ggplot(data = sd2,aes(x=Season_End_Year,y=sd))+
geom_col()+
coord_flip()+
ggtitle("standard deviation between the teams in the ranked between 5-8 ")+
xlab("Season End Year")
```

![21](https://user-images.githubusercontent.com/41892582/192081584-44731117-1fc2-45d7-a06f-25932fd370f3.png)

**9-12 Ranks**

```
sd3<-matrix(nrow = 30,ncol = 2)
sd3<-as.data.frame(sd3)
names(sd3)<-c("Season_End_Year","sd")
for(j in 1993:2022)
{
  sd3[j-1992,2]<-sd(PL_Table[[j]]$points[PL_Table[[j]]$Rank>=9&PL_Table[[j]]$Rank<=12])
  sd3[j-1992,1]<-paste0(j) 
}
ggplot(data = sd3,aes(x=Season_End_Year,y=sd))+
geom_col()+
coord_flip()+
ggtitle("standard deviation between the teams in the ranked between 9-12 ")+
xlab("Season End Year")
```

![22](https://user-images.githubusercontent.com/41892582/192081662-d5a2bad9-6866-44a0-a816-a5aa94eca43e.png)

**13-16 Ranks**

```
sd4<-matrix(nrow = 30,ncol = 2)
sd4<-as.data.frame(sd4)
names(sd4)<-c("Season_End_Year","sd")
for(j in 1993:2022)
{
  sd4[j-1992,2]<-sd(PL_Table[[j]]$points[PL_Table[[j]]$Rank>=13&PL_Table[[j]]$Rank<=16])
  sd4[j-1992,1]<-paste0(j) 
}
ggplot(data = sd4,aes(x=Season_End_Year,y=sd))+
geom_col()+
coord_flip()+
ggtitle("standard deviation between the teams in the ranked between 13-16 ")+
xlab("Season End Year")
```

![23](https://user-images.githubusercontent.com/41892582/192081765-8841aae5-f086-43db-8410-06e3894a6aec.png)

**17-20 Ranks**

```
sd5<-matrix(nrow = 30,ncol = 2)
sd5<-as.data.frame(sd5)
names(sd5)<-c("Season_End_Year","sd")
for(j in 1993:2022)
{
  sd5[j-1992,2]<-sd(PL_Table[[j]]$points[PL_Table[[j]]$Rank>=17&PL_Table[[j]]$Rank<=20])
  sd5[j-1992,1]<-paste0(j) 
}
ggplot(data = sd5,aes(x=Season_End_Year,y=sd))+
geom_col()+
coord_flip()+
ggtitle("standard deviation between the teams in the ranked between 17-20")+
xlab("Season End Year")
```

![24](https://user-images.githubusercontent.com/41892582/192081834-d31029f4-8915-4be3-9719-f179a777f6de.png)

We can't say there is a trend that describes competitiveness through the years, but as we can see, competitiveness oscillates up and down through the years. The most competitive region is the region with teams whose rankings are "13_16" and "9_12".

**winners of PL**
  
  ```
winners_of_pl<-matrix(nrow = 30,ncol = 2)
winners_of_pl<-as.data.frame(winners_of_pl)
names(winners_of_pl)=c("Season_End_Year","winner")
for(i in 1993:2022)
{
  winners_of_pl[i-1992,1]<-paste0(i)
  winner_i<-PL_Table[[i]]$Team[PL_Table[[i]]$Rank==1]
  winners_of_pl[i-1992,2]<-winner_i
}

n_winners_of_pl<-winners_of_pl%>%
  group_by(winner)%>%
  count(winner)%>%
  arrange(desc(n))

## plotting the result  
ggplot(data = n_winners_of_pl,aes(x=n,y=reorder(winner,n),fill=winner))+
  geom_col()+
  ggtitle("Winners of PL")+
  ylab("Team")+
  xlab("Numbers of winning times")+
  theme(legend.position="none")
```

![25](https://user-images.githubusercontent.com/41892582/192082705-92b2f97b-feb8-4348-9c83-702770f8bb0e.png)

It's obvious that Manchester United dominates the most titles and trophies in the PL, and the last one of them was 10 years ago. 
While searching, we discovered the most common thing in Manchester United during their PL dominance was coaching under Sir Alex Ferguson, which led us to inquire about the difference between Manchester United under Sir Alex Ferguson's coaching and Manchester United after him, which also led us to inquire about coaches in the PL and try to answer the question of who is the best coach in PL history.
In our analysis, we will only analyze the coaches who have won the PL twice or more.

**Sir Alex Ferguson's period at Manchester United**

![8ef905d7ed389e2b0cd7f1689092bcb781ea27da](https://user-images.githubusercontent.com/41892582/192768292-2547f700-42a4-4bbd-8990-33468d6d6f77.png)

according to Wikipedia, Sir Alex Ferguson coached the football team in Manchester from 1986 to the season ending in 2013.
which is mean we are going to analyze the results of Manchester United from the beginning of the PL till the end of season 2012-2013.

```
## new subset with home results of Manchester United during the period of Sir Alex Ferguson
sir_home<-subset(PL_r,Home=="Manchester Utd"&Season_End_Year<=2013)%>%
  arrange(Season_End_Year,Wk)

## new subset with away results of Manchester United during the period of Sir Alex Ferguson
sir_away<-subset(PL_r,Away=="Manchester Utd"&Season_End_Year<=2013)%>%
  arrange(Season_End_Year,Wk)
  
##sir points at home
sir_home_point<-sir_home%>%
  group_by(FTR)%>%
  count(FTR)
sir_home_point$points<-case_when(sir_home_point$FTR=="H"~sir_home_point$n*3,
                                 sir_home_point$FTR=="D"~sir_home_point$n*1,
                                 sir_home_point$FTR=="A"~sir_home_point$n*0
                                 )
## sir points away home ground
sir_away_point<-sir_away%>%
  group_by(FTR)%>%
  count(FTR)
sir_away_point$points<-case_when(sir_away_point$FTR=="H"~sir_away_point$n*0,
                                 sir_away_point$FTR=="D"~sir_away_point$n*1,
                                 sir_away_point$FTR=="A"~sir_away_point$n*3
                                 )
## sir goals

##home
sir_goals_home<-sir_home%>%
  group_by(Home)%>%
  summarize(all_home_goal=sum(sir_home$HomeGoals),all_home_goal_against=sum(sir_home$AwayGoals))
  
##away
sir_goals_away<-sir_away%>%
  group_by(Away)%>%
  summarize(all_away_goal=sum(sir_away$AwayGoals),all_away_goal_against=sum(sir_away$HomeGoals)) 
 
```
now we can build a profile of Sir Alex Ferguson to compare with the other coaches and Manchester United after him
let's do the same with other coaches they won the PL twice or more

**Pep Guardiola**
  
  ![pep-guardiola-profile](https://user-images.githubusercontent.com/41892582/192768183-f18564ee-cd5d-4594-ac88-4b217149307d.png)


according to Wikipedia, Pep began his mission as a coach of Manchester City at the beginning of the season that end in 2017 till now.

```
## new subset with home results of Manchester City during the period of Pep Guardiola
pep_home<-subset(PL_r,Home=="Manchester City"&Season_End_Year>=2017)%>%
  arrange(Season_End_Year,Wk)

## new subset with away results of Manchester City during the period of Pep Guardiola
pep_away<-subset(PL_r,Away=="Manchester City"&Season_End_Year>=2017)%>%
  arrange(Season_End_Year,Wk)

## Pep points at home ground
pep_home_point<-pep_home%>%
  group_by(FTR)%>%
  count(FTR)
pep_home_point$points<-case_when(pep_home_point$FTR=="H"~pep_home_point$n*3,
                                 pep_home_point$FTR=="D"~pep_home_point$n*1,
                                 pep_home_point$FTR=="A"~pep_home_point$n*0
)

## Pep points at away grounds
pep_away_point<-pep_away%>%
  group_by(FTR)%>%
  count(FTR)
pep_away_point$points<-case_when(pep_away_point$FTR=="A"~pep_away_point$n*3,
                                 pep_away_point$FTR=="H"~pep_away_point$n*0,
                                 pep_away_point$FTR=="D"~pep_away_point$n*1
)

## pep goals:

##home
pep_goals_home<-pep_home%>%
  group_by(Home)%>%
  summarize(all_home_goal=sum(pep_home$HomeGoals),all_home_goal_against=sum(pep_home$AwayGoals))

##away
pep_goals_away<-pep_away%>%
  group_by(Away)%>%
  summarize(all_away_goal=sum(pep_away$AwayGoals),all_away_goal_against=sum(pep_away$HomeGoals))
```

**Arsene Wenger**
  
  ![arsen](https://user-images.githubusercontent.com/41892582/192767819-922bfb5f-1eeb-480a-9ab8-767f47bd5e0a.jpg)


The Arsenal coach from the game week 9 at the season end year 1997, till the end of the 2018 season

```
## new subset with home results of Arsenal during the period of Arsene Wenger
arsen_home<-subset(PL_r,Home=="Arsenal"&Season_End_Year>=1997&Season_End_Year<=2018)%>%
  arrange(Season_End_Year,Wk)
arsen_home<-arsen_home[-c(1,2,3,4),]

## new subset with away results of Arsenal during the period of Arsene Wenger
arsen_away<-subset(PL_r,Away=="Arsenal"&Season_End_Year>=1997&Season_End_Year<=2018)%>%
  arrange(Season_End_Year,Wk)
arsen_away<-arsen_away[-c(1,2,3,4),]

##arsen points at home ground
arsen_home_point<-arsen_home%>%
  group_by(FTR)%>%
  count(FTR)
arsen_home_point$points<-case_when(arsen_home_point$FTR=="H"~arsen_home_point$n*3,
                                   arsen_home_point$FTR=="D"~arsen_home_point$n*1,
                                   arsen_home_point$FTR=="A"~arsen_home_point$n*0
)

## arsen points at away grounds
arsen_away_point<-arsen_away%>%
  group_by(FTR)%>%
  count(FTR)
arsen_away_point$points<-case_when(arsen_away_point$FTR=="A"~arsen_away_point$n*3,
                                   arsen_away_point$FTR=="H"~arsen_away_point$n*0,
                                   arsen_away_point$FTR=="D"~arsen_away_point$n*1
)

## arsen goals:

##home
arsen_goals_home<-arsen_home%>%
  group_by(Home)%>%
  summarize(all_home_goal=sum(arsen_home$HomeGoals),all_home_goal_against=sum(arsen_home$AwayGoals))

##away
arsen_goals_away<-arsen_away%>%
  group_by(Away)%>%
  summarize(all_away_goal=sum(arsen_away$AwayGoals),all_away_goal_against=sum(arsen_away$HomeGoals))
```

**Jose Mourinho**
  
  ![jose](https://user-images.githubusercontent.com/41892582/192767928-f0d264ae-de9c-4558-b6e2-20a1e8071bcd.jpg)

according to Wikipedia, Jose managed 3 teams in PL, in our analysis, we will consider just his two periods at Chelsea, it's the only team he won the PL with.

Mourinho's first time at Chelsea
```
## new subset with home results of Chelsea during the first period of Jose Mourinho
jose1_home<-subset(PL_r,Home=="Chelsea"& Date>="2004-08-14"&Date<="2007-09-15")%>%
  arrange(Season_End_Year,Wk)

## new subset with away results of Chelsea during the first period of Jose Mourinho
jose1_away<-subset(PL_r,Away=="Chelsea"& Date>="2004-08-14"&Date<="2007-09-15")%>%
  arrange(Season_End_Year,Wk)

##jose points at home ground
jose1_home_point<-jose1_home%>%
  group_by(FTR)%>%
  count(FTR)
jose1_home_point$points<-case_when(jose1_home_point$FTR=="H"~jose1_home_point$n*3,
                                   jose1_home_point$FTR=="D"~jose1_home_point$n*1,
                                   jose1_home_point$FTR=="A"~jose1_home_point$n*0
)

##jose points at away grounds
jose1_away_point<-jose1_away%>%
  group_by(FTR)%>%
  count(FTR)
jose1_away_point$points<-case_when(jose1_away_point$FTR=="A"~jose1_away_point$n*3,
                                   jose1_away_point$FTR=="H"~jose1_away_point$n*0,
                                   jose1_away_point$FTR=="D"~jose1_away_point$n*1
)

## jose goals:

##home
jose1_goals_home<-jose1_home%>%
  group_by(Home)%>%
  summarize(all_home_goal=sum(jose1_home$HomeGoals),all_home_goal_against=sum(jose1_home$AwayGoals))

##away
jose1_goals_away<-jose1_away%>%
  group_by(Away)%>%
  summarize(all_away_goal=sum(jose1_away$AwayGoals),all_away_goal_against=sum(jose1_away$HomeGoals))
```

**Mourinho's second time at Chelsea**
```
## new subset with home results of Chelsea during the second period of Jose Mourinho
jose2_home<-subset(PL_r,Home=="Chelsea"& Date>="2013-08-17"&Date<="2015-12-14")%>%
  arrange(Season_End_Year,Wk)

## new subset with away results of Chelsea during the second period of Jose Mourinho
jose2_away<-subset(PL_r,Away=="Chelsea"& Date>="2013-08-17"&Date<="2015-12-14")%>%
  arrange(Season_End_Year,Wk)

##jose points at home ground
jose2_home_point<-jose2_home%>%
  group_by(FTR)%>%
  count(FTR)
jose2_home_point$points<-case_when(jose2_home_point$FTR=="H"~jose2_home_point$n*3,
                                   jose2_home_point$FTR=="D"~jose2_home_point$n*1,
                                   jose2_home_point$FTR=="A"~jose2_home_point$n*0
                                    )

##jose points at away grounds
jose2_away_point<-jose2_away%>%
  group_by(FTR)%>%
  count(FTR)
jose2_away_point$points<-case_when(jose2_away_point$FTR=="A"~jose2_away_point$n*3,
                                   jose2_away_point$FTR=="H"~jose2_away_point$n*0,
                                   jose2_away_point$FTR=="D"~jose2_away_point$n*1
                                    )

## jose goals:

##home
jose2_goals_home<-jose2_home%>%
  group_by(Home)%>%
  summarize(all_home_goal=sum(jose2_home$HomeGoals),all_home_goal_against=sum(jose2_home$AwayGoals))

##away
jose2_goals_away<-jose2_away%>%
  group_by(Away)%>%
  summarize(all_away_goal=sum(jose2_away$AwayGoals),all_away_goal_against=sum(jose2_away$HomeGoals))
```  

**Manchester United after Sir Alex Ferguson**

The last case we will going to study is Manchester United after Sir Alex Ferguson, we will going to study it as a whole unit

```
## new subset with home results of Manchester United after Sir Alex Ferguson period
without_sir_home<-subset(PL_r,Home=="Manchester Utd"&Season_End_Year>2013)%>%
  arrange(Season_End_Year,Wk)

## new subset with away results of Manchester United after Sir Alex Ferguson period
without_sir_away<-subset(PL_r,Away=="Manchester Utd"&Season_End_Year>2013)%>%
  arrange(Season_End_Year,Wk)

##without sir points at home ground
without_sir_home_point<-without_sir_home%>%
  group_by(FTR)%>%
  count(FTR)
without_sir_home_point$points<-case_when(without_sir_home_point$FTR=="H"~without_sir_home_point$n*3,
                                         without_sir_home_point$FTR=="D"~without_sir_home_point$n*1,
                                         without_sir_home_point$FTR=="A"~without_sir_home_point$n*0
                                         )

##without sir points at away grounds
without_sir_away_point<-without_sir_away%>%
  group_by(FTR)%>%
  count(FTR)
without_sir_away_point$points<-case_when(without_sir_away_point$FTR=="H"~without_sir_away_point$n*0,
                                 without_sir_away_point$FTR=="D"~without_sir_away_point$n*1,
                                 without_sir_away_point$FTR=="A"~without_sir_away_point$n*3
                                         )

##without sir goals :

##home
without_sir_goals_home<-without_sir_home%>%
  group_by(Home)%>%
  summarize(all_home_goal=sum(without_sir_home$HomeGoals),all_home_goal_against=sum(without_sir_home$AwayGoals))
  
##away
without_sir_goals_away<-without_sir_away%>%
  group_by(Away)%>%
  summarize(all_away_goal=sum(without_sir_away$AwayGoals),all_away_goal_against=sum(without_sir_away$HomeGoals))
```

Now we are going to make a comparison between those coaches, and the fields of comparison are average points per match on home ground, average points per match on away grounds, average points per match at all, average goals per match on home ground, average goals per match on away grounds, average goals per match at all, average goals conceded per match on home ground, average goals conceded per match on away grounds, and average goals conceded per match at all.

```
## new dataframe to compare profiles of coaches
coaches_profiles<-matrix(nrow = 5,ncol = 10)
coaches_profiles<-as.data.frame(coaches_profiles)

## assign the names of columns
names(coaches_profiles)=c("coaches","point_home","point_away","point_all","goal_home","goal_away",
                          "goal_all","goal_against_home","goal_against_away","goal_against_all")
coaches_profiles$coaches<-c("sir","pep","arsen","jose1","jose2")

## for loop, to assign the values of every coaches profile from previous dataframes
coaches<-c("sir","arsen","jose1","jose2","pep","without_sir")
for(i in coaches)
  {
    eval(parse(text =paste0("coaches_profiles[coaches_profiles$coaches==","'",i,"'",",2]",
                            "<-sum(",i,"_home_point$points)/sum(",i,"_home_point$n)")))
    eval(parse(text =paste0("coaches_profiles[coaches_profiles$coaches==","'",i,"'",",3]",
                            "<-sum(",i,"_away_point$points)/sum(",i,"_away_point$n)")))
    eval(parse(text =paste0("coaches_profiles[coaches_profiles$coaches==","'",i,"'",",4]",
                            "<-(sum(",i,"_away_point$points)+sum(",i,"_home_point$points))/(sum(",i,"_away_point$n)+sum(",i,"_home_point$n))")))
    eval(parse(text =paste0("coaches_profiles[coaches_profiles$coaches==","'",i,"'",",5]",
                            "<-",i,"_goals_home$all_home_goal/sum(",i,"_home_point$n)")))
    eval(parse(text =paste0("coaches_profiles[coaches_profiles$coaches==","'",i,"'",",6]","<-",
                            i,"_goals_away$all_away_goal/sum(",i,"_away_point$n)")))
    eval(parse(text =paste0("coaches_profiles[coaches_profiles$coaches==","'",i,"'",",7]","<-(",
                            i,"_goals_home$all_home_goal+",i,"_goals_away$all_away_goal)/(sum(",i,"_home_point$n)+sum(",i,"_away_point$n))")))
    eval(parse(text =paste0("coaches_profiles[coaches_profiles$coaches==","'",i,"'",",8]","<-",
                            i,"_goals_home$all_home_goal_against/sum(",i,"_home_point$n)")))
    eval(parse(text =paste0("coaches_profiles[coaches_profiles$coaches==","'",i,"'",",9]","<-",
                            i,"_goals_away$all_away_goal_against/sum(",i,"_away_point$n)")))
    eval(parse(text =paste0("coaches_profiles[coaches_profiles$coaches==","'",i,"'",",10]","<-(",
                            i,"_goals_home$all_home_goal_against+",i,"_goals_away$all_away_goal_against)/(sum(",i,"_home_point$n)+sum(",i,"_away_point$n))")))
  }
```

**Home average points for each coach** 

```
ggplot(data = coaches_profiles,aes(y=reorder(coaches,point_home),x=point_home,fill=coaches))+
  geom_col()+
  ggtitle("Average points at home for each coach")+
  ylab("Coaches")+
  xlab("Points")+
  theme(legend.position="none")
```

![a1](https://user-images.githubusercontent.com/41892582/192694715-654e9744-3b50-4907-adaa-5b82c2aa4cf7.png)

As we saw, Jose Mourinho, in his first period at Chelsea, collected the most points per match on home ground, with more than 2.5 points per match.

**Average away points for each coach** 

```
ggplot(data = coaches_profiles,aes(y=reorder(coaches,point_away),x=point_away,fill=coaches))+
  geom_col()+
  ggtitle("Average points at away for each coach")+
  ylab("Coaches")+
  xlab("Points")+
  theme(legend.position="none")
```

![a2](https://user-images.githubusercontent.com/41892582/192695977-b1c20bdd-0dd5-4461-8778-2608b3bafbfc.png)

with 2.2 points per match, Pep Guardiola is the most collecting points per match on away grounds

**average points per match for each coach**

```
ggplot(data = coaches_profiles,aes(y=reorder(coaches,point_all),x=point_all,fill=coaches))+
  geom_col()+
  ggtitle("average points per match at all for each coach")+
  ylab("Coaches")+
  xlab("Points")+
  theme(legend.position="none")
```

![a3](https://user-images.githubusercontent.com/41892582/192697127-7cfbe570-6ae2-4c00-ae18-8f4a6d3a5519.png)

It's Pep Guardiola the most to collect points at all.

**Average home goals scored by each coach** 
  
  ```
ggplot(data = coaches_profiles,aes(y=reorder(coaches,goal_home),x=goal_home,fill=coaches))+
  geom_col()+
  ggtitle("average goals scored at home for each coach")+
  ylab("Coaches")+
  xlab("Goals")+
  theme(legend.position="none")
```

![a4](https://user-images.githubusercontent.com/41892582/192698855-151329ac-5ebb-4c0c-be2f-33d335d02c2c.png)


**Average away goals scored by each coach**
  
  ```
ggplot(data = coaches_profiles,aes(y=reorder(coaches,goal_away),x=goal_away,fill=coaches))+
  geom_col()+
  ggtitle("average goals scored at away for each coach")+
  ylab("Coaches")+
  xlab("Goals")+
  theme(legend.position="none")
```

![a5](https://user-images.githubusercontent.com/41892582/192698820-072c1b8d-3a84-4fd8-846d-ce96c81c9302.png)


**The average number of goals scored by each coach** 
  
  ```
ggplot(data = coaches_profiles,aes(y=reorder(coaches,goal_all),x=goal_all,fill=coaches))+
  geom_col()+
  ggtitle("average goals scored at all for each coach")+
  ylab("Coaches")+
  xlab("Goals")+
  theme(legend.position="none")
```

![a6](https://user-images.githubusercontent.com/41892582/192699450-af618e3b-7482-4c97-8770-7ff95d36b5f1.png)


As we saw, Pep Guardiola is the most attacking coach at home and away, and that implies, of course, he is the most at all.

**Average goals conceded per home match** 
  
  ```
ggplot(data = coaches_profiles,aes(y=reorder(coaches,-goal_against_home),x=goal_against_home,fill=coaches))+
  geom_col()+
  ggtitle("average goals conceded per match in home ground")+
  ylab("Coaches")+
  xlab("Goals conceded")+
  theme(legend.position="none")
```

![a7](https://user-images.githubusercontent.com/41892582/192701305-c5ed6fdc-7e5c-4131-95bd-6a7f03b4f35f.png)


**Average goals conceded per game away from home** 
  
  ```
ggplot(data = coaches_profiles,aes(y=reorder(coaches,-goal_against_away),x=goal_against_away,fill=coaches))+
  geom_col()+
  ggtitle("average goals conceded per match in away grounds")+
  ylab("Coaches")+
  xlab("Goals conceded")+
  theme(legend.position="none")
```

![a8](https://user-images.githubusercontent.com/41892582/192701242-db0bd657-ddee-4641-9d47-c15159cc754b.png)

**Average goals conceded per game** 
  
  ```
ggplot(data = coaches_profiles,aes(y=reorder(coaches,-goal_against_all),x=goal_against_all,fill=coaches))+
  geom_col()+
  ggtitle("average goals conceded per match at all")+
  ylab("Coaches")+
  xlab("Goals conceded")+
  theme(legend.position="none")
```

![a9](https://user-images.githubusercontent.com/41892582/192701199-d859129e-5bed-4e38-bf1b-d93d308a40d6.png)

As we saw, it's Jose Mourinho, the most successful defensive coach in his first period at Chelsea.

**Manchester United with and after Sir Alex Ferguson**

fields of comparison will be average points per match at all, average goals per match at all, and average goals conceded per match at all.

```
manchester<-matrix(nrow = 2,ncol = 4)
manchester<-as.data.frame(manchester)

names(manchester)=c("manchester","point_all","goal_all","goal_against_all")
manchester$manchester<-c("sir","without_sir")

man_utd<-c("sir","without_sir")
for (i in man_utd)
{

  eval(parse(text =paste0("manchester[manchester$manchester==","'",i,"'",",2]",
                          "<-(sum(",i,"_away_point$points)+sum(",i,"_home_point$points))/(sum(",i,"_away_point$n)+sum(",i,"_home_point$n))")))

  eval(parse(text =paste0("manchester[manchester$manchester==","'",i,"'",",3]","<-(",
                          i,"_goals_home$all_home_goal+",i,"_goals_away$all_away_goal)/(sum(",i,"_home_point$n)+sum(",i,"_away_point$n))")))

  eval(parse(text =paste0("manchester[manchester$manchester==","'",i,"'",",4]","<-(",
                          i,"_goals_home$all_home_goal_against+",i,"_goals_away$all_away_goal_against)/(sum(",i,"_home_point$n)+sum(",i,"_away_point$n))")))

}
```

**Manchester United's average points per match before and after Sir Alex Ferguson** 
  
  ```
ggplot(data = manchester,aes(x=manchester,y=point_all,fill=manchester))+
  geom_col()+
  ggtitle("Manchester United average points per match with Sir and after him")+
  ylab("Points")+
  xlab("Manchester United")+
  theme(legend.position="none")
```

![sir1](https://user-images.githubusercontent.com/41892582/192716817-128da665-cf8c-4cc6-aeff-1f9676909ce0.png)

**Manchester United's average goals per match before and after Sir Alex Ferguson** 

```
ggplot(data = manchester,aes(x=manchester,y=goal_all,fill=manchester))+
  geom_col()+
  ggtitle("Manchester United average Goals per match with Sir and after him")+
  ylab("Goals")+
  xlab("Manchester United")+
  theme(legend.position="none")
```

![sir2](https://user-images.githubusercontent.com/41892582/192716760-17663848-124a-40a7-89de-a0b97d284a7f.png)


**Manchester United's average goals conceded per match before and after Sir Alex Ferguson**
  
  ```
ggplot(data = manchester,aes(x=manchester,y=goal_against_all,fill=manchester))+
  geom_col()+
  ggtitle("Manchester United average goals conceded per match with Sir and after him")+
  ylab("goals conceded")+
  xlab("Manchester United")+
  theme(legend.position="none")
```

![sir3](https://user-images.githubusercontent.com/41892582/192717609-5bb25cb9-74fc-4b63-a916-bb591cbb1181.png)

It's obvious that Manchester United witnessed deterioration on all levels after Sir Alex Ferguson left.

# 5.Share phase

**conclusions**

**1-** The results show the home ground gives a remarkable advantage against the away team. The home team can be expected to score about 33% more goals than the away team and about 48% more points than the away team. Also, the 2021 season proves it's not a psychological effect, not because they were playing near home, it's the effect of the crowd and the sounds that came out of their throats.

**2-** Of course, when it comes to collecting points, more goals and fewer goals conceded mean more points. But football is a competitive game. You have to attack some times and defend others. Our results show that if we had a clean sheet, that was equivalent to a score of 2.6 goals. For me, it's very effective proof of how defensive play is. more effective when it comes to collecting points.

**3-** Our results showed Pep Guardiola is the coach to gain the most points per match in the PL. and collect the points that are the purpose of the game. But we can't emphasise that he is the best in PL history because there are a lot of factors out of our consideration here, like the ability of his team and the power of his rivals.

**for further investigation**

Of course, there are a lot of reasons for football success, perhaps it is hard to count all of the factors that influenced it, and because it's influenced by a human factor represented in his greatest manifestation by the foot of players who maybe have done something unexpected, such as throwing the ball away in front of empty goalposts, or a moment of magic when a player waves in and out some competitor players as if they are even weren't there before sending the ball into the net, or a referee not awarded a ball as a goal despite the ball clearly crossing the line by at least a foot. but maybe in further analysis, we can look at those factors.

**1- the** money spent to bring players to the team

**2- The average age**  of players on every team and their national

**3-** number of crowd attendance for every team

**4- style**  of every team and the number of their shots, corners he gets, and fouls  committed against their players.
