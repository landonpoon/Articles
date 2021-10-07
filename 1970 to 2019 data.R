library(tidyverse)
library(retrosheet)
library(tidymodels)

years <- c(1971:1993,1996:2019)

#this following function was found at the link below, I did not write it
#https://stackoverflow.com/questions/49215193/r-error-cant-join-on-because-of-incompatible-types
matchColClasses <- function(df1, df2) {
  
  sharedColNames <- names(df1)[names(df1) %in% names(df2)]
  sharedColTypes <- sapply(df1[,sharedColNames], class)
  
  for (n in sharedColNames) {
    class(df2[, n]) <- sharedColTypes[n]
  }
  
  return(df2)
}
#the code below I wrote

game.years <- get_retrosheet("game", 1970)

#this is not the most efficient way to do this.
#After doing this project, I have found a better way to do the following, but because I want to document
#what I did I will leave it as is.

for (year in years) {
  game.years <- game.years %>%
    bind_rows(matchColClasses(game.years, get_retrosheet("game",year)))
}

game.years.1 <- game.years %>%
  mutate(Year = format(as.POSIXct(Date, format="%Y-%m-%d"), format="%Y")) %>%
  select(Year, Date, VisTm, HmTm, VisTmGNum, HmTmGNum, VisRuns, HmRuns)

game.years.vis <- game.years.1%>%
  rename(Tm = VisTm) %>%
  mutate(TmWin = ifelse(VisRuns > HmRuns, 1, 0)) %>%
  rename(Opp = HmTm, GNum = VisTmGNum) %>%
  select(Year, GNum, Tm, TmWin, Opp)

game.years.hm <- game.years.1%>%
  rename(Tm = HmTm) %>%
  mutate(TmWin = ifelse(VisRuns < HmRuns, 1, 0)) %>%
  rename(Opp = VisTm, GNum = HmTmGNum) %>%
  select(Year, GNum, Tm, TmWin, Opp)

game.years.2 <- game.years.vis %>%
  bind_rows(game.years.hm) %>%
  mutate(team.id = paste(Year, Tm), opp.id = paste(Year, Opp))

year.wpct <- game.years.2 %>%
  group_by(team.id) %>%
  summarize(szn.wpct = mean(TmWin))

year.wpct.over <- year.wpct %>%
  filter(szn.wpct > 0.5)

game.years.2 <- game.years.2 %>%
  mutate(opp.over = is.element(opp.id, year.wpct.over$team.id))

first.half <- game.years.2 %>%
  filter(GNum <= 81) %>%
  group_by(team.id) %>%
  summarize(first.wpct = mean(TmWin))

first.half <- game.years.2 %>%
  filter(GNum <= 81 & opp.over) %>%
  group_by(team.id) %>%
  summarize(first.wpct.over = mean(TmWin)) %>%
  inner_join(first.half)

second.half <- game.years.2 %>%
  filter(GNum > 81) %>%
  group_by(team.id) %>%
  summarize(second.wpct = mean(TmWin))

second.half <- game.years.2 %>%
  filter(GNum > 81 & opp.over) %>%
  group_by(team.id) %>%
  summarize(second.wpct.over = mean(TmWin)) %>%
  inner_join(second.half)

master.table <- first.half %>%
  inner_join(second.half)

wpct.model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") %>%
  fit(second.wpct.over ~ first.wpct, data = master.table)

wpct.over.model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") %>%
  fit(second.wpct.over ~ first.wpct.over, data = master.table)

wpct.model.second.wpct <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") %>%
  fit(second.wpct ~ first.wpct, data = master.table)

wpct.over.model.second.wpct <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") %>%
  fit(second.wpct ~ first.wpct.over, data = master.table)

ggplot(master.table, aes(first.wpct, second.wpct.over)) +
  geom_jitter(size = 2, alpha = 0.6) +
  geom_smooth(method="lm",se=FALSE) +
  ggtitle("Second Half Wpct Against Teams Over .500 vs. First Half Wpct") +
  labs(y = "Second Half Wpct Against Teams Over .500", x = "First Half Wpct") +
  theme(
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    title = element_text(size = 14),
    plot.margin = margin(0.5,0.5,0.5,0.5, "cm")
  )

ggplot(master.table, aes(first.wpct.over, second.wpct.over)) +
  geom_jitter(size = 2, alpha = 0.6) +
  geom_smooth(method="lm",se=FALSE) +
  ggtitle("Second Half Wpct Against Teams Over .500 vs. First Half Wpct \nAgainst Teams Over .500") +
  labs(y = "Second Half Wpct Against Teams Over .500", x = "First Half Wpct Against Teams Over .500") +
  theme(
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    title = element_text(size = 14),
    plot.margin = margin(0.5,0.5,0.5,0.5, "cm")
  )

ggplot(master.table, aes(first.wpct, second.wpct)) +
  geom_jitter() +
  geom_smooth(method="lm",se=FALSE)

ggplot(master.table, aes(first.wpct.over, second.wpct)) +
  geom_jitter() +
  geom_smooth(method="lm",se=FALSE)

cor(master.table$first.wpct, master.table$second.wpct.over, method = "pearson")

cor(master.table$first.wpct.over, master.table$second.wpct.over, method = "pearson")

grid.table(head(
master.table %>%
  select(team.id, first.wpct, first.wpct.over, second.wpct.over) %>%
  rename(Team = team.id, First_Wpct = first.wpct, First_Wpct_Over = first.wpct.over, Second_Wpct_Over = second.wpct.over)
), rows=NULL)
