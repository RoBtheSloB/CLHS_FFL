
library(jsonlite)
library(gridExtra)
library(zoo)
library(knitr)
library(httr)
library(tidyverse)

options(dplyr.summarise.inform=F)

## Inputs
StartYear <- 2016
EndYear <- 2020

BaseUrl <- "https://fantasy.espn.com/apis/v3/games/ffl/leagueHistory/615594?seasonId="
TotalYears <- EndYear - StartYear + 1

RequestTeam <- NULL
RequestBoxscore <- NULL
RequestDraftDetail <- NULL
RequestSettings <- NULL

for(i in 1:TotalYears) {
  url <- str_c(BaseUrl ,StartYear + i - 1)
  Sys.sleep(1)
  RequestTeam[[i]] <- GET(url ,query = list(view = "mTeam"))
  Sys.sleep(1)
  RequestBoxscore[[i]] <- GET(url ,query = list(view = "mBoxscore"))
  Sys.sleep(1)
  RequestSettings[[i]] <- GET(url ,query = list(view = "mSettings"))
}

## Converting from JSON
JsonConversion <- function(x) {
  x %>%
    content("text") %>%
    fromJSON(flatten = TRUE)
}

RawTeamList <- map(RequestTeam ,JsonConversion)
RawBoxscoreList <- map(RequestBoxscore ,JsonConversion)
RawSettingsList <- map(RequestSettings ,JsonConversion)

## Getting a dataset that has a name with each team by year
TeamNameData <- map(RawTeamList
                    ,~.x %>%
                      as_tibble() %>% 
                      rename(LeagueId = id) %>% 
                      unnest(teams) %>% 
                      rename(TeamId = id) %>% 
                      select(seasonId ,abbrev ,TeamId ,location ,nickname ,primaryOwner ,playoffSeed) %>%
                      mutate(TeamName = str_c(location ,nickname ,sep = " ")) %>%
                      select(seasonId:abbrev ,TeamName ,everything())
) %>%
  bind_rows()

OwnerNameData <- map(RawTeamList
                     ,~.x %>%
                       as_tibble() %>% 
                       rename(LeagueId = id) %>% 
                       unnest(members) %>% 
                       rename(MemberId = id) %>%
                       select(seasonId ,firstName ,lastName ,displayName ,MemberId) %>%
                       mutate(FullName = str_c(firstName ,lastName ,sep = " ")) %>%
                       select(seasonId ,FullName ,everything())
) %>%
  bind_rows()

SettingsData <- map(RawSettingsList
                    ,~.x %>% 
                      as_tibble() %>% 
                      rename(LeagueId = id) %>% 
                      select(seasonId ,settings.scheduleSettings.playoffTeamCount ,settings.scheduleSettings.matchupPeriodCount)
) %>%
  bind_rows()

DraftOrderData <- map(RawSettingsList
                      ,~.x %>% 
                        as_tibble() %>% 
                        rename(LeagueId = id) %>% 
                        select(seasonId ,settings.draftSettings.pickOrder) %>% 
                        unnest(settings.draftSettings.pickOrder) %>% 
                        rename(teamId = settings.draftSettings.pickOrder) %>% 
                        mutate(PickNumber = row_number())
) %>% 
  bind_rows()

TransactionData <- map(RawTeamList
                       ,~.x %>%
                         as_tibble() %>% 
                         rename(LeagueId = id) %>% 
                         unnest(teams) %>% 
                         rename(TeamId = id) %>%
                         select(seasonId ,TeamId ,rankCalculatedFinal ,transactionCounter.acquisitions ,transactionCounter.trades)
) %>%
  bind_rows() %>% 
  mutate(Championship = if_else(rankCalculatedFinal == 1 ,1 ,0))

## Outputting the data
write_csv(TeamNameData ,"Team Name.csv")
write_csv(OwnerNameData ,"Owner Name.csv")
write_csv(SettingsData ,"Settings.csv")
write_csv(DraftOrderData ,"Draft Order.csv")
write_csv(TransactionData ,"Transactions.csv")












