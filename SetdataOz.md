ozon_work_data_set <- ozon_work_data_set[duplicated(ozon_work_data_set$Phone) == FALSE, ] # Удаляем дубликаты
ozon_work_data_set$date <- as.Date(gsub('(\\d{2}).(\\d{2}).(\\d{4})', '\\3-\\2-\\1', 
                                        ozon_work_data_set$date)) # Меняем формат даты обращения


# Меняем формат даты рождения
ozon_work_data_set$dob <- gsub('(\\d{2}).(\\d{2}).(\\d{4})', '\\3-\\2-\\1',
                               ozon_work_data_set$dob) # Меняем формат даты рождения
ozon_work_data_set$Age <- floor(difftime(Sys.Date(),
                                         as.Date(ozon_work_data_set$dob), 
                                         units = 'days') / 365) # Считаем кол-во дней возраста
ozon_work_data_set <- subset(ozon_work_data_set, Age >= 20) # Фильтруем больше >20 лет
ozon_work_data_set <- subset(ozon_work_data_set, Age <= 45)
ozon_work_data_set <- ozon_work_data_set[-c(1, 2, 3, 4, 10), ] #Удаляем
ozon_work_data_set$leads <- 1
ozon_work_data_set <- subset(ozon_work_data_set, date >= date_start)
ozon_work_data_set <- subset(ozon_work_data_set, date <= date_end)
#Группировка
ozon_work_data_set <- ozon_work_data_set %>%
  group_by(`UTM Source`, `UTM Campaign`, `UTM Content`, `UTM Term`) %>% # группируем таблицу по полю 
  summarise(leads                = sum(leads),                  # считаем количество Лидов
            phone_unique         = length(unique(Phone))) %>% # считаем какое к-во 
  arrange(-leads) %>% # сортирум таблицу по полю total_quantity по убыванию
  head(100) # оставляем верхние строки
#Разбить строки
library(dplyr)
library(tidyr)
ozon_work_data_set <- separate(data = ozon_work_data_set, 
                               col = `UTM Campaign`, 
                               into = c("Campaign", "CampaignID"), 
                               sep = "\\|")
ozon_work_data_set <- separate(data = ozon_work_data_set, 
                               col = `UTM Content`, 
                               into = c("vakansi", "Ad_ID", "Ban_id", "addphr", "addphr2", "camp_t", "creative_id", "d_t", "Group_ID", "phr_id", "Other"), 
                               sep = "\\|")
ozon_work_data_set <- subset(ozon_work_data_set, `UTM Source` == "yandex")
ozon_work_data_set <- ozon_work_data_set[ , -c(7:11)]

# -_______________-_____________-_________
ozon_work_data_set <- ozon_work_data_set[ , -c(4, 5, 6, 9)]

ozon_work_data_set <- separate(data = ozon_work_data_set, 
                               col = Group_ID, 
                               into = c("Group", "Group_ID"), 
                               sep = "\\:")
ozon_work_data_set <- separate(data = ozon_work_data_set, 
                               col = phr_id, 
                               into = c("phr", "CriterionId"), 
                               sep = "\\:")

ozon_work_data_set <- ozon_work_data_set[ , -c(4, 6)]

#Загружаем данные из Яндекс Директ
agancy_stats <- yadirGetReport(ReportType = "CUSTOM_REPORT", 
                               DateRangeType = "CUSTOM_DATE", 
                               DateFrom = date_start, 
                               DateTo = date_end, 
                               FieldNames = c("CampaignName",
                                              "CampaignId",
                                              "AdGroupName",
                                              "AdGroupId",
                                              "Criterion",
                                              "CriterionId",
                                              "Impressions",
                                              "Clicks",
                                              "Cost"),
                               Login = "ozon-betapress",
                               AgencyAccount = "betapress.direct",
                               TokenPath = "agency_login")

agancy_stats_work <- agancy_stats[grepl('tver', agancy_stats$CampaignName), ]

#Группировка
agancy_stats_work <- agancy_stats_work %>%
  group_by(CampGroupKey, CampaignName, AdGroupName, Criterion, CriterionId) %>% # группируем таблицу по полю 
  summarise(Cost                = sum(Cost),
            Impressions                = sum(Impressions),
            Clicks                = sum(Clicks)) %>% # считаем какое к-во 
  arrange(-Cost)

ozon_work_data_set <- ozon_work_data_set %>%
  group_by(CampGroupKey, CampaignID, Group_ID, CriterionId, `UTM Term`) %>% # группируем таблицу по полю 
  summarise(leads                = sum(leads)                  # считаем количество Лидов
  ) %>% # считаем какое к-во 
  arrange(-leads)

# Core API
ga_data <- RGA::get_ga(profileId     = "ga:184044283",
                       start.date    = date_start,
                       end.date      = date_end,
                       metrics       = "ga:users,ga:sessions,ga:goal1Completions",
                       dimensions    = "ga:date,ga:sourceMedium,ga:medium,ga:campaign,ga:adContent,ga:keyword",
                       fetch.by      = "year") #При ga:users и ga:NdayUsers могут быть некорректными

ga_data <- subset(ga_data, sourceMedium == "yandex / cpc") #
ga_data <- ga_data %>%
  group_by(campaign, adContent, keyword) %>%
  summarise(users = sum(users),
            goal1Completions = sum(goal1Completions)
  ) %>% arrange(-goal1Completions)

ga_data <- separate(data = ga_data, 
                    col = campaign, 
                    into = c("campaign", "campaign_ID"), 
                    sep = "\\|")
ga_data <- separate(data = ga_data, 
                    col = adContent, 
                    into = c("vacansi", "Adid", "banid", "1", "2", "3", "4", "d_t", "group_id", "criterionid"), 
                    sep = "\\|")
ga_data <- ga_data[ , -c(3, 4, 5, 6, 7, 8, 9, 10)]

ga_data <- separate(data = ga_data, 
                    col = group_id, 
                    into = c("Group", "Group_id"), 
                    sep = "\\:")
ga_data <- separate(data = ga_data, 
                    col = criterionid, 
                    into = c("Criterion", "Criterionid"), 
                    sep = "\\:")
ga_data <- ga_data[ , -c(3, 5)]

ga_data_work <- ga_data[grepl('kladovschik', ga_data$campaign), ]
