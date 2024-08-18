library(tidyverse)
library(readxl)
library(writexl)

#Сбер_Индекс
sber_tvolume <- read.csv("C:/Users/admin/Desktop/учеба/R/1/Данные/СберИндекс/Доля безналичных платежей в торговом обороте.csv", sep = ";")
sber_cactivity <- read.csv("C:/Users/admin/Desktop/учеба/R/1/Данные/СберИндекс/Индекс потребительской активности.csv", sep = ";", fileEncoding = "Windows-1251")
sber_dtourists <- read.csv("C:/Users/admin/Desktop/учеба/R/1/Данные/СберИндекс/Количество внутренних туристов.csv", sep = ";")


sber_tvolume <- distinct(sber_tvolume)
sber_cactivity <- distinct(sber_cactivity)
sber_dtourists <- distinct(sber_dtourists)

sber_tvolume$Год <- as.numeric(format(as.Date(sber_tvolume$Дата, format = "%Y-%m-%d"), "%Y"))
sber_tvolume$Месяц <- format(as.Date(sber_tvolume$Дата, format = "%Y-%m-%d"), "%B")
sber_cactivity$Год <- as.numeric(format(as.Date(sber_cactivity$Дата, format = "%d.%m.%Y"), "%Y"))
sber_cactivity$Месяц <- format(as.Date(sber_cactivity$Дата, format = "%d.%m.%Y"), "%B")
sber_cactivity$Дата <- format(as.Date(sber_cactivity$Дата, format = "%d.%m.%Y"), "%Y-%m")
sber_dtourists$Год <- as.numeric(format(as.Date(sber_dtourists$Дата, format = "%Y-%m-%d"), "%Y"))
sber_dtourists$Месяц <- format(as.Date(sber_dtourists$Дата, format = "%Y-%m-%d"), "%B")
sber_cactivity_summarized = sber_cactivity %>% group_by(Дата, Регион, Год, Месяц) %>% summarise(Значение = median(Значение))
sber_cactivity_grouped <- sber_cactivity_summarized %>% filter(Регион != "Россия", Год == 2020) %>% group_by(Регион, Год, Месяц) %>% summarize(Значение)
sber_dtourists_grouped <- sber_dtourists %>%filter(Регион != "Россия", Год == 2020) %>% group_by(Регион, Год, Месяц) %>% summarize(Значение)
sber_tvolume_grouped <- sber_tvolume %>% filter(Регион != "Россия", Год == 2020) %>% group_by(Регион, Год, Месяц) %>% summarize(Значение)

df_list <- list(sber_dtourists_grouped, sber_tvolume_grouped, sber_cactivity_grouped)
sber_final <- df_list %>% reduce(full_join, by= c("Регион", "Год", "Месяц"))
sber_final <- sber_final %>%
  mutate(Месяц = factor(Месяц, levels =c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь", "Декабрь"), ordered = TRUE, labels=c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь", "Декабрь")))
sber_final <- sber_final %>% 
  mutate(Регион = gsub("Край", "край", Регион)) %>% 
  mutate(Регион = gsub("Область", "область", Регион)) %>%
  mutate(Регион = ifelse(Регион == "Республика Карачаево-Черкессия", "Карачаево-Черкесская Республика", Регион)) %>%
  mutate(Регион = ifelse(Регион == "Ненецкий АО", "Ненецкий автономный округ", Регион)) %>%
  mutate(Регион = ifelse(Регион == "Адыгея", "Республика Адыгея", Регион)) %>%
  mutate(Регион = ifelse(Регион == "Алтай", "Республика Алтай", Регион)) %>%
  mutate(Регион = ifelse(Регион == "Мордовия", "Республика Мордовия", Регион)) %>%
  mutate(Регион = ifelse(Регион == "Республика Северная Осетия-Алания", "Республика Северная Осетия - Алания", Регион)) %>%
  mutate(Регион = ifelse(Регион == "Ханты-Мансийский АО - Югра", "Ханты-Мансийский автономный округ - Югра", Регион)) %>%
  mutate(Регион = ifelse(Регион == "Чукотский АО", "Чукотский автономный округ", Регион)) %>%
  mutate(Регион = ifelse(Регион == "Ямало-Ненецкий АО", "Ямало-Ненецкий автономный округ", Регион)) %>%
  arrange(Регион, Год, Месяц)

sber_final <- sber_final %>% ungroup() %>% fill(Значение.x, Значение.y, Значение, .direction = "up")
sber_final <- sber_final %>% rename(`Кол-во внутр. тур.` = Значение.x,`Доля БП в торг. обороте` = Значение.y, `Индекс ПА` = Значение)

str(sber_final)
rm(sber_tvolume, sber_cactivity, sber_dtourists, sber_cactivity_summarized, sber_cactivity_grouped, sber_dtourists_grouped, sber_tvolume_grouped, df_list)
#Невозможно заменить значения по региону, т.к. значения полностью пустые в определённом регионе. Поэтому сделал так, как выше.

#ДомКлик
files <- list.files("C:/Users/admin/Desktop/учеба/R/1/Данные/ДомКлик/Рейтинг регионов по количеству заявок на кредит", full.names = TRUE)
read_and_combine <- function(files) {
  # Чтение и объединение файлов
  data <- map_dfr(files, ~ {
    # Извлечение имени файла без расширения
    filename <- basename(.x) %>%
      # Извлечение только названия месяца
      str_extract("\\w+")
    
    # Чтение файла Excel и добавление столбца с названием месяца
    read_excel(.x) %>%
      mutate(Месяц = filename, Год = 2020)
  })}
combined_data <- read_and_combine(files)
combined_data <- distinct(combined_data)

domclick_credit_grouped <- combined_data %>% filter(!is.na(Регион)) %>% group_by(Регион, Год, Месяц) %>% summarize(`Всего одобренных заявок`, `Доля онлайн-заявок`, `Доля заявок в офисе банка`) %>% arrange(Регион, Месяц)
domclick_credit_grouped$`Доля онлайн-заявок` <- as.numeric(gsub("%", "", domclick_credit_grouped$`Доля онлайн-заявок`))
domclick_credit_grouped$`Доля заявок в офисе банка` <- as.numeric(gsub("%", "", domclick_credit_grouped$`Доля заявок в офисе банка`))

files <- list.files("C:/Users/admin/Desktop/учеба/R/1/Данные/ДомКлик/Рейтинг регионов по количеству ипотечных сделок", full.names = TRUE)
combined_data <- read_and_combine(files)
combined_data <- distinct(combined_data)
domclick_mortgage_grouped <- combined_data %>% filter(!is.na(Регион)) %>% group_by(Регион, Год, Месяц) %>% summarize(`Всего ипотечных сделок`, `Доля сделок, первичка`, `Доля сделок, вторичка`) %>% arrange(Регион, Месяц)
domclick_mortgage_grouped$`Доля сделок, первичка` <- as.numeric(gsub("%", "", domclick_mortgage_grouped$`Доля сделок, первичка`))
domclick_mortgage_grouped$`Доля сделок, вторичка` <- as.numeric(gsub("%", "", domclick_mortgage_grouped$`Доля сделок, вторичка`))

str(domclick_credit_grouped)
str(domclick_mortgage_grouped)

domclick_final <- full_join(domclick_credit_grouped, domclick_mortgage_grouped, by= c("Регион", "Год", "Месяц"))
domclick_final <- domclick_final %>% mutate(`Всего одобренных заявок` = ifelse(`Всего одобренных заявок` == "—", NA, `Всего одобренных заявок`))
domclick_final <- domclick_final %>% mutate(`Всего ипотечных сделок` = ifelse(`Всего ипотечных сделок` == "—", NA, `Всего ипотечных сделок`))
domclick_final <- domclick_final %>% ungroup() %>% fill(`Всего одобренных заявок`, `Доля онлайн-заявок`, `Доля заявок в офисе банка`,`Всего ипотечных сделок`,`Доля сделок, первичка`,`Доля сделок, вторичка`, .direction = "up")

domclick_final <- domclick_final %>%
  mutate(Месяц = factor(Месяц, levels = c("январь", "февраль", "март", "апрель", "май", "июнь", "июль", "август", "сентябрь", "октябрь", "ноябрь", "декабрь"), ordered = TRUE, labels=c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь", "Декабрь")))

domclick_final <- domclick_final %>% 
  mutate(Регион = gsub("Край", "край", Регион)) %>% 
  mutate(Регион = gsub("Область", "область", Регион)) %>%
  mutate(Регион = ifelse(Регион == "Еврейская Автономная область", "Еврейская автономная область", Регион)) %>%
  mutate(Регион = ifelse(Регион == "Ненецкий Автономный округ", "Ненецкий автономный округ", Регион)) %>%
  mutate(Регион = ifelse(Регион == "Ханты-Мансийский Автономный округ - Югра", "Ханты-Мансийский автономный округ - Югра", Регион)) %>%
  mutate(Регион = ifelse(Регион == "Чукотский Автономный округ", "Чукотский автономный округ", Регион)) %>%
  mutate(Регион = ifelse(Регион == "Ямало-Ненецкий Автономный округ", "Ямало-Ненецкий автономный округ", Регион)) %>%
  arrange(Регион, Год, Месяц)

rm(combined_data, domclick_credit_grouped, domclick_mortgage_grouped, files, read_and_combine)

#Этими строками проверяю разницу между двумя датафреймами по названиям регионов
in_df1 <- setdiff(sber_final$Регион, domclick_final$Регион)
in_df2 <- setdiff(domclick_final$Регион, sber_final$Регион)


#Росстат

#При импорте без предварительной обработки становится очень сложно работать с данными, поэтому я принял решение отредактировать excel файлы вручную перед импортом. 
#Что я сделал:

#В зарплатах: Убрал все лишние надписи, оставил только 2020 год, преобразовал регионы которые содержали "в том числе, без авт.окр и т.д."
data_salary <- read_excel("C:/Users/admin/Desktop/учеба/R/1/Данные/Росстат/Заработная плата.xlsx", sheet = "с 2019") 
data_salary <- distinct(data_salary)

#Решил сделать так, потому что Севастополь, думаю, можно отнести к Крыму, опираясь на данные sber и domclick
data_salary <- data_salary %>% 
  mutate(январь = ifelse(Регион == "Республика Крым", январь + data_salary$январь[data_salary$Регион == "г.Севастополь"], январь)) %>%
  mutate(февраль = ifelse(Регион == "Республика Крым", февраль + data_salary$февраль[data_salary$Регион == "г.Севастополь"], февраль)) %>%
  mutate(март = ifelse(Регион == "Республика Крым", март + data_salary$март[data_salary$Регион == "г.Севастополь"], март)) %>%
  mutate(апрель = ifelse(Регион == "Республика Крым", апрель + data_salary$апрель[data_salary$Регион == "г.Севастополь"], апрель)) %>%
  mutate(май = ifelse(Регион == "Республика Крым", май + data_salary$май[data_salary$Регион == "г.Севастополь"], май)) %>%
  mutate(июнь = ifelse(Регион == "Республика Крым", июнь + data_salary$июнь[data_salary$Регион == "г.Севастополь"], июнь)) %>%
  mutate(июль = ifelse(Регион == "Республика Крым", июль + data_salary$июль[data_salary$Регион == "г.Севастополь"], июль)) %>%
  mutate(август = ifelse(Регион == "Республика Крым", август + data_salary$август[data_salary$Регион == "г.Севастополь"], август)) %>%
  mutate(сентябрь = ifelse(Регион == "Республика Крым", сентябрь + data_salary$сентябрь[data_salary$Регион == "г.Севастополь"], сентябрь)) %>%
  mutate(октябрь = ifelse(Регион == "Республика Крым", октябрь + data_salary$октябрь[data_salary$Регион == "г.Севастополь"], октябрь)) %>%
  mutate(ноябрь = ifelse(Регион == "Республика Крым", ноябрь + data_salary$ноябрь[data_salary$Регион == "г.Севастополь"], ноябрь)) %>%
  mutate(декабрь = ifelse(Регион == "Республика Крым", декабрь + data_salary$декабрь[data_salary$Регион == "г.Севастополь"], декабрь)) %>%
  filter(Регион != "г.Севастополь")

data_salary <- data_salary %>%
  pivot_longer(cols = -Регион, names_to = "Месяц", values_to = "Заработная плата") %>%
  mutate(Квартал = ifelse(Месяц == c("январь", "февраль", "март"), "квартал 1", NA)) %>%
  mutate(Квартал = ifelse(Месяц == c("апрель", "май", "июнь"), "квартал 2", Квартал)) %>%
  mutate(Квартал = ifelse(Месяц == c("июль", "август", "сентябрь"), "квартал 3", Квартал)) %>%
  mutate(Квартал = ifelse(Месяц == c("октябрь", "ноябрь", "декабрь"), "квартал 4", Квартал)) %>%
  mutate(Регион= ifelse(Регион == "Еврейская авт.область", "Еврейская автономная область", Регион)) %>%
  mutate(Регион= ifelse(Регион == "Ненецкий авт.округ", "Ненецкий автономный округ", Регион)) %>% 
  mutate(Регион= ifelse(Регион == "Ханты-Мансийский  авт. округ - Югра", "Ханты-Мансийский автономный округ - Югра", Регион)) %>% 
  mutate(Регион= ifelse(Регион == "Чукотский авт.округ", "Чукотский автономный округ", Регион)) %>% 
  mutate(Регион= ifelse(Регион == "Ямало-Ненецкий авт. округ", "Ямало-Ненецкий автономный округ", Регион)) %>% 
  mutate(Регион= ifelse(Регион == "г.Москва", "Москва", Регион)) %>% 
  mutate(Регион= ifelse(Регион == "Республика Северная  Осетия - Алания", "Республика Северная Осетия - Алания", Регион)) %>%
  mutate(Регион= ifelse(Регион == "г.Санкт-Петербург", "Санкт-Петербург", Регион)) %>%
  mutate(Месяц = factor(Месяц, levels = c("январь", "февраль", "март", "апрель", "май", "июнь", "июль", "август", "сентябрь", "октябрь", "ноябрь", "декабрь"), ordered = TRUE, labels=c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь", "Декабрь")) )%>%
  arrange(Регион, Месяц)
  
#В интернете: Убрал все лишние надписи, оставил только 2019 год, преобразовал регионы которые содержали "в том числе, без авт.окр и т.д."
data_ethernet <- read_excel("C:/Users/admin/Desktop/учеба/R/1/Данные/Росстат/Интернет.xlsx", sheet = "Беспроводная наземная") 
data_ethernet <- distinct(data_ethernet)
data_ethernet$`1 квартал` <- as.numeric(data_ethernet$`1 квартал`)
data_ethernet$`II квартал` <- as.numeric(data_ethernet$`II квартал`)
data_ethernet$`III квартал` <- as.numeric(data_ethernet$`III квартал`)
data_ethernet$`IV квартал` <- as.numeric(data_ethernet$`IV квартал`)
data_ethernet <- data_ethernet %>%
  mutate(Регион = ifelse(Регион == "г. Москва", "Москва", Регион),
         Регион = ifelse(Регион == "г. Санкт-Петербург", "Санкт-Петербург", Регион),
         Регион = ifelse(Регион == "г. Москва", "Москва", Регион),
         Регион = gsub("республика", "Республика", Регион),
         Регион = ifelse(Регион == "Ханты-Мансийский автономный АО - Югра", "Ханты-Мансийский автономный округ - Югра", Регион),
         Регион = ifelse(Регион == "Ямало-Ненецкий автономный АО", "Ямало-Ненецкий автономный округ", Регион)
         ) %>%
  mutate(
    `1 квартал` = ifelse(Регион == "Республика Крым", `1 квартал` + data_ethernet$`1 квартал`[data_ethernet$Регион == "г. Севастополь"], `1 квартал`),
    `II квартал` = ifelse(Регион == "Республика Крым", `II квартал` + data_ethernet$`II квартал`[data_ethernet$Регион == "г. Севастополь"], `II квартал`),
    `III квартал` = ifelse(Регион == "Республика Крым", `III квартал` + data_ethernet$`III квартал`[data_ethernet$Регион == "г. Севастополь"], `III квартал`),
    `IV квартал` = ifelse(Регион == "Республика Крым", `IV квартал` + data_ethernet$`IV квартал`[data_ethernet$Регион == "г. Севастополь"], `IV квартал`)) %>% filter(Регион != "г. Севастополь")
  

data_ethernet <- data_ethernet %>%
  pivot_longer(cols = -Регион, names_to = "Квартал", values_to = "Активные абоненты") %>%
  mutate(Квартал = ifelse(Квартал == "1 квартал", "квартал 1", Квартал),
         Квартал = ifelse(Квартал == "II квартал", "квартал 2", Квартал),
         Квартал = ifelse(Квартал == "III квартал", "квартал 3", Квартал),
         Квартал = ifelse(Квартал == "IV квартал", "квартал 4", Квартал)) %>%
  arrange(Регион) %>%
  fill(`Активные абоненты`, .direction = "up") %>%
  select(Регион, Квартал, `Активные абоненты`)
  
#Этими строками проверяю разницу между двумя датафреймами по названиям регионов
in_df1 <- setdiff(data_salary$Регион, data_ethernet$Регион)
in_df2 <- setdiff(data_ethernet$Регион, data_salary$Регион)


#В безработице: Убрал все лишние надписи, оставил только 2020 год, преобразовал регионы которые содержали "в том числе, без авт.окр и т.д.",
data_unempl <- read_excel("C:/Users/admin/Desktop/учеба/R/1/Данные/Росстат/Уровень безработицы населения.xls", sheet = "3 месяца 15-72 лет") 
data_unempl <- distinct(data_unempl)

data_unempl <- data_unempl %>%
  mutate(
    `январь - март` = ifelse(Регион == "Республика Крым", `январь - март` + data_unempl$`январь - март`[data_unempl$Регион == "г. Севастополь"], `январь - март`),
    `апрель - июнь` = ifelse(Регион == "Республика Крым", `апрель - июнь` + data_unempl$`апрель - июнь`[data_unempl$Регион == "г. Севастополь"], `апрель - июнь`),
    `июль - сентябрь` = ifelse(Регион == "Республика Крым", `июль - сентябрь` + data_unempl$`июль - сентябрь`[data_unempl$Регион == "г. Севастополь"], `июль - сентябрь`),
    `октябрь - декабрь` = ifelse(Регион == "Республика Крым", `октябрь - декабрь` + data_unempl$`октябрь - декабрь`[data_unempl$Регион == "г. Севастополь"], `октябрь - декабрь`)
  ) %>% filter(Регион != "г. Севастополь")


data_unempl <- data_unempl %>%
  pivot_longer(cols = -1, names_to = "Период", values_to = "Ур. безработицы") %>%
  mutate(Квартал = ifelse(Период == "январь - март", "квартал 1", NA)) %>%
  mutate(Квартал = ifelse(Период == "апрель - июнь", "квартал 2", Квартал)) %>%
  mutate(Квартал = ifelse(Период == "июль - сентябрь", "квартал 3", Квартал)) %>%
  mutate(Квартал = ifelse(Период == "октябрь - декабрь", "квартал 4", Квартал)) %>%
  mutate(Регион= ifelse(Регион == "г. Москва", "Москва", Регион)) %>% 
  mutate(Регион= ifelse(Регион == "г.Санкт-Петербург", "Санкт-Петербург", Регион)) %>%
  select(Регион, Квартал, `Ур. безработицы`) %>% drop_na(Квартал) %>% arrange(Регион)

#Этими строками проверяю разницу между двумя датафреймами по названиям регионов
in_df1 <- setdiff(data_salary$Регион, data_unempl$Регион)
in_df2 <- setdiff(data_unempl$Регион, data_salary$Регион)

df_list <- list(data_unempl, data_salary, data_ethernet)
rosstat_final <- df_list %>% reduce(full_join, by= c("Регион", "Квартал"))
rosstat_final <- rosstat_final %>% fill(`Активные абоненты`, .direction = "up")
rm(data_ethernet, data_salary, data_unempl, df_list)

#Этими строками проверяю разницу между двумя датафреймами по названиям регионов
in_df1 <- setdiff(rosstat_final$Регион, domclick_final$Регион)
in_df2 <- setdiff(domclick_final$Регион, rosstat_final$Регион)
#Думаю, что удалю Республику Крым из данных Росстата, т.к. её нет в сбер и домклик.
rosstat_final <- rosstat_final %>% filter(Регион != "Республика Крым")
rm(in_df1, in_df2)

#Объединение в итоговый датафрейм.
df_list <- list(rosstat_final, domclick_final, sber_final)
final <- df_list %>% reduce(full_join, by= c("Регион", "Месяц"))
final <- final %>% select(-Год.x, -Год.y) %>% arrange(Регион, Месяц)

write_xlsx(final, "Данные 2020.xlsx")

rm(df_list, sber_final, domclick_final, rosstat_final, final)
