library(tidyverse)
library(scales)
library(rstatix)
library(readxl)
library(psych)
library(sjstats)
library(sjmisc)
library(tidyr)

setwd("C:/Users/alara/Desktop/Rko")
getwd()

data <- read_xlsx("BP_data.xlsx")

#_____________________________________________________________________________________________________________________
df_Osob_hran <- 
  data %>% 
    select(ID, Osob_hran_01:Osob_hran_10) %>% 
    mutate(across(c(Osob_hran_01:Osob_hran_10), ~ recode(., 
                                         "Rozhodně souhlasím" = 5,
                                         "Spíše souhlasím" = 4,
                                         "Je mi to jedno" = 3,
                                         "Spíše nesouhlasím" = 2,
                                         "Rozhodně nesouhlasím" = 1))) %>% 
    drop_na() %>% 
    mutate(Osob_hran = rowMeans(across(starts_with("Osob_hran_")))) %>% 
    select(-(c(Osob_hran_01:Osob_hran_10)))
    
#_____________________________________________________________________________________________________________________
df_Zmen_chov <- 
  data %>% 
    select(ID, Zmen_chov_01:Zmen_chov_16) %>% 
    mutate(across(c(Zmen_chov_01:Zmen_chov_16), ~ recode(., 
                                         "Nevadí" = 3,
                                         "Je mi to jedno" = 2,
                                         "Vadí" = 1))) %>% 
    drop_na() %>% 
    mutate(Zmen_chov = rowMeans(across(starts_with("Zmen_chov_")))) %>% 
    select(-(c(Zmen_chov_01:Zmen_chov_16)))


#_____________________________________________________________________________________________________________________
df_Ost_hran <- 
  data %>% 
    select(ID, Ost_hran_01:Ost_hran_08) %>% 
    mutate(across(c(Ost_hran_01:Ost_hran_08), ~ recode(., 
                                         "Rozhodně ano" = 5,
                                         "Spíše ano" = 4,
                                         "Je mi to jedno" = 3,
                                         "Spíše ne" = 2,
                                         "Rozhodně ne" = 1))) %>% 
    drop_na() %>% 
    mutate(Ost_hran = rowMeans(across(starts_with("Ost_hran_"))))

#_____________________________________________________________________________________________________________________
df_1 <- 
  left_join(data, df_Ost_hran %>% select(ID, Ost_hran), by = "ID")

df_2 <- 
  left_join(df_1, df_Zmen_chov %>% select(ID, Zmen_chov), by = "ID")

data <- 
  left_join(df_2, df_Osob_hran %>% select(ID, Osob_hran), by = "ID")

#_____________________________________________________________________________________________________________________
# H1a: Osobní kontrola nad svým jednáním při konzumaci alkoholu vymezuje hranici přijatelnosti konzumace. 
osob_kon
Osob_hran
data %>% 
  select(Osob_hran, Osob_kon) %>%
  mutate(Osob_kon = fct_collapse(Osob_kon, 
                                 "Málokdy" = c("Někdy" , "Málokdy")), 
         Osob_kon = fct_relevel(Osob_kon, "Vždycky", "Téměř vždycky", "Často", "Málokdy")) %>% 
  freq_table(Osob_hran, Osob_kon) %>% 
  ggplot(aes(x = Osob_kon, y = Osob_hran, fill = Osob_kon))+
  geom_col()
  


# H1b: Pokud člověk nevykazuje výrazné změny chování, pak je vyšší míra přijatelnosti konzumace alkoholu. 



#_____________________________________________________________________________________________________________________
# H2a: Lidé jsou na sebe benevolentnější než při stanovování hranice u druhých. 
#V podstatě to dokazuji na dalších graf, kdy je to podle genderu a podle věku u sebe a pro ostatní

#_____________________________________________________________________________________________________________________
# H2b: Podle genderu se bude lišit vnímaná přijatelná míra konzumace alkoholu. jak pro sebe tak pro ostatní
gender
Osob_hran
data %>% 
  select(Osob_hran, Gender) %>% 
  group_by(Gender) %>% 
  drop_na() %>% 
  summarise(Osob_hran2 = mean(Osob_hran, na.rm = T)) %>% 
  ggplot(aes(x = Gender, y = Osob_hran2, fill = Gender))+
  geom_col(position = position_dodge(width = 0.75), width = 0.7)+
  scale_fill_manual(values = c("cyan", "pink"))+
  coord_cartesian(ylim = c(1,5))+
  theme_classic()+
  theme(axis.line.y = element_blank(), 
        axis.ticks = element_blank())+
  theme(legend.position = "none")+
  labs(x = "Gender", 
       y = "Hranice přijatelné konzumace pro sebe")

#Test- hypotezy--------------------------------------------------------
#T-test
var.test(Osob_hran ~ Gender, data = data %>% 
           select(Osob_hran, Gender) %>% 
           drop_na(Osob_hran, Gender))

data%>% 
  select(Osob_hran, Gender) %>% 
  group_by(Gender) %>% 
  drop_na() %>% 
  summarise(Osob_hran_mean = mean(Osob_hran, na.rm = T), 
            Osob_hran_sd = sd(Osob_hran, na.rm = T), 
            Osob_hran_var = var(Osob_hran, na.rm = T))


t.test(Osob_hran ~ Gender, data = data %>%
         select(Osob_hran, Gender) %>%
         drop_na(Osob_hran, Gender), 
       var.equal = TRUE)

data %>%
  select(Osob_hran, Gender) %>%
  drop_na(Osob_hran, Gender) %>%
  cohens_d(Osob_hran ~ Gender, var.equal = TRUE)



Ost_hran
data %>% 
  select(Ost_hran, Gender) %>% 
  group_by(Gender) %>% 
  drop_na() %>% 
  summarise(Ost_hran2 = mean(Ost_hran, na.rm = T)) %>% 
  ggplot(aes(x = Gender, y = Ost_hran2, fill = Gender))+
  geom_col(position = position_dodge(width = 0.75), width = 0.7)+
  scale_fill_manual(values = c("cyan", "pink"))+
  coord_cartesian(ylim = c(1,5))+
  theme_classic()+
  theme(axis.line.y = element_blank(), 
        axis.ticks = element_blank())+
  theme(legend.position = "none")+
  labs(x = "Gender", 
       y = "Hranice přijatelné konzumace pro ostatní")

#Test- hypotezy--------------------------------------------------------
#T-test
var.test(Ost_hran ~ Gender, data = data %>% 
           select(Ost_hran, Gender) %>% 
           drop_na(Ost_hran, Gender))

data%>% 
  select(Ost_hran, Gender) %>% 
  group_by(Gender) %>% 
  drop_na() %>% 
  summarise(Ost_hran_mean = mean(Ost_hran, na.rm = T), 
            Ost_hran_sd = sd(Ost_hran, na.rm = T), 
            Ost_hran_var = var(Ost_hran, na.rm = T))


t.test(Ost_hran ~ Gender, data = data %>%
         select(Ost_hran, Gender) %>%
         drop_na(Ost_hran, Gender), 
       var.equal = TRUE)

data %>%
  select(Ost_hran, Gender) %>%
  drop_na(Ost_hran, Gender) %>%
  cohens_d(Ost_hran ~ Gender, var.equal = TRUE)

#_____________________________________________________________________________________________________________________
# H3: Podle věku se bude lišit vnímaná přijatelná hranice konzumace alkoholu. jak pro sebe tak pro ostatní
Vek
Osob_hran
data %>% 
  select(Osob_hran, Vek) %>% 
  group_by(Vek) %>% 
  drop_na() %>% 
  summarise(Osob_hran2 = mean(Osob_hran, na.rm = T)) %>% 
  ggplot(aes(x = Vek, y = Osob_hran2, fill = Vek))+
  geom_col(position = position_dodge(width = 0.75), width = 0.7)+
  scale_fill_manual(values = c("#434343",  "#737373", "gray", "gray36"))+
  coord_cartesian(ylim = c(1,5))+
  theme_classic()+
  theme(axis.line.y = element_blank(), 
        axis.ticks = element_blank())+
  theme(legend.position = "none",
        legend.text = element_text(size = 10, color = "gray23"),
        legend.title = element_text(size = 10))+
  labs(x = "Věk", 
       y = "Hranice přijateltné konzumace pro sebe")


#Test- hypotezy--------------------------------------------------------
data_H3a <- data %>%
  select(Osob_hran, Vek) %>% 
  drop_na()


data_H3a %>%
  group_by(Vek) %>% 
  summarise(Osob_hran_mean = mean(Osob_hran, na.rm = TRUE), 
            Osob_hran_sd = sd(Osob_hran, na.rm = TRUE), 
            Osob_hran_var = var(Osob_hran, na.rm = TRUE))

data_H3a %>% 
  anova_test(Osob_hran ~ Vek)

data_H3a %>% 
  tukey_hsd(Osob_hran ~ Vek)
print(tukey_result)

Ost_hran
data %>% 
  select(Ost_hran, Vek) %>% 
  group_by(Vek) %>% 
  drop_na() %>% 
  summarise(Ost_hran2 = mean(Ost_hran, na.rm = T)) %>% 
  ggplot(aes(x = Vek, y = Ost_hran2, fill = Vek))+
  geom_col(position = position_dodge(width = 0.75), width = 0.7)+
  scale_fill_manual(values = c("#434343",  "#737373", "gray", "gray36"))+
  coord_cartesian(ylim = c(1,5))+
  theme_classic()+
  theme(axis.line.y = element_blank(), 
        axis.ticks = element_blank())+
  theme(legend.position = "none",
        legend.text = element_text(size = 10, color = "gray23"),
        legend.title = element_text(size = 10))+
  labs(x = "Věk", 
       y = "Hranice přijateltné konzumace pro ostatní")


#Test- hypotezy--------------------------------------------------------
data_H3b <- data %>%
  select(Ost_hran, Vek) %>% 
  drop_na()


data_H3b %>%
  group_by(Vek) %>% 
  summarise(Ost_hran_mean = mean(Ost_hran, na.rm = TRUE), 
            Ost_hran_sd = sd(Ost_hran, na.rm = TRUE), 
            Ost_hran_var = var(Ost_hran, na.rm = TRUE))

data_H3b %>% 
  anova_test(Ost_hran ~ Vek)

data_H3b %>% 
  tukey_hsd(Ost_hran ~ Vek)
print(tukey_result)

#_____________________________________________________________________________________________________________________
# H4: Jedinci, kteří se cítí zdraví, budou častěji konzumovat alkohol nežli ti, kteří své zdraví takto nehodnotí. 

data %>% 
  select(Subjekt_zdravi, Osob_frek_alko) %>% 
  mutate(Subjekt_zdravi = fct_collapse(Subjekt_zdravi, 
                                       "Dobré" = c("Velmi dobré", "Dobré"), 
                                       "Špatné" = c("Velmi špatné", "Špatné")), 
         Subjekt_zdravi = fct_relevel(Subjekt_zdravi, "Dobré", "Uspokojivé", "Špatné")) %>%
  mutate(Osob_frek_alko = fct_collapse(Osob_frek_alko, 
                                       "Několikrát týdně" = c("1x - 3x týdně", "4x - 6x týdně"), 
                                       "Několikrát do měsíce" = c("1x do měsíce", "2x do měsíce", "3x do měsíce")), 
         Osob_frek_alko = fct_relevel(Osob_frek_alko, "Denně")) %>% 
  drop_na() %>% 
  freq_table(Subjekt_zdravi, Osob_frek_alko) %>% 
  ggplot(aes(x = Subjekt_zdravi, y = prop, fill = Osob_frek_alko))+
  geom_col(position = position_dodge(width = 0.75), width = 0.7)+
  geom_text(aes(label = scales::label_percent(accuracy = 1, scale = 1, suffix = " %")(prop)),
    position = position_dodge(width = 0.75), vjust = -0.5, color = "black", size = 3.5) +
  scale_fill_manual(values = c("#434343",  "#737373", "gray"))+
  scale_y_continuous(limits = c(0,100), labels = scales::label_percent(scale = 1, suffix = " %", accuracy = 1))+
  theme_classic()+
  theme(axis.line.y = element_blank(), 
        axis.ticks = element_blank())+
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 10, color = "gray23"),
        legend.title = element_text(size = 10))+
  guides(fill = guide_legend(title = "Frekvence komzumace alkoholu"))+
  labs(x = "Pociťované zdraví", 
       y = "Frekvence komzumace alkoholu")

#Test- hypotezy--------------------------------------------------------
data_H4 <- data %>%
  select(Subjekt_zdravi, Osob_frek_alko) %>% 
  mutate(Osob_frek_alko = recode(Osob_frek_alko, 
                                 "1x - 3x týdně" = 2,
                                 "4x - 6x týdně" = 1,
                                 "1x do měsíce" = 5,
                                 "2x do měsíce" = 4,
                                 "3x do měsíce" = 3)) %>% 
  mutate(Subjekt_zdravi = fct_collapse(Subjekt_zdravi, 
                                       "Dobré" = c("Velmi dobré", "Dobré"), 
                                       "Špatné" = c("Velmi špatné", "Špatné"))) %>% 
  drop_na()

data_H4 %>%
  select(Osob_frek_alko, Subjekt_zdravi) %>% 
  group_by(Subjekt_zdravi) %>% 
  summarise(Osob_frek_alko_mean = mean(Osob_frek_alko, na.rm = T), 
            Osob_frek_alko_sd = sd(Osob_frek_alko, na.rm = T), 
            Osob_frek_alko_var = var(Osob_frek_alko, na.rm = T))


data_H4 %>% 
  anova_test(Osob_frek_alko ~ Subjekt_zdravi)

data_H4 %>% 
  tukey_hsd(Osob_frek_alko ~ Subjekt_zdravi)



#_____________________________________________________________________________________________________________________
# H5a: Abstinence je vnímána jako stigma. 
# H5b: Lidé s vyšším vzdělání méně stigmatizují abstinenci. 
# H5c: Ženy méně stigmatizují abstinenci než muži

data %>% 
  select(Stigma_Index, Gender, Vzdelani) %>%
  mutate(Vzdelani = fct_collapse(Vzdelani, 
                                 "Základní vzdělaní a středoškolské s výučním listem" = c("Základní", "Středoškolské s výučním listem"), 
                                 "Středoškolské vzdělání s maturitou" = c("Středoškolské s maturitou"), 
                                 "Vyšší odborné a vysokoškolské vzdělání" = c("Vyšší odborné", "Vysokoškolské")), 
         Vzdelani = fct_relevel(Vzdelani, "Základní vzdělaní a středoškolské s výučním listem")) %>% 
  drop_na() %>% 
  group_by(Gender, Vzdelani) %>% 
  summarise(Stigma_Index2 = mean(Stigma_Index, na.rm = T)) %>% 
  ggplot(aes(x = Gender, y = Stigma_Index2, fill = Vzdelani))+
  geom_col(position = position_dodge(width = 0.75), width = 0.7)+
  scale_fill_manual(values = c("#434343",  "#737373", "gray"))+
  coord_cartesian(ylim = c(1,5))+
  theme_classic()+
  theme(axis.line.y = element_blank(), 
        axis.ticks = element_blank())+
  theme(legend.position = "right",
        legend.text = element_text(size = 10, color = "gray23"),
        legend.title = element_text(size = 10))+
  guides(fill = guide_legend(title = "Nevyšší dosažené vzdělání"))+
  labs(x = "Gender", 
       y = "Stigmatizace abstinence")
  
  