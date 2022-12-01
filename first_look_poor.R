library(tidyverse)
library(rgdal)
library(haven)
library(tidycensus)
library(sf)
library(tigris)
library(rmapshaper)
library(tmap)
library(areal)
library(leaflet)
library(ggmap)
library(rstudioapi)
library(RColorBrewer)
library(extrafont)
###############################################################
school_list <- read_csv("~/R/Projects/Cambodia/IDPoor/poor5.csv")
idpoor1 <- read_csv("~/R/Projects/Cambodia/IDPoor/poor5.csv")
idpoor2 <- read_csv("~/R/Projects/Cambodia/IDPoor/poor6.csv")
idpoor3 <- read_csv("~/R/Projects/Cambodia/IDPoor/poor7.csv")
idpoor4 <- read_csv("~/R/Projects/Cambodia/IDPoor/poor8.csv")
idpoor1<- idpoor1[,1:13]
idpoor2<- idpoor2[,1:13]
idpoor3<- idpoor3[,1:13]
idpoor4<- idpoor4[,1:13]
idpoor1[,1:12] <- lapply(idpoor1[,1:12], as.character)
idpoor2[,1:12] <- lapply(idpoor2[,1:12], as.character)
idpoor3[,1:12] <- lapply(idpoor3[,1:12], as.character)
idpoor4[,1:12] <- lapply(idpoor4[,1:12], as.character)
idpoor  <- bind_rows(idpoor1,idpoor2,idpoor3, idpoor4)
ca_prov_lu <- read_csv("~/R/Projects/Cambodia/IDPoor/ca_prov_lu.csv")
ca_com_lu <- read_csv("~/R/Projects/Cambodia/IDPoor/ca_com_lu.csv")
ca_dist_lu <- read_csv("~/R/Projects/Cambodia/IDPoor/ca_dist_lu.csv")
idpoor <- idpoor %>% mutate(province=str_pad(Province, 2, pad = "0"))
idpoor <- idpoor %>% mutate(district=str_pad(`Town/District/Khan`, 2, pad = "0"))
idpoor <- idpoor %>% mutate(province = str_c("KH", province))
idpoor <- idpoor %>% 
  mutate(d_code = str_c(province, district))
idpoor <- idpoor %>% filter(YOB >= 1920 & YOB <= 2021)
idpoor <- idpoor %>% mutate(age = 2021 - YOB) %>%
  mutate(pri3 = ifelse(age >= 6 & age <= 8,1,0)) %>%
  mutate(pri6 = ifelse(age >= 9 & age <= 11,1,0))%>%
  mutate(pri3f = ifelse(age >= 6 & age <= 8 & Gender == "female",1,0)) %>%
  mutate(pri6f = ifelse(age >= 9 & age <= 11 & Gender == "female",1,0))
idpoor <- idpoor %>% mutate(age = 2021 - YOB) %>%
  mutate(ls = ifelse(age >= 12 & age <= 14,1,0)) %>%
  mutate(us = ifelse(age >= 15 & age <= 17,1,0))%>%
  mutate(lsf = ifelse(age >= 12 & age <= 14 & Gender == "female",1,0)) %>%
  mutate(usf = ifelse(age >= 15 & age <= 17 & Gender == "female",1,0)) %>%
  mutate(sch_age = ifelse(age>= 6 & age <= 17,1,0))
idpoor <- idpoor %>% mutate(poor1 = ifelse(`Poor Level` == "POOR_1",1,0))%>% 
  mutate(poor2 = ifelse(`Poor Level` == "POOR_2",1,0))
idpoor_agg <- idpoor %>% group_by(Key) %>% summarize(pri3 = sum(pri3), pri3f = 
                                                       sum(pri3f),
                                                     pri6 = sum(pri6),
                                                     pri6f = sum(pri6f),
                                                     ls = sum(ls),
                                                     lsf = sum(lsf),
                                                     us = sum(us),
                                                     usf = sum(usf),
                                                     poor1 = max(poor1),
                                                     poor2 = max(poor2),
                                                     sch_age = sum(sch_age))
key <- idpoor[,c(7,16)]
key<- as_tibble(key)
key<- unique(key)
idpoor_agg <- left_join(key,idpoor_agg, by = "Key")
idpoor_dist_agg <- idpoor_agg %>% group_by(d_code, poor1) %>%
  summarize(prim_age = sum(pri3 + pri6), sec_age = sum(ls + us), sch_age = 
              sum(sch_age))%>% mutate(poorcat = ifelse(poor1 == 1, 1,0 ))%>%
  mutate(poorcat = ifelse(poor1 == 0,2,poorcat))

k_census <- read_csv("~/R/Projects/Cambodia/IDPoor/K_census.csv")
k_census$name <-NULL

dis_poor1 <- idpoor_dist_agg %>% filter(poorcat == 1)
dis_poor1 <- dis_poor1[,c(1,3:6)]
dis_poor2 <- idpoor_dist_agg %>% filter(poorcat == 2)
dis_poor2 <- dis_poor2[,c(1,3:6)]
dis_poor <- left_join(dis_poor1, dis_poor2, by = c("d_code"))
dis_poor <- left_join(dis_poor, k_census, by = c("d_code"= "code"))
dis_poor<- dis_poor%>% mutate(sch_age1_hh=  round(sch_age.x/hh,3)) %>% 
  mutate(prim_age1_hh=  round(prim_age.x/hh,3))%>%
  mutate(sec_age1_hh=  round(sec_age.x/hh,3)) %>% 
  mutate(sch_aget_hh=  round((sch_age.y+sch_age.x) /hh,3)) %>%
  mutate(prim_aget_hh=  round((prim_age.y + prim_age.y)/hh,3))%>%
  mutate(sec_aget_hh=  round((sec_age.y + sec_age.y)/hh,3))
IDP_prim <- dis_poor[,c(1,2,6)]
IDP_sec <- dis_poor[,c(1,3,7)]
IDP_tot <- dis_poor[,c(1,4,8)]
dis_poor <- dis_poor[,c(1,12:17)]
plot_poor <- dis_poor %>% mutate(IDP_Q =ifelse(sch_age1_hh >= .13,"Q1",""))%>%
  mutate(IDP_Q =ifelse(sch_age1_hh >= .095 & sch_age1_hh < .13,"Q2",IDP_Q))%>%
  mutate(IDP_Q =ifelse(sch_age1_hh >= .064 & sch_age1_hh < .095,"Q3",IDP_Q))%>%
  mutate(IDP_Q =ifelse(sch_age1_hh >= .042 & sch_age1_hh < .064,"Q4",IDP_Q))%>%
  mutate(IDP_Q =ifelse(sch_age1_hh <.042 ,"Q5",IDP_Q))%>%
  mutate(IDP_Qp =ifelse(sch_age1_hh >= .069,"Q1",""))%>%
  mutate(IDP_Qp =ifelse(prim_age1_hh >= .048 & prim_age1_hh < .069,"Q2",IDP_Qp))%>%
  mutate(IDP_Qp =ifelse(prim_age1_hh >= .032 & prim_age1_hh < .048,"Q3",IDP_Qp))%>%
  mutate(IDP_Qp =ifelse(prim_age1_hh >= .022 & prim_age1_hh < .032,"Q4",IDP_Qp))%>%
  mutate(IDP_Qp =ifelse(prim_age1_hh <.022 ,"Q5",IDP_Qp))%>%
  mutate(IDP_Qs =ifelse(sch_age1_hh >= .065,"Q1",""))%>%
  mutate(IDP_Qs =ifelse(prim_age1_hh >= .046 & prim_age1_hh < .065,"Q2",IDP_Qs))%>%
  mutate(IDP_Qs =ifelse(prim_age1_hh >= .032 & prim_age1_hh < .046,"Q3",IDP_Qs))%>%
  mutate(IDP_Qs =ifelse(prim_age1_hh >= .020 & prim_age1_hh < .032,"Q4",IDP_Qs))%>%
  mutate(IDP_Qs =ifelse(prim_age1_hh <.02 ,"Q5",IDP_Qs))
plot_poor <- plot_poor %>% mutate(IDP2_Q =ifelse(sch_aget_hh >= .359,"Q1",""))%>%
  mutate(IDP2_Q =ifelse(sch_aget_hh >= .258 & sch_aget_hh < .359,"Q2",IDP2_Q))%>%
  mutate(IDP2_Q =ifelse(sch_aget_hh >= 0.188 & sch_aget_hh < .258,"Q3",IDP2_Q))%>%
  mutate(IDP2_Q =ifelse(sch_aget_hh >= 0.127 & sch_aget_hh < 0.188,"Q4",IDP2_Q))%>%
  mutate(IDP2_Q =ifelse(sch_aget_hh <0.127 ,"Q5",IDP2_Q))
register_google(key = "AIzaSyB4H7pcbjnnvEuq7yNKGcsRiXQ0s-vkCvA")
cambodia <-st_read("~/R/Projects/Cambodia/shape/admin2/khm_admbnda_adm2_gov_20181004.shp")
cambodia2 <-st_read("~/R/Projects/Cambodia/shape/admin1/khm_admbnda_adm1_gov_20181004.shp")
cambodia_ft <- fortify(cambodia)
cambodia2_ft<- fortify(cambodia2)
cam_codes <- as_tibble(cambodia_ft)%>% select(4,8,3)
colnames(cam_codes) <- c("d_code","province", "district")
cambodia <- sf::st_transform(cambodia, 3857)
cambodia2 <- sf::st_transform(cambodia, 3857)
cambodiaIDP = left_join(cambodia, plot_poor, by = c("ADM2_PCODE" = "d_code"))
dis_idp <- ggplot() + 
  geom_sf(data = subset(cambodiaIDP,ADM1_PCODE != "KH21" &
                          ADM1_PCODE != "KH08"&
                          ADM1_PCODE != "KH12"&
                          ADM1_PCODE != "KH14"&
                          ADM1_PCODE != "KH20"&
                          ADM1_PCODE != "KH25"), aes(fill = as.factor(IDP_Q)))+ theme_void() +
  scale_fill_brewer(palette = "Blues",direction = -1,name = "School age IDPoor1 children\n per 100 HH in district",labels=c("13+", "9 to 13",
                                                                                                                            "6 to 9","4 to 6", "less than 4") )+ 
  theme(legend.position = "right",legend.title = element_text(size = 8, face = "bold"),
        legend.text = element_text( size = 10),
        plot.title = element_text( size = 14, hjust = .5))+
  geom_sf(data = subset(cambodia2_ft,ADM1_PCODE != "KH21" &
                          ADM1_PCODE != "KH08"&
                          ADM1_PCODE != "KH12"&
                          ADM1_PCODE != "KH14"&
                          ADM1_PCODE != "KH20"&
                          ADM1_PCODE != "KH25"), aes(fill = NA),color = "dark red",lwd =  .9, show.legend = FALSE)+
  geom_sf_text(data = subset(cambodiaIDP,ADM1_PCODE != "KH21" &
                               ADM1_PCODE != "KH08"&
                               ADM1_PCODE != "KH12"&
                               ADM1_PCODE != "KH14"&
                               ADM1_PCODE != "KH20"&
                               ADM1_PCODE != "KH25"), aes(label = ADM2_EN),size = 2,check_overlap = TRUE)+
  ggtitle("Cambodia")+ 
  theme(plot.title = element_text(face = "bold"))
ggsave("dis_idp.png",width = 8, height = 6, dpi = 120)

dis_idp2 <- ggplot() + 
  geom_sf(data = subset(cambodiaIDP,ADM1_PCODE != "KH21" &
                          ADM1_PCODE != "KH08"&
                          ADM1_PCODE != "KH12"&
                          ADM1_PCODE != "KH14"&
                          ADM1_PCODE != "KH20"&
                          ADM1_PCODE != "KH25"), aes(fill = as.factor(IDP2_Q)))+ theme_void() +
  scale_fill_brewer(palette = "Blues",direction = -1,name = "School age IDPoor1 & 2 children\n per 100 HH in district",labels=c("35+", "26 t0 34",
                                                                                                                                "19 to 25","13 to 18", "less than 13") )+ 
  theme(legend.position = "right",legend.title = element_text(size = 8, face = "bold"),
        legend.text = element_text( size = 10),
        plot.title = element_text( size = 14, hjust = .5))+
  geom_sf(data = subset(cambodia2_ft,ADM1_PCODE != "KH21" &
                          ADM1_PCODE != "KH08"&
                          ADM1_PCODE != "KH12"&
                          ADM1_PCODE != "KH14"&
                          ADM1_PCODE != "KH20"&
                          ADM1_PCODE != "KH25"), aes(fill = NA),color = "dark red",lwd =  .9, show.legend = FALSE)+
  geom_sf_text(data = subset(cambodiaIDP,ADM1_PCODE != "KH21" &
                               ADM1_PCODE != "KH08"&
                               ADM1_PCODE != "KH12"&
                               ADM1_PCODE != "KH14"&
                               ADM1_PCODE != "KH20"&
                               ADM1_PCODE != "KH25"), aes(label = ADM2_EN),size = 2,check_overlap = TRUE)+
  ggtitle("Cambodia")+ 
  theme(plot.title = element_text(face = "bold"))
ggsave("dis_idp2.png",width = 8, height = 6, dpi = 120)