##########################################################################################
#Kacper Kalinowski
#Gdzie zlokalizowaæ nowy instytut badawczy UE?
##########################################################################################
library(eurostat)
library(gridExtra)
library(tidyverse)
library(maps)
library(countrycode)
options(scipen = 9999)
remove(list=ls())

#mapa Europy
geodata <- get_eurostat_geospatial(resolution = "60", nuts_level = "0")
geodata$countries <- ifelse(geodata$CNTR_CODE %in% c("PL","CZ", "HU", "SK", "LT"),1 ,0 )

ggplot(data = geodata)+geom_sf(aes(fill = countries), col = 'dim grey', size = .1)+
  coord_sf(xlim = c(-10,30), ylim=c(35,70))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = 'none',
        panel.background = element_rect(fill = 'white'))


#Realny wzrost pkb

#wczytanie danych
real_gdp_growth <- get_eurostat(id = 'tec00115',
                                time_format = 'num',
                                filters = list(geo =c("PL","CZ", "HU", "SK", "LT")))

#wykres realnego wzrostu pkb w czasie
ggplot(data = real_gdp_growth, mapping = aes(x = time, y= values, color = geo))+
    geom_line(size = 0.75)+
  xlab("")+ylab("Procentowy wzrost realnego PKB")+
  geom_point()+
  facet_wrap(~geo, ncol = 2)+
  ggtitle('Procentowy wzrost realnego PKB (w latach 2007-2018)')+
  geom_line(y = 0, col = 'black')+
  theme(plot.background = element_rect(fill = 'grey21'),
        plot.title = element_text(color="white", size=14, face="bold"),
        axis.title.x = element_text(color="white", size=14, face="bold"),
        axis.title.y = element_text(color="white", size=14, face="bold"),
        legend.position = 'none',
        axis.text.x = element_text(face="bold", color="white", 
                                   size=8),
        axis.text.y = element_text(face="bold", color="white", 
                                   size=8))

#wykres slupkowy pkb w czasie kryzysu
real_gdp_growth2009 <- real_gdp_growth %>% filter(time == 2009)

ggplot(data = real_gdp_growth2009, mapping = aes(x = geo, y= values, fill = geo))+
  geom_col()+xlab("")+ylab("Procentowy wzrost realnego PKB")+
  ggtitle('Procentowy wzrost realnego PKB w 2009 roku')+
  geom_text(aes(label = values), position = position_dodge(0.9), vjust = 0, size = 5)+
  theme(plot.background = element_rect(fill = 'grey21'),
        plot.title = element_text(color="white", size=14, face="bold"),
        axis.title.x = element_text(color="white", size=14, face="bold"),
        axis.title.y = element_text(color="white", size=14, face="bold"),
        legend.position = 'none',
        axis.text.x = element_text(face="bold", color="white", 
                                   size=12),
        axis.text.y = element_text(face="bold", color="white", 
                                   size=8),
        panel.background = element_rect(fill = 'white'))


#PKB per capita
 
#wczytanie danych
gdppercapita <- get_eurostat(id = 'sdg_08_10',
                                time_format = 'num',
                                filters = list(geo =c("PL","CZ", "HU", "SK", "LT")))

#przeksztalcenie danych
gdppercapita2018 <- gdppercapita %>% filter(unit == 'CLV10_EUR_HAB' & time == 2018)
geodata <- get_eurostat_geospatial(resolution = "60", nuts_level = "0")
mapgdp <- left_join(geodata, gdppercapita2018)

#pkb per capita na mapie w roku 2018
ggplot(data = mapgdp)+geom_sf(aes(fill = values), col = 'dim grey', size = .5)+
coord_sf(xlim = c(13,26), ylim=c(46.25,56))+
scale_fill_continuous(low = 'skyblue4',
high = 'skyblue', name = "PKB per capita w roku 2018")+
ggtitle('PKB per capita')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = 'white'))


#wskaznik inflacji

#wczytanie danych
inflation_rate <-get_eurostat(id = 'tec00118',
                              time_format = 'num',
                              filters = list(geo =c("PL","CZ", "HU", "SK", "LT"))) 

#wykres inlacji w czasie
ggplot(data = inflation_rate, mapping = aes(x = time, y= values, color = geo))+
  geom_line(size = 0.75)+
  xlab("Rok")+ylab("Inflacja")+
  geom_point()+
  facet_wrap(~geo, ncol = 2)+
  ggtitle('Wskaznik inflacji w podziale na poszczególne pañstwa (w latach 2007-2018)')+
  geom_line(y = 0, col = 'black')+
  theme(plot.background = element_rect(fill = 'grey21'),
        plot.title = element_text(color="white", size=14, face="bold"),
        axis.title.x = element_text(color="white", size=14, face="bold"),
        axis.title.y = element_text(color="white", size=14, face="bold"),
        legend.position = 'none',
        axis.text.x = element_text(face="bold", color="white", 
                                   size=12),
        axis.text.y = element_text(face="bold", color="white", 
                                   size=8))

#stopa bezrobocia

#wczytanie danych
unemployment <- get_eurostat(id = 'tps00203',
                             time_format = 'num',
                             filters = list(geo =c("PL","CZ", "HU", "SK", "LT")))
#przeksztalcenie danych
unemployment2 <- unemployment %>% filter(unit == 'PC_ACT')

#wykres pudelkowy stopy bezrobocia
ggplot(data = unemployment2, mapping = aes(x = geo, y= values, fill = geo))+
  geom_boxplot()+
  xlab("Kraj")+ylab("Stopa bezrobocia (w %)")+
  ggtitle('Stopa bezrobocia wœród aktywnych zawodowo (w latach 2007-2018)')+
  theme(plot.background = element_rect(fill = 'grey21'),
        plot.title = element_text(color="white", size=14, face="bold"),
        axis.title.x = element_text(color="white", size=14, face="bold"),
        axis.title.y = element_text(color="white", size=14, face="bold"),
        legend.position = 'none',
        axis.text.x = element_text(face="bold", color="white", 
                                   size=12),
        axis.text.y = element_text(face="bold", color="white", 
                                   size=8))
#tabelka z wybranymi statystykami
statistics <- data.frame(countries = c('Czechy', 'Wêgry', 'Litwa', 'Polska', 'S³owacja'),
                          fristQ = c(quantile(unemployment2$values[unemployment2$geo == 'CZ'], probs=0.25),
                                     quantile(unemployment2$values[unemployment2$geo == 'HU'], probs=0.25),
                                     quantile(unemployment2$values[unemployment2$geo == 'LT'], probs=0.25),
                                     quantile(unemployment2$values[unemployment2$geo == 'PL'], probs=0.25),
                                     quantile(unemployment2$values[unemployment2$geo == 'SK'], probs=0.25)),
                          median = c(median(unemployment2$values[unemployment2$geo == 'CZ']),
                                     median(unemployment2$values[unemployment2$geo == 'HU']),
                                     median(unemployment2$values[unemployment2$geo == 'LT']),
                                     median(unemployment2$values[unemployment2$geo == 'PL']),
                                     median(unemployment2$values[unemployment2$geo == 'SK'])),
                          thirdQ = c(quantile(unemployment2$values[unemployment2$geo == 'CZ'], probs=0.75),
                                     quantile(unemployment2$values[unemployment2$geo == 'HU'], probs=0.75),
                                     quantile(unemployment2$values[unemployment2$geo == 'LT'], probs=0.75),
                                     quantile(unemployment2$values[unemployment2$geo == 'PL'], probs=0.75),
                                     quantile(unemployment2$values[unemployment2$geo == 'SK'], probs=0.75)))


#Oczekiwana dlugosc zycia

#wczytanie danych
lifeexp <- get_eurostat(id = 'sdg_03_10',
             time_format = 'num',
             filters = list(geo =c("PL","CZ", "HU", "SK", "LT")))

#przeksztalcenie danych
lifeexp2017 <- lifeexp %>% filter(time == 2017 & sex != 'T')

#oczekiwana dlugosc zycia w roku 2017 w podziale na mezczyzn kobiety
ggplot(data = lifeexp2017, mapping = aes(x= geo, y = values, fill= geo))+
  geom_col()+facet_wrap(~sex)+xlab("")+ylab("Oczekiwana d³ugoœæ ¿ycia")+
  ggtitle("Oczekiwana d³ugoœæ ¿ycia w chwili narodzin w podziale na p³eæ i kraje
  (w roku 2017)")+
  coord_sf(ylim = c(70,82))+
  geom_text(aes(label = values), position = position_dodge(0.9), vjust = 0, size = 5)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_fill_discrete(name = "Legenda",
                      labels = c("Czechy", "Wêgry", "Litwa", "Polska", "S³owacja"))+
  theme(plot.background = element_rect(fill = 'grey21'),
        plot.title = element_text(color="white", size=14, face="bold"),
        axis.title.x = element_text(color="white", size=14, face="bold"),
        axis.title.y = element_text(color="white", size=14, face="bold"),
        axis.text.x = element_text(face="bold", color="white", 
                                   size=10),
        axis.text.y = element_text(face="bold", color="white", 
                                   size=8),
        legend.key = element_rect(fill = "grey21"),
        legend.background = element_rect(fill = "grey21"),
        legend.text = element_text(color = 'white', face = 'bold'),
        legend.title = element_blank(),
          panel.background = element_rect(fill = 'white'))+
  scale_x_discrete(labels=c("1" = "Czechy ", "2" = "Wêgry", "3" = "Litwa",
                            '4' = 'Polska', '5' = 'S³owacja'))



#Wskaznik przestepstw
crime <- get_eurostat(id = 'sdg_16_20',
                        time_format = 'num',
                        filters = list(geo =c("PL","CZ", "HU", "SK", "LT")))
#usuniecie brakow danych
crime2 <- na.omit(crime)

#przeksztalcenie danych
crime2total <- crime2 %>% filter(incgrp == 'TOTAL' & time > 2009)

#wykres pudelkowy wskaznika przestepstw
ggplot(data = crime2total, mapping = aes(x = geo, y = values, fill= geo))+
  geom_boxplot()+xlab("")+ylab("% udzia³ populacji zg³aszaj¹cej przestêpstwa")+
  ggtitle('Rozk³ad % udzia³u populacji zg³aszaj¹cej przestêpstwa w podziale na kraje w (latach 2010-2018)')+
  scale_fill_discrete(name = "Legenda",
                      labels = c("Czechy", "Wêgry", "Litwa", "Polska", "S³owacja"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_fill_discrete(name = "Legenda",
                      labels = c("Czechy", "Wêgry", "Litwa", "Polska", "S³owacja"))+
  theme(plot.background = element_rect(fill = 'grey21'),
        plot.title = element_text(color="white", size=14, face="bold"),
        axis.title.y = element_text(color="white", size=10, face="bold"),
        axis.text.x = element_text(face="bold", color="white", 
                                   size=10),
        axis.text.y = element_text(face="bold", color="white", 
                                   size=10),
        legend.key = element_rect(fill = "grey21"),
        legend.background = element_rect(fill = "grey21"),
        legend.text = element_text(color = 'white', face = 'bold'),
        legend.title = element_blank())

#tabelka z wybranymi statystykami
statistics2 <- data.frame(countries = c('Czechy', 'Wêgry', 'Litwa', 'Polska', 'S³owacja'),
          fristQ = c(quantile(crime2total$values[crime2total$geo == 'CZ'], probs=0.25),
                     quantile(crime2total$values[crime2total$geo == 'HU'], probs=0.25),
                     quantile(crime2total$values[crime2total$geo == 'LT'], probs=0.25),
                     quantile(crime2total$values[crime2total$geo == 'PL'], probs=0.25),
                     quantile(crime2total$values[crime2total$geo == 'SK'], probs=0.25)),
          median = c(median(crime2total$values[crime2total$geo == 'CZ']),
                     median(crime2total$values[crime2total$geo == 'HU']),
                     median(crime2total$values[crime2total$geo == 'LT']),
                     median(crime2total$values[crime2total$geo == 'PL']),
                     median(crime2total$values[crime2total$geo == 'SK'])),
          thirdQ = c(quantile(crime2total$values[crime2total$geo == 'CZ'], probs=0.75),
                     quantile(crime2total$values[crime2total$geo == 'HU'], probs=0.75),
                     quantile(crime2total$values[crime2total$geo == 'LT'], probs=0.75),
                     quantile(crime2total$values[crime2total$geo == 'PL'], probs=0.75),
                     quantile(crime2total$values[crime2total$geo == 'SK'], probs=0.75)))
           


#przeksztalcenie danych 
crime2018x <- crime2total %>% filter(time == 2018 ) 
crimeslowacja <- crime2total %>% filter(time == 2017 & geo == 'SK')
crime2018 <- rbind(crime2018x, crimeslowacja)
mapcrime <- left_join(geodata, crime2018)

#mapa z wskaznikiem przestepstw
ggplot(data = mapcrime)+geom_sf(aes(fill = values), col = 'dim grey', size = .5)+
  coord_sf(xlim = c(13,26), ylim=c(46.25,56))+
  scale_fill_continuous(low = 'yellow',
                          high = 'black', name = "WskaŸnik przestêpstw")+
  ggtitle('% udzia³ populacji zg³aszaj¹cej przestêpstwa w 2018 lub 2017 roku')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())


#Osoby bez obywatelstwa

#wczytanie danych
population <- get_eurostat(id = 'tps00001',
                        time_format = 'num',
                        filters = list(geo =c("PL","CZ", "HU", "SK", "LT")))

immigrants <- get_eurostat(id = 'tps00157',
                           time_format = 'num',
                           filters = list(geo =c("PL","CZ", "HU", "SK", "LT")))
#przeksztalcenie danych
population2 <- population %>% filter(time == 2018)
immigrants2 <- immigrants %>% filter(time == 2018)
tabelaobcokrajowcy <- inner_join(population2, immigrants2, by = c('geo', 'time'))
tabelaobcokrajowcy <- na.omit(tabelaobcokrajowcy)
tabelaobcokrajowcy$values.x <- tabelaobcokrajowcy$values.x/1000000
tabelaobcokrajowcy$values.y <- tabelaobcokrajowcy$values.y/1000 


#wykres punktowy - relacja pomiedzy populacja a obcokrajowcami
ggplot(data = tabelaobcokrajowcy, mapping = aes(x= values.y , y = values.x,
                                                fill = geo))+
  geom_point(size = 6, shape = 21, color = 'black')+xlab("Liczba obcokrajowców (w tyœ)")+ylab("Populacja (w mln)")+
  ggtitle("Relacja pomiêdzy populacj¹ kraju i liczb¹ obcokrajowców (w roku 2018)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_fill_discrete(name = "Legenda",
                      labels = c("Czechy", "Wêgry", "Litwa", "Polska", "S³owacja"))+
  theme(plot.background = element_rect(fill = 'grey21'),
        plot.title = element_text(color="white", size=14, face="bold"),
        axis.title.x = element_text(color="white", size=14, face="bold"),
        axis.title.y = element_text(color="white", size=14, face="bold"),
        axis.text.x = element_text(face="bold", color="white", 
                                   size=12),
        axis.text.y = element_text(face="bold", color="white", 
                                   size=12),
        legend.key = element_rect(fill = "grey21"),
        legend.background = element_rect(fill = "grey21"),
        legend.text = element_text(color = 'white', face = 'bold'),
        legend.title = element_blank(),
        panel.background = element_rect(fill = 'white'))

#proporcja obywateli do obcokrajowcow (tabela)
tabelaobcokrajowcy$proporcja <- tabelaobcokrajowcy$values.x/tabelaobcokrajowcy$values.y
tabelaobcokrajowcy2 <- tabelaobcokrajowcy %>% filter(time == 2018) %>% select(geo, proporcja)



#Zadowolenie z dostepu do uslug kulturalnych

#wczytanie danych
culture <- read.csv('urb_percep_1_Data.csv')

#przeksztalcenie danych
culture <- culture %>% filter(CITIES %in% c('Bratislava', 'Warszawa', 'Budapest',
                                            'Vilnius', 'Praha'))

culture$CITIES <- factor(culture$CITIES, levels = c('Praha', 'Budapest', 'Vilnius', 'Warszawa',
                                                    'Bratislava'))

#Zadowolenie z kultury w stolicach (wykres) 
ggplot(data = culture, mapping = aes(x= CITIES, y = Value, fill= CITIES))+
  xlab("")+ylab("Zadowolenie z sektora kultury (w %)")+
  geom_col()+
  coord_sf(ylim = c(75,90))+
  geom_text(aes(label = Value), position = position_dodge(0.9), vjust = 0, size = 5)+
  ggtitle("Zadowolenie z sektora kultury
w wybranych stolicach")+
  scale_fill_discrete(name = "Legenda",
                      labels = c("Praga", "Budapeszt", "Wilno", "Warszawa", "Bratys³awa"))+
  theme(plot.background = element_rect(fill = 'grey21'),
        plot.title = element_text(color="white", size=11, face="bold"),
        axis.title.x = element_text(color="white", size=10, face="bold"),
        axis.title.y = element_text(color="white", size=10, face="bold"),
        axis.text.x = element_text(face="bold", color="white", 
                                   size=7.5),
        axis.text.y = element_text(face="bold", color="white", 
                                   size=8),
        panel.background = element_rect(fill = 'white'),
        legend.background = element_rect(fill = 'grey21'),
        legend.text = element_text(color = 'white', face = 'bold'),
        legend.title = element_blank())+ 
  scale_x_discrete(labels=c("1" = "Praga ", "2" = "Budapeszt", "3" = "Wilno",
                            '4' = 'Warszawa', '5' = 'Bratys³awa'))

