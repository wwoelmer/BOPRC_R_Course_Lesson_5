library(aquarius2018)
library(BoPRC2025)
library(tidyverse)

# Extract Lake Water Quality Time series

Lake_Sites <- unlist(NERMN_Lake()[[1]])
Parameters <- c("TN","TP","CHLA","VC - SD")

Lake_DF <- AQMultiExtractFlat(sitelist = Lake_Sites,param = Parameters,start="2015-01-01",end="2025-01-01")

Lake_DF %>% 
#  filter(Qualifiers %in% c("Labstar - Legacy Data","Routine")) %>% 
 # filter(Parameter== "TN (g/m^3)") %>% 
  filter(Parameter== "TP (g/m^3)") %>%
#  filter(Parameter== "CHLA (mg/m^3)") %>%
#  filter(Parameter== "VC - SD (m)") %>%
  ggplot()+
  geom_point(aes(x=Time,y=Value))+
  facet_wrap(~LocationName, scales="free_y")

### Note - talk to Scott because some data not labelled 'routine'.

write.csv(Lake_DF,file = "Lake_WQ_Timeseries.csv",row.names = F)

# Extract Lake Water Quality Time series
#MK307635 - nukuhou at Glenholme
#MK453465 - nuhuhou at Nukuhou north

Nukuhou_Discharge <- getdata("Discharge.Primary@MK307635",start = "2023-01-20",end = "2023-02-20")
Nukuhou_Rainfall <- getdata("Precip Total.Primary@MK453465",start = "2023-01-20",end = "2023-02-20")

write.csv(Nukuhou_Discharge,"Nukuhou_Discharge_Storms_2023.csv",row.names = F)
write.csv(Nukuhou_Rainfall,"Nukuhou_Rainfall_Storms_2023.csv",row.names = F)



Nukuhou_Discharge %>% 
  ggplot()+
  geom_point(aes(x=Time,y=Value))

library(padr)

Nukuhou_Rainfall %>%
#  thicken(interval = "hour")  %>%
  thicken(interval = "day")  %>%
  group_by(Time_day) %>% 
  summarise(Hourly_Rainfall = sum(Value,na.rm=T)) %>% 
  ggplot()+
  geom_bar(stat="identity",aes(x=Time_day,y=Hourly_Rainfall))

  
library(devtools)
install_local("gg.layers-master.zip")

library(gg.layers)







library(ggplot2)


Precip <- Nukuhou_Rainfall %>%
  #  thicken(interval = "hour")  %>%
  thicken(interval = "hour")  %>%
  group_by(Time_hour) %>% 
  summarise(Hourly_Rainfall = sum(Value,na.rm=T)) %>% 
  pad()


Disc <- Nukuhou_Discharge %>%
  thicken(interval = "hour") %>% 
  group_by(Time_hour) %>% 
  summarise(Hourly_Disc =mean(Value,na.rm=T))%>% 
  pad()



Q_Precip_DF <- merge(Disc,Precip,by="Time_hour")



col_prcp = "blue"  #"#3e89be"
col_runoff = "black"  # "darkorange"

my_theme <-
  theme_dual_axis(col_runoff, col_prcp) +
  theme(
    legend.position.inside = c(0, 1),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    # axis.ticks = element_blank(),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0)
  )

## Visualization ---------------------------------------------------------------
dat <- runoff_data
qmax <- max(Q_Precip_DF$Hourly_Disc) * 1.1

prcp.coef <- guess_prcp_coef(qmax, Q_Precip_DF$Hourly_Rainfall, ratio = 0.5)
# prcp.coef = qmax / pmax * ratio

ggplot(Q_Precip_DF, aes(x = Time_hour, Hourly_Disc )) +
  # theme_test() +
  geom_line() +
  geom_prcpRunoff(
    aes(prcp = Hourly_Rainfall, color = "royalblue"),
    params_prcp = list(color = col_prcp, fill = col_prcp),
    prcp.coef = prcp.coef,
    qmax = qmax,
    color = col_runoff, linewidth = 0.5
  ) +
  scale_x_datetime(date_labels = "%m/%d") +
  theme_bw()+
  my_theme +
  labs(x = "Date", y = expression("Streamflow (m"^"3" * "/s)"))


