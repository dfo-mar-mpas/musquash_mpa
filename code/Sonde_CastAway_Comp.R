#libraries
library(dplyr)
library(tidyr)
library(ggplot2)

#load environmental extractions from castaway_process_2022.R
dat <- read.csv("output/environmental_covariates.csv")

#reformat the temperature data for a bivariate, site-based comparison of Sonde vs Castaway summaries
dat_temp <- rbind(
            dat%>%
            filter(variable=="temperature",location=="Surface")%>%
            spread(platform,value),
            dat%>%
              filter(variable=="temperature",location=="Bottom")%>%
              spread(platform,value)
            )

p1 <- ggplot(dat_temp,aes(x=CastAway,y=Sonde))+
  geom_point()+
  stat_smooth(method="lm")+
  labs(x=expression("CastAway CTD - Temperature " ( degree~C)),
       y=expression("Sonde - Temperature " ( degree~C)))+
  facet_wrap(~location,ncol=2,scales="free")+
  theme_bw();p1

ggsave("output/Sonde_Castaway_comp_2022.png",p1,width=8,height=5,units="in",dpi=300)

mod=glm(Sonde ~ CastAway + location,data=dat_temp)
summary(mod)

dat_sal <- dat%>%
  filter(variable=="salinity",location=="Surface")%>%
  spread(platform,value)
