library(dplyr)
library(dynlm)
library(ggfortify)
library(urca)
library(ggplot2)
library(car)
library(stargazer)

setwd("/Users/andriussvilpauskas/Desktop")
###################################Preparation########################################

### Prep ###

# Convert 'Date' column to Date format
it_df_resub<-it_df_resub%>%
  mutate(Date=as.Date(Date, format= "%Y/%m/%d"))


# Create a binary column 'January' indicating whether the month is January
it_df_resub<-it_df_resub%>%
  mutate("January"=(ifelse(Month==1,1,0)))


# Create a time series object
data_ts<-it_df_resub%>%
  select(FTSE_MIB_PI,MATCH,important,loss,it_u,it_gdp,it_cpi_rate,win,top_7,Monday,Tuesday,
         Wednesday,Thursday,Friday,January,
         MATCH_aft,important_aft,win_aft,loss_aft,top_7_aft,FTSE_ITALIA_SC_PI)%>%
  ts(start = 2003,frequency = 260)


# Fit dynamic linear models for different variables and create summaries
Levels_MIB<-data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~MATCH+important+loss+loss:important+win+top_7,data=.)
summary(Levels_MIB)

Levels_aft_MIB<-data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~MATCH_aft+important_aft+loss_aft+loss_aft:important_aft+win_aft+top_7_aft,data=.)
summary(Levels_aft_MIB)

Levels_SC<-data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~MATCH+important+loss+loss:important+win+top_7,data=.)
summary(Levels_SC)

Levels_aft_SC<-data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~MATCH_aft+important_aft+loss_aft+loss_aft:important_aft+win_aft+top_7_aft,data=.)
summary(Levels_aft_SC)


# Combine fitted values from dynamic linear models to the original time series data
Data_ts<-cbind(data_ts,Levels_MIB$fitted.values,Levels_aft_MIB$fitted.values,
               Levels_SC$fitted.values,Levels_aft_SC$fitted.values)  
colnames(Data_ts)<-c("FTSE_MIB_PI","MATCH","important","loss","it_u","it_gdp","it_cpi_rate",
                     "win","top_7","Monday","Tuesday","Wednesday","Thursday","Friday","January","MATCH_aft","important_aft","win_aft","loss_aft",
                     "top_7_aft","FTSE_ITALIA_SC_PI","e","e_aft","e_sc","e_sc_aft")

####################################Regressions#######################################

###MIB###

# No augmentation, game day
Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~e,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~it_u+log(it_gdp)+it_cpi_rate+e,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~L(d(log(FTSE_MIB_PI)),1)+e,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~L(d(log(FTSE_MIB_PI)),1)+
          it_u+log(it_gdp)+it_cpi_rate+e,data=.)%>%
  summary()

# Augmented, game day
Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~e+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~it_u+log(it_gdp)+it_cpi_rate+e+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~L(d(log(FTSE_MIB_PI)),1)+e+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~L(d(log(FTSE_MIB_PI)),1)+
          it_u+log(it_gdp)+it_cpi_rate+January+Monday+e,data=.)%>%
  summary()

# No augmentation, game day t+1
Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~e_aft,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~it_u+log(it_gdp)+it_cpi_rate+e_aft,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~L(d(log(FTSE_MIB_PI)),1)+e_aft,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~L(d(log(FTSE_MIB_PI)),1)+
          it_u+log(it_gdp)+it_cpi_rate+e_aft,data=.)%>%
  summary()

# Augmented, game day t+1
Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~e_aft+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~it_u+log(it_gdp)+it_cpi_rate+e_aft+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~L(d(log(FTSE_MIB_PI)),1)+e_aft+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~L(d(log(FTSE_MIB_PI)),1)+
          it_u+log(it_gdp)+it_cpi_rate+January+Monday+e_aft,data=.)%>%
  summary()

# No Augmentation, game day and game day t+1
Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~e+e_aft,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~it_u+log(it_gdp)+it_cpi_rate+e+e_aft,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~L(d(log(FTSE_MIB_PI)),1)+e+e_aft,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~L(d(log(FTSE_MIB_PI)),1)+
          it_u+log(it_gdp)+it_cpi_rate+e+e_aft,data=.)%>%
  summary()

# Augmented, game day and game day t+1
Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~e+e_aft+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~it_u+log(it_gdp)+it_cpi_rate+e+e_aft+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~L(d(log(FTSE_MIB_PI)),1)+e+e_aft+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~L(d(log(FTSE_MIB_PI)),1)+
          it_u+log(it_gdp)+it_cpi_rate+January+Monday+e+e_aft,data=.)%>%
  summary()






###Small-Cap###






# No augmentation, game day
Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~e_sc,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~it_u+log(it_gdp)+it_cpi_rate+e_sc,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~L(d(log(FTSE_ITALIA_SC_PI)),1)+e_sc,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~L(d(log(FTSE_ITALIA_SC_PI)),1)+
          it_u+log(it_gdp)+it_cpi_rate+e_sc,data=.)%>%
  summary()

# Augmented, game day
Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~e_sc+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~it_u+log(it_gdp)+it_cpi_rate+e_sc+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~L(d(log(FTSE_ITALIA_SC_PI)),1)+e_sc+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~L(d(log(FTSE_ITALIA_SC_PI)),1)+
          it_u+log(it_gdp)+it_cpi_rate+January+Monday+e_sc,data=.)%>%
  summary()

# No augmentation, game day t+1
Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~e_sc_aft,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~it_u+log(it_gdp)+it_cpi_rate+e_sc_aft,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~L(d(log(FTSE_ITALIA_SC_PI)),1)+e_sc_aft,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~L(d(log(FTSE_ITALIA_SC_PI)),1)+
          it_u+log(it_gdp)+it_cpi_rate+e_sc_aft,data=.)%>%
  summary()

# Augmented, game day t+1
Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~e_sc_aft+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~it_u+log(it_gdp)+it_cpi_rate+e_sc_aft+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~L(d(log(FTSE_ITALIA_SC_PI)),1)+e_sc_aft+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~L(d(log(FTSE_ITALIA_SC_PI)),1)+
          it_u+log(it_gdp)+it_cpi_rate+January+Monday+e_sc_aft,data=.)%>%
  summary()

# No Augmentation, game day and game day t+1
Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~e_sc+e_sc_aft,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~it_u+log(it_gdp)+it_cpi_rate+e_sc+e_sc_aft,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~L(d(log(FTSE_ITALIA_SC_PI)),1)+e_sc+e_sc_aft,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~L(d(log(FTSE_ITALIA_SC_PI)),1)+
          it_u+log(it_gdp)+it_cpi_rate+e_sc+e_sc_aft,data=.)%>%
  summary()

# Augmented, game day and game day t+1
Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~e_sc+e_sc_aft+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~it_u+log(it_gdp)+it_cpi_rate+e_sc+e_sc_aft+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~L(d(log(FTSE_ITALIA_SC_PI)),1)+e_sc+e_sc_aft+January+Monday,data=.)%>%
  summary()

Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~L(d(log(FTSE_ITALIA_SC_PI)),1)+
          it_u+log(it_gdp)+it_cpi_rate+January+Monday+e_sc+e_sc_aft,data=.)%>%
  summary()



##################################Visualisations####################################

#Net result ggplot

it_df%>%
  filter(MATCH==1)%>%
  ggplot(aes(x=Date,y=result_net,color=factor(LL),shape=factor(top_7), alpha = important))+
  geom_point(size = 3.33333)+
  scale_y_continuous(breaks = round(seq(min(it_df$result_net), max(it_df$result_net), by = 1),1)) +
  labs( shape = "Top 7") +
  labs( color = "Surprise Factor") +
  scale_color_manual( values = c("#F8766D", "#00BA38" ,"#619CFF"),
                      labels = c("Forseen outcome", "Unlikely fortunate outcome", "Unlike unfortunate outcome")
  ) +
  scale_shape_manual( values = c(16,17), 
                      label = c("NO", "YES"))+
  ylab("Net Result")+
  scale_alpha(range = c(0.5,1))+
  labs(alpha = "Important")


it_df %>% 
  mutate(LL = ifelse(u_unfortunate_out == 1,"Unlikely unfortunate outcome",
                     ifelse(u_fortunate_out == 1,"Unlikely fortunate outcome",
                            ifelse(unlikely_outcome == 1, 0 ,"Forseen outcome")
                     )))

summary(it_df$LL)

it_df$LL <- ifelse(it_df$u_unfortunate_out == 1,"Unlikely unfortunate outcome",
                   ifelse(it_df$u_fortunate_out == 1,"Unlikely fortunate outcome",
                          ifelse(it_df$win == 1 | it_df$loss == 1 | it_df$draw == 1, "Forseen outcome", 0)))

#Stargazers

regression1<-Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~L(d(log(FTSE_MIB_PI)),1)+
          it_u+log(it_gdp)+it_cpi_rate+e+e_aft,data=.)

regression2<-Data_ts%>%
  dynlm(d(log(FTSE_MIB_PI))~L(d(log(FTSE_MIB_PI)),1)+
          it_u+log(it_gdp)+it_cpi_rate+January+Monday+Tuesday+Wednesday+Thursday+Friday+e+e_aft,data=.)
summary(regression2)

regression3<-Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~L(d(log(FTSE_ITALIA_SC_PI)),1)+
          it_u+log(it_gdp)+it_cpi_rate+e_sc+e_sc_aft,data=.)

regression4<-Data_ts%>%
  dynlm(d(log(FTSE_ITALIA_SC_PI))~L(d(log(FTSE_ITALIA_SC_PI)),1)+
          it_u+log(it_gdp)+it_cpi_rate+January+Monday+Tuesday+Wednesday+Thursday+Friday+e_sc+e_sc_aft,data=.)

vif1<-vif(regression1)
vif2<-vif(Data_ts%>%
            dynlm(d(log(FTSE_MIB_PI))~L(d(log(FTSE_MIB_PI)),1)+
                    it_u+log(it_gdp)+it_cpi_rate+January+Monday+Tuesday+Wednesday+Thursday+e+e_aft,data=.))
vif3<-vif(regression3)
vif4<-vif(Data_ts%>%
            dynlm(d(log(FTSE_ITALIA_SC_PI))~L(d(log(FTSE_ITALIA_SC_PI)),1)+
                    it_u+log(it_gdp)+it_cpi_rate+January+Monday+Tuesday+Wednesday+Thursday+e_sc+e_sc_aft,data=.))


vif_bin1<-vif(Levels_MIB)
vif_bin2<-vif(Levels_aft_MIB)
vif_bin3<-vif(Levels_SC)
vif_bin4<-vif(Levels_aft_SC)


# Print regression results and VIFs using stargazer
stargazer(regression1,
          regression2,
          regression3,
          regression4,
          title = "Regression results",
          align = TRUE,
          type = "text",
          out = "models.htm",
          covariate.labels = c("MIB Last Returns","Small-Cap Last Returns",
                               "Unemployment","GDP","CPI","January","Monday","Tuesday","Wednesday","Thursday",
                               "Friday","game (MIB)","game t-1 (MIB)","game (SC)",
                               "game t-1 (SC)","Constant"),
          column.labels = c("FTSE MIB Daily Returns","FTSE Italia Small-Cap Daily Returns"),
          column.separate = c(2,4))


stargazer(Levels_MIB,
          Levels_aft_MIB,
          Levels_SC,
          Levels_aft_SC,
          title = "Bin Regression Results",
          align = TRUE,
          type = "text",
          out = "models.htm",
          covariate.labels = c("Game","Important Game","Game lost",
                               "Game won","top 7 opponent","Important Game:Game lost",
                               "Game t-1","Important Game t-1","Game t-1 lost",
                               "Game t-1 won","top 7 opponent t-1","Important Game:Game lost t-1",
                               "Constant"),
          column.labels = c("FTSE MIB Daily Returns","FTSE Italia Small-Cap Daily Returns"),
          column.separate = c(2,4))




stargazer(vif1,
          title = "VIF of Non-Augmented FTSE MIB index return regression",
          align = TRUE,
          type = "text",
          out = "models.htm",
          covariate.labels = c("MIB Last Returns","Unlemployent","GDP","CPI","game MIB",
                               "after game MIB"))

stargazer(vif2,
          title = "VIF of Augmented FTSE MIB index return regression",
          align = TRUE,
          type = "text",
          out = "models.htm",
          covariate.labels = c("MIB Last Returns","Unlemployent","GDP","CPI","January",
                               "Monday","Tuesday","Wednesday","Thursday","game MIB","after game MIB"))

stargazer(vif3,
          title = "VIF of Non-Augmented FTSE Italia Small-Cap index return regression",
          align = TRUE,
          type = "text",
          out = "models.htm",
          covariate.labels = c("Small-Cap Last Returns","Unlemployent","GDP","CPI","game Small-Cap",
                               "after game Small-Cap"))

stargazer(vif4,
          title = "VIF of Augmented FTSE Italia Small-Cap index return regression",
          align = TRUE,
          type = "text",
          out = "models.htm",
          covariate.labels = c("Small-Cap Last Returns","Unlemployent","GDP","CPI","January",
                               "Monday","Tuesday","Wednesday","Thursday","game Small-Cap","after game Small-Cap"))


stargazer(vif_bin1,
          title = "VIF of the FTSE MIB index returns on same-day game regression",
          align = TRUE,
          type = "text",
          out = "models.htm",
          covariate.labels = c("Game","Important game","Lost Game","Won Game","top 7 opponent",
                               "Lost Imporatnt Game"))

stargazer(vif_bin2,
          title = "VIF of the FTSE MIB index returns on after-game game regression",
          align = TRUE,
          type = "text",
          out = "models.htm",
          covariate.labels = c("Game","Important game","Lost Game","Won Game","top 7 opponent",
                               "Lost Imporatnt Game"))

stargazer(vif_bin3,
          title = "VIF of the FTSE Italia Small-Cap index returns on same-day game regression",
          align = TRUE,
          type = "text",
          out = "models.htm",
          covariate.labels = c("Game","Important game","Lost Game","Won Game","top 7 opponent",
                               "Lost Imporatnt Game"))

stargazer(vif_bin4,
          title = "VIF of the FTSE Italia Small-Cap index returns on after-game game regression",
          align = TRUE,
          type = "text",
          out = "models.htm",
          covariate.labels = c("Game","Important game","Lost Game","Won Game","top 7 opponent",
                               "Lost Imporatnt Game"))




