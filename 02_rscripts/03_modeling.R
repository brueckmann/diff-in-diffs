#### load packages ####
library(tidyverse)
library(modelsummary)

### Import data ####

# Note current script location
current_script_dir <- this.path::this.dir()
cat("Script directory:", current_script_dir, "\n")

# Set project root
project_root <- this.path::dirname2(current_script_dir)
setwd(project_root)
cat("Project root set to:", getwd(), "\n")


# load data
load(file.path(project_root, "01_data/processed/data.Rdata"))



### For Table 2 ####

# Note: Columns 2–6 report estimates from regression models that include controls for age and gender, 
# as well as fixed effects for education levels and income brackets. 
# Column 3 includes respondents that did not report their car’s fuel and/or emission category. 
# Columns 4–6 include dummies for past Lega vote in legislative, regional, and municipal elections, respectively. 
# Robust standard errors are in parentheses. *p < 0.05; **p < 0.01.




### First, view the data:

# For the inclusion of cars without information on their group assignment, use ``dummy_diesel_ass`` and ``dummy_euro_4_ass``:
  
dat <- data |> 
  select( dummy_diesel_ass, dummy_diesel, dummy_euro_4_ass, dummy_euro_4, no_answer_euro)  


modelsummary::datasummary_skim(     dat ,
                      fun_numeric = list(Unique = NUnique,
                                         `Missing Pct.` = PercentMissing, Mean = Mean, `Std. Dev.` = SD, Min = Min, Median = Median,
                                         Max = Max), 
                      width = 1   )  




tabdiesel <- data %>%
  count(dummy_diesel_ass, dummy_diesel, .drop = FALSE)

diesel_tbl <- tinytable::tt(tabdiesel,
                            width = 1,
                            digits = 0)
diesel_tbl



###### Interesting....


Legaswitch_legis_lm_ms<-estimatr:: lm_robust(sw_to_lega_18_19~dummy_diesel+dummy_euro_4+diesel_euro4, 
                                            data=data, subset=c(target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0))










############
# Figure 4 #
############

# Panel (a) From Legislative Elections 2018  --------
# 1. no control
Legaswitch_legis_lm_ms<-estimatr:: lm_robust(sw_to_lega_18_19~dummy_diesel+dummy_euro_4+diesel_euro4, 
                                  data=data, subset=c(target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0))
# 2. Including individual controls
Legaswitch_legis_lm_cont<-estimatr:: lm_robust(sw_to_lega_18_19~dummy_diesel+dummy_euro_4+diesel_euro4+
                                      age+female+EDU+INC, 
                                    data=data, subset=c(target!=3 & target!=4 & 
                                                                      no_answer_euro==0  & no_answer_2018==0))
# 3. Including unkowncar and assigning the treatment
Legaswitch_legis_lm_cont_Unknowncar<-estimatr:: lm_robust(sw_to_lega_18_19~dummy_diesel_ass+dummy_euro_4_ass+diesel_euro4_ass+
                                                 age+female+EDU+INC+dummy_car_unknown, 
                                               data=data, subset=c(target!=3 & no_answer_euro==0 & no_answer_2018==0))


M.Legaswitch_legis_lm_ms <-  tidy(Legaswitch_legis_lm_ms, conf.int = TRUE)
M.Legaswitch_legis_lm_cont <-  tidy(Legaswitch_legis_lm_cont, conf.int = TRUE)
M.Legaswitch_legis_lm_cont_Unknowncar <-  tidy(Legaswitch_legis_lm_cont_Unknowncar, conf.int = TRUE)

Legaswitch_legis_all<- bind_rows(M.Legaswitch_legis_lm_ms, 
                                 M.Legaswitch_legis_lm_cont, 
                                 M.Legaswitch_legis_lm_cont_Unknowncar,  
                                 .id="Model")

Legaswitch_legis_all<-subset(Legaswitch_legis_all, term=="dummy_diesel"|term=="dummy_euro_4"|term=="diesel_euro4"|
                               term== "dummy_diesel_ass"|term=="dummy_euro_4_ass"|term=="diesel_euro4_ass") 
Legaswitch_legis_all<-Legaswitch_legis_all %>%
  mutate(term2=case_when(term=="dummy_diesel" |term=="dummy_diesel_ass"~"Diesel",
                         term=="dummy_euro_4" |term=="dummy_euro_4_ass"~"Euro 4",
                         term=="diesel_euro4" |term=="diesel_euro4_ass"~"Diesel Euro 4"))

Legaswitch_legis_all$term2<-factor(Legaswitch_legis_all$term2)

Legaswitch_legis_all$Model<-factor(Legaswitch_legis_all$Model, levels=c("1","2","3"),
                                   labels=c("No Controls", "With Controls", 
                                            "Incl. Noncategorized Cars"))
Legaswitch_legis_all$term3<-factor(Legaswitch_legis_all$term2, 
                                   levels =c("Euro 4", "Diesel", "Diesel Euro 4"))



# Coefplot for figure 4  a   
coefplot_2018Legislative<-
  ggplot(Legaswitch_legis_all, aes(term3, estimate,colour=term3, fill=term3))+ 
  scale_fill_manual(values = c("#B8B8B8","#B8B8B8","#821212"))+
  scale_colour_manual(values = c("#B8B8B8","#B8B8B8","#821212"))+
  geom_hline(yintercept=0, linetype="longdash", lwd=0.9, size=2, 
             colour = "#6C7B8B", alpha=1) +
  geom_errorbar(stat = "identity", alpha = 1, 
                position = position_dodge(width = 0.15),
                aes(ymin=estimate - 1.96*std.error, 
                    ymax=estimate + 1.96*std.error), 
                lwd=1.4, width=0) +
  geom_point(stat = "identity", alpha = 1,  size = 3, 
             position = position_dodge(width = 0.15)) + coord_flip() +facet_wrap(~Model)+
  theme_minimal()+theme(legend.position = "none", 
                        plot.background = element_rect(fill = "white"),
                        axis.text.x=element_text(size=14.5),
                        axis.text.y=element_text(size=14.5),
                        strip.text.x = element_text(size = 14.5),
                        panel.spacing = unit(1.7, "lines"))+
  labs(x = "", y = " ", title="",  
       colour="", fill="", shape="", group="" ) 




# Panel (b) From Regional Elections 2018 --------
# 1. no control
Legaswitch_region_lm_ms<-estimatr:: lm_robust(sw_to_lega_reg_19~dummy_diesel+dummy_euro_4+diesel_euro4, 
                                   data=data, subset=c(target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0))
# 2. with controls
Legaswitch_region_lm_cont<-estimatr:: lm_robust(sw_to_lega_reg_19~dummy_diesel+dummy_euro_4+diesel_euro4+
                                       age+female+EDU+INC, 
                                     data=data, subset=c(target!=3 & target!=4 & 
                                                                       no_answer_euro==0 & no_answer_regional==0))
# 3. Assigning Unknown-car models based on self-report of ban effect
Legaswitch_region_lm_cont_Unknowncar<-estimatr:: lm_robust(sw_to_lega_reg_19~dummy_diesel_ass+dummy_euro_4_ass+diesel_euro4_ass+
                                                  age+female+EDU+INC+dummy_car_unknown, 
                                                data=data, subset=c(target!=3 & no_answer_euro==0 & no_answer_regional==0))

# data
M.Legaswitch_region_lm_ms <-  tidy(Legaswitch_region_lm_ms, conf.int = TRUE)
M.Legaswitch_region_lm_cont <-  tidy(Legaswitch_region_lm_cont, conf.int = TRUE)
M.Legaswitch_region_lm_cont_Unknowncar <-  tidy(Legaswitch_region_lm_cont_Unknowncar, conf.int = TRUE)
Legaswitch_region_all<- bind_rows(M.Legaswitch_region_lm_ms, 
                                  M.Legaswitch_region_lm_cont, 
                                  M.Legaswitch_region_lm_cont_Unknowncar,  .id="Model")
Legaswitch_region_all<-subset(Legaswitch_region_all, term=="dummy_diesel"|term=="dummy_euro_4"|term=="diesel_euro4"|
                                term== "dummy_diesel_ass"|term=="dummy_euro_4_ass"|term=="diesel_euro4_ass") 
Legaswitch_region_all<-Legaswitch_region_all %>%
  mutate(term2=case_when(term=="dummy_diesel" |term=="dummy_diesel_ass"~"Diesel",
                         term=="dummy_euro_4" |term=="dummy_euro_4_ass"~"Euro 4",
                         term=="diesel_euro4" |term=="diesel_euro4_ass"~"Diesel Euro 4"))

Legaswitch_region_all$term2<-factor(Legaswitch_region_all$term2)

Legaswitch_region_all$Model<-factor(Legaswitch_region_all$Model, levels=c("1","2","3"),
                                    labels=c("No Controls", "With Controls", 
                                             "Incl. Noncategorized Cars"))
Legaswitch_region_all$term3<-factor(Legaswitch_region_all$term2, 
                                    levels =c("Euro 4", "Diesel", "Diesel Euro 4"))

# Coefplot for figure 4  b
coefplot_2019Regional<-
  ggplot(Legaswitch_region_all, aes(term3, estimate,colour=term3, fill=term3))+ 
  scale_fill_manual(values = c("#B8B8B8","#B8B8B8","#821212"))+
  scale_colour_manual(values = c("#B8B8B8","#B8B8B8","#821212"))+
  geom_hline(yintercept=0, linetype="longdash", lwd=0.9, size=2, 
             colour = "#6C7B8B", alpha=1) +
  geom_errorbar(stat = "identity", alpha = 1, 
                position = position_dodge(width = 0.15),
                aes(ymin=estimate - 1.96*std.error, 
                    ymax=estimate + 1.96*std.error), 
                lwd=1.4, width=0) +
  geom_point(stat = "identity", alpha = 0.7,  size = 3, 
             position = position_dodge(width = 0.15)) + coord_flip() +facet_wrap(~Model)+
  theme_minimal()+theme(legend.position = "none", plot.background = element_rect(fill = "white"),
                        axis.text.x=element_text(size=14.5),
                        axis.text.y=element_text(size=14.5),
                        strip.text.x = element_text(size = 14.5),
                        panel.spacing = unit(1.7, "lines"))+  
  labs(x = "", y = " ", title="",  
       colour="", fill="", shape="", group="" ) 



# Panel (c) From Municipal Elections 2016  ----------
# 1. no control
Legaswitch_munic_lm_ms<-estimatr:: lm_robust(sw_to_lega_16_19~dummy_diesel+dummy_euro_4+diesel_euro4, 
                                  data=data, subset=c(target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0))

# 2. with controls
Legaswitch_munic_lm_cont<-estimatr:: lm_robust(sw_to_lega_16_19~dummy_diesel+dummy_euro_4+diesel_euro4+
                                      age+female+EDU+INC, 
                                    data=data, subset=c(target!=3 & target!=4 & 
                                                                      no_answer_euro==0 & no_answer_municipal==0))

# 3. Assigning Unknown-car models based on self-report of ban effect
Legaswitch_munic_lm_cont_Unknowncar<-estimatr:: lm_robust(sw_to_lega_16_19~dummy_diesel_ass+dummy_euro_4_ass+diesel_euro4_ass+
                                                 age+female+EDU+INC+dummy_car_unknown, 
                                               data=data, subset=c(target!=3 & no_answer_euro==0 & no_answer_municipal==0))

# data
M.Legaswitch_munic_lm_ms <-  tidy(Legaswitch_munic_lm_ms, conf.int = TRUE)
M.Legaswitch_munic_lm_cont <-  tidy(Legaswitch_munic_lm_cont, conf.int = TRUE)
M.Legaswitch_munic_lm_cont_Unknowncar <-  tidy(Legaswitch_munic_lm_cont_Unknowncar, conf.int = TRUE)
Legaswitch_munic_all<- bind_rows(M.Legaswitch_munic_lm_ms, 
                                 M.Legaswitch_munic_lm_cont, 
                                 M.Legaswitch_munic_lm_cont_Unknowncar,  .id="Model")
Legaswitch_munic_all<-subset(Legaswitch_munic_all, term=="dummy_diesel"|term=="dummy_euro_4"|term=="diesel_euro4"|
                               term== "dummy_diesel_ass"|term=="dummy_euro_4_ass"|term=="diesel_euro4_ass") 
Legaswitch_munic_all<-Legaswitch_munic_all %>%
  mutate(term2=case_when(term=="dummy_diesel" |term=="dummy_diesel_ass"~"Diesel",
                         term=="dummy_euro_4" |term=="dummy_euro_4_ass"~"Euro 4",
                         term=="diesel_euro4" |term=="diesel_euro4_ass"~"Diesel Euro 4"))

Legaswitch_munic_all$term2<-factor(Legaswitch_munic_all$term2)

Legaswitch_munic_all$Model<-factor(Legaswitch_munic_all$Model, levels=c("1","2","3"),
                                   labels=c("No Controls", "With Controls", 
                                            "Incl. Noncategorized Cars"))
Legaswitch_munic_all$term3<-factor(Legaswitch_munic_all$term2, 
                                   levels =c("Euro 4", "Diesel", "Diesel Euro 4"))

# Coefplot for figure 4  c - 2016 Municipal Election 
coefplot_2016Municipal<-
  ggplot(Legaswitch_munic_all, aes(term3, estimate,colour=term3, fill=term3))+ 
  scale_fill_manual(values = c("#B8B8B8","#B8B8B8","#821212"))+
  scale_colour_manual(values = c("#B8B8B8","#B8B8B8","#821212"))+
  geom_hline(yintercept=0, linetype="longdash", lwd=0.9, 
             colour = "#6C7B8B", alpha=1) +
  
  geom_errorbar(stat = "identity", alpha = 1, 
                position = position_dodge(width = 0.15),
                aes(ymin=estimate - 1.96*std.error, 
                    ymax=estimate + 1.96*std.error), 
                lwd=1.4, width=0) +
  geom_point(stat = "identity", alpha = 0.7,  size = 3, 
             position = position_dodge(width = 0.15)) + coord_flip() +facet_wrap(~Model)+
  theme_minimal()+theme(legend.position = "none", plot.background = element_rect(fill = "white"),
                        axis.text.x=element_text(size=14.5),
                        axis.text.y=element_text(size=14.5),
                        strip.text.x = element_text(size = 14.5),
                        panel.spacing = unit(1.7, "lines"))+
  labs(x = "", y = " ", title="",  
       colour="", fill="", shape="", group="" ) 




#### What happened before? Figure 6

############
# Figure 6 #
############

# Panel (a) From municipal 2016 to legislative 2018  -----------
# 1. no control
pl16_18v2_lm_ms<-estimatr:: lm_robust(sw_to_lega_16_18~dummy_diesel+dummy_euro_4+diesel_euro4, 
                           data=Replication_data, subset=c( target!=3 & target!=4 & no_answer_2018==0 & no_answer_municipal==0 & vote_lega_municipal==0  ))
# 2. with controls
pl16_18v2_lm_cont<-estimatr:: lm_robust(sw_to_lega_16_18~dummy_diesel+dummy_euro_4+diesel_euro4+
                               age+female+EDU+INC, 
                             data=Replication_data, subset=c(target!=3 & target!=4 & no_answer_2018==0 & no_answer_municipal==0 & vote_lega_municipal==0))
# 3. Including unkown-car and assigning the treatment
pl16_18v2_lm_cont_Unkncar<-estimatr:: lm_robust(sw_to_lega_16_18~dummy_diesel_ass+dummy_euro_4_ass+diesel_euro4_ass+
                                       age+female+EDU+INC+dummy_car_unknown, 
                                     data=Replication_data, subset=c(target!=3 & no_answer_2018==0 & no_answer_municipal==0 & vote_lega_municipal==0))
## Merge all models together
M.pl16_18v2_lm_ms <-  tidy(pl16_18v2_lm_ms, conf.int = TRUE)
M.pl16_18v2_lm_cont <-  tidy(pl16_18v2_lm_cont, conf.int = TRUE)
M.pl16_18v2_lm_cont_Unkncar <-  tidy(pl16_18v2_lm_cont_Unkncar, conf.int = TRUE)

M.pl16_18v_all_main<- bind_rows(M.pl16_18v2_lm_ms, 
                                M.pl16_18v2_lm_cont, 
                                M.pl16_18v2_lm_cont_Unkncar, 
                                .id="Model")
M.pl16_18v_all_main<-subset(M.pl16_18v_all_main, term=="dummy_diesel"|term=="dummy_euro_4"|term=="diesel_euro4"|
                              term== "dummy_diesel_ass"|term=="dummy_euro_4_ass"|term=="diesel_euro4_ass") 
M.pl16_18v_all_main<-M.pl16_18v_all_main %>%
  mutate(term2=case_when(term=="dummy_diesel" |term=="dummy_diesel_ass"~"Diesel",
                         term=="dummy_euro_4" |term=="dummy_euro_4_ass"~"Euro 4",
                         term=="diesel_euro4" |term=="diesel_euro4_ass"~"Diesel Euro 4"))

M.pl16_18v_all_main$term2<-factor(M.pl16_18v_all_main$term2)
M.pl16_18v_all_main$Model<-factor(M.pl16_18v_all_main$Model, levels=c("1","2","3"),
                                  labels=c("No Controls", "With Controls", 
                                           "Incl. Noncategorized Cars"))
M.pl16_18v_all_main$term2<-relevel(M.pl16_18v_all_main$term2, ref="Diesel Euro 4")
M.pl16_18v_all_main$term2<-relevel(M.pl16_18v_all_main$term2, ref="Euro 4")
M.pl16_18v_all_main$term3<-factor(M.pl16_18v_all_main$term2, levels =c("Euro 4", "Diesel", "Diesel Euro 4"))

## coefplot
M.pl16_18v_all_mainsub_Sub<-subset(M.pl16_18v_all_main, term2=="Diesel Euro 4") 
M.pl16_18v_all_mainsub_Sub$Model2<-factor(M.pl16_18v_all_mainsub_Sub$Model, levels =c("Incl. Noncategorized Cars",
                                                                                      "With Controls", "No Controls"))

# Panel (b) From municipal 2016 to regional 2018  ----------
# 1. no control
pl16_regv2_lm_ms<-estimatr:: lm_robust(sw_to_lega_16_reg~dummy_diesel+dummy_euro_4+diesel_euro4, 
                            data=Replication_data, subset=c( target!=3 & target!=4 & no_answer_regional==0 & no_answer_municipal==0 & vote_lega_municipal==0  ))
# 2. with controls
pl16_regv2_lm_cont<-estimatr:: lm_robust(sw_to_lega_16_reg~dummy_diesel+dummy_euro_4+diesel_euro4+
                                age+female+EDU+INC, 
                              data=Replication_data, subset=c(target!=3 & target!=4 & no_answer_regional==0 & no_answer_municipal==0 & vote_lega_municipal==0))
# 3. Including unkown-car and assigning the treatment
pl16_regv2_lm_cont_Unkncar<-estimatr:: lm_robust(sw_to_lega_16_reg~dummy_diesel_ass+dummy_euro_4_ass+diesel_euro4_ass+
                                        age+female+EDU+INC+dummy_car_unknown, 
                                      data=Replication_data, subset=c(target!=3 & no_answer_regional==0 & no_answer_municipal==0 & vote_lega_municipal==0))

## Merge all models together
M.pl16_regv2_lm_ms <-  tidy(pl16_regv2_lm_ms, conf.int = TRUE)
M.pl16_regv2_lm_cont <-  tidy(pl16_regv2_lm_cont, conf.int = TRUE)
M.pl16_regv2_lm_cont_Unkncar <-  tidy(pl16_regv2_lm_cont_Unkncar, conf.int = TRUE)

M.pl16_regv_all_main<- bind_rows(M.pl16_regv2_lm_ms, 
                                 M.pl16_regv2_lm_cont, 
                                 M.pl16_regv2_lm_cont_Unkncar, 
                                 .id="Model")
M.pl16_regv_all_main<-subset(M.pl16_regv_all_main, term=="dummy_diesel"|term=="dummy_euro_4"|term=="diesel_euro4"|
                               term== "dummy_diesel_ass"|term=="dummy_euro_4_ass"|term=="diesel_euro4_ass") 
M.pl16_regv_all_main<-M.pl16_regv_all_main %>%
  mutate(term2=case_when(term=="dummy_diesel" |term=="dummy_diesel_ass"~"Diesel",
                         term=="dummy_euro_4" |term=="dummy_euro_4_ass"~"Euro 4",
                         term=="diesel_euro4" |term=="diesel_euro4_ass"~"Diesel Euro 4"))

M.pl16_regv_all_main$term2<-factor(M.pl16_regv_all_main$term2)

M.pl16_regv_all_main$Model<-factor(M.pl16_regv_all_main$Model, levels=c("1","2","3"),
                                   labels=c("No Controls", 
                                            "With Controls", 
                                            "Incl. Noncategorized Cars"))
M.pl16_regv_all_main$term2<-relevel(M.pl16_regv_all_main$term2, ref="Diesel Euro 4")
M.pl16_regv_all_main$term2<-relevel(M.pl16_regv_all_main$term2, ref="Euro 4")
M.pl16_regv_all_main$term3<-factor(M.pl16_regv_all_main$term2, levels =c("Euro 4", 
                                                                         "Diesel", 
                                                                         "Diesel Euro 4"))
M.pl16_regv_all_mainsub_Sub<-subset(M.pl16_regv_all_main, term2=="Diesel Euro 4") 
M.pl16_regv_all_mainsub_Sub$Model2<-factor(M.pl16_regv_all_mainsub_Sub$Model, 
                                           levels =c("Incl. Noncategorized Cars",
                                                     "With Controls", "No Controls"))

# Coefplot 
switch_16_19_placebo<- bind_rows(M.pl16_18v_all_mainsub_Sub, 
                                 M.pl16_regv_all_mainsub_Sub, 
                                 .id="election18")

switch_16_19_placebo$election18<-factor(switch_16_19_placebo$election18)

switch_16_19_placebo$election18<-factor(switch_16_19_placebo$election18, levels=c("1","2"),
                                        labels=c("(a) From Municipal 2016 to Legislative 2018", 
                                                 "(b) From Municipal 2016 to Regional 2018"))

coef_switch_16_19_placebo<- ggplot(switch_16_19_placebo, aes(Model2, estimate))+ 
  scale_y_continuous(limits = c(-0.3, 0.4))+
  geom_hline(yintercept=0, linetype="dashed", lwd=1.2, 
             colour = "#6E7B8B", alpha=0.7) +
  geom_errorbar(stat = "identity", alpha = 0.8, 
                position = position_dodge(width = 0.3),
                aes(ymin=estimate - 1.96*std.error, 
                    ymax=estimate + 1.96*std.error),
                lwd=1.5, width = 0,  colour="#821212",)+
  geom_point(stat = "identity", alpha = 0.7,  size = 3.5,  colour="#821212",
             position = position_dodge(width = 0.15)) + coord_flip() +
  theme_minimal()+theme(legend.position = "none", plot.background = element_rect(fill = "white"),
                        axis.text.x=element_text(size=13, colour = "#000000"),
                        axis.text.y=element_text(size=13.5, colour = "#000000"),
                        strip.text.x = element_text(size=16.5, colour = "#000000"),
                        panel.spacing = unit(2, "lines"))+
  labs(x = "", y = " ", title=" ",  
       colour="", fill="", shape="", group="" ) + facet_wrap(facets=~election18,nrow=1)
