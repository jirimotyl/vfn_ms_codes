#Short code for creating spaghetti plots of EDSS longitudinal lines (each patient = one line)
##(c) Jiří Motýl (jiri.motyl@vfn.cz)
##2022-10-15

getwd()

#packages needed
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("plyr")
install.packages("janitor")


library(tidyverse)
library(lubridate)
library(dplyr)
library(plyr)
library(janitor)


#loading data from iMed
spaghetti_data_visit <- read_delim("export_imed/221011_imed_all_VI.txt", 
                                   col_select = c("Patient ID", "Visit Date", "EDSS"),
                                   na = "", 
                                   guess_max = (5000),
                                   col_types= list("Patient ID" = col_character(), "Visit Date" = col_date("%d. %m. %Y")),
                                   
)
spec(spaghetti_data_visit)

spaghetti_data_visit_2 <- read_delim("export_imed/221011_imed_all_FF_VI_2.txt", 
                                   col_select = c("Patient ID", "Event Date", "SDMT", "PST (CogEval)", "CVLT-II TL ", "RAVLT", "BVMT-R TL"),
                                   na = "", 
                                   guess_max = (5000),
                                   col_types= list("Patient ID" = col_character(), "Event Date" = col_date("%d. %m. %Y"), "PST (CogEval)" = col_double()),
                                   
)
spec(spaghetti_data_visit_2)

spaghetti_data_id <- read_delim("export_imed/221011_imed_all_ID.txt", 
                                col_select = c("Patient ID", "Birth Date", "Clinical Study Code", "Date of onset"),
                                na = "", 
                                guess_max = (5000),
                                col_types= list("Patient ID" = col_character(), "Birth Date" = col_date("%d. %m. %Y"), "Date of onset" = col_date("%d. %m. %Y")),
                                
)
spec(spaghetti_data_id)

#tidy data
##EDSS change to numeric value (decimal sign: column -> point)
spaghetti_data_visit$EDSS <- str_replace_all(spaghetti_data_visit$EDSS, ",",".")
spaghetti_data_visit$EDSS <- as.numeric(spaghetti_data_visit$EDSS)

##cleaning variable names
spaghetti_data_id <- spaghetti_data_id %>%
  clean_names()
spaghetti_data_visit <- spaghetti_data_visit %>%
  clean_names()
spaghetti_data_visit_2 <- spaghetti_data_visit_2 %>%
  clean_names()
spaghetti_data_visit_2 <- dplyr::rename(spaghetti_data_visit_2, 
                                 visit_date = event_date)

##Joining various csv dataframes into one
spaghetti_data_v01 <- left_join(spaghetti_data_visit, spaghetti_data_id, by = "patient_id")
spaghetti_data <- left_join(spaghetti_data_v01, spaghetti_data_visit_2, by = c("patient_id", "visit_date"))


##slecting specific study participants (fill in "ASA", "SET", "GQ", etc.)
spaghetti_data_plot <- spaghetti_data %>% filter(str_detect(clinical_study_code, 'ASA'))


#computing disease duration
##days
spaghetti_data_plot$duration_days <- difftime(spaghetti_data_plot$visit_date,spaghetti_data_plot$date_of_onset, units = "days")
##weeks
spaghetti_data_plot$duration_weeks <- difftime(spaghetti_data_plot$visit_date,spaghetti_data_plot$date_of_onset, units = "weeks")
##years
spaghetti_data_plot$duration <- ((trunc((spaghetti_data_plot$date_of_onset %--% spaghetti_data_plot$visit_date) / days(1)))/365)


#plots
##addtional function for plot_03 graphics
spaghetti_data_plot = ddply(spaghetti_data_plot, .(patient_id), function(x){
  x$alpha_value = ifelse(runif(n = 1) > 0.9, 1, 0.1)
  x$grouper = factor(rbinom(n=1, size =3 ,prob=0.5), levels=0:3)
  x
})

##main spaghetti plot without graphics - line
plot_01 = ggplot(spaghetti_data_plot, aes(x=duration, y=edss)) + 
  geom_line() + guides(colour="none") + xlab("Disease Duration (Years)") +
  ylab("EDSS")

###line spaghetti plot with colours
plot_02 = plot_01 + aes(colour = factor(patient_id))
plot_02

###line spaghetti plot b&w with random variables highlighted
plot_03 = plot_01 + aes(alpha=alpha_value, group=factor(patient_id)) + guides(alpha="none")
plot_03

###line spaghetti plot with colours and central line
plot_04 = plot_02 + geom_smooth(se=FALSE, colour="black", size=2)
plot_04

##main spaghetti plot without graphics - smooth (Span sets the smoothing level, study "SET" needs at least 1 (minimum))
plot_05 = ggplot(spaghetti_data_plot, aes(x=duration, y=edss)) + 
  geom_smooth(se=FALSE, span=0.7) + guides(colour="none") + xlab("Disease Duration (Years)") +
  ylab("EDSS")

###smooth spaghetti plot with colours
plot_06 = plot_05 + aes(colour = factor(patient_id))
plot_06

###smooth spaghetti plot with colours and central line
plot_07 = plot_06 + geom_smooth(se=FALSE, colour="black", size=2)
plot_07

#checking the number of participants throughout the analysis
length(unique(plot_07$data$patient_id))
length(unique(spaghetti_data_plot$patient_id))


