library(tidyverse)
library(psych)
library(openxlsx)
library(dplyr)
library(conflicted)
library(FactoMineR)
library(factoextra)
library(missMDA) # for conducting imputation of missing data
library(glmnet)
library(reshape)
library(sjPlot)
library(ggcorrplot)
library(ggpubr)
library(car)
library(Hmisc)
library(PupillometryR)
library(ggpp)
library(ggdist)
library(gghalves)
library(cowplot)

# remove environment (optional)
rm(list = ls())

set.seed(123)


#### Read in survey data ####

# read in .csv from directory
MRI<-read.table("C:/Users/aleya/OneDrive/Desktop/Cog Modelling Ageing Project/CleanDataFiles/clean_survey.csv",header=TRUE,sep=",")

##### Imputation #####
MRI_EFA <- MRI %>%
  dplyr::select("Education", "Ladder", "CESDsum", "STAIsum", "EWsum", "SleepHours", "SleepQuality", 
                "SocialActivity", "FoodIntake", "FISum", "Late_PhysicalDays","Mid_PhysicalDays",
                "English", "Young_PhysicalDays",
                "No_SpokenLanguage", "Occupation_Code", 
                "GovSupport", "GeneralHealth", "BloodPressure", "Cholesterol", 
                "Diabetes", "Late_BookNo", "Mid_BookNo", "Young_BookNo", "Internet_Frequency",
                "Late_News", "Late_Radio", "Late_Video", "Late_Books", "Late_Games", "Late_Live", 
                "Late_Movies", "Late_ArtClass", "Late_HobbyClass",
                "SWLS_Sum", "Crowding",
                "Mid_BookNo", "Mid_News", "Mid_Radio", "Mid_Books", 
                "Mid_Games", "Mid_Live", "Mid_Movies", "Mid_ArtClass", 
                "Mid_HobbyClass", 
                "Young_BookNo", "Young_News", "Young_Radio",
                "Young_Books", "Young_Games", "Young_Live", "Young_Movies", "Young_ArtClass", 
                "Young_HobbyClass",
                "SWLS_Sum", "Crowding",
                "LateAdulthood_HousingType","VerbalFluency",
                "Adulthood_PerceivedSES_one", "Adulthood_PerceivedSES_two", 
                "Childhood_PerceivedSES_one", "Childhood_PerceivedSES_two") 

MRI_EFA<-subset(MRI, select = -c(ParticipantID))

res.imput<-imputeFAMD(MRI_EFA, ncp=5) #conducts imputation

MRI_EFA_complete<-res.imput$completeObs

# get only number for some columns 

# convert to character first
MRI_EFA_complete<-MRI_EFA_complete %>% mutate_at(c("Education","SleepQuality" ,"Ladder",
                                                   'Internet_Frequency', "SocialActivity",
                                                   "FoodIntake",
                                                   'Late_PhysicalDays', "English", 'Mid_PhysicalDays','Young_PhysicalDays',
                                                   'Occupation_Code', "GovSupport",
                                                   "GeneralHealth", "BloodPressure",
                                                   "Cholesterol", "Diabetes", "Late_BookNo",  "Mid_BookNo", "Young_BookNo",
                                                   "Internet_Frequency", "LateAdulthood_HousingType", "VerbalFluency",
                                                   "Late_News", "Late_Radio", "Late_Video", "Late_Books", "Late_Games", "Late_Live", 
                                                   "Late_Movies", "Late_ArtClass", "Late_HobbyClass",
                                                   "Mid_BookNo", "Mid_News", "Mid_Radio", "Mid_Books", 
                                                   "Mid_Games", "Mid_Live", "Mid_Movies", "Mid_ArtClass", 
                                                   "Mid_HobbyClass", 
                                                   "Young_BookNo", "Young_News", "Young_Radio",
                                                   "Young_Books", "Young_Games", "Young_Live", "Young_Movies", "Young_ArtClass", 
                                                   "Young_HobbyClass","Adulthood_PerceivedSES_one", "Adulthood_PerceivedSES_two", "Childhood_PerceivedSES_one", "Childhood_PerceivedSES_two"), as.character)

#parse number
MRI_EFA_complete<-MRI_EFA_complete %>% mutate_at(c("Education","SleepQuality" ,"Ladder",
                                                   'Internet_Frequency', "SocialActivity",
                                                   "FoodIntake",
                                                   'Late_PhysicalDays', "English",'Mid_PhysicalDays','Young_PhysicalDays',
                                                   'Occupation_Code', "GovSupport",
                                                   "GeneralHealth", "BloodPressure",
                                                   "Cholesterol", "Diabetes", "Late_BookNo",  "Mid_BookNo", "Young_BookNo",
                                                   "Internet_Frequency",
                                                  "LateAdulthood_HousingType", "VerbalFluency",
                                                   "Late_News", "Late_Radio", "Late_Video", "Late_Books", "Late_Games", "Late_Live", 
                                                   "Late_Movies", "Late_ArtClass", "Late_HobbyClass",
                                                   "Mid_BookNo", "Mid_News", "Mid_Radio", "Mid_Books", 
                                                   "Mid_Games", "Mid_Live", "Mid_Movies", "Mid_ArtClass", 
                                                   "Mid_HobbyClass", 
                                                   "Young_BookNo", "Young_News", "Young_Radio",
                                                   "Young_Books", "Young_Games", "Young_Live", "Young_Movies", "Young_ArtClass", 
                                                   "Young_HobbyClass", "Adulthood_PerceivedSES_one", "Adulthood_PerceivedSES_two", "Childhood_PerceivedSES_one", "Childhood_PerceivedSES_two"), parse_number)

# saving for ridge regression
survey_complete<-MRI_EFA_complete

set_theme(base = theme_bw())

#### FA ####

FA_survey<- sapply(survey_complete, as.numeric)

FA_survey<-scale(FA_survey)

library(psych)
cortest.bartlett(FA_survey) # significant, can proceed
KMO(FA_survey) #good range

scree(FA_survey, pc=FALSE)  # 4 factors

Nfacs <- 5  

fit <- factanal(FA_survey, Nfacs, rotation="promax", scores = 'Bartlett')
FAScores<-as.data.frame(fit$scores) #get scores per participant 

colnames(FAScores)[1:5] <- c("Education_SES", "CognitiveStimulation", "Hobbies",
                         "MentalHealth", "ChildSES")

loads<-as.data.frame(unclass(fit$loadings))

#cut-off for FA: https://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/thresholds

print(fit, digits=2, cutoff=0.3, sort=TRUE)

# get variance 
x <- loadings(fit)
vx <- colSums(x^2)

rbind(`SS loadings` = vx,
      `Proportion Var` = vx/nrow(x),
      `Cumulative Var` = cumsum(vx/nrow(x)))


##### plot loadings #####

# change row Names
rownames(loads) <- c("Education.Level", "Perceived.Current.SES", "Depression", "Anxiety",
                     "Financial.Worries", "Sleep.Hours", "Sleep.Quality", "Social.Activity",
                     "Food.Intake.Changes", "Food.Insecurity", "Physical.Activity.Late.Adult",
                     "Physical.Activity.Mid.Adult", "English.Speaking", "Physical.Activity.Child",
                     "Num.Spoken.Languages", "Occupational.Complexity", "Government.Support", "Health.Condition",
                     "High.Blood.Pressure", "High.Cholesterol", "Diabetic", "Num.Books.Late.Adult",
                     "Num.Books.Mid.Adult", "Num.Books.Child", "Internet.Usage.Frequency", "Read.News.Late.Adult",
                     "Radio.Late.Adult", "Watch.Videos.Late.Adult", 'Read.Books.Late.Adult', "Play.Boardgames.Late.Adult",
                     "Attend.Live.Shows.Late.Adult", "Watch.Movies.Late.Adult", "Attend.Art.Class.Late.Adult","Attend.Hobby.Class.Late.Adult",
                     "Life.Satisfaction", "Crowding.Index",
                     "Read.News.Mid.Adult","Radio.Mid.Adult", 'Read.Books.Mid.Adult', "Play.Boardgames.Mid.Adult",
                     "Attend.Live.Shows.Mid.Adult", "Watch.Movies.Mid.Adult", "Attend.Art.Class.Mid.Adult",
                     "Attend.Hobby.Class.Mid.Adult","Read.News.Child","Radio.Child", 'Read.Books.Child', "Play.Boardgames.Child",
                     "Attend.Live.Shows.Child", "Watch.Movies.Child", "Attend.Art.Class.Child",
                     "Attend.Hobby.Class.Child","Current.Housing.Type", "Verbal.Fluency", "Perceived.Mid.Adult.SES1", "Perceived.Mid.Adult.SES2",
                     "Perceived.Child.SES1", "Perceived.Child.SES2")

loads$Variables <- row.names(loads) # convert row names to a column



colnames(loads)[1:5] <- c("F1:SES & Cognitive Reserve",
                          "F2:Cognitive & Physical Stimulation",
                          "F3:Hobbies",
                          "F4:Mental Health Symptoms",
                          "F5:Childhood SES")

   

loadings.m <- melt(loads, id="Variables", 
                   measure=c("F1:SES & Cognitive Reserve",
                             "F2:Cognitive & Physical Stimulation",
                             "F3:Hobbies",
                             "F4:Mental Health Symptoms",
                             "F5:Childhood SES"))

colnames(loadings.m)[2]<-"Factor"
colnames(loadings.m)[3]<-"Loading"


fa_survey_plot<-ggplot(loadings.m, aes(Variables, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide="colourbar") +
  ylab("Loading Strength") + #improve y-axis label
  theme_bw(base_size=10)+
  theme(axis.title.y=element_blank())
  

tiff(file="C:/Users/aleya/OneDrive/Documents/Tasks/plots/fa_survey_plot.tiff",width = 12, height = 8, units = "in", res = 300)
fa_survey_plot
dev.off()



#### Read in cognitive and brain data ####
#wcst data
WCST_data<-read.csv('C:/Users/aleya/OneDrive/Desktop/Cog Modelling Ageing Project/CleanDataFiles/forWCSTMRIAnalysis.csv', header=TRUE,sep=",")
WCST_data <- subset(WCST_data, select =  c( subject_id, correct, persev_err, non_persev_err,
                                           unique_error, trials_to_complete_cat1,
                                           cat_completed, r, p, d, f, PC1, PC2, PC3, PC4, MRI, GenderScore,
                                           Age, Ethnicity))
#gng data
GNG_data<-read.csv('C:/Users/aleya/OneDrive/Desktop/Cog Modelling Ageing Project/CleanDataFiles/forGnGMRIAnalysis.csv', header=TRUE,sep=",")
GNG_data <- subset(GNG_data, select =  c(subject_id, go_errors,
                                         no_go_errors, go_rt, no_go_rt, a, v, t, z,
                                         v.0., v.1., PC1, PC2, PC3, MRI, GenderScore,
                                         Age, Ethnicity))

#brain age
brainAge<-read.csv('C:/Users/aleya/OneDrive/Desktop/Cog Modelling Ageing Project/CleanDataFiles/brainAge.csv', header=TRUE,sep=",")
colnames(brainAge)[1] <- c("subject_id")
brainAge <- subset(brainAge, select =  c(subject_id, brain.predicted_age))

#globals
#grey and white matter volume
gm<-read.csv('C:/Users/aleya/OneDrive/Desktop/Cog Modelling Ageing Project/CleanDataFiles/MRI_globals.csv', header=TRUE,sep=",")
gm <- subset(gm, select =  c(subject_id, GreyMatter, WhiteMatter))

brainvars<- merge (brainAge, gm, by = 'subject_id')

#wcst
brainvars_matching_wcst <-semi_join(brainvars,WCST_data, by=("subject_id"))

WCST_brain<- merge (brainvars_matching_wcst, WCST_data, by = 'subject_id')

# to get a measure of how much the brain has aged compared to actual age
# the higher the worse the deterioration
WCST_brain$predMinusActual <- WCST_brain$brain.predicted_age - WCST_brain$Age

#gng
brainvars_matching_gng <-semi_join(brainvars,GNG_data, by=("subject_id"))

GNG_brain<- merge (brainvars_matching_gng, GNG_data, by = 'subject_id')

GNG_brain$predMinusActual <- GNG_brain$brain.predicted_age - GNG_brain$Age

#survey variables with completed cases 
#add back IDs
survey_complete$subject_id <- MRI$ParticipantID
FAScores$subject_id<-MRI$ParticipantID


# gng with survey 
survey_matching_gng <-semi_join(survey_complete,GNG_brain, 
                                by=("subject_id"))

gng_matching_survey <-semi_join(GNG_brain,survey_complete, by=("subject_id"))

GNG_survey<- merge(gng_matching_survey,survey_matching_gng, by = 'subject_id')
GNG_survey<-merge(GNG_survey, FAScores, by = 'subject_id')

# wcst with survey
survey_matching_wcst <-semi_join(survey_complete,WCST_brain, 
                                by=("subject_id"))

wcst_matching_survey <-semi_join(WCST_brain,survey_complete, 
                                 by=("subject_id"))

wcst_survey<- merge(wcst_matching_survey,survey_matching_wcst, 
                              by = 'subject_id')

wcst_survey<-merge(wcst_survey, FAScores, by = 'subject_id')

#### Linear regs between demographic variables and profiles ####

#dummy coding for ethnicity
# Load the library
library(fastDummies)

# Create dummy variable
all_gng <- dummy_cols(GNG_survey, 
                   select_columns = "Ethnicity")

all_wcst <- dummy_cols(wcst_survey, 
                      select_columns = "Ethnicity")

# gng

dvList<-c("PC1", "PC2", "PC3", "v", "v.0.", "v.1.","a","t","z")

gng_lm_models_demo <- list()
gng_html_model_demo<- list()


for (m in 1:length(dvList)) {
  
  model<-lm(as.formula(paste(dvList[[m]],"~ LateAdulthood_HousingType + GenderScore + Age + Ethnicity_Chinese + Ethnicity_Malay + Ethnicity_Indian")), data=all_gng)
  
  gng_lm_models_demo[[m]]<-model
  
  gng_html_model_demo[[m]]<-tab_model(model, show.se = TRUE)
  
  
}


#wcst

dvList<-c("PC1", "PC2", "PC3", "PC4", "r", "p", "d", "f")

wcst_lm_models_demo <- list()
wcst_html_model_demo<- list()


for (m in 1:length(dvList)) {
  
  model<-lm(as.formula(paste(dvList[[m]],"~ LateAdulthood_HousingType + GenderScore + Age + Ethnicity_Chinese + Ethnicity_Malay + Ethnicity_Indian")), data=all_wcst)
  
  wcst_lm_models_demo[[m]]<-model
  
  wcst_html_model_demo[[m]]<-tab_model(model, show.se = TRUE)
  
  
}

#### Corrected p-values (FWER) for demographics ####

IVWCSTPC_pvals<-list()
IVWCSTparams_pvals<-list()
IVGNGPC_pvals<-list()
IVGNGparams_pvals<-list()

for (m in 1:7) {
  IVWCSTPC_pvals[[m]]<-c(summary(wcst_lm_models_demo[[1]])$coefficients[m,4],
                         summary(wcst_lm_models_demo[[2]])$coefficients[m,4],
                         summary(wcst_lm_models_demo[[3]])$coefficients[m,4],
                         summary(wcst_lm_models_demo[[4]])$coefficients[m,4])
  
  IVWCSTparams_pvals[[m]]<-c(summary(wcst_lm_models_demo[[5]])$coefficients[m,4],
                             summary(wcst_lm_models_demo[[6]])$coefficients[m,4],
                             summary(wcst_lm_models_demo[[7]])$coefficients[m,4],
                             summary(wcst_lm_models_demo[[8]])$coefficients[m,4])
  
  IVGNGPC_pvals[[m]]<-c(summary(gng_lm_models_demo[[1]])$coefficients[m,4],
                        summary(gng_lm_models_demo[[2]])$coefficients[m,4],
                        summary(gng_lm_models_demo[[3]])$coefficients[m,4])
  
  IVGNGparams_pvals[[m]]<-c(summary(gng_lm_models_demo[[4]])$coefficients[m,4],
                            summary(gng_lm_models_demo[[5]])$coefficients[m,4],
                            summary(gng_lm_models_demo[[6]])$coefficients[m,4],
                            summary(gng_lm_models_demo[[7]])$coefficients[m,4],
                            summary(gng_lm_models_demo[[8]])$coefficients[m,4],
                            summary(gng_lm_models_demo[[9]])$coefficients[m,4])
  
  
}

# adjust p-values

adj_p_wcst_PCs<-list()
adj_p_wcst_params<-list()
adj_p_gng_PCs<-list()
adj_p_gng_params<-list()

for (m in 1:7) {
  
  adj_p_wcst_PCs[[m]]<-p.adjust(IVWCSTPC_pvals[[m]], method="holm")
  
  adj_p_wcst_params[[m]]<-p.adjust(IVWCSTparams_pvals[[m]], method="holm")
  
  adj_p_gng_PCs[[m]]<-p.adjust(IVGNGPC_pvals[[m]], method="holm")
  
  adj_p_gng_params[[m]]<-p.adjust(IVGNGparams_pvals[[m]], method="holm")
  
}

# order of p-values: intercept, house type, gender, age, chinese, malay, indian
# order of gng params: "v", "v.0.", "v.1.","a","t","z"

##### demographic figures ######

#Age and PCs

WCSTdata_long <- gather(wcst_survey, PC, PCScore, PC1:PC4, factor_key=TRUE)
GNGdata_long <- gather(GNG_survey, PC, PCScore, PC1:PC3, factor_key=TRUE)

set_theme(base = theme_bw())

age_gng<-ggplot(GNGdata_long, aes(x=Age, y=PCScore, group=PC, color = PC)) +
  geom_smooth(method = lm)+
  xlab("Age") + ylab("PC Score") + ggtitle("A. GnG Profiles by Age")+ labs(color=NULL)

age_wcst<-ggplot(WCSTdata_long, aes(x=Age, y=PCScore, group=PC, color = PC)) +
  geom_smooth(method = lm)+
  xlab("Age") + ylab("PC Score") + ggtitle("B. WCST Profiles by Age")+  labs(color=NULL)

age_plot<-ggarrange(age_gng, age_wcst, nrow = 2, ncol = 1)


# bar plot of profiles by late adult housing

#gng
GNGdata_long$LateAdulthood_HousingType[GNGdata_long$LateAdulthood_HousingType==1]<- "Middle-to-High Cost"
GNGdata_long$LateAdulthood_HousingType[GNGdata_long$LateAdulthood_HousingType==0]<- "Low Cost"

gng_summary <- GNGdata_long %>%
  group_by(PC,LateAdulthood_HousingType) %>%
  summarise(
    sd = sd(PCScore, na.rm=TRUE),
    mean = mean(PCScore, na.rm=TRUE),
    se = sd/sqrt(n_distinct(subject_id))
  )


housing_gng<-ggplot(GNGdata_long, aes(x = LateAdulthood_HousingType, y = PCScore, fill = PC)) +
  geom_flat_violin(aes(fill = PC),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  #geom_point(aes(x = LateAdulthood_HousingType, y = PCScore, colour = PC),position = position_jitter(width = .01), size = 3, shape = 20, alpha = 0.5)+
  geom_boxplot(aes(x = LateAdulthood_HousingType, y = PCScore, fill = PC), position = position_dodgenudge(width= 0.3,x=-.2), outlier.shape = NA, alpha = .5, width = .3, colour = "black") +
  geom_line(data = gng_summary, aes(x = LateAdulthood_HousingType, y = mean, group = PC, colour = PC), linetype = 3, position = position_nudge(x=0.1))+
  geom_point(data = gng_summary, aes(x = LateAdulthood_HousingType, y = mean, group = PC, colour = PC), shape = 18, position = position_nudge(x=0.1)) +
  geom_errorbar(data = gng_summary, aes(x = LateAdulthood_HousingType, y = mean, group = PC, colour = PC, ymin = mean-se, ymax = mean+se), width = .05, position = position_nudge(x=0.1))+
  #scale_colour_brewer(palette = "Dark2")+
  #scale_fill_brewer(palette = "Dark2")+
  labs(y = expression(paste ("PC Score")))+ 
  scale_x_discrete(labels= c('Low Cost', 'Middle-to-High Cost'))+
  labs(linetype = "PC", color = "PC")+
  theme_bw()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15), 
        axis.title.y = element_text(size = 15),
        #panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(), 
        axis.title.x=element_blank(), legend.position='none')+
  ggtitle("C. GnG Profiles by Housing Type")+
  theme(plot.title = element_text(size = 13))+
  ggplot2::annotate("text", x = 1.6, y = 4, label = "**Low-Cost Housing ~ ↑GnG-PC2", size=4)

housing_gng

#wcst

WCSTdata_long$LateAdulthood_HousingType[WCSTdata_long$LateAdulthood_HousingType==1]<- "Middle-to-High Cost"
WCSTdata_long$LateAdulthood_HousingType[WCSTdata_long$LateAdulthood_HousingType==0]<- "Low Cost"


wcst_summary <- WCSTdata_long %>%
  group_by(PC,LateAdulthood_HousingType) %>%
  summarise(
    sd = sd(PCScore, na.rm=TRUE),
    mean = mean(PCScore, na.rm=TRUE),
    se = sd/sqrt(n_distinct(subject_id))
  )


housing_wcst <- ggplot(WCSTdata_long, aes(x = LateAdulthood_HousingType, y = PCScore, fill = PC)) +
  geom_flat_violin(aes(fill = PC), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA) +
  #geom_point(aes(x = LateAdulthood_HousingType, y = PCScore, colour = PC), position = position_jitter(width = .01), size = 3, shape = 20, alpha = 0.5) +
  geom_boxplot(aes(x = LateAdulthood_HousingType, y = PCScore, fill = PC), position = position_dodgenudge(width= 0.3,x=-.2), outlier.shape = NA, alpha = .5, width = .3, colour = "black") +
  geom_line(data = wcst_summary, aes(x = LateAdulthood_HousingType, y = mean, group = PC, colour = PC), linetype = 3, position = position_nudge(x = 0.1)) +
  geom_point(data = wcst_summary, aes(x = LateAdulthood_HousingType, y = mean, group = PC, colour = PC), shape = 18, position = position_nudge(x = 0.1)) +
  geom_errorbar(data = wcst_summary, aes(x = LateAdulthood_HousingType, y = mean, group = PC, colour = PC, ymin = mean - se, ymax = mean + se), width = .05, position = position_nudge(x = 0.1)) +
  labs(y = expression(paste("PC Score"))) + 
  scale_x_discrete(labels = c('Low Cost', 'Middle-to-High Cost')) +
  labs(linetype = "PC", color = "PC") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15), 
        axis.title.y = element_text(size = 15),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = 'none') +
  ggtitle("D. WCST Profiles by Housing Type") +
  theme(plot.title = element_text(size = 13)) +
  ggplot2::annotate("text", x = 1.5, y = 14, label = "**Low-Cost Housing ~ ↑WCST-PC3", size = 4) +
  ggplot2::annotate("text", x = 1.5, y = 12, label = "**Middle-to-High Cost Housing ~ ↑WCST-PC1/PC2/PC4", size = 4)


housing_wcst



housing_plot<-ggarrange(housing_gng, housing_wcst, nrow = 2, ncol = 1)

demo_plot<-ggarrange(age_plot, housing_plot)


tiff(file="C:/Users/aleya/OneDrive/Documents/Tasks/plots/demog_plot.tiff",width = 15, height = 10, units = "in", res = 300)
demo_plot
dev.off()

#Age and parameters
WCSTdata_long <- gather(all_wcst, parameter, ParamScore, r:f, factor_key=TRUE)
GNGdata_long <- gather(all_gng, parameter, ParamScore, a:v.1., factor_key=TRUE)

set_theme(base = theme_bw())

age_gng<-ggplot(GNGdata_long, aes(x=Age, y=ParamScore, group=parameter, color = parameter)) +
  geom_smooth(method = lm)+
  xlab("Age") + ylab("Parameter Value") + ggtitle("A. GnG Parameters by Age")+ labs(color=NULL)

age_wcst<-ggplot(WCSTdata_long, aes(x=Age, y=ParamScore, group=parameter, color = parameter)) +
  geom_smooth(method = lm)+
  xlab("Age") + ylab("Parameter Value") + ggtitle("B. WCST Parameters by Age")+  labs(color=NULL)

age_plot<-ggarrange(age_gng, age_wcst, nrow = 2, ncol = 1)


#### Linear Regs for profiles and survey FA variables ####

##### GnG #####

dvList<-c("PC1", "PC2", "PC3", "v", "v.0.", "v.1.","a","t","z")

# convert ethnicity to numeric to regress this out
GNG_survey$Ethnicity<-as.numeric(as.factor
                                           (GNG_survey$Ethnicity))

# scale other variables 
GNG_survey[c("GenderScore", "Age", "Ethnicity",
                       "predMinusActual", "GreyMatter", 
                       "Education_SES", "CognitiveStimulation", "Hobbies", "MentalHealth", 
                       "ChildSES")]<- 
  lapply(GNG_survey[c("GenderScore", "Age", "Ethnicity", 
                              "predMinusActual", "GreyMatter", 
                              "Education_SES", "CognitiveStimulation", "Hobbies", "MentalHealth", 
                              "ChildSES")], scale)


gng_lm_models <- list()
gng_html_model <- list()
gng_vif_list<-list()

for (m in 1:length(dvList)) {
  
model<-lm(as.formula(paste(dvList[[m]],"~ GenderScore + Age + Ethnicity + predMinusActual+ GreyMatter + Education_SES + CognitiveStimulation + Hobbies + MentalHealth + ChildSES")), data=GNG_survey)

gng_lm_models[[m]]<-model

gng_html_model[[m]]<-tab_model(model, show.se = TRUE,
                               pred.labels = c("(Intercept)", 
                                               "Gender", "Age",
                                               "Ethnicity","BrainAge - ActualAge", 
                                               "GMV", 
                                               "F1:SES & Cognitive Reserve",
                                               "F2:Cognitive & Physical Stimulation",
                                               "F3:Hobbies",
                                               "F4:Mental Health Symptoms",
                                               "F5:Childhood SES"))

gng_vif_list[[m]]<-vif(gng_lm_models[[m]])

}

set_theme(base = theme_bw())

#### plot regression results #####
dvList<-c("PC1: Well-Performing", "PC2: Slow Responding", 
          "PC3: Random Responding","v", "v.0.", "v.1.","a","t","z")

pc_reg_gng<-list()

for (j in 1:length(dvList)) {
gng_lm<-plot_model(gng_lm_models[[j]], title = dvList[[j]], rm.terms = c("GenderScore", "Age", "Ethnicity"),
           show.values = TRUE, show.p = FALSE, value.offset = .3)+ 
  scale_x_discrete(labels=c("F5:Childhood SES","F4:Mental Health Symptoms","F3:Hobbies","F2:Cognitive & Physical Stimulation",
                            "F1:SES & Cognitive Reserve",
                            "GMV", "BrainAge - ActualAge"))



pc_reg_gng[[j]]<-annotate_figure(gng_lm, top = text_grob("GLM", 
                                                color = "black", face = "bold", size = 14)) 
}


##### WCST #####

dvList<-c("PC1", "PC2", "PC3", "PC4", "r", "p", "d", "f")

# convert ethnicity to numeric to regress this out
wcst_survey$Ethnicity<-as.numeric(as.factor
                                           (wcst_survey$Ethnicity))


# scale other variables 
wcst_survey[c("GenderScore", "Age",
                        "predMinusActual", "GreyMatter", "Ethnicity",
                        "Education_SES", "CognitiveStimulation", "Hobbies", "MentalHealth", 
                        "ChildSES")]<- 
  lapply(wcst_survey[c("GenderScore", "Age",
                                 "predMinusActual", "GreyMatter", "Ethnicity",
                                 "Education_SES", "CognitiveStimulation", "Hobbies", "MentalHealth", 
                                 "ChildSES")], scale)

wcst_lm_models <- list()
wcst_html_model <- list()
wcst_vif_list<-list()

for (m in 1:length(dvList)) {
  
  model<-lm(as.formula(paste(dvList[[m]],"~ GenderScore + Age + Ethnicity + predMinusActual+ GreyMatter + Education_SES + CognitiveStimulation + Hobbies +MentalHealth + ChildSES")), data=wcst_survey)
  
  wcst_lm_models[[m]]<-model
  
  wcst_html_model[[m]]<-tab_model(model, show.se = TRUE,
                                   pred.labels = c("(Intercept)", 
                                                   "Gender", "Age",
                                                   "Ethnicity","BrainAge - ActualAge", 
                                                   "GMV", "F1:SES & Cognitive Reserve",
                                                   "F2:Cognitive & Physical Stimulation",
                                                   "F3:Hobbies",
                                                   "F4:Mental Health Symptoms",
                                                   "F5:Childhood SES"))
  # variance inflation factor to test for multi-collinearity
  wcst_vif_list[[m]]<-vif(wcst_lm_models[[m]])
  
}

#### plot regression results #####

set_theme(base = theme_bw())

dvList<-c("PC1: Well-Performing", "PC2: Perseverative", "PC3: Poor Performing + Rigid Focusing", 
          "PC4: Slow Initial Learning", "r", "p", "d", "f")

pc_reg_wcst<-list()

for (j in 1:length(dvList)) {
  wcst_lm<-plot_model(wcst_lm_models[[j]], title = dvList[[j]], rm.terms = c("GenderScore", "Age", "Ethnicity"),
                              show.values = TRUE, show.p = FALSE, value.offset = .3)+ 
    scale_x_discrete(labels=c("F5:Childhood SES","F4:Mental Health Symptoms","F3:Hobbies","F2:Cognitive & Physical Stimulation",
                              "F1:SES & Cognitive Reserve",
                              "GMV", "BrainAge - ActualAge"))
  
  pc_reg_wcst[[j]]<-annotate_figure(wcst_lm, top = text_grob("GLM", 
                                                           color = "black", face = "bold", size = 14)) 
}


ggarrange(pc_reg_wcst[[1]], pc_reg_wcst[[2]], pc_reg_wcst[[3]], pc_reg_wcst[[4]])


#### Correlations between FA predictor variables ####
gng_forCorr<- GNG_survey%>%
  dplyr::select("GenderScore", "Age",
                "predMinusActual", "GreyMatter", 
                "Education_SES", "CognitiveStimulation", "Hobbies", "MentalHealth", 
                "ChildSES")

corr <- round(cor(gng_forCorr), 1)
p.mat <- cor_pmat(gng_forCorr)

ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE,
           type = "lower", insig = "blank")


wcst_forCorr<- wcst_survey%>%
  dplyr::select("GenderScore", "Age",
                "predMinusActual", "GreyMatter", 
                "Education_SES", "CognitiveStimulation", "Hobbies", "MentalHealth", 
                "ChildSES")

corr <- round(cor(wcst_forCorr), 1)
p.mat <- cor_pmat(wcst_forCorr)

ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE,
           type = "lower", insig = "blank")

#### Corrected p-values ####
IVWCSTPC_pvals<-list()
IVWCSTparams_pvals<-list()
IVGNGPC_pvals<-list()
IVGNGparams_pvals<-list()

for (m in 1:7) {
  IVWCSTPC_pvals[[m]]<-c(summary(wcst_lm_models[[1]])$coefficients[m+4,4],
                    summary(wcst_lm_models[[2]])$coefficients[m+4,4],
                    summary(wcst_lm_models[[3]])$coefficients[m+4,4],
                    summary(wcst_lm_models[[4]])$coefficients[m+4,4])
  
  IVWCSTparams_pvals[[m]]<-c(summary(wcst_lm_models[[5]])$coefficients[m+4,4],
                    summary(wcst_lm_models[[6]])$coefficients[m+4,4],
                    summary(wcst_lm_models[[7]])$coefficients[m+4,4],
                    summary(wcst_lm_models[[8]])$coefficients[m+4,4])
  
  IVGNGPC_pvals[[m]]<-c(summary(gng_lm_models[[1]])$coefficients[m+4,4],
                         summary(gng_lm_models[[2]])$coefficients[m+4,4],
                         summary(gng_lm_models[[3]])$coefficients[m+4,4])
  
  IVGNGparams_pvals[[m]]<-c(summary(gng_lm_models[[4]])$coefficients[m+4,4],
                             summary(gng_lm_models[[5]])$coefficients[m+4,4],
                             summary(gng_lm_models[[6]])$coefficients[m+4,4],
                             summary(gng_lm_models[[7]])$coefficients[m+4,4],
                             summary(gng_lm_models[[8]])$coefficients[m+4,4],
                            summary(gng_lm_models[[9]])$coefficients[m+4,4])
  
  
}

# adjust p-values

adj_p_wcst_PCs<-list()
adj_p_wcst_params<-list()
adj_p_gng_PCs<-list()
adj_p_gng_params<-list()

for (m in 1:7) {
  
adj_p_wcst_PCs[[m]]<-p.adjust(IVWCSTPC_pvals[[m]], method="holm")

adj_p_wcst_params[[m]]<-p.adjust(IVWCSTparams_pvals[[m]], method="holm")

adj_p_gng_PCs[[m]]<-p.adjust(IVGNGPC_pvals[[m]], method="holm")

adj_p_gng_params[[m]]<-p.adjust( IVGNGparams_pvals[[m]], method="holm")
  
}

# order of IVs in lists: predMinusActual, GMV, F1, F2, F3, F4, F5


# the factors show moderately strong correlations between each other 
#and with other IVs suggesting ridge regression is appropriate

#### Ridge Regression for profiles and survey FA variables ####

##### GnG #####
dvList<-c("PC1", "PC2", "PC3", "v", "v.0.", "v.1.","a","t","z")

GnG_mseridge <- list()
GnG_rsquared<- list()
GNG_ridge_models <- list()

for (m in 1:length(dvList)) {
  
  firstcol = which(colnames(GNG_survey)=="Education_SES")
  lastcol = which(colnames(GNG_survey)=="ChildSES")
  gender =which(colnames(GNG_survey)=="GenderScore")
  Age = which(colnames(GNG_survey)=="Age")
  Ethnicity<- which(colnames(GNG_survey)=="Ethnicity")
  brainAge = which(colnames(GNG_survey)=="predMinusActual")
  gmv = which(colnames(GNG_survey)=="GreyMatter")
  
  pred<-GNG_survey[c(gender, Age, Ethnicity,brainAge,gmv,firstcol:lastcol)]
  
  pred<- sapply(pred, as.numeric)
  
  pred<-scale(pred)
  
  train_rows <- sample(1:nrow(GNG_survey), .7*nrow(GNG_survey), replace = F)
  
  
  dv <- GNG_survey[[dvList[[m]]]]
  #dv <- GNG_survey$PC2
  
  pred.train <- pred[train_rows,]
  dv.train <- dv[train_rows]
  
  pred.test <- pred[-train_rows,]
  dv.test <- dv[-train_rows]
  
  ridge <- cv.glmnet(x=pred.train, y=dv.train, type.measure="mse", 
                     alpha=0, family="gaussian", nlambda=200)
  
  ridge.predicted <- predict(ridge, ridge$lambda.1se, new=pred.test)
  mean_sq<-mean((dv.test - ridge.predicted)^2)
  #calculate r-square
  rsq <- cor(dv.test, ridge.predicted)^2
  
  
  GnG_mseridge[[m]] <- mean_sq
  
  GnG_rsquared[[m]] <- rsq
  GNG_ridge_models[[m]]<-ridge
  
}

# to look at coefs per model
predict(GNG_ridge_models[[1]], type = "coef")

# for plotting

gng_forPlotting<-list()

for (k in 1:length(dvList)) {

  # Get coefs corresponding to best alpha
  model_coefs <- predict(GNG_ridge_models[[k]], type = "coef")
  
  # store coefficients
  sig_coefs<-summary(model_coefs)
  
  # get corresponding names of predictors
  col_num<-sig_coefs$i 
  
  new_char<- "Intercept" #account for intercept
  
  column_names<-colnames(pred)
  
  new_colnames <- c(new_char, column_names[1:(length(column_names) - 1)], 
                    column_names[length(column_names)])
  
  sig_column_names <- new_colnames[col_num]
  
  sig_coefs$colName<-sig_column_names
  
  gng_forPlotting[[k]] <- sig_coefs
  
}

# plots of ridge results
set_theme(base = theme_bw())

gng_ridgePlots<-list()

for (k in 1:length(dvList)) {

gng_plot <- gng_forPlotting[[k]] %>%
  mutate(pos = x >= 0)

# change row variables to be able to index based on colour
gng_plot$pos[gng_plot$pos == FALSE] = 'neg'
gng_plot$pos[gng_plot$pos == TRUE] = 'pos'

#remove covariates and preserve order
gng_plot <- gng_plot[-(1:4),]
gng_plot$colName <- factor(gng_plot$colName, levels = gng_plot$colName)

gng_ridge<-ggplot(gng_plot,aes(colName,x, fill = pos))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, size = 10))+
  scale_fill_manual(values=c(neg="firebrick1",pos="steelblue"))+
  scale_x_discrete(labels=c("BrainAge - ActualAge", "GMV" ,"F1:SES & Cognitive Reserve",
                            "F2:Cognitive & Physical Stimulation",
                            "F3:Hobbies",
                            "F4:Mental Health Symptoms",
                            "F5:Childhood SES"))+
  theme(legend.position="none")+
  ylim(-0.15, 0.1) + 
  ylab("Coefficient") +
  xlab("Predictor")

gng_ridgePlots[[k]]<-annotate_figure(gng_ridge, top = text_grob("Ridge Regression", 
                                                      color = "black", face = "bold", size = 14)) 

}

##### WCST #####

# DVs
dvList<-c("PC1", "PC2", "PC3", "PC4", "r", "p", "d", "f")

wcst_mseridge <- list()
wcst_rsquared<- list()
wcst_ridge_models <- list()

for (m in 1:length(dvList)) {
  
  firstcol = which(colnames(wcst_survey)=="Education_SES")
  lastcol = which(colnames(wcst_survey)=="ChildSES")
  gender =which(colnames(wcst_survey)=="GenderScore")
  Age = which(colnames(wcst_survey)=="Age")
  Ethnicity<- which(colnames(wcst_survey)=="Ethnicity")
  brainAge = which(colnames(wcst_survey)=="predMinusActual")
  gmv = which(colnames(wcst_survey)=="GreyMatter")
  
  pred<-wcst_survey[c(gender, Age, Ethnicity, brainAge, gmv,firstcol:lastcol)]
  
  pred<- sapply(pred, as.numeric)
  
  pred<-scale(pred)
  
  train_rows <- sample(1:nrow(wcst_survey), .7*nrow(wcst_survey), replace = F)
  
  
  dv <- wcst_survey[[dvList[[m]]]]
  #dv <- wcst_survey$PC2
  
  pred.train <- pred[train_rows,]
  dv.train <- dv[train_rows]
  
  pred.test <- pred[-train_rows,]
  dv.test <- dv[-train_rows]
  
  ridge <- cv.glmnet(x=pred.train, y=dv.train, type.measure="mse", 
                     alpha=0, family="gaussian", nlambda=200)
  
  ridge.predicted <- predict(ridge, ridge$lambda.1se, new=pred.test)
  mean_sq<-mean((dv.test - ridge.predicted)^2)
  #calculate r-square
  rsq <- cor(dv.test, ridge.predicted)^2
  
  
  wcst_mseridge[[m]] <- mean_sq
  
  wcst_rsquared[[m]] <- rsq
  wcst_ridge_models[[m]]<-ridge
  
}

# to look at coefs per model
predict(wcst_ridge_models[[1]], type = "coef")

# for plotting

wcst_forPlotting<-list()

for (k in 1:length(dvList)) {
  
  # Get coefs corresponding to best alpha
  model_coefs <- predict(wcst_ridge_models[[k]], type = "coef")
  
  # store coefficients
  sig_coefs<-summary(model_coefs)
  
  # get corresponding names of predictors
  col_num<-sig_coefs$i 
  
  new_char<- "Intercept" #account for intercept
  
  column_names<-colnames(pred)
  
  new_colnames <- c(new_char, column_names[1:(length(column_names) - 1)], 
                    column_names[length(column_names)])
  
  sig_column_names <- new_colnames[col_num]
  
  sig_coefs$colName<-sig_column_names
  
  wcst_forPlotting[[k]] <- sig_coefs
  
}

# plots of ridge results
set_theme(base = theme_bw())

wcst_ridgePlots<-list()

for (k in 1:length(dvList)) {
  
  wcst_plot <- wcst_forPlotting[[k]] %>%
    mutate(pos = x >= 0)
  
  # change row variables to be able to index based on colour
  wcst_plot$pos[wcst_plot$pos == FALSE] = 'neg'
  wcst_plot$pos[wcst_plot$pos == TRUE] = 'pos'
  
  #remove covariates and preserve order
  wcst_plot <- wcst_plot[-(1:4),]
  wcst_plot$colName <- factor(wcst_plot$colName, levels = wcst_plot$colName)
  
  wcst_ridge<-ggplot(wcst_plot,aes(colName,x, fill = pos))+
    geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle = 90, size = 10))+
    scale_fill_manual(values=c(neg="firebrick1",pos="steelblue"))+
    scale_x_discrete(labels=c("BrainAge - ActualAge", "GMV", "F1:SES & Cognitive Reserve",
                              "F2:Cognitive & Physical Stimulation",
                              "F3:Hobbies",
                              "F4:Mental Health Symptoms",
                              "F5:Childhood SES"))+
    theme(legend.position="none")+
    ylim(-0.6, 0.7) + 
    ylab("Coefficient") +
    xlab("Predictor")
  
  wcst_ridgePlots[[k]]<-annotate_figure(wcst_ridge, top = text_grob("Ridge Regression", 
                                                                  color = "black", face = "bold", size = 14)) 
}

##### combining linear and ridge regression plots #####

# GnG

gng_plot_mixed<-list()

dvList<-c("PC1", "PC2", "PC3", "v", "v.0.", "v.1.","a","t","z")

for (k in 1:length(dvList)) {
  gng_plot_first<-ggarrange (pc_reg_gng[[k]],
                           gng_ridgePlots[[k]], ncol = 2, nrow = 1)
  gng_plot_mixed[[k]]<- gng_plot_first + theme(panel.border = element_rect(color = "black", 
                                                    fill = NA))
}


gng_regs_final<-ggarrange (gng_plot_mixed[[1]],
                      gng_plot_mixed[[2]],
                      gng_plot_mixed[[3]], ncol = 1, nrow = 3)

gng_regs_final<-annotate_figure(gng_regs_final, top = text_grob("A.Go/No-Go Results", 
                                      color = "black", size = 14))


tiff(file="C:/Users/aleya/OneDrive/Documents/Tasks/plots/gng_regs.tiff",width = 11, height = 12, units = "in", res = 300)
gng_regs_final
dev.off()


#WCST

wcst_plot_mixed<-list()

dvList<-c("PC1", "PC2", "PC3", "PC4", "r", "p", "d", "f")

for (k in 1:length(dvList)) {
  wcst_plot_first<-ggarrange (pc_reg_wcst[[k]],
                             wcst_ridgePlots[[k]], ncol = 2, nrow = 1)
  wcst_plot_mixed[[k]]<- wcst_plot_first + theme(panel.border = element_rect(color = "black", 
                                                                           fill = NA))
}


wcst_regs_final<-ggarrange (wcst_plot_mixed[[1]],
                           wcst_plot_mixed[[2]],
                           wcst_plot_mixed[[3]],
                           wcst_plot_mixed[[4]],
                           ncol = 1, nrow = 4)

wcst_regs_final<-annotate_figure(wcst_regs_final, top = text_grob("B.WCST Results", 
                                                                color = "black", size = 14))



tiff(file="C:/Users/aleya/OneDrive/Documents/Tasks/plots/wcst_regs.tiff",width = 13, height = 15, units = "in", res = 300)
wcst_regs_final
dev.off()


#### Correlations between PCs, params, and FAs, Brain ####

forCorr_wcst = subset(wcst_survey, 
                      select = c(Age, 
                                 predMinusActual, GreyMatter, WhiteMatter,
                                 Education_SES, CognitiveStimulation, Hobbies,
                                 MentalHealth, 
                                 ChildSES, PC1:PC4, r, p, d, f))

correlations <- rcorr(as.matrix(forCorr_wcst), type=c("spearman"))
sig_corr <- correlations$P<.05
corr_all<-correlations$r
corr_all<-corr_all[c("Age", 
                     "predMinusActual", "GreyMatter", "WhiteMatter",
                     "Education_SES", "CognitiveStimulation", "Hobbies",
                     "MentalHealth", "ChildSES"), 
                   c("PC1","PC2","PC3","PC4","r", "p", "d", "f", "predMinusActual")]
corr_all_p<-correlations$P
corr_all_p<-corr_all_p[c("Age", 
                           "predMinusActual", "GreyMatter","WhiteMatter", 
                           "Education_SES", "CognitiveStimulation", "Hobbies",
                           "MentalHealth",  "ChildSES"), 
                         c("PC1","PC2","PC3","PC4","r", 
                           "p", "d", "f", "predMinusActual")]

write.csv(corr_all, 'C:/Users/aleya/OneDrive/Documents/Tasks/plots/wcst_corr_reg.csv')

forCorr_gng = subset(GNG_survey, 
                      select = c(Age, 
                                 predMinusActual, GreyMatter, WhiteMatter,
                                 Education_SES, CognitiveStimulation, Hobbies,
                                 MentalHealth, 
                                 ChildSES, PC1:PC3, a:v.1.))

correlations <- rcorr(as.matrix(forCorr_gng), type=c("spearman"))
sig_corr <- correlations$P<.05
corr_all<-correlations$r
corr_all<-corr_all[c("Age", 
                     "predMinusActual", "GreyMatter", "WhiteMatter",
                     "Education_SES", "CognitiveStimulation", "Hobbies",
                     "MentalHealth",  "ChildSES"), 
                   c("PC1","PC2","PC3","v", "v.0.", "v.1.","a","t","z", "predMinusActual")]
corr_all_p<-correlations$P
corr_all_p<-corr_all_p[c("Age", 
                         "predMinusActual", "GreyMatter","WhiteMatter", 
                         "Education_SES", "CognitiveStimulation", "Hobbies",
                         "MentalHealth", "ChildSES"), 
                       c("PC1","PC2","PC3","v", "v.0.", "v.1.","a","t","z", "predMinusActual")]

write.csv(corr_all, 'C:/Users/aleya/OneDrive/Documents/Tasks/plots/gng_corr_reg.csv')


#### Checking White matter ####

###### GnG #####
dvList<-c("PC1", "PC2", "PC3", "v", "v.0.", "v.1.","a","t","z")

# convert ethnicity to numeric to regress this out
GNG_survey$Ethnicity<-as.numeric(as.factor
                                           (GNG_survey$Ethnicity))

# scale other variables 
GNG_survey[c("GenderScore", "Age", "Ethnicity",
                       "predMinusActual", "GreyMatter", "WhiteMatter",
                       "Education_SES", "CognitiveStimulation", "Hobbies", "MentalHealth", 
                       "ChildSES")]<- 
  lapply(GNG_survey[c("GenderScore", "Age", "Ethnicity", 
                                "predMinusActual", "GreyMatter", "WhiteMatter",
                                "Education_SES", "CognitiveStimulation", "Hobbies", "MentalHealth", 
                                "ChildSES")], scale)


gng_lm_models <- list()
gng_html_model <- list()
gng_vif_list<-list()

for (m in 1:length(dvList)) {
  
  model<-lm(as.formula(paste(dvList[[m]],"~ GenderScore + Age + Ethnicity + predMinusActual+ GreyMatter + WhiteMatter + Education_SES + CognitiveStimulation + Hobbies + MentalHealth + ChildSES")), data=GNG_survey)
  
  gng_lm_models[[m]]<-model
  
  gng_html_model[[m]]<-tab_model(model, show.se = TRUE,
                                 pred.labels = c("(Intercept)", 
                                                 "Gender", "Age",
                                                 "Ethnicity","BrainAge - ActualAge", 
                                                 "GMV", 
                                                 "WMV",
                                                 "F1:SES & Cognitive Reserve",
                                                 "F2:Cognitive & Physical Stimulation",
                                                 "F3:Hobbies",
                                                 "F4:Mental Health Symptoms",
                                                 "F5:Childhood SES"))
  
  gng_vif_list[[m]]<-vif(gng_lm_models[[m]])
  
}

# do p-value adjustment for GNG-PC2
p.adjust(c(0.739,0.328, 0.014), method="holm")

###### WCST #####

dvList<-c("PC1", "PC2", "PC3", "PC4", "r", "p", "d", "f")

# convert ethnicity to numeric to regress this out
wcst_survey$Ethnicity<-as.numeric(as.factor
                                            (wcst_survey$Ethnicity))


# scale other variables 
wcst_survey[c("GenderScore", "Age",
                        "predMinusActual", "GreyMatter", "WhiteMatter", "Ethnicity",
                        "Education_SES", "CognitiveStimulation", "Hobbies", "MentalHealth", 
                        "ChildSES")]<- 
  lapply(wcst_survey[c("GenderScore", "Age",
                                 "predMinusActual", "GreyMatter", "WhiteMatter", "Ethnicity",
                                 "Education_SES", "CognitiveStimulation", "Hobbies", "MentalHealth", 
                                 "ChildSES")], scale)

wcst_lm_models <- list()
wcst_html_model <- list()
wcst_vif_list<-list()

for (m in 1:length(dvList)) {
  
  model<-lm(as.formula(paste(dvList[[m]],"~ GenderScore + Age + Ethnicity + predMinusActual+ GreyMatter + WhiteMatter + Education_SES + CognitiveStimulation + Hobbies +MentalHealth + ChildSES")), data=wcst_survey)
  
  wcst_lm_models[[m]]<-model
  
  wcst_html_model[[m]]<-tab_model(model, show.se = TRUE,
                                  pred.labels = c("(Intercept)", 
                                                  "Gender", "Age",
                                                  "Ethnicity","BrainAge - ActualAge", 
                                                  "GMV", "WMV","F1:SES & Cognitive Reserve",
                                                  "F2:Cognitive & Physical Stimulation",
                                                  "F3:Hobbies",
                                                  "F4:Mental Health Symptoms",
                                                  "F5:Childhood SES"))
  # variance inflation factor to test for multi-collinearity
  wcst_vif_list[[m]]<-vif(wcst_lm_models[[m]])
  
}


#### Mediation ####
# read in .csv from directory
roi_vol<-read.table("C:/Users/aleya/OneDrive/Desktop/Cog Modelling Ageing Project/CleanDataFiles/roi_vol.csv",header=TRUE,sep=",")

# match wcst data to roi_vol
colnames(roi_vol)[1]<-"subject_id"

roi_matching_wcst <-semi_join(roi_vol,wcst_survey, by=("subject_id"))

WCST_survey_roi<- merge (roi_matching_wcst, wcst_survey, by = 'subject_id')


# match gng data to roi_vol
roi_matching_gng <-semi_join(roi_vol,GNG_survey, by=("subject_id"))

GNG_survey_roi<- merge (roi_matching_gng, GNG_survey, by = 'subject_id')

#run mediations

library(mediation)

##### WCST PC3 #####

med_List<-c("GreyMatter", "WhiteMatter", "predMinusActual", "Amygdala", "Cerebellum", "DLPFC", "PFC")

wcst_mediations_PC3 <- list()

for (m in 1:length(med_List)) {
  
  fitM <- lm(as.formula(paste(med_List[[m]],"~ Education_SES")), data=WCST_survey_roi) 
  # Construct the formula for the fitY model
  formula_Y <- as.formula(paste("PC3 ~", paste(c(med_List[[m]], "Education_SES"), collapse = " + ")))
  
  fitY <- lm(formula_Y, data=WCST_survey_roi)
  
  fitMed <- mediation::mediate(fitM, fitY, treat="Education_SES", mediator=med_List[[m]])
  
  wcst_mediations_PC3[[m]]<-fitMed
  
}

summary(wcst_mediations_PC3[[5]])



##### WCST PC1 #####

med_List<-c("GreyMatter", "WhiteMatter", "predMinusActual", "Amygdala", "Cerebellum", "DLPFC", "PFC")

wcst_mediations_PC1 <- list()

for (m in 1:length(med_List)) {
  
  fitM <- lm(as.formula(paste(med_List[[m]],"~ Education_SES")), data=WCST_survey_roi) 
  # Construct the formula for the fitY model
  formula_Y <- as.formula(paste("PC1 ~", paste(c(med_List[[m]], "Education_SES"), collapse = " + ")))
  
  fitY <- lm(formula_Y, data=WCST_survey_roi)
  
  fitMed <- mediation::mediate(fitM, fitY, treat="Education_SES", mediator=med_List[[m]])
  
  wcst_mediations_PC1[[m]]<-fitMed
  
}

summary(wcst_mediations_PC1[[1]])


##### GnG PC3 #####

med_List<-c("GreyMatter", "WhiteMatter", "predMinusActual", "Amygdala", "Cerebellum", "DLPFC", "PFC", "IFG", "Insula", "SuppMotor")

gng_mediations_PC2 <- list()

for (m in 1:length(med_List)) {
  
  fitM <- lm(as.formula(paste(med_List[[m]],"~ Education_SES")), data=GNG_survey_roi) 
  # Construct the formula for the fitY model
  formula_Y <- as.formula(paste("PC2 ~", paste(c(med_List[[m]], "Education_SES"), collapse = " + ")))
  
  fitY <- lm(formula_Y, data=GNG_survey_roi)
  
  fitMed <- mediation::mediate(fitM, fitY, treat="Education_SES", mediator=med_List[[m]])
  
  gng_mediations_PC2[[m]]<-fitMed
  
}

summary(gng_mediations_PC2[[3]])


##### GnG z #####

med_List<-c("GreyMatter", "WhiteMatter", "predMinusActual", "Amygdala", "Cerebellum", "DLPFC", "PFC", "IFG", "Insula", "SuppMotor")

gng_mediations_z <- list()

for (m in 1:length(med_List)) {
  
  fitM <- lm(as.formula(paste(med_List[[m]],"~ Education_SES")), data=GNG_survey_roi) 
  # Construct the formula for the fitY model
  formula_Y <- as.formula(paste("z ~", paste(c(med_List[[m]], "Education_SES"), collapse = " + ")))
  
  fitY <- lm(formula_Y, data=GNG_survey_roi)
  
  fitMed <- mediation::mediate(fitM, fitY, treat="Education_SES", mediator=med_List[[m]])
  
  gng_mediations_z[[m]]<-fitMed
  
}

summary(gng_mediations_z[[5]])



##### Create forest plots for mediation #####

##### Function for extracting ACME and confidence intervals from each mediation result #####

mediationData_forPlotting<-function(file) {mediation_results <- lapply(file, function(x) {
  data.frame(
    Mediator = x$mediator,
    ACME = x$d0,
    LowerCI = x$d0.ci[1],
    UpperCI = x$d0.ci[2]
  )
})

# Combine all results into a single data frame
mediation_df <- do.call(rbind, mediation_results)

# Determine significance based on whether the confidence interval includes zero
mediation_df <- mediation_df %>%
 # mutate(Significant = ifelse(LowerCI != 0 | UpperCI != 0, "Significant", "Not Significant"))
  mutate(Significant = ifelse(LowerCI > 0 | UpperCI < 0, "Significant", "Not Significant"))

return (mediation_df)
} # end of function

##### WCST PC1 #####

mediation_df<-mediationData_forPlotting(wcst_mediations_PC1)
# Create the forest plot
med1<-ggplot(mediation_df, aes(x = ACME, y = Mediator, color = Significant)) +
  geom_point() +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_color_manual(values = c("Significant" = "darkgreen", "Not Significant" = "black")) +
  labs(title = "A. Mediators on SES & Cognitive Reserve -> WCST-PC1 Relationship",
       x = "",
       y = "",
       color = "Significance") +
  theme_bw()+
  guides(color = "none")+
  theme(plot.title = element_text(size = 10)) 



##### WCST PC3 #####

mediation_df<-mediationData_forPlotting(wcst_mediations_PC3)
# Create the forest plot
med2<-ggplot(mediation_df, aes(x = ACME, y = Mediator, color = Significant)) +
  geom_point() +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_color_manual(values = c("Significant" = "red", "Not Significant" = "black")) +
  labs(title = "B. Mediators on SES & Cognitive Reserve -> WCST-PC3 Relationship",
       x = "",
       y = "",
       color = "Significance") +
  theme_bw()+
  guides(color = "none")+
  theme(plot.title = element_text(size = 10)) 


##### GnG PC2 #####

mediation_df<-mediationData_forPlotting(gng_mediations_PC2)
# Create the forest plot
med3<-ggplot(mediation_df, aes(x = ACME, y = Mediator, color = Significant)) +
  geom_point() +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_color_manual(values = c("Significant" = "red", "Not Significant" = "black")) +
  labs(title = "C. Mediators on SES & Cognitive Reserve -> GNG-PC2 Relationship",
       x = "ACME (Average Causal Mediation Effect)",
       y = "",
       color = "Significance") +
  theme_bw()+
  guides(color = "none")+
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10)) 


##### GnG z #####

mediation_df<-mediationData_forPlotting(gng_mediations_z)
# Create the forest plot
med4<-ggplot(mediation_df, aes(x = ACME, y = Mediator, color = Significant)) +
  geom_point() +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_color_manual(values = c("Significant" = "darkgreen", "Not Significant" = "black")) +
  labs(title = "D. Mediators on SES & Cognitive Reserve -> Start. Point Parameter (z)",
       x = "ACME (Average Causal Mediation Effect)",
       y = "",
       color = "Significance") +
  theme_bw()+
  guides(color = "none")+
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10)) 


medPlot<-ggarrange(med1, med2, med3, med4, nrow = 2, ncol = 2)

tiff(file="C:/Users/aleya/OneDrive/Desktop/Cog Modelling Ageing Project/npj aging/Review/med_plot.tiff",width = 15, height = 10, units = "in", res = 300)
medPlot
dev.off()


#### Checking regressions in sample with all data ####

sub_ids_intersect<-intersect(GNG_survey$subject_id, wcst_survey$subject_id)
gng_match_wcst<-GNG_survey[GNG_survey$subject_id %in% sub_ids_intersect,]
wcst_match_gng<-wcst_survey[wcst_survey$subject_id %in% sub_ids_intersect,]

#gng
dvList<-c("PC1", "PC2", "PC3", "v", "v.0.", "v.1.","a","t","z")


gng_lm_models <- list()
gng_html_model <- list()
gng_vif_list<-list()


for (m in 1:length(dvList)) {
  
  model<-lm(as.formula(paste(dvList[[m]],"~ GenderScore + Age + Ethnicity + predMinusActual+ GreyMatter + Education_SES + CognitiveStimulation + Hobbies + MentalHealth + ChildSES")), data=gng_match_wcst)
  
  gng_lm_models[[m]]<-model
  
  gng_html_model[[m]]<-tab_model(model, show.se = TRUE,
                                 pred.labels = c("(Intercept)", 
                                                 "Gender", "Age",
                                                 "Ethnicity","BrainAge - ActualAge", 
                                                 "GMV", 
                                                 "F1:SES & Cognitive Reserve",
                                                 "F2:Cognitive & Physical Stimulation",
                                                 "F3:Hobbies",
                                                 "F4:Mental Health Symptoms",
                                                 "F5:Childhood SES"))
  
  gng_vif_list[[m]]<-vif(gng_lm_models[[m]])
  
}

set_theme(base = theme_bw())

##### plot regression results #####
dvList<-c("PC1: Well-Performing", "PC2: Slow Responding", 
          "PC3: Random Responding","v", "v.0.", "v.1.","a","t","z")

pc_reg_gng<-list()

for (j in 1:length(dvList)) {
  gng_lm<-plot_model(gng_lm_models[[j]], title = dvList[[j]], rm.terms = c("GenderScore", "Age", "Ethnicity"),
                     show.values = TRUE, show.p = FALSE, value.offset = .3)+ 
    scale_x_discrete(labels=c("F5:Childhood SES","F4:Mental Health Symptoms","F3:Hobbies","F2:Cognitive & Physical Stimulation",
                              "F1:SES & Cognitive Reserve",
                              "GMV", "BrainAge - ActualAge"))
  
  
  
  pc_reg_gng[[j]]<-annotate_figure(gng_lm, top = text_grob("GLM", 
                                                           color = "black", face = "bold", size = 14)) 
}


dvList<-c("PC1", "PC2", "PC3", "PC4", "r", "p", "d", "f")


wcst_lm_models <- list()
wcst_html_model <- list()
wcst_vif_list<-list()

for (m in 1:length(dvList)) {
  
  model<-lm(as.formula(paste(dvList[[m]],"~ GenderScore + Age + Ethnicity + predMinusActual+ GreyMatter + Education_SES + CognitiveStimulation + Hobbies +MentalHealth + ChildSES")), data=wcst_match_gng)
  
  wcst_lm_models[[m]]<-model
  
  wcst_html_model[[m]]<-tab_model(model, show.se = TRUE,
                                  pred.labels = c("(Intercept)", 
                                                  "Gender", "Age",
                                                  "Ethnicity","BrainAge - ActualAge", 
                                                  "GMV", "F1:SES & Cognitive Reserve",
                                                  "F2:Cognitive & Physical Stimulation",
                                                  "F3:Hobbies",
                                                  "F4:Mental Health Symptoms",
                                                  "F5:Childhood SES"))
  # variance inflation factor to test for multi-collinearity
  wcst_vif_list[[m]]<-vif(wcst_lm_models[[m]])
  
}

#### plot regression results #####

set_theme(base = theme_bw())

dvList<-c("PC1: Well-Performing", "PC2: Perseverative", "PC3: Poor Performing + Rigid Focusing", 
          "PC4: Slow Initial Learning", "r", "p", "d", "f")

pc_reg_wcst<-list()

for (j in 1:length(dvList)) {
  wcst_lm<-plot_model(wcst_lm_models[[j]], title = dvList[[j]], rm.terms = c("GenderScore", "Age", "Ethnicity"),
                      show.values = TRUE, show.p = FALSE, value.offset = .3)+ 
    scale_x_discrete(labels=c("F5:Childhood SES","F4:Mental Health Symptoms","F3:Hobbies","F2:Cognitive & Physical Stimulation",
                              "F1:SES & Cognitive Reserve",
                              "GMV", "BrainAge - ActualAge"))
  
  pc_reg_wcst[[j]]<-annotate_figure(wcst_lm, top = text_grob("GLM", 
                                                             color = "black", face = "bold", size = 14)) 
}


ggarrange(pc_reg_wcst[[1]], pc_reg_wcst[[2]], pc_reg_wcst[[3]], pc_reg_wcst[[4]])

