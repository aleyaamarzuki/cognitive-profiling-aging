rm(list=ls())

set.seed(123)

#Load these libraries 
library(ggplot2)
library(ez)
library(nlme)
library(gridExtra)
library(plyr)
library(lme4)
library(lmerTest)
library(psych)
library(Hmisc)
library(car)
library(tidyverse)
library(dplyr)
library(reshape2)
library(ggpubr)
library(rstatix)
library(RVAideMemoire)
library(welchADF)
library(ggdist)
library(gghalves)
library(cowplot)
library(MASS)
library(sfsmisc)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(brms)
library(PupillometryR)
library(ggpp)
library(wesanderson)
library(BayesFactor)
library(readxl)
library(corrplot)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(psych)
library(ggcorrplot)
library(corrplot)

# directories

#plot dir
plot_dir<-'C:/Users/aleya/OneDrive/Documents/Tasks/plots/'

#### read in files ####
getwd()
# read in wcst 
WCSTonly<-read.table('./forWCSTMRIAnalysis.csv', header=TRUE,sep=",")

# read in gng 
GNGonly<-read.table('C:/Users/aleya/OneDrive/Desktop/Cog Modelling Ageing Project/CleanDataFiles/forGnGMRIAnalysis.csv', header=TRUE,sep=",")

#### get stats for cognitive variables ####
wcst_cog<-WCSTonly %>%
  summarise_at(vars(correct:f),funs(mean, sd, range), na.rm = TRUE)

GNGonly2<-GNGonly %>%  filter(subject_id!=628)

gng_cog<-GNGonly2 %>%
  summarise_at(vars(go_errors:v.1.),funs(mean, sd, range), na.rm = TRUE)


#### PCA ####

##### GnG only #####

#pca for dimensionality reduction

# remove outlier
GNGonly2<-GNGonly %>%  filter(subject_id!=628)

gng_K<-subset(GNGonly2, select = c(go_errors:v.1.))

gng_K<-subset(gng_K, select = -c(dc,v))

gng_K <- na.omit(gng_K)

# Do the PCA 

my.prc <- prcomp(gng_K, center=TRUE, scale=TRUE)
screeplot(my.prc, main="Scree Plot", xlab="Components") #to check eigenvalues, only those >1 will be accepted
screeplot(my.prc, main="Scree Plot", type="line" )

# DotPlot PC1

load    <- my.prc$rotation
sorted.loadings <- load[order(load[, 1]), 1]
myTitle <- "Loadings Plot for PC1" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

# DotPlot PC2

sorted.loadings <- load[order(load[, 2]), 2]
myTitle <- "Loadings Plot for PC2"
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

# Now draw the BiPlot
biplot(my.prc, cex=c(1, 0.7))


# get individual values per subject for each PC and apply promax rotation
gng_K <- scale(gng_K)
k = 3 # change depending on how many factors should be retained based on screeplot
library(psych)
fit<-principal(gng_K, rotate="promax", nfactors=k, scores=TRUE)
gngpca_scores<-fit$scores
loadings<-fit$loadings[, 1:3]

# for plotting purposes
head(gng_K)
corr_matrix <- cor(gng_K)
ggcorrplot(corr_matrix)
data.pca <- princomp(corr_matrix)
summary(data.pca)
# Graph of the variables
fviz_pca_var(data.pca, col.var = "black")

gngpca_scores<-as.data.frame(gngpca_scores)

gng_mod<-gngpca_scores %>%
  summarise_at(vars(RC2:RC3),funs(mean, sd, range), na.rm = TRUE)

# plot PCs

gng_plot<-as.data.frame(loadings)
gng_plot$colName<- rownames(gng_plot)


dvList<-c("RC1", "RC2", "RC3")
titles<-c("PC1: Well-performing", 
          "PC2: Slow Responding", 
          "PC3: Random Responding")

set_theme(base = theme_bw())

gng_plot$colName <- factor(gng_plot$colName, levels = gng_plot$colName)

gng_pca_plot<-list()

for (k in 1:length(dvList)) {

# change row variables to be able to index based on colour
gng_plot$pos[gng_plot[,dvList[[k]]] >= 0] = 'pos'
gng_plot$pos[gng_plot[,dvList[[k]]] < 0] = 'neg'


gng_pca_plot[[k]]<-ggplot(gng_plot,
                          aes_string(x = "colName",
                                     y = dvList[[k]], 
                                     fill = "pos"))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, size = 10))+
  scale_fill_manual(values=c(neg="salmon",pos="seagreen3"))+
  scale_x_discrete(labels=c("Go Errors", "No-Go Errors", "Go RT",
                            "No-Go RT", "a", "Ter", "z", "v.No-Go", 
                            "v.Go"))+
  theme(legend.position="none")+
  ylab("Loading Strength")+
  xlab("Measure")+
  ggtitle(titles[[k]])
}

gng_pca_merged<-ggarrange(gng_pca_plot[[1]], gng_pca_plot[[2]], gng_pca_plot[[3]])

gng_pca_merged<-annotate_figure(gng_pca_merged, top = text_grob("A. Go/No-Go", 
                                         color = "black", size = 14))+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA))

tiff(file="C:/Users/aleya/OneDrive/Documents/Tasks/plots/gngPCA_plot.tiff",width = 10, height = 8, units = "in", res = 300)
gng_pca_merged
dev.off()

#### WCST  only ####

wcst_K<-subset(WCSTonly, select = c(correct:f))
wcst_K <- na.omit(wcst_K)

# Do the PCA 

my.prc <- prcomp(wcst_K, center=TRUE, scale=TRUE)
screeplot(my.prc, main="Scree Plot", xlab="Components") #to check eigenvalues, only those >1 will be accepted
screeplot(my.prc, main="Scree Plot", type="line" )

# DotPlot PC1

load    <- my.prc$rotation
sorted.loadings <- load[order(load[, 1]), 1]
myTitle <- "Loadings Plot for PC1" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

# DotPlot PC2

sorted.loadings <- load[order(load[, 2]), 2]
myTitle <- "Loadings Plot for PC2"
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

# Now draw the BiPlot
biplot(my.prc, cex=c(1, 0.7))


# get individual values per subject for each PC and apply promax rotation
wcst_K <- scale(wcst_K)
k = 4 # change depending on how many factors should be retained based on screeplot
library(psych)
fit<-principal(wcst_K, rotate="promax", nfactors=k, scores=TRUE)
wcstpca_scores<-fit$scores
loadings<-fit$loadings[, 1:4]

wcstpca_scores<-as.data.frame(wcstpca_scores)

wcst_mod<-wcstpca_scores %>%
  summarise_at(vars(RC1:RC4),funs(mean, sd, range), na.rm = TRUE)

# for plotting purposes
head(wcst_K)
corr_matrix <- cor(wcst_K)
library(ggcorrplot)
ggcorrplot(corr_matrix)
data.pca <- princomp(corr_matrix)
summary(data.pca)
# Graph of the variables
fviz_pca_var(data.pca, col.var = "black")


# plot PCs

wcst_plot<-as.data.frame(loadings)
wcst_plot$colName<- rownames(wcst_plot)


dvList<-c("RC1", "RC2", "RC3", "RC4")
titles<-c("PC1: Well-performing", 
          "PC2: Perseverative", 
          "PC3: Poor Performing + Rigid Focusing",
          "PC4: Slow Initial Learning")

set_theme(base = theme_bw())

wcst_plot$colName <- factor(wcst_plot$colName, levels = wcst_plot$colName)

wcst_pca_plot<-list()

for (k in 1:length(dvList)) {
  
  # change row variables to be able to index based on colour
  wcst_plot$pos[wcst_plot[,dvList[[k]]] >= 0] = 'pos'
  wcst_plot$pos[wcst_plot[,dvList[[k]]] < 0] = 'neg'
  
  
  wcst_pca_plot[[k]]<-ggplot(wcst_plot,
                            aes_string(x = "colName",
                                       y = dvList[[k]], 
                                       fill = "pos"))+
    geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle = 90, size = 10))+
    scale_fill_manual(values=c(neg="salmon",pos="seagreen3"))+
    scale_x_discrete(labels=c("Correct", "Persev. Errors",
                              "Non-Persev. Errors", "Unique Errors",
                              "Trials to 1st Set.",
                              "Sets Completed", "r", "p", "d", "f"))+
    theme(legend.position="none")+
    ylab("Loading Strength")+
    ylim(-1, 1) + 
    xlab("Measure")+
    ggtitle(titles[[k]])
}

wcst_pca_merged<-ggarrange(wcst_pca_plot[[1]], wcst_pca_plot[[2]], wcst_pca_plot[[3]], wcst_pca_plot[[4]])

wcst_pca_merged<-annotate_figure(wcst_pca_merged, top = text_grob("B. WCST", 
                                                                color = "black", size = 14))+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA))

tiff(file="C:/Users/aleya/OneDrive/Documents/Tasks/plots/wcstPCA_plot.tiff",width = 10, height = 8, units = "in", res = 300)
wcst_pca_merged
dev.off()



# Combine gng and wcst PCA plots

pca_final<-ggarrange (gng_pca_merged, wcst_pca_merged, 
                           ncol = 1, nrow = 2)


tiff(file="C:/Users/aleya/OneDrive/Documents/Tasks/plots/PCA_plot.tiff",width = 10, height = 15, units = "in", res = 300)
pca_final
dev.off()
