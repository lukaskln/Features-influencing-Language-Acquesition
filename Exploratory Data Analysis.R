####### Setup #######

library(tidyverse)
library(gridExtra)
library(GGally)
library(readr)
library(rcompanion)

vColors = c("#049DD9","#03A64A","#F2AC29","#F2CA80","#F22929")

theme_set(theme_minimal())

dfData <- read.table("data_exam_june2020.txt", header = TRUE)

dfData <- dfData %>% mutate(id = factor(id), success = factor(success))

####### Descriptive Statistics #######

str(dfData)

summary(dfData)

group_by(dfData, age_group, structure, success) %>% 
  summarise(Observations = n())

sum(is.na(dfData)) # no nas


# Sucess per Student

# xyplot(success ~ 1 | id,
    #   data = dfData, type = 'b', scale = list(c(tick.number = 2)), 
    #   as.table = TRUE)

# Success per Factor

p1 = 
  ggplot(data = dfData,aes(x = id, fill = success)) + 
  geom_bar() +
  scale_fill_manual(values = vColors[c(1,2)]) +
  scale_color_manual(values = vColors[c(1,2)]) + 
  ylab("Observations") + xlab("id of Child") +
  theme(panel.grid.major.x = element_blank()) + 
  theme(legend.position = "none")
  
p2 = 
  ggplot(data = dfData,aes(x = age_group, fill = success)) + 
  geom_bar() +
  scale_fill_manual(values = vColors[c(1,2)]) +
  scale_color_manual(values = vColors[c(1,2)]) + 
  ylab("Observations") + xlab("Age Group") +
  theme(panel.grid.major.x = element_blank()) + 
  theme(legend.position = "none") + ylim(0,1200)

p3 = 
  ggplot(data = dfData,aes(x = structure, fill = success)) + 
  geom_bar() +
  scale_fill_manual(values = vColors[c(1,2)]) +
  scale_color_manual(values = vColors[c(1,2)]) + 
  ylab("Observations") + xlab("Structure") +
  theme(panel.grid.major.x = element_blank()) + 
  theme(legend.position = "none")+ ylim(0,1200)

p4 =
  ggplot(data = dfData,aes(x = match_mismatch, fill = success)) + 
  geom_bar() +
  scale_fill_manual(values = vColors[c(1,2)]) +
  scale_color_manual(values = vColors[c(1,2)]) + 
  ylab("Observations") + xlab("Match or Mismatch") +
  theme(panel.grid.major.x = element_blank())+ ylim(0,1200)
  
grid.arrange(p2,p3,p4,p1, layout_matrix = rbind(c(1,2,3),c(4,4,4)), top = "Success per Factor")

# Interactions

p1 = 
  ggplot(data = dfData,aes(x = interaction(age_group,structure), fill = success)) + 
  geom_bar() +
  scale_fill_manual(values = vColors[c(1,2)]) +
  scale_color_manual(values = vColors[c(1,2)]) + 
  ylab("Observations") + xlab("Interaction Age Group with Structure") +
  theme(panel.grid.major.x = element_blank()) + 
  theme(legend.position = "none") + ylim(0,600) 

p2 = 
  ggplot(data = dfData,aes(x = interaction(age_group,match_mismatch), fill = success)) + 
  geom_bar() +
  scale_fill_manual(values = vColors[c(1,2)]) +
  scale_color_manual(values = vColors[c(1,2)]) + 
  ylab("Observations") + xlab("Interaction Age Group with Mismatch") +
  theme(panel.grid.major.x = element_blank()) + 
  theme(legend.position = "none") + 
  ylim(0,600) 

p3 = 
  ggplot(data = dfData,aes(x = interaction(structure,match_mismatch), fill = success)) + 
  geom_bar() +
  scale_fill_manual(values = vColors[c(1,2)]) +
  scale_color_manual(values = vColors[c(1,2)]) + 
  ylab("Observations") + xlab("Interaction Structure with Mismatch") +
  theme(panel.grid.major.x = element_blank()) + ylim(0,600)

grid.arrange(p1,p2,p3, nrow = 1, top = "Success per Interaction")

####### Correlation Statistics #######

# Indepentend between each other

vCraV = c()

for(i in 1:4){
  vCraV[i] = cramerV(table(dfData[,c(i,5)]))
}

vCraV = data.frame(cbind(vCraV,Name = colnames(dfData)[1:4]), stringsAsFactors = F)

ggplot(data = vCraV, aes(y = as.numeric(vCraV), x = Name)) + 
  geom_bar(stat = "identity", fill = vColors[3]) +
  geom_text(aes(x=Name,y=as.numeric(vCraV) + 0.05 ,label= round(as.numeric(vCraV),2)),vjust=0, color = "grey30") +
  ylab("Cramer's V") + 
  xlab("Variable") +
  theme(panel.grid.major.x = element_blank()) + 
  ylim(0,1) 

# Test all for quasi seperation in binary regression so beta is not going to infinity

table(as.integer(dfData$structure)-1 > 0, as.integer(dfData$success)-1) # Nearest one



