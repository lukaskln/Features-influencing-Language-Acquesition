####### Setup #######

library(gee)
library(geepack)
library(statmod)

####### Model #######
model.gee = 
  geeglm(as.integer(success)-1 ~ structure*match_mismatch + age_group, 
        data = dfData, 
        id = id,
        corstr = "exchangeable",
        family = binomial(link = "logit"))

summary(model.gee)

model.gee2 = 
  gee(as.integer(success)-1 ~ age_group + structure + match_mismatch, 
         data = dfData, 
         id = id,
         corstr = "exchangeable",
         family = binomial(link = "logit"))

summary(model.gee2)

####### Validation #######

# Variable selection by AIC/QIC

QIC(model.gee) %>% round(.,2)

# Hypothesis Testing Wald test

anova(model.gee)

# Confidence interval by normal approximation
confint.geeglm <- function(object, parm, level = 0.95, ...) {
  cc <- coef(summary(object))
  mult <- qnorm((1+level)/2)
  citab <- with(as.data.frame(cc),
                cbind(lwr=Estimate-mult*Std.err,
                      upr=Estimate+mult*Std.err))
  rownames(citab) <- rownames(cc)
  round(citab[parm,],4)
}

confint.geeglm(model.gee)

dfConf = data.frame(Coefficients = names(model.gee$coefficients),Coefficient_Values = exp(model.gee$coefficients),
                    Conf_Lower = exp(confint.geeglm(model.gee)[,1]), Conf_Higher = exp(confint.geeglm(model.gee)[,2]), stringsAsFactors = FALSE)

ggplot(data = dfConf, aes(x = Coefficients, y = Coefficient_Values, colour = Coefficients)) + 
  geom_errorbar(aes(ymin = Conf_Lower, ymax = Conf_Higher), width=.1, cex = 1, color = vColors[1]) +
  geom_line(data = dfConf, aes(x = Coefficients, y = Coefficient_Values, group = 1), color = vColors[2], cex = 0.5) +
  geom_point(x = dfConf$Coefficients, y = dfConf$Coefficient_Values, cex = 2.5, color = vColors[1]) +
  theme(panel.grid.major.x = element_blank(), axis.text.x=element_text(angle=25, hjust=1), legend.position = "none") +
  ylab("Coefficient-Values with CIs") + xlab("Coefficients")

# Residual plot

dfRes = data.frame(Index = 1:length(qresiduals(model.gee)), 
                   Pearson_Residuals = resid(model.gee, type = "pearson"), 
                   Estimation = as.character(as.numeric(model.gee$fitted.values > 0.5)
                                            )
                  )

ggplot(dfRes, aes(x = Index, y = Pearson_Residuals, color = Estimation)) + 
  geom_point() + 
  geom_hline(yintercept = 0,color = vColors[3], linetype= 5, cex = 1) +
  scale_color_manual(values = vColors[c(1,2)]) +
  ylab("Pearson Residuals") + xlab("Fitted Values")

ggplot(dfRes, aes(x = dfData$id , y = Pearson_Residuals)) + 
  geom_boxplot(color = vColors[1], outlier.colour = vColors[3]) +
  geom_hline(yintercept = 0,color = vColors[5], linetype= 5, cex = 0.7) +
  geom_hline(yintercept = 2,color = "grey50", linetype= 5, cex = 0.5) +
  geom_hline(yintercept = -2,color = "grey50", linetype= 5, cex = 0.5) +
  ylab("Pearson Residuals") + xlab("id of Child")

# Confusion Matrix

table(as.integer(dfData$success)-1, as.numeric(model.gee$fitted.values > 0.5))

# No overdispersion possible, mean always determines variance:

mean(as.integer(dfData$success)-1)
mean(as.integer(dfData$success)-1) * (1-mean(as.integer(dfData$success)-1))
var(as.integer(dfData$success)-1)


p1 =
ggplot(dfRes, aes(x = dfData$structure , y = Pearson_Residuals)) + 
  geom_boxplot(color = vColors[1], outlier.colour = vColors[3]) +
  geom_hline(yintercept = 0,color = vColors[5], linetype= 5, cex = 0.7) +
  geom_hline(yintercept = 2,color = "grey50", linetype= 5, cex = 0.5) +
  geom_hline(yintercept = -2,color = "grey50", linetype= 5, cex = 0.5) +
  ylab("Pearson Residuals") + xlab("Structure")

p2 =
ggplot(dfRes, aes(x = interaction(dfData$structure, dfData$success) , y = Pearson_Residuals)) + 
  geom_boxplot(color = vColors[1], outlier.colour = vColors[3]) +
  geom_hline(yintercept = 0,color = vColors[5], linetype= 5, cex = 0.7) +
  geom_hline(yintercept = 2,color = "grey50", linetype= 5, cex = 0.5) +
  geom_hline(yintercept = -2,color = "grey50", linetype= 5, cex = 0.5) +
  ylab("Pearson Residuals") + xlab("Interaction Structure with Success")

grid.arrange(p1,p2, nrow = 1)




