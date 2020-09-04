####### Setup #######

library(drgee)

####### Robust Estimation #######

### First robust Model ###

dfRes.Error = dfRes[dfData$structure == "OR"  & dfRes$Pearson_Residuals > 2.5,]

'%ni%' <- Negate('%in%')

dfData.Robust = dfData[dfRes$Index %ni% dfRes.Error$Index,]

levels(dfData.Robust$age_group) = c("5yo", "7+8yo", "7+8yo")

model.gee.robust = 
  geeglm(as.integer(success)-1 ~ age_group + structure + match_mismatch, 
         data = dfData.Robust, 
         id = id,
         corstr = "exchangeable",
         family = binomial(link = "logit"))

summary(model.gee.robust)
QIC(model.gee.robust)

table(as.integer(dfData.Robust$success)-1, as.numeric(model.gee.robust$fitted.values > 0.5))

dfRes.Robust = data.frame(Index = 1:length(qresiduals(model.gee.robust)), Pearson_Residuals = resid(model.gee.robust, type = "pearson"), Estimation = as.character(as.numeric(model.gee.robust$fitted.values > 0.5)))

ggplot(dfRes.Robust, aes(x = dfData.Robust$id , y = Pearson_Residuals)) + 
  geom_boxplot(color = vColors[1], outlier.colour = vColors[3]) +
  geom_hline(yintercept = 0,color = vColors[5], linetype= 5, cex = 0.7) +
  geom_hline(yintercept = 2,color = "grey50", linetype= 5, cex = 0.5) +
  geom_hline(yintercept = -2,color = "grey50", linetype= 5, cex = 0.5) +
  ylab("Pearson Residuals") + xlab("id of Child")


ggplot(dfRes.Robust, aes(x = dfData.Robust$structure , y = Pearson_Residuals)) + 
  geom_boxplot(color = vColors[1], outlier.colour = vColors[3]) +
  geom_hline(yintercept = 0,color = vColors[5], linetype= 5, cex = 0.7) +
  geom_hline(yintercept = 2,color = "grey50", linetype= 5, cex = 0.5) +
  geom_hline(yintercept = -2,color = "grey50", linetype= 5, cex = 0.5) +
  ylab("Pearson Residuals") + xlab("Structure")


qqnorm(qresiduals(model.gee.robust),pch=16,bty='n',main='Randomized Quantile Residuals', col = vColors[1], ylab = "Sample Quantiles")
qqline(qresiduals(model.gee.robust),lwd=2,col= vColors[5])

model.gee.robust %>% coefficients() %>% exp()


### Second robust Model ###

model.gee.robust2 = drgee(outcome = "success", eformula = formula(success ~ age_group + structure + match_mismatch),
                          iaformula = formula(success~age_group + structure + match_mismatch), elink="logit", 
                          data = dfData, estimation.method = "e")
summary(model.gee.robust2)

getScoreResidualsFromClogit(fit = model.gee.robust2, x =  model.gee.robust2$drgee.data$z, y = as.integer(dfData$success)-1, id = dfData$id)


res <- (as.integer(dfData$success)-1 - model.gee.robust2$fitted) / 
  sqrt(model.gee$fitted * (1 - model.gee$fitted)) 

dfRes = data.frame(Index = 1:length(qresiduals(model.gee)), Pearson_Residuals =res, Estimation = as.character(as.numeric(model.gee$fitted.values > 0.5)))

ggplot(dfRes, aes(x = dfData$id , y = Pearson_Residuals)) + 
  geom_boxplot(color = vColors[1], outlier.colour = vColors[3]) +
  geom_hline(yintercept = 0,color = vColors[5], linetype= 5, cex = 0.7) +
  geom_hline(yintercept = 2,color = "grey50", linetype= 5, cex = 0.5) +
  geom_hline(yintercept = -2,color = "grey50", linetype= 5, cex = 0.5) +
  ylab("Pearson Residuals") + xlab("Structure")
