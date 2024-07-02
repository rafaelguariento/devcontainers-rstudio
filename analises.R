
###datasets###

envir<-read.table("data_eds_2007_2022.txt", header = T)
dim(envir)

###Correlation among variables######

library(corrplot)
M<-cor(envir[,c(3:6)])
head(round(M,2))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200), addCoef.col = "black", type="upper", 
         order="hclust",diag=FALSE)

library(GGally)
pm <- ggpairs(envir[,c(3:6)])
pm

###Choice of variables for model selection###
library(usdm)
vifstep(envir[,c(3:6)])


envir <- na.omit(envir)
sapply(envir, is.numeric)
cols_to_scale <- setdiff(names(envir), c('bee', 'caterpillar', 'scorpion', 'snake', 'spider','ID','year'))
envir[cols_to_scale] <- scale(envir[cols_to_scale])

tools::package_dependencies("Matrix", which = "LinkingTo", reverse = TRUE)[[1L]]
install.packages("lme4", type = "source")

oo <- options(repos = "https://cran.r-project.org/")
utils::install.packages("Matrix")
utils::install.packages("lme4")
options(oo)

library(lme4)
library(MuMIn)

################################Model selection########################
###################Scorpion##########################################

###A null model (no effect of landscape-scale variables or human population)
m1<-glmer(scorpion~1 + (1|ID),data=envir,family=poisson)

###Biodiversity is influenced by landscape-scale variables
m2<-glmer(scorpion~cover_loss + (1|ID), data=envir,family=poisson)
m3<-glmer(scorpion~fire + (1|ID), data=envir,family=poisson)
m4<-glmer(scorpion~urbanization + (1|ID), data=envir,family=poisson)
m5<-glmer(scorpion~cover_loss + fire + (1|ID), data=envir,family=poisson)
m6<-glmer(scorpion~cover_loss + urbanization + (1|ID), data=envir,family=poisson)
m7<-glmer(scorpion~fire + urbanization + (1|ID), data=envir,family=poisson)
m8<-glmer(scorpion~cover_loss + fire + urbanization + (1|ID), data=envir,family=poisson)


model.sel(m1,m2,m3,m4,m5,m6,m7,m8)
sw(model.sel(m1,m2,m3,m4,m5,m6,m7,m8))

####Checking for overdispersion

library(performance)
check_overdispersion(m8)###overdispersion detected

###Model selection with negative binomial####

###A null model (no effect of landscape-scale variables or human population)
m1<-glmer.nb(scorpion~1 + (1|ID),data=envir)

###Biodiversity is influenced by landscape-scale variables
m2<-glmer.nb(scorpion~cover_loss + (1|ID), data=envir)
m3<-glmer.nb(scorpion~fire + (1|ID), data=envir)
m4<-glmer.nb(scorpion~urbanization + (1|ID), data=envir)
m5<-glmer.nb(scorpion~cover_loss + fire + (1|ID), data=envir)
m6<-glmer.nb(scorpion~cover_loss + urbanization + (1|ID), data=envir)
m7<-glmer.nb(scorpion~fire + urbanization + (1|ID), data=envir)
m8<-glmer.nb(scorpion~cover_loss + fire + urbanization + (1|ID), data=envir)


model.sel(m1,m2,m3,m4,m5,m6,m7,m8)
sw(model.sel(m1,m2,m3,m4,m5,m6,m7,m8))

library(sjPlot)
tab_model(m4)
tab_model(m6)
tab_model(m7)
tab_model(m8)

model.avg(m4,m6,m7,m8)

################################Model selection########################
###################Bee##########################################

###A null model (no effect of landscape-scale variables or human population)
m9<-glmer(bee~1 + (1|ID),data=envir,family=poisson)

###Biodiversity is influenced by landscape-scale variables
m10<-glmer(bee~cover_loss + (1|ID), data=envir,family=poisson)
m11<-glmer(bee~fire + (1|ID), data=envir,family=poisson)
m12<-glmer(bee~urbanization + (1|ID), data=envir,family=poisson)
m13<-glmer(bee~cover_loss + fire + (1|ID), data=envir,family=poisson)
m14<-glmer(bee~cover_loss + urbanization + (1|ID), data=envir,family=poisson)
m15<-glmer(bee~fire + urbanization + (1|ID), data=envir,family=poisson)
m16<-glmer(bee~cover_loss + fire + urbanization + (1|ID), data=envir,family=poisson)


model.sel(m9,m10,m11,m12,m13,m14,m15,m16)
sw(model.sel(m9,m10,m11,m12,m13,m14,m15,m16))

####Checking for overdispersion

library(performance)
check_overdispersion(m16)###overdispersion detected

###Model selection with negative binomial####

###A null model (no effect of landscape-scale variables or human population)
m9<-glmer.nb(bee~1 + (1|ID),data=envir)

###Biodiversity is influenced by landscape-scale variables
m10<-glmer.nb(bee~cover_loss + (1|ID), data=envir)
m11<-glmer.nb(bee~fire + (1|ID), data=envir)
m12<-glmer.nb(bee~urbanization + (1|ID), data=envir)
m13<-glmer.nb(bee~cover_loss + fire + (1|ID), data=envir)
m14<-glmer.nb(bee~cover_loss + urbanization + (1|ID), data=envir)
m15<-glmer.nb(bee~fire + urbanization + (1|ID), data=envir)
m16<-glmer.nb(bee~cover_loss + fire + urbanization + (1|ID), data=envir)

model.sel(m9,m10,m11,m12,m13,m14,m15,m16)
sw(model.sel(m9,m10,m11,m12,m13,m14,m15,m16))

tab_model(m16)
summary(m16)


################################Model selection########################
###################Caterpillar##########################################

###A null model (no effect of landscape-scale variables or human population)
m17<-glmer(caterpillar~1 + (1|ID),data=envir,family=poisson)

###Biodiversity is influenced by landscape-scale variables
m18<-glmer(caterpillar~cover_loss + (1|ID), data=envir,family=poisson)
m19<-glmer(caterpillar~fire + (1|ID), data=envir,family=poisson)
m20<-glmer(caterpillar~urbanization + (1|ID), data=envir,family=poisson)
m21<-glmer(caterpillar~cover_loss + fire + (1|ID), data=envir,family=poisson)
m22<-glmer(caterpillar~cover_loss + urbanization + (1|ID), data=envir,family=poisson)
m23<-glmer(caterpillar~fire + urbanization + (1|ID), data=envir,family=poisson)
m24<-glmer(caterpillar~cover_loss + fire + urbanization + (1|ID), data=envir,family=poisson)


model.sel(m17,m18,m19,m20,m21,m22,m23,m24)
sw(model.sel(m17,m18,m19,m20,m21,m22,m23,m24))

####Checking for overdispersion

library(performance)
check_overdispersion(m24)###overdispersion detected

###Model selection with negative binomial####

###A null model (no effect of landscape-scale variables or human population)
m17<-glmer.nb(caterpillar~1 + (1|ID),data=envir)

###Biodiversity is influenced by landscape-scale variables
m18<-glmer.nb(caterpillar~cover_loss + (1|ID), data=envir)
m19<-glmer.nb(caterpillar~fire + (1|ID), data=envir)
m20<-glmer.nb(caterpillar~urbanization + (1|ID), data=envir)
m21<-glmer.nb(caterpillar~cover_loss + fire + (1|ID), data=envir)
m22<-glmer.nb(caterpillar~cover_loss + urbanization + (1|ID), data=envir)
m23<-glmer.nb(caterpillar~fire + urbanization + (1|ID), data=envir)
m24<-glmer.nb(caterpillar~cover_loss + fire + urbanization + (1|ID), data=envir)

model.sel(m17,m18,m19,m20,m21,m22,m23,m24)
sw(model.sel(m17,m18,m19,m20,m21,m22,m23,m24))

tab_model(m23)
tab_model(m24)
model.avg(m23,m24)


################################Model selection########################
###################Snake##########################################

###A null model (no effect of landscape-scale variables or human population)
m25<-glmer(snake~1 + (1|ID),data=envir,family=poisson)

###Biodiversity is influenced by landscape-scale variables
m26<-glmer(snake~cover_loss + (1|ID), data=envir,family=poisson)
m27<-glmer(snake~fire + (1|ID), data=envir,family=poisson)
m28<-glmer(snake~urbanization + (1|ID), data=envir,family=poisson)
m29<-glmer(snake~cover_loss + fire + (1|ID), data=envir,family=poisson)
m30<-glmer(snake~cover_loss + urbanization + (1|ID), data=envir,family=poisson)
m31<-glmer(snake~fire + urbanization + (1|ID), data=envir,family=poisson)
m32<-glmer(snake~cover_loss + fire + urbanization + (1|ID), data=envir,family=poisson)


model.sel(m25,m26,m27,m28,m29,m30,m31,m32)
sw(model.sel(m25,m26,m27,m28,m29,m30,m31,m32))

####Checking for overdispersion

library(performance)
check_overdispersion(m32)###overdispersion detected

###Model selection with negative binomial####

###A null model (no effect of landscape-scale variables or human population)
m25<-glmer.nb(snake~1 + (1|ID),data=envir)

###Biodiversity is influenced by landscape-scale variables
m26<-glmer.nb(snake~cover_loss + (1|ID), data=envir)
m27<-glmer.nb(snake~fire + (1|ID), data=envir)
m28<-glmer.nb(snake~urbanization + (1|ID), data=envir)
m29<-glmer.nb(snake~cover_loss + fire + (1|ID), data=envir)
m30<-glmer.nb(snake~cover_loss + urbanization + (1|ID), data=envir)
m31<-glmer.nb(snake~fire + urbanization + (1|ID), data=envir)
m32<-glmer.nb(snake~cover_loss + fire + urbanization + (1|ID), data=envir)

model.sel(m25,m26,m27,m28,m29,m30,m31,m32)
sw(model.sel(m25,m26,m27,m28,m29,m30,m31,m32))

tab_model(m30)
tab_model(m32)
model.avg(m30,m32)


################################Model selection########################
###################Spider##########################################

###A null model (no effect of landscape-scale variables or human population)
m33<-glmer(spider~1 + (1|ID),data=envir,family=poisson)

###Biodiversity is influenced by landscape-scale variables
m34<-glmer(spider~cover_loss + (1|ID), data=envir,family=poisson)
m35<-glmer(spider~fire + (1|ID), data=envir,family=poisson)
m36<-glmer(spider~urbanization + (1|ID), data=envir,family=poisson)
m37<-glmer(spider~cover_loss + fire + (1|ID), data=envir,family=poisson)
m38<-glmer(spider~cover_loss + urbanization + (1|ID), data=envir,family=poisson)
m39<-glmer(spider~fire + urbanization + (1|ID), data=envir,family=poisson)
m40<-glmer(spider~cover_loss + fire + urbanization + (1|ID), data=envir,family=poisson)


model.sel(m33,m34,m35,m36,m37,m38,m39,m40)
sw(model.sel(m33,m34,m35,m36,m37,m38,m39,m40))

####Checking for overdispersion

library(performance)
check_overdispersion(m40)###overdispersion detected

###Model selection with negative binomial####

###A null model (no effect of landscape-scale variables or human population)
m33<-glmer.nb(spider~1 + (1|ID),data=envir)

###Biodiversity is influenced by landscape-scale variables
m34<-glmer.nb(spider~cover_loss + (1|ID), data=envir)
m35<-glmer.nb(spider~fire + (1|ID), data=envir)
m36<-glmer.nb(spider~urbanization + (1|ID), data=envir)
m37<-glmer.nb(spider~cover_loss + fire + (1|ID), data=envir)
m38<-glmer.nb(spider~cover_loss + urbanization + (1|ID), data=envir)
m39<-glmer.nb(spider~fire + urbanization + (1|ID), data=envir)
m40<-glmer.nb(spider~cover_loss + fire + urbanization + (1|ID), data=envir)

model.sel(m33,m34,m35,m36,m37,m38,m39,m40)
sw(model.sel(m33,m34,m35,m36,m37,m38,m39,m40))

tab_model(m39)
tab_model(m40)
model.avg(m39,m40)


##########Fitting Generalized Linear Mixed-Effects Models without model selection###
###########Scorpion#################

m1<-glmer(scorpion~cover_loss + fire + urbanization + (1|ID), data=envir,family=poisson)

####Checking for overdispersion

check_overdispersion(m1)###overdispersion detected

##negative binomial

m1<-glmer.nb(scorpion~cover_loss + fire + urbanization + (1|ID), data=envir)
tab_model(m1)
summary(m1)

###########bee#################

m1<-glmer(bee~cover_loss + fire + urbanization + (1|ID), data=envir,family=poisson)

####Checking for overdispersion

check_overdispersion(m1)###overdispersion detected

##negative binomial

m1<-glmer.nb(bee~cover_loss + fire + urbanization + (1|ID), data=envir)
tab_model(m1)
summary(m1)

###########caterpillar#################

m1<-glmer(caterpillar~cover_loss + fire + urbanization + (1|ID), data=envir,family=poisson)

####Checking for overdispersion

check_overdispersion(m1)###overdispersion detected

##negative binomial

m1<-glmer.nb(caterpillar~cover_loss + fire + urbanization + (1|ID), data=envir)
tab_model(m1)
summary(m1)

###########snake#################

m1<-glmer(snake~cover_loss + fire + urbanization + (1|ID), data=envir,family=poisson)

####Checking for overdispersion

check_overdispersion(m1)###overdispersion detected

##negative binomial

m1<-glmer.nb(snake~cover_loss + fire + urbanization + (1|ID), data=envir)
tab_model(m1)
summary(m1)

###########spider#################

m1<-glmer(spider~cover_loss + fire + urbanization + (1|ID), data=envir,family=poisson)

####Checking for overdispersion

check_overdispersion(m1)###overdispersion detected

##negative binomial

m1<-glmer.nb(spider~cover_loss + fire + urbanization + (1|ID), data=envir)
tab_model(m1)
summary(m1)


##############GAMM####################################################################
######################################################################################

###########Scorpion#################

library(mgcv)
library(DHARMa)
library(gratia)
library(ggplot2)

m1 <- mgcv::gamm(scorpion ~ s(cover_loss)+s(fire)+s(urbanization), 
                   correlation = corARMA(form = ~ 1| year, p=1), family = poisson, niterPQL=30, data=envir)

summary(m1$gam)##não dá os coeficientes para cada variável

appraise(m1$gam)& #gam check
  theme_bw()

####Checking for overdispersion

res1 <- resid(m1, type="pearson")
overdispersion_sco <- sum(res1^2)/m1$gam$df.residual
overdispersion_sco # 0 = no problem with overdispersion

###Negative binomial

m1 <- mgcv::gamm(scorpion ~ s(cover_loss)+s(fire)+s(urbanization), 
                 correlation = corARMA(form = ~ 1| year, p=1), family = negbin(1), niterPQL=400, data=envir)


###########bee#################

m1 <- mgcv::gamm(bee ~ s(cover_loss)+s(fire)+s(urbanization), 
                 correlation = corARMA(form = ~ 1| year, p=1), family = poisson, niterPQL=30, data=envir)

summary(m1$gam)

appraise(m1$gam)& #gam check
  theme_bw()


####Checking for overdispersion

res1 <- resid(m1, type="pearson")
overdispersion_sco <- sum(res1^2)/m1$gam$df.residual
overdispersion_sco # 0 = no problem with overdispersion

###########caterpillar#################

m1 <- mgcv::gamm(caterpillar ~ s(cover_loss)+s(fire)+s(urbanization), 
                 correlation = corARMA(form = ~ 1| year, p=1), family = poisson, niterPQL=30, data=envir)

summary(m1$gam)


####Checking for overdispersion

res1 <- resid(m1, type="pearson")
overdispersion_sco <- sum(res1^2)/m1$gam$df.residual
overdispersion_sco # 0 = no problem with overdispersion


###########snake#################

m1 <- mgcv::gamm(snake ~ s(cover_loss)+s(fire)+s(urbanization), 
                 correlation = corARMA(form = ~ 1| year, p=1), family = poisson, niterPQL=30, data=envir)

summary(m1$gam)


####Checking for overdispersion

res1 <- resid(m1, type="pearson")
overdispersion_sco <- sum(res1^2)/m1$gam$df.residual
overdispersion_sco # 0 = no problem with overdispersion

###########spider#################

m1 <- mgcv::gamm(spider ~ s(cover_loss)+s(fire)+s(urbanization), 
                 correlation = corARMA(form = ~ 1| year, p=1), family = poisson, niterPQL=30, data=envir)

summary(m1$gam)


####Checking for overdispersion

res1 <- resid(m1, type="pearson")
overdispersion_sco <- sum(res1^2)/m1$gam$df.residual
overdispersion_sco # 0 = no problem with overdispersion



#################NLME######################

###Scorpion

library(nlme)
library(MASS)

# Compare models with same fixed effects and other correlation structure 

m1a <- glmmPQL(scorpion~cover_loss + fire + urbanization,random=~1|ID,data=envir,family=poisson, correlation=corAR1(form = ~ year|ID))

m1b <- glmmPQL(scorpion~cover_loss + fire + urbanization,random=~1|ID,data=envir,family=poisson, correlation=corARMA(form = ~ year|ID,p=1))

m1c <- glmmPQL(scorpion~cover_loss + fire + urbanization,random=~1|ID,data=envir,family=poisson, correlation=corCAR1(form = ~ year|ID))

m1d <- glmmPQL(scorpion~cover_loss + fire + urbanization,random=~1|ID,data=envir,family=poisson, correlation=corExp(form = ~ year|ID))

m1e <- glmmPQL(scorpion~cover_loss + fire + urbanization,random=~1|ID,data=envir,family=poisson, correlation=corGaus(form = ~ year|ID))

m1f <- glmmPQL(scorpion~cover_loss + fire + urbanization,random=~1|ID,data=envir,family=poisson, correlation=corLin(form = ~ year|ID))

m1g <- glmmPQL(scorpion~cover_loss + fire + urbanization,random=~1|ID,data=envir,family=poisson, correlation=corRatio(form = ~ year|ID))

m1h <- glmmPQL(scorpion~cover_loss + fire + urbanization,random=~1|ID,data=envir,family=poisson, correlation=corSpher(form = ~ year|ID))


plot(m1e)

library(r2glmm)

#r2beta Compute R Squared for Mixed Models
#Description: Computes coefficient of determination (R squared) from edwards et al., 2008 and the generalized R squared from Jaeger et al., 2016. Currently implemented for linear mixed models with lmer and lme objects. For generalized linear mixed models, only glmmPQL are supported.

r2beta(m1a, partial = TRUE, method = "sgv")

r.squaredGLMM(m1a)#package MuMIn

###Bee

m1a <- glmmPQL(bee~cover_loss + fire + urbanization,random=~1|ID,data=envir,family=poisson, correlation=corAR1(form = ~ year|ID))

plot(m1a)

r.squaredGLMM(m1a)#package MuMIn

###Caterpillar

m1a <- glmmPQL(caterpillar~cover_loss + fire + urbanization,random=~1|ID,data=envir,family=poisson, correlation=corAR1(form = ~ year|ID))

plot(m1a)

r.squaredGLMM(m1a)#package MuMIn

###Snake

m1a <- glmmPQL(snake~cover_loss + fire + urbanization,random=~1|ID,data=envir,family=poisson, correlation=corAR1(form = ~ year|ID))

plot(m1a)

r.squaredGLMM(m1a)#package MuMIn


###Spider

m1a <- glmmPQL(spider~cover_loss + fire + urbanization,random=~1|ID,data=envir,family=poisson, correlation=corAR1(form = ~ year|ID))

plot(m1a)

r.squaredGLMM(m1a)#package MuMIn