###############################################################################

# GE-prefix ###################################################################

###############################################################################


# pakketten opladen #----------------------------------------------------------

library(lme4)
library(effects)
library(dplyr)
library(MASS)
library(Hmisc)
library(vcd)
library(car)

library(tidyverse)
library(mclm)
library(skimr)

library(lmerTest)
library(party)
library(afex)
library(ggeffects)

library(lattice)
library(MuMIn)

rm(list = ls())
options(scipen = 10)

# dataset inlezen #------------------------------------------------------------

GE <- read.csv("ge_prefix.csv", sep=",", header=TRUE,
               stringsAsFactors = TRUE)

str(GE)

# opnieuw ordenen #------------------------------------------------------------

table(GE$ge_prefix)
GE$ge_prefix <- relevel(GE$ge_prefix, ref = "nee")

table(GE$werkwoord)
GE$werkwoord <- relevel(GE$werkwoord, ref = "worden")

table(GE$lokalisering)
GE$lokalisering <- relevel(GE$lokalisering, ref = "Vlaanderen")

# halfcentury aanmaken op basis van decennium #--------------------------------

GE$halfcentury <- ifelse(GE$decennium < 1350, 1300, ifelse(GE$decennium < 1400, 1350,
                  ifelse(GE$decennium < 1450, 1400, ifelse(GE$decennium < 1500, 1450,
                  ifelse(GE$decennium < 1550, 1500, ifelse(GE$decennium < 1600, 1550,
                  1600))))))
table(GE$halfcentury)
xtabs(~ decennium + halfcentury, data = GE)

# werkwoord_short aanmaken met kortere benaming #------------------------------

GE$werkwoord_short <- ifelse(GE$werkwoord == "blijven", "bl",
                            ifelse(GE$werkwoord == "brengen", "br",
                            ifelse(GE$werkwoord == "komen", "ko",
                            ifelse(GE$werkwoord == "lijden", "li",
                            ifelse(GE$werkwoord == "worden", "wo", "vi")))))
GE$werkwoord_short <-  as.factor(GE$werkwoord_short)

# verkennen van de dataset#----------------------------------------------------

# ge-prefix:
t <- xtabs(~ ge_prefix, data = GE) %>%
  print()
t %>%
  prop.table() %>%
  round(2)
ggplot(GE) +
  geom_bar(aes(x = ge_prefix, fill = ge_prefix))

# decennium:
t <- xtabs(~ decennium, data = GE) %>%
  print()
t %>%
  prop.table() %>%
  round(2)
ggplot(GE) +
  geom_bar(aes(x = decennium, fill = decennium)) +
  ylab("aantal")
ggplot(GE) +
  geom_bar(aes(x = halfcentury, fill = halfcentury))

# werkwoord:
t <- xtabs(~ werkwoord, data = GE) %>%
  print()
t %>%
  prop.table() %>%
  round(2)
ggplot(GE) +
  geom_bar(aes(x = werkwoord, fill = werkwoord))

# werkwoordsvolgorde:
t <- xtabs(~ werkwoordsvolgorde, data = GE) %>%
  print()
t %>%
  prop.table() %>%
  round(2)
ggplot(GE) +
  geom_bar(aes(x = werkwoordsvolgorde, fill = werkwoordsvolgorde))

# zinstype:
t <- xtabs(~ zinstype, data = GE) %>%
  print()
t %>%
  prop.table() %>%
  round(2)
ggplot(GE) +
  geom_bar(aes(x = zinstype, fill = zinstype))

# genre:
t <- xtabs(~ genre, data = GE) %>%
  print()
t %>%
  prop.table() %>%
  round(2)
ggplot(GE) +
  geom_bar(aes(x = genre, fill = genre))

# lokalisering:
t <- xtabs(~ lokalisering, data = GE) %>%
  print()
t %>%
  prop.table() %>%
  round(2)
ggplot(GE) +
  geom_bar(aes(x = lokalisering, fill = lokalisering))

ggplot(GE) +
  geom_bar(aes(x = decennium, fill = lokalisering))
a <- ggplot(GE) +
  geom_bar(aes(x = halfcentury, fill = lokalisering)) +
  ylab("aantal") +
  xlab("halve eeuw") +
  labs(fill = "lokalisering")
a
a +
  scale_fill_grey()

# verkennende analyse: decision tree #-----------------------------------------

GE_ctree <- ctree(ge_prefix ~ decennium + werkwoord + werkwoordsvolgorde +
                    lokalisering + zinstype + genre,
                  data = GE)
plot(GE_ctree)

GE_ctree2 <- ctree(ge_prefix ~ decennium + werkwoord_short + werkwoordsvolgorde +
                     lokalisering + zinstype + genre,
                   data = GE)
plot(GE_ctree2)

# verkennende analyse: random forest #-----------------------------------------

set.seed(123)
GE_forest <- cforest(ge_prefix ~ decennium + werkwoord + werkwoordsvolgorde +
                       lokalisering + zinstype + genre,
                     data = GE)

forest_varimp <- varimp(GE_forest, conditional = FALSE)

# C-index berekenen; tussen 0,5 en 1: hoe dichter bij 1, hoe beter het model
prob2.rf <- unlist(treeresponse(GE_forest))[c(FALSE, TRUE)]
somerssmallcrf <- somers2(prob2.rf, as.numeric(GE$ge_prefix) - 1)
somerssmallcrf["C"]

dotplot(sort(forest_varimp), xlab="variable importance", panel = function(x,y){
  panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1)
  panel.abline(v=abs(min(forest_varimp)), col='red',
               lty='longdash', lwd=2)
}
)

# meervoudig analysemodel ZONDER interacties (met random effect) #-------------

# basismodel:
summary(basicmodel <- glmer(ge_prefix ~ decennium + werkwoord +
                              werkwoordsvolgorde + zinstype + genre +
                              lokalisering + (1|titel),
                            family=binomial, control = glmerControl(optimizer = "bobyqa",
                            optCtrl = list(maxfun = 10000)), data = GE))
# geen significantie bij werkwoordsvolgorde en zinstype

# Likelihood Ratio Test:
basicmodel.lrt <- mixed(
  ge_prefix ~ decennium + werkwoord + werkwoordsvolgorde + zinstype + genre +
    lokalisering + (1|titel),
  data = GE, family = "binomial",
  method = "LRT",
  control = glmerControl(optimizer = "bobyqa")
) %>%
  print()
# de uitvoering kan enkele minuten duren
# werkwoordsvolgorde en zinstype maken het model niet beter

# tweede basismodel (zonder werkwoordsvolgorde en zinstype):
summary(basicmodel2 <- glmer(ge_prefix ~ decennium + werkwoord + genre +
                               lokalisering + (1|titel),
                             family=binomial,
                             control = glmerControl(optimizer = "bobyqa",
                             optCtrl = list(maxfun = 10000)), data = GE))

# meervoudig analysemodel MET interacties (met random effect) #----------------

# eerste basismodel met interacties:
summary(basicmodelinteract <- glmer(ge_prefix ~ decennium*werkwoord + genre +
                                      lokalisering*decennium + (1|titel),
                                    family=binomial,
                                    control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 10000)), data = GE))

# Likelihood Ratio Test:
basicmodelinteract.lrt <- mixed(
  ge_prefix ~ decennium*werkwoord + genre + decennium*lokalisering + (1|titel),
  data = GE, family = "binomial",
  method = "LRT",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000))
) %>%
  print()
# de uitvoering kan enkele minuten duren
# lokalisering, decennium:werkwoord en decennium:lokalisering maken
# het model niet beter

# tweede basismodel met interacties:
summary(basicmodelinteract2 <- glmer(ge_prefix ~ decennium + werkwoord + genre +
                                       lokalisering*decennium + (1|titel),
                                     family=binomial,
                                     control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 10000)), data = GE))
# plots verdwijnen van eerdere significantiescores

# derde basismodel met interacties:
summary(basicmodelinteract3 <- glmer(ge_prefix ~ decennium*werkwoord + genre +
                                       lokalisering + (1|titel),
                                     family=binomial,
                                     control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 10000)), data = GE))
# model voldoet het beste aan de noden

r.squaredGLMM(basicmodelinteract3)

plot(allEffects(basicmodelinteract3))
plot(predictorEffect("genre", basicmodelinteract3), colors="blue",
     multiline=TRUE, ylim=c(0,1), rescale.axis=FALSE, xlab="genre",
     ylab="kans op ge-prefix", main="")
plot(predictorEffect("lokalisering", basicmodelinteract3), colors="blue",
     multiline=TRUE, ylim=c(0,1), rescale.axis=FALSE, xlab="enge lokalisering",
     ylab="kans op ge-prefix", main="")
plot(predictorEffect("decennium", basicmodelinteract3), multiline=TRUE,
     ylim=c(0,1), rescale.axis=FALSE, xlab="decennium",
     ylab="kans op ge-prefix", main="")
plot(predictorEffect("werkwoord", basicmodelinteract3), multiline=TRUE,
     ylim=c(0,1), rescale.axis=FALSE, xlab="werkwoord",
     ylab="kans op ge-prefix", main="")
plot(effect("werkwoord", basicmodelinteract3), colors="blue", multiline=FALSE,
     ylim=c(0,1), rescale.axis=FALSE, xlab="werkwoord",
     ylab="kans op ge-prefix", main="")
plot(effect("decennium", basicmodelinteract3), colors="blue", multiline=FALSE,
     ylim=c(0,1), rescale.axis=FALSE, xlab="decennium",
     ylab="kans op ge-prefix", main="")

# correlatiemaat werkwoord en log(freq) #--------------------------------------

# correlatie met hoofdeffect:
correlatie <- data.frame(estimates = c(0, 9.221, 18.652, 2.282, 12.723),
                         logfreq = c(3.808, 3.241, 3.542, 3.952, 4.168))
correlatie

res <- cor.test(correlatie$estimates, correlatie$logfreq, method = c("pearson"))
res

# correlatie met interactie-effect:
correlatie2 <- data.frame(estimates = c(0, -0.006, -0.012, -0.002, -0.008),
                          logfreq = c(3.808, 3.241, 3.542, 3.952, 4.168))
correlatie2

res2 <- cor.test(correlatie2$estimates, correlatie2$logfreq, method = c("pearson"))
res2