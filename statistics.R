setwd("/Projects1/Fien/BiofilmRings")

# Importing and reading the data
library(openxlsx)
ATP <- read.xlsx("/Projects1/Fien/BiofilmRings/Statistics_ATP_FCM.xlsx",sheet=1,colNames=TRUE)
ATP_SK <- ATP[ATP$Place == "SK",]
ATP_OT <- ATP[ATP$Place == "OT",]
ATP_T1 <- ATP[ATP$Timepoint == "T1",]
ATP_T2 <- ATP[ATP$Timepoint == "T2",]
FCM <- read.xlsx("/Projects1/Fien/BiofilmRings/Statistics_ATP_FCM.xlsx",sheet=2,colNames=TRUE)
FCM_SK <- FCM[FCM$Place == "SK",]
FCM_OT <- FCM[FCM$Place == "OT",]
FCM_T1 <- FCM[FCM$Timepoint == "T1",]
FCM_T2 <- FCM[FCM$Timepoint == "T2",]
CLSM <- read.xlsx("/Projects1/Fien/BiofilmRings/Statistics_ATP_FCM.xlsx",sheet=3,colNames=TRUE)
CLSM_SK <- CLSM[CLSM$Place == "SK",]
CLSM_OT <- CLSM[CLSM$Place == "OT",]
CLSM_T1 <- CLSM[CLSM$Timepoint == "T1",]
CLSM_T2 <- CLSM[CLSM$Timepoint == "T2",]

# Check normality, n = 18, p < 0.05: not normal!
library(ggpubr)
ggqqplot((FCM_T1$AConcentration))
shapiro.test((FCM_T1$AConcentration)) #
ggqqplot(FCM_SK$AConcentration)
shapiro.test(log10(FCM_SK$AConcentration)) #normal
ggqqplot(CLSM_T1$Roughness)
shapiro.test((CLSM_T2$Biomass))

# Residual analysis: residuals are normal: data is normal: close to 0.05 for FCM_SK, > 0.05 for FCM_OT, ATP_OT, ATP_SK
# fit a model and check for normality of residuals
model <- lm((Biomass) ~ Place, data = CLSM_T1) #not normal (but was already normal)
shapiro.test(resid(model))

model <- lm((AConcentration) ~ Timepoint, data = FCM_OT) #normal
shapiro.test(resid(model))

# Both normal, SK/OT: not homogen

# perform Levene's test for homogeneity of variances: p < 0.05: non homogenic variances (FCM;ATP_SK); homogenic (ATP_OT)
library(car)
leveneTest(log10(AConcentration) ~ Timepoint, data = FCM_SK)

library(car)
leveneTest((AConcentration) ~ Timepoint, data = FCM_OT)

library(car)
leveneTest(AConcentration ~ Place, data = FCM_T1)

library(car)
leveneTest(Biomass ~ Place, data = CLSM_T1)


# normal residuals + homogenic variances ATP_OT: p < 0.05
t.test(ATP_OT[ATP_OT$Timepoint == "T1",]$ATP , ATP_OT[ATP_OT$Timepoint == "T2",]$ATP, var.equal = TRUE)

t.test(CLSM_OT[CLSM_OT$Timepoint == "T1",]$Biomass , CLSM_OT[CLSM_OT$Timepoint == "T2",]$Biomass, var.equal = TRUE)
t.test(CLSM_OT[CLSM_OT$Timepoint == "T1",]$AvThichkness , CLSM_OT[CLSM_OT$Timepoint == "T2",]$AvThichkness, var.equal = TRUE)
t.test(CLSM_OT[CLSM_OT$Timepoint == "T1",]$Roughness , CLSM_OT[CLSM_OT$Timepoint == "T2",]$Roughness, var.equal = TRUE)
t.test(CLSM_SK[CLSM_SK$Timepoint == "T1",]$AvThichkness , CLSM_SK[CLSM_SK$Timepoint == "T2",]$AvThichkness, var.equal = TRUE)


# normal residuals + non homogenic variances ATP_SK p>0.05; FCM_OT p<0.05
t.test(ATP_SK[ATP_SK$Timepoint == "T1",]$ATP , ATP_SK[ATP_SK$Timepoint == "T2",]$ATP, var.equal = FALSE)
t.test(FCM_OT[FCM_OT$Timepoint == "T1",]$Concentration , FCM_OT[FCM_OT$Timepoint == "T2",]$Concentration, var.equal = FALSE)

t.test(CLSM_SK[CLSM_SK$Timepoint == "T1",]$Biomass , CLSM_SK[CLSM_SK$Timepoint == "T2",]$Biomass, var.equal = FALSE)
t.test(CLSM_SK[CLSM_SK$Timepoint == "T1",]$Roughness , CLSM_SK[CLSM_SK$Timepoint == "T2",]$Roughness, var.equal = FALSE)

t.test((FCM_OT[FCM_OT$Timepoint == "T1",]$Concetration2) , FCM_OT[FCM_OT$Timepoint == "T2",]$Concetration2, var.equal = FALSE)
#p-value = 0.003194

t.test(CLSM_T1[CLSM_T1$Place == "SK",]$Biomass , CLSM_T1[CLSM_T1$Place == "OT",]$Biomass, var.equal = FALSE)


t.test((FCM_OT[FCM_OT$Timepoint == "T1",]$AConcentration) , FCM_OT[FCM_OT$Timepoint == "T2",]$AConcentration, var.equal = FALSE)
wilcox.test(FCM_OT[FCM_OT$Timepoint == "T1",]$AConcentration, FCM_OT[FCM_OT$Timepoint == "T2",]$AConcentration)
#0.003194; 0.01061
t.test((FCM_SK[FCM_SK$Timepoint == "T1",]$AConcentration) , FCM_SK[FCM_SK$Timepoint == "T2",]$AConcentration, var.equal = FALSE)
#0.8121

# not normal residuals /+ non homogenice variances: FCM_SK: p < 0.05
wilcox.test(FCM_SK[FCM_SK$Timepoint == "T1",]$Concentration , FCM_SK[FCM_SK$Timepoint == "T2",]$Concentration)

wilcox.test(CLSM_OT[CLSM_OT$Timepoint == "T1",]$MaxThickness , CLSM_OT[CLSM_OT$Timepoint == "T2",]$MaxThickness)
wilcox.test(CLSM_SK[CLSM_SK$Timepoint == "T1",]$MaxThickness , CLSM_SK[CLSM_SK$Timepoint == "T2",]$MaxThickness)

wilcox.test(FCM_T1[FCM_T1$Place == "SK",]$AConcentration, FCM_T1[FCM_T1$Place == "OT",]$AConcentration)
wilcox.test(CLSM_T2[CLSM_T2$Place == "SK",]$Biomass, CLSM_T2[CLSM_T2$Place == "OT",]$Biomass)
