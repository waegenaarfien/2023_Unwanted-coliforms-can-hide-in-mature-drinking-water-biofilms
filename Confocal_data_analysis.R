setwd("/Projects1/Fien/BiofilmRings")

# Importing and reading the data
library(openxlsx)
library(ggplot2)
library(ggsignif)
data <- read.xlsx("/Projects1/Fien/BiofilmRings/Summary_comstat_confocal microscopy.xlsx",sheet=11,colNames=TRUE)
 
# Averages 
Biomass_av <- aggregate(x= data$`Average.Thickness.(over.entire.surface)`,
                          by=list(data$Place, data$Stain, data$BioRep),
                          FUN= function(x) c(avg = mean(x), sdev = sd(x)))
Biomass_av <- data.frame(cbind(Biomass_av[,1:3], Biomass_av$x[,1:2]))

Biomass_av_av <- aggregate(x= Biomass_av$avg,
                        by=list(Biomass_av$Group.1, Biomass_av$Group.2),
                        FUN= function(x) c(avg = mean(x), sdev = sd(x)))
Biomass_av_av <- data.frame(cbind(Biomass_av_av[,1:2], Biomass_av_av$x[,1:2]))

colnames(Biomass_av_av) <- c("Place","Stain","Avg.Biomass","Sd.Biomass")

colours <- c("#FFA633","#338CFF","#33FF40","#F2FF33","#33FFF7")

positions <- c("DAPI", "Sypro", "ConA")

plot_biomass <- ggplot(data=Biomass_av_av, aes(x = Stain, y = Avg.Biomass, fill = Place))+
  geom_bar(stat = "identity", position = position_dodge())  +
  geom_errorbar(aes(ymin=Avg.Biomass-Sd.Biomass,ymax=Avg.Biomass+Sd.Biomass, x = Stain),width=0.2, position=position_dodge(.9))+
  scale_x_discrete(limits = positions, labels = c("DAPI", "Sypro Ruby", "Con A")) +
  scale_fill_manual(values = colours)+
  scale_colour_manual(values = colours)+
  scale_fill_discrete(name = "Source", labels = c("Groundwater", "Surface water"))+
  labs(x="Stain", y = "Biomass (µm³/µm²)", size =3)+
  theme_bw(base_size = 30, base_family = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.background = element_rect(colour = "black", fill = "#EFEFEE"))
plot_biomass

# Export
dpi=300
png("figures/Biomass.png",width=12*dpi,height=8*dpi,res=dpi)
plot_biomass
dev.off()

##########################
library(ggpubr)
data_cells <- data[data$Stain == "DAPI",]
data_sypro <- data[data$Stain == "Sypro",]
data_prot <- data[data$Stain == "ConA",]
mean(data_sypro[data_sypro$Place == "SK",]$Biomass)
sd(data_sypro[data_sypro$Place == "SK",]$Biomass)

ggqqplot((data_prot$Biomass))
shapiro.test(data_prot$Biomass) # normal on log transformed data
wilcox.test(data_sypro[data_sypro$Place == "OT",]$Biomass,data_sypro[data_sypro$Place == "SK",]$Biomass)

bartlett.test(log10(Biomass) ~ Place, data = data_cells) #homogenic variances

# two sample t.test: p < 0.05


## boxplot
plot_biomass <- ggplot(data=data, aes(x = Stain, y = Biomass, fill = Place))+
  geom_boxplot(outlier.colour="black",
               outlier.size=2, position=position_dodge(1))+
  stat_compare_means(aes(group = Place), label = "p.signif", method = "t.test",size = 6, label.y = 10, method.args = list(var.equal = TRUE))+
  scale_x_discrete(limits = positions, name = NULL, labels = c("Cells", "Proteins", "Sugars")) +
  scale_fill_brewer(palette="Accent", name = "", labels = c("Treated groundwater", "Treated surface water"))+
  labs(y = "Biovolume (µm³/µm²)", size =2)+
  theme_bw(base_size = 20, base_family = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.background = element_rect(colour = "black", fill = "#EFEFEE"))
plot_biomass

# Export
dpi=300
png("figures/Biomass_boxplot.png",width=12*dpi,height=8*dpi,res=dpi)
plot_biomass
dev.off()


############### other parameters + statistics

ggqqplot(log10(data_sypro$`Average.Thickness.(over.entire.surface)`))
shapiro.test(log10(data_cells$`Average.Thickness.(over.entire.surface)`)) # normal on log transformed data

bartlett.test(log10(data_prot$`Average.Thickness.(over.entire.surface)`) ~ Place, data = data_cells) #homogenic variances, no for roughness

# for samples that are not all the same, check amount of stars
t.test(data_sypro[data_cells$Place == "SK",]$MaxThickness , data_cells[data_cells$Place == "OT",]$MaxThickness, var.equal = TRUE)
wilcox.test(data_sypro[data_cells$Place == "SK",]$Roughness , data_sypro[data_sypro$Place == "OT",]$Roughness)

# two sample t.test: p < 0.05

## boxplot, normal + homogeen (except for cells but ok)
plot1 <- ggplot(data=data, aes(x = Stain, y = MaxThickness, fill = Place))+
  geom_boxplot(outlier.colour="black",
               outlier.size=2, position=position_dodge(1))+
  stat_compare_means(aes(group = Place), label = "p.signif", method = "t.test" ,size = 6,label.y = 157, method.args = list(var.equal = TRUE))+
  scale_x_discrete(limits = positions, name = NULL, labels = c("Cells", "Proteins", "Sugars")) +
  scale_fill_brewer(palette="Accent", name = "Source", labels = c("Groundwater", "Surface water"))+
  labs(y = "Maximum thickness (µm)", size =2)+
  theme_bw(base_size = 20, base_family = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.background = element_rect(colour = "black", fill = "#EFEFEE"))
plot1

# Export
dpi=300
png("figures/Maxthickness_boxplot.png",width=12*dpi,height=8*dpi,res=dpi)
plot1
dev.off()

#Roughness: normal, no homogenic variances, not normal, only protein 
plot2 <- ggplot(data=data, aes(x = Stain, y = Roughness, fill = Place))+
  geom_boxplot(outlier.colour="black",
               outlier.size=2, position=position_dodge(1))+
  stat_compare_means(aes(group = Place), label = "p.signif", method = "wilcox.test",size = 6,label.y = 1.95)+
  scale_x_discrete(limits = positions, name = NULL,labels = c("Cells", "Proteins", "Sugars")) +
  scale_fill_brewer(palette="Accent", name = "", labels = c("Treated groundwater", "Treated surface water"))+
  labs(y = "Roughness (-)", size =2)+
  theme_bw(base_size = 20, base_family = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.background = element_rect(colour = "black", fill = "#EFEFEE"))
plot2

# Export
dpi=300
png("figures/Roughness_boxplot.png",width=12*dpi,height=8*dpi,res=dpi)
plot2
dev.off()

#Av thickness: normal + homogenic variances
plot3 <- ggplot(data=data, aes(x = Stain, y = `Average.Thickness.(over.entire.surface)`, fill = Place))+
  geom_boxplot(outlier.colour="black",
               outlier.size=2, position=position_dodge(1))+
  stat_compare_means(aes(group = Place), label = "p.signif", method = "t.test",size = 6,label.y = 54,method.args = list(var.equal = TRUE))+
  scale_x_discrete(limits = positions,name=NULL, labels = c("Cells", "Proteins", "Sugars")) +
  scale_fill_brewer(palette="Accent", name = "", labels = c("Treated groundwater", "Treated surface water"))+
  labs(y = "Average thickness (µm)", size =2)+
  theme_bw(base_size = 20, base_family = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.background = element_rect(colour = "black", fill = "#EFEFEE"))
plot3

# Export
dpi=300
png("figures/AVGhickness_boxplot.png",width=12*dpi,height=8*dpi,res=dpi)
plot3
dev.off()

library(ggpubr)
combined <- ggarrange(
  plot_biomass, plot2, plot1, plot3, labels = c("A", "B", "C", "D"),font.label = list(size = 16),
  common.legend = TRUE, legend = "bottom"
)
combined
# Export
dpi=400
png("figures/combined_boxplot.png",width=16*dpi,height=10*dpi,res=dpi)
combined
dev.off()


combined4 <- ggarrange(
  plot_biomass, plot2, plot3, 
  nrow = 1, labels = c("D", "E", "F"),font.label = list(size = 16),
  common.legend = TRUE, legend = "bottom"
)
combined4
# Export
dpi=300
png("figures/combined_boxplot4.png",width=7*dpi,height=14*dpi,res=dpi)
combined4
dev.off()



### Sypro op dapi, ConA op dapi
dapi <- data[data$Stain == "DAPI",]
sypro <- data[data$Stain == "Sypro",]
cona <- data[data$Stain == "ConA",]

for (i in 1:length(dapi$Place)){
  dapi$prot[i] <- sypro$Biomass[i]/dapi$Biomass[i]
  dapi$sug[i] <- cona$Biomass[i]/dapi$Biomass[i]
}

shapiro.test(log10(dapi$prot)) #normal on log10

bartlett.test(log10(dapi$prot) ~ Place, data = dapi) #homogenic variances, p < 0.05

t.test(dapi[dapi$Place == "SK",]$sug , dapi[dapi$Place == "OT",]$sug, var.equal = TRUE) #ns


## boxplot
plot_prot <- ggplot(data=dapi, aes(x = Place, y = prot))+
  geom_boxplot(outlier.colour="black",
               outlier.size=2, position=position_dodge(1))+
  stat_compare_means(method = "t.test",size = 6, method.args = list(var.equal = TRUE), label.x.npc = "middle")+
  scale_x_discrete(name=NULL, labels = c("Treated groundwater", "Treated surface water")) +
  #geom_errorbar(aes(ymin=Avg.Biomass-Sd.Biomass,ymax=Avg.Biomass+Sd.Biomass, x = Stain),width=0.2, position=position_dodge(.9))+
  #scale_x_discrete(limits = positions, labels = c("Cells", "Proteins", "Sugars")) +
  #scale_fill_brewer(palette="Accent", name = "Source", labels = c("Groundwater", "Surface water"))+
  #scale_colour_manual(values = colours)+
  #scale_fill_discrete(name = "Source", labels = c("Groundwater", "Surface water"))+
  labs(y = "µm³ proteins/µm³ biomass", size =2)+
  theme_bw(base_size = 25, base_family = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.background = element_rect(colour = "black", fill = "#EFEFEE"))
plot_prot

# Export
dpi=300
png("figures/Biomass_boxplot_prot.png",width=12*dpi,height=8*dpi,res=dpi)
plot_biomass
dev.off()

## boxplot
plot_sug <- ggplot(data=dapi, aes(x = Place, y = sug))+
  geom_boxplot(outlier.colour="black",
               outlier.size=2, position=position_dodge(1))+
  stat_compare_means(method = "t.test",size = 6, method.args = list(var.equal = TRUE), label.x.npc = "middle")+
  scale_x_discrete(name=NULL, labels = c("Treated groundwater", "Treated surface water")) +
  #geom_errorbar(aes(ymin=Avg.Biomass-Sd.Biomass,ymax=Avg.Biomass+Sd.Biomass, x = Stain),width=0.2, position=position_dodge(.9))+
  #scale_x_discrete(limits = positions, labels = c("Cells", "Proteins", "Sugars")) +
  #scale_fill_brewer(palette="Accent", name = "Source", labels = c("Groundwater", "Surface water"))+
  #scale_colour_manual(values = colours)+
  #scale_fill_discrete(name = "Source", labels = c("Groundwater", "Surface water"))+
  labs(y = "µm³ sugars/µm³ biomass", size =3)+
  theme_bw(base_size = 25, base_family = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.background = element_rect(colour = "black", fill = "#EFEFEE"))
plot_sug

# Export
dpi=300
png("figures/Biomass_boxplot_sugars.png",width=12*dpi,height=8*dpi,res=dpi)
plot_biomass
dev.off()


combined_CLSM_app <- ggarrange(
  plot_sug, plot_prot, 
  nrow = 1, labels = c("A", "B"),font.label = list(size = 16),
  common.legend = TRUE, legend = "bottom"
)
combined_CLSM_app
# Export
dpi=300
png("figures/combined_CLSM_appendix.png",width=14*dpi,height=6*dpi,res=dpi)
combined_CLSM_app
dev.off()


