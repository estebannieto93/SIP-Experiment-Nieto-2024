library(ggplot2)
library(ggpubr)
library(rstatix)
library(paletteer)
library(openxlsx)
library(DT)
library(viridis)
library(patchwork)
library(reshape)
library(viridis)
library(dplyr)



#LT soil

df.LT= read.csv2("LT-GC.csv")

df.LT$Time <- factor(df.LT$Time, 
                          levels = c(0, 7,15, 30))


#ACY

#identificacion de outliers
df.LT %>%
  group_by(Time) %>%
  identify_outliers(Acenaphthylene)

df.LT.id2= subset(df.LT, df.LT$ID != "2")

# Shapiro-test
df.LT.id2 %>%
  group_by(Time) %>%
  shapiro_test(Acenaphthylene)

#Levene test, The homogeneity of variance assumption of the between-subject factor (group) can be checked using the Levene's test. The test is performed at each level of time variable
df.LT.id2 %>%
  levene_test(Acenaphthylene~ Time)

#Repeated measure
res.aov <- anova_test(
  data = df.LT.id2, dv = Acenaphthylene, wid = ID,
  within = Time
)
get_anova_table(res.aov)


###PHE

#identificacion de outliers
df.LT %>%
  group_by(Time) %>%
  identify_outliers(Phenanthrene)

df.LT.id4= subset(df.LT, df.LT$ID != "4")

df.LT.id4 %>%
  group_by(Time) %>%
  shapiro_test(Phenanthrene)

# Levene test, The homogeneity of variance assumption of the between-subject factor (group) can be checked using the Levene's test. The test is performed at each level of time variable
df.LT.id4 %>%
  levene_test(Phenanthrene ~ Time)

#Repeated measure
res.aov.phe <- anova_test(
  data = df.LT.id4, dv = Phenanthrene, wid = ID,
  within = Time
)
get_anova_table(res.aov.phe)


#ANT


#identificacion de outliers
df.LT %>%
  group_by(Time) %>%
  identify_outliers(Anthracene)

df.LT.id6= subset(df.LT, df.LT$ID != "6")

df.LT.id6 %>%
  group_by(Time) %>%
  shapiro_test(Anthracene)

# Levene test, The homogeneity of variance assumption of the between-subject factor (group) can be checked using the Levene's test. The test is performed at each level of time variable
df.LT.id2 %>%
  levene_test(Anthracene ~ Time)

#Repeated measure
res.aov.ant <- anova_test(
  data = df.LT.id2, dv = Anthracene, wid = ID,
  within = Time
)
get_anova_table(res.aov.ant)

### FLN
df.LT %>%
  group_by(Time) %>%
  identify_outliers(Fluoranthene)


df.LT.id2 %>%
  group_by(Time) %>%
  shapiro_test(Fluoranthene)


#Levene test, The homogeneity of variance assumption of the between-subject factor (group) can be checked using the Levene's test. The test is performed at each level of time variable
df.LT.id2 %>%
  levene_test(Fluoranthene~ Time)
#Repeated measure

res.aov.F <- anova_test(
  data = df.LT.id2, dv = Fluoranthene, wid = ID,
  within = Time
)
get_anova_table(res.aov.F)


### PYR
df.LT %>%
  group_by(Time) %>%
  identify_outliers(Pyrene)


df.LT.id6 %>%
  group_by(Time) %>%
  identify_outliers(Pyrene)

df.LT.id6 %>%
  group_by(Time) %>%
  shapiro_test(Pyrene)


#Levene test, The homogeneity of variance assumption of the between-subject factor (group) can be checked using the Levene's test. The test is performed at each level of time variable
df.LT.id6 %>%
  levene_test(Pyrene~ Time)
#Repeated measure

res.aov.pi <- anova_test(
  data = df.LT.id6, dv = Pyrene, wid = ID,
  within = Time
)
get_anova_table(res.aov.pi)


#B[a]A
df.LT %>%
  group_by(Time) %>%
  identify_outliers(Benzoanthracene)


df.LT.id1= subset(df.LT, df.LT$ID != "1")

df.LT.id1 %>%
  group_by(Time) %>%
  shapiro_test(Benzoanthracene)


# Levene test
df.LT.id1 %>%
levene_test(Benzoanthracene ~ Time)

#Repeated measure 
res.aov.Benzoa <- anova_test(
  data = df.LT.id1, dv = Benzoanthracene, wid = ID,
  within = Time
)
get_anova_table(res.aov.Benzoa)


#CRY
df.LT.id4= subset(df.LT, df.LT$ID != "4")

df.LT.id4 %>%
  group_by(Time) %>%
  identify_outliers(Crysene)


df.LT.id4 %>%
  group_by(Time) %>%
  shapiro_test(Crysene)


# Levene test,
df.LT.id4%>%levene_test(Crysene ~ Time)

#ANOVA
res.aov.Cri <- anova_test(
  data = df.LT.id4, dv = Crysene, wid = ID,
  within = Time
)
get_anova_table(res.aov.Cri)

#B[a]P
df.LT %>%
  group_by(Time) %>%
  identify_outliers(Benzoapireno)


df.LT.id1= subset(df.LT, df.LT$ID != "1")

df.LT.id1 %>%
  group_by(Time) %>%
  shapiro_test(Benzoapireno)


# Levene test
df.LT.id1 %>%
  levene_test(Benzoapireno ~ Time)

#Repeated measure 
res.aov.BenzoaP <- anova_test(
  data = df.LT.id1, dv = Benzoapireno, wid = ID,
  within = Time
)
get_anova_table(res.aov.BenzoaP)






#graph

df.LT2= df.LT[,2:11]

LT=melt(df.LT2)

LT$PAH= factor(LT$variable,
               levels=c("Acenaphthylene",  "Anthracene", "Phenanthrene", "pireno","Fluoranthene", "Benzo.a.anthracene", "Crysene", "Benzoapireno"),
               labels= c("ACY","ANT", "PHN", "PYR", "FLN", "B[a]A", "CRY", "B[a]P"))

LT$Time <- factor(LT$Time, 
                     levels = c(0, 7,15, 30))

LT$PAH= factor(LT$variable,
                levels=c("Acenaphthylene",  "Anthracene", "Phenanthrene","Pyrene","Fluoranthene", "Benzo(a)anthracene", "Crysene", "Benzoapireno"),
                labels= c("ACY","ANT", "PHN", "PYR", "FLN", "B[a]A", "CRY", "B[a]P"))


g1=ggbarplot(LT, x="PAH", y= "value", 
             fill= "Time", 
             add = "mean_sd", 
             position = position_dodge(), width = 0.9,
             error.bar.color = "black") + theme_bw()+
  scale_fill_viridis(discrete = T, begin=0.2, end=0.6)+
  ylab(bquote('mg.Kg dry soil '^-1))+ xlab(NULL)


###ST-soil

df.ST=read.csv2("ST-GC.csv")

df.ST[is.na(df.ST)] <- 0

#PYR
df.ST%>%
  group_by(Time) %>%
  shapiro_test(PYR)

#identificacion de outliers
df.ST %>%
  group_by(Time) %>%
  identify_outliers(PYR)

df.ST.id1= subset(df.ST, df.ST$ID != "1")

#Repeated measure
res.aov.P <- anova_test(
  data = df.ST.id1, dv = PYR, wid = ID,
  within = Time
)
get_anova_table(res.aov.P)

##PHN
df.ST%>%
  group_by(Time) %>%
  shapiro_test(PHN)

#identificacion de outliers
df.ST %>%
  group_by(Time) %>%
  identify_outliers(PHN)


#Repeated measure
res.aov.F <- anova_test(
  data = df.ST, dv = PHN, wid = ID,
  within = Time
)
get_anova_table(res.aov.F)


#ANT
df.ST%>%
  group_by(Time) %>%
  shapiro_test(ANT)

#identificacion de outliers
df.ST %>%
  group_by(Time) %>%
  identify_outliers(ANT)


#Repeated measure
res.aov.ANT <- anova_test(
  data = df.ST.id1, dv = ANT, wid = ID,
  within = Time
)
get_anova_table(res.aov.ANT)



#FLU
df.ST%>%
  group_by(Time) %>%
  shapiro_test(FLU)

#identificacion de outliers
df.ST %>%
  group_by(Time) %>%
  identify_outliers(FLU)


#Repeated measure
res.aov.FLU <- anova_test(
  data = df.ST, dv = FLU, wid = ID,
  within = Time
)
get_anova_table(res.aov.FLU)




pah= read.csv2("ST-data.csv")

pah$PAH= factor(pah$PAH,
                levels = c("Fluorene","Anthracene", "Phenanthrene", "Pyrene"),
                labels = c("FLU", "ANT", "PHE", "PYR"))

pah$Tiempo= factor(pah$Tiempo)

g2=ggbarplot(pah, x="PAH", y= "Value", 
             fill= "Tiempo", 
             add = "mean_sd", 
             position = position_dodge(), width = 0.95,
             error.bar.color = "black") + theme_bw()+
  xlab(NULL)

#Pairwise comparison by PAH species
stat.test <- pah %>%
  group_by(PAH) %>%
  pairwise_t_test(
    Value ~ Tiempo, paired = TRUE,
    p.adjust.method = "holm"
  )

stat.test 

View(stat.test)


stat.test$PAH= factor(stat.test$PAH,
                      levels = c("FLU", "ANT","PHE", "PYR"))

stat.test$p.adj.signif[4:6]= "ns"

stat.test= stat.test%>% add_xy_position(x = "PAH")

g2=g2 + stat_pvalue_manual(stat.test, tip.length = 0.010, hide.ns = T) + scale_fill_viridis(discrete = T, begin=0.2, end=0.6)

g2=g2 +ylab(bquote('mg.Kg dry soil '^-1))


G3=g2+g1+plot_layout(guides = "collect")+ plot_annotation(tag_levels = "A")  & labs(fill= "Time (days)") &
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=13),
        legend.text = element_text(size=12),
        legend.title = element_text(size=13)) & ylim(0,600)

ggsave("Figure 1.jpg", G3,  dpi=300, height= 5, width = 11)
ggsave("Figure 1.tiff", G3,  dpi=300, height= 5, width = 11)
