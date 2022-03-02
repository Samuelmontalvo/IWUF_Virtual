library(readxl)
Lactate_Data <- read_excel("Lactate_Data.xlsx")
View(Lactate_Data)
attach(Lactate_Data)

library(tidyverse)

Lactate_Data <- Lactate_Data %>% pivot_longer(Pre:`Post 10`, names_to = 
                                                "Period", values_to = "Lactate")

Lactate_Data <- Lactate_Data %>% rename(Sex = Gender) %>% 
                 mutate(Sex = case_when(Sex == "M"  ~ "Male", 
                                        Sex =="F" ~ "Female"))

Lactate_Data$Period <- factor(Lactate_Data$Period)

Lactate_Data$Period <- ordered(Lactate_Data$Period, 
                               levels = c("Pre","Post 0",
                               "Post 3","Post 5","Post 10"))

library(writexl)
write_xlsx(Lactate_Data,"Lactate_Data2.xlsx")
Lactate_Data2 <- read_excel("Lactate_Data2.xlsx")
View(Lactate_Data2)


  #distribution test
library(rstatix)
Lactate_Data2 %>% group_by(Period) %>% shapiro_test(Lactate)

# Distribution plots
library(ggplot2)
library(ggridges)
Lactate_Data2 %>% ggplot(aes(x=Lactate,y=Period, fill=Period)) +
  geom_density_ridges(rel_min_height = 0.005) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) + 
  scale_fill_brewer(palette = 5) +
  theme_ridges() 



library(lme4)
library(lmerTest)

LmemModel <- lmer(Lactate ~ Period + (1|ID), data = Lactate_Data2)

summary(LmemModel)
Anova(LmemModel)

# GLMM model
#test of the random effects in the model
rand(GlmmModel)

library(kableExtra)

pwc <- Lactate_Data2 %>% pairwise_t_test(Lactate ~ Period, paired = T,
                  p.adjust.method	= "holm")
pwc %>% kable()%>%
  kable_classic_2(full_width = F)


# Effect size Cohen's D with Hedge's g correction for small sample size
effect <- Lactate_Data2 %>% cohens_d(Lactate ~ Period,
                 paired = TRUE, hedges.correction = TRUE)
effect %>% kable()%>%
  kable_classic_2(full_width = F)


Lactate_Data2 %>%
  ggplot(aes(x=Period,y=Lactate, fill=Period)) + geom_boxplot() +
  geom_jitter() +
  scale_fill_brewer(palette="Spectral") + ylab("Lactate (mmol/L)") +
  theme_bw()
             


library(ggpubr)
ggboxplot(Lactate_Data2, x = "Period", y = "Lactate",
        color = "Period", palette = get_palette("Dark2", 8),
        ylab = ylab("Lactate (mmol/L)")) +
  stat_pvalue_manual(pwc,hide.ns = TRUE, y.position = 20, 
                     step.increase = 0.1) + geom_jitter()


Lactate_Plot <- ggboxplot(Lactate_Data2, x = "Period", y = "Lactate",
          color = "Period", palette = get_palette("Dark2", 8),
          ylab = ylab("Blood Lactate (mmol/L)"))  +
  stat_pvalue_manual(pwc,hide.ns = TRUE, y.position = 20, 
                     step.increase = 0.1)
ggsave("Lactate_plot.png")


##NEW Data with JH and Lactate
NSCA_2022 <- read_excel("NSCA_2022.xlsx")
View(NSCA_2022)

statpvalue <- NSCA_2022 %>% filter(is.na(JH) == FALSE) %>%  t_test(JH  ~ Period, paired = TRUE)

JH_plot <- NSCA_2022 %>% filter(is.na(JH) == FALSE) %>% 
  ggboxplot(x = "Period", y = "JH",
                color = "Period", palette = c("#00AFBB", "#E7B800"),
                order = c("Pre", "Post 0"),
                ylab = "Jump Height (m)", xlab = "Groups")+  
  stat_compare_means(method = "t.test", paired = TRUE, bracket.size = 10,
                     label.x = 1.4, 
                     label.y = .7) 


ggarrange(Lactate_Plot,JH_plot,
          labels = c("A","B"))
ggsave("NSCA_Plot.png")

ggsave("JH_plot.png")
library(rstatix)
cor <- cor.test(NSCA_2022$Lactate, NSCA_2022$JH, method = "pearson",
                exact=FALSE)
cor$p
plot(NSCA_2022$Lactate,NSCA_2022$JH)