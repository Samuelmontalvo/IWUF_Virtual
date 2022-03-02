library(readxl)
df <- read_excel("IWUF_Virtual_Data.xlsx",sheet = "Data All")
View(df)
attach(df)

df$Gender <- as.factor(df$Gender)
df$Routine <- as.factor(df$Routine)
df$Period <- as.factor(df$Period)

library(dplyr)
library(rstatix)
df %>% group_by(Tags) %>% shapiro_test(Jump_Height)

## Jump Height
group_by(df, Period) %>%
  summarise(
    count = n(),
    mean = mean(mRSI, na.rm = TRUE),
    sd = sd(mRSI, na.rm = TRUE))

t.test(mRSI ~ Period, data = df, paired = TRUE)
 

# Effect size Cohen's D with Hedge's g correction for small sample size
df  %>% cohens_d(mRSI ~ Period, 
                    paired = TRUE, hedges.correction = TRUE)


library("ggpubr")
JH <- ggboxplot(df, x = "Period", y = "Jump_Height",
          color = "Period", palette = c("#00AFBB", "#E7B800"),
          order = c("Pre", "Post"),
          ylab = "Jump Height (m)", xlab = "Groups")+  
  stat_compare_means(method = "t.test", paired = TRUE,
                     label.x = 1.4, 
                     label.y = .7) + geom_jitter()


ggplot(df,aes(Period,Jump_Height, fill=Period)) + geom_boxplot() +
   ylab("Jump Height (m)") + theme_bw() +
  geom_point() + geom_line(aes(group=ID))


ggsave("vertical_jump.png")

## RSI modified

t.test(mRSI ~ Tags, data = df, paired = TRUE)


library("ggpubr")
ggboxplot(df, x = "Tags", y = "mRSI", 
          color = "Tags", palette = c("#00AFBB", "#E7B800"),
          order = c("Pre", "Post"),
          ylab = "RSI modified", xlab = "Groups")+
  stat_compare_means(method = "t.test", paired = TRUE, label.x = 1.4, 
                     label.y = .75)




## Propulsive net impulse

t.test(Propulsive_Net_Impulse ~ Tags, data = df, paired = TRUE)


library("ggpubr")
ggboxplot(df, x = "Tags", y = "Propulsive_Net_Impulse", 
          color = "Tags", palette = c("#00AFBB", "#E7B800"),
          order = c("Pre", "Post"),
          ylab = "Propulsive Net Impulse (m/s)", xlab = "Groups")+ 
  geom_point(aes(group=Tags), position = position_dodge(0.2)) +
  geom_line(aes(group=ID)) +
  stat_compare_means(method = "t.test", paired = TRUE, label.x = 1.4, 
                     label.y = 250) 



