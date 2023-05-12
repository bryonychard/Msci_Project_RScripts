library(cowplot)
library(tidyverse)
library(dplyr)
library(tidyr)

df = read_csv('mutant_2_re.csv')


df = df %>% mutate(treatment=ifelse(grepl('^B', well), 'BLK',
                                    ifelse(grepl('^C', well), 'BL21(DE3)',
                                           ifelse(grepl('^D', well), 'BL21(DE3) ??lpxM::KanR', 
                                                  ifelse(grepl('^F', well), 'BL21(DE3) ??lpxM::KanR', 
                                                         ifelse(grepl('^E', well), 'E', 'Unknown'))))))

df$treatment <- factor(df$treatment, levels = c("BLK", "BL21(DE3)", "BL21(DE3) ??lpxM::KanR"))

mean <- df %>% group_by(treatment, time_hours) %>% summarize(mean=mean(mean_od))

mean2 <- mean[-c(50:147),]

df2 <- cbind(df, mean2)

df3 <- df2[,-5]
df3 <- df3[,-5]

df3$OD <- (df3$mean_od - df3$mean)

df4 <- df3[-c(1:490),]

df4[df4$OD < 0, "OD"] <- 0

#Now we have a group treatment for each well, we can group them together on a single plot per group.
# we just need to specify the group in the geom_line function.


ggplot(df4, aes(x=time_hours, y=OD)) +
  geom_line(aes(group=well), size=0.5, color='#0072B2') +
  scale_x_continuous('Time (hours)', expand = c(0, 0)) +
  scale_y_continuous('OD600', expand = c(0, 0), limits=c(0, NA)) +
  facet_wrap(~treatment) +
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())




