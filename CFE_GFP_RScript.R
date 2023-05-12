# load data

library("readxl")

my_data <- read_excel("gfp_data.xlsx")

# mean of buffer

library(dplyr)

buffer_mean <- my_data %>%
  filter(Sample == "Buffer") %>%
  summarise(buffer_mean = mean(Absorbance[1:3])) %>%
  pull()

df2 <- my_data %>%
  mutate(abs_back = Absorbance - buffer_mean)

# remove buffer rows

df3 <- df2[-c(1:3),]

# subtract negative controls from samples

df3$abs_back[2:4] <- df3$abs_back[2:4] - df3$abs_back[1]
df3$abs_back[6:8] <- df3$abs_back[6:8] - df3$abs_back[5]
df3$abs_back[10:12] <- df3$abs_back[10:12] - df3$abs_back[9]

# removes negative controls

df4 <- subset(df3, !(1:nrow(df3) %in% c(1, 5, 9, 13)))

# mean and sd

library(plyr)
mean_se<-ddply(df4, "Sample", summarize, mean=mean(abs_back), sd=sd(abs_back))

# plot

library(ggplot2)
library(ggpubr)

mean_se$Sample <- factor(mean_se$Sample,
                  levels = c("TXTL+", "WT+", "msbB+"))



ggplot(mean_se, aes(x = Sample, y = mean, fill = Sample)) +
  geom_bar(stat = "identity", width = 0.7, fill = "green3", color = "black", size = 1.2) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.4, color = "black", size = 1.1) +
  labs(x = "Cell Extract", y = "Mean Fluorescence Intensity") +
  scale_x_discrete(labels=c('myTXTL', 'BL21(DE3)', 'BL21(DE3) ??msbB::kanR')) +
  theme_pubr(base_size = 18) +
  theme(axis.title = element_text(size = 18, face = "bold"),
        axis.line = element_line(size = 1.1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(angle=35, vjust = 1, hjust=1),
        legend.position = "none") +
  scale_y_continuous(limits = c(0, 20000), expand = c(0, 0))

# linear model

df4$Sample <- as.factor(df4$Sample)

df4$Sample <- df4$Sample %>% relevel("TXTL+")

gfp_lm1 <- lm(abs_back ~ Sample, df4)
summary(gfp_lm1)
par(mfrow = c(2, 2))
plot(gfp_lm1)



df4$Sample <- df4$Sample %>% relevel("WT+")

gfp_lm2 <- lm(abs_back ~ Sample, df4)
summary(gfp_lm2)
par(mfrow = c(2, 2))
plot(gfp_lm2)



# test without myTXTL

# load data

library("readxl")

my_data <- read_excel("gfp_data_2.xlsx")

# mean of buffer

library(dplyr)

buffer_mean <- my_data %>%
  filter(Sample == "Buffer") %>%
  summarise(buffer_mean = mean(Absorbance[1:3])) %>%
  pull()

df2 <- my_data %>%
  mutate(abs_back = Absorbance - buffer_mean)

# remove buffer rows

df3 <- df2[-c(1:3),]

# subtract negative controls from samples

df3$abs_back[2:4] <- df3$abs_back[2:4] - df3$abs_back[1]
df3$abs_back[6:8] <- df3$abs_back[6:8] - df3$abs_back[5]

# removes negative controls

df4 <- subset(df3, !(1:nrow(df3) %in% c(1, 5)))

# linear model

gfp_lm <- lm(abs_back ~ Sample, df4)
summary(gfp_lm)
par(mfrow = c(2, 2))
plot(gfp_lm)

t.test(abs_back ~ Sample, df4)
