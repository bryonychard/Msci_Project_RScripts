library("readxl")

my_data <- read_excel("PFU_T7.xlsx")

my_data$Sample <- factor(my_data$Sample, levels = c("T7C", "WT7", "M7"))

# Square root transformation of PFU
my_data$PFU_sqrt <- sqrt(my_data$PFU)

library(ggplot2)
library(ggpubr)

ggplot(my_data, aes(x = Sample, y = PFU_sqrt, fill = Sample)) +
  geom_boxplot(outlier.size = 2, outlier.shape=15, alpha = 0.7, lwd=1, fatten = 1.2) +
  geom_jitter(color = "black", size = 2, alpha=0.7, position=position_jitter(0)) +
  stat_boxplot(geom ='errorbar', color="black", size=1, width=0.6) +
  stat_summary(fun = "mean", colour = "black", size = 3, geom = "point", shape=8) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) + # Custom fill colors
  ylab(expression(bold(sqrt("PFU/mL")))) +
  scale_x_discrete(labels=c('T7 control', 'BL21(DE3)', 'BL21(DE3) ??msbB::kanR')) +
  theme_pubr(base_size = 20) + # Use classic theme
  theme(axis.line = element_line(size = 1.2), # Custom axis line thickness
        axis.text.x = element_text(angle=30, vjust = 1, hjust=1, size=13), # Custom x-axis text size
        axis.text.y = element_text(size = 13), # Custom y-axis text size
        axis.title = element_text(size = 14, face = "bold"), # Custom axis title size and font
        legend.position = "none", # Remove legend
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines
        panel.background = element_blank(), # Remove plot background
        strip.background = element_rect(fill = "#E0E0E0"), # Custom panel strip background color
        strip.text = element_text(size = 12, face = "bold")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 35000))



# linear model test

# comparing to T7 control

t7_lm1 <- lm(PFU_sqrt ~ Sample, my_data)
summary(t7_lm1)
par(mfrow = c(2, 2))
plot(t7_lm1)

# comparing to WT7

my_data$Sample <- as.factor(my_data$Sample)

my_data$Sample <- my_data$Sample %>% relevel("WT7")

t7_lm2 <- lm(PFU_sqrt ~ Sample, my_data)
summary(t7_lm2)


my_data$Sample <- my_data$Sample %>% relevel("M7")

t7_lm3 <- lm(PFU_sqrt ~ Sample, my_data)
summary(t7_lm3)






# t4

library("readxl")

my_data <- read_excel("PFU_t4.xlsx")

my_data$Sample <- factor(my_data$Sample, levels = c("T4C", "WT4", "M4"))

my_data$PFU <- ifelse(my_data$PFU == 0, 1e-10, my_data$PFU)

# Square root transformation of PFU
my_data$PFU_sqrt <- sqrt(my_data$PFU)

ggplot(my_data, aes(x = Sample, y = PFU_sqrt, fill = Sample)) +
  geom_boxplot(outlier.size = 2, outlier.shape=15, alpha = 0.7, lwd=0.8, fatten = 1.2) +
  geom_jitter(color = "black", size = 2, alpha=0.7, position=position_jitter(0)) +
  stat_boxplot(geom ='errorbar', color="black", size=1, width=0.6) +
  stat_summary(fun = "mean", colour = "black", size = 3, geom = "point", shape=8) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) + # Custom fill colors
  ylab(expression(bold(sqrt("PFU/mL")))) +
  scale_x_discrete(labels=c('T4 control', 'BL21(DE3)', 'BL21(DE3) ??msbB::kanR')) +
  theme_pubr() + # Use classic theme
  theme(axis.line = element_line(size = 1.2), # Custom axis line thickness
        axis.text.x = element_text(angle=30, vjust = 1, hjust=1, size=13), # Custom x-axis text size
        axis.text.y = element_text(size = 13), # Custom y-axis text size
        axis.title = element_text(size = 14, face = "bold"), # Custom axis title size and font
        legend.position = "none", # Remove legend
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines
        panel.background = element_blank(), # Remove plot background
        strip.background = element_rect(fill = "#E0E0E0"), # Custom panel strip background color
        strip.text = element_text(size = 12, face = "bold")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 35000))

# linear mode]

# Comparing to control

t4_lm1 <- lm(PFU_sqrt ~ Sample, my_data)
summary(t4_lm1)
par(mfrow = c(2, 2))
plot(t4_lm1)

# Comparing to WT4

my_data$Sample <- as.factor(my_data$Sample)

my_data$Sample <- my_data$Sample %>% relevel("WT4")

t4_lm2 <- lm(PFU_sqrt ~ Sample, my_data)
summary(t4_lm2)
par(mfrow = c(2, 2))
plot(t4_lm2)

