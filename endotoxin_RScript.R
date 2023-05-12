
library(ggplot2)
library(ggpubr)

# load data

library("readxl")

df <- read_excel("endotoxin_allvalues.xlsx")

df$Sample <- factor(df$Sample, levels = c("WTC", "MC", "T7C", "WT7", "M7", "T4C", "WT4", "M4", "CFEWT", "CFEM"))

library(plyr)
mean_se<-ddply(df, "Sample", summarize, mean=mean(EU), sd=sd(EU))

mean_se$Sample <- factor(mean_se$Sample,
                         levels = c("WTC", "MC", "T7C", "WT7", "M7", "T4C", "WT4", "M4", "CFEWT", "CFEM"))

p <- ggplot(mean_se, aes(x = Sample, y = mean, fill = Sample)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", size = 1.3) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5, color = "black", size=1.2) +
  labs(x = "Sample", y = "Endotoxin units / mL") +
  scale_x_discrete(labels=c('BL21(DE3) control', 'BL21(DE3) ??msbB::kanR  control', 'T7 control', 'BL21(DE3) T7', 'BL21(DE3) ??msbB::kanR T7', 'T4 control', 'BL21(DE3) T4', 'BL21(DE3) ??msbB::kanR T4', 'BL21(DE3) CFE', 'BL21(DE3) ??msbB::kanR CFE')) +
  scale_fill_manual(values=c("grey", "grey", "#0072B2", "#0072B2", "#0072B2", "#009E73", "#009E73", "#009E73", "#D55E00", "#D55E00")) +
  theme_pubr(base_size = 16) +
  theme(axis.title = element_text(size = 18, face = "bold"),
        axis.line = element_line(size = 1.3),
        axis.text.x = element_text(angle=50, vjust = 1, hjust=1, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.y = element_text(vjust = 2, hjust = 0.7),
        legend.position = "none")

scientific_10 <- function(x) {   parse(text=gsub("e\\+*", " %*% 10^", scales::scientific_format()(x))) }

library(ggbreak)

p + scale_y_break(c(75000, 4000000), scales = 1) + scale_y_continuous(label=scientific_10)



# linear model

df$Sample <- as.factor(df$Sample)

EU_lm1 <- lm(EU ~ Sample, df)
summary(EU_lm1)
par(mfrow = c(2, 2))
plot(EU_lm1)

df$Sample <- df$Sample %>% relevel("CFEWT")

EU_lm2 <- lm(EU ~ Sample, df)
summary(EU_lm2)
par(mfrow = c(2, 2))
plot(EU_lm2)

df$Sample <- df$Sample %>% relevel("CFEM")

EU_lm3 <- lm(EU ~ Sample, df)
summary(EU_lm3)
par(mfrow = c(2, 2))
plot(EU_lm3)



# T7

# load data

library("readxl")

my_data <- read_excel("EUmL_T7_all.xlsx")

my_data$Sample <- factor(my_data$Sample, levels = c("WTC", "MC", "T7C", "WT7", "M7"))

library(plyr)

mean_se<-ddply(my_data, "Sample", summarize, mean=mean(EU), sd=sd(EU))

mean_se$Sample <- factor(mean_se$Sample,
                         levels = c("WTC", "MC", "T7C", "WT7", "M7"))

library(ggplot2)
library(ggpubr)

p <- ggplot(mean_se, aes(x = Sample, y = mean, fill = Sample)) +
  geom_bar(stat = "identity", width = 0.8, color = "black", size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5, color = "black", size=1.1) +
  labs(x = "Sample", y = "Endotoxin units / mL") +
  scale_x_discrete(labels=c('BL21(DE3) control', 'BL21(DE3) ??msbB::kanR  control', 'T7 control', 'BL21(DE3)', 'BL21(DE3) ??msbB::kanR')) +
  scale_fill_manual(values=c("#009E73", "#0072B2", "black", "#D55E00", "#CC79A7")) +
  theme_pubr(base_size = 16) +
  theme(axis.title = element_text(size = 18, face = "bold"),
        axis.line = element_line(linewidth = 1.3),
        axis.text.x = element_text(angle=35, vjust = 1, hjust=1),
        legend.position = "none")

scientific_10 <- function(x) {   parse(text=gsub("e\\+*", " %*% 10^", scales::scientific_format()(x))) }

p + scale_y_continuous(label=scientific_10, expand = c(0, 0), limits = c(0, NA))

# Linear model

t7_lm1 <- lm(EU ~ Sample, my_data)
summary(t7_lm1)
par(mfrow = c(2, 2))
plot(t7_lm1)


my_data$Sample <- as.factor(my_data$Sample)

my_data$Sample <- my_data$Sample %>% relevel("MC")

t7_lm2 <- lm(EU ~ Sample, my_data)
summary(t7_lm2)
par(mfrow = c(2, 2))
plot(t7_lm2)

my_data$Sample <- my_data$Sample %>% relevel("WT7")

t7_lm3 <- lm(EU ~ Sample, my_data)
summary(t7_lm3)
par(mfrow = c(2, 2))
plot(t7_lm3)


my_data$Sample <- my_data$Sample %>% relevel("M7")

t7_lm4 <- lm(EU ~ Sample, my_data)
summary(t7_lm4)
par(mfrow = c(2, 2))
plot(t7_lm4)



# T4

# load data

library("readxl")

my_data <- read_excel("EUml_T4_all.xlsx")

my_data$Sample <- factor(my_data$Sample, levels = c("WTC", "MC", "T4C", "WT4", "M4"))

library(plyr)

mean_se<-ddply(my_data, "Sample", summarize, mean=mean(EU), sd=sd(EU))

mean_se$Sample <- factor(mean_se$Sample,
                         levels = c("WTC", "MC", "T4C", "WT4", "M4"))

# plot graph

library(ggplot2)
library(ggpubr)


p <- ggplot(mean_se, aes(x = Sample, y = mean, fill = Sample)) +
  geom_bar(stat = "identity", width = 0.8, color = "black", size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5, color = "black", size=1.1) +
  labs(x = "Sample", y = "Endotoxin units / mL") +
  scale_x_discrete(labels=c('BL21(DE3) control', 'BL21(DE3) ??msbB::kanR  control', 'T4 control', 'BL21(DE3)', 'BL21(DE3) ??msbB::kanR')) +
  scale_fill_manual(values=c("#009E73", "#0072B2", "black", "#D55E00", "#CC79A7")) +
  theme_pubr(base_size = 16) +
  theme(axis.title = element_text(size = 18, face = "bold"),
        axis.line = element_line(linewidth = 1.3),
        axis.text.x = element_text(angle=35, vjust = 1, hjust=1),
        axis.title.y = element_text(hjust=0.8),
        legend.position = "none")


scientific_10 <- function(x) {   parse(text=gsub("e\\+*", " %*% 10^", scales::scientific_format()(x))) }

p + scale_y_continuous(label=scientific_10, expand = c(0, 0), limits = c(0, NA))

# Linear model

t4_lm1 <- lm(EU ~ Sample, my_data)
summary(t4_lm1)
par(mfrow = c(2, 2))
plot(t4_lm1)


my_data$Sample <- as.factor(my_data$Sample)

my_data$Sample <- my_data$Sample %>% relevel("MC")

t4_lm2 <- lm(EU ~ Sample, my_data)
summary(t4_lm2)
par(mfrow = c(2, 2))
plot(t4_lm2)

my_data$Sample <- my_data$Sample %>% relevel("WT4")

t4_lm3 <- lm(EU ~ Sample, my_data)
summary(t4_lm3)
par(mfrow = c(2, 2))
plot(t4_lm3)


my_data$Sample <- my_data$Sample %>% relevel("M4")

t4_lm4 <- lm(EU ~ Sample, my_data)
summary(t4_lm4)
par(mfrow = c(2, 2))
plot(t4_lm4)



# CFE

# load data

library("readxl")

my_data <- read_excel("EUml_CFE_all.xlsx")

my_data$Sample <- factor(my_data$Sample, levels = c("CFEWT", "CFEM"))


# Linear model

CFE_lm1 <- lm(EU ~ Sample, my_data)
summary(CFE_lm1)
par(mfrow = c(2, 2))
plot(CFE_lm1)


# plot graph

library(plyr)

mean_se<-ddply(my_data, "Sample", summarize, mean=mean(EU), sd=sd(EU))

mean_se$Sample <- factor(mean_se$Sample,
                         levels = c("CFEWT", "CFEM"))

p <- ggplot(mean_se, aes(x = Sample, y = mean, fill = Sample)) +
  geom_bar(stat = "identity", width = 0.8, color = "black", size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5, color = "black", size=1.1) +
  labs(x = "Cell Extract", y = "Endotoxin units / mL") +
  scale_x_discrete(labels=c('BL21(DE3)', 'BL21(DE3) ??msbB::kanR')) +
  scale_fill_manual(values=c("#009E73", "#0072B2")) +
  theme_pubr(base_size = 16) +
  theme(axis.title = element_text(size = 18, face = "bold"),
        axis.line = element_line(linewidth = 1.3),
        axis.text.x = element_text(angle=35, vjust = 1, hjust=1),
        legend.position = "none")

scientific_10 <- function(x) {   parse(text=gsub("e\\+*", " %*% 10^", scales::scientific_format()(x))) }

p + scale_y_continuous(label=scientific_10, expand = c(0, 0), limits = c(0, NA))  