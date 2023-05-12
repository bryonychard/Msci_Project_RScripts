library(survival)
library(survminer)
library(ggplot2)
library("readxl")


#sorting and analysing melanisation of galleria exps
library(tidyverse)

#create vector of time and batch values
#for Kp there are 7 time points, 50 galleria per tp (in fact PBS and 10-3 treatment only had 5 larvae, but will remove these later) )
time_hrs<-rep(c(0,12,14,16,18,20,22,24), c(90,90,90,90,90,90,90,90))

treatment<-gl(5,5,50)

b<-seq(6, 10, by = 1)
box2<-rep(c(b), c(5,5,5,5,5))

df <- data.frame(treatment, box2)

treatment <- data.frame(value = c(df$treatment, df$box2))

treatment <- treatment[-c(71,72,73,74,75,96,97,98,99,100),]
treatment <- data.frame(treatment)

merged_df <- rbind(treatment, treatment, treatment, treatment, treatment, treatment, treatment, treatment)


# combine time,treatment
df1<-data.frame(time_hrs,merged_df)

#set working directory to where csv result files are stored
setwd("~/Desktop")
#use function list.files to create a vector of names of files
raw.files <- tibble(filename = list.files('imgs'))
#add column with full path to file as extra column
raw.file.paths <- raw.files  %>%
  mutate(filepath = paste0('imgs/', filename))
#Create a new R function called read.csv.and.add.filename which expects to be passed a path to a csv file as an input. 
#This function reads the csv file at the path (converting it to a dataframe)
#and adds a new column containing the original file path it read from. It then returns this dataframe
read.csv.and.add.filename <- function(filepath){
  read_csv(filepath) %>%
    mutate(filepath=filepath)
}

#use 'do' function to read in csv and create combined dataframe, combined with function above, add pathway so know which file it's from
raw.data.with.paths <- raw.file.paths %>%
  rowwise() %>%
  do(., read.csv.and.add.filename(.$filepath))

df2<-raw.data.with.paths

#fist column has no name so call it "well"
colnames(df2)[1] <- "well"

#combine combined melanisation data (df2) with time/treatment (df1) 
df3<-cbind(df1, df2)

#add column 1-melanisation so that plots show increasing melanisation
df3$inv <- 1 - df3$Melanisation

#function to calculate mean and sd to draw error bars on plot
data_summary <- function(data, varname, groupnames, conf.interval=.95){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE), N=length(x[[col]]))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  data_sum$se <- data_sum$sd / sqrt(data_sum$N)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, data_sum$N-1)
  data_sum$ci <- data_sum$se * ciMult
  return(data_sum)
}

#create new df with the summary statistics 
df5 <- data_summary(df3, varname="inv", 
                    groupnames=c("treatment","time_hrs"))

# plots

df5$treatment <- factor(df5$treatment)

#plot melanisation data with confidence intervals (or sd/se)
df5 %>%
  ggplot(aes(time_hrs, inv, col = treatment)) +
  geom_point() +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin=inv-ci, ymax=inv+ci), width=.1) +
  xlab("Time (hours)") + ylab("Mean melanisation (inverse pixel intensity)") +
  scale_color_manual(labels = c("LB control","T7 control", "T4 control", "WT control", "Mutant Control", "WT +T7", "WT + T4", "M + T7", "M + T4"), values = c("#000000", "#D55E00", "#009E73", "#56B4E9",  "#CC79A7", "blue", "green", "red", "yellow" )) +
  theme_bw(base_size = 12) + theme(legend.title= element_blank()) + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# T7

df5_t7 <- df5[-c(33:40, 49:56, 65:72),]

df5_t7 <- df5_t7 %>% arrange(factor(treatment, levels = c('1', '2', '4', '3', '6', '8')))

df5_t7 %>%
  ggplot(aes(time_hrs, inv, col = treatment)) +
  geom_point() +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin=inv-ci, ymax=inv+ci), width=.1) +
  xlab("Time (hours)") + ylab("Mean melanisation (inverse pixel intensity)") +
  scale_color_manual(labels = c("LB control","T7 control", "BL21(DE3) control", "BL21(DE3) ??msbB::kanR Control", "BL21(DE3)", "BL21(DE3) ??msbB::kanR"), values = c("#000000", "#D55E00", "#009E73", "#56B4E9",  "#CC79A7", "#E69F00")) +
  theme_pubr(base_size = 15) +
  theme(legend.title= element_blank(), axis.line = element_line(size = 1),
        axis.text = element_text(size=13),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  
# T4

df5_t4 <- df5[-c(9:16, 41:48, 57:64),]

df5_t42 <- df5_t4 %>% arrange(factor(treatment, levels = c('1', '5', '4', '3', '7', '9')))

df5_t4 %>%
  ggplot(aes(time_hrs, inv, col = treatment)) +
  geom_point() +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin=inv-ci, ymax=inv+ci), width=.1) +
  xlab("Time (hours)") + ylab("Mean melanisation (inverse pixel intensity)") +
  scale_color_manual(labels = c("LB control", "BL21(DE3) ??msbB::kanR Control", "BL21(DE3) control", "T4 Control", "BL21(DE3)", "BL21(DE3) ??msbB::kanR"), values = c("#000000", "#56B4E9", "#009E73", "#D55E00",  "#CC79A7", "#E69F00")) +
  theme_pubr(base_size = 15) +
  theme(legend.title= element_blank(), axis.line = element_line(size = 1), 
        axis.text = element_text(size=13),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# linear model test

df3$treatment <- factor(df3$treatment)

library(lme4)
m1 <- lm(inv ~ treatment*as.factor(time_hrs), data = df3)
summary(m1)
anova(m1)
par(mfrow = c(2, 2))
plot(m1)

hist(residuals(m1))
qqnorm(residuals(m1))


# CFE

#sorting and analysing melanisation of galleria exps
library(tidyverse)

#create vector of time and batch values
#for Kp there are 7 time points, 50 galleria per tp (in fact PBS and 10-3 treatment only had 5 larvae, but will remove these later) )
time_hrs<-rep(c(0,12,14,16,18,20,22,24), c(30,30,30,30,30,30,30,30))

treatment<-gl(3,5,30)


# combine time,treatment
df1<-data.frame(time_hrs,treatment)

#set working directory to where csv result files are stored
setwd("~/Desktop")
#use function list.files to create a vector of names of files
raw.files <- tibble(filename = list.files('imgs2.5'))
#add column with full path to file as extra column
raw.file.paths <- raw.files  %>%
  mutate(filepath = paste0('imgs2.5/', filename))
#Create a new R function called read.csv.and.add.filename which expects to be passed a path to a csv file as an input. 
#This function reads the csv file at the path (converting it to a dataframe)
#and adds a new column containing the original file path it read from. It then returns this dataframe
read.csv.and.add.filename <- function(filepath){
  read_csv(filepath) %>%
    mutate(filepath=filepath)
}

#use 'do' function to read in csv and create combined dataframe, combined with function above, add pathway so know which file it's from
raw.data.with.paths <- raw.file.paths %>%
  rowwise() %>%
  do(., read.csv.and.add.filename(.$filepath))

df2<-raw.data.with.paths

#fist column has no name so call it "well"
colnames(df2)[1] <- "well"

#combine combined melanisation data (df2) with time/treatment (df1) 
df3<-cbind(df1, df2)

#add column 1-melanisation so that plots show increasing melanisation
df3$inv <- 1 - df3$Melanisation

#function to calculate mean and sd to draw error bars on plot
data_summary <- function(data, varname, groupnames, conf.interval=.95){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE), N=length(x[[col]]))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  data_sum$se <- data_sum$sd / sqrt(data_sum$N)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, data_sum$N-1)
  data_sum$ci <- data_sum$se * ciMult
  return(data_sum)
}

#create new df with the summary statistics 
df5 <- data_summary(df3, varname="inv", 
                    groupnames=c("treatment","time_hrs"))

# plots

df5$treatment <- factor(df5$treatment)

#plot melanisation data with confidence intervals (or sd/se)
df5 %>%
  ggplot(aes(time_hrs, inv, col = treatment)) +
  geom_point() +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin=inv-ci, ymax=inv+ci), width=.1) +
  xlab("Time (hours)") + ylab("Mean melanisation (inverse pixel intensity)") +
  scale_color_manual(labels = c("LB control","BL21(DE3)", "BL21(DE3) ??msbB::kanR"), values = c("#000000", "#56B4E9",  "#E69F00")) +
  theme_pubr(base_size = 15) +
  theme(legend.title= element_blank(), axis.line = element_line(size = 1), 
        axis.text = element_text(size=13),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))





# linear model test

df3$treatment <- factor(df3$treatment)

library(lme4)
m1 <- lm(inv ~ treatment*as.factor(time_hrs), data = df3)
summary(m1)
anova(m1)
par(mfrow = c(2, 2))
plot(m1)

hist(residuals(m1))
qqnorm(residuals(m1))

