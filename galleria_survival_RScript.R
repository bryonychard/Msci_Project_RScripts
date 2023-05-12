library(survival)
library(survminer)
library(ggplot2)
library("readxl")

survival <- read_excel("t7_survival.xlsx")

str(survival)
Km<-survfit(Surv(survivetime, status)~treat_int, data=survival) #kaplan-Meier
survdiff(Surv(survivetime,status)~treatment, data=survival) #logrank sig test
survival$treatment
survival$treat_int

km_pairs<-pairwise_survdiff(Surv(survivetime, status)~treatment, data=survival, p.adjust.method = "BH")
km_pairs

#dose_response plot for Kp and Ec
t7_surv_plot<-ggsurvplot(
  Km,
  data = survival,
  surv.median.line = "hv",
  size = 1.1, xlab="Time (hours)",                # change line size
  palette = c("#000000", "#D55E00", "#009E73", "#56B4E9", "#CC79A7", "#E69F00"),
  font.x =  c(14, "bold"),
  font.y = c(14, "bold"),
  font.tickslab = c(12, "bold"),
  xlim = c(0, 25),
  legend.title = "",
  legend.labs = c("LB control","T7 Control", "BL21(DE3) Control", "BL21(DE3) ??msbB::kanR Control", "BL21(DE3)", "BL21(DE3) ??msbB::kanR"),
  ggtheme = theme_classic(base_size = 16))

t7_surv_plot  



survival <- read_excel("t4_survival.xlsx")

str(survival)
Km<-survfit(Surv(survivetime, status)~treat_int, data=survival) #kaplan-Meier
survdiff(Surv(survivetime,status)~treatment, data=survival) #logrank sig test
survival$treatment
survival$treat_int

km_pairs<-pairwise_survdiff(Surv(survivetime, status)~treatment, data=survival, p.adjust.method = "BH")
km_pairs

#dose_response plot for Kp and Ec
t4_surv_plot<-ggsurvplot(
  Km,
  data = survival,
  surv.median.line = "hv",
  size = 1.1, xlab="Time (hours)",                # change line size
  palette = c("#000000", "#D55E00", "#009E73", "#56B4E9", "#CC79A7", "#E69F00"),
  font.x =  c(14, "bold"),
  font.y = c(14, "bold"),
  font.tickslab = c(12, "bold"),
  xlim = c(0, 25),
  legend.title = "",
  legend.labs = c("LB control","T4 Control", "BL21(DE3) Control", "BL21(DE3) ??msbB::kanR Control", "BL21(DE3)", "BL21(DE3) ??msbB::kanR"),
  ggtheme = theme_classic(base_size = 16))    # Change ggplot2 theme

t4_surv_plot



survival <- read_excel("CFE_survival2.xlsx")

str(survival)
Km<-survfit(Surv(survivetime, status)~treat_int, data=survival) #kaplan-Meier
survdiff(Surv(survivetime,status)~treatment, data=survival) #logrank sig test
survival$treatment
survival$treat_int

km_pairs<-pairwise_survdiff(Surv(survivetime, status)~treatment, data=survival, p.adjust.method = "BH")
km_pairs

#dose_response plot for Kp and Ec
cfe_surv_plot<-ggsurvplot(
  Km,
  data = survival,
  surv.median.line = "hv",
  size = 1.1, xlab="Time (hours)",                # change line size
  palette = c("#000000", "#56B4E9", "#E69F00"),
  font.x =  c(14, "bold"),
  font.y = c(14, "bold"),
  font.tickslab = c(12, "bold"),
  xlim = c(0, 25),
  legend.title = "",
  legend.labs = c("LB control", "BL21(DE3)", "BL21(DE3) ??msbB::kanR"),
  ggtheme = theme_classic(base_size = 16))    # Change ggplot2 theme

cfe_surv_plot

