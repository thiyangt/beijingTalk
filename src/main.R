## ---- packages
library(tidyverse)
library(patchwork)
library(png)
library(tsfeatures)
library(RColorBrewer)
library(ggcorrplot) # to draw  ggcorrplot
library(M4comp2018)
library(GGally) # to draw scatterplot matrix

## ---- lengthboxplot
yearly_M4 <- Filter(function(l) l$period == "Yearly", M4)
quarterly_M4 <- Filter(function(l) l$period == "Quarterly", M4)
monthly_M4 <- Filter(function(l) l$period == "Monthly", M4)
weekly_M4 <- Filter(function(l) l$period == "Weekly", M4)
daily_M4 <- Filter(function(l) l$period == "Daily", M4)
hourly_M4 <- Filter(function(l) l$period == "Hourly", M4)

length_Y <- data.frame(length=sapply(yearly_M4, function(temp){length(c(temp$x, temp$xx))}))
length_Q <- data.frame(length=sapply(quarterly_M4, function(temp){length(c(temp$x, temp$xx))}))
length_M <- data.frame(length=sapply(monthly_M4, function(temp){length(c(temp$x, temp$xx))}))
length_W <- data.frame(length=sapply(weekly_M4, function(temp){length(c(temp$x, temp$xx))}))
length_D <- data.frame(length=sapply(daily_M4, function(temp){length(c(temp$x, temp$xx))}))
length_H <- sapply(hourly_M4, function(temp){length(c(temp$x, temp$xx))})

len_y <- ggplot(length_Y, aes(y = length, x = "yearly"))+
  geom_point(position=position_jitter(width=0.1), color=alpha("red",0.1)) +
    stat_boxplot(geom='errorbar', linetype=1, width=0.1, color="blue", size=1.25)+
      geom_boxplot(outlier.size=0, width=0.1)+theme(axis.title.x=element_blank(), axis.text.x=element_blank())+ggtitle("yearly")
len_q <- ggplot(length_Q, aes(y = length, x = "quarterly"))+
  geom_point(position=position_jitter(width=0.1), color=alpha("red",0.1)) +
  stat_boxplot(geom='errorbar', linetype=1, width=0.1, color="blue", size=1.25)+
  geom_boxplot(outlier.size=0, width=0.1)+theme(axis.title.x=element_blank(), axis.text.x=element_blank())+ggtitle("quarterly")
len_m <- ggplot(length_Q, aes(y = length, x = "monthly"))+
  geom_point(position=position_jitter(width=0.1), color=alpha("red",0.1)) +
  stat_boxplot(geom='errorbar', linetype=1, width=0.1, color="blue", size=1.25)+
  geom_boxplot(outlier.size=0, width=0.1)+theme(axis.title.x=element_blank(), axis.text.x=element_blank())+ggtitle("monthly")
len_w <- ggplot(length_W, aes(y = length, x = "weekly"))+
  geom_point(position=position_jitter(width=0.1), color=alpha("red",0.1)) +
  stat_boxplot(geom='errorbar', linetype=1, width=0.1, color="blue", size=1.25)+
  geom_boxplot(outlier.size=0, width=0.1)+theme(axis.title.x=element_blank(), axis.text.x=element_blank())+ggtitle("weekly")
len_d <- ggplot(length_D, aes(y = length, x = "daily"))+
  geom_point(position=position_jitter(width=0.1), color=alpha("red",0.1)) +
  stat_boxplot(geom='errorbar', linetype=1, width=0.1, color="blue", size=1.25)+
  geom_boxplot(outlier.size=0, width=0.1)+theme(axis.title.x=element_blank(), axis.text.x=element_blank())+ggtitle("daily")
df <- data.frame(length=c("748", "1008"), frequency=c(169, 245))
len_h <- ggplot(df, aes(x=length, y=frequency)) +
  geom_bar(stat="identity")+ggtitle("hourly")
((len_y | len_q | len_m) / (len_w|len_d|len_h))

## ---- pca
# yearly
source("src/pcaprojection.R")
load("data/yearly/features_M4Y.rda")
load("data/yearly/yearly_features_training.rda")
## Construct PCA space from simulated, M1 and M3 and then project M4-competition data
pca_ref_calc <- calculate_pca(yearly_features_training)
# vars_transformed <- apply(pca_ref_calc$prcomp_out$x, 2, var)
# vars_transformed/sum(vars_transformed)
ref_pca <- pca_ref_calc$pca_components
## Project M4 yearly data
load("data/yearly/features_M4Y.rda")
M4Y_PCA <- pca_projection(pca_ref_calc$prcomp_out, features_M4Y)
## draw PCA plot
pca_all_yearly <- bind_rows(ref_pca, M4Y_PCA,  .id="source")
pcaY1 <- ggplot(pca_all_yearly, aes(x=PC1, y=PC2, color=source)) + geom_point(alpha=0.2)+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", NA))+theme(aspect.ratio = 1, axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold"))+
  ggtitle("Yearly")+geom_point(data = pca_all_yearly[1:826, ], aes(x=PC1, y=PC2), colour="black", alpha=0.5)+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 8, face = "bold"))
pcaY2 <- ggplot(pca_all_yearly, aes(x=PC1, y=PC2, color=source)) + geom_point(alpha=0.2)+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1"))+theme(aspect.ratio = 1, axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold"))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 8, face = "bold"))

# quarterly
load("data/quarterly/features_M4Q.rda")
load("data/quarterly/quarterly_features_training.rda")
## Construct PCA space from simulated, M1 and M3 and then project M4-competition data
pca_ref_calc_q <- calculate_pca(quarterly_features_training)
# vars_transformedq <- apply(pca_ref_calc_q$prcomp_out$x, 2, var)
# vars_transformedq/sum(vars_transformedq)
ref_pca_q <- pca_ref_calc_q$pca_components
## Project M4 yearly data
load("data/quarterly/features_M4Q.rda")
M4Q_PCA <- pca_projection(pca_ref_calc_q$prcomp_out, features_M4Q)
## draw PCA plot
pca_all_quarterly <- bind_rows(ref_pca_q, M4Q_PCA,  .id="source")
pcaQ1 <- ggplot(pca_all_quarterly, aes(x=PC1, y=PC2, color=source)) + geom_point(alpha=0.2)+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", NA))+
  geom_point(data = pca_all_quarterly[1:959, ], aes(x=PC1, y=PC2), colour="black", alpha=0.5)+
  theme(aspect.ratio = 1, axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold"))+ggtitle("Quarterly")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 8, face = "bold"))
pcaQ2 <- ggplot(pca_all_quarterly, aes(x=PC1, y=PC2, color=source)) + geom_point(alpha=0.2)+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1"))+theme(aspect.ratio = 1, axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold"))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 8, face = "bold"))

# monthly data
load("data/monthly/features_M4M.rda")
load("data/monthly/monthly_features_training.rda")
## Construct PCA space from simulated, M1 and M3 and then project M4-competition data
pca_ref_calc_m <- calculate_pca(monthly_features_training)
# vars_transformedm <- apply(pca_ref_calc_m$prcomp_out$x, 2, var)
# vars_transformedm/sum(vars_transformedm)
ref_pca_m <- pca_ref_calc_m$pca_components
## Project M4 yearly data
load("data/monthly/features_M4M.rda")
M4M_PCA <- pca_projection(pca_ref_calc_m$prcomp_out, features_M4M)
## draw PCA plot
pca_all_monthly <- bind_rows(ref_pca_m, M4M_PCA,  .id="source")
pcaM1 <- ggplot(pca_all_monthly, aes(x=PC1, y=PC2, color=source)) + geom_point(alpha=0.2)+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", NA))+
  geom_point(data = pca_all_monthly[1:2045, ], aes(x=PC1, y=PC2), colour="black", alpha=0.5)+
  theme(aspect.ratio = 1, axis.text=element_text(size=8))+ggtitle("Monthly")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 8, face = "bold"), axis.title=element_text(size=8,face="bold"))
pcaM2 <- ggplot(pca_all_monthly, aes(x=PC1, y=PC2, color=source)) + geom_point(alpha=0.2)+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1"))+theme(aspect.ratio = 1, axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold"))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 8, face = "bold"))


(pcaY1|pcaQ1|pcaM1)/(pcaY2|pcaQ2|pcaM2)

## ---- pca2
## Weekly data
load("data/weekly/features_M4W.rda")
load("data/weekly/features_weekly_marbased_x.rda")
## Construct PCA space from simulated and then project M4-competition data
pca_ref_calc_w <- calculate_pca(features_weekly_marbased_x)
ref_pca_w <- pca_ref_calc_w$pca_components
# vars_transformedw <- apply(pca_ref_calc_w$prcomp_out$x, 2, var)
# vars_transformedw/sum(vars_transformedw)
## Project M4 yearly data
load("data/weekly/features_M4W.rda")
M4W_PCA <- pca_projection(pca_ref_calc_w$prcomp_out, features_M4W)
## draw PCA plot
pca_all_weekly <- bind_rows(ref_pca_w, M4W_PCA,  .id="source")
pcaW1 <- ggplot(pca_all_weekly, aes(x=PC1, y=PC2, color=source)) + geom_point(alpha=0.2)+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", NA))+theme(aspect.ratio = 1, axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold"))+ggtitle("Weekly")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 8, face = "bold"))
pcaW2 <- ggplot(pca_all_weekly, aes(x=PC1, y=PC2, color=source)) + geom_point(alpha=0.2)+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1"))+theme(aspect.ratio = 1, axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold"))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 8, face = "bold"))

## daily data
load("data/daily/features_M4D.rda")
load("data/daily/features_daily_marbased.rda")
## Construct PCA space from simulated and then project M4-competition data
pca_ref_calc_d <- calculate_pca(features_daily_marbased)
# vars_transformedd <- apply(pca_ref_calc_d$prcomp_out$x, 2, var)
# vars_transformedd/sum(vars_transformedd)
ref_pca_d <- pca_ref_calc_d$pca_components
## Project M4 daily data
load("data/daily/features_M4D.rda")
M4D_PCA <- pca_projection(pca_ref_calc_d$prcomp_out, features_M4D)
## draw PCA plot
pca_all_daily <- bind_rows(ref_pca_d, M4D_PCA,  .id="source")
pcaD1 <- ggplot(pca_all_daily, aes(x=PC1, y=PC2, color=source)) + geom_point(alpha=0.2)+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", NA))+theme(aspect.ratio = 1, axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold"))+ggtitle("Daily")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 8, face = "bold"))
pcaD2 <- ggplot(pca_all_daily, aes(x=PC1, y=PC2, color=source)) + geom_point(alpha=0.2)+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1"))+theme(aspect.ratio = 1, axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold"))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 8, face = "bold"))


## hourly data
load("data/hourly/features_M4H.rda")
load("data/hourly/features_hourly_marbased.rda")
## Construct PCA space from simulated and then project M4-competition data
pca_ref_calc_h <- calculate_pca(features_hourly_marbased)
# vars_transformedh <- apply(pca_ref_calc_h$prcomp_out$x, 2, var)
# vars_transformedh/sum(vars_transformedh)
ref_pca_h <- pca_ref_calc_h$pca_components
## Project M4 hourly data
load("data/hourly/features_M4H.rda")
M4H_PCA <- pca_projection(pca_ref_calc_h$prcomp_out, features_M4H)
## draw PCA plot
pca_all_hourly <- bind_rows(ref_pca_h, M4H_PCA,  .id="source")
pcaH1 <- ggplot(pca_all_hourly, aes(x=PC1, y=PC2, color=source)) + geom_point(alpha=0.2)+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", NA))+theme(aspect.ratio = 1, axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold"))+ggtitle("Hourly")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 8, face = "bold"))
pcaH2 <- ggplot(pca_all_hourly, aes(x=PC1, y=PC2, color=source)) + geom_point(alpha=0.2)+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1"))+theme(aspect.ratio = 1, axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold"))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 8, face = "bold"))

(pcaW1|pcaD1|pcaH1)/(pcaW2|pcaD2|pcaH2)


## ---- corY
source("src/corrplot.R")
load("data/yearly/classlabelM1Y.rda")
clm1y <- classlabelM1Y$accuracy
mase_m1y <- clm1y[seq(1, nrow(clm1y), by = 2), ] 
## M3 competition yearly series
load("data/yearly/classlabelM3Y.rda") # on monash cluster
clm3y <- classlabelM3Y$accuracy
mase_m3y <- clm3y[seq(1, nrow(clm3y), by = 2), ] 
## MAR-based simulated time series
load("data/yearly/yearly_MARaccuracy.rda") # on monash cluster
## preparation of "Y" matrix
m1m3_mase <- rbind(mase_m1y, mase_m3y)
Y <- rbind(m1m3_mase, yearly_MARaccuracy)
Y <- data.frame(Y)
ggpairs(Y, upper = list(continuous = corrplot),axisLabels="none")
#ggpairs(Y,axisLabels="none")

## ---- corQ
## M1 competition quarterly series
load("data/quarterly/classlabelM1Q.rda") # on monash cluster
clm1q <- classlabelM1Q$accuracy
mase_m1q <- clm1q[seq(1, nrow(clm1q), by = 2), ][,-10] # remove mstl as it similar to ets
## M3 competition quarterly series
load("data/quarterly/classlabelM3Q.rda") 
clm3q <- classlabelM3Q$accuracy
mase_m3q <- clm3q[seq(1, nrow(clm3q), by = 2), ][,-10] # remove mstl as it similar to ets
## MAR-based simulated time series
load("data/quarterly/quarterly_MARaccuracy.rda") # on monash cluster
## preparation of "YQ" matrix
m1m3_mase <- rbind(mase_m1q, mase_m3q)
YQ <- rbind(m1m3_mase, quarterly_MARaccuracy)
YQ <- data.frame(YQ)
ggpairs(YQ, upper = list(continuous = corrplot),axisLabels="none")
#ggpairs(YQ, axisLabels="none")

## ---- corM
## Preparation of my data set for training
## M1 competition monthly series
load("data/monthly/classlabelM1M.rda") 
clm1m <- classlabelM1M$accuracy
mase_m1m <- clm1m[seq(1, nrow(clm1m), by = 2), ][,-10] # remove mstl as it similar to ets
## M3 competition monthly series
load("data/monthly/classlabelM3M.rda") 
clm3m <- classlabelM3M$accuracy
mase_m3m <- clm3m[seq(1, nrow(clm3m), by = 2), ][,-10] # remove mstl as it similar to ets
## MAR-based simulated time series
load("data/monthly/monthly_MARaccuracy.rda") 
m1m3_mase <- rbind(mase_m1m, mase_m3m)
YM <- rbind(m1m3_mase, monthly_MARaccuracy)
YM <- data.frame(YM)
ggpairs(YM, upper = list(continuous = corrplot),axisLabels="none")
#ggpairs(YM, axisLabels="none")