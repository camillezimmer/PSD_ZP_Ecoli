# Fresh clean up ----

rm(list = ls())


# Loading libraries ----
library(ggpubr)
library(readxl)
library(esquisse)
library(forcats)
library(onewaytests)
library(rstatix)
library(patchwork)
library(tidyverse)

# Loading the data into a dataframe ----

raw_zeta <- read_excel("data/zeta_runs.xlsx", col_names = T)


# Data wrangling ----

#making sure only zeta measurements are selected
#this mean the column Type must be selected as a record in Malvern's software
raw_zeta <- filter(raw_zeta, Type == 'Zeta')

#sub spaces and - for _ in column names and make all letters lowercase for column names
names(raw_zeta) <- gsub(" ", "_", names(raw_zeta))
names(raw_zeta) <- gsub("-", "_", names(raw_zeta))
names(raw_zeta) <- tolower(names(raw_zeta))

#Bringing relevant columns to the front
raw_zeta <- select(raw_zeta, 
                   sample_name, turbidity, zp, zeta_deviation, cond, 
                   mean_count_rate, attenuator, derived_count_rate,
                   quality_factor, spectral_quality_factor, zeta_runs,
                   everything())

#Renaming the mutaflor capsule runs that I redid from 1 to 3 instead of 4 to 6
raw_zeta<- raw_zeta %>% 
  mutate(sample_name = 
           ifelse(substr(sample_name, 1, 2) != 'mc',
                  sample_name,
                  ifelse(substr(sample_name, 1, 4) == 'mc10',
                         paste0(substr(sample_name,1,2),
                                as.character(as.numeric(substr(sample_name, 3,4))-3),
                                substr(sample_name, 5,7)),
                         paste0(substr(sample_name,1,2),
                                as.character(as.numeric(substr(sample_name, 3,3))-3),
                                substr(sample_name, 4,6)))))

  

#creating useful variables from the sample_name
raw_zeta <-  raw_zeta %>%
  separate(sample_name, c('strain_run', 'meas_num'), sep = ' ', remove = F) %>% 
  separate(strain_run, c('strain_id','run_id'), sep = 2, remove = F)

#changing some columns into factors
raw_zeta$turb_lvl <- as.factor(raw_zeta$turbidity)
raw_zeta$strain_run <- as.factor(raw_zeta$strain_run)
raw_zeta$strain_id <- as.factor(raw_zeta$strain_id)
raw_zeta$run_id <- as.factor(raw_zeta$run_id)
raw_zeta$meas_num <- as.factor(raw_zeta$meas_num)

# Data quality check ----

#data does not meet quality criteria if quality factor is under 1
QF_reject = filter(raw_zeta, quality_factor < 1)

#data does not meet quality criteria if spectral quality factor is under 1
#however, on automatic mode, the zetasizer will stop the measurement when 1 is
#obtained. Therefore it is only a treshold value. It happens that it is not 
#exactly 1 (e.g. 0,999), this is why signif is used.
SQF_reject = filter(raw_zeta, signif(spectral_quality_factor,1) < 1)

#data that can't be used (distinct to remove duplicates)
data_reject <- distinct(rbind(QF_reject, SQF_reject))

#Data that pass the quality tests (we can analyze the zp)
zp_raw <- anti_join(raw_zeta, data_reject)


# Identifying and deleting Z-Ave measurements that are outliers ----


#outlier remover function, outlier definition used is over 1.5*IQR
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

#making a dataframe of the zp outliers per run
zp_outliers <- zp_raw %>% 
  group_by(strain_run) %>%
  mutate(zp = remove_outliers(zp)) %>% 
  ungroup() %>% 
  filter(is.na(zp))

#making a dataframe where the z_ave outliers have been removed
#could do an anti_join with zp_ouliers instead, but decided to opt for
#the code below to make it more robust since the imported columns may vary, and
#the key for the anti_join might not be in the dataframes.
zp_cleaned <- zp_raw %>% 
  group_by(strain_run) %>%
  mutate(zp = remove_outliers(zp)) %>% 
  ungroup() %>% 
  filter(!is.na(zp))


# Data calculations for further analysis ----

#choose below if the wanted data for analysis contains outliers or not
#for outliers: choice <-  'raw', no outliers: choice <- 'cleaned'

choice <- 'cleaned'
list_df = list('raw' = zp_raw, 'cleaned' = zp_cleaned)

#Average zeta potential for each strain, this will be useful for plotting later
means_zp_strain <- list_df[[choice]] %>% 
  group_by(strain_id) %>% 
  summarise(mean_zp = mean(zp))

#Average zeta potential for each run, this will be useful for plotting later
means_zp_strainrun <- list_df[[choice]] %>% 
  group_by(strain_run) %>% 
  summarise(mean_zp = mean(zp))


#summary of zp data by run (strain_run)
summary_run <- list_df[[choice]] %>% 
  group_by(strain_run) %>% 
  summarise(across(c(zp, zeta_deviation, cond, quality_factor, mean_count_rate),
                   list(mean = mean, sd = sd, min = min, max = max)),
            turbidity_mean = mean(turbidity),
            count = n()
  ) %>% 
  mutate(CI95 = 1.96*zp_sd,
         zp_95up = zp_mean + 1.96*zp_sd,
         zp_95down = zp_mean - 1.96*zp_sd)


# summary by strain_id
summary_strain <- list_df[[choice]] %>% 
  group_by(strain_id) %>% 
  summarise(across(c(zp, zeta_deviation, cond, quality_factor, mean_count_rate),
                   list(mean = mean, sd = sd, min = min, max = max)),
            turbidity_mean = mean(turbidity),
            count = n()
  ) %>% 
  mutate(CI95 = 1.96*zp_sd,
         zp_95up = zp_mean + 1.96*zp_sd,
         zp_95down = zp_mean - 1.96*zp_sd)

# Plots ----

#boxplot of the zp measurements per run before removing outliers
ggplot(zp_raw) +
  aes(x = strain_run, y = zp, fill = strain_id, colour = strain_id) +
  geom_boxplot(alpha = 0.5) +
  scale_y_reverse()+
  scale_fill_hue(direction = 1) +
  theme_bw()+
  facet_wrap(~strain_id, ncol=3, scales = 'free_x')+
  stat_summary(fun = mean, geom = 'point', size=4, show.legend = F)

#boxplot of the zp measurements per run after removing outliers to make sure the removal worked
ggplot(zp_cleaned) +
  aes(x = strain_run, y = zp, fill = strain_id, colour = strain_id) +
  geom_boxplot(alpha = 0.5) +
  scale_y_reverse()+
  scale_fill_hue(direction = 1) +
  theme_bw()+
  facet_wrap(~strain_id, ncol=3, scales = 'free_x')+
  stat_summary(fun = mean, geom = 'point', size=4, show.legend = F)

#Boxplot of zp per run with fill = turbidity
#this will be the same plot as one of the two previous depending on the choice made for data selection
#the only difference is tha turbidity is assigned to fill to see if it has any effect on the results
ggplot(list_df[[choice]]) +
  aes(x = strain_run, y = zp, fill = turbidity) +
  geom_boxplot(alpha = 0.5)+
  scale_y_reverse()+
  scale_fill_distiller(palette = "Blues", 
                       direction = 1) +
  theme_bw() +
  facet_wrap(vars(strain_id), scales = "free_x")+
  stat_summary(fun = mean, geom = 'point', size=4)

#Boxplot of zeta_deviation / abs(zp) per run with fill = turbidity (coef. of variation)
#zeta_deviation is the standard deviation of the mean zeta potential obtained during a measurement
ggplot(list_df[[choice]]) +
  aes(x = strain_run, y = zeta_deviation/abs(zp), fill = turbidity) +
  geom_boxplot(alpha = 0.5)+
  scale_fill_distiller(palette = "Blues", 
                       direction = 1) +
  theme_bw() +
  facet_wrap(vars(strain_id), scales = "free_x")+
  stat_summary(fun = mean, geom = 'point', size=4)

#Boxplot of quality factor per run with fill = turbidity
#The higher the value is for the quality factor, the better. Min = 1
ggplot(list_df[[choice]]) +
  aes(x = strain_run, y = quality_factor, fill = turbidity) +
  geom_boxplot(alpha = 0.5)+
  scale_fill_distiller(palette = "Blues", 
                       direction = 1) +
  theme_bw() +
  facet_wrap(vars(strain_id), scales = "free_x")+
  stat_summary(fun = mean, geom = 'point', size=4)

#Bar chart of mean z_ave per run with error bars, fill = turbidity
ggplot(summary_run) +
  aes(x = strain_run, y = zp_mean, fill = turbidity_mean) +
  geom_col() +
  scale_y_reverse()+
  scale_fill_distiller(palette = "Blues", 
                       direction = 1) +
  geom_errorbar( aes(x= strain_run, ymin=zp_95down, ymax=zp_95up), width=0.2, alpha=0.8, size=0.75)+
  theme_bw()+
  facet_wrap(~substr(strain_run, 1,2), ncol=3, scales = 'free_x')

#Bar chart of mean zp per strain with error bars, fill = mean turbidity
ggplot(summary_strain) +
  aes(x = strain_id, y = zp_mean, fill = turbidity_mean) +
  geom_col() +
  scale_y_reverse()+
  scale_fill_distiller(palette = "Blues", 
                       direction = 1) +
  geom_errorbar( aes(x= strain_id, ymin=zp_95down, ymax=zp_95up), width=0.1, alpha=0.5, size=0.5)+
  geom_text(aes(label = round(zp_mean,2)),fontface = 'bold', size = 12 / .pt, vjust =0, hjust= 1.25)+
  theme_minimal()

#Boxplot and density plot of zp per run
bp_run <- ggplot(list_df[[choice]])+
  aes(x = strain_run, y = zp, colour = run_id, fill = run_id)+
  geom_boxplot(alpha = 0.5)+
  scale_y_reverse()+
  theme_bw()+
  labs(y = 'Zeta potential [mV]', x = element_blank())+
  ggtitle('Boxplots and means')+
  theme(plot.title = element_text(hjust = 0.5, colour = 'steelblue'),legend.position = "none")+
  coord_flip()+
  facet_wrap(~strain_id, nrow=3, scales = 'free_y')+
  stat_summary(fun = mean, geom = 'point', size=4)


dp_run <- ggplot(list_df[[choice]])+
  aes(x = zp, fill = run_id, colour = run_id)+
  geom_density(adjust = 1L, alpha = 0.5)+
  theme_bw()+
  labs(x = 'Zeta potential [mV]', y = element_blank())+
  ggtitle('Density plots and means')+
  theme(plot.title = element_text(hjust = 0.5, colour = 'steelblue'))+
  geom_vline(data = means_zp_strainrun, aes(xintercept=mean_zp, colour = substr(strain_run, 3,4)),linetype = 'dashed', size = 0.75, show.legend = F)+
  facet_wrap(~substr(strain_run, 1,2), nrow=3, scales = 'free_y')+
  scale_x_reverse()

bp_run+dp_run+plot_annotation(
  title = 'Boxplots and density plots of zeta potential by run',
  theme = theme(plot.title = element_text(face = 'bold', hjust = 0.5))
)

#Boxplot and density plot of zp per strain
bp_strain <- ggplot(list_df[[choice]])+
  aes(x = strain_id, y = zp, colour = strain_id, fill = strain_id)+
  geom_boxplot(alpha = 0.5)+
  scale_y_reverse()+
  theme_bw()+
  labs(y = 'Zeta potential [mV]', x = element_blank())+
  ggtitle('Boxplots and means')+
  theme(plot.title = element_text(hjust = 0.5, colour = 'steelblue'),legend.position = "none")+
  coord_flip()+
  facet_wrap(~strain_id, nrow=3, scales = 'free_y')+
  stat_summary(fun = mean, geom = 'point', size=4)


dp_strain <- ggplot(list_df[[choice]])+
  aes(x = zp, fill = strain_id)+
  geom_density(adjust = 1L, alpha = 0.5, aes(colour = strain_id))+
  theme_bw()+
  scale_x_reverse()+
  labs(x = 'Zeta potential [mV]', y = element_blank())+
  ggtitle('Density plots and means')+
  theme(plot.title = element_text(hjust = 0.5, colour = 'steelblue'))+
  facet_wrap(~strain_id, nrow=3, scales = 'free_y')+
  geom_vline(data = means_zp_strain, aes(xintercept=mean_zp, colour = strain_id), linetype = 'dashed', size = 0.75, show.legend = F)

bp_strain+dp_strain+plot_annotation(
  title = 'Boxplots and density plots of zeta potential by strain',
  theme = theme(plot.title = element_text(face = 'bold', hjust = 0.5))
)


# Statistical analysis ----

#chosen alpha value: 5%

#Assessing the normality distribution assumption

#sample size
list_df[[choice]] %>% filter(!is.na(zp)) %>% group_by(strain_id) %>% count()
#30 values for ka, 32 for ma and 29 for mc (outliers removed)

#Shapiro-Wilk normality test and Q-Q plots
nor.test(zp ~ strain_id, data = list_df[[choice]])

#ka and mc do not pass Shapiro normality test, ma does.


#Assessing homogeneity of variance, using the Fligner test because distribution is not normal for all groups
homog.test(zp ~ strain_id, data = list_df[[choice]], method = "Fligner")
#variances are not homogeneous across groups (p-value of 0.00021)

#I can't find a transformation to make all groups normal (tried box-cox also)

#Kruskal-Wallis test + things to keep in mind
#can't confirm the null, just reject?
#if the distributions are not similar between groups, it is only a test of dominance?
#if the distributions between groups are similar = test of medians, not means?
#can be regarded as difference of means if distribution similar and symetrical? 
#can we block??

kruskal.test(zp ~ strain_id, data = list_df[[choice]])
#p-value = very small -> reject the Null

#Dunn's post-hoc test
dunn_test(data = list_df[[choice]], zp ~ strain_id, p.adjust.method = "holm", detailed = T)
#would indicate no difference between ma and mc, but differences for all other comparisons
#values are difference in mean ranks... not mean ZP


#dummy tests ---- 

#ANOVA comparing the ZP of the three scenarios (strain_id)
#blocking on run_id to reduce error due to variation between runs
list_df[[choice]] %>% 
  aov(zp ~ strain_id + run_id, data = .) %>% 
  summary()
#very small p-value; Null rejected -> The ZP mean differs for at least one strain

#post-hoc Tukey test
list_df[[choice]] %>% 
  aov(zp ~ strain_id + run_id, data = .) %>% 
  TukeyHSD()
#The ZP of Mutaflor does not seem to be affected by the preparation (avg_zp_capsule similar to avg_zp_agar, p_value = 0.19)
#The ZP of K12 is statistically different than both Mutaflor preparations
#difference in means ma-ka = 3,19, 95CI = [1,82, 4,56]
#difference in means mc-ka = 2,17, 95CI = [0,76, 3,58]
#difference in means mc-ma = -1,02, 95CI = [-2,41, 0,37]


#Bootstrap test ----

#We know from kruskal test that there is a difference across groups
#We would like to quanitfy that difference, so we can bootstrap and make a confidence interval for each strain
#We do not use bootstrap for hypothesis testing here, since we already did that with kruskal. We want to quantify the 
#difference in mean ZP across groups. Therefore, we do not mix strains/groups when sampling! 


set.seed(13579)   # set a seed for consistency/reproducability
n_ka <- sum(zp_cleaned$strain_id=='ka') # the number of observations to sample from ka 
n_ma <- sum(zp_cleaned$strain_id=='ma')  # the number of observations to sample from ma
n_mc <- sum(zp_cleaned$strain_id=='mc')  # the number of observations to sample from mc
B <- 100000  # the number of bootstrap samples

Boot_ka <- matrix( sample(zp_cleaned$zp[zp_cleaned$strain_id=="ka"], size= B*n_ka, 
                              replace=TRUE), ncol=B, nrow=n_ka)
Boot_ma <- matrix( sample(zp_cleaned$zp[zp_cleaned$strain_id=="ma"], size= B*n_ma, 
                          replace=TRUE), ncol=B, nrow=n_ma)
Boot_mc <- matrix( sample(zp_cleaned$zp[zp_cleaned$strain_id=="mc"], size= B*n_mc, 
                          replace=TRUE), ncol=B, nrow=n_mc) 

#check dim to be sure all is good
dim(Boot_ka); dim(Boot_ma); dim(Boot_mc)
Boot_ka[1:10, 1:10]
Boot_ma[1:10, 1:10]
Boot_mc[1:10, 1:10]

# calculate the difference in MEANS for each of the bootsamples for ka-ma
Boot_Diff_Means_KaMa <- colMeans(Boot_ka) - colMeans(Boot_ma)
# check that it worked
length(Boot_Diff_Means_KaMa)
# look at the first 10 diff in means
Boot_Diff_Means_KaMa[1:10]

# calculate the difference in MEANS for each of the bootsamples for ka-mc
Boot_Diff_Means_KaMc <- colMeans(Boot_ka) - colMeans(Boot_mc)
# check that it worked
length(Boot_Diff_Means_KaMc)
# look at the first 10 diff in means
Boot_Diff_Means_KaMc[1:10]

# calculate the difference in MEANS for each of the bootsamples for ma-mc
Boot_Diff_Means_MaMc <- colMeans(Boot_ma) - colMeans(Boot_mc)
# check that it worked
length(Boot_Diff_Means_MaMc)
# look at the first 10 diff in means
Boot_Diff_Means_MaMc[1:10]

# Making the 95CI using the percentile method

#ka-ma
KaMa_low <- quantile(Boot_Diff_Means_KaMa, prob=0.025)
KaMa_up <- quantile(Boot_Diff_Means_KaMa, prob=0.975)
KaMa_mean <- mean(Boot_Diff_Means_KaMa)

#ka-mc
KaMc_low <- quantile(Boot_Diff_Means_KaMc, prob=0.025)
KaMc_up <- quantile(Boot_Diff_Means_KaMc, prob=0.975)
KaMc_mean <- mean(Boot_Diff_Means_KaMc)

#ma-mc
MaMc_low <- quantile(Boot_Diff_Means_MaMc, prob=0.025)
MaMc_up <- quantile(Boot_Diff_Means_MaMc, prob=0.975)
MaMc_mean <- mean(Boot_Diff_Means_MaMc)

#Plotting the Bootstraps ----

#making a dataframe
Boot_diffMeans_df <- data.frame(KaMa = Boot_Diff_Means_KaMa, KaMc = Boot_Diff_Means_KaMc, MaMc = Boot_Diff_Means_MaMc)


boot_plot_KaMa <- Boot_diffMeans_df %>%  
  ggplot(aes(x = KaMa)) +
  geom_histogram(aes(y = ..density..),
                 color = "gray", fill = "white") +
  geom_density(fill = "black", alpha = 0.2)+
  theme_bw()+
  theme(legend.spacing.y = unit(1.0, 'cm'), plot.title = element_text(hjust = 0.5), legend.position = "none")+
  guides(colour = guide_legend(byrow = TRUE))+
  geom_vline(aes(xintercept= KaMa_mean, colour = 'mean'), linetype = 'dashed', size = 0.75, show.legend = T)+
  geom_vline(aes(xintercept= KaMa_low, colour = '95CI'), linetype = 'dashed', size = 0.75, show.legend = T)+
  geom_vline(aes(xintercept= KaMa_up, colour = '95CI'), linetype = 'dashed', size = 0.75, show.legend = T)+
  scale_x_continuous(breaks = seq(round(min(Boot_Diff_Means_KaMa), 0), round(max(Boot_Diff_Means_KaMa),0), 1))+
  ggtitle('Ka-Ma')

boot_plot_KaMc <- Boot_diffMeans_df %>%  
  ggplot(aes(x = KaMc)) +
  geom_histogram(aes(y = ..density..),
                 color = "gray", fill = "white") +
  geom_density(fill = "black", alpha = 0.2)+
  theme_bw()+
  theme(legend.spacing.y = unit(1.0, 'cm'), plot.title = element_text(hjust = 0.5), legend.position = "none")+
  guides(colour = guide_legend(byrow = TRUE))+
  geom_vline(aes(xintercept= KaMc_mean, colour = 'mean'), linetype = 'dashed', size = 0.75, show.legend = T)+
  geom_vline(aes(xintercept= KaMc_low, colour = '95CI'), linetype = 'dashed', size = 0.75, show.legend = T)+
  geom_vline(aes(xintercept= KaMc_up, colour = '95CI'), linetype = 'dashed', size = 0.75, show.legend = T)+
  scale_x_continuous(breaks = seq(round(min(Boot_Diff_Means_KaMc), 0), round(max(Boot_Diff_Means_KaMc),0), 1))+
  ggtitle('Ka-Mc')

boot_plot_MaMc <- Boot_diffMeans_df %>%  
  ggplot(aes(x = MaMc)) +
  geom_histogram(aes(y = ..density..),
                 color = "gray", fill = "white") +
  geom_density(fill = "black", alpha = 0.2)+
  theme_bw()+
  theme(legend.spacing.y = unit(1.0, 'cm'), plot.title = element_text(hjust = 0.5))+
  guides(colour = guide_legend(byrow = TRUE))+
  geom_vline(aes(xintercept= MaMc_mean, colour = 'mean'), linetype = 'dashed', size = 0.75, show.legend = T)+
  geom_vline(aes(xintercept= MaMc_low, colour = '95CI'), linetype = 'dashed', size = 0.75, show.legend = T)+
  geom_vline(aes(xintercept= MaMc_up, colour = '95CI'), linetype = 'dashed', size = 0.75, show.legend = T)+
  scale_x_continuous(breaks = seq(round(min(Boot_Diff_Means_MaMc), 0), round(max(Boot_Diff_Means_MaMc),0), 1))+
  ggtitle('Ma-Mc')

boot_plot_KaMa +boot_plot_KaMc+boot_plot_MaMc + 
  plot_annotation(
  title = 'Distribution of the difference in means and 95% confidence interval for the bootstrapped samples',
  theme = theme(plot.title = element_text(face = 'bold', hjust = 0.5))
)

