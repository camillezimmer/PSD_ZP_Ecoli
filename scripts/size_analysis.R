# Fresh clean up ----

rm(list = ls())


# Loading libraries ----
library(ggpubr)
library(readxl)
library(esquisse)
library(onewaytests)
library(rstatix)
library(forcats)
library(patchwork)
library(tidyverse)

# Loading the data into a dataframe ----

raw_size <- read_excel("data/size_runs.xlsx", col_names = T)


# Data wrangling ----

#making sure only size measurements are selected
raw_size <- filter(raw_size, Type == 'Size')

#sub spaces and - for _ in column names and make all letters lowercase for column names
names(raw_size) <- gsub(" ", "_", names(raw_size))
names(raw_size) <- gsub("-", "_", names(raw_size))
names(raw_size) <- tolower(names(raw_size))

#Bringing relevant columns to the front
raw_size <- select(raw_size, 
        sample_name, turbidity, z_ave, pdi, cumulants_fit_error, intercept, 
        in_range,mean_count_rate, attenuator, derived_count_rate, pk_1_area_int,
        pk_1_mean_int, pk_2_area_int, pk_2_mean_int, multimodal_fit_error,
        measurement_position, everything())

#Renaming the mutaflor capsule runs because of the first 3 runs that were discarded (old capsules)
#there is a warning message that I can't make go away about NAs introduced by coercion, but after checking
#for NAs in the dataframe there are none... the problem comes from the fact that mc10s is not same length as the other mcXs runs
raw_size<- raw_size %>%
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
raw_size <-  raw_size %>%
  separate(sample_name, c('strain_run', 'meas_num'), sep = ' ', remove = F) %>% 
  separate(strain_run, c('strain_id','run_id'), sep = 2, remove = F)
  
#changing some columns into factors
raw_size$turb_lvl <- as.factor(raw_size$turbidity)
raw_size$strain_run <- as.factor(raw_size$strain_run)
raw_size$strain_id <- as.factor(raw_size$strain_id)
raw_size$run_id <- as.factor(raw_size$run_id)
raw_size$meas_num <- as.factor(raw_size$meas_num)


# Data quality check ----
PdI_reject = filter(raw_size, pdi > 0.7)
Intercept_reject <- filter(raw_size, intercept < 0.1 | intercept > 1)
InRange_reject <- filter(raw_size, in_range < 90)
Zavg_reject <- filter(raw_size, pdi > 0.5 | cumulants_fit_error > 0.005)
Multimodal_reject <- filter(raw_size, multimodal_fit_error > 0.005)

#data that fails Cumulants and Multimodal analysis (unusable)
size_reject <- semi_join(Zavg_reject, Multimodal_reject)

#data that can't be used (distinct to remove duplicates)
data_reject <- distinct(rbind(Intercept_reject, InRange_reject, PdI_reject, size_reject))

#Data that can be analyzed with Cumulants analysis (Z-Ave)
cumulants_raw <- anti_join(raw_size, rbind(data_reject, Zavg_reject))

#data that needs to be interpreted with peak size results
#this is data for which Z-Ave is not suitable (bad PdI or Cumulants fit error), 
#but with a multimodal fit error low enough to make it suitable for peak size.
peak_size <- anti_join(
  anti_join(Zavg_reject, Multimodal_reject),
  rbind(Intercept_reject, InRange_reject))

  
# Identifying and deleting Z-Ave measurements that are outliers ----


#outlier remover function, outliers defined as over 1.5*IQR
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

#making a dataframe of the z_ave outliers per run
cumulants_outliers <- cumulants_raw %>% 
  group_by(strain_run) %>%
  mutate(z_ave = remove_outliers(z_ave)) %>% 
  ungroup() %>% 
  filter(is.na(z_ave))

#making a dataframe where the z_ave outliers have been removed
#could do an anti_join with cumulants_ouliers instead, but decided to go for
#the code below to make it more robust since the imported columns may vary, and
#the key for the anti_join might not be in the dataframes.
cumulants_cleaned <- cumulants_raw %>% 
  group_by(strain_run) %>%
  mutate(z_ave = remove_outliers(z_ave)) %>% 
  ungroup() %>% 
  filter(!is.na(z_ave))


# Data calculations for further analysis ----

#choose below if the wanted data for analysis contains outliers or not
#for outliers: choice <-  'raw', no outliers: choice <- 'cleaned'

choice <- 'cleaned'
list_df = list('raw' = cumulants_raw, 'cleaned' = cumulants_cleaned)


means_z_strain <- list_df[[choice]] %>% 
  group_by(strain_id) %>% 
  summarise(mean_z = mean(z_ave))

means_z_strainrun <- list_df[[choice]] %>% 
  group_by(strain_run) %>% 
  summarise(mean_z = mean(z_ave))


#summary of cumulants data by run (strain_run)
summary_run <- list_df[[choice]] %>% 
  group_by(strain_run) %>% 
  summarise(across(c(z_ave, pdi, cumulants_fit_error, intercept, in_range, mean_count_rate),
                   list(mean = mean, sd = sd, min = min, max = max)),
            turbidity_mean = mean(turbidity),
            count = n()
            ) %>% 
  mutate(CI95 = 1.96*z_ave_sd,
         z_ave_95up = z_ave_mean + 1.96*z_ave_sd,
         z_ave_95down = z_ave_mean - 1.96*z_ave_sd)


# summary by strain_id
summary_strain <- list_df[[choice]] %>% 
  group_by(strain_id) %>% 
  summarise(across(c(z_ave, pdi, cumulants_fit_error, intercept, in_range, mean_count_rate),
                   list(mean = mean, sd = sd, min = min, max = max)),
            turbidity_mean = mean(turbidity),
            count = n()
  ) %>% 
  mutate(CI95 = 1.96*z_ave_sd,
         z_ave_95up = z_ave_mean + 1.96*z_ave_sd,
         z_ave_95down = z_ave_mean - 1.96*z_ave_sd)


# Plots ----

#boxplot of the z_ave measurements per run before removing outliers
ggplot(cumulants_raw) +
  aes(x = strain_run, y = z_ave, fill = strain_id, colour = strain_id) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_hue(direction = 1) +
  theme_bw()+
  facet_wrap(~strain_id, ncol=3, scales = 'free_x')+
  stat_summary(fun = mean, geom = 'point', size=4, show.legend = F)

#boxplot of the z_ave measurements per run after removing outliers to make sure it worked
ggplot(cumulants_cleaned) +
  aes(x = strain_run, y = z_ave, fill = strain_id, colour = strain_id) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_hue(direction = 1) +
  theme_bw()+
  facet_wrap(~strain_id, ncol=3, scales = 'free_x')+
  stat_summary(fun = mean, geom = 'point', size=4, show.legend = F)

#Boxplot of z_ave per run with fill = turbidity
#this will be the same plot as one of the two previous depending on the choice made for data selection
#the only difference is tha turbidity is assigned to fill to see if it has any effect on the results
ggplot(list_df[[choice]]) +
  aes(x = strain_run, y = z_ave, fill = turbidity) +
  geom_boxplot(alpha = 0.5)+
  scale_fill_distiller(palette = "Blues", 
                       direction = 1) +
  theme_bw() +
  facet_wrap(vars(strain_id), scales = "free_x")+
  stat_summary(fun = mean, geom = 'point', size=4)

#Boxplot of PdI per run with fill = turbidity
ggplot(list_df[[choice]]) +
  aes(x = strain_run, y = pdi, fill = turbidity) +
  geom_boxplot(alpha = 0.5)+
  scale_fill_distiller(palette = "Blues", 
                       direction = 1) +
  theme_bw() +
  facet_wrap(vars(strain_id), scales = "free_x")+
  stat_summary(fun = mean, geom = 'point', size=4)

#Bar chart of mean z_ave per run with error bars, fill = turbidity
ggplot(summary_run) +
 aes(x = strain_run, y = z_ave_mean, fill = turbidity_mean) +
 geom_col() +
 scale_fill_distiller(palette = "Blues", 
                      direction = 1) +
  geom_errorbar( aes(x= strain_run, ymin=z_ave_95down, ymax=z_ave_95up), width=0.2, alpha=0.8, size=0.75)+
  theme_bw()+
  facet_wrap(~substr(strain_run, 1,2), ncol=3, scales = 'free_x')

#Bar chart of mean z_ave per strain with error bars, fill = mean turbidity
ggplot(summary_strain) +
  aes(x = strain_id, y = z_ave_mean, fill = turbidity_mean) +
  geom_col() +
  scale_fill_distiller(palette = "Blues", 
                       direction = 1) +
  geom_errorbar( aes(x= strain_id, ymin=z_ave_95down, ymax=z_ave_95up), width=0.1, alpha=0.5, size=0.5)+
  geom_text(aes(label = round(z_ave_mean,2)),fontface = 'bold', size = 12 / .pt, vjust =0, hjust= 1.25)+
  theme_minimal()

#Boxplot and density plot of z_ave per run
bp_run <- ggplot(list_df[[choice]])+
  aes(x = strain_run, y = z_ave, colour = run_id, fill = run_id)+
  geom_boxplot(alpha = 0.5)+
  theme_bw()+
  labs(y = 'Z-Ave size [nm]', x = element_blank())+
  ggtitle('Boxplots and means')+
  theme(plot.title = element_text(hjust = 0.5, colour = 'steelblue'),legend.position = "none")+
  coord_flip()+
  facet_wrap(~strain_id, nrow=3, scales = 'free_y')+
  stat_summary(fun = mean, geom = 'point', size=4)


dp_run <- ggplot(list_df[[choice]])+
  aes(x = z_ave, fill = run_id, colour = run_id)+
  geom_density(adjust = 1L, alpha = 0.5)+
  theme_bw()+
  labs(x = 'Z-Ave size [nm]', y = element_blank())+
  ggtitle('Density plots and means')+
  theme(plot.title = element_text(hjust = 0.5, colour = 'steelblue'))+
  geom_vline(data = means_z_strainrun, aes(xintercept=mean_z, colour = substr(strain_run, 3,4)),linetype = 'dashed', size = 0.75, show.legend = F)+
  facet_wrap(~substr(strain_run, 1,2), nrow=3, scales = 'free_y')     
    
bp_run+dp_run+plot_annotation(
  title = 'Boxplots and density plots of Z-Ave size by run',
  theme = theme(plot.title = element_text(face = 'bold', hjust = 0.5))
)

#Boxplot and density plot of z_ave per strain
bp_strain <- ggplot(list_df[[choice]])+
  aes(x = strain_id, y = z_ave, colour = strain_id)+
  geom_boxplot(aes(fill = strain_id), alpha = 0.5)+
  theme_bw()+
  labs(y = 'Z-Ave size [nm]', x = element_blank())+
  ggtitle('Boxplots and means')+
  theme(plot.title = element_text(hjust = 0.5, colour = 'steelblue'),legend.position = "none")+
  coord_flip()+
  theme(legend.position = "none")+
  facet_wrap(~strain_id, nrow=3, scales = 'free_y')+
  stat_summary(fun = mean, geom = 'point', size=4)


dp_strain <- ggplot(list_df[[choice]])+
  aes(x = z_ave, fill = strain_id)+
  geom_density(aes(colour = strain_id), alpha = 0.5, adjust = 1L)+
  theme_bw()+
  labs(x = 'Z-Ave size [nm]', y = element_blank())+
  ggtitle('Density plots and means')+
  theme(plot.title = element_text(hjust = 0.5, colour = 'steelblue'))+
  facet_wrap(~strain_id, nrow=3, scales = 'free_y')+
  geom_vline(data = means_z_strain, aes(xintercept=mean_z, colour = strain_id), linetype = 'dashed', size = 0.75, show.legend = F)

bp_strain+dp_strain+plot_annotation(
  title = 'Boxplots and density plots of Z-Ave size by strain',
  theme = theme(plot.title = element_text(face = 'bold', hjust = 0.5))
)


# Statistical analysis ----

#chosen alpha value: 5%

#Assessing the normality distribution assumption

#sample size
list_df[[choice]] %>% filter(!is.na(z_ave)) %>% group_by(strain_id) %>% count()
#29 values for ka, 31 for ma and 32 for mc (outliers removed)

#Shapiro-Wilk normality test and Q-Q plots
nor.test(z_ave ~ strain_id, data = list_df[[choice]])

#ka and mc pass normality test, but ma fails (p-value = 0.024 on the shapiro test).

#Assessing homogeneity of variance, using the Fligner test because distribution is not normal for all groups
homog.test(z_ave ~ strain_id, data = list_df[[choice]], method = "Fligner")

#variances are not homogeneous across groups (p-value of 0.022)

#z_ave^2 transformation works to make all data fit normal distribution,
df_trans <- list_df[[choice]] %>% 
  mutate(z_ave_trans = z_ave^2)

nor.test(z_ave_trans ~ strain_id, data = df_trans)

#but variances are still not homogeneous. What to do?
homog.test(z_ave_trans ~ strain_id, data = df_trans, method = "Bartlett")

#The total sample size is over 30 (29+31+32 = 92) and the group sizes are roughly equal: 32/29 = 1.1 < 1.5
#I think we can still run ANOVA since it is quite robust to violations of homoscedasticity in this case?
#I have also read that Bartlett test can be too sensitive... to confirm
#good read: https://www.statisticssolutions.com/the-assumption-of-homogeneity-of-variance/
#Maybe we should do a Welch's ANOVA instead??
#https://www.statisticshowto.com/welchs-anova/
#https://statisticsbyjim.com/anova/welchs-anova-compared-to-classic-one-way-anova/
#Welch's Anova seems to be the best option and it is easy to implement so I don't see why we should do standard ANOVA instead

oneway.test(z_ave_trans ~ strain_id +run_id, data = df_trans , var.equal = FALSE)
#p-value very small -> reject the Null; The Z_ave^2 differs for at least one strain

#post-hoc test for Welch's ANOVA : Games-Howell
#can we block??? does not seem to work with this formula... what should we do?
games_howell_test(df_trans, z_ave_trans ~ strain_id, detailed = T)
#ka-mc do not differ significantly (p_value of 0.096), ka-ma and ma-mc do (very small p-values)
#ka-ma mean diff = 488 374 -> sqrt() = 698nm
#ka-mc mean diff = 78 353 -> sqrt() = 280nm 
#ma-mc mean diff = 410 021 -> sqrt() = 640nm
#do we back transform like this??

#trying ANOVA instead with the transformation
summary(aov(z_ave_trans ~ strain_id +run_id, data = df_trans))
TukeyHSD(aov(z_ave_trans ~ strain_id +run_id, data = df_trans))
#exact same results almost
