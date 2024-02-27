library(ggplot2)
library(readxl)
library(esquisse)
library(dplyr)
library(tidyr)

col_names <- array(
  read_excel("C:/Users/louis/OneDrive - Université Laval/stage 2022/zetasizer/data analysis/July20Dummy.xlsx",
             n_max = 1, col_names = F
  ))

df20 <- read_excel("C:/Users/louis/OneDrive - Université Laval/stage 2022/zetasizer/data analysis/July20Dummy.xlsx", 
                 col_names = FALSE, skip = 2)

colnames(df20) = col_names

#select relevant columns
my_vars = c("Sample Name","Measurement Date and Time", "Z-Ave", 
            "Pk 1 Mean Int", "Pk 1 Area Int","Pk 2 Mean Int",
            "Pk 2 Area Int","Mean Count Rate","Derived Count Rate", 
            "Attenuator", "Measurement Position",
            "PdI", "Intercept", "In Range",
            "Multimodal Fit Error", "Cumulants Fit Error", 
            'Custom Parameter Value')
df20_size <- df20[my_vars]

df20_size <- rename(df20_size, 'Turbidity' = 'Custom Parameter Value')
df20_size$Turbidity <- df20_size$Turbidity / 10 

#sub spaces and - for _ in column names
names(df20_size) <- gsub(" ", "_", names(df20_size))
names(df20_size) <- gsub("-", "_", names(df20_size))

#create a sample_id
df20_size <- separate(df20_size,
                    Sample_Name,
                    c("ID", "extra"),
                    remove = F,
                    extra = 'merge')

df20_size[c('strain')] <- lapply(df20_size[c('ID')],factor)
df20_size$turb_lvl <- as.factor(df20_size$Turbidity)

by_ID <- group_by(df20_size, ID)
df_stats <- summarise(by_ID,
                      count = n(),
                      turb_av = mean(Turbidity),
                      turb_sd = sd(Turbidity),
                      Z_Ave_av = mean(Z_Ave),
                      Z_Ave_sd = sd(Z_Ave),
                      PdI_av = mean(PdI),
                      PdI_sd = sd(PdI)
)


ggplot(df20_size) +
 aes(x = strain, y = Cumulants_Fit_Error, fill = turb_lvl) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 theme_minimal()

ggplot(df20_size) +
  aes(x = strain, y = PdI, fill = turb_lvl) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

ggplot(df20_size) +
  aes(x = strain, y = Z_Ave, fill = turb_lvl) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal()
