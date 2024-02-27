library(ggplot2)
library(readxl)
library(esquisse)
library(dplyr)
library(tidyr)
library(readxl)

col_names <- array(
  read_excel("C:/Users/louis/OneDrive - Université Laval/stage 2022/zetasizer/July15Dummy.xlsx",
             n_max = 1, col_names = F
  ))

df <- read_excel("C:/Users/louis/OneDrive - Université Laval/stage 2022/zetasizer/July15Dummy.xlsx", 
                 col_names = FALSE, skip = 2)

colnames(df) = col_names

#select relevant columns
my_vars = c("Sample Name","Measurement Date and Time", "Z-Ave", 
            "Pk 1 Mean Int", "Pk 1 Area Int","Pk 2 Mean Int",
            "Pk 2 Area Int","Mean Count Rate","Derived Count Rate", 
            "Attenuator", "Measurement Position",
            "PdI", "Intercept", "In Range",
            "Multimodal Fit Error", "Cumulants Fit Error", 
            'Custom Parameter Value')
df_size <- df[my_vars]

df_size <- rename(df_size, 'Turbidity' = 'Custom Parameter Value')
df_size$Turbidity <- replace(df_size$Turbidity,df_size$Turbidity ==0, NA)

#sub spaces and - for _ in column names
names(df_size) <- gsub(" ", "_", names(df_size))
names(df_size) <- gsub("-", "_", names(df_size))

#create a sample_id
df_size <- separate(df_size,
                    Sample_Name,
                    c("ID", "Set", 'extra'),
                    remove = F,
                    extra = 'merge')

df_size[c('ID', 'Set')] <- lapply(df_size[c('ID', 'Set')],factor)
df_size$turb_lvl <- as.factor(df_size$Turbidity)

by_ID <- group_by(df_size, ID)
df_stats <- summarise(by_ID,
                    count = n(),
                    turb_av = mean(Turbidity),
                    turb_sd = sd(Turbidity),
                    Z_Ave_av = mean(Z_Ave),
                    Z_Ave_sd = sd(Z_Ave),
                    PdI_av = mean(PdI),
                    PdI_sd = sd(PdI)
                      )

ggplot(df_size) +
 aes(x = turb_lvl, y = Z_Ave, fill = ID) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 theme_minimal()

ggplot(df_size) +
 aes(x = turb_lvl, y = PdI, fill = ID) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 theme_minimal()

ggplot(df_size) +
 aes(x = turb_lvl, y = Cumulants_Fit_Error, fill = ID) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 theme_minimal()

