library(ggplot2)
library(readxl)
library(esquisse)
library(dplyr)
library(tidyr)
library(readxl)


col_names <- array(
  read_excel("C:/Users/louis/OneDrive - Université Laval/stage 2022/zetasizer/July14Dummy.xlsx",
             n_max = 1, col_names = F
  ))

df <- read_excel("C:/Users/louis/OneDrive - Université Laval/stage 2022/zetasizer/July14Dummy.xlsx", 
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

df_size <- mutate(df_size, Turbidity = ifelse(Turbidity >= 100, 
                                      (Turbidity %% 10)/10 + Turbidity %/% 10, 
                                      Turbidity))

df_size$Turbidity <- replace(df_size$Turbidity,df_size$Turbidity ==0, NA)
df_size <- mutate(df_size, turb_approx = ifelse(is.na(Turbidity),88*2,Turbidity))

#sub spaces and - for _ in column names
names(df_size) <- gsub(" ", "_", names(df_size))
names(df_size) <- gsub("-", "_", names(df_size))

#create a sample_id
df_size <- separate(df_size,
               Sample_Name,
               c("ID", "Set"),
               remove = F,
               extra = "drop")

df_size[c('ID', 'Set')] <- lapply(df_size[c('ID', 'Set')],factor)
df_size$ID <- factor(df_size$ID, c('MP50', 'MP100', 'MP200', "MP400", 'MP800'))

by_ID <- group_by(df_size, ID)
df_avg <- summarise(by_ID,
            count = n(),
            turbidity = mean(turb_approx),
            Z_Ave = mean(Z_Ave),
            PdI = mean(PdI))

df_size$turb_approx <- factor(df_size$turb_approx)


ggplot(df_size) +
 aes(x = turb_approx, y = Z_Ave, fill = ID) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 theme_minimal()


ggplot(df_size) +
 aes(x = turb_approx, y = PdI, fill = ID) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 theme_minimal()

ggplot(df_size) +
 aes(x = turb_approx, y = Cumulants_Fit_Error, fill = ID) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 theme_minimal()



