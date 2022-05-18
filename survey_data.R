library(fmsb)
library(stats)
library(tidyverse)
library(xlsx)

# code below based adjusted and based on https://github.com/elenamondino/nationwide_survey/blob/main/Rcode.R
df <- read.csv("data/Dataset_round1_august2020.csv", header = TRUE, na = "999")
# Rename the columns for ease of use
colnames(df) <- c("INTNR", "gender", "age", "region_ita", "region_swe",
                  "lik_ep", "lik_fl", "lik_dr", "lik_wf", "lik_ea", "lik_ta", "lik_dv", "lik_ec", "lik_cc",
                  "dam_ep",  "dam_fl", "dam_dr", "dam_wf", "dam_ea", "dam_ta", "dam_dv", "dam_ec", "dam_cc",
                  "dam_oth_ep", "dam_oth_fl", "dam_oth_dr", "dam_oth_wf", "dam_oth_ea", "dam_oth_ta", "dam_oth_dv", "dam_oth_ec", "dam_oth_cc",
                  "prep_aut_ep", "prep_aut_fl", "prep_aut_dr", "prep_aut_wf", "prep_aut_ea", "prep_aut_ta", "prep_aut_dv", "prep_aut_ec", "prep_aut_cc",
                  "prep_ep", "prep_fl", "prep_dr", "prep_wf", "prep_ea", "prep_ta", "prep_dv", "prep_ec", "prep_cc",
                  "know_aut_ep", "know_aut_fl", "know_aut_dr", "know_aut_wf", "know_aut_ea", "know_aut_ta", "know_aut_dv", "know_aut_ec", "know_aut_cc",
                  "know_ep", "know_fl", "know_dr", "know_wf", "know_ea", "know_ta", "know_dv", "know_ec", "know_cc",
                  "exp_ep", "exp_fl", "exp_dr", "exp_wf", "exp_ea", "exp_ta", "exp_dv", "exp_ec", "exp_cc",
                  "edu", "income", "work", "sector", "pol", "weight", "area")
df$area2 <- ifelse(df$area == 1, 1,
                   ifelse(df$area == 2, 1, 2))
df$area2 <- as.factor(df$area2) # creates a factor with 2 levels (Italy = 1, Sweden = 2)
# here we exclude not needed columns
df_research <- df
not_needed_columns <- c("INTNR", "region_ita", "region_swe", "weight", "pol", "area")
df_research <- df_research %>% select(-not_needed_columns)

# How to create the radar charts (example for the variable "Impact") ----
library(fmsb)
library(stats)

df_dam <- df[, c(15:32, 85)] # takes only the columns needed for the chart
only_columns_for_chart <- df_dam %>% names()


df_new <- read.csv("data/18052022_survey.csv", header = TRUE, na = "999")
not_needed_columns <- c("Sygnatura.czasowa")
df_new <- df_new %>% select(-not_needed_columns)

mapping_tibble <- readxl ::read_excel("data\\mapping_tibble.xlsx")

df_new <- df_new %>%
  rename_at(vars(as.character(mapping_tibble$our_names)),
            ~ as.character(mapping_tibble$original_names))

df_new <- df_new %>%
  mutate(across(everything(), as.character))

df_new_likert_longer <- df_new %>%
  mutate(nr_row = 1:nrow(.), .before = lik_ep) %>%
  select(nr_row:know_cc, sector, income) %>%
  pivot_longer(cols = -nr_row)

df_new_yes_no_longer <- df_new %>%
  mutate(nr_row = 1:nrow(.), .before = exp_ep) %>%
  select(nr_row:exp_cc) %>%
  pivot_longer(cols = -nr_row)

# no mapping for age
df_new_age <- df_new %>%
    select(age) %>%
    mutate(nr_row = 1:nrow(.))

# simple likert mapping
df_new_likert_mapped <- df_new_likert_longer %>%
  mutate(first_char = substr(value, 1, 2)) %>%
  mutate(first_char = str_trim(first_char)) %>%
  mutate(first_char = str_replace_all(first_char, "\\.", "")) %>%
  mutate(int_value = if_else(first_char %in% c("1", "2", "3", "4", "5"),
                             first_char, NULL)) %>%
  pivot_wider(id_cols = nr_row, names_from = name, values_from =  int_value)

# other mapping
df_for_other_mapping <-
  df_new %>%
  mutate(nr_row = 1:nrow(.), .before = edu) %>%
  select(nr_row:gender, -income, -sector) %>%
  pivot_longer(cols = -nr_row)

values_mapping <- readxl::read_excel("Data/values_mapping.xlsx")

df_new_all <-
  df_for_other_mapping %>%
    left_join(values_mapping, on = "value") %>%
    pivot_wider(id_cols = nr_row, names_from = name, values_from =  int_value)

# simple yes/no mapping
df_new_yes_no_mapped <- df_new_yes_no_longer %>%
  left_join(values_mapping, on = "value") %>%
  distinct() %>%
  pivot_wider(id_cols = nr_row, names_from = name, values_from =  int_value)


df_new_all_cols <- df_new_yes_no_mapped %>%
  inner_join(df_new_likert_mapped, on = "nr_row") %>%
  inner_join(df_new_all, on = "nr_row") %>%
  inner_join(df_new_age, on = "nr_row")

# mapping all the values to integer
df_new_all_cols_int_mapped <- df_new_all_cols %>%
  select(-work) %>%
  mutate(across(everything(), as.numeric))
df_new_all_cols_int_mapped$area2 <-  3
df_new_all_cols_int_mapped$area2 <-  as.factor(df_new_all_cols_int_mapped$area2)
df_research_names <- df_research %>% names()
df_new_final <- df_new_all_cols_int_mapped %>%
  relocate(all_of(only_columns_for_chart))

df_extended <- bind_rows(df_research, df_new_final)

# plots below based adjusted and based on https://github.com/elenamondino/nationwide_survey/blob/main/Rcode.R
library(fmsb)
library(stats)
#df_dam <- df_extended[, c(15:32, 85)] # takes only the columns needed for the chart
df_dam_extended <- df_extended %>%
  select(all_of(only_columns_for_chart))
# Dataframe for the variable "Impact on respondent"
df_dam_ind <- aggregate(cbind(dam_ep, dam_fl, dam_dr, dam_wf,
                              dam_ea, dam_ta, dam_dv, dam_ec, dam_cc) ~ area2,
                        data = df_dam_extended, mean, na.rm = FALSE) # calculates the mean for each of the variables listed in cbind, per country

# Dataframe for the variable "Impact on others in the country"
df_dam_oth <- aggregate(cbind(dam_oth_ep, dam_oth_fl, dam_oth_dr, dam_oth_wf,
                              dam_oth_ea, dam_oth_ta, dam_oth_dv, dam_oth_ec, dam_oth_cc) ~ area2,
                        data = df_dam_extended, mean, na.rm = TRUE) # calculates the mean for each of the variables listed in cbind, per country

colnames(df_dam_ind) <- c("area", "Epidemics", "Floods", "Drought", "Wildfires", "Earthquakes",
                          "Terror attacks", "Domestic violence", "Economic crises", "Climate Change")
colnames(df_dam_oth) <- c("area", "Epidemics", "Floods", "Drought", "Wildfires", "Earthquakes",
                          "Terror attacks", "Domestic violence", "Economic crises", "Climate Change")

# Create dataframe for Italy --------
df_dam_ita <- rbind(df_dam_ind[1,], df_dam_oth[1,]) # puts together the first rows of the above dataframes (i.e. those referring to Italy)
df_dam_ita <- df_dam_ita[,2:10]
rownames(df_dam_ita) <- c("on the respondent", "on others in the country")
colnames(df_dam_ita) <- c("Epidemics", "Floods", "Drought", "Wildfires", "Earthquakes",
                          "Terror attacks", "Domestic violence", "Economic crises", "Climate Change")
df_dam_ita <- rbind(rep(5,9) , rep(1,9) , df_dam_ita) # adds rows for the successful creation of radarcharts


# Create dataframe for Sweden ---------
df_dam_swe <- rbind(df_dam_ind[2,], df_dam_oth[2,]) # puts together the second rows of the above dataframes (i.e. those referring to Sweden)
df_dam_swe <- df_dam_swe[,2:10]
rownames(df_dam_swe) <- c("on the respondent", "on others in the country")
colnames(df_dam_swe) <- c("Epidemics", "Floods", "Drought", "Wildfires", "Earthquakes",
                          "Terror attacks", "Domestic violence", "Economic crises", "Climate Change")
df_dam_swe <- rbind(rep(5,9) , rep(1,9) , df_dam_swe) # adds rows for the successful creation of radar charts

# Create dataframe for Poland ---------
df_dam_pol <- rbind(df_dam_ind[3,], df_dam_oth[3,]) # puts together the second rows of the above dataframes (i.e. those referring to Sweden)
df_dam_pol <- df_dam_pol[,2:10]
rownames(df_dam_pol) <- c("on the respondent", "on others in the country")
colnames(df_dam_pol) <- c("Epidemics", "Floods", "Drought", "Wildfires", "Earthquakes",
                          "Terror attacks", "Domestic violence", "Economic crises", "Climate Change")
df_dam_pol <- rbind(rep(5,9) , rep(1,9) , df_dam_pol) # adds rows for the successful creation of radar charts


# Create the radar charts ------------
colors_border <- c("#b9de28ff", "#47972aff")

chart_ita <- radarchart(df_dam_ita,
                        axistype = 1 ,
                        #customize the polygons
                        pcol = colors_border,
                        #pfcol = , # for filling the polygons
                        pty = 32,
                        plwd = 2,
                        plty = 1,
                        #customize the grid
                        cglcol = "grey",
                        cglty = 1,
                        axislabcol = "grey",
                        caxislabels = seq(1,5,1),
                        cglwd = 0.8,
                        #custom labels
                        vlcex = 0.9,
                        title = "Italy - Perceived impact of the following threats")
legend(x = 1.5, y = 1, legend = c("on the respondent", "on others in the country"),
       bty = "n", pch = 20 , col = colors_border, text.width = 2, cex = 0.8, pt.cex = 2)

chart_swe <- radarchart(df_dam_swe,
                        axistype = 1 ,
                        #customize the polygons
                        pcol = colors_border,
                        #pfcol = , # for filling the polygons
                        pty = 32,
                        plwd = 2,
                        plty = 1,
                        #customize the grid
                        cglcol = "grey",
                        cglty = 1,
                        axislabcol = "grey",
                        caxislabels = seq(1,5,1),
                        cglwd = 0.8,
                        #custom labels
                        vlcex = 0.9,
                        title = "Sweden - Perceived impact of the following threats")
legend(x = 1.5, y = 1, legend = c("on the respondent", "on others in the country"),
       bty = "n", pch = 20 , col = colors_border, text.width = 2, cex = 0.8, pt.cex = 2)

chart_swe <- radarchart(df_dam_pol,
                        axistype = 1 ,
                        #customize the polygons
                        pcol = colors_border,
                        #pfcol = , # for filling the polygons
                        pty = 32,
                        plwd = 2,
                        plty = 1,
                        #customize the grid
                        cglcol = "grey",
                        cglty = 1,
                        axislabcol = "grey",
                        caxislabels = seq(1,5,1),
                        cglwd = 0.8,
                        #custom labels
                        vlcex = 0.9,
                        title = "Poland - Perceived impact of the following threats")
legend(x = 1.5, y = 1, legend = c("on the respondent", "on others in the country"),
       bty = "n", pch = 20 , col = colors_border, text.width = 2, cex = 0.8, pt.cex = 2)
