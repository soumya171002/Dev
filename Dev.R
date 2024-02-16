
library(tidyverse)
library(ggplot2)
library(knitr)
install.packages("knitr")
data <- read_rds("ETD_230918.RDS")

names(data)
data_edit <-data |> 
  filter(country == "Bangladesh",
         var == "VA_Q15" | var == "EMP",
         year == 2015)

data_1 <- data_edit |> 
  select(-c("country", "cnt", "year", "Warflag"))

data_2 <- data_1 |> 
  pivot_longer(cols = c("Agriculture", "Manufacturing","Mining", "Utilities", "Construction",
                        "Trade", "Transport", "Business", "Finance", "Realestate", "Government", "Other", "Total"),
               names_to = "sectors",
               values_to = "value") |>
  pivot_wider(id_cols = "sectors",
              names_from = "var",
              values_from = "value")
#Q1 and 2---------------------------
data_3 <- data_2 |> 
   mutate(VA = (VA_Q15 *1000000/80),
          emp = (EMP*1000),
          lp = VA/emp,
          lp_norm = lp/3068.131,
          emp_share = EMP/59696.514) |> 
  arrange(lp_norm) 

data_3_graph <- data_3 |> 
  filter(sectors != "Total") |>
  mutate(cumilative = cumsum(emp_share)) |>
  mutate(left = c(0,0.432688984,0.5841619,0.7348751,0.8006607,0.8726514,0.9204730,
                  0.9731190,0.9864238,0.9891068,0.9963740,0.9984785))
  

#Q3-------------------------------------

data_3_graph |> 
  ggplot(mapping = aes(ymin = 0))+
  geom_rect(aes(xmin = left, xmax = cumilative, ymax = lp_norm, fill = sectors, color = sectors))+
  scale_x_continuous(labels = scales::percent)+
  xlab("Share of total employment (%)")+
  ylab("Sector-relative labor productivity, 
       the economywide labor productivity = 1")+
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(main = "Labor productivity gaps in Bangladesh, 2015.")
 #Agri is hiring a lot, services sectors employs lowest amount and there is 
##huge gap in productivity across sectors and going way beyond 1, the normalised total productivity.
##Agri. lowest productivity, manifacturing is just 0.15 emp share considerinh importamce in exports 
##inverse relatinionship between productivity according to theory 
#Q4-------------------------------------------
data_3 <- data_3 |>
  mutate(wt_lp = (lp * emp_share)) |>
  mutate(emp_share_alt = c(0.150713239, 0.151472882, 1.000000000, 0.43268898, 
                           0.065785617, 0.071990653, 0.047821653, 0.052646005, 
                           0.013304797, 0.002682987, 0.007267141, 0.002104511,
                           0.001521529)) |>
  mutate(alt_wt_lp = (lp * emp_share_alt)) |>
  mutate(alt_lp = (VA / (emp_share_alt * 59696514)))

 # percentage change in labor productivity
data_3 |>
  filter(sectors != "Total") |>
  summarise(salp = sum(alt_wt_lp), slp = sum(wt_lp)) |>
  summarise(change = ((salp - slp) / slp) * 100)

data_3 |>
  filter(sectors != "Total") |>
  summarise(salp = sum(alt_lp), slp = sum(lp)) |>
  summarise(change = ((salp - slp) / slp) * 100)
# attempted to make a new column for the agri and manufacturing flip but that wasn't working. 
# Open to any suggestions

#Q5------------------------------------------
dat <- data |> 
  filter(country == "Bangladesh", var != "VA") |>
  select(-c("country", "cnt", "Warflag")) |>
  pivot_longer(cols = c("Agriculture", "Manufacturing","Mining", "Utilities", "Construction",
                        "Trade", "Transport", "Business", "Finance", "Realestate", "Government", "Other", "Total"),
               names_to = "sectors",
               values_to = "value") |> 
  pivot_wider(id_cols = c("sectors", "year"),
              names_from = "var",
              values_from = "value") |>
  mutate(VA = (VA_Q15 *1000000/80),
         emp = (EMP*1000),
         lp = VA/emp)

# I would suggest we use the above as the master data set and then proceed to Q1

# below is one attempt
dat_6 <- dat |>
  filter(sectors != "Total") |>
  group_by(year) |>
  summarise(mean = mean(lp), sd = sd(lp)) |>
  mutate(coef_var = sd / mean) |>
  ggplot(aes(x = year, y = coef_var)) +
  geom_point()+
  stat_smooth(method = "lm")
dat_6
#this is another attempt
dat_avg <- dat |>
  group_by(sectors) |>
  summarise(avg_lp = mean(lp))

dat |> 
  left_join(dat_avg, by = "sectors") |>
 mutate(lp = log(lp),
         avg_lp  = log(avg_lp),
    coef_var = (lp - avg_lp) / avg_lp) |>
  ggplot(aes(x = coef_var, y = lp, color = sectors)) +
  geom_point()

#Over time, the cov is reducing 
#Q6---------------------------------

dat_4 <- dat |>
  filter(sectors != "Total") |>
  mutate(broad_sectors = case_when(
    sectors == "Agriculture" ~ "Agriculture",
    sectors == "Manufacturing" | sectors == "Mining"
    | sectors == "Utilities" ~ "Manufacturing", 
    TRUE ~ "Services"
  )) |>
  filter(year == 1990 | year == 2005 | year == 2018) |>
  select(-c(sectors))

#Q7 ---------------------------------

dat_5 <- dat_4 |>
  group_by(broad_sectors, year) |>
  summarise(VA = sum(VA), emp = sum(emp)) |>
  mutate(lp = VA/emp)

dat_6 <- dat_5 |>
  group_by(year) |>
  summarise(LP = sum(VA)/sum(emp),
            EMP = sum(emp))

merged_dat <- left_join(dat_5, dat_6, by = "year") |>
  mutate(emp_share = (emp/EMP))
  
  
merged_dat_1 <- merged_dat |>
  filter(year == "1990"| year == "2005") |>
  mutate(del_LP = ifelse(year == "2005", LP - lag(LP), NA),
         del_lp = ifelse(year == "2005", lp - lag(lp), NA),
         with_1 = (lag(emp_share))*del_lp,
         acro_1 = (emp_share - lag(emp_share)) * lp,
         within_1 = with_1/del_LP,
         across_1 = acro_1/del_LP) 

wa_sum_1 <- merged_dat_1 |>
  filter(year == "2005") |>
  summarise(check = sum(across_1) + sum(within_1))|>
  summarise(sum_check = sum(check))

merged_dat_2 <- merged_dat |>
  filter(year == "2005"| year == "2018") |>
  mutate(del_LP = ifelse(year == "2018", LP - lag(LP), NA),
         del_lp = ifelse(year == "2018", lp - lag(lp), NA),
         with_2 = (lag(emp_share))*del_lp,
         acro_2 = (emp_share - lag(emp_share)) * lp,
         within_2 = with_2/del_LP,
         across_2 = acro_2/del_LP)

wa_sum_2 <- merged_dat_2 |>
  filter(year == "2018") |>
  summarise(check = (sum(across_2) + sum(within_2))) |>
  summarise(sum_check = sum(check))

ggplot(data = merged_dat, aes(x = year, y = emp_share, group = broad_sectors, color = broad_sectors)) +
  geom_line() +
  labs(x = "Years", y = "Sector-wise Employment Share in Bangladesh", color = "Sector") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +  
  theme(legend.position = "right")

# Q7 Explanation :
# The within term accounts for approximately 53% of the total productivity change from 1990 to 2005
# and the across term accounts for 47%. From 2005 to 2018, approximately 72% the change in productivity 
# has resulted from within productivity changes in the sectors in Bangladesh and approximately 28 percentage
# change in productivity resulted from labor movement across the sectors. From 2005 to 2018, the employment share
# of services increased from 40% to 45%, and agriculture fell from 48% to 40%. 

# Q8 Explanation :
# In 1990 to 2005, approximately 25% in agriculture, 28% in manufacturing and 0.2% decrease in services. In 2005-2018, 
# approximately 8.6% decrease in agriculture, 16.78% increase in manufacturing and 20.12% increase in services.


# Q9 Explanation :
# Covariance falling as a result of Productivity gap falling is a good thing.


