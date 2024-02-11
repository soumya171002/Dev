library(tidyverse)

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
#Q1 and 2
data_3 <- data_2 |> 
   mutate(VA = (VA_Q15 *1000000/80),
          emp = (EMP*1000),
          lp = VA/emp,
          lp_norm = lp/3068.131,
          emp_share = EMP/59696.514) |> 
  arrange(lp_norm) 

data_3_graph <- data_3 |> 
  filter(sectors != "Total")
data_4 <- data_3_graph |> 
  mutate(cumilative = cumsum(emp_share))

#Q3

data_4_graph <- data_4 |> 
  mutate(left = c(0,0.432688984,0.5841619,0.7348751,0.8006607,0.8726514,0.9204730,
                  0.9731190,0.9864238,0.9891068,0.9963740,0.9984785))


data_4_graph |> 
  ggplot(mapping = aes(ymin = 0))+
  geom_rect(aes(xmin = left, xmax = cumilative, ymax = lp_norm, fill = sectors, color = sectors))+
  scale_x_continuous(labels = scales::percent)+
  xlab("Share of total employment (%)")+
  ylab("Sector-relative labor productivity, 
       the economywide labor productivity = 1")+
  theme_clas
 
