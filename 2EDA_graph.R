library(ggplot2)
library(gridExtra)

data <- read_excel("D:/MTECH/SEM 3/predictive analytics/ca2/dataCA2.xlsx")
df1<-data
df1 <- df1 %>%
  mutate(Year = year(DATE),
         Month = month(DATE),
         Day = day(DATE))

# Aggregate the advertising expenses and GRP by year
df_yearly <- df1 %>%
  group_by(Year) %>%
  summarise(
    SMS_Expense = sum(`Advertising Expenses (SMS)`, na.rm = TRUE),
    Newspaper_Expense = sum(`Advertising Expenses(Newspaper ads)`, na.rm = TRUE),
    Radio_Expense = sum(`Advertising Expenses(Radio)`, na.rm = TRUE),
    TV_Expense = sum(`Advertising Expenses(TV)`, na.rm = TRUE),
    Internet_Expense = sum(`Advertising Expenses(Internet)`, na.rm = TRUE),
    
    SMS_GRP = sum(`GRP(SMS)`, na.rm = TRUE),
    Newspaper_GRP = sum(`GRP (NewPaper ads)`, na.rm = TRUE),
    Radio_GRP = sum(`GRP(Radio`, na.rm = TRUE),
    TV_GRP = sum(`GRP(TV)`, na.rm = TRUE),
    Internet_GRP = sum(`GRP(Internet)`, na.rm = TRUE),
    Sales = sum(`SALES ($)`, na.rm = TRUE)
  )

View(df_yearly)

p1 <- ggplot(df_yearly, aes(x = Year)) +
  geom_line(aes(y = SMS_Expense, color = "SMS"), size = 1) +
  geom_line(aes(y = Newspaper_Expense, color = "Newspaper"), size = 1) +
  geom_line(aes(y = Radio_Expense, color = "Radio"), size = 1) +
  geom_line(aes(y = TV_Expense, color = "TV"), size = 1) +
  geom_line(aes(y = Internet_Expense, color = "Internet"), size = 1) +
  labs(title = "Money Spent on Advertising Medium",
       x = "Year", y = "Expense ($)") +
  scale_color_manual(values = c("SMS" = "blue", "Newspaper" = "red", "Radio" = "green", 
                                "TV" = "purple", "Internet" = "orange"),name = "Advertising Medium") +
  theme_minimal()


p2 <- ggplot(df_yearly, aes(x = Year)) +
  geom_line(aes(y = SMS_GRP, color = "SMS"), size = 1) +
  geom_line(aes(y = Newspaper_GRP, color = "Newspaper"), size = 1) +
  geom_line(aes(y = Radio_GRP, color = "Radio"), size = 1) +
  geom_line(aes(y = TV_GRP, color = "TV"), size = 1) +
  geom_line(aes(y = Internet_GRP, color = "Internet"), size = 1) +
  labs(title = "GRP of Advertising Mediums",
       x = "Year", y = "GRP") +
  scale_color_manual(values = c("SMS" = "blue", "Newspaper" = "red", "Radio" = "green", 
                                "TV" = "purple", "Internet" = "orange"),name = "Advertising Medium") +
  theme_minimal()


p3<- ggplot(df_yearly,aes(x=Year))+
            geom_line(aes(y=Sales,color="Sales"))+
            scale_color_manual(values = c("Sales" ="orange"),name = "Total sales") +
            theme_minimal()

# Plot both charts
grid.arrange(p1, p2,p3, ncol = 1)




