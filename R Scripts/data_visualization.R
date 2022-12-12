plt_sex <-brfss_train %>% 
  ggplot(aes(x = heart_disease, group = factor(sex) ,fill=factor(sex))) + 
  geom_bar(position =  position_stack()) + 
  labs(y = "Cases", x = "Heart Disease", title = 'Barplot of heart disease by sex') +
  scale_fill_discrete(labels=c('Male', 'Female')) + 
  guides(fill=guide_legend(title="Sex")) + 
  theme_minimal()

plt_sex

labels <- as_labeller(c('1' = 'White','2' = 'Black','3' = 'American Indian','4' = 'Asian','5' = 'Natice Hawaiian','6' = 'Other','7' = 'Multiracial','8' = 'Hispanic'))
plt_race <- brfss_train %>% 
  ggplot(aes(x = race, group = factor(heart_disease) ,fill=factor(heart_disease))) + 
  geom_bar(position = position_stack()) + 
  facet_wrap(~race,scales = "free",labeller = labels) + 
  guides(fill=guide_legend(title="Heart Disease")) + 
  scale_fill_discrete(labels=c('No', 'Yes')) + 
  labs(title = 'Barplot of heart disease from each race ') + 
  theme_minimal() 


plt_race



plt_age <- brfss_train %>% 
  ggplot(aes(x = age, group = factor(heart_disease) ,fill=factor(heart_disease))) + 
  geom_bar(position = position_stack()) + 
  scale_x_discrete(labels = c('18 to 24', '25 to 29', '30 to 34', 
                              '35 to 39', '40 to 44', '45 to 49','50 to 59','60 to 64',
                              '65 to 69', '70 to 74', '75 to 79', '80+')) + 
  
  guides(fill=guide_legend(title="Heart Disease")) +
  scale_fill_discrete(labels=c('No', 'Yes')) + 
  labs(y = "Cases", x = "Age groups", title = 'Barplot of heart disease by age') + 
  theme_minimal()

plt_age


plt_income <- brfss_train %>% 
  ggplot(aes(x = income, group = factor(heart_disease) ,fill=factor(heart_disease))) + 
  geom_bar(position = position_stack()) + 
  scale_x_discrete(labels = c('<10k', '10k to 15k', '15k to 20k', 
                              '20k to 25k', '25k to 30k', '30k to 35k',
                              '35k to 40k','40k to 50k',
                              '50k to 75k', '75k+')) + 
  guides(fill=guide_legend(title='Heart Disease')) +
  scale_fill_discrete(labels=c('No', 'Yes')) + 
  labs(y = "Cases", x = "Income Levels", title = 'Barplot of heart disease by income levels') + 
  theme_minimal()

plt_income


mental_health <- brfss_train %>% subset (mental_health != 88)
plt_mh <- mental_health %>% 
  ggplot(aes(x = heart_disease, group = factor(mental_health) ,fill=factor(mental_health))) + 
  geom_bar(position = position_stack()) + 
  guides(fill=guide_legend(title="Days mental health not good")) + 
  theme_minimal() 


plt_ph <- brfss_train %>% 
  ggplot(aes(x = heart_disease, group = factor(physical_health) ,fill=factor(physical_health))) + geom_bar(position = position_stack()) + 
  guides(fill=guide_legend(title="Days physical health not good")) +
  theme_minimal()

grid.arrange(plt_mh, plt_ph, ncol = 2) 


brfss_train %>% 
  ggplot(aes(x = heart_disease, group = factor(heavy_smoker) ,fill=factor(heavy_smoker))) + 
  geom_bar(position = position_stack()) + 
  guides(fill=guide_legend(title="heavy smoking")) +
  scale_fill_discrete(labels=c('Yes', 'No')) + 
  labs(title = 'Barplot of heart disease by smoking status') + 
  theme_minimal()

brfss_train %>% 
  ggplot(aes(x = heart_disease, group = factor(heavy_alcohol) ,fill=factor(heavy_alcohol))) + 
  geom_bar(position = position_stack()) + 
  guides(fill=guide_legend(title="heavy drinker")) +
  scale_fill_discrete(labels=c('No', 'Yes')) + 
  labs(title = 'Barplot of heart disease by alcohol consumption') + 
  theme_minimal()

ggplot(brfss_train, aes(x=bmi, color=heart_disease)) +
  geom_density() + 
  theme_minimal()