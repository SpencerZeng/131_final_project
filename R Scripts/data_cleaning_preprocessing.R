# selected variable of interest 
brfss_selected <- brfss_raw %>% select('CVDCRHD4', 'X_BMI5', 'SMOKE100', 'X_RFDRHV7','CVDSTRK3','MENTHLTH', 'PHYSHLTH','DIFFWALK','EXERANY2','DIABETE4','X_RACE','X_AGEG5YR','BPHIGH4','TOLDHI2','CVDSTRK3','FRUIT2','FVGREEN1','INCOME2','SEXVAR')

# rename columns
brfss_selected <- brfss_selected %>% rename(
  'heart_disease' = 'CVDCRHD4',
  'bmi' = 'X_BMI5',
  'heavy_smoker' = 'SMOKE100',
  'heavy_alcohol' = 'X_RFDRHV7',
  'stroke' = 'CVDSTRK3',
  'mental_health' = 'MENTHLTH',
  'physical_health' = 'PHYSHLTH',
  'difficult_walk' = 'DIFFWALK',
  'exercise' = 'EXERANY2',
  'diabetes' = 'DIABETE4',
  'race' = 'X_RACE',
  'age' = 'X_AGEG5YR',
  'high_bp' = 'BPHIGH4',
  'high_chol'= 'TOLDHI2',
  'stroke'='CVDSTRK3',
  'fruit' = 'FRUIT2',
  'veggies' = 'FVGREEN1',
  'income' = 'INCOME2',
  'sex' = 'SEXVAR'
)

# check for missing values
missing_values <- brfss_selected %>%
  is.na() %>%
  colSums()

# drop missing values 
brfss <- brfss_selected %>% 
  drop_na() 

# drop levels
brfss <- brfss %>% subset(
  heart_disease != 7 & heart_disease !=9 & heavy_smoker != 7 & heavy_smoker != 9
  & heavy_alcohol != 7 & heavy_alcohol != 9 & stroke != 7 & stroke != 9 & mental_health != 77 & mental_health != 99 
  & physical_health != 77 & physical_health != 99 & difficult_walk != 7 & difficult_walk != 9 &
    diabetes != 7 & diabetes != 9 & age != 14 & high_bp != 7 & high_bp != 9 & exercise != 7 &
    exercise != 9 & fruit != 777 & fruit != 999 & veggies != 777 & veggies != 999 & income !=
    77 & income != 99 & high_chol != 7 & high_chol != 9 & race != 9)

# convert to factors 
brfss <- brfss %>% mutate(
  heart_disease = factor(heart_disease,levels = c(1,2)),
  heavy_smoker = factor(heavy_smoker),
  heavy_alcohol = factor(heavy_alcohol),
  stroke = factor(stroke),
  mental_health = factor(mental_health),
  physical_health = factor(physical_health),
  exercise = factor(exercise),
  diabetes = factor(diabetes),
  race = factor(race),
  high_bp = factor(high_bp),
  high_chol = factor(high_chol),
  income = factor(income),
  sex = factor(sex))

# balance the data 
brfss_balanced <- ovun.sample(heart_disease~., data=brfss, p=0.5, seed = 474,
                              method="under")$data

# split the data

brfss_split <- brfss_balanced %>% 
  initial_split(prop = 0.7, strata = "heart_disease")

brfss_train <- training(brfss_split)
brfss_test <- testing(brfss_split)