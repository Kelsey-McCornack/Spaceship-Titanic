Spaceship Titanic
================
Kelsey McCornack
2023-04-17

``` r
# Load libraries
library(dplyr)
library(tidyr)
library(DescTools) # not using this?
library(corrplot)
library(ggplot2)
```

``` r
# Load data sets
train_raw <- read.csv('train.csv')
test_raw <- read.csv('test.csv')
```

``` r
# Merging training and testing sets for cleaning purposes
train <- train_raw
test <- test_raw

test$Transported <- NA
train$Set <- 0
test$Set <- 1

master <- rbind(train, test)
head(master)
```

    ##   PassengerId HomePlanet CryoSleep Cabin   Destination Age   VIP RoomService
    ## 1     0001_01     Europa     False B/0/P   TRAPPIST-1e  39 False           0
    ## 2     0002_01      Earth     False F/0/S   TRAPPIST-1e  24 False         109
    ## 3     0003_01     Europa     False A/0/S   TRAPPIST-1e  58  True          43
    ## 4     0003_02     Europa     False A/0/S   TRAPPIST-1e  33 False           0
    ## 5     0004_01      Earth     False F/1/S   TRAPPIST-1e  16 False         303
    ## 6     0005_01      Earth     False F/0/P PSO J318.5-22  44 False           0
    ##   FoodCourt ShoppingMall  Spa VRDeck              Name Transported Set
    ## 1         0            0    0      0   Maham Ofracculy       False   0
    ## 2         9           25  549     44      Juanna Vines        True   0
    ## 3      3576            0 6715     49     Altark Susent       False   0
    ## 4      1283          371 3329    193      Solam Susent       False   0
    ## 5        70          151  565      2 Willy Santantines        True   0
    ## 6       483            0  291      0 Sandie Hinetthews        True   0

``` r
# Remove and reconfigure columns
cols_to_remove <- c('PassengerId', 'Name', 'Survived')
master <- master[, !(names(master) %in% cols_to_remove)]
master <- separate(master, Cabin, into = c('CabinDeck', 'CabinNum', 'CabinSide'), sep = '/')

head(master)
```

    ##   HomePlanet CryoSleep CabinDeck CabinNum CabinSide   Destination Age   VIP
    ## 1     Europa     False         B        0         P   TRAPPIST-1e  39 False
    ## 2      Earth     False         F        0         S   TRAPPIST-1e  24 False
    ## 3     Europa     False         A        0         S   TRAPPIST-1e  58  True
    ## 4     Europa     False         A        0         S   TRAPPIST-1e  33 False
    ## 5      Earth     False         F        1         S   TRAPPIST-1e  16 False
    ## 6      Earth     False         F        0         P PSO J318.5-22  44 False
    ##   RoomService FoodCourt ShoppingMall  Spa VRDeck Transported Set
    ## 1           0         0            0    0      0       False   0
    ## 2         109         9           25  549     44        True   0
    ## 3          43      3576            0 6715     49       False   0
    ## 4           0      1283          371 3329    193       False   0
    ## 5         303        70          151  565      2        True   0
    ## 6           0       483            0  291      0        True   0

``` r
# Examine data
cat('Data Types: \n\n')
```

    ## Data Types:

``` r
sapply(master, class)
```

    ##   HomePlanet    CryoSleep    CabinDeck     CabinNum    CabinSide  Destination 
    ##  "character"  "character"  "character"  "character"  "character"  "character" 
    ##          Age          VIP  RoomService    FoodCourt ShoppingMall          Spa 
    ##    "numeric"  "character"    "numeric"    "numeric"    "numeric"    "numeric" 
    ##       VRDeck  Transported          Set 
    ##    "numeric"  "character"    "numeric"

``` r
cat('\nNA Counts:\n\n')
```

    ## 
    ## NA Counts:

``` r
colSums(is.na(master))
```

    ##   HomePlanet    CryoSleep    CabinDeck     CabinNum    CabinSide  Destination 
    ##            0            0            0          299          299            0 
    ##          Age          VIP  RoomService    FoodCourt ShoppingMall          Spa 
    ##          270            0          263          289          306          284 
    ##       VRDeck  Transported          Set 
    ##          268         4277            0

``` r
cat('\nProportion NAs by Column:\n\n')
```

    ## 
    ## Proportion NAs by Column:

``` r
round(colSums(is.na(master))/nrow(master), 4)
```

    ##   HomePlanet    CryoSleep    CabinDeck     CabinNum    CabinSide  Destination 
    ##       0.0000       0.0000       0.0000       0.0231       0.0231       0.0000 
    ##          Age          VIP  RoomService    FoodCourt ShoppingMall          Spa 
    ##       0.0208       0.0000       0.0203       0.0223       0.0236       0.0219 
    ##       VRDeck  Transported          Set 
    ##       0.0207       0.3298       0.0000

``` r
cat('\nTotal Proprion NA values:\n\n')
```

    ## 
    ## Total Proprion NA values:

``` r
round(sum(rowSums(is.na(master))>0)/nrow(master), 4)
```

    ## [1] 0.4263

## Clean Data

``` r
colnames(master)
```

    ##  [1] "HomePlanet"   "CryoSleep"    "CabinDeck"    "CabinNum"     "CabinSide"   
    ##  [6] "Destination"  "Age"          "VIP"          "RoomService"  "FoodCourt"   
    ## [11] "ShoppingMall" "Spa"          "VRDeck"       "Transported"  "Set"

``` r
# Clean data
head(master)
```

    ##   HomePlanet CryoSleep CabinDeck CabinNum CabinSide   Destination Age   VIP
    ## 1     Europa     False         B        0         P   TRAPPIST-1e  39 False
    ## 2      Earth     False         F        0         S   TRAPPIST-1e  24 False
    ## 3     Europa     False         A        0         S   TRAPPIST-1e  58  True
    ## 4     Europa     False         A        0         S   TRAPPIST-1e  33 False
    ## 5      Earth     False         F        1         S   TRAPPIST-1e  16 False
    ## 6      Earth     False         F        0         P PSO J318.5-22  44 False
    ##   RoomService FoodCourt ShoppingMall  Spa VRDeck Transported Set
    ## 1           0         0            0    0      0       False   0
    ## 2         109         9           25  549     44        True   0
    ## 3          43      3576            0 6715     49       False   0
    ## 4           0      1283          371 3329    193       False   0
    ## 5         303        70          151  565      2        True   0
    ## 6           0       483            0  291      0        True   0

``` r
master <- master %>%
  mutate_at(vars(matches('CryoSleep|VIP')), ~ifelse(. == 'True', 1, 0)) %>%
  mutate_at(vars(matches('Cryosleep|VIP')), factor) %>%
  mutate(across(where(is.character), ~ replace_na(., Mode(., na.rm = TRUE)))) %>%
  mutate(across(where(is.numeric), ~ replace_na(., median(., na.rm = TRUE)))) %>%
  mutate_if(is.character, as.factor)

sapply(master, class)
```

    ##   HomePlanet    CryoSleep    CabinDeck     CabinNum    CabinSide  Destination 
    ##     "factor"     "factor"     "factor"     "factor"     "factor"     "factor" 
    ##          Age          VIP  RoomService    FoodCourt ShoppingMall          Spa 
    ##    "numeric"     "factor"    "numeric"    "numeric"    "numeric"    "numeric" 
    ##       VRDeck  Transported          Set 
    ##    "numeric"     "factor"    "numeric"

``` r
colSums(is.na(master))
```

    ##   HomePlanet    CryoSleep    CabinDeck     CabinNum    CabinSide  Destination 
    ##            0            0            0            0            0            0 
    ##          Age          VIP  RoomService    FoodCourt ShoppingMall          Spa 
    ##            0            0            0            0            0            0 
    ##       VRDeck  Transported          Set 
    ##            0            0            0

``` r
head(master)
```

    ##   HomePlanet CryoSleep CabinDeck CabinNum CabinSide   Destination Age VIP
    ## 1     Europa         0         B        0         P   TRAPPIST-1e  39   0
    ## 2      Earth         0         F        0         S   TRAPPIST-1e  24   0
    ## 3     Europa         0         A        0         S   TRAPPIST-1e  58   1
    ## 4     Europa         0         A        0         S   TRAPPIST-1e  33   0
    ## 5      Earth         0         F        1         S   TRAPPIST-1e  16   0
    ## 6      Earth         0         F        0         P PSO J318.5-22  44   0
    ##   RoomService FoodCourt ShoppingMall  Spa VRDeck Transported Set
    ## 1           0         0            0    0      0       False   0
    ## 2         109         9           25  549     44        True   0
    ## 3          43      3576            0 6715     49       False   0
    ## 4           0      1283          371 3329    193       False   0
    ## 5         303        70          151  565      2        True   0
    ## 6           0       483            0  291      0        True   0

``` r
# Split data back into testing and training sets
train <- master %>%
  filter(Set == 0) %>%
  select(-Set)

test <- master %>%
  filter(Set == 1) %>%
  select(-Set)

train$Transported <- train_raw$Transported

train <- train  %>%
  mutate_at('Transported', ~ifelse(. == 'True', 1, 0)) %>%
  mutate_at('Transported', factor)
```
