# Questions
Alex Stern  
July 5, 2017  



## Introduction
In this analysis we look at the breweries that each beer came from, and examine the alcohol content and bitterness of each beer by the state from which it was brewed.  We also try to determine if there is a correlation between ABV and IBU.


```r
load(file="datasets.RData")
#Question 1
print(table(Breweries$State))
```

```
## 
## AK AL AR AZ CA CO CT DC DE FL GA HI IA ID IL IN KS KY LA MA MD ME MI MN MO 
##  7  3  2 11 39 47  8  1  2 15  7  4  5  5 18 22  3  4  5 23  7  9 32 12  9 
## MS MT NC ND NE NH NJ NM NV NY OH OK OR PA RI SC SD TN TX UT VA VT WA WI WV 
##  2  9 19  1  5  3  3  4  2 16 15  6 29 25  5  4  1  3 28  4 16 10 23 20  1 
## WY 
##  4
```

```r
#Here each state is shown with the number of breweries in each state listed below the state.

#Question 2
names(Beers)[5] <- "Brew_ID"
BrewedBeers <- merge(Beers, Breweries, by="Brew_ID")
head(BrewedBeers, n=6)
```

```
##   Brew_ID        Name.x Beer_ID   ABV IBU
## 1       1  Get Together    2692 0.045  50
## 2       1 Maggie's Leap    2691 0.049  26
## 3       1    Wall's End    2690 0.048  19
## 4       1       Pumpion    2689 0.060  38
## 5       1    Stronghold    2688 0.060  25
## 6       1   Parapet ESB    2687 0.056  47
##                                 Style Ounces            Name.y        City
## 1                        American IPA     16 NorthGate Brewing Minneapolis
## 2                  Milk / Sweet Stout     16 NorthGate Brewing Minneapolis
## 3                   English Brown Ale     16 NorthGate Brewing Minneapolis
## 4                         Pumpkin Ale     16 NorthGate Brewing Minneapolis
## 5                     American Porter     16 NorthGate Brewing Minneapolis
## 6 Extra Special / Strong Bitter (ESB)     16 NorthGate Brewing Minneapolis
##   State
## 1    MN
## 2    MN
## 3    MN
## 4    MN
## 5    MN
## 6    MN
```

```r
#Here the data is merged by Brew_ID and these are the first 6 observations.
tail(BrewedBeers, n=6)
```

```
##      Brew_ID                    Name.x Beer_ID   ABV IBU
## 2405     556             Pilsner Ukiah      98 0.055  NA
## 2406     557  Heinnieweisse Weissebier      52 0.049  NA
## 2407     557           Snapperhead IPA      51 0.068  NA
## 2408     557         Moo Thunder Stout      50 0.049  NA
## 2409     557         Porkslap Pale Ale      49 0.043  NA
## 2410     558 Urban Wilderness Pale Ale      30 0.049  NA
##                        Style Ounces                        Name.y
## 2405         German Pilsener     12         Ukiah Brewing Company
## 2406              Hefeweizen     12       Butternuts Beer and Ale
## 2407            American IPA     12       Butternuts Beer and Ale
## 2408      Milk / Sweet Stout     12       Butternuts Beer and Ale
## 2409 American Pale Ale (APA)     12       Butternuts Beer and Ale
## 2410        English Pale Ale     12 Sleeping Lady Brewing Company
##               City State
## 2405         Ukiah    CA
## 2406 Garrattsville    NY
## 2407 Garrattsville    NY
## 2408 Garrattsville    NY
## 2409 Garrattsville    NY
## 2410     Anchorage    AK
```

```r
#Here the data is merged by Brew_ID and these are the last 6 observations.

#Question 3
sum(is.na(BrewedBeers$Brew_ID))
```

```
## [1] 0
```

```r
#This is the number of missing values in the Brew_ID column.
sum(is.na(BrewedBeers$Name.x))
```

```
## [1] 0
```

```r
#This is the number of missing values in the Name.x column.
sum(is.na(BrewedBeers$Beer_ID))
```

```
## [1] 0
```

```r
#This is the number of missing values in the Beer_ID column.
sum(is.na(BrewedBeers$ABV))
```

```
## [1] 62
```

```r
#This is the number of missing values in the ABV column.
sum(is.na(BrewedBeers$IBU))
```

```
## [1] 1005
```

```r
#This is the number of missing values in the IBU column.
sum(is.na(BrewedBeers$Style))
```

```
## [1] 5
```

```r
#This is the number of missing values in the Style column.
sum(is.na(BrewedBeers$Ounces))
```

```
## [1] 0
```

```r
#This is the number of missing values in the Ounces column.
sum(is.na(BrewedBeers$Name.y))
```

```
## [1] 0
```

```r
#This is the number of missing values in the Name.y column.
sum(is.na(BrewedBeers$City))
```

```
## [1] 0
```

```r
#This is the number of missing values in the City column.
sum(is.na(BrewedBeers$State))
```

```
## [1] 0
```

```r
#This is the number of missing values in the State column.

#Question 4
ABVbyState <- sapply(split(BrewedBeers$ABV, BrewedBeers$State), median, na.rm=TRUE)
print(ABVbyState)
```

```
##     AK     AL     AR     AZ     CA     CO     CT     DC     DE     FL 
## 0.0560 0.0600 0.0520 0.0550 0.0580 0.0605 0.0600 0.0625 0.0550 0.0570 
##     GA     HI     IA     ID     IL     IN     KS     KY     LA     MA 
## 0.0550 0.0540 0.0555 0.0565 0.0580 0.0580 0.0500 0.0625 0.0520 0.0540 
##     MD     ME     MI     MN     MO     MS     MT     NC     ND     NE 
## 0.0580 0.0510 0.0620 0.0560 0.0520 0.0580 0.0550 0.0570 0.0500 0.0560 
##     NH     NJ     NM     NV     NY     OH     OK     OR     PA     RI 
## 0.0550 0.0460 0.0620 0.0600 0.0550 0.0580 0.0600 0.0560 0.0570 0.0550 
##     SC     SD     TN     TX     UT     VA     VT     WA     WI     WV 
## 0.0550 0.0600 0.0570 0.0550 0.0400 0.0565 0.0550 0.0555 0.0520 0.0620 
##     WY 
## 0.0500
```

```r
#This is the median ABV for every state.
barplot(ABVbyState)
```

![](Case01_files/figure-html/datasets-1.png)<!-- -->

```r
#This is a barplot of the median ABV for each state.
IBUbyState <- sapply(split(BrewedBeers$IBU, BrewedBeers$State), median, na.rm=TRUE)
print(IBUbyState)
```

```
##   AK   AL   AR   AZ   CA   CO   CT   DC   DE   FL   GA   HI   IA   ID   IL 
## 46.0 43.0 39.0 20.5 42.0 40.0 29.0 47.5 52.0 55.0 55.0 22.5 26.0 39.0 30.0 
##   IN   KS   KY   LA   MA   MD   ME   MI   MN   MO   MS   MT   NC   ND   NE 
## 33.0 20.0 31.5 31.5 35.0 29.0 61.0 35.0 44.5 24.0 45.0 40.0 33.5 32.0 35.0 
##   NH   NJ   NM   NV   NY   OH   OK   OR   PA   RI   SC   SD   TN   TX   UT 
## 48.5 34.5 51.0 41.0 47.0 40.0 35.0 40.0 30.0 24.0 30.0   NA 37.0 33.0 34.0 
##   VA   VT   WA   WI   WV   WY 
## 42.0 30.0 38.0 19.0 57.5 21.0
```

```r
#This is the median IBU for every state.
barplot(IBUbyState)
```

![](Case01_files/figure-html/datasets-2.png)<!-- -->

```r
#This is a barplot of the median IBU for each state.

#Question 5
maxABV <- subset(BrewedBeers, ABV == max(BrewedBeers$ABV, na.rm=TRUE))
print(maxABV$State)
```

```
## [1] "CO"
```

```r
#This finds the observation with the maximum ABV, and prints its state.
maxIBU <- subset(BrewedBeers, IBU == max(BrewedBeers$IBU, na.rm=TRUE))
print(maxIBU$State)
```

```
## [1] "OR"
```

```r
#This finds the observation with the maximum IBU, and prints its state.

#Question 6
summary(BrewedBeers$ABV)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
## 0.00100 0.05000 0.05600 0.05977 0.06700 0.12800      62
```

```r
#These are the summary statistics for the ABV data.

#Question 7
plot(BrewedBeers$ABV, BrewedBeers$IBU)
```

![](Case01_files/figure-html/datasets-3.png)<!-- -->

```r
cor(BrewedBeers$ABV, BrewedBeers$IBU, use="complete.obs")
```

```
## [1] 0.6706215
```

```r
#This plots a scatterplot of the ABV vs. IBU, and calculates its correlation.
```
## Conclusion
We discover that there is a positive correlation between alcohol content and bitterness.

