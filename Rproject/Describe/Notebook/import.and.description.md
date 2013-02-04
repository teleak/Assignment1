Data Import and Description 
=============================

### Import data 

```r
raw <- read.csv("/Users/Teleak/Desktop/RCourse/DataAnalysis/AnalysisAssignment1/Rproject/WorkingData/loansData.csv")
```

### Some descriptives of the data frame 'raw.'

```r
names(raw)
```

```
##  [1] "Amount.Requested"               "Amount.Funded.By.Investors"    
##  [3] "Interest.Rate"                  "Loan.Length"                   
##  [5] "Loan.Purpose"                   "Debt.To.Income.Ratio"          
##  [7] "State"                          "Home.Ownership"                
##  [9] "Monthly.Income"                 "FICO.Range"                    
## [11] "Open.CREDIT.Lines"              "Revolving.CREDIT.Balance"      
## [13] "Inquiries.in.the.Last.6.Months" "Employment.Length"
```

### Some munging -- making names easier to type

```r
new.names <- c("requested", "loaned", "interest", "length", "purpose", "debts", 
    "state", "home", "income", "fico", "o.credit", "r.credit", "inquiries", 
    "employed")
old.names <- names(raw)
(names.convert <- as.data.frame(cbind(old.names, new.names)))
```

```
##                         old.names new.names
## 1                Amount.Requested requested
## 2      Amount.Funded.By.Investors    loaned
## 3                   Interest.Rate  interest
## 4                     Loan.Length    length
## 5                    Loan.Purpose   purpose
## 6            Debt.To.Income.Ratio     debts
## 7                           State     state
## 8                  Home.Ownership      home
## 9                  Monthly.Income    income
## 10                     FICO.Range      fico
## 11              Open.CREDIT.Lines  o.credit
## 12       Revolving.CREDIT.Balance  r.credit
## 13 Inquiries.in.the.Last.6.Months inquiries
## 14              Employment.Length  employed
```

```r
data <- raw
names(data) <- new.names
names(data)
```

```
##  [1] "requested" "loaned"    "interest"  "length"    "purpose"  
##  [6] "debts"     "state"     "home"      "income"    "fico"     
## [11] "o.credit"  "r.credit"  "inquiries" "employed"
```

### Some more data description with easier names (helps avoid confusion later)

```r
dim(data)
```

```
## [1] 2500   14
```

```r
str(data)
```

```
## 'data.frame':	2500 obs. of  14 variables:
##  $ requested: int  20000 19200 35000 10000 12000 6000 10000 33500 14675 7000 ...
##  $ loaned   : num  20000 19200 35000 9975 12000 ...
##  $ interest : Factor w/ 275 levels "10.00%","10.08%",..: 263 40 214 275 33 121 254 154 96 239 ...
##  $ length   : Factor w/ 2 levels "36 months","60 months": 1 1 2 1 1 1 1 2 1 1 ...
##  $ purpose  : Factor w/ 14 levels "car","credit_card",..: 3 3 3 3 2 10 3 2 2 2 ...
##  $ debts    : Factor w/ 1669 levels "0.04%","0.17%",..: 390 1178 1000 346 656 775 1102 374 1129 1488 ...
##  $ state    : Factor w/ 46 levels "AK","AL","AR",..: 37 39 5 16 28 7 19 18 5 5 ...
##  $ home     : Factor w/ 5 levels "MORTGAGE","NONE",..: 1 1 1 1 5 4 5 1 5 5 ...
##  $ income   : num  6542 4583 11500 3833 3195 ...
##  $ fico     : Factor w/ 38 levels "640-644","645-649",..: 20 16 11 12 12 7 17 14 10 16 ...
##  $ o.credit : int  14 12 14 10 11 17 10 12 9 8 ...
##  $ r.credit : int  14272 11140 21977 9346 14469 10391 15957 27874 7246 7612 ...
##  $ inquiries: int  2 1 1 0 0 2 0 0 1 0 ...
##  $ employed : Factor w/ 12 levels "< 1 year","1 year",..: 1 4 4 7 11 5 3 3 10 5 ...
```

```r
head(data, n = 5)
```

```
##       requested loaned interest    length            purpose  debts state
## 81174     20000  20000    8.90% 36 months debt_consolidation 14.90%    SC
## 99592     19200  19200   12.12% 36 months debt_consolidation 28.36%    TX
## 80059     35000  35000   21.98% 60 months debt_consolidation 23.81%    CA
## 15825     10000   9975    9.99% 36 months debt_consolidation 14.30%    KS
## 33182     12000  12000   11.71% 36 months        credit_card 18.78%    NJ
##           home income    fico o.credit r.credit inquiries employed
## 81174 MORTGAGE   6542 735-739       14    14272         2 < 1 year
## 99592 MORTGAGE   4583 715-719       12    11140         1  2 years
## 80059 MORTGAGE  11500 690-694       14    21977         1  2 years
## 15825 MORTGAGE   3833 695-699       10     9346         0  5 years
## 33182     RENT   3195 695-699       11    14469         0  9 years
```

```r
apply(data, 2, function(col) length(which(is.na(col))))  ### shows count of NA values by column
```

```
## requested    loaned  interest    length   purpose     debts     state 
##         0         0         0         0         0         0         0 
##      home    income      fico  o.credit  r.credit inquiries  employed 
##         0         1         0         2         2         2         0
```

```r
apply(data, 2, function(col) length(which(col == "")))  ### shows count of blank values by column
```

```
## requested    loaned  interest    length   purpose     debts     state 
##         0         0         0         0         0         0         0 
##      home    income      fico  o.credit  r.credit inquiries  employed 
##         0         0         0         0         0         0         0
```

```r
(has.na <- data[!(complete.cases(data)), ])  ### rows with NA data
```

```
##        requested loaned interest    length purpose debts state home income
## 101596      5000   4525    7.43% 36 months   other    1%    NY NONE     NA
## 101515      3500    225   10.28% 36 months   other   10%    NY RENT  15000
##           fico o.credit r.credit inquiries employed
## 101596 800-804       NA       NA        NA < 1 year
## 101515 685-689       NA       NA        NA < 1 year
```

### clean percent signs from interest and debt columns; months from length column

```r
### some cleanup
data$interest <- as.numeric(sub("%", "", data$interest))  ### remove percent sign on interest
data$debts <- as.numeric(sub("%", "", data$debts))  ### remove percent sign on interest
data$length <- as.numeric(sub(" months", "", data$length))
```


### looking at quantitative qualitative data summaries

```r
sapply(data[, c(1:3, 6, 9, 11:13)], summary, na.rm = TRUE)
```

```
## $requested
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1000    6000   10000   12400   17000   35000 
## 
## $loaned
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6000   10000   12000   16000   35000 
## 
## $interest
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    5.42   10.20   13.10   13.10   15.80   24.90 
## 
## $debts
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    9.75   15.30   15.40   20.70   34.90 
## 
## $income
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     588    3500    5000    5690    6800  103000       1 
## 
## $o.credit
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     2.0     7.0     9.0    10.1    13.0    38.0       2 
## 
## $r.credit
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##       0    5590   11000   15200   18900  271000       2 
## 
## $inquiries
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   0.000   0.000   0.906   1.000   9.000       2
```

```r
sapply(data[, c(4, 5, 7, 8, 10, 14)], unique)
```

```
## $length
## [1] 36 60
## 
## $purpose
##  [1] debt_consolidation credit_card        other             
##  [4] moving             car                vacation          
##  [7] home_improvement   house              major_purchase    
## [10] educational        medical            wedding           
## [13] small_business     renewable_energy  
## 14 Levels: car credit_card debt_consolidation ... wedding
## 
## $state
##  [1] SC TX CA KS NJ CT MA LA FL DC OH AL AZ GA WV NH VA NY MD HI PA WA IL
## [24] NC WI SD AK DE MN MO RI CO NM MI OK NV UT AR KY VT OR IA MT IN WY MS
## 46 Levels: AK AL AR AZ CA CO CT DC DE FL GA HI IA IL IN KS KY LA MA ... WY
## 
## $home
## [1] MORTGAGE RENT     OWN      OTHER    NONE    
## Levels: MORTGAGE NONE OTHER OWN RENT
## 
## $fico
##  [1] 735-739 715-719 690-694 695-699 670-674 720-724 705-709 685-689
##  [9] 665-669 725-729 730-734 740-744 760-764 675-679 765-769 780-784
## [17] 830-834 660-664 710-714 785-789 750-754 700-704 680-684 755-759
## [25] 790-794 810-814 775-779 815-819 745-749 805-809 800-804 655-659
## [33] 770-774 795-799 640-644 645-649 820-824 650-654
## 38 Levels: 640-644 645-649 650-654 655-659 660-664 665-669 ... 830-834
## 
## $employed
##  [1] < 1 year  2 years   5 years   9 years   3 years   10+ years 8 years  
##  [8] 6 years   1 year    7 years   4 years   n/a      
## 12 Levels: < 1 year 1 year 10+ years 2 years 3 years 4 years ... n/a
```

