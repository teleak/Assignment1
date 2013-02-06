
#### Load required libraries

require(plyr)
require(Hmisc)
require(lattice)
require(RColorBrewer)
require(sm)


### read in preprocessed data 

loans<-read.csv( '~/Desktop/RCourse/DataAnalysis/AnalysisAssignment1/Rproject/ProcessedData/Working/loans.csv')
data<-read.csv('~/Desktop/RCourse/DataAnalysis/AnalysisAssignment1/Rproject/ProcessedData/Working/data.csv')


### dimensions of data

dim(loans)
dim(data)

### Describe loans -- noticed that summary doesn't show loan amount of -0.01 in two rows. 

summary(loans)
range(loans$loaned)
subset(loans, loaned == -0.01)


### dropping rows from loaned that show loan amount < 0 (assuming this must be error)

min(loans$loaned)
loans<-loans[loans$loaned >= 0,]
min(loans$loaned)


### Break income down into ranges

attach(loans)
	range(income)
	inc.low<-floor(min(income))
	inc.high<-ceiling(max(income))
######## add  log income 2 as factor, antilog = (2^x)
	log.income<-log2(loans$income)
 	l.inc.low<-min(log.income) ### min of log income
	l.inc.high<-max(log.income)
	income.groups2<-cut2(log.income, seq(l.inc.low, l.inc.high, by=1))
	loans$log2.inc.range<-as.factor(income.groups2)

### debts
	head(loans)
	range(debts)
	debt.range<-cut2(debts, seq(min(debts), max(debts), by=5))
	loans$debt.range<-as.factor(debt.range)
	table(debt.range)
detach(loans)


### Some exploratory

attach(loans)
	hist(loaned, breaks=100
		,main="Amount Loaned"
		,col="orange")
	plot(loaned~log10(income))
	boxplot(loaned ~ income) ### not very informative
	histogram(~loaned | log2.inc.range,
			main="Loan amount by Log(2) Income Range")
	histogram(~loaned | debt.range,
			main="Loan amount by Debt to Income Ratio")
detach(loans)
