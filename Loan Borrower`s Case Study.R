
#Setting the Working Directory
#setwd("D:\\Datascience_June-2018\\EDA_CASE_STUDY\\loan")

#Loading the dtaa into Data frame from CSV File  
loan <- read.csv("loan.csv",stringsAsFactors = FALSE)

#structure of the DF
str(loan)

#----DATA CLEANSING Started----

##Assumption: We are taking only 2 types,They are FULLY PAID and CHARGEDOFF in this analysis 
#as the CURRENT Payer may pay or not pay the full loan.So neglecting it

loan <- loan[-which(toupper(loan$loan_status)=="CURRENT"), ]

#deleting the columns which has only 1 unique value
loan <- loan[sapply(loan, function(x) length(unique(x)) > 1)]

#removing the URL,DESC,TITLE,id,member_id as doesn't has much information
loan <- subset(loan, select = -c(url,desc,title,id,member_id))

#considering only the Months
loan$term <- substr(loan$term, 2, 3)

#removing percent symbol from int_rate and revol_util
loan$int_rate <-  as.numeric(substr(loan$int_rate, 0, nchar(loan$int_rate) - 1))
loan$revol_util <-  as.numeric(substr(loan$revol_util, 0, nchar(loan$revol_util) - 1))

#the below mentioned columns has only 0 and NA values and which is of no use and hence removing those columns from the loan DF
loan <- subset(loan, select = -c(collections_12_mths_ex_med, chargeoff_within_12_mths, tax_liens))

##Deriving the New Columns And rounding off
#This will be useful to analyse the customer might be charged off, if the customer not able to pay the loan
loan['monthly_income'] <- round(loan$annual_inc / 12)
loan['monthly_insatllment_inc'] <- round((loan$installment / loan$monthly_income)*100)


library(lubridate)

#Deriving the New columns of Month and Year
loan$issue_d<- fast_strptime(loan$issue_d, format="%b-%y") #converting to proper format
loan$issue_d_month<- month(as.Date(loan$issue_d, format="%Y-%m-%d")) #Evaluating month from issue_d
loan$issue_d_year<- year(as.Date(loan$issue_d, format="%Y-%m-%d")) #Evaluating year from issue_d

#rounding off the other values for easy evaluation further
loan$funded_amnt_inv <- round(loan$funded_amnt_inv)
loan$installment <- round(loan$installment)
loan$annual_inc <- round(loan$annual_inc)
loan$dti <- round(loan$dti)
loan$total_pymnt <- round(loan$total_pymnt)
loan$total_pymnt_inv <- round(loan$total_pymnt_inv)
loan$total_rec_prncp <- round(loan$total_rec_prncp)
loan$total_rec_int <- round(loan$total_rec_int)
loan$last_pymnt_amnt <- round(loan$last_pymnt_amnt)

#----DATA CLEANSING COMPLETED----

#Loading the Loan DF to the CSV FILE to have a gleanse look of the data
write.csv(loan, "Changed_Loan_DF.csv")

#------univariate analysis----
library(ggplot2)
#home_ownership column
ggplot(loan, aes(x = loan_status)) + geom_bar()+
  ggtitle("Home_Ownership for charged off and fully paid")+geom_text(stat='count',aes(label=..count..),vjust=-1)
ggsave("count of home_ownership for charged off and fully paid.png")

#home ownership with loan statuses and the Charged off is high for Mortgage and Rent
ggplot(loan, aes(x = loan$loan_status)) + geom_bar(stat = "count") + facet_wrap(~ loan$home_ownership)+
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.25)+labs(title="Home ownership with loan statuses", x="Loan Status")
ggsave("Home ownership with loan statuses.png")

#considering charged_off as loan_status
charged_off <- subset(loan, loan$loan_status == "Charged Off")
nrow(charged_off)
#So out of 39717 loans granted 5627 were charged off


#Which Home_ownership has been more charged off
ggplot(charged_off, aes(x = home_ownership)) + geom_bar(stat = "count") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -1)+
  ggtitle("Home_ownership for Charged Off")
ggsave("Home_ownership for Charged Off.png")
#So the in charged of people 2839 people are rented and 2327 peoples houses are under mortgage

#Based on the Verification_Status, which has been more charged off
ggplot(charged_off, aes(x=verification_status)) + geom_bar(stat = "count") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("Verification_Status for Charged Off")
ggsave("Verification_Status for Charged Off.png")
#So the Verification_Status of Not verified has more charged off

#Based on the Addr_state, which has been more charged off
ggplot(charged_off, aes(x=addr_state))+geom_bar(stat="count")+
  geom_text(stat = 'count', aes(label=..count..), vjust=-1)+
  ggtitle("addr_state for Charged Off")
#CA has the Highest Numer for the Charged off

#Based on the term, which has been more charged off
ggplot(charged_off, aes(x=term))+geom_bar(stat = "count")+
  geom_text(stat = 'count', aes(label=..count..), vjust=-1)+
  ggtitle("term for charged off")
#36 months has been more charged of than 60 months Tenure.

#lets see the relation between grade and interest rate

ggplot(loan) + geom_bar(aes(loan$grade, loan$int_rate, fill = as.factor(loan$loan_status)), 
                        position = "dodge", stat = "summary", fun.y = "mean")+
  labs(title="Distribution of Grades and Interest Rates for various Loan Status", x="Loan Grade", y="Interest Rate")
ggsave("DIstribution of Grades and Interest Rates for various Loan Status.png")

#By observing The above graph,Based on the bins of the interest rate the grade has been decided.So lets analyse based on the Interest rate,
#Which has been charged off
ggplot(charged_off, aes(x=grade))+geom_bar(stat="count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("Grade for Charged off")
#Grade B has been more charged off

#Based on the purpose, which has been more charged off
ggplot(charged_off, aes(x=purpose))+geom_bar(stat="count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle(("Purpose For Charged Off"))
#debt consolidation has more charged off


#Lets see the loan_purpose And Loan_status in %
ggplot(loan,aes(x=purpose, col=loan_status))+geom_bar(position = "fill")+
 ggtitle("loan_purpose And Loan_status in percentage")
## For overall Population of loan we see that higher % of people with purpose as small business as charged off
#But we see that loan for Debt has higher %(around 2767 loans) among charged off

#---- Bivariate Analysis----
#want to find the Corelation between Loan_amt and funded_amt
plot(
  charged_off$loan_amnt,
  charged_off$funded_amnt_inv,
  main = "correlation between loan_amnt and funded_amnt_inv",
  xlab = "loan_amnt ",
  ylab = "funded_amnt_inv ",
  pch = 19
)
abline(lm(charged_off$loan_amnt ~ charged_off$funded_amnt_inv), col = "red") # regression line (y~x)
cor(loan$loan_amnt, loan$funded_amnt_inv)
#correlation is abot 0.95 for the above

#emp_length with Loan_status and Loan_term
ggplot(loan, aes(x = loan$emp_length)) + geom_bar(position = "fill", aes(fill =loan$loan_status)) + facet_wrap( ~ loan$term)+
  labs(title="Emp_length with Loan_status and Loan_term", x="Emplyment Tenure")


#The Emp_Length with 10+ Years has been more charged off for 36 Months Tenure and 
#Emp_Length with 7 Years has been more charged off for 60 Months Tenure

# Loan where montly installment is around 5-10% of monthly income on Mortgage/Rent are high defaulters
ggplot(charged_off,
       aes(x = charged_off$monthly_insatllment_inc)) + geom_bar(stat = "count")+labs(title="Count of Monthly Installment Inc Charged Off", x="Monthly Installment Inc")
ggsave("Count of Monthly Installment Inc Charged Off.png")

ggplot(charged_off,
       aes(x = charged_off$monthly_insatllment_inc)) + geom_bar(stat = "count", aes(fill =charged_off$emp_length)) +
  facet_wrap( ~ charged_off$home_ownership)+labs(title="Monthly Installment increment with Home ownership and Employee Length", x="Monthly Installment Increment")
ggsave("Monthly Installment increment with Home ownership and Employee Length.png")

ggplot(loan, aes(x = loan$addr_state, col = loan$loan_status)) + geom_bar(position = "fill")+
  labs(title="Percentage Of defaulters across all states", x="State", y="Percent of Loan Status")
ggsave("Percentage Of defaulters across all states.png")

ggplot(loan, aes(loan$int_rate))+geom_histogram(binwidth = 2)+labs(title="frequency of interest rates levied", x="interest rate", y="count")
ggsave("frequency of interest rates levied.png") #interest rates levied is mostly between 7 to 15%


ggplot(charged_off)+geom_histogram(aes(charged_off$installment),fill="#FF6666", binwidth = 50, color="black")+scale_x_continuous(breaks = round(seq(min(charged_off$installment), max(charged_off$installment), by = 50),0))+labs(title="group installment counts who are defaulted",x="Grouped Installment value", y="Count")
ggsave("group installment counts who are defaulted.png")
#From this we can analyze that most charge Offs are happening for customers whose installment ranges from 70+ to ~400 units.


ggplot(charged_off, aes(x=charged_off$sub_grade))+geom_histogram(stat="count")+labs(title="Frequency of Charged Off borrowers based on Sub-Grade", x="Sub Grade", y="Count of borrowers")
ggsave("Frequency of Charged Off borrowers based on Sub-Grade.png")
# From above plot, its pretty clear that, Charged Off scenarios are more frequent for sub grades B3 to C3 and D2 to E2.


ggplot(loan, aes(x=loan$loan_status, y=loan$annual_inc)) + geom_boxplot()+scale_y_log10()
#On plotting a log scale for annual income, we can see many outliers, hence removing these
#outliers and considering data unto 200000
loan_annualIncome<- subset(loan, loan$annual_inc<150000)

ggplot(loan_annualIncome, aes(x=loan_annualIncome$loan_status, y=loan_annualIncome$annual_inc))+geom_boxplot()+labs(title="loan_status vs annual income",x="loan status", y="range of income")
ggsave("loan_status vs annual income.png")


#Verification status per year
ggplot(loan, aes(x=loan$issue_d_year, fill=loan$verification_status))+geom_bar(stat="count")+labs(title="Verification status per year", x="Issue Year", y="Count of Verification status")
ggsave("Verification status per year.png")
#Based on above plot, its clear that in year 2010 and 2011, most borrowers were not verified, hence checking chargeOffs during these years.

#Loan Status per year
ggplot(loan, aes(x=loan$issue_d_year, fill=loan$loan_status))+geom_bar(stat="count")+labs(title="Loan Status per year", x="Issue Year", y="Counts of Loan Status")
ggsave("Loan Status per year.png")
#As predicted, chargeOff is mostly seen during 2011, hence verification of the borrowers is a prime cause.

#no of deliquent incidents year basis
ggplot(loan, aes(x=loan$issue_d_year,y=loan$delinq_2yrs))+geom_bar(stat="identity")+facet_wrap(~loan$loan_status, ncol = 3)+labs(title="no of deliquent incidents year basis", x="Loan Issued Year", y="No of Deliquent Incidents")
ggsave("no of deliquent incidents year basis.png")
#All charged off incidents occured in 2011

#---End Of Analysis---