#-- How has the family breadwinner role changed over time?
# look at sex, household, and who made the most money in that household: group by household to find max income and summarize sex
#-- How does state affect the relationship between work/life balance?
# (Factoring in time commuting to and from work.)

# SERIAL: household serial number
# HHWT: indicates how many households in the U.S. population are represented by a given household in an IPUMS
# CLUSTER: is designed for use with STRATA in Taylor series linear approximation for correction of complex sample design characteristics.See the STRATA variable description for more details.
# STRATA: is designed for use with CLUSTER in Taylor series linear approximation for correction of complex sample design characteristics.
# PERNUM: numbers all persons within each household consecutively in the order in which they appear on the original census or survey form. When combined with SAMPLE and SERIAL, PERNUM uniquely identifies each person within the IPUMS.
# OWNERSHP: indicates whether the housing unit was rented or owned by its inhabitants. Housing units acquired with a mortgage or other lending arrangement(s) are classified as "owned," even if repayment was not yet completed.
# MORTGAGE: indicates whether an owner-occupied housing unit was owned free and clear or was encumbered by a mortgage, loan, or other type of debt. (See also OWNERSHP.)
# VALUEH: reports the value of housing units in contemporary dollars. For 1930, 1940, and from 2008 onward, VALUEH is a continuous variable. The other years report the midpoint of an interval; see codes and frequencies for intervals.
# PERWT: indicates how many persons in the U.S. population are represented by a given person in an IPUMS sample.
# NCHILD: counts the number of own children (of any age or marital status) residing with each individual. NCHILD includes step-children and adopted children as well as biological children. Persons with no children present are coded "0."
# MARST: gives each person's current marital status.
# EMPSTAT: indicates whether the respondent was a part of the labor force -- working or seeking work -- and, if so, whether the person was currently unemployed. The second digit preserves additional related information available for some years but not others. See LABFORCE for a dichotomous variable that identifies whether a person participated in the labor force or not and is available for all years in the IPUMS.
# CLASSWKR: indicates whether respondents worked for their own enterprise(s) or for someone else as employees. Workers with multiple sources of employment were classified according to the work relationship in which they spent the most time during the reference day or week. As described below, CLASSWKR contains other related information in most years.
# UHRSWORK: reports the number of hours per week that the respondent usually worked, if the person worked during the previous year. The census inquiry relates to the previous calendar year, while the ACS and the PRCS uses the previous 12 months as the reference period.
# INCTOT: reports each respondent's total pre-tax personal income or losses from all sources for the previous year. The censuses collected information on income received from these sources during the previous calendar year; for the ACS and the PRCS, the reference period was the past 12 months. Amounts are expressed in contemporary dollars, and users studying change over time must adjust for inflation
# FTOTINC: reports the total pre-tax money income earned by one's family (as defined by FAMUNIT) from all sources for the previous year. For the census samples, the reference period is the previous calendar year; for the ACS/PRCS, it is the previous 12 months.
# PWSTATE2: reports the state in which the respondent's primary workplace was located. If the person worked abroad, this is also indicated. In some cases, the state is not identified, such as in the 1980 Puerto Rico census (see below).
# PWCITY: identifies the city in which the respondent worked in the week prior to the census. IPUMS constructs PWCITY from PWCNTYGP in 1980, from PWPUMA in 1990, and from PWPUMA00 in 2000 and in the 2005-2011 ACS samples. Differing confidentiality restrictions and coding procedures limit the identifiable cities over time. (See CITY for details on limitations in identifying cities.)
# TRANWORK: reports the respondent's primary means of transportation to work on the most recent day worked (1970), or over the course of the previous week (the 1960 and 1980-2000 censuses, the ACS, and the PRCS). The primary means of transportation was that used on the most days or to cover the greatest distance.
# TRANTIME: reports the total amount of time, in minutes, that it usually took the respondent to get from home to work last week.

install.packages('ipumsr')
library(ipumsr)
ddi <- read_ipums_ddi('usa_00002.xml')
fdata <- read_ipums_micro(ddi)
fdata
library(ggplot2)
library(tidyverse)
library(lubridate)



breadwinner <- fdata %>% group_by(SERIAL) %>%
  summarize(INCTOT, SEX, AGE, proportion = INCTOT/sum(INCTOT), YEAR) %>% group_by(SERIAL) %>% filter(proportion == max(proportion)) %>%
  summarize(SERIAL, SEX, YEAR, AGE, proportion)

male <- breadwinner %>% filter(SEX == 1) %>% mutate(AGE = as.integer(AGE)) %>%
  group_by(YEAR) %>% summarize(average_age = mean(AGE))

female <- breadwinner %>% filter(SEX == 2) %>% mutate(AGE = as.integer(AGE)) %>%
  group_by(YEAR) %>% summarize(average_age = mean(AGE))

together <- breadwinner %>% mutate(AGE = as.integer(AGE)) %>%
  group_by(YEAR) %>% summarize(average_age = mean(AGE))
  
per_year_sex <- breadwinner %>% group_by(YEAR, SEX) %>%
  summarize(totalinyear = n(), SEX, YEAR, AGE, proportion, SERIAL)

view(male)
view(female)
view(together)

ggplot(together, aes(x=YEAR, y= average_age)) + geom_line() + ggtitle('Average Age of Breadwinner Over Time') + ylab('Average Age')

total <- per_year_sex %>% select(YEAR, SEX, totalinyear) %>% distinct() %>% mutate(SEX = as.character(SEX)) %>% 
  mutate(SEX = as.factor(SEX))
total$SEX <- recode_factor(total$SEX, '1' = 'male', '2' = 'female' )

total %>% ggplot(aes(x=YEAR, weight=totalinyear, fill=SEX)) + geom_bar(position = 'dodge', width = 1/2) + 
  ylab('Breadwinners') + ggtitle('Number of Household Breadwinners Per Sex Each Year') + 
  scale_fill_manual(name="SEX", labels = c("Male","Female"), values=c('steelblue1','pink')) + theme_bw()

?scale_color_manual
#-----------------------------------------------------------------------------------------------------------------------


balance <- fdata %>% filter(UHRSWORK != 0) %>% mutate(both_ways = TRANTIME*2) %>% 
  group_by(PWSTATE2) %>% summarize(time_at_work = mean(UHRSWORK), time_to_work = mean(both_ways)) %>%
  mutate(State = as.integer(PWSTATE2))

lab <- ipums_val_labels(balance$PWSTATE2) %>% rename(State = val, state_name = lbl)

balance1 <- left_join(balance, lab, by = 'State') %>% select(-PWSTATE2) %>% group_by(State) %>%
  mutate(work = time_at_work + time_to_work) %>% mutate(life = 168 - work) %>%
  mutate(percent_of_week_in_work = work/168)

balance1$state_name <- tolower(balance1$state_name)

library(maps)
states <- map_data('state')
unique(states$region)
library(ggthemes)

# to get rid of the n/a (i'm assuming work from home) and areas not in US
# also to get rid of hawaii and alaska which isn't in the state df but will add that info into a table (missing)

balance2 <- balance1[c(2, 4:12, 14:52),] %>% rename(region = state_name)

balancemap <- balance2 %>% inner_join(states, by = 'region')

ggplot(balancemap, aes(x = long, y = lat, group=group, fill= percent_of_week_in_work)) + geom_polygon() +
  coord_map() + ggtitle('Average Percent of Week Spent in Work by State') + 
  scale_fill_distiller(palette = 'Blues', direction = 1) + theme_classic()

missing <- balance1 %>% filter(state_name == 'n/a' | state_name == 'alaska' | state_name == 'hawaii') %>%
  select(state_name, percent_of_week_in_work)

view(missing)

all_states <- balance1[c(2:52),]
average <- mean(all_states$percent_of_week_in_work)
average #(0.5023069)
all_states %>% filter(percent_of_week_in_work > average)

