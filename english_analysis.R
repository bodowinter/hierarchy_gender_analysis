## Bodo Winter
## March 2, 2017
## First analysis of hierarchy data

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Load libraries:

library(tidyverse)
library(lme4)
library(afex)
library(stringr)
library(ggplot2)

## Preliminaries:

setwd('/Users/winterb/Research/power_sarah_hierarchy/new_analysis/')
eng <- read_csv('english_data.csv')

## Get rid of unwanted columns:

eng <- eng %>%
	select(-(Session:ExperimentVersion),
		-Group, -RandomSeed,
		-(RandomSeed:Block),
		-(list1:list9),
		-ComprehensionQuestion.RESP,
		-ComprehensionQuestion.RT,
		-(MainConditionTable:MainList),
		-MainQuestion.RTTime,
		-MainScreen.RTTime,
		-(PracticeDisplayTime:Procedure),
		-(QuestionDisplayTime:TopProfession))

## Get rid of practice trials:

eng <- eng %>%
	filter(!is.na(MainScreen.RESP))
	
## Rename columns:

eng <- eng %>%
	rename(Task = ExperimentName,
		Subj = Subject,
		SubjGender = Gender,
		BottomProf = Bottomprofession,
		Comp.RESP = MainQuestion.RESP,
		Comp.RT = MainQuestion.RT,
		Country = HomeCountry,
		Language = NativeLanguage,
		GenderCond = GenderCondition,
		HierarchyCond = HierarchyCondition,
		Resp = MainScreen.RESP,
		RT = MainScreen.RT)

## Rename content of all columns that contain responses:

up_function <- function(x) ifelse(x == '{UPARROW}', 'up', 'down')
eng <- eng %>%
	mutate(Comp.RESP = up_function(Comp.RESP),
		Resp = up_function(Resp))
rm(up_function)

## Rename task column:
## A = which one is more powerful?
## B = which one is less powerful?

eng <- eng %>%
	mutate(Task = ifelse(Task == 'professions_englishA', 'powerful', 'powerless'))

## Create a unique condition column:

eng <- eng %>% 
	mutate(Subj = gl(nrow(eng) / 128, 128))

## Log transform RTs:

eng <- eng %>%
	mutate(LogRT = log(RT),
		LogComp.RT = ifelse(Comp.RT != 0, log(Comp.RT), NA))

## Exclude non-native speakers and lefties (not enough anyway):
		# perhaps check the "Country" column at some point:

eng <- eng %>%
	filter(str_detect(Language, 'english|English'),
		Handedness == 'right')

## Get rid of those columns (not needed now anymore):

eng <- eng %>%
	select(-Language, -Handedness, -Country)



##------------------------------------------------------------------
## Comprehension question analysis:
##------------------------------------------------------------------

## Find correct repsonses to female question:

with(eng,
	female_up <<- Question == 'Was the female at the top or bottom?')
with(eng,
	female_up <<- female_up & GenderCond == 'FemaleTop' & Comp.RESP == 'up')
with(eng,
	female_down <<- Question == 'Was the female at the top or bottom?')
with(eng,
	female_down <<- female_down & GenderCond == 'MaleTop' & Comp.RESP == 'down')
female_corr <- female_up | female_down

## Find correct repsonses to male question:

with(eng,
	male_up <<- Question == 'Was the male at the top or bottom?')
with(eng,
	male_up <<- male_up & GenderCond == 'MaleTop' & Comp.RESP == 'up')
with(eng,
	male_down <<- Question == 'Was the male at the top or bottom?')
with(eng,
	male_down <<- male_down & GenderCond == 'FemaleTop' & Comp.RESP == 'down')
male_corr <- male_up | male_down

## Combine and attach to data frame:

comp_corrects <- female_corr | male_corr
eng$Comp.ACC <- comp_corrects

## Clean up:

rm(comp_corrects, male_corr, female_corr, male_up, male_down,
	female_up, female_down)

## Get a dataframe of only the comprehension question responses:

comps <- eng %>%
	filter(!is.na(Comp.ACC))

## Get accuracies per subject:

comps %>% group_by(Subj) %>%
	summarize(ACC = mean(Comp.ACC)) %>%
	print(n = length(unique(eng$Subj)))
	# subject 3 performed with 79%

## Check accuracies by nature of question:

comps %>% group_by(GenderCond, Comp.RESP) %>%
	summarize(ACC = mean(Comp.ACC))

## For analysis contrast code predictors:

comps <- comps %>%
	mutate(GenderCond_c = as.factor(GenderCond),
		Comp.RESP_c = as.factor(Comp.RESP))
contrasts(comps$Comp.RESP_c) <- contr.sum(2)
contrasts(comps$GenderCond_c) <- contr.sum(2)

## Make an analysis of this:

comps.afex <- mixed(Comp.ACC ~ GenderCond_c * Comp.RESP_c +
	(1 + Comp.RESP_c + GenderCond_c|Subj) + (1|PairName),
	data = comps, family = 'binomial', method = 'LRT',
	control = glmerControl(optimizer = 'bobyqa'))
	# no interactions with condition

## Now we can get rid of the question and question RT stuff:

eng <- eng %>%
	select(-Comp.RESP, -Comp.RT, -Question, -Comp.ACC)


##------------------------------------------------------------------
## Check some basic stuff of the analysis:
##------------------------------------------------------------------

## How many men and women?

eng %>% count(SubjGender) %>% mutate(n = n / 128)
	# only four men, exclude?

## Exclude the men and then exclude the gender column:

eng <- eng %>%
	filter(SubjGender == 'female') %>%
		select(-SubjGender)
		# so I am proceeding with 29 subjects



##------------------------------------------------------------------
## Accuracy analysis:
##------------------------------------------------------------------

## What is accurate? For the powerful experiment:

with(eng,
	powerful_correct <<- Task == 'powerful' &
		Resp == 'up' &
		HierarchyCond == 'TopPower')
with(eng,
	powerful_correct <<- powerful_correct | Task == 'powerful' &
		Resp == 'down' & HierarchyCond == 'BottomPower')
with(eng,
	powerless_correct <<- Task == 'powerless' &
		Resp == 'down' &
		HierarchyCond == 'TopPower')
with(eng,
	powerless_correct <<- powerless_correct | Task == 'powerless' &
		Resp == 'up' & HierarchyCond == 'BottomPower')

## Add this to the dataframe:

eng$ACC <- powerful_correct | powerless_correct

## Clean up:

rm(powerful_correct, powerless_correct)

## Analyze this:

ACC.afex <- mixed(ACC ~ (HierarchyCond + GenderCond) * Task +
	(1|Subj) + (1|ItemNo),
	data = eng, family = 'binomial', method = 'LRT',
	control = glmerControl(optimizer = 'bobyqa'))
	# look at GenderCond:Task

## Check inaccurates by subject:

eng %>% group_by(Subj) %>%
	summarize(ACC = mean(ACC)) %>% print(n = length(unique(eng$Subj)))
	# all over 80 percent, no subjects to be excldued

## How many inaccurates?

(1 - (sum(eng$ACC) / nrow(eng))) * 100		# 5.7 %

## Exclude inaccurates:

eng <- eng %>%
	filter(ACC == 1)



##------------------------------------------------------------------
## RT cleaning:
##------------------------------------------------------------------

## Check RT distribution:

ggplot(eng, aes(x = RT)) + geom_density(fill = 'steelblue')
	## WOOOOOAH!

## Check RT by subject:

eng %>% group_by(Subj) %>%
	summarize(RT = mean(RT)) %>% print(n = length(unique(eng$Subj)))
	# some vast variation

## Exclude responses over 5000:

eng <- eng %>%
	filter(RT < 5000)

## Exclude within subjects:

these_subs <- unique(eng$Subj)
new_df <- c()
sd_fac <- 2.5
for (i in seq_along(these_subs)) {
	this_df <- filter(eng, Subj == these_subs[i])
	rts <- this_df$LogRT
	UB <- mean(rts) + sd_fac * sd(rts)
	LB <- mean(rts) - sd_fac * sd(rts)
	this_df <- filter(this_df,
		!(rts > UB | rts < LB))
	new_df <- rbind(new_df, this_df)
	}
rm(i, LB, rts, these_subs, this_df, UB)

## Compare:

(1 - (nrow(new_df) / nrow(eng))) * 100




##------------------------------------------------------------------
## Analyze results:
##------------------------------------------------------------------

## Create a factor for hierarchy/gender congruency (male = powerful):

with(new_df,
	male_power <<- HierarchyCond == 'TopPower' & GenderCond == 'MaleTop')
with(new_df,
	male_power <<- male_power | (HierarchyCond == 'BottomPower' & GenderCond == 'FemaleTop'))
new_df$GenderCongruent <- male_power
rm(male_power)

## Contrast code all predictors:

new_df <- mutate(new_df,
	GenderCond_c = as.factor(GenderCond),
	Task_c = as.factor(Task),
	HierarchyCond_c = as.factor(HierarchyCond),
	Resp_c = as.factor(Resp),
	GenderCongruent_c = as.factor(GenderCongruent))
contrasts(new_df$GenderCond_c) <- contr.sum(2)
contrasts(new_df$Task_c) <- contr.sum(2)
contrasts(new_df$HierarchyCond_c) <- contr.sum(2)
contrasts(new_df$Resp_c) <- contr.sum(2)
contrasts(new_df$GenderCongruent_c) <- contr.sum(2)

## Make a model of this:

summary(xmdl <- lmer(LogRT ~ GenderCond_c + Task_c +
	HierarchyCond_c + Task_c:HierarchyCond_c +
	GenderCond_c:HierarchyCond_c + Task_c:GenderCond_c +
	(1 + HierarchyCond_c + GenderCond_c|Subj) + (1 + Task_c|ItemNo),
	# (1 + GenderCond_c + HierarchyCond_c|Subj) +
	# (1 + GenderCond_c + Task_c + HierarchyCond_c|ItemNo),
		data = new_df))


summary(xmdl2 <- lmer(LogRT ~ GenderCongruent_c + Task_c +
	Task_c:GenderCongruent_c +
	(1 + GenderCongruent_c|Subj) + (1 + Task_c|ItemNo),
	# (1 + GenderCond_c + HierarchyCond_c|Subj) +
	# (1 + GenderCond_c + Task_c + HierarchyCond_c|ItemNo),
		data = new_df))

## Simple binary comparisons:

new_df %>%
	group_by(Task) %>%
	summarise(RT = round(mean(RT), -1))

new_df %>%
	group_by(GenderCongruent) %>%
	summarise(RT = round(mean(RT), -1))

new_df %>%
	group_by(HierarchyCond) %>%
	summarise(RT = round(mean(RT), -1))

new_df %>%
	group_by(GenderCond) %>%
	summarise(RT = round(mean(RT), -1))

## Binary comparisons:

new_df %>%
	group_by(Task, HierarchyCond) %>%
	summarise(RT = round(mean(RT), -1))

new_df %>%
	group_by(Task, Resp, HierarchyCond) %>%
	summarise(RT = round(mean(RT), -1))

new_df %>%
	group_by(Task, GenderCond) %>%
	summarise(RT = round(mean(RT), -1))

new_df %>%
	group_by(Task, Resp, GenderCond) %>%
	summarise(RT = round(mean(RT), -1))

new_df %>%
	group_by(Task, GenderCongruent) %>%
	summarise(RT = round(mean(RT), -1))


