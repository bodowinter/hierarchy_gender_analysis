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
## Accuracy analysis:
##------------------------------------------------------------------

## What is accurate? For the powerful experiment:

with(eng,
	powerful_correct <<- Task == 'powerful' & Resp == 'up' & HierarchyCond == 'TopPower')
with(eng,
	powerful_correct <<- powerful_correct | Task == 'powerful' &
		Resp == 'down' & HierarchyCond == 'BottomPower')
with(eng,
	powerless_correct <<- Task == 'powerless' & Resp == 'down' & HierarchyCond == 'TopPower')
with(eng,
	powerless_correct <<- powerless_correct | Task == 'powerless' &
		Resp == 'up' & HierarchyCond == 'BottomPower')

## Add this to the dataframe:

eng$ACC <- powerful_correct | powerless_correct

## Clean up:

rm(powerful_correct, powerless_correct)

## 





eng %>% group_by(TopGender, Button, HierarchyCondition) %>%
	summarise(ACC = round(mean(ACC), 2))



xmdl.ACC <- mixed(ACC ~ HierarchyCondition * Button * ExperimentName +
	(1 + Button + HierarchyCondition|SubID) +
	(1 + Button + HierarchyCondition|PairName),
	data = eng, method = 'LRT',
	family = 'binomial')

xmdl.RT <- mixed(LogRT ~ HierarchyCondition * Button * ExperimentName +
	(1 + Button + HierarchyCondition|SubID) +
	(1 + Button + HierarchyCondition|PairName),
	data = eng, method = 'LRT')


eng <- filter(eng, !incorrects)



with(eng, table(Button, ExperimentName, HierarchyCondition))




cond1 <- eng$GenderCondition == 'MaleTop' & eng$HierarchyCondition == 'TopPower'
cond2 <- eng$GenderCondition == 'MaleBottom' & eng$HierarchyCondition == 'BottomPower'
cond3 <- eng$GenderCondition == 'FemaleTop' & eng$HierarchyCondition == 'BottomPower'
cond4 <- eng$GenderCondition == 'FemaleBottom' & eng$HierarchyCondition == 'TopPower'

met.cong1 <- eng$Button == 'up' & eng$HierarchyCondition == 'TopPower'
met.cong2 <- eng$Button == 'down' & eng$HierarchyCondition == 'TopPower'


cond <- cond1 | cond2 | cond3 | cond4

eng$Congruency <- 'incongruent'
eng[cond, ]$Congruency <- 'congruent'

eng %>% print(width = Inf)

ggplot(eng,
	aes(x = c('HierarchyCondition', 'Button'), y = LogRT, fill = GenderCondition)) +
	geom_boxplot() + facet_wrap(~ExperimentName)

ggplot(eng,
	aes(x = Congruency, y = LogRT, fill = Button)) +
	geom_boxplot()


eng %>% group_by(Button) %>%
	summarise(RT = round(mean(MainScreen.RT), 0))
eng %>% group_by(HierarchyCondition) %>%
	summarise(RT = round(mean(MainScreen.RT), 0))
eng %>% group_by(ExperimentName) %>%
	summarise(RT = round(mean(MainScreen.RT), 0))
eng %>% group_by(TopGender) %>%
	summarise(RT = round(mean(MainScreen.RT), 0))


xmdl <- mixed(LogRT ~ TopGender +
	(1 + TopGender|SubID) + (1 + TopGender|PairName),
	data = eng, method = 'LRT')

xmdl.power <- mixed(LogRT ~ TopGender + HierarchyCondition + 
	(1 + TopGender + HierarchyCondition|SubID) +
	(1 + TopGender + HierarchyCondition|PairName),
	data = powerful, method = 'LRT')
xmdl.powerless <- mixed(LogRT ~ TopGender + HierarchyCondition + 
	(1 + TopGender + HierarchyCondition|SubID) +
	(1 + TopGender + HierarchyCondition|PairName),
	data = powerless, method = 'LRT')



powerful <- filter(eng,
	str_detect(ExperimentName, 'englishA'))
powerless <- filter(eng,
	str_detect(ExperimentName, 'englishB'))

eng %>% group_by(ExperimentName, HierarchyCondition) %>%
	summarise(RT = round(mean(MainScreen.RT), 0))

eng %>% group_by(Button, Congruency) %>%
	summarise(RT = round(mean(MainScreen.RT), 0))	


xmdl.exp <- mixed(LogRT ~ Button * Congruency +
	(1 + Congruency|SubID) + (1 + Button|PairName),
	data = eng, method = 'LRT')


xmdl.exp <- mixed(LogRT ~ ExperimentName * HierarchyCondition +
	(1 + HierarchyCondition|SubID) + (1 + ExperimentName|PairName),
	data = eng, method = 'LRT')



powerful %>% group_by(Button, HierarchyCondition) %>%
	summarise(RT = round(mean(MainScreen.RT), 0))
powerless %>% group_by(Button, TopGender) %>%
	summarise(RT = round(mean(MainScreen.RT), 0))

eng %>% group_by(Button, ExperimentName) %>%
	summarise(RT = round(mean(MainScreen.RT), 0))



eng %>%
	ggplot(aes(x = ExperimentName, y = LogRT, fill = Button)) +
		geom_boxplot()
	summarise(RT = round(mean(MainScreen.RT), 0))


xmdl.exp <- mixed(LogRT ~ ExperimentName +
	(1|SubID) + (1 + ExperimentName|PairName),
	data = eng, method = 'LRT')




eng %>% group_by(Congruency) %>%
	summarise(RT = round(mean(MainScreen.RT), 2))



eng %>% group_by(Button, HierarchyCondition) %>%
	summarise(LogRT = mean(LogRT))

eng %>% group_by(ExperimentName, Button, HierarchyCondition) %>%
	summarise(RT = mean(MainScreen.RT))
eng %>% group_by(Button, HierarchyCondition, ExperimentName) %>%
	summarise(LogRT = mean(LogRT))
	
eng %>% group_by(Button, TopGender) %>%
	summarise(RT = mean(MainScreen.RT))
eng %>% group_by(Button, TopGender) %>%
	summarise(LogRT = mean(LogRT))

eng %>% group_by(Button, TopGender) %>%
	summarise(RT = mean(MainScreen.RT))
eng %>% group_by(Button, TopGender) %>%
	summarise(LogRT = mean(LogRT))

eng %>% group_by(Gender, Congruency) %>%
	summarise(RT = mean(MainScreen.RT))

eng %>% group_by(Congruency) %>%
	summarise(RT = mean(MainScreen.RT))

library(afex)
xmdl.hier <- mixed(LogRT ~ HierarchyCondition +
	(1 + HierarchyCondition|SubID) + (1 + HierarchyCondition|PairName),
	data = eng, method = 'LRT')
xmdl.cong <- mixed(LogRT ~ Congruency +
	(1 + Congruency|SubID) + (1 + Congruency|PairName),
	data = eng, method = 'LRT')
xmdl.button <- mixed(LogRT ~ Button +
	(1 + Button|SubID) + (1 + Button|PairName),
	data = eng, method = 'LRT')



summary(xmdl <- lmer(LogRT ~ 
	ExperimentName:Button:HierarchyCondition + 
	(1 + Button + HierarchyCondition|SubID) +
	(1 + Button + HierarchyCondition|PairName),
	data = eng))




xmdl.button <- mixed(LogRT ~ Button + HierarchyCondition + ExperimentName +
	Button:HierarchyCondition + 
	(1 + Button + HierarchyCondition|SubID) +
	(1 + Button + HierarchyCondition|PairName),
	data = eng, method = 'LRT')



xmdl.button2 <- mixed(LogRT ~ Button * HierarchyCondition * ExperimentName +
	TopGender + TopGender:Button + TopGender:HierarchyCondition +
	TopGender:HierarchyCondition:Button + 
	(1 + Button + HierarchyCondition|SubID) +
	(1 + Button + HierarchyCondition|PairName),
	data = eng, method = 'LRT')




