knitr::opts_chunk$set(echo = TRUE)
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(Rmisc))
suppressPackageStartupMessages(library(ez))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))
options(scipen=999)



df=read.csv("C:/Users/skrjanec/Downloads/Plausibility_C_Results",
            header=FALSE,
            comment.char="#",
            encoding="UTF-8",
            col.names=c("Time", "IP_hash", "Ibex_Controller", "Ibex_Item", "Ibex_Element",
                        "Cond", "Item", "Question", "Answer", "Correct", "RT"),
            fill=TRUE)

summary(df)
demogs = df[df$Cond == "Consent", ]
ages=as.numeric(as.character(demogs[(demogs$Question=="age"), "Answer"]))
genders=demogs[(demogs$Question=="sex"), ]
summary(as.numeric(as.character(ages)))
summary(droplevels(as.factor(genders$Answer)))
ratings=df[(df$Ibex_Controller == "Question" & df$Cond != "practice"), ]

ratings$Item=as.numeric(as.character(ratings$Item))
ratings$Rating=as.numeric(as.character(ratings$Answer))
ratings=droplevels(ratings)

summary(ratings)

xtabs(~ IP_hash + Cond, ratings)
xtabs(~ IP_hash + Item, ratings)
xtabs(~ Item + Cond, ratings)

means.byItem = aggregate(Rating ~ Item + Cond + Question, data=ratings, FUN=mean, na.rm=T)
means.byItem=means.byItem[with(means.byItem, order(means.byItem$Item, means.byItem$Cond)), ]
head(means.byItem)

meansSE.byItem = summarySE(ratings, measurevar="Rating", groupvars=c("Item", "Cond", "Question"), na.rm=T)
tail(meansSE.byItem)
summary(ratings)
summary(ratings$RT)

ratings_before_removal = ratings

# this paper show that is several languages, the average reading speed is 68 milisecond per character
# https://www.researchgate.net/publication/245023269_Standardized_Assessment_of_Reading_Performance_The_New_International_Reading_Speed_Texts_IReST 
# on average the items have 107 characters, 78.24 * 68 = 5320.32. We will take 2,000 ms as the lower bound, and 10,000 as the upper bound

head(ratings[ratings$RT<2000,])
length(ratings$RT[ratings$RT<2000])  # 228 responses (not items!) have a RT lower than 2000 ms
ratings=ratings[ratings$RT>2000,]
head(ratings[ratings$RT>10000,])
length(ratings$RT[ratings$RT>10000])  # 450 responses have a RT higher than 10k ms
ratings=ratings[ratings$RT<10000,]

# now check by participants: some might be generally slower/faster, which is okay. But within a participant there might be extreme responses that
# are atypical of this participant
summarySE(ratings, measurevar="RT", groupvars=c("IP_hash"))
summarySE(ratings_before_removal, measurevar="RT", groupvars=c("IP_hash"))

# We should decide on a threshold for the removal of
# participants with to little data points. While it is very clear here, for a simple experiment as this, setting
# a threshold of 80-90% seems reasonable. (As in, participants with less data points than that threshold are
#                                         removed)
#
# Each subject has 79 responses. 85% of 79 is 67.15, or 68 as an integer rounded up. 68 is the threshold for the minimum number of responses
0.85*79

meansSE.RT = summarySE(ratings, measurevar="RT", groupvars=c("IP_hash"), na.rm=T)
meansSE.RT
nrow(meansSE.RT[meansSE.RT$N<68,]) # 17 participants have lost 15% or more of their responses in the above cleaning for RT
# these participants are: 

# "094cc6621a6d029779299532d0b23b6e" 
# "234d8709103ca5d6b0523e93e7d9ec2e" 
# "33e68e689df8d65f4e98fd4f12c965b6" 
# "49a10b213190a84c67f3e21212a6dbc8"
# "49dbaf3d29cc9217609a3a5643dcb792" 
# "651f3b4f3eb7e7e4a2ac6f447a270b65" 
# "6d0a8cc9e66b9a44f70bb514d90a1a79" 
# "7085ef35b9be88ab300c73140b5727b4"
# "7b18d43509e8d97b3ba40feb6420961b" 
# "9454d2bd7de1deb89968b2d8fda1c0b7" 
# "bfd27a6dd1972701dcce0d6a5aed0300" 
# "e162a24497fbc9bbc309e7c255addcc2"
# "e2083c91df8d39dd5c665ebf96af4e61" 
# "ec9b61f691292c26279d508c09c20199" 
# "f2cbec2aa67f93793934b9945591fdec" 
# "f9bd414b5492750c5a9df67f625989a8"
# "fac53fd73476a77979332bf2910833a4"

ratings=ratings[ratings$IP_hash!="094cc6621a6d029779299532d0b23b6e",]
ratings=ratings[ratings$IP_hash!="234d8709103ca5d6b0523e93e7d9ec2e",]
ratings=ratings[ratings$IP_hash!="33e68e689df8d65f4e98fd4f12c965b6",]
ratings=ratings[ratings$IP_hash!="49a10b213190a84c67f3e21212a6dbc8",]
ratings=ratings[ratings$IP_hash!="49dbaf3d29cc9217609a3a5643dcb792",]
ratings=ratings[ratings$IP_hash!="651f3b4f3eb7e7e4a2ac6f447a270b65",]
ratings=ratings[ratings$IP_hash!="6d0a8cc9e66b9a44f70bb514d90a1a79",]
ratings=ratings[ratings$IP_hash!="7085ef35b9be88ab300c73140b5727b4",]
ratings=ratings[ratings$IP_hash!="7b18d43509e8d97b3ba40feb6420961b",]
ratings=ratings[ratings$IP_hash!="9454d2bd7de1deb89968b2d8fda1c0b7",]
ratings=ratings[ratings$IP_hash!="bfd27a6dd1972701dcce0d6a5aed0300",]
ratings=ratings[ratings$IP_hash!="e162a24497fbc9bbc309e7c255addcc2",]
ratings=ratings[ratings$IP_hash!="e2083c91df8d39dd5c665ebf96af4e61",]
ratings=ratings[ratings$IP_hash!="ec9b61f691292c26279d508c09c20199",]
ratings=ratings[ratings$IP_hash!="f2cbec2aa67f93793934b9945591fdec",]
ratings=ratings[ratings$IP_hash!="f9bd414b5492750c5a9df67f625989a8",]
ratings=ratings[ratings$IP_hash!="fac53fd73476a77979332bf2910833a4",]


# check the count of participants with less than 68 responses now. It should be 0.
meansSE.RT = summarySE(ratings, measurevar="RT", groupvars=c("IP_hash"), na.rm=T)
nrow(meansSE.RT[meansSE.RT$N<68,])


# We compute their mean RTs and add the Standard 
# Deviations from those RTs for each of them. With this information we can compute the upper and lower
# bounds of reasonable RTs for each participant:
updown = meansSE.RT
updown$upper = updown$RT+(2*updown$sd)
updown$lower = updown$RT-(2*updown$sd)

# We now have to add these upper and lower bounds to the full data frame by merging them. Then we check
#how many data points we loose based on the SD approach
only_upper_lower = updown[, c("IP_hash", "upper", "lower")]
cleanratings = merge(ratings,only_upper_lower, by="IP_hash")


length(cleanratings[(cleanratings$RT<cleanratings$lower),]$Item) # 2 responses have a lower RT than the lower bound of their authors (subjects) allows
length(cleanratings[(cleanratings$RT>cleanratings$upper),]$Item) # 161 responses have a higher RT than the upper bound of their authors (subjects) allows
cleanratings = cleanratings[(cleanratings$RT>=cleanratings$lower),] 
cleanratings = cleanratings[(cleanratings$RT<=cleanratings$upper),]

summarySE(cleanratings, measurevar="Rating", groupvars=c("IP_hash"), na.rm=T)

# check again whether there are subjects below a 85% threshold, so they should have at least 68 responses

meansSE.RT2 = summarySE(cleanratings, measurevar="Rating", groupvars=c("IP_hash"), na.rm=T)
nrow(meansSE.RT2[meansSE.RT2$N<68,])  # 8 participants have less than 68 responses, so remove them

# IP_hash  N   Rating        sd        se        ci
# 2  09b08988207eadc1cf3c8d0c8749ba67 67 4.223881 2.3601547 0.2883388 0.5756870
# 9  2a5e1366de6a1ea5e429c73bbfbb86d8 67 3.537313 2.3952501 0.2926264 0.5842474
# 12 3c894d558f2df321e23e711d441030ba 66 3.742424 2.644650 0.3255339 0.6501361

# 16 45713910b6fb6d572b52577804515142 67 3.388060 2.3352063 0.2852909 0.5696016
# 27 99cb3074a504f5ed4f3ad4ce1e6b8445 65 2.272727 2.3893704 0.2941112 0.5873806

# 41 ce06c3a52e6b7c845666b55d128acb87 67 3.373134 2.5574361 0.3124405 0.6238077
# 42 dd4c344ab9525e475ba3cb6e09e4cffb 67 4.000000 2.4370872 0.2977376 0.5944523
# 44 f5353da1f1ff599fe72098c5c57d6978 66 4.257576 2.0328881 0.2502312 0.4997463

cleanratings=cleanratings[cleanratings$IP_hash!="09b08988207eadc1cf3c8d0c8749ba67",]
cleanratings=cleanratings[cleanratings$IP_hash!="2a5e1366de6a1ea5e429c73bbfbb86d8",]
cleanratings=cleanratings[cleanratings$IP_hash!="3c894d558f2df321e23e711d441030ba",]
cleanratings=cleanratings[cleanratings$IP_hash!="45713910b6fb6d572b52577804515142",]
cleanratings=cleanratings[cleanratings$IP_hash!="99cb3074a504f5ed4f3ad4ce1e6b8445",]
cleanratings=cleanratings[cleanratings$IP_hash!="ce06c3a52e6b7c845666b55d128acb87",]
cleanratings=cleanratings[cleanratings$IP_hash!="dd4c344ab9525e475ba3cb6e09e4cffb",]
cleanratings=cleanratings[cleanratings$IP_hash!="f5353da1f1ff599fe72098c5c57d6978",]

meansSE.Rating2 = summarySE(cleanratings, measurevar="Rating", groupvars=c("IP_hash"), na.rm=T)
nrow(meansSE.Rating2[meansSE.Rating2$N<68,]) # now 0

cleanratings=droplevels(cleanratings)
summary(cleanratings)

# Now check how many participants and how many responses we are left with after finishing the data cleaning
final_nparticipants = nrow(meansSE.Rating2)  # 39
final_nresponses = sum(meansSE.Rating2$N)  # 2,812

meansSE.Ratings_before_cleaning = summarySE(ratings_before_removal, , measurevar="Rating", groupvars=c("IP_hash"), na.rm=T)
first_nparticipants = nrow(meansSE.Ratings_before_cleaning)  # 64
first_nresponses = sum(meansSE.Ratings_before_cleaning$N)  # 5,056

# percentage of participants and responses removed
100 * (first_nparticipants - final_nparticipants) / first_nparticipants  # 39.06 %
100 * (first_nresponses - final_nresponses) / first_nresponses  # 44.38 %

# Create new columns in the dataframe for each of the factors
new_cleanratings = cleanratings


# Plausibility
new_cleanratings <- new_cleanratings %>%
        mutate(Plausibility = case_when(
                endsWith(Cond, "uic") ~ "implausible", # implausible
                endsWith(Cond, "ric") ~ "implausible", 
                endsWith(Cond, "rinc") ~ "implausible",
                endsWith(Cond, "uinc") ~ "implausible", 
                endsWith(Cond, "rpnc") ~ "plausible", # plausible
                endsWith(Cond, "upnc") ~ "plausible", 
                endsWith(Cond, "rpc") ~ "plausible", 
                endsWith(Cond, "upc") ~ "plausible" 
        ))

# Connector
new_cleanratings <- new_cleanratings %>%
        mutate(Connector = case_when(
                endsWith(Cond, "uic") ~ "causal", # causal
                endsWith(Cond, "ric") ~ "causal", 
                endsWith(Cond, "rinc") ~ "non-causal",
                endsWith(Cond, "uinc") ~ "non-causal", # non-causal
                endsWith(Cond, "rpnc") ~ "non-causal", 
                endsWith(Cond, "upnc") ~ "non-causal", 
                endsWith(Cond, "rpc") ~ "causal", 
                endsWith(Cond, "upc") ~ "causal" 
        ))

# Association
new_cleanratings <- new_cleanratings %>%
        mutate(Association = case_when(
                endsWith(Cond, "uic") ~ "unassociated", # unrelated
                endsWith(Cond, "ric") ~ "associated", # related
                endsWith(Cond, "rinc") ~ "associated",
                endsWith(Cond, "uinc") ~ "unassociated", 
                endsWith(Cond, "rpnc") ~ "associated", 
                endsWith(Cond, "upnc") ~ "unassociated", 
                endsWith(Cond, "rpc") ~ "associated", 
                endsWith(Cond, "upc") ~ "unassociated" 
        ))

# check
head(new_cleanratings)


### ANOVA ###
'ezANOVA(cleanratings,
        dv = .(Rating),
        wid = .(IP_hash),
        within = .(Cond))

ezANOVA(cleanratings,
        dv = .(Rating),
        wid = .(Item),
        within = .(Cond))
'

### CONTRAST CODING ####

contrasts.Matrix = matrix(c(1/2, 1/2, # Intercept
                            -1, 1), # Comparison
                          ncol = 2) # number of columns of the matrix (N)
transposed.Matrix = t(contrasts.Matrix)
solved.Matrix = solve(transposed.Matrix)
solved.Matrix

contrasts.Bi = solved.Matrix[,2]
contrasts.Bi


# code the IV - plausibility, association, connector
new_cleanratings$Plausibility = as.factor(new_cleanratings$Plausibility)
new_cleanratings$Association = as.factor(new_cleanratings$Association)
new_cleanratings$Connector = as.factor(new_cleanratings$Connector)

contrasts(new_cleanratings$Plausibility) <- contrasts.Bi # IV
contrasts(new_cleanratings$Connector) <- contrasts.Bi # IV
contrasts(new_cleanratings$Association) <- contrasts.Bi # IV

# check the created contrasts and pay attention to the signs
contrasts(new_cleanratings$Plausibility)
contrasts(new_cleanratings$Connector)
contrasts(new_cleanratings$Association)

# switch the signs for Association since we expect that items with associated words will get a higher rating
new_cleanratings$Association = factor(new_cleanratings$Association,levels(new_cleanratings$Association)[c(2,1)])
contrasts(new_cleanratings$Association) <- contrasts.Bi # IV
contrasts(new_cleanratings$Association)

# switch the signs for Connector as well, though we expect to observe less of a difference between the two levels
new_cleanratings$Connector = factor(new_cleanratings$Connector,levels(new_cleanratings$Connector)[c(2,1)])
contrasts(new_cleanratings$Connector) <- contrasts.Bi # IV
contrasts(new_cleanratings$Connector)

new_cleanratings$Answer2 <- as.numeric(as.character(new_cleanratings$Answer))


# Helpful: https://psyteachr.github.io/ug3-stats/linear-mixed-effects-models-with-one-random-factor.html (when explaining interaction in random eff.)
# Also: https://ourcodingclub.github.io/tutorials/mixed-models/ 

### defining the LME model ###
# the model includes the main effects of Association, Plausibility and Connector, as well as their interactions
# the random effect structure also considers these given the participants and items
Model0 = lmer(Answer2 ~ 1 + Association*Plausibility*Connector + (1 + Association*Plausibility*Connector |IP_hash) + (1 + Association*Plausibility*Connector|Item),
              REML=FALSE, data=new_cleanratings,
              control = lmerControl(calc.derivs=FALSE),
              na.action = na.omit)
summary(Model0)

# Let's focus on the important points from the output:
#        Estimate: The difference between the two sides of the comparison. For the Intercept, that is the grand mean.
# A significant Intercept simply means that the grand mean is different from 0. The sign of the Estimate tells
# us which side of the comparison has the higher ratings. Here, that means that, statistically, the difference in
# ratings of the items is 3.87, meaning that related words are rated 3.87 points higher than unrelated words.
# t-value: the actual statistical value. Usually, |t|>2 is considered to be reliable (significant).
# Pr(>|t|): This is the p value many know. For LMMs thats actually not really telling as the t value is enough
# information. But many more conservative researchers still want the p-value to be mentioned and some
# journals will question you if you don't add it. . .

coef(summary(Model0))

### PLOTTING ###
meansSE.Condition = summarySE(new_cleanratings, measurevar="Rating", groupvars=c("Cond", "Plausibility", "Association", "Connector"), na.rm=T)


meansSE.Condition <- meansSE.Condition %>%
        mutate(Condition = case_when(
                endsWith(Cond, "C_ric") ~ "Implausible Associated Causal", # unrelated
                endsWith(Cond, "C_rinc") ~ "Implausible Associated Non-Causal", # related
                endsWith(Cond, "C_rpc") ~ "Plausible Associated Causal",
                endsWith(Cond, "C_rpnc") ~ "Plausible Associated Non-Causal", 
                endsWith(Cond, "C_uic") ~ "Implausible Unassociated Causal", 
                endsWith(Cond, "C_uinc") ~ "Implausible Unassociated Non-Causal", 
                endsWith(Cond, "C_upc") ~ "Plausible Unassociated Causal", 
                endsWith(Cond, "C_upnc") ~ "Plausible Unassociated Non-Causal" 
        ))




#install.packages("ggpubr")
library(ggpubr)

# plot the mean ratings
# PLOT 1: ratings given the plausibility
b1 = ggplot(data=meansSE.Condition, aes(x=Plausibility, y=Rating, fill=Condition)) +
        geom_bar(stat="identity", position=position_dodge()) + 
        geom_errorbar(aes(ymin=Rating-sd, ymax=Rating+sd),
                      width=.2, position=position_dodge(.9)) + 
        theme(legend.position="none")


# PLOT 2: ratings given the association
b2 = ggplot(data=meansSE.Condition, aes(x=Association, y=Rating, fill=Condition)) +
        geom_bar(stat="identity", position=position_dodge()) + 
        geom_errorbar(aes(ymin=Rating-sd, ymax=Rating+sd),
                      width=.2, position=position_dodge(.9)) + 
        theme(legend.position="none")
       

# PLOT 3: ratings given the connector
b3 = ggplot(data=meansSE.Condition, aes(x=Connector, y=Rating, fill=Condition)) +
        geom_bar(stat="identity", position=position_dodge()) + 
        geom_errorbar(aes(ymin=Rating-sd, ymax=Rating+sd),
                      width=.2, position=position_dodge(.9)) + 
        theme(legend.position="none")


ggarrange(b1, b2, b3, ncol=1, nrow=3, common.legend = TRUE, legend="right")


#multiplot(b1, b3, b2, cols=2)  

'        scale_fill_discrete(name="Condition",
                            breaks=c("C_ric", "C_rinc", "C_rpc", "C_rpnc", "C_uic", "C_uinc", "C_upc", "C_upnc"),
                            labels=c("Implausible Associated Causal", "Implausible Associated Non-Causal", "Plausible Associated Causal", 
                                     "Plausible Associated Non-Causal", "Implausible Unassociated Causal", "Implausible Unassociated Non-Causal",
                                     "Plausible Unassociated Causal", "Plausible Unassociated Non-Causal")) + 
        theme(legend.position="right", legend.margin=margin(30,35,35,35), legend.box.margin=margin(0,0,0,0),
              legend.text = element_text(size = 10)
        )'


# SOME MORE NICE PLOTS: not with means, but all data points

ggplot(data = new_cleanratings) +
        aes(x = Rating, y = Plausibility, color = Connector, shape = Association) + 
        geom_jitter()
        coord_flip()

ggplot(data = new_cleanratings) +
        aes(x = Rating, y = Plausibility, color = Connector) + 
        geom_jitter()
        coord_flip()
 
ggplot(data = new_cleanratings) +
        aes(x = Rating, y = Connector, color = Association) + 
        geom_jitter()
        coord_flip()      
        
ggplot(data = new_cleanratings) +
       aes(x = Rating, y = Association, color = Connector) + 
       geom_jitter()
       coord_flip()
       

 ### Posthoc testing ###
install.packages("multcomp")
library(multcomp)
summary(glht(Model0, linfct = mcp(Connector = "Tukey", Plausibility="Tukey", Association="Tukey")), test = adjusted("holm"))

