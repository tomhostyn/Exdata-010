library (dplyr)
library (ggplot2)
library (lubridate)

# define tags
kTags <- c("happy", "sad", "excited", "bored", "cool", "warm", "go", "stay")

#define probability of tag occuring in a new post
if (!exists ("kNewProb")){
  kNewProb <- runif (length (kTags))
  kNewProb <- kNewProb/sum(kNewProb)
}

#define probability of tag occuring in a chained post
if (!exists ("kReactProb")){
  kReactProb <- runif (length (kTags))
  kReactProb <- kReactProb/sum(kReactProb)
}

# ratio new tag/total tags
kNewTagRatio <- 1/5



GenerateUsers <- function (num.users) {

  # generate Humans
  
  userID <- 1:num.users
  
  # women are nerds in this universe
  gender <- sample(c("Male", "Female"), num.users, replace=TRUE, prob = c(0.40, 0.60)) 
  
  # pick age using normal distribution around 30, but make sure we have a peak at 20
  age <- abs (c(rnorm (round(num.users * 0.7), mean= 30, sd = 10),
              rnorm (num.users - round(num.users * 0.7), mean= 20, sd = 3)))
  
  enthusiasm <- runif (num.users)
  
  # females are a bit more enthusiastic
  enthusiasm [gender == "Female"] <- enthusiasm [gender == "Female"] * 1.2 

  # 30 somethings are a lot more enthusiastic
  enthusiasm [30 <= age && age < 40] <- enthusiasm [30 <= age && age < 40] * 1.5 
  
  data.frame (userID, gender, age, enthusiasm)
}

GenerateNewTag <- function(date, user) {
  newRow <- data.frame (id = gTagCount,
                        chain=gTagCount, 
                        tag = sample (kTags, 1, prob=kNewProb),
                        strength = sample(1:3, 1),
                        date = date,
                        userID = user)
  gTagCount <<- gTagCount + 1
  gTags <<- rbind (gTags, newRow)
}

GenerateReactionTag <- function(date, user) {
  # TODO : don't pick uniformly!
  prevEmo <- sample (1:nrow(gTags[gTags$chain == gTags$id, ]), 1)
  newRow <- data.frame (id = gTagCount,
              chain=gTags[gTags$chain == gTags$id, ][prevEmo, "id"], 
              tag = sample (kTags, 1, prob=kReactProb),
              strength = sample(1:3, 1),
              date = date,
              userID = user)
  gTagCount <<- gTagCount + 1
  gTags <<- rbind (gTags, newRow)
}

Recruit <- function (N){
  recruited <- sample (gInactiveUsers, min(length(gInactiveUsers), N), replace=FALSE)
  gInactiveUsers <<- gInactiveUsers[! gInactiveUsers %in% recruited]
  gActiveUsers <<- c(gActiveUsers, recruited)
}

Dropoff <- function (N){
  dropoff <- sample (gActiveUsers, min(length(gActiveUsers), N), replace=FALSE)
  gActiveUsers <<- gActiveUsers[! gActiveUsers %in% dropoff]
  gInactiveUsers <<- c(gInactiveUsers, dropoff)
}

GetTodaysUsers <- function (factor) {
  todaysEnthusiasm <- gUserPool[gActiveUsers, "enthusiasm"] * abs(rnorm (gActiveUsers, mean=1)) * factor
  numTags <- sum (todaysEnthusiasm)
  sample (gActiveUsers, round(numTags), replace = TRUE, prob = todaysEnthusiasm/numtags)
}


SimulateDay <- function (date){

  #
  #  Users come and go
  #
  
  # have 1 recruitment day per month on avg
  recruitmentDay <- sample (c(1,0), 1, prob = c(1/30, 29/30))
  
  # recruit about 20 users on a recruitment day
  recruited <- recruitmentDay * rnorm (1, mean=20, sd=2)
  Recruit (recruited)
  
  # some dropoff. say ~ 0.03% per day 
  dropoff <- round(abs(rnorm (1, mean=length(gActiveUsers) * 0.003, sd=3)))
  Dropoff(dropoff)

  
  # viral effect : some users are added without recruitment.  say ~0.05% of active user base
  recruited <- round(abs(rnorm (1, mean=length(gActiveUsers) * 0.005, sd=3)))
  Recruit (recruited)
  
  
  #
  # users create tags
  #
  todaysEnthusiasm <- gUserPool[gActiveUsers, "enthusiasm"] * abs(rnorm (gActiveUsers, mean=1))
  numTags <- sum (todaysEnthusiasm)
  users <- sample (gActiveUsers, round(numTags), replace = TRUE, prob = todaysEnthusiasm/numTags)
  
  split <- round (length(users)*kNewTagRatio)
  sapply (1:split, 
         function (u) {GenerateNewTag(date, u)})
  
  sapply (split+1:length(users), 
         function (u) {GenerateReactionTag(date, u)})
}

SetupData <- function () {
  gUserPool <<- GenerateUsers (10000)
  gActiveUsers <<- c()
  gInactiveUsers <<- 1:nrow(gUserPool)

  gTags <<- data.frame()
  gTagCount <<- 1
  
  # start with 20 beta users
  Recruit (20)
  
  # run for some time
  start <- ymd ("20141215")
  test.period <- start + days(0:99)
  
  sapply (test.period, SimulateDay)
  gTags
}

  
#
#  Plot the data !
#

saveggplot <- function (plot_f){
  filename <- paste ("tagproject/", as.character(substitute(plot_f)), ".png", sep="")
  p <- plot_f()
  ggsave(filename=filename, plot=p, dpi=100)
}

saveAll <- function () {
  saveggplot(plotUsers1)
  saveggplot(plotUsers2)
  saveggplot(plotUsers3)
  saveggplot(plotUsers4)
  saveggplot(plotUsers5)
  saveggplot(plotChains1)
  saveggplot(plotChains2)
}


plotUsers1 <- function () {
  #
  #  plot Weekly active users
  #  using ggplot
  start <- min(gTags$date)
  
  weeklyUse  <- gTags %>%
    transform (week = date - wday(date)) %>%
    group_by(week) %>% 
    summarize (sum = length(unique (userID)))
  
  ggplot (weeklyUse, aes(x=week, y=sum)) + 
    geom_point() + geom_line() + geom_smooth(method="lm") +
    labs (x = "Week", y = "Unique users", title="Unique weekly users")
}

plotUsers2 <- function () {
  #
  #  plot Weekly created tags per gender
  #  using ggplot
  start <- min(gTags$date)
  
  by.gender  <- gTags %>%
    inner_join(gUserPool, by="userID") %>%
    transform (week = date - wday(date)) %>%
    group_by(week, gender) %>% 
    summarize (sum = length(unique (chain)))
    
  ggplot (by.gender, aes(x=week, y=sum)) + 
    facet_wrap(~gender) +
    geom_point() + geom_line() + geom_smooth(method="lm") +
    labs (x = "Week", y = "created tags", title="Created tags over time (per Gender)")
}

plotUsers3 <- function () {
  #
  #  plot Weekly created tags per age group
  #  using ggplot
  start <- min(gTags$date)
  
  by.age  <- gTags %>%
    inner_join(gUserPool, by="userID") %>%
    transform (week = date - wday(date)) %>%
    transform (age.group = floor (age/10) * 10 ) %>%
    group_by(week, age.group) %>% 
    summarize (sum = length(unique (chain)))
  
  ggplot (by.age, aes(x=week, y=sum)) + 
    facet_wrap(~age.group) +
    geom_point() + geom_line() + geom_smooth(method="lm") +
    labs (x = "Week", y = "Created tags", title="Created tags over time (per age group)")
}

plotUsers4 <- function () {
  #
  #  plot created tags per age group & gender
  #  using ggplot
  start <- min(gTags$date)
  
  by.age  <- gTags %>%
    inner_join(gUserPool, by="userID") %>%
    group_by(age, gender) 
  
  ggplot (by.age, aes(x=age)) + 
    facet_wrap(~gender) +
    geom_histogram(binwidth=1, colour="black", fill="white")+
    labs (x = "Age", y = "Created tags", title="Created tags by age")
}

plotUsers5 <- function () {
  #
  #  plot created tags per age group & gender (relative)
  #  using ggplot
    
  by.age  <- gTags %>%
    inner_join(gUserPool, by="userID") %>%
    transform (age = floor(age/5) * 5) %>%
    group_by(gender, age)  %>% 
    summarize (sum = length(id)/length(unique(userID)))
  
  ggplot (by.age, aes(x=age, y = sum)) + 
    facet_wrap(~gender) +
    geom_point() +
    labs (x = "Age", y = "Created tags", title="Created tags by age (relative)")
}



plotChains1 <- function () {
  #
  #  plot emotion tags length
  # 
    
  chain  <- gTags %>%
    group_by(chain) %>% 
    summarize (sum = length(id)-1)

  histo <- chain %>%
    group_by (sum) %>%
    summarize (hist = length(chain))
    
  ggplot (chain, aes(x=sum)) + 
    geom_histogram (binwidth=1, colour="black", fill="white") + 
    geom_vline(aes(xintercept=mean(sum)),
               color="red", linetype="dashed", size=1) +
    annotate (geom="text", x=mean(chain$sum), y = 0.8 * max(histo$hist), hjust=-0.2, 
              label=paste ("mean =", round (mean(chain$sum)), 2)) + 
    annotate (geom="text", x=max(chain$sum), y = 0.1 * max(histo$hist), hjust= 1, 
              label=paste ("max =", max(chain$sum))) + 
    labs (x = "Number of chained tags", y = "Count", title="Histogram of tag chains")
}

# plotChains2 <- function () {
#   #
#   #  plot emotion tags length
#   # 
#   
#   chain  <- gTags %>%
#     group_by(chain) %>% 
#     summarize (sum = length(id)-1)
#   
#   histo <- chain %>%
#     group_by (sum) %>%
#     summarize (hist = length(chain))
#   
#   ggplot (chain, aes(x=sum)) + 
#     geom_histogram (binwidth=1, colour="black", fill="white") + 
#     geom_vline(aes(xintercept=mean(sum)),
#                color="red", linetype="dashed", size=1) +
#     annotate (geom="text", x=mean(chain$sum), y = 0.8 * max(histo$hist), hjust=-0.2, 
#               label=paste ("mean =", round (mean(chain$sum)), 2)) + 
#     annotate (geom="text", x=max(chain$sum), y = 0.1 * max(histo$hist), hjust= 1, 
#               label=paste ("max =", max(chain$sum))) + 
#     labs (x = "Number of chained tags", y = "Count", title="Histogram of tag chains")
# }
