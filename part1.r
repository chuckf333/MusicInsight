## ------------------------------------------------------------------------
suppressMessages(install.packages("tidyverse", repos="http://cran.us.r-project.org"))
suppressMessages(library("tidyverse"))


## ------------------------------------------------------------------------
survey <- read_csv("https://raw.githubusercontent.com/introdsci/MusicSurvey/master/music-survey.csv")
preferences <- read_csv("https://raw.githubusercontent.com/introdsci/MusicSurvey/master/preferences-survey.csv")


## ------------------------------------------------------------------------
nrow(survey)
colnames(survey)



## ------------------------------------------------------------------------
colnames(survey)[colnames(survey)=="Timestamp"] <- "time_submitted"
colnames(survey)[colnames(survey)=="First, we are going to create a pseudonym for you to keep this survey anonymous (more or less). Which pseudonym generator would you prefer?"] <- "pseudonym_generator"
colnames(survey)[colnames(survey)=="What is your pseudonym?"] <- "pseudonym"
colnames(survey)[colnames(survey)=="Sex"] <- "sex"
colnames(survey)[colnames(survey)=="Major"] <- "academic_major"
colnames(survey)[colnames(survey)=="Academic Year"] <- "academic_level"
colnames(survey)[colnames(survey)=="Year you were born (YYYY)"] <- "year_born"
colnames(survey)[colnames(survey)=="Which musical instruments/talents do you play? (Select all that apply)"] <- "instrument_list"
colnames(survey)[colnames(survey)=="Artist"] <- "favorite_song_artist"
colnames(survey)[colnames(survey)=="Song"] <- "favorite_song"
colnames(survey)[colnames(survey)=="Link to song (on Youtube or Vimeo)"] <- "favorite_song_link"

colnames(survey)


## ------------------------------------------------------------------------
library("dplyr")
library("tidyr")


## ------------------------------------------------------------------------
Person <- tibble(time_submitted = survey$time_submitted, pseudonym_generator = survey$pseudonym_generator, pseudonym = survey$pseudonym, sex = survey$sex, academic_major = survey$academic_major, academic_level = survey$academic_level, year_born = survey$year_born)


## ------------------------------------------------------------------------
Favorite_song <- tibble(pseudonym = survey$pseudonym, artist = survey$favorite_song_artist, song = survey$favorite_song, link = survey$favorite_song_link)


## ------------------------------------------------------------------------
survey$time_submitted <- as.POSIXlt(parse_datetime(survey$time_submitted, "%m/%d/%y %H:%M"))
survey$time_submitted[1]$min


## ------------------------------------------------------------------------
Person$academic_level <- as.factor(Person$academic_level)
levels(Person$academic_level)

Person$academic_major <- as.factor(Person$academic_major)
levels(Person$academic_major)

levels(Person$academic_major)[levels(Person$academic_major)=="Computer information systems"] <- "Computer Information Systems"

levels(Person$academic_major)


## ------------------------------------------------------------------------
Ratings <- preferences %>% 
            gather(key="artist_song",value="rating",3:45)
colnames(Ratings)[colnames(Ratings)=="What was your pseudonym?"] <- "pseudonym"
Ratings$pseudonym <- as.factor(Ratings$pseudonym)
Ratings$artist_song <- as.factor(Ratings$artist_song)
head(Ratings)


## ------------------------------------------------------------------------
library("ggplot2")

ggplot(Ratings, aes(x = artist_song, y = rating)) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_bar(stat = "identity")



## ------------------------------------------------------------------------
ggplot(Ratings, aes(x = rating)) + geom_histogram(binwidth = 1)


## ------------------------------------------------------------------------
ggplot(Ratings, aes(x = pseudonym, y = rating)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 60, hjust = 1))
ggplot(Ratings, aes(x = pseudonym, y = rating)) + geom_violin() + theme(axis.text.x = element_text(angle = 60, hjust = 1))
ggplot(Ratings, aes(x = pseudonym, y = rating)) + geom_jitter() + theme(axis.text.x = element_text(angle = 60, hjust = 1))


## ------------------------------------------------------------------------
earliest_time <- min(Ratings$Timestamp[Ratings$pseudonym=="Angel Angel"])
Ratings <- Ratings %>% filter(!(pseudonym=="Angel Angel" & Timestamp!=earliest_time))
earliest_time <- min(Ratings$Timestamp[Ratings$pseudonym=="Mission Theory"])
Ratings <- Ratings %>% filter(!(pseudonym=="Mission Theory" & Timestamp!=earliest_time))


## ------------------------------------------------------------------------
inner_join(Favorite_song, Ratings, by = "pseudonym")


