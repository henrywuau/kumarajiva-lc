#Load packages
library(tidyr)
library(tidytext)
library(dplyr)
library(readr)
library(readxl)

#Import subset of K's translations
k_corpus <- read_excel("kumarajiva_translation_corpus.xlsx")
View(k_corpus)

#unnest
unnest_corpus <- unnest_tokens(k_corpus,tokens,text,token="characters" ) 

#counting genre
genre_count_corpus <- unnest_corpus %>%
  count(genre,sort=TRUE) %>%
  arrange(desc(n)) %>%
  mutate(percent = n/sum(genre_count_corpus$n))

#n2
n2_corpus <- unnest_tokens(unnest_corpus,n2gram,tokens,token="ngrams", n=2)

simple_n2_count <- n2_corpus %>%
  count(n2gram,sort=TRUE) %>%
  arrange(desc(n))

n2_count_table <- add_count(n2_corpus, n2gram, sort = T, name = "n2_count")

#n3
n3_corpus <- unnest_tokens(unnest_corpus,n3gram,tokens,token="ngrams", n=3)
simple_n3_count <- n3_corpus %>%
  count(n3gram,sort=TRUE) %>%
  arrange(desc(n))

#n4
n4_corpus <- unnest_tokens(unnest_corpus,n4gram,tokens,token="ngrams", n=4)
simple_n4_count <- n4_corpus %>%
  count(n4gram,sort=TRUE) %>%
  arrange(desc(n))

n4_count_table <- add_count(n4_corpus, n4gram, sort = T, name="n4_count")

#n5
n5_corpus <- unnest_tokens(unnest_corpus,n5gram,tokens,token="ngrams", n=5)
simple_n5_count <- n5_corpus %>%
  count(n5gram,sort=TRUE) %>%
  arrange(desc(n))

#write tables
##write_excel_csv(simple_n4_count,path = "/Users/henry/Documents/Honours/Kumarajiva text analysis/k_n4_detail.csv")
#n2 with n4 filtered out

over50n4 <- filter(simple_n4_count,n>=50)
over100n4 <- filter(simple_n4_count,n>=100)
over100n2 <- filter(simple_n2_count,n>=100)
over50n3 <- filter(simple_n3_count,n>=50)

#n3 with n4 over 100/50 filtered out:
  n3filtered <- filter(simple_n3_count,!vapply(simple_n3_count$n3gram, function (.) any(grepl(., over100n4$n4gram, fixed = TRUE)),logical(1L))) 

write_excel_csv(n3filtered,path = "/Users/henry/Documents/Honours/Kumarajiva text analysis/k_n3_detail.csv")

n3filtered100 <- filter(n3filtered,n>=100)

#n2 with n3+n4 filtered
n2_100 <- filter(simple_n2_count,n>=100)
n2filtered <- filter(n2_100,!vapply(n2_100$n2gram, function (.) any(grepl(., over100n4$n4gram, fixed = TRUE)),logical(1L)))
n2filter2 <- filter(n2filtered,!vapply(n2filtered$n2gram, function (.) any(grepl(., n3filtered100$n3gram, fixed = TRUE)),logical(1L))) 
write_excel_csv(n2filter2,path = "/Users/henry/Documents/Honours/Kumarajiva text analysis/k_n2_detail.csv")


##add n3 filter?
n2_table_filtered <- right_join(n2_count_table,n2filtered) 

filter(n2_count_table,!vapply(n2_count_table$n2gram, function(.) any(grepl(.,n4_count_table$n4gram,fixed=TRUE)),logical(1L))) %>%
  View(n2_count_table)

#READ IN FINAL COMMON FEATURES LIST
k_feat_final <- read_excel("k_feat_final.xlsx")

##A mixed list of ngrams is unusable because we need to match them with a set of tokenised ngrams eg. of fixed length. Therefore I will start with n2 - note n2 has much fewer translit (reason?)
k_n2 <- read_excel("k_n2_detail.xlsx")

n2_final <- inner_join(n2_count_table,k_n2,by="n2gram")

#n2 by genre
n2_final_genre <-
n2_final %>%
  group_by(genre,n2gram) %>%
  mutate(new=n()) %>%
  mutate(genre_percent=new/n) %>%
  View()

#view n2 by genre
ggplot(n2_final,(aes(x=reorder(n2gram,-n),y=n,fill=genre)))+geom_bar(stat="identity")
ggsave("n2_final.png", width = 30, height = 10, dpi = 400)

ggplot(n2_final,(aes(x=reorder(n2gram,-n),y=n,fill=genre)))+geom_bar(stat="identity",position="fill")
ggsave("n2_final_genre.png", width = 30, height = 10, dpi = 400)

##n2 by percentage of sutra
n2_sutra <-
  n2_final %>%
  group_by(genre,n2gram) %>%
  mutate(new=n()) %>%
  mutate(genre_percent=new/n) %>%
  filter(genre=="S") 

ggplot(n2_sutra,(aes(x=reorder(n2gram,-genre_percent),y=(genre_percent))))+geom_point(aes(colour=type,size=10))+geom_hline(yintercept=0.71162183)
ggsave("n2_sutra.png", width = 30, height = 10, dpi = 400)

##n2 by percentage of v
n2_vinaya <-
  n2_final %>%
  group_by(genre,n2gram) %>%
  mutate(new=n()) %>%
  mutate(genre_percent=new/n) %>%
  filter(genre=="V") 

ggplot(n2_vinaya,(aes(x=reorder(n2gram,-genre_percent),y=(genre_percent))))+geom_point(aes(colour=type,size=10))+geom_hline(yintercept=0.07433781)
ggsave("n2_vinaya.png", width = 30, height = 10, dpi = 400)

##n2 by percentage of a
n2_abhidharma <-
  n2_final %>%
  group_by(genre,n2gram) %>%
  mutate(new=n()) %>%
  mutate(genre_percent=new/n) %>%
  filter(genre=="A") 

ggplot(n2_abhidharma,(aes(x=reorder(n2gram,-genre_percent),y=(genre_percent))))+geom_point(aes(colour=type,size=10))+geom_hline(yintercept=0.21404036)
ggsave("n2_abhidharma.png", width = 30, height = 10, dpi = 400)

####N2 by date of composition

n2_final_date <-
  n2_final %>%
  group_by(date,n2gram) %>%
  mutate(new=n()) %>%
  mutate(date_percent=new/n)

n2_final_date %>% 
  filter(!is.na(date)) %>%
group_by(n2gram) %>% 
  summarize(avg=mean(date)) %>%
  View()


n2_sutra_date <-
  n2_final %>%
  group_by(date,n2gram) %>%
  filter(!is.na(date)) %>%
  mutate(new=n()) %>%
  mutate(date_percent=new/n) %>%
  filter(genre=="S")

ggplot(arrange(n2_sutra_date,date),(aes(x=reorder(n2gram,-date),y=n,fill=date)))+geom_bar(stat="identity",position="fill")
ggsave("n2_sutra_date.png", width = 30, height = 10, dpi = 400)

#among certain type
n2_g <-
n2_final_date %>%
  filter(type=="g") 
ggplot(arrange(n2_g,date),(aes(x=reorder(n2gram,-date),y=n,fill=date)))+geom_bar(stat="identity",position="fill")
ggsave("n2_grammar_date.png", width = 30, height = 10, dpi = 400)

##is this just by title?
n2_g <-
  n2_final_date %>%
  filter(type=="g") 
ggplot(arrange(n2_g,date),(aes(x=reorder(n2gram,-date),y=n,fill=title)))+geom_bar(stat="identity",position="fill")
ggsave("n2_grammar_date_by_text.png", width = 30, height = 10, dpi = 400)

n2_a <-
n2_final_date %>%
  filter(type=="a")
ggplot(n2_a,(aes(x=reorder(n2gram,-date),y=n,fill=date)))+geom_bar(stat="identity",position="fill")
ggsave("n2_translation.png", width = 30, height = 10, dpi = 400)


ggplot(n2_final_date,aes(x=reorder(n2gram,-date),y=date))+geom_point(aes(colour=type,size=date_percent))
ggsave("n2_f_date_type.png", width = 30, height = 10, dpi = 400)

#sorting pre406

n2_pre406 <-
  n2_final %>%
  group_by(date,n2gram) %>%
  filter(!is.na(date)) %>%
  mutate(new=n()) %>%
  mutate(date_percent=new/n) %>%
  filter(date<406)

ggplot(n2_pre406,aes(reorder(x=n2gram,-date_percent),y=date_percent))+geom_point(aes(colour=n2_pre406$type))
ggsave("n2_pre406.png", width = 30, height = 10, dpi = 400)

n2_post406 <-
  n2_final %>%
  group_by(date,n2gram) %>%
  filter(!is.na(date)) %>%
  mutate(new=n()) %>%
  mutate(date_percent=new/n) %>%
  filter(date>=406)

ggplot(n2_post406,aes(reorder(x=n2gram,-date_percent),y=date_percent))+geom_point(aes(colour=n2_post406$type))
ggsave("n2_post406.png", width = 30, height = 10, dpi = 400)

###N3###
k_n3 <- read_excel("k_n3_final.xlsx")
n3_final <- inner_join(n3_corpus,k_n3,by="n3gram")

##n2 by percentage of sutra
n3_sutra <-
  n3_final %>%
  group_by(genre,n3gram) %>%
  mutate(new=n()) %>%
  mutate(genre_percent=new/n) %>%
  filter(genre=="S") 

ggplot(n3_sutra,(aes(x=reorder(n3gram,-genre_percent),y=(genre_percent))))+geom_point(aes(colour=type,size=10))+geom_hline(yintercept=0.71162183)
ggsave("n3_sutra.png", width = 30, height = 10, dpi = 400)


###########

#Sort language features by genre
n2_by_genre <-
n2_table_filtered  %>% 
  group_by(genre,n2gram) %>% 
  mutate(new = n()) %>%
  mutate(genre_percent=new/n)

#sutra features
sutra_features <- 
n2_by_genre %>%
  filter(genre=="S") %>% 
  filter(genre_percent>0.71162183)

count(sutra_features,n2gram) %>%
  View()

#lun features
lun_features <-
n2_by_genre %>%
  filter(genre=="A") %>%
  filter(genre_percent>0.21404036)

count(lun_features,n2gram) %>%
  View()

#vinaya features
vin_features <-
  n2_by_genre %>%
  filter(genre=="V") %>%
  filter(genre_percent>0.07433781) 

count(vin_features,n2gram) %>%
  View()

  #graph
ggplot(n2_table_filtered,(aes(x=reorder(n2gram,-n),y=n,fill=genre)))+geom_bar(stat="identity")
ggsave("n2_filtered_genre.png", width = 30, height = 10, dpi = 400)

#Sort language features by date of composition
n2_by_date <- n2_table_filtered %>%
  group_by(date,n2gram) %>% 
  mutate(new = n()) %>%
  filter(!is.na(date)) %>%
  mutate(date_percent=new/n)

ggplot(n2_table_filtered,(aes(x=reorder(n2gram,-n),y=bigram_count,fill=date)))+geom_bar(stat="identity") 
ggsave("n2_filtered_date.png", width = 30, height = 10, dpi = 400)

#visualisation
library(ggplot2)
library(showtext)
library(jsonlite)
library(curl)
font_add_google("Noto Serif TC","Noto Serif TC")
showtext_auto()

#by genre
ggplot(big_count_corpus,(aes(x=reorder(bigram,-bigram_count),y=bigram_count,fill=genre)))+geom_bar(stat="identity")
ggsave("corpus_bigrams_genre.png", width = 30, height = 10, dpi = 400)

#by time
ggplot(big_count_corpus,(aes(x=reorder(bigram,-bigram_count),y=bigram_count,fill=date)))+geom_bar(stat="identity")
ggsave("corpus_bigrams_date.png", width = 30, height = 10, dpi = 400)

#fill
ggplot(big_count_corpus,(aes(x=reorder(bigram,-bigram_count),y=bigram_count,fill=genre)))+geom_bar(stat="identity",position="fill")
ggsave("corpus_bigrams_genre_fill.png", width = 30, height = 10, dpi = 400)

ggplot(big_count_corpus,(aes(x=reorder(bigram,-bigram_count),y=bigram_count,fill=date)))+geom_bar(stat="identity",position="fill")
ggsave("corpus_bigrams_date_fill.png", width = 30, height = 10, dpi = 400)
