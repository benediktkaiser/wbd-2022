## --------- Housekeeping ---------------------------------------------------------------

if(!require(data.table)){install.packages("data.table");library(data.table)}
if(!require(dplyr)){install.packages("dplyr");library(dplyr)}
if(!require(ggplot2)){install.packages("ggplot2");library(ggplot2)}
if(!require(ggraph)){install.packages("ggraph");library(ggraph)}
if(!require(Hmisc)){install.packages("Hmisc");library(Hmisc)}
if(!require(igraph)){install.packages("igraph");library(igraph)}
if(!require(psych)){install.packages("psych");library(psych)}
if(!require(quanteda)){install.packages("quanteda");library(quanteda)}
if(!require(rtweet)){install.packages("rtweet");library(rtweet)}
if(!require(tidyr)){install.packages("tidyr");library(tidyr)}
if(!require(tidytext)){install.packages("tidytext");library(tidytext)}
if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)}
if(!require(textdata)){install.packages("textdata");library(textdata)}

## --------- Load the data ------------------------------------------------------------------------

tweets <- read_twitter_csv("election2020tweets.csv")
names(tweets)
dim(tweets)

## --------- Data Wrangling -----------------------------------------------------------------------

# Creating an ID/index variable
tweets <- tweets %>% mutate(row_num = row_number()) # Create ID column

# Number of daily tweets line plot
ts_plot(tweets, by="2 minutes", col="red", lwd = 0.8) +
  theme_classic() +
  labs(x=NULL, y=NULL, 
       title="Trend line of NFT tweets, 2-minute intervals",
       caption= "WBD @ Maastricht University")

# cleaning the tweets' text
tweets <- tweets %>%
  mutate(clean_text = text,  
         clean_text = tolower(clean_text),
         clean_text = gsub("@\\S+", "", clean_text),
         clean_text = gsub("http(s?):\\S+", "", clean_text),
         clean_text = gsub("#\\S+", "", clean_text),
         clean_text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_text),
         clean_text = gsub("[[:punct:]]", "", clean_text), 
         clean_text = gsub("<U\\+?[0-9a-fA-F]+>", " ", clean_text),
         clean_text = gsub("[^\x20-\x7E]", "", clean_text),
         clean_text = gsub("[[:digit:]]", "", clean_text),
         clean_text = gsub("\n", " ", clean_text),
         clean_text = gsub("^\\s+|\\s+$|\\s+(?=\\s)", "", clean_text, perl=T))


# The difference between the original tweets and the clean ones
tweets %>%
  dplyr::select(row_num, text, clean_text) %>%
  group_by(row_num) %>%
  head

# Next, we remove words that have no meaning except grammatical

tweets2 <- tweets %>% 
  unnest_tokens(input = text, output = word)

tweets3 <- tweets %>% 
  unnest_tokens(input = clean_text, output = word)

tweets4 <- tweets %>% 
  unnest_tokens(input = clean_text, output = word)%>%
  filter(!word %in% stop_words$word)

# Stop words
data("stop_words") 
head(stop_words)
# View(stop_words)

# Un-nesting tokens, cleaning text
tweets5 <- tweets %>% 
  unnest_tokens(input = clean_text, output = word) %>%
  filter(!word %in% stop_words$word) %>%
  group_by(row_num) %>%
  summarise(cleaner_text = paste(word, collapse = " "))

tweets <- tweets %>% 
  left_join(tweets5)
names(tweets)

# The differences between original, cleaned and cleaner tweets
tweets %>%
  dplyr::select(row_num, text, clean_text, cleaner_text) %>%
  group_by(row_num) %>%
  head(10)

tweets2 %>% count(word, sort=TRUE) %>% top_n(25)
tweets3 %>% count(word, sort=TRUE) %>% top_n(25)
tweets4 %>% count(word, sort=TRUE) %>% top_n(25)

# We can visualize these differences
tweets2 %>%
  count(word, sort=TRUE) %>%
  top_n(25) %>%
  ggplot(aes(x= reorder(word, n), y= n)) +
  geom_bar(stat ="identity", fill= "red")+
  coord_flip()+
  theme_classic()+
  labs(title="Original tweets",
       x = "Words",
       y = "Frequency")

tweets3 %>%
  count(word, sort=TRUE) %>%
  top_n(25) %>%
  ggplot(aes(x= reorder(word, n), y= n)) +
  geom_bar(stat ="identity", fill= "blue")+
  coord_flip()+
  theme_classic()+
  labs(title="Wrangled tweets",
       x = "Words",
       y = "Frequency")

tweets4 %>%
  count(word, sort=TRUE) %>%
  top_n(25) %>%
  ggplot(aes(x= reorder(word, n), y= n)) +
  geom_bar(stat ="identity", fill= "darkgreen")+
  coord_flip()+
  theme_classic()+
  labs(title="No StopWords tweets",
       x = "Words",
       y = "Frequency")

tweets6 <- tweets %>% 
  dplyr::select(cleaner_text) %>%
  unnest_tokens(paired_words, cleaner_text, token = "ngrams", n = 2)

## --------- Semantic n-gram networks ---------------------------------------------------------------

tweets6 %>%
  count(paired_words, sort = T)

tweets7 <- tweets6 %>%
  separate(paired_words, c("word1", "word2", sep = " "))

tweets8 <- tweets7 %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

tweets9 <- tweets8 %>%
  count(word1, word2, sqrt = T)

# Creating a word network

set.seed(20220111)
tweets9 %>%
  filter(n >= 250) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_node_point(color = "blue", size = 4, alpha = 0.3) +
  geom_node_text(aes(label = name), vjust = 3, size = 3) + 
  geom_edge_link0(alpha = 0.6) +
  labs(title = "Word network, 20,000+ appearances") + 
  theme(panel.background = element_rect("papayawhip"))

## --------- Metadata analysis --------------------------------------------------------------------

tweets %>%
  summarise(mean(retweet_count),
            mean(favorite_count),
            mean(quote_count),
            mean(reply_count))

# Top tweeters
tweets %>%
  count(screen_name, sort=TRUE) %>%
  top_n(20) 

tweets %>% 
  count(screen_name, sort=TRUE) %>%
  top_n(20) %>%
  ggplot(aes(x= reorder(screen_name, n), y= n)) +
  geom_bar(stat ="identity", color = "darkorange1", fill = "darkorange1")+
  coord_flip()+
  theme_classic()+
  labs(title="Top 20 tweeters",
       x = "Twitter handles",
       y = "Number of tweets")

# Top sources
tweets %>% 
  count(source, sort=TRUE) %>%
  top_n(20)

tweets %>% 
  count(source, sort=TRUE) %>%
  top_n(20) %>%
  ggplot(aes(x= reorder(source, n), y= n)) +
  geom_bar(stat ="identity", color = "red", fill = "red")+
  coord_flip()+
  theme_classic()+
  labs(title="Top 20 platforms",
       x = "Platform",
       y = "Number of tweets")

# Top hashtags

tweets %>% 
  count(hashtags, sort=TRUE) %>%
  top_n(20) %>%
  na.omit()

tweets %>% 
  count(hashtags, sort=TRUE) %>%
  top_n(20) %>%
  na.omit() %>%
  ggplot(aes(x= reorder(hashtags, n), y= n)) +
  geom_bar(stat ="identity", color = "deeppink1", fill = "deeppink1")+
  coord_flip()+
  theme_classic()+
  labs(title="Top 20 hashtags",
       x = "Hashtags",
       y = "Number of tweets")

# Top mentioned screen names 

tweets %>% 
  count(mentions_screen_name, sort=TRUE) %>%
  top_n(20) %>%
  na.omit()

tweets %>% 
  count(mentions_screen_name, sort=TRUE) %>%
  top_n(20) %>%
  na.omit() %>%
  ggplot(aes(x= reorder(mentions_screen_name, n), y= n)) +
  geom_bar(stat ="identity", color = "dodgerblue1", fill = "dodgerblue1")+
  coord_flip()+
  theme_classic()+
  labs(title="Top 20 mentiones",
       x = "Mentioned SN",
       y = "Number of tweets")


## --------- Sentiment analysis: NRC --------------------------------------------------------------

get_sentiments("nrc")

tweetnrc <- tweets %>%
  mutate(row_num = row_number()) %>%
  group_by(row_num) %>%
  unnest_tokens(word, cleaner_text) %>%
  full_join(get_sentiments("nrc")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill=0)

names(tweetnrc)
rcorr(as.matrix(tweetnrc[, 2:11]))

boxplot(tweetnrc[,c(-1, -7, -8, -12)], outline = F,col = rainbow(8))

tweets <- tweets %>%
  left_join(tweetnrc)
names(tweets)


tweets %>%
  mutate(datetime=as.POSIXct(created_at, format="%Y-%m-%d %H:%M:%S"),
         interval= as.POSIXct(cut(datetime, breaks = "5 mins"))) %>%
  group_by(interval) %>%
  summarise(meananticipation = mean(anticipation),
            meanjoy = mean(joy),
            meansurprise = mean(surprise),
            meantrust = mean(trust))%>%
  ggplot() +
  geom_line(aes(x=interval, y=meananticipation, color = "Anticipation")) +
  geom_line(aes(x=interval, y=meanjoy, color = "Joy")) +
  geom_line(aes(x=interval, y=meansurprise, color = "Surprise")) +
  geom_line(aes(x=interval, y=meantrust, color = "Trust"))+
  #geom_vline(xintercept= as.POSIXct("2020-12-9 17:51:42", format="%Y-%m-%d %H:%M:%S")) +
  theme_classic() +
  labs(x=NULL, y = NULL,
       title = "Positive emotions",
       subtitle = "Scores are averages per 5 minutes") +
  scale_color_manual(name="Positive sentiments in tweets",
                     values = c("Anticipation" = "chartreuse2", 
                                "Joy"= "brown2",
                                "Surprise"= "darkmagenta",
                                "Trust" = "deepskyblue1"))
tweets %>%
  mutate(datetime=as.POSIXct(created_at, format="%Y-%m-%d %H:%M:%S"),
         interval= as.POSIXct(cut(datetime, breaks = "5 mins"))) %>%
  group_by(interval) %>%
  summarise(meananger = mean(anger),
            meanfear = mean(fear),
            meandisgust = mean(disgust),
            meansadness = mean(sadness))%>%
  ggplot() +
  geom_line(aes(x=interval, y=meananger, color = "Anger")) +
  geom_line(aes(x=interval, y=meanfear, color = "Fear")) +
  geom_line(aes(x=interval, y=meandisgust, color = "Disgust")) +
  geom_line(aes(x=interval, y=meansadness, color = "Sadness"))+
  #geom_vline(xintercept= as.POSIXct("2020-12-9 17:51:42", format="%Y-%m-%d %H:%M:%S")) +
  theme_classic() +
  labs(x=NULL, y = NULL,
       title = "Negative emotions",
       subtitle = "Scores are averages per 5 minutes") +
  scale_color_manual(name="Negative sentiments in tweets",
                     values = c("Anger" = "chartreuse2", 
                                "Fear"= "brown2",
                                "Disgust"= "darkmagenta",
                                "Sadness" = "deepskyblue1"))

## --------- ACA lexicons -------------------------------------------------------------------------

populism.liberalism.lexicon <- dictionary(
  list(populism = c("elit*", "consensus*", "undemocratic*", "referend*", 
                    "corrupt*", "propagand", "politici*", "*deceit*", 
                    "*deceiv*", "*betray*", "shame*", "scandal*", 
                    "truth*", "dishonest*", "establishm*", "ruling*"), 
       # developed by Ken Benoit
       liberalism = c("liber*", "free*", "indiv*", "open*", "law*", 
                      "rules", "order", "rights", "trade", "global", 
                      "inter*", "trans*", "minori*", "exchange", "market*"))) 
# developed by Cornelius Puschmann & Mario Haim

woke.lexicon <- dictionary(
  list(woke_term = c(
    "aapi", "accountab*", "ally", "antiracism", "appropriat*", "bame", 
    "binary", "bipoc", "black", "black lives matter", "blm", "bopo", 
    "bropropriate", "brown", "cancel culture", "cis", "cisgender", "class", 
    "clicktivism", "conscious capitalism", "critical race theory", "crt", 
    "dead name", "decololization", "discriminat*", "disparate impact", 
    "divers*", "diversity", "drag", "environmental justice", "equity", 
    "fast fashion", "flexitarianism", "gaslight", "gender", "greenwash", 
    "hate spe*", "heteronormativ*", "heteronormative", "identity politics", 
    "implicit bias", "inclusi*", "indigenous", "internalis* ", "internaliz*", 
    "intersectional*", "latinx", "lgbt*", "mansplain*", "marginaliz*", 
    "masculinity", "micro agress*", "microagress*", "misogyn*", "nonbinary", 
    "of color", "pansexual", "pinkwash", "playing field", "poc", "privilege*", 
    "pronoun*", "queer", "queerbaiting", "rac*", "race", "safe space", 
    "social justice", "spectrum", "structural", "supremac*", "systemic", 
    "theme house", "toxic", "trigger*", "unearned", "white fragility", 
    "whitewash", "zero waste")))

start <- Sys.time()
# Counting the number of occurrences of each lexicon term in the cleaner tweets
# We will create a document-feature matrices for that:
tweetsdfm1 <- dfm(tweets$cleaner_text, dictionary = populism.liberalism.lexicon) 
tweetsdfm2 <- dfm(tweets$cleaner_text, dictionary = woke.lexicon)

# Converting the document-feature matrices to dataframes

tweetsdfm2a <- convert(tweetsdfm1, "data.frame")
tweetsdfm2b <- convert(tweetsdfm2, "data.frame")

mean(tweetsdfm2a$populism)
mean(tweetsdfm2a$liberalism)
mean(tweetsdfm2b$woke)

describe(tweetsdfm2a)
describe(tweetsdfm2b)

# Indexing these new dataframes by their row numbers
tweetsdfm2a$row_num <- seq.int(nrow(tweetsdfm2a))
tweetsdfm2b$row_num <- seq.int(nrow(tweetsdfm2b)) 

# Joining them to the main tweetext dataframe
tweets <- tweets %>% 
  left_join(tweetsdfm2a)
tweets <- tweets %>% 
  left_join(tweetsdfm2b)

names(tweets)

## --------- Save the data ------------------------------------------------------------------------

fwrite(tweets, "mydata2.csv") # Change the name of the csv file to your preferred file's name