## ---- Housekeeping ----------------------------------------------------------------------------------

if(!require(academictwitteR)){install.packages("academictwitteR");library(academictwitteR)}
if(!require(rtweet)){install.packages("rtweet");library(rtweet)}
if(!require(dplyr)){install.packages("dplyr");library(dplyr)}
if(!require(tidyr)){install.packages("tidyr");library(tidyr)}

## ---- Setup ----------------------------------------------------------------------------------

set_bearer()
get_bearer()
datapath = "election2020_data/"

## ---- Search for Tweets ----------------------------------------------------------------------------------

df4 <- get_all_tweets(
  query = c("#election2020", "#IVoted", "#IVotedEarly", "#YoVoté", "#BidenHarris", "Biden", "Trump", "#TRUMP2020", "#potus", "#vote", "#donaldtrump", "#MyPresident", "#November3rd"),
  is_retweet=FALSE,
  lang="en",
  start_tweets = "2020-10-27T00:00:00Z",
  end_tweets = "2020-11-03T23:59:59Z",
  file = "election2020",
  data_path = datapath,
  n = 100
)

## ---- Format Tweets ----------------------------------------------------------------------------------

df4 <- df4 %>%
  mutate(tweetid = row_number())
names(df4)

# Unwwnesting 
df4 <- unnest(df4, cols= c(entities, public_metrics, attachments, geo)) 
df_hashes <- unnest(df4, cols=hashtags)
df_mentions <- unnest(df4, cols=mentions, names_repair=tidyr::tidyr_legacy)
df_urls <- unnest(df4, cols=urls)
df_reftweets <- unnest(df4, cols=referenced_tweets, names_repair=tidyr::tidyr_legacy)

# Renesting and joining back
df_hashes <- df_hashes %>% group_by(tweetid) %>% summarise(hashes=list(tag))
df4 <- df4 %>% left_join(df_hashes)

df_mentions <- df_mentions %>% group_by(tweetid) %>% summarise(mentioned_username=list(username), mentioned_userid = list(id1))
df4 <- df4 %>% left_join(df_mentions)

df_urls <- df_urls %>% group_by(tweetid) %>% summarise(urls_url=list(url), urls_expanded_url= list(expanded_url))
df4 <- df4 %>% left_join(df_urls)

df_reftweets <- df_reftweets %>% group_by(tweetid) %>% summarise(type=list(type), rt_tweetid= list(id1))
df4 <- df4 %>% left_join(df_reftweets)

# Now looking at the User's data:
df_users <- bind_tweets(data_path = datapath, user=TRUE)
df_users <- df_users %>% unnest(public_metrics)

df_users <- df_users %>%
  rename(user_id = id,
         profile_url = url,
         account_created_at = created_at,
         friends_count = following_count,
         screen_name = username,
         statuses_count = tweet_count)
names(df_users)

# Merging User data with tweet data
df4 <- df4 %>%
  rename(user_id = author_id) %>%
  inner_join(df_users) %>%
  distinct(id, .keep_all=TRUE)

# Transforming, adding, and geting rid of redundant columns
df4$type<-as.character(df4$type)
df4$rt_tweetid<-as.character(df4$rt_tweetid)
df4 <- df4 %>%
  mutate(is_quote = case_when(type == "quoted"~TRUE, TRUE~FALSE),
         is_reply = case_when(type == "replied_to"~TRUE, TRUE~FALSE),
         is_retweet = case_when(type == "retweeted"~TRUE, TRUE~FALSE))

# Restructure and rename
df4 <- df4 %>%
  dplyr::select(user_id = user_id,
                status_id=id,
                screen_name,
                created_at,
                text,
                source,
                reply_to_status_id=rt_tweetid,
                reply_to_user_id = in_reply_to_user_id,
                #reply_to_screen_name,
                is_quote,
                is_reply,
                is_retweet,
                favorite_count=like_count,
                retweet_count,
                quote_count,
                reply_count,
                hashtags = hashes,
                urls_url,
                urls_expanded_url,
                mentions_user_id = mentioned_userid,
                mentions_screen_name = mentioned_username,
                lang,
                location,
                description,
                account_created_at,
                followers_count,
                friends_count,
                statuses_count,
                profile_url,
                profile_image_url)

names(df4)[1] <- "user_id"
names(df4)[2] <- "status_id"
names(df4)[3] <- "screen_name"
names(df4)[4] <- "created_at"
names(df4)[5] <- "text"
names(df4)[6] <- "source"
names(df4)[7] <- "reply_to_status_id"
names(df4)[8] <- "reply_to_user_id"
names(df4)[9] <- "reply_to_screen_name"
names(df4)[10] <- "is_quote"
names(df4)[11] <- "is_retweet"
names(df4)[12] <- "favorite_count"
names(df4)[13] <- "retweet_count"
names(df4)[14] <- "quote_count"
names(df4)[15] <- "reply_count"
names(df4)[16] <- "hashtags"
names(df4)[17] <- "urls_url"
names(df4)[18] <- "urls_expanded_url"
names(df4)[19] <- "mentions_user_id"
names(df4)[20] <- "mentions_screen_name"
names(df4)[21] <- "lang"
names(df4)[22] <- "location"
names(df4)[23] <- "description"
names(df4)[24] <- "account_created_at"
names(df4)[25] <- "followers_count"
names(df4)[26] <- "friends_count"
names(df4)[27] <- "statuses_count"
names(df4)[28] <- "profile_url"
names(df4)[29] <- "profile_image_url"

# Setting variable in date form
df4 <- df4 %>% mutate(created_at = as.POSIXct(created_at, format="%Y-%m-%dT%H:%M:%OS"))

## ---- Save File ----------------------------------------------------------------------------------

write_as_csv(df4, "election2020tweets.csv") # Saving the dataset
