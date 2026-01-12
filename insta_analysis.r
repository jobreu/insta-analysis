# Import & check data ####

library(readr)

insta <- read_csv("INSERT_FILE_NAME_HERE") # Namen der entsprechenden Datei (inkl. Dateiendung) einfügen

names(insta)

library(dplyr)

glimpse(insta)

insta %>% 
  count(author) %>% 
  arrange(desc(n)) %>% 
  head(20)

# Text analysis ####

library(quanteda)

## Create corpus ####

insta_corpus <- insta %>% 
  select(id, timestamp, unix_timestamp,
         url, body, 
         author, author_fullname,
         hashtags, usertags,
         num_likes, num_comments,
         location_name) %>% 
  corpus(docid_field = "id",
         text_field = "body")

insta_corpus

## Tokenization & stopword removal ####

tokens_insta <- tokens(insta_corpus,
                       remove_punct = TRUE,
                       remove_symbols = TRUE,
                       remove_numbers = TRUE,
                       remove_url = TRUE)

tokens_insta <- tokens_remove(tokens_insta,
                              stopwords("de"))

tokens_insta

## Create document-feature matrix ####

dfm_insta <- dfm(tokens_insta)

dfm_insta

## Frequency analysis & visualization ####

library(quanteda.textstats)

### Top Words ####

dfm_insta %>%
  dfm_remove(pattern = c("@*", "#*")) %>% # ohne User Tags und Hashtags
  textstat_frequency(n = 20)

### Top Hashtags & User Tags ####

dfm_tag <- dfm_select(dfm_insta, pattern = "#*")
toptag <- names(topfeatures(dfm_tag, 50)) # 50 häufigste Hashtags
head(toptag, 10) # 10 häufigste Hashtags

dfm_users <- dfm_select(dfm_insta, pattern = "@*")
topuser <- names(topfeatures(dfm_users, 50)) # 50 häufigste User Tags
head(topuser, 10) # 10 häufigste User Tags

### Wordcloud & Barplot ####

library(quanteda.textplots)

dfm_insta %>% 
  dfm_remove(pattern = c("@*", "#*")) %>% # ohne User Mentions & Hashtags
  dfm_trim(min_termfreq = 10) %>% # Wörter müssen mind. 10x vorkommen
  textplot_wordcloud()

tstat_freq <- dfm_insta %>% 
  dfm_remove(pattern = c("@*", "#*")) %>%
  textstat_frequency(n = 20) # Wörter müssen mind. 20x vorkommen

ggplot(tstat_freq, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_col() + 
  labs(x = "Frequency", y = "Feature") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)))

### Co-occurrence networks: hashtags & user mentions ####

fcmat_tag <- fcm(dfm_tag)
head(fcmat_tag)

fcmat_topgat <- fcm_select(fcmat_tag, pattern = toptag)
textplot_network(fcmat_topgat, 
                 min_freq = 0.1,
                 edge_alpha = 0.8,
                 edge_size = 5)

fcmat_users <- fcm(dfm_users)
head(fcmat_users)

fcmat_users <- fcm_select(fcmat_users, pattern = topuser)
textplot_network(fcmat_users,
                 min_freq = 0.1,
                 edge_color = "orange",
                 edge_alpha = 0.8,
                 edge_size = 5)

# Network Analysis ####

library(igraph)
library(tidyr)
library(stringr)

## Coauthor & User Tag Networks ####

coauthors <- insta %>%
  filter(!is.na(coauthors)) %>%
  separate_rows(coauthors, sep = ",") %>%
  mutate(coauthors = str_trim(coauthors)) %>%
  select(source = author, target = coauthors) %>% 
  count(source, target, name = "weight") %>% 
  filter(weight >= 2)

g1 = graph_from_data_frame(coauthors, 
                           directed = FALSE)

plot(g1, 
     edge.width = E(g1)$weight * 2,
     edge.arrow.size = 0.5,
     edge.label = E(g1)$weight,
     main = "Weighted Coauthor Network")

tags <- insta %>%
  filter(!is.na(usertags)) %>%
  separate_rows(usertags, sep = ",") %>%
  mutate(tags = str_trim(usertags)) %>%
  select(source = author, target = tags) %>% 
  count(source, target, name = "weight") %>% 
  filter(weight >= 2)

g2 = graph_from_data_frame(tags, directed = TRUE)

plot(g2, 
     edge.width = E(g2)$weight * 2,
     edge.arrow.size = 0.5,
     edge.label = E(g2)$weight,
     main = "Weighted User Tag Network")

## Network Measures ####

mean_distance(g1, directed = FALSE)

edge_density(g1)

print(closeness(g1, normalized = T))

print(degree(g2, normalized = T, mode="in"))
print(degree(g2, normalized = T, mode="out"))

# Filter posts ####

## Filter by date ####
insta_filtered1 <- insta %>%
  filter(timestamp >= as.POSIXct("2025-01-01"))

## 50 most recent posts per author ####
insta_filtered2 <- insta %>%
  group_by(author) %>%
  slice_max(order_by = timestamp, n = 50) %>%
  ungroup()
