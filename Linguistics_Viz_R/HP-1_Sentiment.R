library(tidytext)
library(tidyverse)
library(textstem)

# pre-processing -----------------------------------------------------------

load("Files/philosophers_stone.rda")

philosophers_stonе
length(philosophers_stone)

stone <- tibble(chapter = 1:length(philosophers_stone), text = philosophers_stone)
stone

stone_tidy <- stone %>%
  unnest_tokens(word, text) %>%
  mutate(lemma = lemmatize_words(word)) %>%
  anti_join(stop_words)

stone_tidy

stone_tidy <- 
  stone_tidy %>%
  filter(lemma != 'find' & lemma != 'watch') %>%
  select(chapter, lemma)

stone_tidy
  
print(stone_tidy %>%
  count(lemma, sort = TRUE), n = 30) # most frequent lemmas

# sentiment ---------------------------------------------------------------

stone_tidy_for_plot <- stone_tidy

nrc_sents <- get_sentiments("nrc")
print(nrc_sents, n = 30)
nrc_sents %>%
  distinct(sentiment) # NRC sentiments

two_sents <- get_sentiments("nrc") %>%
  filter(sentiment == "joy" | sentiment == "fear") %>%
  rename(lemma = word) %>%
  arrange(lemma)

print(two_sents, n = 200)

nrc_word_counts <- stone_tidy %>%
  inner_join(two_sents, relationship = "many-to-many") %>%
  count(lemma, sentiment, sort = TRUE) %>%
  ungroup()

nrc_word_counts <- nrc_word_counts %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(lemma = reorder(lemma, n)) %>%
  filter(lemma != 'find' & lemma != 'watch')

nrc_word_counts


# Top-20 fear and joy words plot -------------------------------------------

library(harrypotter)

fear_joy_plot <- nrc_word_counts %>%
  ggplot() +
  geom_col(aes(lemma, n, fill = sentiment), show.legend = FALSE, alpha = .73) +
  facet_wrap(~sentiment, scales = "free_y", nrow = 2) +
  labs(title = 'Most common fear and joy words in "Harry Potter-1"', subtitle = 'and their contribution to sentiment\n\n',
       y = 'absolute frequency in "Harry Potter and the Philosopher\'s Stone"', x = NULL) +
  coord_flip() +
  theme_bw(base_family = "serif") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c('#003f35', '#FFB933'))

fear_joy_plot

# Sentiment Comparison Cloud ----------------------------------------------

library(wordcloud)
library(reshape2)

sent_cloud <- stone_tidy %>%
  inner_join(two_sents, relationship = "many-to-many") %>%
  count(lemma, sentiment, sort = TRUE)

joy_fear_cloud <- sent_cloud %>%
  acast(lemma ~ sentiment, value.var='n', fill=0) %>%
  comparison.cloud(random.order = TRUE, colors=c('#003f35', '#FFB933'), max.words = 500, title.size = 5, title.bg.colors = NULL,
                   title.colors = c('#555555', '#7D3333'))

# Sentiment cloud as .png -------------------------------------------------

library(png)

ggp <- ggplot(data = NULL, aes()) +
  geom_point() +
  theme_bw(base_family = "serif") +
  labs(caption = "https://github.com/meladush", title = 'Sentiment comparison cloud\n', x = '\nwords of fear and joy') +
  theme(plot.title = element_text(hjust = 0.5))

img <- readPNG("Files/sent_cloud.png", native = TRUE)
ggp_img <- ggp +
  inset_element(p = img, 0.09, 0.09, 0.99, 0.99, align_to = 'panel')

ggp_img

# Sentiment by Chapters ---------------------------------------------------

afinn <- get_sentiments("afinn") %>%  # сентимент-лексикон 1
  rename(lemma = word)
print(afinn, n = 50)

bing <- get_sentiments("bing") %>% # сентимент-лексикон 2
  rename(lemma = word)
print(bing, n = 50)

loughran <- get_sentiments('loughran') %>% # сентимент-лексикон 3
  filter(sentiment == "positive" | sentiment == "negative") %>%
  rename(lemma = word)

nrc_pos_neg <- get_sentiments("nrc") %>% # сентимент-лексикон 4
  filter(sentiment == "positive" | sentiment == "negative") %>%
  rename(lemma = word) %>%
  arrange(lemma)

hp_sentiment_afinn <- stone_tidy %>%
  inner_join(afinn) %>%
  group_by(chapter) %>%
  summarise(sentiment_score = sum(value)) %>%
  mutate(method = "AFINN")
hp_sentiment_afinn

hp_sentiment_bing <- stone_tidy %>%
  inner_join(bing) %>%
  group_by(chapter) %>%
  count(sentiment) %>%
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  mutate(sentiment_score = positive - negative) %>%
  mutate(method = "Bing et al.") %>%
  select(chapter, sentiment_score, method)
hp_sentiment_bing

hp_sentiment_loughran <- stone_tidy %>%
  inner_join(loughran) %>%
  group_by(chapter) %>%
  count(sentiment) %>%
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  mutate(sentiment_score = positive - negative) %>%
  mutate(method = "Loughran") %>%
  select(chapter, sentiment_score, method)
hp_sentiment_loughran

hp_sentiment_nrc <- stone_tidy %>%
  inner_join(nrc_pos_neg) %>%
  group_by(chapter) %>%
  count(sentiment) %>%
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  mutate(sentiment_score = positive - negative) %>%
  mutate(method = "NRC") %>%
  select(chapter, sentiment_score, method)
hp_sentiment_nrc

# проверим, что по всем лексиконам преобладает негативный сентимент

stone_tidy %>%
  inner_join(bing) %>%
  count(sentiment)

stone_tidy %>%
  inner_join(afinn) %>%
  summarise(sent_score = sum(value))

stone_tidy %>%
  inner_join(loughran) %>%
  count(sentiment)

stone_tidy %>%
  inner_join(nrc_pos_neg) %>%
  count(sentiment)

# Sentiment Plot -------------------------------------------------------

sent_book_1 <- bind_rows(hp_sentiment_afinn, hp_sentiment_bing,
          hp_sentiment_loughran, hp_sentiment_nrc) %>%
  ggplot(aes(chapter, sentiment_score, fill = method)) +
  geom_col(show.legend = FALSE, width = .8) +
  labs(caption = "https://github.com/meladush", y = 'Sentiment Trend', x = '\nChapters from 1 to 17', title = 'Sentiment distribution in "Harry Potter and the Philosopher\'s Stone"', subtitle = 'by chapter, using 4 different sentiment lexicons') +
  scale_x_continuous(breaks = 1:17) +
  facet_wrap(~method, ncol = 1, scales = 'free') +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_hp(discrete = TRUE, option = "Ravenclaw") +
  theme(axis.text.y = element_text(size = 5))

sent_book_1
