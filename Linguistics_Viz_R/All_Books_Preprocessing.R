library(tidyverse)
library(tidytext)
library(textstem)
data(stop_words)

# Preprocess_Book_1 -------------------------------------------------------

custom_stop_words <- bind_rows(data_frame(word = c('youre', 'wouldnt', 'hes', 'couldnt', 'ive', 'wasnt', "didnt", 'dont', 'hadnt', 'hed', 'im'), lexicon = c("custom")), stop_words)
print(custom_stop_words, n = 50)

philosophers_stone_1 <- read_file("Files/1_Philosophers_Stone.txt")
harry_1 <- tibble(text = philosophers_stone_1)
tidy_harry_1 <- harry_1 %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>% # если нужно удалять стоп-слова
  mutate(lemma = lemmatize_words(word)) %>%
  mutate(book = "1. Philosopher's Stone") %>%
  mutate(total = length(word))
print(tidy_harry_1, n = 50)

word_count_1 <- tidy_harry_1 %>%
  count(lemma, sort = TRUE)
print(word_count_1, n = 30)

# Preprocess_Books 2-7 ----------------------------------------------------

chamber_of_secrets_2 <- read_file("Files/2_Chamber_of_Secrets.txt")
harry_2 <- tibble(text = chamber_of_secrets_2)
tidy_harry_2 <- harry_2 %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>%
  mutate(lemma = lemmatize_words(word)) %>%
  mutate(book = "2. Chamber of Secrets") %>%
  mutate(total = length(word))
print(tidy_harry_2, n = 100)

word_count_2 <- tidy_harry_2 %>%
  count(lemma, sort = TRUE)
print(word_count_2, n = 100)


prizoner_of_azkaban_3 <- read_file("Files/3_Prisoner_of_Azkaban.txt")
harry_3 <- tibble(text = prizoner_of_azkaban_3)
tidy_harry_3 <- harry_3 %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>%
  mutate(lemma = lemmatize_words(word)) %>%
  mutate(book = "3. Prisoner of Azkaban") %>%
  mutate(total = length(word))
print(tidy_harry_3, n = 100)

word_count_3 <- tidy_harry_3 %>%
  count(lemma, sort = TRUE)
print(word_count_3, n = 30)


goblet_of_fire_4 <- read_file("Files/4_Goblet_of_Fire.txt")
harry_4 <- tibble(text = goblet_of_fire_4)
tidy_harry_4 <- harry_4 %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>%
  mutate(lemma = lemmatize_words(word)) %>%
  mutate(book = "4. Goblet of Fire") %>%
  mutate(total = length(word))
print(tidy_harry_4, n = 100)

word_count_4 <- tidy_harry_4 %>%
  count(lemma, sort = TRUE)
print(word_count_4, n = 100)


order_of_phoenix_5 <- read_file("Files/5_Order_of_the_Phoenix.txt")
harry_5 <- tibble(text = order_of_phoenix_5)
tidy_harry_5 <- harry_5 %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>%
  mutate(lemma = lemmatize_words(word)) %>%
  mutate(book = "5. Order of the Phoenix") %>%
  mutate(total = length(word))
print(tidy_harry_5, n = 100)

word_count_5 <- tidy_harry_5 %>%
  count(lemma, sort = TRUE)
print(word_count_5, n = 100)


half_blood_prince_6 <- read_file("Files/6_Half-Blood_Prince.txt")
harry_6 <- tibble(text = half_blood_prince_6)
tidy_harry_6 <- harry_6 %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>%
  mutate(lemma = lemmatize_words(word)) %>%
  mutate(book = "6. Half-Blood Prince") %>%
  mutate(total = length(word))
print(tidy_harry_6, n = 100)

word_count_6 <- tidy_harry_6 %>%
  count(lemma, sort = TRUE)
print(word_count_6, n = 100)

half_blood_prince_7 <- read_file("Files/7_Deathly_Hallows.txt")
harry_7 <- tibble(text = half_blood_prince_7)
tidy_harry_7 <- harry_7 %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>%
  mutate(lemma = lemmatize_words(word)) %>%
  mutate(book = "7. Deathly Hallows") %>%
  mutate(total = length(word))
print(tidy_harry_7, n = 100)

word_count_7 <- tidy_harry_7 %>%
  count(lemma, sort = TRUE)
print(word_count_7, n = 30)

# Joining Tibbles 1-7, count frequencies-----------------------------------

joined_hp <- tidy_harry_1 %>%
  full_join(tidy_harry_2) %>%
  full_join(tidy_harry_3) %>%
  full_join(tidy_harry_4) %>%
  full_join(tidy_harry_5) %>%
  full_join(tidy_harry_6) %>%
  full_join(tidy_harry_7)

joined_hp

joined_count_hp <- joined_hp %>%
  group_by(book) %>%
  count(book, lemma, sort = TRUE, total) %>%
  ungroup() %>%
  select(lemma, n, book, total) %>%
  mutate(freq = (n * 10000) / total)

joined_count_hp


# Simple Counter Plot ----------------------------------------------------

library(harrypotter) #цветовые палитры Хогвартса
library(forcats)

n_word_counter <- joined_count_hp %>%
  group_by(book) %>%
  top_n(20) %>%
  ungroup %>%
  mutate(book = as.factor(book),
         lemma = reorder_within(lemma, freq, book)) %>%
  ggplot() +
  geom_col(aes(lemma, freq, fill = book), show.legend = FALSE) +
  facet_wrap(~book, ncol = 4, scales = 'free') +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  ggthemes::theme_tufte() +
  labs(caption = "https://github.com/meladush", title = 'Most frequent words in "Harry Potter" books', x = 'Lemma',
       y = 'Frequency per 10.000 tokens', capture = '@meladush') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_hp(discrete = TRUE, option = 'Gryffindor', alpha = 0.8) +
  theme(axis.text.x = element_text(size = 4))

n_word_counter

# TF-IDF Plot ------------------------------------------------------------

joined_count_hp

book_words <- joined_count_hp %>%
  bind_tf_idf(lemma, book, n) %>%
  select(-total)
print(book_words, n = 100)

book_words <- book_words %>%
mutate(word = reorder(lemma, tf_idf)) %>%
arrange(desc(tf_idf))
book_words

tf_idf_plot <- book_words %>%
  group_by(book) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  top_n(20) %>%
  ungroup %>%
  ggplot() +
  geom_col(aes(word, tf_idf, fill = book), show.legend = FALSE) +
  facet_wrap(~book, ncol = 4, scales = "free") +
  coord_flip() +
  ggthemes::theme_tufte() +
  labs(caption = "https://github.com/meladush", title = 'Highest TF-IDF words (keywords) in "Harry Potter" books') +
  xlab('Lemma') +
  ylab('TF-IDF\n\n*Some words are repeated because of the preprocessing of ’s and plural -s (e.g. pettigrew/pettigrews, dementor/dementors).\nIt should, ideally, be corrected manually throughout all the texts.') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_hp(discrete = TRUE, option = "Slytherin") +
  theme(axis.text.x = element_text(size = 5))

tf_idf_plot

# Words Search ------------------------------------------------------------

#my_search_word  <-  'boy'
#my_search_word_2 <- 'joy'
#my_search_word_3 <- 'death'
#my_search_word_4 <- 'friend'

joined_hp_vis <- joined_count_hp %>%
  filter(lemma == my_search_word | lemma == my_search_word_2 |
           lemma == my_search_word_3 | lemma == my_search_word_4)

library(plotly)

words_plot <- joined_hp_vis %>% ggplot +
  geom_col(aes(x = lemma, y = freq, fill = book), position = 'fill', width = .7) +
  labs(caption = "https://github.com/meladush", itle = 'Frequency distribution of user-defined words in "Harry Potter" books', y = 'freq per 10.000 tokens (all books = 1,0)',
       x = 'Word search') +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_hp_d(option = "Ravenclaw", alpha = 0.9) +
  theme(axis.text.x = element_text(size = 15)) 

words_plot
word_search_plot <- ggplotly(words_plot)
word_search_plot
