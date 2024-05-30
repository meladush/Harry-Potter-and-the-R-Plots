library(tidyverse)
library(tidytext)
library(harrypotter) # цветовые палитры Хогвартса
library(forcats)
library(patchwork)
library(plotly)
library(textstem) # для лемматизации
data(stop_words)


# Preprocessing -----------------------------------------------------------

custom_stop_words <- bind_rows(data_frame(word = c('youre', 'wouldnt', 'hes', 'couldnt', 'ive', 'wasnt', "didnt", 'dont', 'hadnt', 'hed', 'im'), lexicon = c("custom")), stop_words)
print(custom_stop_words, n = 50)

print(tidy_harry_1, n = 50)
print(word_count_1, n = 30)

print(joined_count_hp, n = 30)

# Counters ----------------------------------------------------------------

n_word_counter #plot

print(book_words, n = 25) # tibble with tf-idf

tf_idf_plot
n_word_counter / tf_idf_plot

# Sentiment for Book 1 !! --------------------------------------------------

print(nrc_sents, n = 30)

nrc_sents %>%
  distinct(sentiment) # NRC sentiments

fear_joy_plot # fear-joy words

ggp_img # sentiment cloud as image

patch <- fear_joy_plot + ggp_img
patch

print(afinn, n = 50)

print(bing, n = 50)

stone_tidy %>% # все 4 метода считают суммарный сентимент как отрицательный
  inner_join(loughran) %>%
  count(sentiment)

stone_tidy %>%
  inner_join(afinn) %>%
  summarise(sent_score = sum(value))

sent_book_1 # sentiment per chapter plot

# Word Search -------------------------------------------------------------

my_search_word  <-  'boy'
my_search_word_2 <- 'joy'
my_search_word_3 <- 'death'
my_search_word_4 <- 'friend'

joined_hp_vis <- joined_count_hp %>%
  filter(lemma == my_search_word | lemma == my_search_word_2 |
           lemma == my_search_word_3 | lemma == my_search_word_4)

print(joined_hp_vis %>% arrange(lemma), n = 28)

words_plot <- joined_hp_vis %>% ggplot +
  geom_col(aes(x = lemma, y = freq, fill = book), position = 'fill', width = .7) +
  labs(title = 'Frequency distribution of user-defined words in "Harry Potter" books', y = 'Word frequency per 10.000 tokens (all books = 1,0)\n',
       x = '\nWord search') +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_hp_d(option = "Ravenclaw", alpha = .8) +
  theme(axis.text.x = element_text(size = 15)) 

words_plot
word_search_plot <- ggplotly(words_plot)
word_search_plot
