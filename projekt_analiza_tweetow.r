#' ---
#' title: "System analizy trendów i sentymentu w tweetach dotyczących AI"
#' author: "Emilia Wojciechowska, Albert Wojtyniak, Dawid Mańkowski"
#' date:   "07.06.2026 "
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: paper      
#'     highlight: espresso      
#'     toc: true            
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show    
#'     number_sections: false 
#' ---

#' # Wymagane pakiety ----

# install.packages(c("tm", "tidytext", "wordcloud", "RColorBrewer", "tidyverse", "topicmodels", "DT", "lubridate", "scales","dplyr","ggplot2"))
library(tidyverse)
library(lubridate)
library(tidytext)
library(wordcloud)
library(topicmodels)
library(scales)
library(DT)
library(dplyr) 
library(tm) 
library(RColorBrewer)
library(ggplot2)

#' # 1. Funkcja top_terms_by_topic_LDA ----

top_terms_by_topic_LDA <- function(input_text, # wektor lub kolumna tekstowa z ramki danych
                                   plot = TRUE, # domyślnie rysuje wykres
                                   k = 3) # wyznaczona liczba k tematów
{    
  corpus <- VCorpus(VectorSource(input_text))
  
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeWords, ai_stopwords)
  corpus <- tm_map(corpus, stripWhitespace)
  
  DTM <- DocumentTermMatrix(corpus)
  
  # usuń wszystkie puste wiersze w macierzy częstości
  # ponieważ spowodują błąd dla LDA
  unique_indexes <- unique(DTM$i) # pobierz indeks każdej unikalnej wartości
  DTM <- DTM[unique_indexes,]    # pobierz z DTM podzbiór tylko tych unikalnych indeksów
  
  # wykonaj LDA
  lda <- LDA(DTM, k = k, control = list(seed = 1010))
  topics <- tidy(lda, matrix = "beta") # pobierz słowa/tematy w uporządkowanym formacie tidy
  
  # pobierz dziesięć najczęstszych słów dla każdego tematu
  top_terms <- topics  %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) # uporządkuj słowa w malejącej kolejności informatywności
  
  # rysuj wykres (domyślnie plot = TRUE)
  if(plot == T){
    # dziesięć najczęstszych słów dla każdego tematu
    top_terms %>%
      mutate(term = reorder_within(term, beta, topic)) %>% # posortuj słowa według wartości beta 
      ggplot(aes(term, beta, fill = factor(topic))) + # rysuj beta według tematu
      geom_col(show.legend = FALSE) + # wykres kolumnowy
      facet_wrap(~ topic, scales = "free") + # każdy temat na osobnym wykresie
      labs(
        title = paste("Modelowanie tematów LDA ( k =",k,")"),
        x = NULL,
        y = "β (ważność słowa w temacie)"
      ) +
      coord_flip() +
      scale_x_reordered() + 
      theme_minimal() +
      scale_fill_brewer(palette = "Set1") +
      scale_y_continuous(n.breaks = 4,
                         labels = scientific)      
  }else{ 
    # jeśli użytkownik nie chce wykresu
    # wtedy zwróć listę posortowanych słów
    return(top_terms)
  }
}

#' # 2. Przygotowanie danych ----

#' ## 2.1. Parametry globalne ----

set.seed(1010)
TEXT_SAMPLE_SIZE <- 100000
LDA_SAMPLE_SIZE  <- 30000
ai_stopwords <- c(
  "ai", "artificial", "intelligence", "artificialintelligence",
  "rt", "amp", "https", "tco",
  "will", "can", "one", "new", "use", "using",
  "get", "make", "like", "via", "ekk")

#' ## 2.2. Wczytanie danych ----

data_path <- "C:/Users/Dawid/Desktop/projekt_psi/tweets_ai.csv"
tweets_raw <- read_csv(
  data_path,
  show_col_types = FALSE,
  col_select = c(
    id, date, time, tweet, language,
    replies_count, retweets_count, likes_count,
    hashtags
  )
)

glimpse(tweets_raw)

#' # 3. Czyszczenie danych ----

# Po wczytaniu danych ograniczono zbiór do tweetów w języku angielskim, usunięto puste rekordy oraz duplikaty. Dzięki temu analiza tekstowa jest bardziej spójna i mniej obciążająca obliczeniowo.
tweets_clean <- tweets_raw %>%
  mutate(
    date = ymd(date),
    tweet = as.character(tweet),
    tweet = str_squish(tweet)
  ) %>%
  filter(
    language == "en",
    !is.na(tweet),
    tweet != "",
    !is.na(date)
  ) %>%
  distinct(tweet, .keep_all = TRUE) %>%
  mutate(
    year = year(date),
    month = floor_date(date, unit = "month")
  )

#' ## 3.1. Czyszczenie tekstu ----

clean_text <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("http\\S+|www\\S+", " ") %>%
    str_replace_all("@\\w+", " ") %>%
    str_replace_all("&amp;", "and") %>%
    str_replace_all("#", "") %>%
    str_replace_all("[^a-z\\s]", " ") %>%
    str_squish()
}

tweets_clean <- tweets_clean %>%
  mutate(clean_tweet = clean_text(tweet)) %>%
  filter(clean_tweet != "")

head(tweets_clean$clean_tweet, 5)

#' # 4. Analiza trendów w czasie ----

# Sprawdzono jak zmieniała się liczba tweetów dotyczących AI w czasie
tweets_monthly <- tweets_clean %>%
  count(month, name = "n_tweets")

head(tweets_monthly)

ggplot(tweets_monthly, aes(x = month, y = n_tweets)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Liczba tweetów dotyczących AI w czasie",
    x = "Miesiąc",
    y = "Liczba tweetów"
  ) +
  theme_minimal()
#' ## 4.1. Próbkowanie danych ----

# Ze względu na duży rozmiar zbioru danych dalsze analizy tekstowe wykonano na losowej próbie tweetów, dzięki set.seed() mamy odtwarzalność wyników.
tweets_text_sample <- tweets_clean %>%
  slice_sample(n = min(TEXT_SAMPLE_SIZE, nrow(tweets_clean)))

cat("Liczba tweetów w próbie tekstowej:", nrow(tweets_text_sample))

#' ## 4.2. Tokenizacja i usuwanie stopwords ----

# Tekst tweetów został podzielony na pojedyncze słowa. Później usunięto słowa mało informacyjne np. stopwords i wybrane słowa związane z Twitterem
custom_stop_words <- tibble(
  word = ai_stopwords,
  lexicon = "custom"
)

all_stop_words <- bind_rows(stop_words, custom_stop_words)

tweet_words <- tweets_text_sample %>%
  select(id, date, year, month, clean_tweet) %>%
  unnest_tokens(word, clean_tweet) %>%
  anti_join(all_stop_words, by = "word") %>%
  filter(
    str_detect(word, "^[a-z]+$"),
    nchar(word) > 2
  )

head(tweet_words)

#' # 5. Analiza częstości słów ----

# Sprawdzono jakie słowa najczęściej pojawiają się w tweetach dot. AI
top_words <- tweet_words %>%
  count(word, sort = TRUE)

top_words_df <- data.frame(
  word = top_words$word,
  freq = top_words$n
)

# Wyświetl top 10
print(head(top_words_df, 10))

# Tabela najczęstszych słów
datatable(head(top_words_df, 30))


#' ## 5.1. Chmura słów ----

# Najczęściej pojawiające się pojęcia w analizowanych tweetach
top_words_for_cloud <- top_words %>%
  slice_max(n, n = 80)

wordcloud(
  words = top_words_for_cloud$word,
  freq = top_words_for_cloud$n,
  max.words = 80,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2"),
  scale = c(3, 0.5)
)

#' # 6. Analiza sentymentu ----

#' ## 6.1. Wczytanie słowników sentymentu ----

afinn <- read.csv("afinn.csv", stringsAsFactors = FALSE)
bing <- read.csv("bing.csv", stringsAsFactors = FALSE)
loughran <- read.csv("loughran.csv", stringsAsFactors = FALSE)
nrc <- read.csv("nrc.csv", stringsAsFactors = FALSE)

#' ## 6.2. Łączenie słów z słownikami ----

# Słownik Bing
tweets_bing <- tweet_words %>%
  inner_join(bing, by = "word")

# Słownik NRC
tweets_nrc <- tweet_words %>%
  inner_join(nrc, by = "word", relationship = "many-to-many")

# Słownik AFINN
tweets_afinn <- tweet_words %>%
  inner_join(afinn, by = "word")

# Słownik Loughran
tweets_loughran <- tweet_words %>%
  inner_join(loughran, by = "word", relationship = "many-to-many")

#' ## 6.3. Wykres Bing (Sentyment tweetów) ----

bing_count <- tweets_bing %>%
  count(sentiment)

ggplot(bing_count, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE, width = 0.75) +
  labs(
    title = "Sentyment tweetów (Słownik Bing)",
    x = "Sentyment",
    y = "Liczba słów"
  ) +
  theme_minimal()

#' ## 6.4. Wykres NRC (Emocje w tweetach, bez positive i negative) ----

nrc_count <- tweets_nrc %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  count(sentiment)

ggplot(nrc_count, aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE, width = 0.75) +
  coord_flip() +
  labs(
    title = "Emocje w tweetach (Słownik NRC)",
    x = "Rodzaj emocji",
    y = "Liczba słów"
  ) +
  theme_minimal()

#' ## 6.5. Wykres AFINN (Natężenie emocji w tweetach) ----

afinn_count <- tweets_afinn %>%
  count(value)

ggplot(afinn_count, aes(x = factor(value), y = n)) +
  geom_col(width = 0.75) +
  labs(
    title = "Natężenie emocji w tweetach (Słownik AFINN)",
    x = "Wartość",
    y = "Liczba słów"
  ) +
  theme_minimal()

#' ## 6.6. Wykres Loughran (Wydźwięk słów) ----

loughran_count <- tweets_loughran %>%
  count(sentiment)

ggplot(loughran_count, aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE, width = 0.75) +
  coord_flip() + 
  labs(
    title = "Wydźwięk słów (Słownik Loughran)",
    x = "Kategoria",
    y = "Liczba słów"
  ) +
  theme_minimal()

#' ## 6.7. Wykres Bing (Top 15 słów) ----

bing_top_words <- tweets_bing %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 15, with_ties = FALSE) %>%
  ungroup()

ggplot(bing_top_words, aes(x = reorder_within(word, n, sentiment), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Najczęściej występujące słowa (Słownik Bing)",
    x = NULL,
    y = "Liczba wystąpień słowa"
  ) +
  theme_minimal()

#' ## 6.8. Wykres NRC (Top 8 słów dla każdej emocji) ----

nrc_top_words <- tweets_nrc %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 8, with_ties = FALSE) %>%
  ungroup()

ggplot(nrc_top_words, aes(x = reorder_within(word, n, sentiment), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free", ncol = 3) +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Charakterystyczne słowa dla poszczególnych emocji (Słownik NRC)",
    x = NULL,
    y = "Liczba wystąpień słowa"
  ) +
  theme_minimal()

#' ## 6.9. Wykres AFINN (dla wartości -5, -4, 4, 5) ----

afinn_top_words <- tweets_afinn %>%
  filter(value %in% c(-5, -4, 4, 5)) %>%
  count(word, value, sort = TRUE) %>%
  group_by(value) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(is_positive = value > 0,
         value = factor(value, levels = c(-4, -5, 4, 5))
  )

ggplot(afinn_top_words, aes(x = reorder_within(word, n, value), y = n, fill = is_positive)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ value, scales = "free", ncol = 2,
             labeller = labeller(value = c("-4" = "Wartość AFINN: -4",
                                           "-5" = "Wartość AFINN: -5",
                                           "4" = "Wartość AFINN: 4",
                                           "5" = "Wartość AFINN: 5"))) +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Słowa o najsilniejszym nacechowaniu emocjonalnym (Słownik AFINN)",
    x = NULL,
    y = "Liczba wystąpień słowa"
  ) +
  theme_minimal()

#' # 7. Analiza TF-IDF w podziale na lata (2017-2021) ----

#' ## 7.1. Przygotowanie korpusu ----

# Pogrupowanie wszystkich tweetów z danego roku
tweets_by_year <- tweets_clean %>%
  group_by(year) %>%
  summarise(all_tweets = paste(clean_tweet, collapse = " "), .groups = "drop")

# Utworzenie korpusu
corpus_years <- VCorpus(VectorSource(tweets_by_year$all_tweets))

# Czyszczenie korpusu
corpus_years <- tm_map(corpus_years, removeWords, stopwords("english"))
corpus_years <- tm_map(corpus_years, removeWords, ai_stopwords)
corpus_years <- tm_map(corpus_years, stripWhitespace)

#' ## 7.2. Budowa macierzy TDM ----

tdm_tfidf <- TermDocumentMatrix(
  corpus_years,
  control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))
)

# Przekształcenie do zwykłej macierzy
tdm_tfidf_m <- as.matrix(tdm_tfidf)

# Nazwanie kolumn latami
colnames(tdm_tfidf_m) <- tweets_by_year$year

# Znalezienie top 10 słów dla każdego roku
top_tfidf_per_year <- as.data.frame(as.table(tdm_tfidf_m)) %>%
  setNames(c("word", "year", "tf_idf")) %>%
  group_by(year) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup()

#' ## 7.3. Wykres TF-IDF dla poszczególnych lat) ----

ggplot(top_tfidf_per_year, aes(x = reorder_within(word, tf_idf, year), y = tf_idf, fill = factor(year))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ year, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(
    labels = scientific,
    n.breaks = 3
  ) +
  labs(
    title = "Najbardziej charakterystyczne słowa dla poszczególnych lat (TF-IDF)",
    x = NULL,
    y = "Wartość wskaźnika TF-IDF"
  ) +
  theme_minimal()


#' # 8. Modelowanie tematów LDA na próbie 30 000 tweetów ----

#' ## 8.1. Próbkowanie danych ----

set.seed(1010)
lda_sample <- tweets_clean[sample(nrow(tweets_clean), LDA_SAMPLE_SIZE), ]

#' ## 8.2. Wywołanie LDA dla różnych wartości k ----

for (k in c(2, 3, 4, 6)) {
  print(top_terms_by_topic_LDA(lda_sample$clean_tweet, k = k))
}