# Projeto 1 - Análise de Sentimento

## Etapa 1 - Pacotes e autentificação.


library(rtweet)
library(stringr)
library(ggplot2)
library(tm)
library(SnowballC)
library(stringi)
library(RColorBrewer)
library(wordcloud)
library(stringr)
library(plyr)
library(dplyr)
library(lattice)
library(Rstem)
library(SnowballC)
library(sentiment)
setwd("C:/Users/Lucas/Documents/R/DSA - R/Projetos/Projeto 1")


# Obtendo 200 tweets com a palavra russia.
tweet_df <-
  search_tweets("russia",
                n = 200,
                lang = "en",
                include_rts = FALSE)
names(tweet_df)

tweets <- tweet_df %>%
  select(
    user_id,
    status_id,
    created_at,
    screen_name,
    text,
    favorite_count,
    retweet_count,
    urls_expanded_url
  )



## Tratamendo dos dados coletados através de text mining



# Tratamento (limpeza, organização e transformação) dos dados coletados
tweetcorpus <- stri_trans_tolower(tweets$text)
tweetcorpus <- VCorpus(VectorSource(tweetcorpus))
tweetcorpus <- tm_map(tweetcorpus, removePunctuation)
tweetcorpus <-
  tm_map(tweetcorpus, removeWords, stopwords("portuguese"))


# WordCloud


wordcloud(
  tweetcorpus,
  min.freq = 4,
  scale = c(5, 1),
  random.color = F,
  max.word = 70,
  random.order = F,
  colors = brewer.pal(8, "Dark2")
)


tweetdm <- TermDocumentMatrix(tweetcorpus)
findFreqTerms(tweetdm, lowfreq = 11)

# Buscando associações
findAssocs(tweetdm, 'russia', 0.60)

# Removendo termos esparsos (não utilizados frequentemente)
tweet2tdm <- removeSparseTerms((tweetdm), sparse = 0.9)

# Criando escala nos dados
tweet2tdmscale <- scale(tweet2tdm)


# Matriz de distância
tweetdist <- dist(tweet2tdmscale, method = "euclidean")

# Preparando o dendograma
tweetfit <- hclust(tweetdist)

# Criando o dendograma (verificando como as palavras se agrupam)
plot(tweetfit)

# Verificando os grupos
cutree(tweetfit, k = 5)

# Visualizando os grupos de palavras no dendograma
rect.hclust(tweetfit, k = 4, border = "red")


## Análise de sentimento

# Criando uma função para avaliar o sentimento
sentimento.score = function(sentences,
                            pos.words,
                            neg.words,
                            .progress = 'none')
{
  # Criando um array de scores com lapply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   sentence = gsub("[[:punct:]]", "", sentence)
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   sentence = gsub("\\d+", "", sentence)
                   tryTolower = function(x)
                   {
                     y = NA
                     try_error = tryCatch(
                       tolower(x),
                       error = function(e)
                         e
                     )
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     return(y)
                   }
                   
                   sentence = sapply(sentence, tryTolower)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress = .progress)
  scores.df = data.frame(text = sentences, score = scores)
  return(scores.df)
}

# Mapeando as palavras positivas e negativas
pos = readLines("palavras_positivas.txt")
neg = readLines("palavras_negativas.txt")

# Criando uma massa de dados para teste
teste = c("Russia is the future",
          "Ukraine is awesome",
          "War could not be bad",
          "learn about war")

# Testando a função em nossa massa de dados dummy
testeSentimento = sentimento.score(teste, pos, neg)
class(testeSentimento)

scores = sentimento.score(tweets$text, pos, neg, .progress = 'text')

scores$muito.pos = as.numeric(scores$score >= 1)
scores$muito.neg = as.numeric(scores$score <= -1)

# Calculando total
numpos = sum(scores$muito.pos)
numneg = sum(scores$muito.neg)

totalScore = round(100 * numpos / (numpos + numneg))
head(scores)



# Histograma mostrando o score das palavras. Nesse caso nota-se que existem em maioria palavras neutras,
# porém há mais palavras positivas do que negativas

histogram(
  data = scores,
  ~ scores$score,
  main = "Análise de Sentimentos",
  xlab = "",
  sub = "Scores"
)


# Usando classificador Naive Bayes para análise de sentimento

# Classificando emoção
class_emo = classify_emotion(tweets$text, algorithm = "bayes", prior = 1.0)
emotion = class_emo[, 7]

# Substituindo NA's por "Desconhecido"
emotion[is.na(emotion)] = "Desconhecido"

# Classificando polaridade
class_pol = classify_polarity(tweets$text, algorithm = "bayes")
polarity = class_pol[, 4]

# Gerando um dataframe com o resultado
sent_df = data.frame(
  text = as.character(tweetdm),
  emotion = emotion,
  polarity = polarity,
  stringsAsFactors = FALSE
)

# Ordenando o dataframe
sent_df = within(sent_df, emotion <-
                   factor(emotion, levels = names(sort(
                     table(emotion), decreasing = TRUE
                   ))))

## Visualização


# Emoções encontradas
ggplot(sent_df, aes(x = emotion)) + geom_bar(aes(y = ..count.., fill = emotion)) + 
  scale_fill_brewer(palette = "Dark2") + labs(x = "Categorias", y = "Número de Tweets")

# Polaridade
ggplot(sent_df, aes(x = polarity)) +
  geom_bar(aes(y = ..count.., fill = polarity)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(x = "Categorias de Sentimento", y = "Número de Tweets")
