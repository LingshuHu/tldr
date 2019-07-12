# source ~/.virtualenvs/r-tensorflow/bin/activate

devtools::install_github("rstudio/keras")
library(keras)
install_keras()
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

x_train[1,,]
length(x_train[1,1,])

x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
# rescale
x_train <- x_train / 255
x_test <- x_test / 255

y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)


model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 30, batch_size = 128,
  validation_split = 0.2
)

###################################################
################ text classification ##############
###################################################

library(keras)
library(dplyr)
library(ggplot2)
library(purrr)

imdb <- dataset_imdb(num_words = 10000)

c(train_data, train_labels) %<-% imdb$train
c(test_data, test_labels) %<-% imdb$test

word_index <- dataset_imdb_word_index()

## explore the data
paste0("Training entries: ", length(train_data), ", labels: ", length(train_labels))

train_data[2]
length(train_data[[1]])
length(train_data[[2]])

word_index_df <- data.frame(
  word = names(word_index),
  idx = unlist(word_index, use.names = FALSE),
  stringsAsFactors = FALSE
)

# The first indices are reserved
word_index_df <- word_index_df %>% dplyr::mutate(idx = idx + 3)
word_index_df <- word_index_df %>%
  add_row(word = "<PAD>", idx = 0)%>%
  add_row(word = "<START>", idx = 1)%>%
  add_row(word = "<UNK>", idx = 2)%>%
  add_row(word = "<UNUSED>", idx = 3)

word_index_df <- word_index_df %>% arrange(idx)

decode_review <- function(text){
  paste(map(text, function(number) word_index_df %>%
              filter(idx == number) %>%
              select(word) %>%
              pull()),
        collapse = " ")
}

decode_review(train_data[[1]])

train_data <- pad_sequences(
  train_data,
  value = word_index_df %>% filter(word == "<PAD>") %>% select(idx) %>% pull(),
  padding = "post",
  maxlen = 256
)

test_data <- pad_sequences(
  test_data,
  value = word_index_df %>% filter(word == "<PAD>") %>% select(idx) %>% pull(),
  padding = "post",
  maxlen = 256
)

length(train_data[1, ])
length(train_data[2, ])

train_data[1, ]

# input shape is the vocabulary count used for the movie reviews (10,000 words)
vocab_size <- 10000

model <- keras_model_sequential()
model %>%
  layer_embedding(input_dim = vocab_size, output_dim = 16) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% summary()

############### embedding with GloVe ########################
str(rownames(word_vectors))
str(word_index_df$word)
length(word_vectors[1,])
which(rownames(word_vectors ==)
word_vectors["nulla", ]

lines <- readLines(file.path('glove.6B/glove.6B.50d.txt'))
lines_split <- strsplit(lines, split = " ")
lines_split[1]
word_vectors_glove <- read.csv("glove.6B/glove.6B.50d.txt")
word_vectors_glove <- matrix(data = lines_split)
word_vectors_glove2 <- do.call("rbind", word_vectors_glove)
word_vectors_glove3 <- word_vectors_glove2[, 2:51]
rownames(word_vectors_glove3) <- word_vectors_glove2[, 1]
word_vectors_glove3["the", ]

embedding_matrix_diy <- matrix(0L, nrow = length(word_index_df$word), ncol = length(word_vectors[1,]))

matchs <- match(word_index_df$word, rownames(word_vectors))
i <- 0
for (word in word_index_df$word) {
  i <- i + 1
  if (word %in% rownames(word_vectors_glove3)) {
    embedding_matrix_diy[i, ] <- word_vectors_glove3[word, ]
  }
}

model <- keras_model_sequential()
model %>%
  layer_embedding(input_dim = length(word_index_df$word), output_dim = 50, weights = list(embedding_matrix_diy), trainable = FALSE) %>%
  layer_lstm(units = 100, return_sequences = T) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 16, activation = "sigmoid") %>%
  layer_dense(units = 1, activation = "sigmoid")

summary(model)

model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)

x_val <- train_data[1:10000, ]
partial_x_train <- train_data[10001:nrow(train_data), ]

y_val <- train_labels[1:10000]
partial_y_train <- train_labels[10001:length(train_labels)]

history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 20,
  batch_size = 512,
  validation_data = list(x_val, y_val),
  verbose=1
)

range(word_index_df$idx)

# finally, vectorize the text samples into a 2D integer tensor
tokenizer <- text_tokenizer(num_words=vocab_size)
tokenizer %>% fit_text_tokenizer(texts)

word_index <- tokenizer$



prepare_embedding_matrix <- function() {
  embedding_matrix <- matrix(0L, nrow = num_words, ncol = EMBEDDING_DIM)
  for (word in names(word_index)) {
    index <- word_index[[word]]
    if (index >= MAX_NUM_WORDS)
      next
    embedding_vector <- embeddings_index[[word]]
    if (!is.null(embedding_vector)) {
      # words not found in embedding index will be all-zeros.
      embedding_matrix[index,] <- embedding_vector
    }
  }
  embedding_matrix
}

#############################################################
################ Glove ######################################
#############################################################

install.packages("text2vec")
library(text2vec)

text8_file = "~/text8"
if (!file.exists(text8_file)) {
  download.file("http://mattmahoney.net/dc/text8.zip", "~/text8.zip")
  unzip ("~/text8.zip", files = "text8", exdir = "~/")
}
wiki = readLines(text8_file, n = 1, warn = FALSE)

# Create iterator over tokens
tokens = space_tokenizer(wiki)
# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens, progressbar = FALSE)
vocab = create_vocabulary(it)
vocab = prune_vocabulary(vocab, term_count_min = 5L)

# Use our filtered vocabulary
vectorizer = vocab_vectorizer(vocab)
# use window of 5 for context words
tcm = create_tcm(it, vectorizer, skip_grams_window = 5L)

glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
wv_main = glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01)
dim(wv_main)

wv_context = glove$components
dim(wv_context)

word_vectors = wv_main + t(wv_context)

berlin = word_vectors["paris", , drop = FALSE] -
  word_vectors["france", , drop = FALSE] +
  word_vectors["germany", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = berlin, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)


#############################################################
############# Regression ####################################
#############################################################
boston_housing <- dataset_boston_housing()

c(train_data, train_labels) %<-% boston_housing$train
c(test_data, test_labels) %<-% boston_housing$test

library(tibble)

column_names <- c('CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX', 'RM', 'AGE',
                  'DIS', 'RAD', 'TAX', 'PTRATIO', 'B', 'LSTAT')
train_df <- as_tibble(train_data)
colnames(train_df) <- column_names

train_df

# Test data is *not* used when calculating the mean and std.

# Normalize training data
train_data <- scale(train_data)

# Use means and standard deviations from training set to normalize test set
col_means_train <- attr(train_data, "scaled:center")
col_stddevs_train <- attr(train_data, "scaled:scale")
test_data <- scale(test_data, center = col_means_train, scale = col_stddevs_train)

train_data[1, ] # First training sample, normalized

build_model <- function() {

  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = dim(train_data)[2]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)

  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )

  model
}

model <- build_model()
model %>% summary()

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)

epochs <- 500

# Fit the model and store training stats
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)

library(ggplot2)

plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(ylim = c(0, 5))


library(reticulate)
virtualenv_list()
use_virtualenv("r-tensorflow")

kdata <- import("keras.datasets")
imdb <- kdata$imdb
imdb <- imdb$load_dat


##############################################################
######## pretrained word embeddings ##########################
##############################################################

library(keras)

GLOVE_DIR <- 'glove.6B'
TEXT_DATA_DIR <- '20_newsgroup'
MAX_SEQUENCE_LENGTH <- 1000
MAX_NUM_WORDS <- 20000
EMBEDDING_DIM <- 100
VALIDATION_SPLIT <- 0.2

# download data if necessary
download_data <- function(data_dir, url_path, data_file) {
  if (!dir.exists(data_dir)) {
    download.file(paste0(url_path, data_file), data_file, mode = "wb")
    if (tools::file_ext(data_file) == "zip")
      unzip(data_file, exdir = tools::file_path_sans_ext(data_file))
    else
      untar(data_file)
    unlink(data_file)
  }
}
download_data(GLOVE_DIR, 'http://nlp.stanford.edu/data/', 'glove.6B.zip')
download_data(TEXT_DATA_DIR, "http://www.cs.cmu.edu/afs/cs.cmu.edu/project/theo-20/www/data/", "news20.tar.gz")

# first, build index mapping words in the embeddings set
# to their embedding vector

cat('Indexing word vectors.\n')

embeddings_index <- new.env(parent = emptyenv())
lines <- readLines(file.path(GLOVE_DIR, 'glove.6B.100d.txt'))
for (line in lines) {
  values <- strsplit(line, ' ', fixed = TRUE)[[1]]
  word <- values[[1]]
  coefs <- as.numeric(values[-1])
  embeddings_index[[word]] <- coefs
}

cat(sprintf('Found %s word vectors.\n', length(embeddings_index)))

# second, prepare text samples and their labels
cat('Processing text dataset\n')

texts <- character()  # text samples
labels <- integer() # label ids
labels_index <- list()  # dictionary: label name to numeric id

for (name in list.files(TEXT_DATA_DIR)) {
  path <- file.path(TEXT_DATA_DIR, name)
  if (file_test("-d", path)) {
    label_id <- length(labels_index)
    labels_index[[name]] <- label_id
    for (fname in list.files(path)) {
      if (grepl("^[0-9]+$", fname)) {
        fpath <- file.path(path, fname)
        t <- readLines(fpath, encoding = "latin1")
        t <- paste(t, collapse = "\n")
        i <- regexpr(pattern = '\n\n', t, fixed = TRUE)[[1]]
        if (i != -1L)
          t <- substring(t, i)
        texts <- c(texts, t)
        labels <- c(labels, label_id)
      }
    }
  }
}

cat(sprintf('Found %s texts.\n', length(texts)))

# finally, vectorize the text samples into a 2D integer tensor
tokenizer <- text_tokenizer(num_words=MAX_NUM_WORDS)
tokenizer %>% fit_text_tokenizer(texts)

# save the tokenizer in case we want to use it again
# for prediction within another R session, see:
# https://keras.rstudio.com/reference/save_text_tokenizer.html
save_text_tokenizer(tokenizer, "tokenizer")

sequences <- texts_to_sequences(tokenizer, texts)

word_index <- tokenizer$word_index
cat(sprintf('Found %s unique tokens.\n', length(word_index)))

data <- pad_sequences(sequences, maxlen=MAX_SEQUENCE_LENGTH)
labels <- to_categorical(labels)

cat('Shape of data tensor: ', dim(data), '\n')
cat('Shape of label tensor: ', dim(labels), '\n')

# split the data into a training set and a validation set
indices <- 1:nrow(data)
indices <- sample(indices)
data <- data[indices,]
labels <- labels[indices,]
num_validation_samples <- as.integer(VALIDATION_SPLIT * nrow(data))

x_train <- data[-(1:num_validation_samples),]
y_train <- labels[-(1:num_validation_samples),]
x_val <- data[1:num_validation_samples,]
y_val <- labels[1:num_validation_samples,]

cat('Preparing embedding matrix.\n')

# prepare embedding matrix
num_words <- min(MAX_NUM_WORDS, length(word_index) + 1)
prepare_embedding_matrix <- function() {
  embedding_matrix <- matrix(0L, nrow = num_words, ncol = EMBEDDING_DIM)
  for (word in names(word_index)) {
    index <- word_index[[word]]
    if (index >= MAX_NUM_WORDS)
      next
    embedding_vector <- embeddings_index[[word]]
    if (!is.null(embedding_vector)) {
      # words not found in embedding index will be all-zeros.
      embedding_matrix[index,] <- embedding_vector
    }
  }
  embedding_matrix
}

embedding_matrix <- prepare_embedding_matrix()

# load pre-trained word embeddings into an Embedding layer
# note that we set trainable = False so as to keep the embeddings fixed
embedding_layer <- layer_embedding(
  input_dim = num_words,
  output_dim = EMBEDDING_DIM,
  weights = list(embedding_matrix),
  input_length = MAX_SEQUENCE_LENGTH,
  trainable = FALSE
)

cat('Training model\n')

# train a 1D convnet with global maxpooling
sequence_input <- layer_input(shape = list(MAX_SEQUENCE_LENGTH), dtype='int32')

preds <- sequence_input %>%
  embedding_layer %>%
  layer_conv_1d(filters = 128, kernel_size = 5, activation = 'relu') %>%
  layer_max_pooling_1d(pool_size = 5) %>%
  layer_conv_1d(filters = 128, kernel_size = 5, activation = 'relu') %>%
  layer_max_pooling_1d(pool_size = 5) %>%
  layer_conv_1d(filters = 128, kernel_size = 5, activation = 'relu') %>%
  layer_max_pooling_1d(pool_size = 35) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = length(labels_index), activation = 'softmax')


model <- keras_model(sequence_input, preds)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'rmsprop',
  metrics = c('acc')
)

model %>% fit(
  x_train, y_train,
  batch_size = 128,
  epochs = 10,
  validation_data = list(x_val, y_val)
)
