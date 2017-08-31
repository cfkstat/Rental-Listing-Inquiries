## data import
packages <- c("jsonlite", "dplyr", "purrr", "tidytext", "ggplot2", "ggthemes",
              "rpart.plot", "RecordLinkage","sampling", "caret", "xgboost",
              "pROC", "ROCR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)
# "rattle",
read.fromJson <- function(filepath){
  data <- fromJSON(filepath)
  var <- setdiff(names(data), c("photos","features"))
  data <- map_at(data, var, unlist) %>%
    tibble::as_tibble(.) %>%
    mutate(feature_count = lengths(features)) %>%
    mutate(photo_count = lengths(photos)) %>%
    select(-features, -photos) %>%
    data.frame
}

rental_data <- read.fromJson("train.json")
# test_data <- read.fromJson("test.json")

test_data <- test_data
## data exploration
glimpse(rental_data)


# The distribution of Target.
theme_set(theme_economist())
plot.theme <- theme(
  legend.position = "bottom",
  axis.text.x=element_text(size=12),
  axis.text.y=element_text(size=12),
  axis.title.x=element_text(size=14),
  axis.title.y=element_text(size=14),
  axis.title = element_text(size=16, vjust=3),
  plot.title = element_text(hjust = 0.5)
)
rental_data %>%
  group_by(interest_level) %>%
  count() %>%
  arrange(desc(n)) %>%
  ggplot(aes(x=interest_level, y=n, fill=interest_level)) +
  geom_bar(stat="identity") +
  labs(x = "interest level", y = "Freq", title = "The distribution of interse level") +
  geom_text(aes(label = n, vjust=-0.2)) +
  plot.theme




rental_data %>%
  group_by(interest_level, bathrooms) %>%
  count() %>%
  arrange(desc(bathrooms)) %>%
  ggplot(aes(x=as.character(bathrooms), y=n, fill=bathrooms)) +
  geom_bar(stat="identity") +
  facet_wrap(~interest_level) +
  coord_flip() +
  labs(x = "bath room count", y = "Freq", title = "The distribution of bath rooms number by interest level") +
  geom_text(aes(label = n, hjust=-0.2)) +
  plot.theme

rental_data %>%
  group_by(interest_level, feature_count) %>%
  count() %>%
  arrange(desc(feature_count)) %>%
  ggplot(aes(x=as.factor(feature_count), y=n, fill=feature_count)) +
  geom_bar(stat="identity") +
  facet_wrap(~interest_level) +
  coord_flip() +
  labs(x = "feature count", y = "Freq", title = "fatures count interset level") +
  geom_text(aes(label = n, hjust=0.5)) +
  theme(legend.position = "bottom")  +
  plot.theme


rental_data %>%
  group_by(interest_level, photo_count) %>%
  count() %>%
  dplyr::arrange(desc(photo_count)) %>%
  ggplot(aes(x=as.factor(photo_count), y=n, fill=photo_count)) +
  geom_bar(stat="identity") +
  facet_wrap(~interest_level) +
  coord_flip() +
  labs(x = "photo count", y = "Freq", title = "photos count by interset level") +
  geom_text(aes(label = n, hjust=-0.2)) +
  plot.theme




detect_outlier <- function(column) {
  iqr = quantile(column, .75) - quantile(column, .25)
  low = quantile(column, .25) - 1.5 * iqr
  high = quantile(column, .75) + 1.5 * iqr
  outlier_index = ifelse(column < low | column > high, TRUE, FALSE)
  results <- list("index" = outlier_index, "values"  = column[outlier_index], 'low' = low, 'high' = high, 'iqr' = iqr)
  return(results)
}

rental_data %>%
  ggplot(aes(x=interest_level, y=price)) +
  geom_violin((aes(fill = interest_level))) +
  geom_boxplot(width=.1, fill="black", outlier.color="red") +
    stat_summary(fun.y=median, geom="point",
                 fill="blue", shape=21, size=2.5) +
  ylim(0, quantile(rental_data$price, .99)) +
  plot.theme

rental_data %>%
  ggplot(aes(x=interest_level, y=longitude)) +
  geom_violin((aes(fill = interest_level))) +
  geom_boxplot(width=.1, fill="black", outlier.color="red") +
  stat_summary(fun.y=median, geom="point",
               fill="blue", shape=21, size=2.5) +
  ylim(0, quantile(rental_data$longitude, .99)) +
  plot.theme

rental_data %>%
  ggplot(aes(x=interest_level, y=latitude)) +
  geom_violin((aes(fill = interest_level))) +
  geom_boxplot(width=.1, fill="black", outlier.color="red") +
  stat_summary(fun.y=median, geom="point",
               fill="blue", shape=21, size=2.5) +
  ylim(0, quantile(rental_data$latitude, .99)) +
  plot.theme


# From the numer plot, we should deal with the outlier observations.

rental_data <- rental_data %>%
  filter(!detect_outlier(price)$index) %>%
  filter(!detect_outlier(latitude)$index) %>%
  filter(!detect_outlier(longitude)$index)



rental_data %>%
  ggplot(aes(x=price, y=..density..)) +
  geom_histogram(fill="cornsilk", colour="red", size=.2,
                 binwidth = diff(range(rental_data$price))/20) +
  geom_density() +
  facet_grid(.~interest_level) +
  labs(title = "The distribution of price by interset level") +
  plot.theme

rental_data %>%
  ggplot(aes(x=latitude, y=..density..)) +
  geom_histogram(fill="cornsilk", colour="red", size=.2,
                 binwidth = diff(range(rental_data$latitude))/20) +
  geom_density() +
  facet_grid(.~interest_level) +
  labs(title = "The distribution of latitude by interset level") +
  plot.theme

rental_data %>%
  ggplot(aes(x=longitude, y=..density..)) +
  geom_histogram(fill="cornsilk", colour="red", size=.2,
                 binwidth = diff(range(rental_data$longitude))/20) +
  geom_density() +
  facet_grid(.~interest_level) +
  labs(title = "The distribution of latitude by interset level") +
  plot.theme

# Variable Transform

##1. Creating Features from Street Address and Display Address
rental_data <- rental_data %>%
  mutate(distance = levenshteinSim(tolower(street_address),tolower(display_address))) %>%
  select(bathrooms, bedrooms,latitude, longitude, price, interest_level, feature_count, photo_count, distance)

test_data <- test_data %>%
  mutate(distance = levenshteinSim(tolower(street_address),tolower(display_address))) %>%
  select(bathrooms, bedrooms,latitude, longitude, price, feature_count, photo_count, distance)

sample.flag <- strata(rental_data, stratanames = "interest_level",
                      size = 3*rep(min(table(rental_data$interest_level)),3),
                      method = "srswr")


## data partition
train.data <- rental_data[sample.flag$ID_unit,]
test.data <- rental_data[-sample.flag$ID_unit,]


train_matrix <- Matrix::sparse.model.matrix(interest_level ~ .-1, data = train.data)

test_matrix <- Matrix::sparse.model.matrix(interest_level ~ .-1, data = test.data)

dtrain <- xgb.DMatrix(train_matrix,
                      label = as.integer(as.factor(train.data$interest_level))-1)
dtest <- xgb.DMatrix(test_matrix, label = as.integer(as.factor(test.data$interest_level))-1)



param <- list(objective = "multi:softprob",
              booster="gbtree", eval_metric = "merror",
              max.depth = 40,eta = 0.05, nthread =8,
              alpha=0.0001)

watchlist <- list(eval = dtest, train = dtrain)

system.time(
  fit.xgboost.tree <- xgb.train(
    nrounds = 50,
    verbose = 1,
    params = param,
    num_class = 3,
    dtrain,
    watchlist)
)

importanceRaw <- xgb.importance(train_matrix@Dimnames[[2]], model = fit.xgboost.tree, data = train_matrix, label = label)

# Cleaning for better display
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequence=NULL)]
#同时去掉cover frequence
head(importanceClean)

xgb.plot.importance(importance_matrix = importanceRaw)


require(RCurl)
sit = getURLContent('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)
con = gzcon(rawConnection(sit, 'rb'))
source(con)
close(con)


fit.xgboost.tree.train.prob <- predict(fit.xgboost.tree, train_matrix)
base_num <- 1:nrow(train_matrix)
high <- fit.xgboost.tree.train.prob[base_num]
low <- fit.xgboost.tree.train.prob[nrow(train_matrix)+base_num]
medium <- fit.xgboost.tree.train.prob[2*nrow(train_matrix)+base_num]
fit.xgboost.tree.train.class <- data.frame(high, low, medium)

tree.train.result <- factor(apply(fit.xgboost.tree.train.class,1, function(x) which.max(x)-1))
levels(tree.train.result) <- c("high", "low", "medium")
tree.predict.result <- data.frame(actual = train.data$interest_level,
                             predict = train.result)

tree.train.error <- mean(tree.train.result==as.factor(train.data$interest_level))
plot.table(round(as.matrix(prop.table(table(tree.predict.result))), 4),
           smain=paste0('Accuracy: ',round(tree.train.error,2)), highlight = TRUE, colorbar = TRUE)


fit.xgboost.tree.test.prob <- predict(fit.xgboost.tree, test_matrix)
base_num <- 1:nrow(test_matrix)
high <- fit.xgboost.tree.test.prob[base_num]
low <- fit.xgboost.tree.test.prob[nrow(test_matrix)+base_num]
medium <- fit.xgboost.tree.test.prob[2*nrow(test_matrix)+base_num]
fit.xgboost.tree.test.class <- data.frame(high, low, medium)

tree.test.result <- factor(apply(fit.xgboost.tree.test.class,1, function(x) which.max(x)-1))
levels(tree.test.result) <- c("high", "low", "medium")
tree.predict.test.result <- data.frame(actual = test.data$interest_level,
                             predict = tree.test.result)
tree.test.error <- mean(tree.test.result==as.factor(test.data$interest_level))
plot.table(round(as.matrix(prop.table(table(tree.predict.test.result))), 4),
           smain=paste0('Accuracy: ',round(tree.test.error,2)), highlight = TRUE, colorbar = TRUE)





param <- list(objective = "multi:softprob",
              booster="gblinear", eval_metric = "merror",
              max.depth = 40,eta = 0.05, nthread =8,
              alpha=0.0001)

watchlist <- list(eval = dtest, train = dtrain)

system.time(
  fit.xgboost.linear <- xgb.train(
    nrounds = 50,
    verbose = 1,
    params = param,
    num_class = 3,
    dtrain,
    watchlist)
)





fit.xgboost.linear.train.prob <- predict(fit.xgboost.linear, train_matrix)
base_num <- 1:nrow(train_matrix)
high <- fit.xgboost.linear.train.prob[base_num]
low <- fit.xgboost.linear.train.prob[nrow(train_matrix)+base_num]
medium <- fit.xgboost.linear.train.prob[2*nrow(train_matrix)+base_num]
fit.xgboost.linear.train.class <- data.frame(high, low, medium)

linear.train.result <- factor(apply(fit.xgboost.linear.train.class,1, function(x) which.max(x)-1))
levels(linear.train.result) <- c("high", "low", "medium")
linear.predict.result <- data.frame(actual = train.data$interest_level,
                                    predict = linear.train.result)
linear.train.error <- mean(linear.train.result==as.factor(train.data$interest_level))
plot.table(round(as.matrix(prop.table(table(linear.predict.result))), 4),
           smain=paste0('Accuracy: ',round(linear.train.error,2)), highlight = TRUE, colorbar = TRUE)


fit.xgboost.linear.test.prob <- predict(fit.xgboost.linear, test_matrix)
base_num <- 1:nrow(test_matrix)
high <- fit.xgboost.linear.test.prob[base_num]
low <- fit.xgboost.linear.test.prob[nrow(test_matrix)+base_num]
medium <- fit.xgboost.linear.test.prob[2*nrow(test_matrix)+base_num]
fit.xgboost.linear.test.class <- data.frame(high, low, medium)

linear.test.result <- factor(apply(fit.xgboost.linear.test.class,1, function(x) which.max(x)-1))
levels(linear.test.result) <- c("high", "low", "medium")
linear.predict.test.result <- data.frame(actual = test.data$interest_level,
                                  predict = linear.test.result)
linear.test.error <- mean(linear.test.result==as.factor(test.data$interest_level))
plot.table(round(as.matrix(prop.table(table(linear.predict.test.result))), 4),
           smain=paste0('Accuracy: ',round(linear.test.error,2)), highlight = TRUE, colorbar = TRUE)



