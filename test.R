source("vlog.R")

logloss <- function(p, y) {
  y <- as.integer(as.character(y))
  sum(y*log(p) + (1-y)*log(1-p))
}

vv <- vlog(0)
data(iris)
iris <- iris[1:100, ]
iris$Species <- factor(as.integer(iris$Species == "setosa"))
model <- glm(Species ~ ., data = iris, family = binomial(link = vv))
logit_model <- glm(Species ~ ., data = iris, family = binomial)
summary(logit_model)
p <- predict(logit_model, iris, type = "response")
logloss(p, iris$Species)
p <- predict(model, iris, type = "response")
logloss(p, iris$Species)
