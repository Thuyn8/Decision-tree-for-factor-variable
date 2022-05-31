
# Plurality function returning a level of an attribute which has the most "votes"
plurality <- function(examples) {
  return(names(which.max(table(examples[, ncol(examples)]))))
}


# entropy function 
entropy <- function(responses) {
  p <- table(responses) / length(responses)
  total = 0
  for (i in 1:length(p)) {
    if (p[i] > 0)
      total <- total + p[i] * log2(p[i])
  }
  return(-total)
}

# importance function to look for the most important attribute 
importance <- function(a, examples) {
  responses <- examples[, ncol(examples)]
  start_entropy <- entropy(responses)
  remainder <- 0
  for (i in 1:length(unique(examples[, a]))) {
    level.i <- unique(examples[, a])[i]
    responses.i <- subset(responses, examples[, a] == level.i)
    p <- length(responses.i) / nrow(examples)
    remainder <- remainder + p * entropy(responses.i)
  }
  return(start_entropy - remainder)
}

# decision tree

learn.decision.tree <- function(examples, attributes, parent_examples) {
  if (nrow(examples) == 0) {
    return(plurality(parent_examples))
  } else if (length(unique(examples[, ncol(examples)])) == 1) {
    return(unique(examples[, ncol(examples)])[1])
  } else if (length(attributes) == 0) {
    return(plurality(examples))
  } else {
    max_importance = -Inf
    A = NULL
    for (a in attributes) {
      imp <- importance(a, examples)
      if (imp > max_importance) {
        max_importance <- imp
        A <- a
      }
    }
    tree <- list()
    tree$attribute <- A
    tree$children <- list()
    for (v in unique(examples[, A])) {
      exs <- subset(examples, examples[, A] == v)
      subtree <- learn.decision.tree(exs, attributes[!(attributes %in% A)], examples)
      tree$children[[as.character(v)]] <- subtree
    }
    return(tree)
  }
}

# Perdition for future input
dt.predict <- function(dt, newdata) {
  while (is.list(dt)) {
    dt <- dt$children[[levels(newdata[[dt$attribute]])[newdata[[dt$attribute]]]]]
  }
  return(dt)
}

## testing 

## creating a data called data
data <- data.frame(
  Alt = as.factor(c(T, T, F, T, T, F, F, F, F, T, F, T)),
  Bar = as.factor(c(F, F, T, F, F, T, T, F, T, T, F, T)),
  Fri = as.factor(c(F, F, F, T, T, F, F, F, T, T, F, T)),
  Hun = as.factor(c(T, T, F, T, F, T, F, T, F, T, F, T)),
  Pat = as.factor(c("Some", "Full", "Some", "Full", "Full", "Some", "None", "Some", "Full", "Full", "None", "Full")),
  Price = as.factor(c("$$$", "$", "$", "$", "$$$", "$$", "$", "$$", "$", "$$$", "$", "$")),
  Rain = as.factor(c(F, F, F, T, F, T, T, T, T, F, F, F)),
  Res = as.factor(c(T, F, F, F, T, T, F, T, F, T, F, F)),
  Type = as.factor(c("French", "Thai", "Burger", "Thai", "French", "Italian", "Burger", "Thai", "Burger", "Italian", "Thai", "Burger")),
  Est = as.factor(c("0-10", "30-60", "0-10", "10-30", ">60", "0-10", "0-10", "0-10", ">60", "10-30", "0-10", "30-60")),
  Will.Wait = as.factor(c(T, F, T, T, F, T, F, T, F, F, F, T))
)

## prediction
for (i in 1:nrow(data)){
  pred <-dt.predict(learn.decision.tree(data,colnames(data)[-ncol(data)], NULL), data[i,])
  print(pred)
}

