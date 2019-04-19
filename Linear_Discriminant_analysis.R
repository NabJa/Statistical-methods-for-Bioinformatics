library(MASS)
library(ggplot2)
library(tidyr)
library(data.table)

#Generate random data with 3 vaiables (income, credit, spending). Valuable is grouping
valuable <- rbinom(100, 1, .5)
income <- rnorm(100, 10, 2)
#income[valuable == 1] <- income[valuable == 1] + 2
credit <- rnorm(100, 3, 1)
#credit[valuable == 0] <- credit[valuable == 0] - 2
spending <- rnorm(100, 5, 3)

#Convert and plot data
data <- data.table(valuable, income, credit, spending)
data1 <- melt(data, id.vars = "valuable")
ggplot(data = data1, aes(variable, value, color = as.factor(valuable))) + geom_jitter()

#Doing LDA
model <- lda(valuable ~ income + credit + spending, data = data)
plot(model)

# Assess the accuracy of the prediction
# percent correct for each category of valuable
ct <-table(data$valuable, model$class)
sum(diag(prop.table(ct)))
plot(model, dimen=1, type="b")


#Some pretty fancy plots. (partimat has lot of other options!)
library(klaR)
partimat(as.factor(valuable) ~ income + credit + spending,
         data = data ,method="lda", plot.matrix = T)


