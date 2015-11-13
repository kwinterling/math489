library(ggplot2)

au_model <- read.csv("ausmodeldata.csv")
au_test <- read.csv("austestdata.csv")

audata <- rbind(au_model, au_test)

linear_model <- function() {
    linear_fit <- lm(Population ~ Date, data = au_model)
    ggplot(data = audata, aes(x = Date, y = Population)) +
        geom_point() +
        stat_smooth(data = au_model,
            aes(x = Date, y = Population),
            method = "lm",
            formula = y ~ x,
            fullrange = TRUE) +
        ggtitle("Plot of Austin Population with Linear Model")
    ggsave("auslinear.png")
}

quadratic_model <- function() {
    ggplot(data = audata, aes(x = Date, y = Population)) +
        geom_point() +
        stat_smooth(data = au_model,
            method = "lm",
            formula = y ~ poly(x, 2),
            fullrange = TRUE) +
        ggtitle("Austin Population with Quadratic Model")
    ggsave("ausquad.png")
}

powerlaw_model <- function() {
    ggplot(data = audata, aes(x = log(Date - 1839), y = log(Population))) +
        geom_point() +
        stat_smooth(data = au_model,
            aes(x = log(Date - 1839), y = log(Population)),
            method = "lm",
            formula = y ~ x,
            fullrange = TRUE) +
        ggtitle("Log-Log Plot with Power Law Fit")
    ggsave("auspowerlaw.png")
}

exp_model <- function() {
    ggplot(data = audata, aes(x = (Date - 1839), y = log(Population))) +
        geom_point() +
        stat_smooth(data = au_model,
            aes(x = (Date - 1839), y = log(Population)),
            method = "lm",
            formula = y ~ x,
            fullrange = TRUE) +
        ggtitle("Linear-Log Plot")
    ggsave("ausexp.png")
}

run_all <- function() {
    linear_model()
    quadratic_model()
    powerlaw_model()
    exp_model()
}
