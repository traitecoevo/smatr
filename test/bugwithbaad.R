
dat <- read.csv("test/baaddf.csv")

sm <- sma(y ~ x*Group, data=dat, quiet=TRUE, log="xy")
