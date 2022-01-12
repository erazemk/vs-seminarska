# Uvoz knjižnice
library(car)

mozgani <- read.csv("mozgani.csv", header = TRUE)

# Transformacija podatkov
lgmteza <- log(mozgani$mozteza)
lgtteza <- log(mozgani$telteza)

(model <- lm(lgmteza ~ lgtteza, data = mozgani))

mozgani[hatvalues(model) > 4/57,]
mozgani[abs(rstandard(model)) > 2,]

# Razsevni diagram
png("razsevni-diagram.png")
plot(
    x = log(mozgani$telteza),
    y = log(mozgani$mozteza),
    xlab = "Telesna teža sesalcev (v kg)",
    ylab = "Možganska teža sesalcev (v g)",
    xlim = c(-7, 10),
    ylim = c(-3, 10),
	axes = FALSE
)
axis(1, pos = -3, at = seq(-8, 10, by = 2))
axis(2, pos = -7, at = seq(-4, 10, by = 2))
dev.off()

# Graf za preverjanje linearnosti modela
png("linearnost-modela.png")
par(mar=c(4,4,1,1))
plot(model, which = 1, caption = "", ann = F)
title(
    xlab = expression(widehat(y) == widehat(a) + widehat(b) * x),
    ylab = "Ostanki"
)
dev.off()

# Graf normalnosti porazdelitve naključnih napak
png("normalnost-porazdelitve.png")
par(mar=c(4,4,1,1))
plot(model, which = 2, caption = "", ann = F)
title(xlab = "Teoretični kvantili", ylab = "St. ostanki")
dev.off()

# Graf homogenosti variance in ncvTest rezultat
png("homogenost-variance.png")
par(mar=c(4,4,1,1))
plot(model, which = 3, caption = "", ann = F)
title(
    xlab = expression(widehat(y) == widehat(a) + widehat(b) * x),
    ylab = expression(sqrt(paste("|St. ostanki|")))
)
dev.off()
ncvTest(model)

# Graf vpliva posameznih točk na model
png("vpliv-tock-na-model.png")
par(mar=c(4,4,1,1))
plot(model, which = 4, caption = "", ann = F)
title(xlab = "Meritev", ylab = "Cookova razdalja")
dev.off()

# Razsevni diagram s pobarvanimi točkami
png("razsevni-diagram-pobarvan.png")
plot(
    x = log(mozgani$telteza),
    y = log(mozgani$mozteza),
    xlab = "Telesna teža sesalcev (v kg)",
    ylab = "Možganska teža sesalcev (v g)",
    xlim = c(-7, 10),
    ylim = c(-3, 10),
	axes = FALSE,
    abline(model, lwd = 2)
)
axis(1, pos = -3, at = seq(-8, 10, by = 2))
axis(2, pos = -7, at = seq(-4, 10, by = 2))
points(
    log(mozgani$telteza)[c(29)],
    log(mozgani$mozteza)[c(29)],
    col="blue",
    pch=19
)
dev.off()

any(cooks.distance(model)[c(29)] >= qf(0.5, 2, 57))

# Poglavje 6
summary(model)

# 95% interval zaupanja
round(confint(model), 3)

# Interval predikcije
xlgteza = data.frame(lgtteza = log(c(100, 500, 2000)))
exp(predict(model, xlgteza, interval = "predict"))