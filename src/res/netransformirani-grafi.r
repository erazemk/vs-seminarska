mozgani <- read.csv("mozgani.csv", header = TRUE)
model <- lm(mozteza ~ telteza, data = mozgani)

which(cooks.distance(model) > 4/57)
any(cooks.distance(model)[c(16)] >= qf(0.5, 2, 57))
any(cooks.distance(model)[c(29)] >= qf(0.5, 2, 57))
any(cooks.distance(model)[c(30)] >= qf(0.5, 2, 57))

png("netrans-linearnost-modela.png")
par(mar=c(4,4,1,1))
plot(model, which = 1, caption = "", ann = F, main =)
title(
    xlab = expression(widehat(y) == widehat(a) + widehat(b) * x),
    ylab = "Ostanki"
)
dev.off()

png("netrans-normalnost-porazdelitve.png")
par(mar=c(4,4,1,1))
plot(model, which = 2, caption = "", ann = F)
title(xlab = "Teoretični kvantili", ylab = "St. ostanki")
dev.off()

png("netrans-homogenost-variance.png")
par(mar=c(4,4,1,1))
plot(model, which = 3, caption = "", ann = F)
title(
    xlab = expression(widehat(y) == widehat(a) + widehat(b) * x),
    ylab = expression(sqrt(paste("|St. ostanki|")))
)
dev.off()

png("netrans-vpliv-tock-na-model.png")
par(mar=c(4,4,1,1))
plot(model, which = 4, caption = "", ann = F)
title(xlab = "Meritev", ylab = "Cookova razdalja")
dev.off()