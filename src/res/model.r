mozgani <- read.csv("mozgani.csv", header = TRUE)
model <- lm(mozteza ~ telteza, data = mozgani)

png("linearnost-modela.png")
plot(model, which = 1, caption = "", ann = F)
title(
    xlab = expression(widehat(y) == widehat(a) + widehat(b) * x),
    ylab = "Ostanki"
)
dev.off()

png("normalnost-porazdelitve.png")
plot(model, which = 2, caption = "", ann = F)
title(xlab = "Teoreticni kvantili", ylab = "St. ostanki")
dev.off()

png("homogenost-variance.png")
plot(model, which = 3, caption = "", ann = F)
title(
    xlab = expression(widehat(y) == widehat(a) + widehat(b) * x),
    ylab = expression(sqrt(paste("|St. ostanki|")))
)
dev.off()

png("vpliv-tock-na-model.png")
plot(model, which = 4, caption = "", ann = F)
title(xlab = "Meritev", ylab = "Cookova razdalja")
dev.off()