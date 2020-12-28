mozgani <- read.csv("mozgani.csv", header = TRUE)

png("razsevni-diagram.png")

plot(
    x = log(mozgani$telteza),
    y = log(mozgani$mozteza),
    xlab = "Telesna teza sesalcev",
    ylab = "Mozganska teza sesalcev",
    xlim = c(-7, 10),
    ylim = c(-3, 10),
		axes = FALSE
)

axis(1, pos = -3, at = seq(-8, 10, by = 2))
axis(2, pos = -7, at = seq(-4, 10, by = 2))

dev.off()