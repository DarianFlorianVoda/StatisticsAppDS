lm(eruptions ~ waiting, data=faithful)
head(faithful)


eruption.lm = lm(eruptions ~ waiting, data=faithful)
coeffs = coefficients(eruption.lm); coeffs
waiting = 80 # the waiting time
duration = coeffs[1] + coeffs[2]*waiting
duration
plot(faithful$waiting, faithful$eruptions, xlab="Time waited", ylab="Eruption duration")
abline(lm(faithful$eruptions ~ faithful$waiting))

eruption.lm = lm(eruptions ~ waiting, data=faithful)
summary(eruption.lm)$r.squared

summary(eruption.lm)
