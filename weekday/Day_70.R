library(pwr)
# Power Student's t-test
pwr.t.test(d = 0.8, sig.level = 0.05, power = 0.9, type = 'two.sample')
pwr.t.test(n = 25, d = 0.5, sig.level = 0.01)

# for group with different length
pwr.t2n.test(n1 = 30, n2 = 25, sig.level = 0.05, d = 0.7)

# power anova test
pwr.anova.test(k = 5, f = 0.25, sig.level = 0.05, power = 0.8)

# power effect for correlation coef
pwr.r.test(r = 0.25, sig.level = 0.05, power = 0.9)

# power analysis for linear models
pwr.f2.test(u = 3, f2 = 0.0769, sig.level = 0.05, power = 0.9)

# power analysis for comparison of proportions
pwr.2p.test(h = ES.h(0.65, 0.6), sig.level = 0.05, power = 0.9, alternative = 'greater')

# chi-squared power test
pwr.chisq.test(w = 0.1853, df = 2, sig.level = 0.05, power = 0.9)