### Equivalence testing ###

library(TOSTER)

# Table 9 (Section E)
TOSTtwo(m1 = .798106, m2 = .7878879, sd1 = .2767908, sd2 = .2792989, n1 = 988, n2 = 1004, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .7849557, m2 = .782714, sd1 = .2475573, sd2 = .2591889, n1 = 988, n2 = 1007, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .4473087, m2 = .4560795, sd1 = .3575155, sd2 = .3477307, n1 = 971, n2 = 981, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .737365, m2 = .7240271, sd1 = .2353684, sd2 = .237364, n1 = 985, n2 = 1005, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .7681823, m2 = .756747, sd1 = .26676, sd2 = .2526929, n1 = 988, n2 = 1008, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .7545037, m2 = .7418696, sd1 = .225613, sd2 = .2308087, n1 = 974, n2 = 1000, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .7682766, m2 = .7640976, sd1 = .2255043, sd2 = .2308947, n1 = 964, n2 = 972, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .753016, m2 = .737061, sd1 = .2063509, sd2 = .2059901, n1 = 970, n2 = 997, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)

# Table 10 (Section E)
TOSTtwo(m1 = .798106, m2 = 0.7785754, sd1 = .2767908, sd2 = 0.2806266, n1 = 988, n2 = 1001, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .7849557, m2 = 0.7666786, sd1 = .2475573, sd2 = .2709399, n1 = 988, n2 = 1002, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .4473087, m2 = 0.4663382, sd1 = .3575155, sd2 = 0.3501507, n1 = 971, n2 = 979, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .737365, m2 = .7082624, sd1 = .2353684, sd2 = .2392348, n1 = 985, n2 = 1006, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .7681823, m2 = 0.7372135, sd1 = .26676, sd2 = 0.2736245, n1 = 988, n2 = 999, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .7545037, m2 = 0.7414807, sd1 = .225613, sd2 = 0.2251814, n1 = 974, n2 = 995, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .7682766, m2 = .750427, sd1 = .2255043, sd2 = .2339921, n1 = 964, n2 = 968, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .753016, m2 = 0.729438, sd1 = .2063509, sd2 = 0.2071572, n1 = 970, n2 = 985, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)

# Table 11 (Section E)
TOSTtwo(m1 = .798106, m2 = 0.7609388, sd1 = .2767908, sd2 = .2999624 , n1 = 988, n2 = 1000, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .7849557, m2 = 0.7611126, sd1 = .2475573, sd2 = 0.2820858, n1 = 988, n2 = 1005, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .4473087, m2 =0.455724, sd1 = .3575155, sd2 = .3480356, n1 = 971, n2 = 978, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .737365, m2 = 0.6978697, sd1 = .2353684, sd2 = .2523018, n1 = 985, n2 = 1004, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .7681823, m2 = 0.7352476, sd1 = .26676, sd2 = .2796954 , n1 = 988, n2 = 1003, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .7545037, m2 =0.7169927, sd1 = .225613, sd2 = .251806, n1 = 974, n2 = 1000, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .7682766, m2 = 0.7398453, sd1 = .2255043, sd2 = .2480872, n1 = 964, n2 = 968, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)
TOSTtwo(m1 = .753016, m2 = 0.7155976, sd1 = .2063509, sd2 = 0.2229154, n1 = 970, n2 = 989, low_eqbound_d = -0.09, high_eqbound_d = 0.09, alpha = 0.05, var.equal = FALSE)



