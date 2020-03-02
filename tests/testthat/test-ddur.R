test_that("ddur from/to character", {
	expect_true(is.ddur(as.ddur("P0")))
	expect_true(is.ddur(as.ddur("P2W")))
	expect_true(is.ddur(as.ddur("P2Y2M")))
	expect_true(is.ddur(as.ddur("P2W2D")))
	expect_true(is.ddur(as.ddur("P2Y2M2W2D")))

	expect_equal(format(as.ddur("0")), "P0D")
	expect_equal(format(as.ddur(0L)), "P0D")
	expect_equal(format(as.ddur("P2W")), "P14D")
	expect_equal(format(as.ddur("P-2W")), "P-14D")
	expect_equal(format(as.ddur(-14L)), "P-14D")
	expect_equal(format(as.ddur("P2W2D")), "P16D")
	expect_equal(format(as.ddur("P2Y2M2W2D")), "P26M16D")

	expect_equal(format(as.ddur("ON")), "P1D")
	expect_equal(format(as.ddur("TN")), "P2D")
	expect_equal(format(as.ddur("SN")), "P3D")
	expect_true(is.na(as.ddur("XN")))
	expect_true(is.na(as.ddur("")))
})

test_that("ddur from factor", {
	x <- c("ON", "SN", "TN", "SN", "200", "200D", "P2M")
	y <- factor(x)
	expect_equal(as.ddur(x), as.ddur(y))

	x <- c("ON", "SN", "TN", "SN", "200", "200D", "P2M")
	y <- ordered(x, levels=c("ON","SN","TN"))
	expect_equal(as.ddur(y), as.ddur(c("ON","SN","TN","SN",NA,NA,NA)))
})

test_that("ddur accessors", {
	f <- as.ddur("P19M15D")
	expect_equal(year(f), 1L)
	expect_equal(semi(f), 3L)
	expect_equal(quarter(f), 6L)
	expect_equal(month(f), 19L)
	expect_equal(week(f), 2L)
	expect_equal(dday(f), 15L)
})

test_that("ddur accessors bang", {
	f <- as.ddur("P19M15D")
	year(f) <- 2
	expect_equal(f, as.ddur("P24M15D"))
	month(f) <- -1
	expect_equal(f, as.ddur("P-1M15D"))
	week(f) <- 2
	expect_equal(f, as.ddur("P-1M21D"))
	dday(f) <- -1
	expect_equal(f, as.ddur("P-1M-1D"))
})

test_that("ddur trunc'ing", {
	expect_equal(trunc(as.ddur("P19M15D"), "year"), as.ddur("P12M"))
	expect_equal(trunc(as.ddur("P103M-15D"), "year"), as.ddur("P96M"))
	expect_equal(trunc(as.ddur("P-19M15D"), "year"), as.ddur("P-12M"))
	expect_equal(trunc(as.ddur("P-12M15D"), "year"), as.ddur("P0M"))
	expect_equal(trunc(as.ddur("P19M15D"), "month"), as.ddur("P19M"))
	expect_equal(trunc(as.ddur("P-19M-15D"), "month"), as.ddur("P-19M"))
	expect_equal(trunc(as.ddur("P-19M15D"), "month"), as.ddur("P-18M"))
	expect_equal(trunc(as.ddur("P15D"), "week"), as.ddur("P14D"))
	expect_equal(trunc(as.ddur("P-15D"), "week"), as.ddur("P-14D"))
	expect_equal(trunc(as.ddur("P-20D"), "week"), as.ddur("P-14D"))
	expect_equal(trunc(as.ddur("P1M"), "week"), as.ddur("P28D"))
	expect_equal(trunc(as.ddur("P2M"), "day"), as.ddur("P61D"))
	expect_equal(trunc(as.ddur("P-2M1D"), "day"), as.ddur("P-60D"))
	expect_equal(trunc(as.ddur("P12M"), "day"), as.ddur("P365D"))
	expect_equal(trunc(as.ddur("P12M-1D"), "day"), as.ddur("P364D"))
	expect_equal(trunc(as.ddur("P-12M1D"), "day"), as.ddur("P-364D"))
})
