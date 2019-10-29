test_that("wcnt from/to character", {
	expect_equal(format(as.wcnt("Sun")), "S")
	expect_equal(format(as.wcnt("-Sun")), "-1S")
	expect_equal(format(as.wcnt("2")), "2")
	expect_equal(as.wcnt("2Thu"), as.wcnt(2L, 4L))
	expect_equal(as.wcnt("-3"), as.wcnt(-3L, 0L))
	expect_equal(as.wcnt("R"), as.wcnt(0L, 4L))
})

test_that("wcnt arith", {
	expect_equal(as.wcnt("2") + 7L, as.wcnt("3"))
	expect_equal(as.wcnt("2") + 9L, as.wcnt(3L, 2L))
	expect_equal(as.wcnt("2") + -7L, as.wcnt("1"))
	expect_equal(as.wcnt("2") + -1L, as.wcnt(1L, 7L))
	expect_equal(as.wcnt("2") + -8L, as.wcnt("Sun"))

	expect_equal(as.wcnt("2R") + 7L, as.wcnt("3R"))
	expect_equal(as.wcnt("2R") + 9L, as.wcnt(3L, 6L))
	expect_equal(as.wcnt("2R") + -7L, as.wcnt("1R"))
	expect_equal(as.wcnt("2R") + -1L, as.wcnt(2L, 3L))
	expect_equal(as.wcnt("2R") + -8L, as.wcnt("1Wed"))
})

test_that("wcnt/ddur", {
	expect_equal(as.ddur(as.wcnt("10")), as.ddur(10L))
	expect_equal(as.ddur(as.wcnt("-4")), as.ddur("P-4D"))
})
