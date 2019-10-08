test_that("converting from/to character", {
	d <- c(
		"2000-01-01", "2000-01-31",
		"2000-02-01", "2000-02-28", "2000-02-29",
		"2000-03-01", "2000-03-31",
		"2000-04-01", "2000-04-30",
		"2000-05-01", "2000-05-31",
		"2000-06-01", "2000-06-30",
		"2000-07-01", "2000-07-31",
		"2000-08-01", "2000-08-31",
		"2000-09-01", "2000-09-30",
		"2000-10-01", "2000-10-31",
		"2000-11-01", "2000-11-30",
		"2000-12-01", "2000-12-31",
		"2001-01-01", "2001-01-31",
		"2001-02-01", "2001-02-28",
		"2001-03-01", "2001-03-31",
		"2001-04-01", "2001-04-31",
		"2001-05-01", "2001-05-31")

	expect_equal(format(as.FDate(d))[-33], d[-33])
	expect_equal(format(as.FDate("2001-04-31")), "2001-04-30")
	expect_equal(as.FDate("20140102"), as.FDate("2014-01-02"))
	expect_equal(as.FDate("20140102"), as.FDate("2014-1-2"))
	expect_equal(as.FDate(20140102L), as.FDate("2014-01-02"))

	expect_true(is.na(as.FDate("2014-01-32")))
	expect_true(is.na(as.FDate("20140132")))
	expect_true(is.na(as.FDate(20140132L)))
	expect_true(is.na(as.FDate("2014-13-02")))
	expect_true(is.na(as.FDate("20141302")))
	expect_true(is.na(as.FDate(20141302L)))
})

test_that("FDate accessors", {
	expect_equal(year(seq.FDate("2000-01-01","2000-12-31")), rep(2000L, 366L))
	expect_equal(yday(seq.FDate("2000-01-01","2000-12-31")), (1L:366L))
	expect_equal(yday(seq.FDate("2001-01-01","2001-12-31")), (1L:365L))
	expect_equal(semi(seq.FDate("2000-01-01","2000-07-00")), rep(1L, 31+29+31+30+31+30))

	expect_equal(semi(seq.FDate("2000-07-01","2001-01-00")), rep(2L, 31+31+30+31+30+31))
	expect_equal(sday(seq.FDate("2000-01-01","2000-06-31")), seq_len(31+29+31+30+31+30))
	expect_equal(sday(seq.FDate("2001-07-01","2001-12-31")), seq_len(31+31+30+31+30+31))

	expect_equal(quarter(seq.FDate("2000-01-01","2000-04-00")), rep(1L, 31+29+31))
	expect_equal(quarter(seq.FDate("2001-07-01","2001-10-00")), rep(3L, 31+31+30))
	expect_equal(qday(seq.FDate("2000-01-01","2000-12-31")), c(seq_len(31+29+31), seq_len(30+31+30), seq_len(31+31+30), seq_len(31+30+31)))

	expect_equal(month(seq.FDate("2000-01-10","2001-01-00",by="1M")), seq_len(12L))
	expect_equal(mday(seq.FDate("2000-01-01","2000-12-31")), unlist(lapply(c(31,29,31,30,31,30, 31,31,30,31,30,31), seq_len)))

	## 2018's isoweeks coincide with calendar
	expect_equal(week(seq.FDate("2018-01-01", "2018-12-30")), unlist(lapply(seq_len(52L), rep.int, times=7L)))
	expect_equal(wday(seq.FDate("2018-01-01", "2018-12-30")), rep.int(c(1,2,3,4,5,6,0), 52L))
})

test_that("FDate accessors", {
	f <- as.FDate("2000-02-29")
	year(f) <- 2001
	expect_equal(f, as.FDate("2001-02-29"))
	expect_equal(f, as.FDate("2001-02-28"))
	f <- as.FDate("2000-01-01")
	year(f) <- 2001
	expect_equal(f, as.FDate("2001-01-01"))
	f <- as.FDate("2000-12-31")
	year(f) <- 2001
	expect_equal(f, as.FDate("2001-12-31"))

	f <- as.FDate("2000-03-31")
	month(f) <- 1
	expect_equal(f, as.FDate("2000-01-31"))
	month(f) <- 4
	expect_equal(f, as.FDate("2000-04-31"))
	expect_equal(f, as.FDate("2000-04-30"))
	month(f) <- 2
	expect_equal(f, as.FDate("2000-02-31"))
	expect_equal(f, as.FDate("2000-02-30"))
	expect_equal(f, as.FDate("2000-02-29"))
	month(f) <- 12
	expect_equal(f, as.FDate("2000-12-29"))

	f <- as.FDate("2000-02-29")
	mday(f) <- 20
	expect_equal(f, as.FDate("2000-2-20"))
	mday(f) <- 31
	expect_equal(f, as.FDate("2000-2-29"))
	mday(f) <- 29
	expect_equal(f, as.FDate("2000-2-29"))

	f <- as.FDate("2000-04-29")
	mday(f) <- 31
	expect_equal(mday(f), 30L)
	expect_equal(f, as.FDate("2000-04-31"))
	expect_equal(f, as.FDate("2000-04-30"))
	mday(f) <- 01
	expect_equal(mday(f), 1L)
	mday(f) <- 29
	expect_equal(mday(f), 29L)
})

test_that("FDate poset", {
	d <- as.FDate(c("2014-01-02", "2013-02-02","2015-01-06"))
	expect_equal(min(d), d[2L])
	expect_equal(max(d), d[3L])
	expect_equal(range(d), c(d[2L], d[3L]))
})