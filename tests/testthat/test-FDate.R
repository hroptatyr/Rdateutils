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

	expect_true(is.na(as.FDate("2014-01-32")))
	expect_true(is.na(as.FDate("20140132")))
	expect_true(is.na(as.FDate("2014-13-02")))
	expect_true(is.na(as.FDate("20141302")))

	expect_equal(as.FDate("20140102"), as.FDate("2014-01-02"))
	expect_true(is.na(as.FDate("20140132")))
	expect_true(is.na(as.FDate("20141302")))
	expect_equal(as.FDate("2010402"), as.FDate("201-04-02"))
})

test_that("FDate from factor", {
	x <- c("2008-01-31", "2014-02-31", "2004-S1", "2004-S1", "2003-Q", "2000")
	y <- factor(x)
	expect_equal(as.FDate(x), as.FDate(y))
})

test_that("FDate from integer", {
	expect_equal(as.FDate(20140102L), as.FDate("2014-01-02"))
	expect_true(is.na(as.FDate(20140132L)))
	expect_true(is.na(as.FDate(20141302L)))
	expect_equal(as.FDate(20141200L), as.FDate("2014-12"))
	expect_equal(as.FDate(20140000L), as.FDate("2014-A"))
})

test_that("FDate v NAs", {
	expect_true(is.na(as.FDate(NA_character_)))
	expect_true(is.na(as.FDate(as.Date(NA_character_))))
	expect_true(is.na(as.FDate(NA_integer_)))
	expect_true(is.na(as.FDate(as.Date(NA_integer_, origin="1970-01-01"))))
	expect_true(is.na(as.FDate(NA)))
	expect_true(is.na(as.FDate(as.Date(NA))))
	expect_true(is.na(as.FDate(NA_real_, origin="1970-01-01")))
	expect_true(is.na(as.FDate(as.Date(NA_real_, origin="1970-01-01"))))
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
	expect_equal(wday(seq.FDate("2018-01-01", "2018-12-30")), rep.int(c(1,2,3,4,5,6,7), 52L))
})

test_that("FDate accessors bang", {
	f <- as.FDate("2000-02-29")
	year(f) <- 2001
	expect_equal(f, as.FDate("2001-02-29"))
	expect_equal(format(f), "2001-02-28")
	f <- as.FDate("2000-01-01")
	year(f) <- 2001
	expect_equal(f, as.FDate("2001-01-01"))
	f <- as.FDate("2000-12-31")
	year(f) <- 2001
	expect_equal(f, as.FDate("2001-12-31"))

	f <- as.FDate("2000-11-22")
	yday(f) <- yday(f)
	expect_equal(f, as.FDate("2000-11-22"))
	f <- as.FDate("2000-01-29")
	yday(f) <- 14
	expect_equal(f, as.FDate("2000-01-14"))
	yday(f) <- 60
	expect_equal(f, as.FDate("2000-02-29"))
	yday(f) <- 0
	expect_equal(f, as.FDate("2000-A"))
	yday(f) <- -1
	expect_equal(f, as.FDate("2000-12-31"))
	yday(f) <- -31
	expect_equal(f, as.FDate("2000-12-01"))
	yday(f) <- -32
	expect_equal(f, as.FDate("2000-11-30"))
	yday(f) <- -366
	expect_equal(f, as.FDate("2000-01-01"))
	yday(f) <- -367
	expect_equal(f, as.FDate("2000-A"))
	g <- f <- seq.FDate("2000-01-01", "2002-A")
	yday(f) <- yday(g)
	expect_equal(f, g)

	f <- as.FDate("2000-03-31")
	month(f) <- 1
	expect_equal(f, as.FDate("2000-01-31"))
	month(f) <- 4
	expect_equal(f, as.FDate("2000-04-31"))
	expect_equal(format(f), "2000-04-30")
	month(f) <- 2
	expect_equal(f, as.FDate("2000-02-31"))
	expect_equal(format(f), "2000-02-29")
	month(f) <- 12
	expect_equal(f, as.FDate("2000-12-31"))

	f <- as.FDate("2000-02-29")
	mday(f) <- 20
	expect_equal(f, as.FDate("2000-2-20"))
	mday(f) <- 31
	expect_equal(f, as.FDate("2000-2-31"))
	expect_true(as.Date(f) == as.Date("2000-2-29"))
	mday(f) <- 29
	expect_equal(f, as.FDate("2000-2-29"))
	mday(f) <- -1
	expect_equal(f, as.FDate("2000-2-29"))
	mday(f) <- 0
	expect_equal(f, as.FDate("2000-G"))
	mday(f) <- -2
	expect_equal(f, as.FDate("2000-2-28"))

	f <- as.FDate("2000-04-29")
	mday(f) <- 31
	expect_equal(mday(f), 30L)
	expect_equal(f, as.FDate("2000-04-31"))
	expect_equal(format(f), "2000-04-30")
	mday(f) <- 01
	expect_equal(mday(f), 1L)
	mday(f) <- 29
	expect_equal(mday(f), 29L)
	mday(f) <- -1
	expect_equal(f, as.FDate("2000-4-30"))
	mday(f) <- 0
	expect_equal(f, as.FDate("2000-J"))
	mday(f) <- -2
	expect_equal(f, as.FDate("2000-4-29"))
	f <- as.FDate("2000-Q1")
	mday(f) <- 0
	expect_equal(f, as.FDate("2000-01"))
	f <- as.FDate("2000-Q4")
	mday(f) <- 0
	expect_equal(f, as.FDate("2000-V"))
	f <- as.FDate("2000-S2")
	mday(f) <- 0
	expect_equal(f, as.FDate("2000-N"))
	f <- as.FDate("2000-S2")
	mday(f) <- 1
	expect_equal(f, as.FDate("2000-07-01"))
	f <- as.FDate("2001-A")
	mday(f) <- 2
	expect_equal(f, as.FDate("2001-01-02"))
	f <- as.FDate("2000-A")
	mday(f) <- 0
	expect_equal(f, as.FDate("2000-F"))

	f <- as.FDate("2000-Q1")
	sday(f) <- 0
	expect_equal(f, as.FDate("2000-S1"))
	sday(f) <- 1
	expect_equal(f, as.FDate("2000-01-01"))
	sday(f) <- 31
	expect_equal(f, as.FDate("2000-01-31"))
	sday(f) <- 32
	expect_equal(f, as.FDate("2000-02-01"))
	sday(f) <- 59
	expect_equal(f, as.FDate("2000-02-28"))
	sday(f) <- 60
	expect_equal(f, as.FDate("2000-02-29"))
	sday(f) <- 61
	expect_equal(f, as.FDate("2000-03-01"))
	sday(f) <- -1
	expect_equal(f, as.FDate("2000-06-30"))
	sday(f) <- -30
	expect_equal(f, as.FDate("2000-06-01"))
	sday(f) <- -31
	expect_equal(f, as.FDate("2000-05-31"))
	sday(f) <- -122
	expect_equal(f, as.FDate("2000-03-01"))
	sday(f) <- -123
	expect_equal(f, as.FDate("2000-02-29"))
	f <- as.FDate("2000-09")
	sday(f) <- 4
	expect_equal(f, as.FDate("2000-07-04"))
	sday(f) <- 0
	expect_equal(f, as.FDate("2000-S2"))
	f <- as.FDate("2000-08-01")
	sday(f) <- sday(f)
	expect_equal(f, as.FDate("2000-08-01"))
	sday(f) <- -1
	expect_equal(f, as.FDate("2000-12-31"))
	sday(f) <- 200
	expect_equal(f, as.FDate("2000-12-31"))
	sday(f) <- -200
	expect_equal(f, as.FDate("2000-S2"))

	f <- as.FDate("2000-Q1")
	qday(f) <- 0
	expect_equal(f, as.FDate("2000-Q1"))
	qday(f) <- 1
	expect_equal(f, as.FDate("2000-01-01"))
	qday(f) <- 31
	expect_equal(f, as.FDate("2000-01-31"))
	qday(f) <- 32
	expect_equal(f, as.FDate("2000-02-01"))
	qday(f) <- 59
	expect_equal(f, as.FDate("2000-02-28"))
	qday(f) <- 60
	expect_equal(f, as.FDate("2000-02-29"))
	qday(f) <- 61
	expect_equal(f, as.FDate("2000-03-01"))
	qday(f) <- -1
	expect_equal(f, as.FDate("2000-03-31"))
	qday(f) <- -31
	expect_equal(f, as.FDate("2000-03-01"))
	qday(f) <- -32
	expect_equal(f, as.FDate("2000-02-29"))
	qday(f) <- -33
	expect_equal(f, as.FDate("2000-02-28"))
	f <- as.FDate("2000-05")
	qday(f) <- 4
	expect_equal(f, as.FDate("2000-04-04"))
	qday(f) <- 0
	expect_equal(f, as.FDate("2000-Q2"))
	f <- as.FDate("2000-08-01")
	qday(f) <- qday(f)
	expect_equal(f, as.FDate("2000-08-01"))
	qday(f) <- 0
	expect_equal(f, as.FDate("2000-Q3"))
	qday(f) <- -1
	expect_equal(f, as.FDate("2000-09-30"))
	f <- as.FDate("2000-11-30")
	qday(f) <- 31+31
	expect_equal(f, as.FDate("2000-12-01"))
	qday(f) <- 0
	expect_equal(f, as.FDate("2000-Q4"))
	qday(f) <- 31+31
	expect_equal(f, as.FDate("2000-12-01"))
	qday(f) <- -1
	expect_equal(f, as.FDate("2000-12-31"))
	qday(f) <- 100
	expect_equal(f, as.FDate("2000-12-31"))
	qday(f) <- -100
	expect_equal(f, as.FDate("2000-Q4"))

	f <- as.FDate("2019-10-21")
	week(f) <- 42
	expect_equal(f, as.FDate("2019-10-14"))
	week(f) <- 1
	expect_equal(f, as.FDate("2018-12-31"))
	f <- as.FDate("2019-10-23")
	week(f) <- 43
	expect_equal(f, as.FDate("2019-10-23"))
	week(f) <- 53
	expect_equal(f, as.FDate("2020-01-01"))

	f <- as.FDate("2019-10-21")
	wday(f) <- 3
	expect_equal(f, as.FDate("2019-10-23"))
	wday(f) <- 0
	expect_equal(f, as.FDate("2019-10-27"))
	f <- as.FDate("2019-01-01")
	wday(f) <- 1
	expect_equal(f, as.FDate("2018-12-31"))
	f <- as.FDate("2019-12-31")
	wday(f) <- 3
	expect_equal(f, as.FDate("2020-01-01"))
})

test_that("FDate poset", {
	d <- as.FDate(c("2014-01-02", "2013-02-02","2015-01-06"))
	expect_equal(min(d), d[2L])
	expect_equal(max(d), d[3L])
	expect_equal(range(d), c(d[2L], d[3L]))
})

test_that("FDate iso week", {
	expect_equal(week.FDate("2019-02-02"), 5L)
	expect_equal(week.FDate("2019-06-02"), 22L)
	expect_equal(mweek.FDate("2019-02-02"), 0L)
	expect_equal(mweek.FDate("2019-06-02"), 0L)
	expect_equal(mweek.FDate("2019-06-03"), 1L)

	expect_equal(wcnt.FDate("2019-02-02"), 5L)
	expect_equal(wcnt.FDate("2019-06-02"), 22L)
	expect_equal(wcnt.FDate("2017-01-28"), 4L)
	expect_equal(wcnt.FDate("2017-01-29"), 5L)
	expect_equal(mwcnt.FDate("2019-02-02"), 1L)
	expect_equal(mwcnt.FDate("2019-06-02"), 1L)
	expect_equal(mwcnt.FDate("2019-06-03"), 1L)
})
