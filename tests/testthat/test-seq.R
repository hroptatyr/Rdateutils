test_that("FDate sequencing", {
	expect_equal(length(seq(as.FDate("2000-01-01"),as.FDate("2004-01-00"))), 366L+365L+365L+365L)
	expect_equal(length(seq.FDate("2001-01-01","2003-12-31")), 365L+365L+365L)
	expect_equal(seq(as.FDate(20000101L), "2000-02-29", by=7L), as.FDate(c("2000-01-01","2000-01-08","2000-01-15","2000-01-22","2000-01-29","2000-02-05","2000-02-12","2000-02-19","2000-02-26")))
	expect_equal(seq(as.FDate(20000101L), "2000-02-29", by=7L, from.last=TRUE), as.FDate(c("2000-01-04","2000-01-11","2000-01-18","2000-01-25","2000-02-01","2000-02-08","2000-02-15","2000-02-22","2000-02-29")))
	expect_equal(seq(as.FDate("2000-02-29"), 20000222L), as.FDate(c("2000-02-29","2000-2-28","2000-2-27","2000-2-26","2000-02-25","2000/02/24","2000-02-23","2000/02/22")))

	expect_equal(seq.FDate("2000-01-01","2000-12-23",by="1M"), as.FDate(c(20000101L, 20000201L, 20000301L, 20000401L, 20000501L, 20000601L, 20000701L, 20000801L, 20000901L, 20001001L, 20001101L, 20001201L)))
	expect_equal(seq.FDate("2000-01-01","2000-12-23",by="-1M"), as.FDate(integer(0)))
	expect_equal(seq.FDate("2000-12-23","2000-01-01",by="1M"), as.FDate(integer(0)))
	expect_equal(seq.FDate("2000-12-31","2000-01-01",by="-1M"), as.FDate(c(20001231L, 20001131L, 20001031L, 20000931L, 20000831L, 20000731L, 20000631L, 20000531L, 20000431L, 20000331L, 20000231L, 20000131L)))
	expect_equal(format(seq.FDate("2000-12-31","2000-01-01",by="-1M")), c("2000-12-31","2000-11-30","2000-10-31","2000-09-30","2000-08-31","2000-07-31","2000-06-30","2000-05-31","2000-04-30","2000-03-31","2000-02-29","2000-01-31"))

	expect_equal(seq.FDate("2000-01-01","2000-12-23",by="1M",from.last=T), as.FDate(c(20000123L, 20000223L, 20000323L, 20000423L, 20000523L, 20000623L, 20000723L, 20000823L, 20000923L, 20001023L, 20001123L, 20001223L)))
	expect_equal(seq.FDate("2000-01-01","2000-12-23",by="-1M",from.last=T), as.FDate(integer(0)))
	expect_equal(seq.FDate("2000-12-23","2000-01-01",by="1M",from.last=T), as.FDate(integer(0)))
	expect_equal(seq.FDate("2000-12-31","2000-01-01",by="-1M",from.last=T), as.FDate(c(20001201L, 20001101L, 20001001L, 20000901L, 20000801L, 20000701L, 20000601L, 20000501L, 20000401L, 20000301L, 20000201L, 20000101L)))
	expect_equal(format(seq.FDate("2000-12-31","2000-01-01",by="-1M",from.last=T)), c("2000-12-01","2000-11-01","2000-10-01","2000-09-01","2000-08-01","2000-07-01","2000-06-01","2000-05-01","2000-04-01","2000-03-01","2000-02-01","2000-01-01"))

	expect_equal(seq(as.FDate("2000-A"), as.FDate("2007-01-01"), ddur("1Y")), as.FDate(c("2000-A","2001-A","2002-A","2003-A","2004-A","2005-A","2006-A","2007-A")))
	expect_equal(seq(as.FDate("2000-S1"), as.FDate("2003-Q3"), ddur("6M")), as.FDate(c("2000-S1","2000-S2","2001-S1","2001-S2","2002-S1","2002-S2","2003-S1","2003-S2")))
	expect_equal(seq(as.FDate("2000-S1"), as.FDate("2003-Q3"), ddur("1Y6M")), as.FDate(c("2000-S1","2001-S2","2003-S1")))
	expect_equal(seq(as.FDate("2000-Q4"), as.FDate("2003-F"), ddur("6M")), as.FDate(c("2000-Q4","2001-Q2","2001-Q4","2002-Q2","2002-Q4")))
	expect_equal(seq(as.FDate("2004-J"), as.FDate("2003-Z")), as.FDate(c("2004-04","2004-03","2004-02","2004-01","2003-12")))

	expect_equal(seq(as.FDate("2000-2"), as.FDate("2000-H"), ddur("15D")), as.FDate(c(20000200L, 20000215L)))
})

test_that("ddur sequencing", {
	expect_equal(seq.ddur("P1M", "P7M"), as.ddur(c("P1M","P2M","P3M","P4M","P5M","P6M","P7M")))
	expect_true(!length(seq.ddur("P1M-5D", "P-7M8D", "1D")))
	expect_equal(seq.ddur("P-2M1D", "P4M10D", "P1M1D"), as.ddur(c("P-2M1D", "P-1M2D", "P3D", "P1M5D", "P2M6D", "P3M7D", "P4M8D")))
})
