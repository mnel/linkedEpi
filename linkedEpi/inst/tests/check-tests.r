context('Unique ppn identifer checks for ease of merging')




test_that('check.unique returns FALSE for easy test case',{
  df.non <- data.frame(ppn = c(1L,1L,2L), var1 = seq_len(3))
  df.unique <- data.frame(ppn = seq_len(3), var2 = seq_len(3))
  expect_that(check.unique(df.non), is_false())
  expect_that(check.unique(df.unique), is_true())
})

