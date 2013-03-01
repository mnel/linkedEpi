library(testthat)

context('Unique ppn identifer checks for ease of merging')

test_that('check.unique returns FALSE for easy test case',{
  df.non <- data.frame(ppn = c(1L,1L,2L), var1 = seq_len(3))
  df.unique <- data.frame(ppn = seq_len(3), var2 = seq_len(3))
  expect_that(check.unique(df.non), is_false())
  expect_that(check.unique(df.unique), is_true())
})

test_that('merge.unique does not merge data sets with non-unique keys for easy test case',{
  df.non <- data.frame(ppn = c(1L,1L,2L), var1 = seq_len(3))
  df.unique <- data.frame(ppn = seq_len(3), var2 = seq_len(3))
  expect_error(merge.unique(df.non, df.unique, by="ppn"))
})

context('File sequence generation')
test_that('fileseq correctly adds a file seq column to a data frame', {
  df = data.frame(name=c("Bob the Builder", "Spongebob Squarepants"))
  expect_equal(fileseq(df),
               data.frame(name=c("Bob the Builder", "Spongebob Squarepants"),
                          fileseq=1:2))
})

test_that('morbseq correctly adds a morb seq column to a data frame', {
  df <- data.frame(ppn = c(1L,1L,2L), var1 = seq_len(3))
  expect_equal(morbseq(df, id="ppn"),
               data.frame(ppn = c(1L,1L,2L), var1 = seq_len(3),
                          morbseq=c(1,2,1)))
})
