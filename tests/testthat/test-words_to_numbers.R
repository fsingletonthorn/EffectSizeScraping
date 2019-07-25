context("written numbers to numerics")

test_that("Numbers to numerics works on a chunk of text", {
example_numerics <- "1213. 133. The PRQ is a 12-item, 4-point Likert scale
(from 1 = Never to 4 = Very Often) with 3 sub-scores
bullying (PRQ-Bully), being victimized (PRQ-Victim),
and pro-social behavior (PRQ-Prosocial). A translated,
backtranslated final Arabic version of the scale was found
to be accurate showing good internal consistency in this
sample [“PRQ-Victim” (alpha = .74)
and “PRQ-Bullies” (alpha = 74)]. 1200. And 12"

example <- "One-thousand, two hundred and thirteen. One-hundred and thirty three. The PRQ is a twelve-item, four-point Likert scale
(from one = Never to four = Very Often) with three sub-scores
bullying (PRQ-Bully), being victimized (PRQ-Victim),
and pro-social behavior (PRQ-Prosocial). A translated,
backtranslated final Arabic version of the scale was found
to be accurate showing good internal consistency in this
sample [“PRQ-Victim” (alpha = .seventy four)
and “PRQ-Bullies” (alpha = seventy-four)]. One thousand two hundred. And twelve"

expect_true(words_to_numbers(example) == example_numerics)
})

test_that("words to numbers works", {
  expect_identical(words_to_numbers("one hundred"),
                   "100")
})

test_that("words to numbers works", {
  expect_identical(words_to_numbers("one hundred two"),
                   "102")
})

test_that("words to numbers works", {
  expect_identical(words_to_numbers("one hundred and two"),
                   "102")
})

test_that("words to numbers works", {
  expect_identical(words_to_numbers("one hundred and five"),
                   "105")
})


test_that("words to numbers works", {
  expect_identical(words_to_numbers("one hundred and twenty five"),
                   "125")
})

test_that("words to numbers works", {
  expect_identical(words_to_numbers("four thousand and thirty"),
                   "4030")
})

test_that("words to numbers works", {
  expect_identical(words_to_numbers("six million five thousand and two"),
                   "6005002")
})

test_that("words to numbers works", {
  expect_identical(words_to_numbers("six million five thousand and two"),
                   "6005002")
})

test_that("words to numbers works", {
  expect_identical(words_to_numbers("sixty Seven"),
                   "67")
})

test_that("words to numbers works", {
  expect_identical(words_to_numbers("one hundred and sixty nine"),
                   "169")
})

test_that("words to numbers works", {
  expect_identical(words_to_numbers("thousand one hundred and eleven"),
                   "1111")
})


test_that("words to numbers works", {
  expect_identical(words_to_numbers("twenty thousand five hundred and sixty nine"),
                   "20569")
})



test_that("words to numbers works", {
  expect_identical(words_to_numbers("one-hundred and five"),
                   "105")
})



test_that("words to numbers works", {
  expect_identical(words_to_numbers("five quintillion"),
                   "5000000000000000000")
})



test_that("four-thousand and thirty", {
  expect_identical(words_to_numbers("four-thousand and thirty"),
                   "4030")
})

test_that("words to numbers works", {
  expect_identical(words_to_numbers("one-hundred and twenty-Five"),
                   "125")
})

test_that("words to numbers works", {
  expect_identical(words_to_numbers("four-thousand and thirty"),
                   "4030")
})

test_that("words to numbers works", {
  expect_identical(words_to_numbers("six-million five-thousand and two"),
                   "6005002")
})


test_that("words to numbers works", {
  expect_identical(words_to_numbers("thousand, one-hundred and eleven"),
                   "1111")
})

test_that("words to numbers works", {
  expect_identical(words_to_numbers("twenty-thousand, five-hundred and sixty-nine"),
                   "20569")
})


test_that("words to numbers works", {
  expect_identical(words_to_numbers("there were twenty-thousand, five-hundred and sixty-nine X in the five quintillion Y"),
                   "there were 20569 X in the 5000000000000000000 Y")
})

test_that("words to numbers works", {
  expect_identical(words_to_numbers("one two three four sixteen hundred twelve thousand and twelve one thousand"),
                   "1 2 3 4 1600 12012 1000")
})

test_that("words to numbers works", {
  expect_identical(words_to_numbers("one two three four sixteen hundred twelve thousand and twelve one thousand, also we have sixty-four dogs"),
                   "1 2 3 4 1600 12012 1000, also we have 64 dogs")
})

test_that("words to numbers works", {
  expect_identical(words_to_numbers("sixteen hundred, twelve thousand and twelve"),
                   "1600, 12012")
})


test_that("one two three", {
  expect_identical(words_to_numbers('1 2 3'), "1 2 3")
})
# 
test_that("words to numbers works", {
  expect_identical(words_to_numbers('test one two three test'), 'test 1 2 3 test')
})
# 
# 
test_that("forty two point five", {
  expect_identical(words_to_numbers('forty two point five'), "42.5")
})
# 
test_that("ten point five", {
  expect_identical(words_to_numbers('ten point five'), "10.5")
})
# 
test_that("digit one", {
  expect_identical(words_to_numbers('digit one'), 'digit 1')
})
# 
test_that("digit one ", {
  expect_identical(words_to_numbers('digit one '), 'digit 1 ')
})
# 
test_that("one thirty", {
  expect_identical(words_to_numbers('one thirty'), '1 30')
})
# 
test_that("thousand", {
  expect_identical(words_to_numbers('thousand'), "1000")
})
# 
test_that("million", {
  expect_identical(words_to_numbers('million'), "1000000")
})
# 
test_that("billion", {
  expect_identical(words_to_numbers('billion'), "1000000000")
})
# 
test_that("xxxxxxx one hundred", {
  expect_identical(words_to_numbers('xxxxxxx one hundred'), 'xxxxxxx 100')
})
# 
test_that("and", {
  expect_identical(words_to_numbers('and'), 'and')
})
# 
test_that("a", {
  expect_identical(words_to_numbers('a'), 'a')
})
# 
test_that("junkvalue", {
  expect_identical(words_to_numbers('junkvalue'), 'junkvalue')
})

test_that("ten point five, seven point eight", {
  expect_identical(words_to_numbers("ten point five, seven point eight"), "10.5, 7.8")
})
# 
test_that("eleven dot one", {
  expect_identical(words_to_numbers('eleven dot one'), "11.1")
})
# 
test_that("Fifty People, One Question Brooklyn", {
  expect_identical(words_to_numbers('Fifty People, One Question Brooklyn'), ('50 People, 1 Question Brooklyn'))
})
# 
test_that("Model Fifty-One Fifty-Six", {
  expect_identical(words_to_numbers('Model Fifty-One Fifty-Six'), ('Model 51 56'))
})
# 
test_that("Fifty Million Frenchmen", {
  expect_identical(words_to_numbers('Fifty Million Frenchmen'), ('50000000 Frenchmen'))
})
# 
test_that("A Thousand and One Wives", {
  expect_identical(words_to_numbers('Thousand and One Wives'), ('1001 Wives'))
})
# 
test_that("Ten Thousand Pictures of You", {
  expect_identical(words_to_numbers('Ten Thousand Pictures of You'), ('10000 Pictures of You'))
})
# 
test_that("one thirty", {
  expect_identical(words_to_numbers('one thirty'), ("1 30"))
})
# 
test_that("six sixty two", {
  expect_identical(words_to_numbers('six sixty two'), ("6 62"))
})
# 
test_that("one hundred thousand", {
  expect_identical(as.numeric(words_to_numbers('one hundred thousand')), (100000))
})
# 
test_that("I have zero apples and four oranges", {
  expect_identical(words_to_numbers('I have zero apples and four oranges'), ('I have 0 apples and 4 oranges'))
})
# 
test_that("Dot two Dot", {
  expect_identical(words_to_numbers('Dot two Dot'), ('.2 Dot'))
})
# 
test_that("seventeen dot two four dot twelve dot five", {
  expect_identical(words_to_numbers('seventeen dot two four dot twelve'), ('17.2 4.12'))
})

test_that("16 million", {
  expect_identical(words_to_numbers("16 million"), ("16000000"))
})

test_that("10 thousand", {
  expect_identical(words_to_numbers('10 thousand'), ("10000"))
})


test_that("10 thousand and 12", {
  expect_identical(words_to_numbers('10 thousand and 12'), ("10012"))
})

# 
# // these dont work below fml
# 
# // it('one thirty thousand", {
#   //   expect(wtn('one thirty thousand'), { impliedHundreds true }), (130000))
#   // }))
# 
# // it('nineteen eighty thousand", {
#   //   expect(wtn('nineteen eighty thousand'), { impliedHundreds true }), ('19 80000'))
#   // }))
# 
# // it('one hundred two thousand", {
#   //   expect(wtn('one hundred two thousand'), (102000))
#   // }))
# 
# // it('one hundred and two thousand", {
#   //   expect(wtn('one hundred and two thousand'), (102000))
#   // }))

# 
# test_that("words to numbers works", {
#   expect_identical(as.numeric(words_to_numbers("sixty hundred thousand")),
#                    6e+06)
# })

# # 
# test_that("three point one four one five nine two six", {
#   expect_identical(words_to_numbers('three point one four one five nine two six'), 
#                    3.1415926)
# })
# 
