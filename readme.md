# svrepmisc

[![Travis-CI Build Status](https://travis-ci.org/carlganz/svrepmisc.svg?branch=master)](https://travis-ci.org/carlganz/svrepmisc)
[![Coverage Status](https://img.shields.io/codecov/c/github/carlganz/svrepmisc/master.svg)](https://codecov.io/github/carlganz/svrepmisc?branch=master)

This packages uses `survey::withReplicates` to implement several regression methods for replicate weights. 

The initial goal is to have easy-to-implement facsimilies for complex survey regression methods available in other softwares like SAS and Stata.

- SAS's proc surveylogistic offers multinomial logistic regression
- Stata offers truncated, and interval regressions for survey data
- Stata offers negative binomial regression for survey data
- Complex Surveys: A Guide to Analysis Using R Appendix E uses negative binomial as an example for `survey::withReplicates`
- Documentation for `survey::withReplicates` uses `quantreg::qr` as an example
