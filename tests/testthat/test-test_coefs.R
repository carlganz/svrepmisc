context("test_coefs")

library(survey)
library(svrepmisc)
library(nnet)
library(quantreg)
library(MASS)
library(crch)
library(intReg)

# use survey package example
# replicate weights - jackknife (this is slower)
data(api)
dstrat<-svydesign(id=~1,strata=~stype, weights=~pw,
                  data=apistrat, fpc=~fpc)
jkstrat<-as.svrepdesign(dstrat)

test_that("coefs work for multinomial", {
  svy <- as.vector(svymultinom(factor(stype)~factor(sch.wide)+api00+api99+meals,jkstrat))
  reg <- coef(nnet::multinom(factor(stype)~factor(sch.wide)+api00+api99+meals,apistrat,weights=apistrat$pw))
  reg <- as.vector(svrepmisc:::mat2vec(reg))
  expect_equal(svy,reg)
})

test_that("coefs work for quantile", {
  svy <- as.vector(svyrq(api00~api99+factor(sch.wide)+meals,jkstrat))
  reg <- coef(quantreg::rq(api00~api99+factor(sch.wide)+meals,data=apistrat,weights=apistrat$pw))
  reg <- as.vector(reg)
  expect_equal(svy,reg)
})

test_that("coefs work for quantile smoother", {
  svy <- as.vector(svyrqss(api00~api99+factor(sch.wide)+meals,jkstrat))
  reg <- coef(quantreg::rqss(api00~api99+factor(sch.wide)+meals,data=apistrat,weights=apistrat$pw))
  reg <- as.vector(reg)
  expect_equal(svy,reg)
})

test_that("coefs work for negative binomial", {
  svy <- as.vector(svynb(meals~api99+factor(sch.wide)+api00,jkstrat))
  reg <- coef(MASS::glm.nb(meals~api99+factor(sch.wide)+api00,data=apistrat,weights=apistrat$pw))
  reg <- as.vector(reg)
  expect_equal(svy,reg)
})

test_that("coefs work for truncated reg", {
  svy <- as.vector(svytrch(acs.core~api00+api99+factor(sch.wide)+meals,jkstrat,left=15))
  reg <- coef(crch::trch(acs.core~api00+api99+factor(sch.wide)+meals,data=apistrat,weights=apistrat$pw,left=15))
  reg <- as.vector(reg)
  expect_equal(svy,reg)
})

test_that("coefs work for censored reg", {
  svy <- as.vector(svycrch(acs.core~api00+api99+factor(sch.wide)+meals,jkstrat,left=15))
  reg <- coef(crch::crch(acs.core~api00+api99+factor(sch.wide)+meals,data=apistrat,weights=apistrat$pw,left=15))
  reg <- as.vector(reg)
  expect_equal(svy,reg)
})

test_that("coefs work for intreg",{
  newVar <- sample(1:3,200,replace=TRUE)
  apistrat$low <- c(10000,50000,100000)[newVar]
  apistrat$high <- c(49999,99999,199999)[newVar]
  jkstrat <- update(jkstrat,low = c(10000,50000,100000)[newVar],high = c(49999,99999,199999)[newVar])
  svy <- as.vector(svyintReg(cbind(low,high)~api00+api99+factor(sch.wide)+meals,jkstrat))
  reg <- coef(intReg(cbind(low,high)~api00+api99+factor(sch.wide)+meals,data=apistrat,weights=apistrat$pw))
  reg <- as.vector(reg)
  expect_equal(svy,reg)
})
