#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#

#
#    Copyright (C) 2015 Tudor-Alin Miu
#
#

rm(list=ls())

setwd('~/R/sf_ALTLAR/R/al.usc.had')
source('../ml.utils.R')

#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/b.nb.30-gamma.2/l.al.frame.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/generated.seg/seg.size.2/l.al.frame.Rdata')

#l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated/pamap/l.al.exp.lambda.3.frame.Rdata'
#l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated/pamap/l.al.exp.lambda.3.frame.Rdata'
#l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/l.al.unif.size.200.frame.Rdata'
#l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/retry/l.al.exp.lambda.1.frame.Rdata'

## PAMAP :

l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/pamap/l.al.unif.size.200.beta.0.1.frame.Rdata'
#l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/pamap/l.al.unif.size.200.beta.0.5.frame.Rdata'
#l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/pamap/l.al.unif.size.200.beta.1.frame.Rdata'
###l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/pamap/l.al.unif.size.200.beta.2.5.frame.Rdata'

#l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/pamap/l.al.exp.lambda.3.beta.0.1.frame.Rdata'
#l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/pamap/l.al.exp.lambda.3.beta.0.5.frame.Rdata'
#l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/pamap/l.al.exp.lambda.3.beta.1.frame.Rdata'
###l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/pamap/l.al.exp.lambda.3.beta.2.5.frame.Rdata'


## USC-HAD :

#l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/usc-had/l.al.unif.size.200.beta.0.1.frame.Rdata'
#l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/usc-had/l.al.unif.size.200.beta.0.5.frame.Rdata'
#l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/usc-had/l.al.unif.size.200.beta.1.frame.Rdata'
###l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/usc-had/l.al.unif.size.200.beta.2.5.frame.Rdata'

#l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/usc-had/l.al.exp.lambda.3.beta.0.1.frame.Rdata'
#l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/usc-had/l.al.exp.lambda.3.beta.0.5.frame.Rdata'
#l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/usc-had/l.al.exp.lambda.3.beta.1.frame.Rdata'
###l.al.file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/usc-had/l.al.exp.lambda.3.beta.2.5.frame.Rdata'


load(l.al.file.name)

tokens = strsplit(x=l.al.file.name, split='\\.')[[1]]

size = NA
if ('unif' %in% tokens) {
  i.size = which(regexpr(pattern='[0-9]+', text=tokens) >= 0)[1]
  size = as.integer(tokens[i.size])
  if (is.na(size)) {
    stop('invalid size')
  }
} else {
  size = 200
}


#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/b.nb.30-gamma.6/l.rs.frame.Rdata')

load('~/R/sf_ALTLAR/output/al.usc.had.modulated/pamap/l.rs.exp.lambda.3.frame.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/retry/l.rs.exp.lambda.1.frame.Rdata')

# load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/generated.seg/seg.size.2/l.rs.frame.Rdata')
# l.rs = l.al
# l.al = l.al.old

#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/b.nb.30-gamma.6/l.al.seg.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/b.nb.30-gamma.6/l.rs.seg.Rdata')

GetPerformance = function(l) {
  
  get.performance.item = function(l.item) {
    sapply(l.item$cms.full, WeightedFMeasure)
  }
  
  l.items = unlist(l$data, recursive=F)
  perfs = lapply(l.items, get.performance.item)
  
  len.perfs = rep(NA, times=length(perfs))
  for (i.perf in seq_along(perfs)) {
    perf = perfs[[i.perf]]
    len.perfs[i.perf] = length(perf)
  }
  
  sum.perfs = rep(0, times=max(len.perfs))
  for (i.perf in seq_along(perfs)) {
    perf = perfs[[i.perf]]
    num.zeros = max(len.perfs) - length(perf)
    perf = c(perf, rep(0, times=num.zeros))
    sum.perfs = sum.perfs + perf
  }
  
  for (i in seq_along(sum.perfs)) {
    n = length(which(len.perfs >= i))
    #cat('i =', i, '__')
    #cat('n =', n, '\n')
    sum.perfs[i] = sum.perfs[i] / n
  }
  
  return( sum.perfs )
}

i = seq_len(size)
al.perf = GetPerformance(l.al)[i]
rs.perf = GetPerformance(l.rs)[i]


plot(al.perf, type='l', lwd=2, ylim=c(0, 1), col='red', 
     xlab='Num. Annotations', ylab='F-Score')
lines(rs.perf, col='blue', lwd=2)
legend(x='bottomright', legend=c('Active Learning                ', 
                                 'Random Selection               '), col=c('red', 'blue'), lwd=c(2, 2))

# TODO
# improvement:
#  - ecdf
# For how many subjects is there improvement? <= (m.al.seg.full - m.rs.seg.full)[1:10] (or 11:20, 21:30...)


# al.improvement = al.perf - rs.perf
# 
# cat('max improvement:', max(al.improvement), '\n')
# e = ecdf(al.improvement)
# plot(e, ylab="ECDF", xlab='F-Score', main='')
# 
# x=0
# cat('x=', x, '; e(x)=', e(x), '\n', sep='')
# lines(x=c(x, x), y=c(0, e(x)), col='red', lty='solid', lwd=2)
# lines(x=c(-1, x), y=c(e(x), e(x)), col='red', lty='solid', lwd=2)
# 
# x=0.025
# cat('x=', x, '; e(x)=', e(x), '\n', sep='')
# lines(x=c(x, x), y=c(0, e(x)), col='blue', lty='solid', lwd=2)
# lines(x=c(-1, x), y=c(e(x), e(x)), col='blue', lty='solid', lwd=2)
# 
# legend(x='bottomright', legend=c('Positive Gains Threshold                ',
#                              'Gains over 2.5% Threshold               '),
#        col=c('red', 'blue'), lwd=c(2, 2))



#########
## plot distribution of annotations in time
GetAnnotationCurve = function(l) {
  get.curve.item = function(l.item) {
    y = rep(0, times=2000)
    ts = l.item$timestamps - 9
    y[ts] = 1
    return( y )
  }
  
  l.items = unlist(l$data, recursive=F)
  ys = sapply(l.items, get.curve.item)
  return( apply(X=ys, MARGIN=1, FUN=mean) )
}

al.distr = GetAnnotationCurve(l.al)
al.ma = arima(x=al.distr, order=c(5, 0, 5))
#al.ma = arima(x=al.distr, order=c(6, 0, 5))

al.ma.y = al.distr - al.ma$residuals

y.min = min(c(al.distr, al.ma.y))
y.max = max(c(al.distr, al.ma.y))
ylim = c(y.min, y.max)

plot(al.distr, type='l', col=rgb(red=0.7, green=0.7, blue=0.7, alpha=1),
     xlab='Timestamp', ylab='Num. Annotations', ylim=ylim)
lines(al.ma.y, col='red')

if ('exp' %in% tokens) {
  i.lambda = which(regexpr(pattern='[0-9]+', text=tokens) >= 0)
  lambda = as.integer(tokens[i.lambda])[1]
  
  x = seq(from=0, to=1, length.out=2000)
  
  corr.factor = (lambda / (1-exp(-lambda))) / 10
  
  y.ideal = exp(-lambda * x) * corr.factor
  x.ideal = x * 2000
} else {
  lambda = size / 2000
  x.ideal = seq(2000)
  y.ideal = rep(lambda, times=2000)
}

lines(x=x.ideal, y=y.ideal, col='blue')

print(ks.test(x=al.ma.y, y=y.ideal, alternative='two.sided'))

i = seq_len(50)

plot(al.distr[i], type='l', col=rgb(red=0.7, green=0.7, blue=0.7, alpha=1),
     xlab='Timestamp', ylab='Num. Annotations', ylim=ylim)
lines(al.ma.y[i], col='red')
lines(x=x.ideal[i], y=y.ideal[i], col='blue')
print(ks.test(x=al.ma.y[i], y=y.ideal[i], alternative='two.sided'))

GetAnnotationTimestamps = function(l) {
  get.timestamps = function(l.item) {
    l.item$timestamps
  }
  
  l.items = unlist(l$data, recursive=F)
  ys = sapply(l.items, get.timestamps)
  
  return( unlist(ys) )
}


#########
## plot distribution of annotations in time - CDF
GetAnnotationCurve.F = function(l) {
  get.curve.item = function(l.item) {
    y = rep(0, times=2000)
    ts = l.item$timestamps - 9
    for (timestamp in ts) {
      y[timestamp:length(y)] = 1 + y[timestamp:length(y)]
    }
    
    return( y )
  }
  
  l.items = unlist(l$data, recursive=F)
  ys = sapply(l.items, get.curve.item)
  return( apply(X=ys, MARGIN=1, FUN=mean) )
}

al.distr = GetAnnotationCurve.F(l.al)
#al.ma = arima(x=al.distr, order=c(5, 0, 5))
#al.ma = arima(x=al.distr, order=c(6, 0, 5))

#al.ma.y = al.distr - al.ma$residuals

#y.min = min(c(al.distr, al.ma.y))
#y.max = max(c(al.distr, al.ma.y))
#ylim = c(y.min, y.max)

plot(al.distr, type='l', col='black',
     xlab='Timestamp', ylab='Num. Annotations')
#lines(al.ma.y, col='red')

if ('exp' %in% tokens) {
  i.lambda = which(regexpr(pattern='[0-9]+', text=tokens) >= 0)
  lambda = as.integer(tokens[i.lambda])[1]
  
  x = seq(from=0, to=1, length.out=2000)
  
  #corr.factor = (lambda / (1-exp(-lambda))) / 10  
  #y.ideal = exp(-lambda * x) * corr.factor
  
  y.ideal = size * (1 - exp(-lambda * x))
  y.ideal.max = y.ideal[length(y.ideal)]
  y.ideal = y.ideal / y.ideal.max * size  
  
  x.ideal = x * 2000
} else {
  lambda = size / 2000
  x.ideal = seq(2000)
  #y.ideal = rep(lambda, times=2000)
  y.ideal = size * x.ideal / 2000
}

lines(x=x.ideal, y=y.ideal, col='blue', lwd=2, lty='dashed')

print(ks.test(x=al.distr, y=y.ideal, alternative='two.sided'))

i = seq_len(50)

plot(al.distr[i], type='l', col='black',
     xlab='Timestamp', ylab='Num. Annotations')
#lines(al.ma.y[i], col='red')
lines(x=x.ideal[i], y=y.ideal[i], col='blue', lwd=2, lty='dashed')
#print(ks.test(x=al.ma.y[i], y=y.ideal[i], alternative='two.sided'))
