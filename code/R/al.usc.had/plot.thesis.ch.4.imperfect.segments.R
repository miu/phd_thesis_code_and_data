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

setwd('~/R/sf_ALTLAR/R/usc.had.seg.al')

source('../ml.utils.R')

## USC-HAD
#load('~/R/sf_ALTLAR/output/imperfect.seg/usc.had/varying.f.mode/threshold_0.2/f_1/sample.Rdata')
#l.rs = obj
#load('~/R/sf_ALTLAR/output/imperfect.seg/usc.had/varying.gamma.min/threshold_0.2/gamma_6/sample.Rdata')
#l.al = obj
#fm.ideal = 0.8862342
#fm.pop = 0.299211

## PAMAP
load('~/R/sf_ALTLAR/output/imperfect.seg/pamap.2.protocol/varying.f.mode/threshold_0.2/f_1/sample.Rdata') ## very few reps
load('~/R/sf_ALTLAR/output/imperfect.seg/pamap.2.protocol.variation/pamap.2.protocol/varying.f.mode/f_1/sample.Rdata')
l.rs = obj
load('~/R/sf_ALTLAR/output/imperfect.seg/pamap.2.protocol/varying.gamma.min/threshold_0.2/gamma_6/sample.Rdata')
l.al = obj
fm.ideal = 0.8146419
fm.pop = 0.369723

GetPerformance.rep = function(l.rep) {
  sapply(l.rep$cms.data$cms, WeightedFMeasure)
}

GetPerformance.all = function(l) {
  
  max.len = -1
  for (l.rep in l) {
    this.len = length(l.rep$cms.data$cms)
    if (this.len > max.len) {
      max.len = this.len
    }
  }
  
  l.perf = lapply(l, GetPerformance.rep)
  for (i in seq_along(l.perf)) {
    this.len = length(l.perf[[i]])
    num.times = max.len - this.len
    
    last.value = l.perf[[i]][this.len]
    l.perf[[i]] = c(l.perf[[i]], rep(x=last.value, times=num.times))
  }
  
  
  m.perf = do.call(rbind, l.perf)
  avg.perf = apply(X=m.perf, MARGIN=2, FUN=mean, na.rm=T)
  
  return( avg.perf )
}

al.perf = GetPerformance.all(l.al)[1:40]
rs.perf = GetPerformance.all(l.rs)[1:40]

min.len = min(length(al.perf), length(rs.perf))
al.perf = al.perf[seq_len(min.len)]
rs.perf = rs.perf[seq_len(min.len)]

plot(al.perf, type='l', ylim=c(0, 1), lwd=2, col='red',
     xlab='Num. Annotations', ylab='F-Score')
lines(rs.perf, lwd=2, col='blue')
lines(x=c(1, min.len), y=c(fm.ideal, fm.ideal), lty='dashed')
lines(x=c(1, min.len), y=c(fm.pop, fm.pop), lty='dotted')
legend(x='bottomright', legend=c('Online Active Learning            ',
                                 'Random Selection                  ',
                                 'All Annotations                   ',
                                 'Population Model                  '), 
       lwd=c(2, 2, 1, 1), 
       lty=c('solid', 'solid', 'dashed', 'dotted'), 
       col=c('red', 'blue', 'black', 'black'))

my.ecdf = function(y) {
  y.ecdf = ecdf(y)
  x = seq(from=min(y), to=max(y), length.out=length(y))
  z = y.ecdf(x)
  names(z) = x
  return( list(x=x, y=z) )
}

al.improvement = al.perf - rs.perf
al.ecdf = my.ecdf(al.improvement)

cat('Percentage improvement =', ecdf(al.improvement)(0) * 100, '%\n')
cat('Max improvement =', max(al.improvement), '\n')
