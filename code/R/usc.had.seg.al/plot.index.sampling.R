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

setwd('~/R/sf_ALTLAR/R/usc.had.seg.al/')

source('../ml.utils.R')

### OBSOLETE
#load('~/R/sf_ALTLAR/output/al.usc.had/usc.had.seg.al/varying.f.mode/f_1/sample.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/generated.seg/seg.size.3-6/varying.f.mode/f_1/sample.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/pamap.2.protocol/varying.f.mode/f_1/sample.Rdata')
### OBSOLETE

#load('~/R/sf_ALTLAR/output/usc.had.seg.al/imperfect.seg.al/output/usc.had.seg.al/varying.f.mode/f_1/sample.Rdata')
#fm.ideal = 0.8828063

load('~/R/sf_ALTLAR/output/usc.had.seg.al/imperfect.seg.al/output/usc.had.seg.al/pamap.2.protocol/varying.f.mode/f_1/sample.Rdata')
fm.ideal = 0.8146419

l.rs = obj

### OBSOLETE
#load('~/R/sf_ALTLAR/output/al.usc.had/usc.had.seg.al/varying.gamma.min/gamma_6/sample.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/generated.seg/seg.size.3-6/varying.gamma.min/gamma_6/sample.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/pamap.2.protocol/varying.gamma.min/gamma_6/sample.Rdata')
### OBSOLETE

#load('~/R/sf_ALTLAR/output/usc.had.seg.al/imperfect.seg.al/output/usc.had.seg.al/varying.gamma.min/gamma_6/sample.Rdata')
load('~/R/sf_ALTLAR/output/usc.had.seg.al/imperfect.seg.al/output/usc.had.seg.al/pamap.2.protocol/varying.gamma.min/gamma_6/sample.Rdata')
l.al = obj

my.ecdf = function(y) {
  y.ecdf = ecdf(y)
  x = seq(from=min(y), to=max(y), length.out=length(y))
  z = y.ecdf(x)
  names(z) = x
  return( list(x=x, y=z) )
}

get.subject.i = function(l.rep) {
  l.rep$subject.i
}

GetPerformanceFromRep = function(l.rep) {
  sapply(l.rep$cms.data$cms, WeightedFMeasure)
}

GetSegLengthsFromRep = function(l.rep) {
  l.rep$cms.data$seg.lengths
}

GetPeakHeightsFromRep = function(l.rep) {
  l.rep$cms.data$peak.heights
}

GetAvgPerformance = function(l, cms.name='cms', which.perf=GetPerformanceFromRep) {
  l.perfs = sapply(l, which.perf)
  
  max.length = 0
  min.length = Inf
  for (perf in l.perfs) {
    if (length(perf) > max.length) {
      max.length = length(perf)
    }
    
    if (length(perf) < min.length) {
      min.length = length(perf)
    }
  }
  
  cat('min.length =', min.length, '\n')
  
  for (i in seq_along(l.perfs)) {
    curr.length = length(l.perfs[[i]])
    
    if (curr.length < max.length) {
      l.perfs[[i]][(curr.length+1):max.length] = NA
    }
  }
  
  m.perfs = do.call(what=rbind, args=l.perfs)
  
  avg.perf = apply(m.perfs, MARGIN=2, FUN=mean, na.rm=T)
  
  return( avg.perf )
}

PlotPerformances = function(al.perf, rs.perf, fm.ideal) {
  
  max.len = max(c(length(al.perf), length(rs.perf)))
  y.max = max(c(al.perf, rs.perf, fm.ideal))
  
  plot(al.perf, col='red', type='l', xlab='Num. of Annotations', ylab='F-Score', lwd=2, ylim=c(0, y.max))
  lines(rs.perf, col='blue', lwd=2)
  lines(x=c(0, max.len), y=c(fm.ideal, fm.ideal), lty='dashed', lwd=2)
  legend(x='bottomright', col=c('red', 'blue', 'black'), lwd=c(2, 2, 2), lty=c('solid', 'solid', 'dashed'),
         legend=c('Active Learning               ',
                  'Random Selection              ',
                  'Ideal'))  
}

i.limit = seq_len(40)
#i.limit = seq_len(50)

al.perf = GetAvgPerformance(l.al)[i.limit]
rs.perf = GetAvgPerformance(l.rs)[i.limit]
PlotPerformances(al.perf, rs.perf, fm.ideal=fm.ideal)

al.seg.lengths = unlist(sapply(l.al, GetSegLengthsFromRep))
rs.seg.lengths = unlist(sapply(l.rs, GetSegLengthsFromRep))

al.peak.heights = unlist(sapply(l.al, GetPeakHeightsFromRep))
rs.peak.heights = unlist(sapply(l.rs, GetPeakHeightsFromRep))

al.improvement = al.perf - rs.perf
e = ecdf(al.improvement)
my.e = my.ecdf(al.improvement)
#plot(e, ylab="ECDF", xlab='F-Score', main='')
plot(y=my.e$y, x=my.e$x, ylim=c(0, 1), xlab='Improvement (F-Score)', ylab='ECDF', type='l', lwd=2)


x = 0
cat('x=', x, '; e(x)=', e(x), '\n', sep='')
cat('max improvement:', max(al.improvement), '\n')
cat('max mean improvement:', max(al.improvement), '\n')
#lines(x=c(x, x), y=c(0, e(x)), col='red', lty='solid', lwd=2)
#lines(x=c(-1, x), y=c(e(x), e(x)), col='red', lty='solid', lwd=2)
lines(x=c(x, x), y=c(0, e(x)), lty='dashed')
lines(x=c(-1, x), y=c(e(x), e(x)), lty='dashed')

x = 0.025
cat('x=', x, '; e(x)=', e(x), '\n', sep='')
cat('max improvement:', max(al.improvement), '\n')
cat('max mean improvement:', max(al.improvement), '\n')
#lines(x=c(x, x), y=c(0, e(x)), col='blue', lty='solid', lwd=2)
#lines(x=c(-1, x), y=c(e(x), e(x)), col='blue', lty='solid', lwd=2)

#legend(x='topleft', legend=c('Positive Gains Threshold                ', 
#                             'Gains over 2.5% Threshold                '),
#       col=c('red', 'blue'), lwd=c(2, 2))

stop('Before contrasting ecdfs')

########################
## Contrasting ECDFs 

load('~/R/sf_ALTLAR/output/al.usc.had/usc.had.seg.al/varying.f.mode/f_1/sample.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/generated.seg/seg.size.3-6/varying.f.mode/f_1/sample.Rdata')
#;oad('~/R/sf_ALTLAR/output/al.usc.had/big.machine/pamap.2.protocol/varying.f.mode/f_1/sample.Rdata')
l.rs.1 = obj

#load('~/R/sf_ALTLAR/output/al.usc.had/usc.had.seg.al/varying.gamma.min/gamma_6/sample.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/generated.seg/seg.size.3-6/varying.gamma.min/gamma_6/sample.Rdata')
load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/pamap.2.protocol/varying.gamma.min/gamma_6/sample.Rdata')
l.al.1 = obj


load('~/R/sf_ALTLAR/output/al.usc.had/usc.had.seg.al/varying.f.mode/f_1/sample.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/generated.seg/seg.size.3-6/varying.f.mode/f_1/sample.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/pamap.2.protocol/varying.f.mode/f_1/sample.Rdata')
l.rs.2 = obj

load('~/R/sf_ALTLAR/output/al.usc.had/usc.had.seg.al/varying.gamma.min/gamma_6/sample.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/generated.seg/seg.size.3-6/varying.gamma.min/gamma_6/sample.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/pamap.2.protocol/varying.gamma.min/gamma_6/sample.Rdata')
l.al.2 = obj


PlotContrast = function(l.al.1, l.rs.1, l.al.2, l.rs.2) {
  i.limit = seq_len(40)
  
  al.1.perf = GetAvgPerformance(l.al.1)[i.limit]
  rs.1.perf = GetAvgPerformance(l.rs.1)[i.limit]
  
  al.2.perf = GetAvgPerformance(l.al.2)[i.limit]
  rs.2.perf = GetAvgPerformance(l.rs.2)[i.limit]
  
  al.1.improvement = al.1.perf - rs.1.perf
  al.2.improvement = al.2.perf - rs.2.perf
  
  my.e.1 = my.ecdf(al.1.improvement)
  my.e.2 = my.ecdf(al.2.improvement)
  
  v.1 = ecdf(al.1.improvement)(0)
  v.2 = ecdf(al.2.improvement)(0)
  
  x.min = min(c(al.1.improvement, al.2.improvement))
  if (x.min < min(al.1.improvement)) {
    my.e.1$x = c(x.min, my.e.1$x)
    my.e.1$y = c(0, my.e.1$y)
  } else {
    my.e.2$x = c(x.min, my.e.2$x)
    my.e.2$y = c(0, my.e.2$y)
  }
  
  x.max = max(c(al.1.improvement, al.2.improvement))
  if (x.max > max(al.1.improvement)) {
    my.e.1$x = c(my.e.1$x, x.max)
    my.e.1$y = c(my.e.1$y, 1)
  } else {
    my.e.2$x = c(my.e.2$x, x.max)
    my.e.2$y = c(my.e.2$y, 1)
  }
  
  plot(y=my.e.1$y, x=my.e.1$x, ylim=c(0, 1), xlab='Improvement (F-Score)', ylab='ECDF', col='red', type='l', lwd=2)
  lines(y=my.e.2$y, x=my.e.2$x, col='blue', lwd=2)
}

PlotContrast(l.al.1, l.rs.1, l.al.2, l.rs.2)
