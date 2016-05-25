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

my.ecdf = function(y) {
  y.ecdf = ecdf(y)
  x = seq(from=min(y), to=max(y), length.out=length(y))
  z = y.ecdf(x)
  names(z) = x
  return( list(x=x, y=z) )
}

#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/b.nb.30-gamma.2/l.al.frame.Rdata')
load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/b.nb.30-gamma.6/l.al.frame.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/generated.seg/seg.size.2/l.al.frame.Rdata')

for (subject.i in seq_along(l.al$data)) {
  for (rep.i in seq_along(l.al$data[[subject.i]])) {
    dup.cms.myopic = list()
    dup.cms.full = list()
    for (i in seq_along(l.al$data[[subject.i]][[rep.i]]$cms.myopic)) {
      dup.cms.myopic[[1 + length(dup.cms.myopic)]] = l.al$data[[subject.i]][[rep.i]]$cms.myopic[[i]]
      dup.cms.myopic[[1 + length(dup.cms.myopic)]] = l.al$data[[subject.i]][[rep.i]]$cms.myopic[[i]]
      dup.cms.myopic[[1 + length(dup.cms.myopic)]] = l.al$data[[subject.i]][[rep.i]]$cms.myopic[[i]]
      
      dup.cms.full[[1 + length(dup.cms.full)]] = l.al$data[[subject.i]][[rep.i]]$cms.full[[i]]
      dup.cms.full[[1 + length(dup.cms.full)]] = l.al$data[[subject.i]][[rep.i]]$cms.full[[i]]
      dup.cms.full[[1 + length(dup.cms.full)]] = l.al$data[[subject.i]][[rep.i]]$cms.full[[i]]
    }
    
    l.al$data[[subject.i]][[rep.i]]$cms.myopic = dup.cms.myopic
    l.al$data[[subject.i]][[rep.i]]$cms.full = dup.cms.full
  }
}

l.al.old = l.al

load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/b.nb.30-gamma.6/l.rs.frame.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/b.nb.30-gamma.2/l.rs.frame.Rdata')

# load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/generated.seg/seg.size.2/l.rs.frame.Rdata')
# l.rs = l.al
# l.al = l.al.old

#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/b.nb.30-gamma.6/l.al.seg.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/b.nb.30-gamma.6/l.rs.seg.Rdata')

GetPerformance.seg = function(l, subject.index) {
  # l$data
  # first index: subject
  # second index: repetition
  
  l.reps = l$data[[subject.index]]
  n = length(l.reps)
  
  perf.myopic = rep(0, times=length(l.reps[[1]]$cms.myopic))
  perf.full = rep(0, times=length(l.reps[[1]]$cms.full))
  perf.seg.myopic = rep(0, times=length(l.reps[[1]]$cms.seg.myopic))
  perf.seg.full = rep(0, times=length(l.reps[[1]]$cms.seg.full))
  
  for (i in seq_len(n)) {
    l.rep = l.reps[[i]]
    perf.myopic = perf.myopic + (sapply(l.rep$cms.myopic, WeightedFMeasure)) / n
    perf.full = perf.full + (sapply(l.rep$cms.full, WeightedFMeasure)) / n
    perf.seg.myopic = perf.seg.myopic + (sapply(l.rep$cms.seg.myopic, WeightedFMeasure)) / n
    perf.seg.full = perf.seg.full + (sapply(l.rep$cms.seg.full, WeightedFMeasure)) / n
    
  }
  
  return( list(perf.myopic=perf.myopic, perf.full=perf.full,
               perf.seg.myopic=perf.seg.myopic, perf.seg.full=perf.seg.full) )
}

GetPerformance = function(l, subject.index) {
  # l$data
  # first index: subject
  # second index: repetition
  
  l.reps = l$data[[subject.index]]
  n = length(l.reps)
  
  perf.myopic = rep(0, times=length(l.reps[[1]]$cms.myopic))
  perf.full = rep(0, times=length(l.reps[[1]]$cms.full))
  
  for (i in seq_len(n)) {
    l.rep = l.reps[[i]]
    perf.myopic = perf.myopic + (sapply(l.rep$cms.myopic, WeightedFMeasure)) / n
    perf.full = perf.full + (sapply(l.rep$cms.full, WeightedFMeasure)) / n
  }
  
  return( list(perf.myopic=perf.myopic, perf.full=perf.full) )
}


num.subjects = length(l.al$data)
m.al.full = NULL
m.rs.full = NULL

m.al.myopic = NULL
m.rs.myopic = NULL
for (subject.index in seq_len(num.subjects)) {
  al.perfs = GetPerformance(l=l.al, subject.index=subject.index)
  
  ## TODO
  ## it seems al.perfs replicates each entry three times
  i.subsample = seq(from=1, by=3, to=length(al.perfs$perf.full))
  #al.perfs = al.perfs[i.subsample]
  
  #y.al.myopic = al.perfs$perf.myopic
  y.al.myopic = al.perfs$perf.myopic[i.subsample]
  
  m.al.myopic = rbind(m.al.myopic, y.al.myopic)
  
  
  #y.al.full = al.perfs$perf.full
  y.al.full = al.perfs$perf.full[i.subsample]
  
  m.al.full = rbind(m.al.full, y.al.full)
  
  rs.perfs = GetPerformance(l=l.rs, subject.index=subject.index)
  y.rs.myopic = rs.perfs$perf.myopic
  m.rs.myopic = rbind(m.rs.myopic, y.rs.myopic)
  y.rs.full = rs.perfs$perf.full
  m.rs.full = rbind(m.rs.full, y.rs.full)
  #y.al.seg.myopic = perfs$perf.seg.myopic
  #y.al.seg.full = perfs$perf.seg.full
  
  #plot(y.al.full, type='l', ylim=c(0, 1), col='red', main=paste('Full; Subject', subject.index))
  #lines(y.rs.full, col='blue')
  
  #plot(y.al.myopic, type='l', lty='dashed', ylim=c(0, 1), col='red', main=paste('Myopic; Subject', subject.index))
  #lines(y.rs.myopic, lty='dashed', col='blue')
}

y.al.full.mean = apply(X=m.al.full, MARGIN=2, FUN=mean)
y.rs.full.mean = apply(X=m.rs.full, MARGIN=2, FUN=mean)
plot(y.al.full.mean, type='l', lwd=2, ylim=c(0, 1), col='red', 
     xlab='Num. Annotations', ylab='F-Score')
lines(y.rs.full.mean, col='blue', lwd=2)
legend(x='bottomright', legend=c('Active Learning                ', 
                                 'Random Selection               '), col=c('red', 'blue'), lwd=c(2, 2))

# TODO
# improvement:
#  - ecdf
# For how many subjects is there improvement? <= (m.al.seg.full - m.rs.seg.full)[1:10] (or 11:20, 21:30...)


m.al.full.improvement = m.al.full - m.rs.full
y.al.full.improvement.mean = apply(X=m.al.full.improvement, MARGIN=2, FUN=mean)

cat('max improvement:', max(y.al.full.improvement.mean), '\n')
e = ecdf(y.al.full.improvement.mean)
#plot(e, ylab="ECDF", xlab='F-Score', main='')
my.e = my.ecdf(y.al.full.improvement.mean)
plot(y=my.e$y, x=my.e$x, ylim=c(0, 1), xlab='Improvement (F-Score)', ylab='ECDF', type='l', lwd=2)

x=0
cat('x=', x, '; e(x)=', e(x), '\n', sep='')
#lines(x=c(x, x), y=c(0, e(x)), col='red', lty='solid', lwd=2)
#lines(x=c(-1, x), y=c(e(x), e(x)), col='red', lty='solid', lwd=2)
lines(x=c(x, x), y=c(0, e(x)), lty='dashed')
lines(x=c(-1, x), y=c(e(x), e(x)), lty='dashed')

x=0.025
cat('x=', x, '; e(x)=', e(x), '\n', sep='')
#lines(x=c(x, x), y=c(0, e(x)), col='blue', lty='solid', lwd=2)
#lines(x=c(-1, x), y=c(e(x), e(x)), col='blue', lty='solid', lwd=2)


#legend(x='topleft', legend=c('Positive Gains Threshold                ',
#                             'Gains over 2.5% Threshold               '),
#       col=c('red', 'blue'), lwd=c(2, 2))

y.al.full.improvement.sd = apply(X=m.al.full.improvement, MARGIN=2, FUN=sd)
y.minus.sd = y.al.full.improvement.mean - y.al.full.improvement.sd
y.plus.sd = y.al.full.improvement.mean + y.al.full.improvement.sd
ylim = c(min(y.minus.sd), max(y.plus.sd))
#plot(y.al.full.improvement.mean, ylim=ylim, type='l', lwd=2, col='red', main='Improvement; Full; Frame-based')
#lines(y.minus.sd, col='red', lty='dotted')
#lines(y.plus.sd, col='red', lty='dotted')
#lines(x=1:length(y.minus.sd), y=rep(0, length(y.minus.sd)), col='black')



y.al.myopic.mean = apply(X=m.al.myopic, MARGIN=2, FUN=mean)
y.rs.myopic.mean = apply(X=m.rs.myopic, MARGIN=2, FUN=mean)
#plot(y.al.myopic.mean, type='l', lwd=2, ylim=c(0, 1), col='red', main="Myopic; Mean")
#lines(y.rs.myopic.mean, col='blue', lwd=2)

m.al.myopic.improvement = m.al.myopic - m.rs.myopic
y.al.myopic.improvement.mean = apply(X=m.al.myopic.improvement, MARGIN=2, FUN=mean)
y.al.myopic.improvement.sd = apply(X=m.al.myopic.improvement, MARGIN=2, FUN=sd)
y.minus.sd = y.al.myopic.improvement.mean - y.al.myopic.improvement.sd
y.plus.sd = y.al.myopic.improvement.mean + y.al.myopic.improvement.sd
ylim = c(min(y.minus.sd), max(y.plus.sd))
#plot(y.al.myopic.improvement.mean, ylim=ylim, type='l', lwd=2, col='red', main='Improvement; myopic')
#lines(y.minus.sd, col='red', lty='dotted')
#lines(y.plus.sd, col='red', lty='dotted')
#lines(x=1:length(y.minus.sd), y=rep(0, length(y.minus.sd)), col='black')

my.e.1 = my.e


########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
#stop('before the other gamma')
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################


load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/b.nb.30-gamma.2/l.al.frame.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/b.nb.30-gamma.6/l.al.frame.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/generated.seg/seg.size.2/l.al.frame.Rdata')

for (subject.i in seq_along(l.al$data)) {
  for (rep.i in seq_along(l.al$data[[subject.i]])) {
    dup.cms.myopic = list()
    dup.cms.full = list()
    for (i in seq_along(l.al$data[[subject.i]][[rep.i]]$cms.myopic)) {
      dup.cms.myopic[[1 + length(dup.cms.myopic)]] = l.al$data[[subject.i]][[rep.i]]$cms.myopic[[i]]
      dup.cms.myopic[[1 + length(dup.cms.myopic)]] = l.al$data[[subject.i]][[rep.i]]$cms.myopic[[i]]
      dup.cms.myopic[[1 + length(dup.cms.myopic)]] = l.al$data[[subject.i]][[rep.i]]$cms.myopic[[i]]
      
      dup.cms.full[[1 + length(dup.cms.full)]] = l.al$data[[subject.i]][[rep.i]]$cms.full[[i]]
      dup.cms.full[[1 + length(dup.cms.full)]] = l.al$data[[subject.i]][[rep.i]]$cms.full[[i]]
      dup.cms.full[[1 + length(dup.cms.full)]] = l.al$data[[subject.i]][[rep.i]]$cms.full[[i]]
    }
    
    l.al$data[[subject.i]][[rep.i]]$cms.myopic = dup.cms.myopic
    l.al$data[[subject.i]][[rep.i]]$cms.full = dup.cms.full
  }
}

l.al.old = l.al

#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/b.nb.30-gamma.6/l.rs.frame.Rdata')
load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/b.nb.30-gamma.2/l.rs.frame.Rdata')

# load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/generated.seg/seg.size.2/l.rs.frame.Rdata')
# l.rs = l.al
# l.al = l.al.old

#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/b.nb.30-gamma.6/l.al.seg.Rdata')
#load('~/R/sf_ALTLAR/output/al.usc.had/big.machine/b.nb.30-gamma.6/l.rs.seg.Rdata')

GetPerformance.seg = function(l, subject.index) {
  # l$data
  # first index: subject
  # second index: repetition
  
  l.reps = l$data[[subject.index]]
  n = length(l.reps)
  
  perf.myopic = rep(0, times=length(l.reps[[1]]$cms.myopic))
  perf.full = rep(0, times=length(l.reps[[1]]$cms.full))
  perf.seg.myopic = rep(0, times=length(l.reps[[1]]$cms.seg.myopic))
  perf.seg.full = rep(0, times=length(l.reps[[1]]$cms.seg.full))
  
  for (i in seq_len(n)) {
    l.rep = l.reps[[i]]
    perf.myopic = perf.myopic + (sapply(l.rep$cms.myopic, WeightedFMeasure)) / n
    perf.full = perf.full + (sapply(l.rep$cms.full, WeightedFMeasure)) / n
    perf.seg.myopic = perf.seg.myopic + (sapply(l.rep$cms.seg.myopic, WeightedFMeasure)) / n
    perf.seg.full = perf.seg.full + (sapply(l.rep$cms.seg.full, WeightedFMeasure)) / n
    
  }
  
  return( list(perf.myopic=perf.myopic, perf.full=perf.full,
               perf.seg.myopic=perf.seg.myopic, perf.seg.full=perf.seg.full) )
}

GetPerformance = function(l, subject.index) {
  # l$data
  # first index: subject
  # second index: repetition
  
  l.reps = l$data[[subject.index]]
  n = length(l.reps)
  
  perf.myopic = rep(0, times=length(l.reps[[1]]$cms.myopic))
  perf.full = rep(0, times=length(l.reps[[1]]$cms.full))
  
  for (i in seq_len(n)) {
    l.rep = l.reps[[i]]
    perf.myopic = perf.myopic + (sapply(l.rep$cms.myopic, WeightedFMeasure)) / n
    perf.full = perf.full + (sapply(l.rep$cms.full, WeightedFMeasure)) / n
  }
  
  return( list(perf.myopic=perf.myopic, perf.full=perf.full) )
}


num.subjects = length(l.al$data)
m.al.full = NULL
m.rs.full = NULL

m.al.myopic = NULL
m.rs.myopic = NULL
for (subject.index in seq_len(num.subjects)) {
  al.perfs = GetPerformance(l=l.al, subject.index=subject.index)
  
  ## TODO
  ## it seems al.perfs replicates each entry three times
  i.subsample = seq(from=1, by=3, to=length(al.perfs$perf.full))
  #al.perfs = al.perfs[i.subsample]
  
  #y.al.myopic = al.perfs$perf.myopic
  y.al.myopic = al.perfs$perf.myopic[i.subsample]
  
  m.al.myopic = rbind(m.al.myopic, y.al.myopic)
  
  
  #y.al.full = al.perfs$perf.full
  y.al.full = al.perfs$perf.full[i.subsample]
  
  m.al.full = rbind(m.al.full, y.al.full)
  
  rs.perfs = GetPerformance(l=l.rs, subject.index=subject.index)
  y.rs.myopic = rs.perfs$perf.myopic
  m.rs.myopic = rbind(m.rs.myopic, y.rs.myopic)
  y.rs.full = rs.perfs$perf.full
  m.rs.full = rbind(m.rs.full, y.rs.full)
  #y.al.seg.myopic = perfs$perf.seg.myopic
  #y.al.seg.full = perfs$perf.seg.full
  
  #plot(y.al.full, type='l', ylim=c(0, 1), col='red', main=paste('Full; Subject', subject.index))
  #lines(y.rs.full, col='blue')
  
  #plot(y.al.myopic, type='l', lty='dashed', ylim=c(0, 1), col='red', main=paste('Myopic; Subject', subject.index))
  #lines(y.rs.myopic, lty='dashed', col='blue')
}

y.al.full.mean = apply(X=m.al.full, MARGIN=2, FUN=mean)
y.rs.full.mean = apply(X=m.rs.full, MARGIN=2, FUN=mean)
plot(y.al.full.mean, type='l', lwd=2, ylim=c(0, 1), col='red', 
     xlab='Num. Annotations', ylab='F-Score')
lines(y.rs.full.mean, col='blue', lwd=2)
legend(x='bottomright', legend=c('Active Learning                ', 
                                 'Random Selection               '), col=c('red', 'blue'), lwd=c(2, 2))

# TODO
# improvement:
#  - ecdf
# For how many subjects is there improvement? <= (m.al.seg.full - m.rs.seg.full)[1:10] (or 11:20, 21:30...)


m.al.full.improvement = m.al.full - m.rs.full
y.al.full.improvement.mean = apply(X=m.al.full.improvement, MARGIN=2, FUN=mean)

cat('max improvement:', max(y.al.full.improvement.mean), '\n')
e = ecdf(y.al.full.improvement.mean)
#plot(e, ylab="ECDF", xlab='F-Score', main='')
my.e = my.ecdf(y.al.full.improvement.mean)
plot(y=my.e$y, x=my.e$x, ylim=c(0, 1), xlab='Improvement (F-Score)', ylab='ECDF', type='l', lwd=2)

x=0
cat('x=', x, '; e(x)=', e(x), '\n', sep='')
#lines(x=c(x, x), y=c(0, e(x)), col='red', lty='solid', lwd=2)
#lines(x=c(-1, x), y=c(e(x), e(x)), col='red', lty='solid', lwd=2)
lines(x=c(x, x), y=c(0, e(x)), lty='dashed')
lines(x=c(-1, x), y=c(e(x), e(x)), lty='dashed')

x=0.025
cat('x=', x, '; e(x)=', e(x), '\n', sep='')
#lines(x=c(x, x), y=c(0, e(x)), col='blue', lty='solid', lwd=2)
#lines(x=c(-1, x), y=c(e(x), e(x)), col='blue', lty='solid', lwd=2)


#legend(x='topleft', legend=c('Positive Gains Threshold                ',
#                             'Gains over 2.5% Threshold               '),
#       col=c('red', 'blue'), lwd=c(2, 2))

y.al.full.improvement.sd = apply(X=m.al.full.improvement, MARGIN=2, FUN=sd)
y.minus.sd = y.al.full.improvement.mean - y.al.full.improvement.sd
y.plus.sd = y.al.full.improvement.mean + y.al.full.improvement.sd
ylim = c(min(y.minus.sd), max(y.plus.sd))
#plot(y.al.full.improvement.mean, ylim=ylim, type='l', lwd=2, col='red', main='Improvement; Full; Frame-based')
#lines(y.minus.sd, col='red', lty='dotted')
#lines(y.plus.sd, col='red', lty='dotted')
#lines(x=1:length(y.minus.sd), y=rep(0, length(y.minus.sd)), col='black')



y.al.myopic.mean = apply(X=m.al.myopic, MARGIN=2, FUN=mean)
y.rs.myopic.mean = apply(X=m.rs.myopic, MARGIN=2, FUN=mean)
#plot(y.al.myopic.mean, type='l', lwd=2, ylim=c(0, 1), col='red', main="Myopic; Mean")
#lines(y.rs.myopic.mean, col='blue', lwd=2)

m.al.myopic.improvement = m.al.myopic - m.rs.myopic
y.al.myopic.improvement.mean = apply(X=m.al.myopic.improvement, MARGIN=2, FUN=mean)
y.al.myopic.improvement.sd = apply(X=m.al.myopic.improvement, MARGIN=2, FUN=sd)
y.minus.sd = y.al.myopic.improvement.mean - y.al.myopic.improvement.sd
y.plus.sd = y.al.myopic.improvement.mean + y.al.myopic.improvement.sd
ylim = c(min(y.minus.sd), max(y.plus.sd))
#plot(y.al.myopic.improvement.mean, ylim=ylim, type='l', lwd=2, col='red', main='Improvement; myopic')
#lines(y.minus.sd, col='red', lty='dotted')
#lines(y.plus.sd, col='red', lty='dotted')
#lines(x=1:length(y.minus.sd), y=rep(0, length(y.minus.sd)), col='black')


########################################################################################
########################################################################################
# versus plot
########################################################################################
########################################################################################

my.e.2 = my.e


x.min = min(c(my.e.2$x, my.e.1$x))
if (x.min < min(my.e.1$x)) {
  my.e.1$x = c(x.min, my.e.1$x)
  my.e.1$y = c(0, my.e.1$y)
} else {
  my.e.2$x = c(x.min, my.e.2$x)
  my.e.2$y = c(0, my.e.2$y)
}

x.max = max(c(my.e.2$x, my.e.1$x))
if (x.max > max(my.e.1$x)) {
  my.e.1$x = c(my.e.1$x, x.max)
  my.e.1$y = c(my.e.1$y, 1)
} else {
  my.e.2$x = c(my.e.2$x, x.max)
  my.e.2$y = c(my.e.2$y, 1)
}

plot(y=my.e.1$y, x=my.e.1$x, ylim=c(0, 1), xlab='Improvement (F-Score)', ylab='ECDF', 
     type='l', lwd=2, col='purple')
lines(y=my.e.2$y, x=my.e.2$x, lwd=2, col='orange')

gamma.6.text = expression(paste(gamma, ' = 6       '))
gamma.2.text = expression(paste(gamma, ' = 2       '))
legend(x='topleft', lwd=c(2, 2), col=c('purple', 'orange'), legend=c(gamma.6.text, gamma.2.text))

