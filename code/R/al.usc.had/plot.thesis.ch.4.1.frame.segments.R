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

setwd('~/R/sf_ALTLAR/R/al.usc.had/')

source('../ml.utils.R')

### PAMAP
load('~/R/sf_ALTLAR/output/al.usc.had/pamap.2/l.al.frame.gamma.2.Rdata') ## => l.al
l.al.inf = l.al 
load('~/R/sf_ALTLAR/output/al.usc.had/pamap.2/l.al.frame.Rdata') ## => l.al
load('~/R/sf_ALTLAR/output/al.usc.had/pamap.2/l.rs.frame.Rdata') ## => l.rs
fm.ideal = 0.8146419
fm.pop = 0.369723


### USC-HAD
#load('~/R/sf_ALTLAR/csvm0074.output/al.usc.had/usc.had/l.al.frame.gamma.2.Rdata') ## => l.al
#l.al.inf = l.al 
#load('~/R/sf_ALTLAR/csvm0074.output/al.usc.had/usc.had/l.al.frame.Rdata') ## => l.al
#load('~/R/sf_ALTLAR/csvm0074.output/al.usc.had/usc.had/l.rs.frame.Rdata') ## => l.rs
#fm.ideal = 0.8862342
#fm.pop = 0.299211


GetPerformance.rep = function(l.rep) {
  sapply(l.rep$cms.full, WeightedFMeasure)
}

GetPerformance.user = function(l.user) {
  l.perf = lapply(X=l.user, FUN=GetPerformance.rep)
  m.perf = do.call(what=rbind, args=l.perf)  
  avg.perf = apply(X=m.perf, MARGIN=2, FUN=mean)
  
  return( avg.perf )
}

GetPerformance.all = function(l) {
  l.perf = lapply(X=l$data, FUN=GetPerformance.user)
  m.perf = do.call(what=rbind, args=l.perf)  
  avg.perf = apply(X=m.perf, MARGIN=2, FUN=mean)
  
  return( avg.perf )
}

al.perf = GetPerformance.all(l=l.al)
rs.perf = GetPerformance.all(l=l.rs)
al.inf.perf = GetPerformance.all(l=l.al.inf)

plot(al.perf, type='l', ylim=c(0, 1), col='red', lwd=2, 
     xlab='Num. Annotations', ylab='F-Score')
lines(rs.perf, col='blue', lwd=2)
lines(x=c(1, length(al.perf)), y=c(fm.ideal, fm.ideal), lty='dashed')
lines(x=c(1, length(al.perf)), y=c(fm.pop, fm.pop), lty='dotted')
legend(x='bottomright', legend=c('Online Active Learning            ',
                                 'Random Selection                  ',
                                 'All Annotations                   ',
                                 'Population Model                  '),
       col=c('red', 'blue', 'black', 'black'), 
       lwd=c(2, 2, 1, 1),
       lty=c('solid', 'solid', 'dashed', 'dotted'))

my.ecdf = function(y) {
  y.ecdf = ecdf(y)
  x = seq(from=min(y), to=max(y), length.out=length(y))
  z = y.ecdf(x)
  names(z) = x
  return( list(x=x, y=z) )
}

al.improvement = al.perf - rs.perf
al.inf.improvement = al.inf.perf - rs.perf

al.ecdf = my.ecdf(al.improvement)
al.inf.ecdf = my.ecdf(al.inf.improvement)

plot(al.ecdf, xlim=c(min(c(al.ecdf$x, al.inf.ecdf$x)), max(c(al.ecdf$x, al.inf.ecdf$x))), 
     type='l', lwd=2, col='brown', xlab='Improvement over RS', ylab='ECDF')
lines(al.inf.ecdf, lwd=2, col='purple')
legend(x='topleft', legend=c(paste(expression(gamma), '= 6           '), 
                             paste(expression(gamma), '= 2           ')), 
       col=c('brown', 'purple'), lwd=c(2, 2))

cat('Percentage improvement =', ecdf(al.improvement)(0) * 100, '%\n')
cat('Max improvement =', max(al.improvement), '\n')

num.improvement = length(which(al.improvement > 0))

cat(num.improvement, '/', length(al.improvement), '=', num.improvement / length(al.improvement), '\n')
