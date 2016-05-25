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

setwd('~/R/sf_ALTLAR/R/opportunity.butterworth.dtw')

source('../ml.utils.R')


##### old functions #####
my.ecdf = function(y) {
  y.ecdf = ecdf(y)
  x = seq(from=min(y), to=max(y), length.out=length(y))
  z = y.ecdf(x)
  names(z) = x
  return( list(x=x, y=z) )
}

GetSinglePerformance = function(l.single, perf.function=WeightedFMeasure) {
  sapply(X=l.single$cms, FUN=perf.function)
}

GetAvgPerformance = function(l, perf.function=WeightedFMeasure) {
  l.single.perfs = lapply(X=l, FUN=GetSinglePerformance, perf.function=perf.function)
  
  avg.perf = rep(0, times=length(l.single.perfs[[1]]))
  
  N = length(l.single.perfs)
  for (l.single.perf in l.single.perfs) {
    avg.perf = avg.perf + (l.single.perf / N)
  }
  return( avg.perf )
}

perf.function = WeightedFMeasure

PlotALVersusRS = function(subject.number, k, metric, gamma, filtered) {
  al.file = paste('../../output/opportunity.butterworth.dtw/subject_', subject.number, 
                  '/output/l.', filtered,
                  '.subject.', subject.number,
                  '.k.', k, 
                  '.', metric, 
                  '.gamma.', gamma, 
                  '.Rdata', sep='')
  load(file=al.file)
  al.data = l
  al.perf = GetAvgPerformance(l=al.data, perf.function=perf.function)
  
  rs.file = paste('../../output/opportunity.butterworth.dtw/subject_', subject.number, 
                  '/output/l.', filtered,
                  '.subject.', subject.number,
                  '.k.', k, 
                  '.random.Rdata', sep='')
  load(file=rs.file)
  rs.data = l
  rs.perf = GetAvgPerformance(l=rs.data, perf.function=perf.function)
  
  #   main.text = paste('Metric=', metric, '; gamma=', gamma, '; ', filtered,
  #                     '\nsubject=', subject.number, 'k=', k, sep='')
  #   plot(al.perf, type='l', ylim=c(0, 1), main=main.text, col='red', 
  #        ylab='F-Score', xlab='Number of annotated segments')
  #   lines(rs.perf, col='blue')
  #   legend(x='bottomright', legend=c('Adaptive Selection', 'Random Selection'), 
  #          col=c('red', 'blue'), lty=c('solid', 'solid'))
  
  return( list(al.perf=al.perf, rs.perf=rs.perf) )
}

ExplorationPlots = function(subject.number, k) {
  filtered = 'filtered'  
  metric = 'confidence'
  
  #for (filtered in c('unfiltered', 'filtered')) {
  for (metric in c('confidence', 'margin')) {
    for (gamma in c(1, 2, 3, 5, 6)) {
      PlotALVersusRS(subject.number=subject.number, k=k, metric=metric, gamma=gamma, filtered=filtered)
    }  
  }
  #}
}

PlotAllSubjects = function(k, filtered, metric, gamma) {
  l.perfs = list()
  for (subject.number in seq_len(4)) {
    l.perf = PlotALVersusRS(subject.number=subject.number, k=k, metric=metric, gamma=gamma, filtered=filtered)
    l.perfs[[subject.number]] = l.perf
  }
  
  all.al.perfs = NULL
  all.rs.perfs = NULL
  
  for (subject.number in seq_len(4)) {
    al.perf = l.perfs[[subject.number]]$al.perf
    rs.perf = l.perfs[[subject.number]]$rs.perf
    
    if (is.null(all.al.perfs)) {
      all.al.perfs = al.perf
    } else {
      all.al.perfs = rbind(all.al.perfs, al.perf)
    }
    
    if (is.null(all.rs.perfs)) {
      all.rs.perfs = rs.perf
    } else {
      all.rs.perfs = rbind(all.rs.perfs, rs.perf)
    }
    
    if (subject.number == 1) {
      plot(al.perf, type='l', lty='solid', col='red', ylim=c(0, 0.8), 
           ylab='F-Score', xlab='Num. Annotations Provided')
      lines(rs.perf, lty='dotted', col='red')
      x = 140
      points(x=x, y=al.perf[x], pch=0, col='red')
      points(x=x, y=rs.perf[x], pch=0, col='red')
    } else if (subject.number == 2) {
      lines(al.perf, lty='solid', col='blue')
      lines(rs.perf, lty='dotted', col='blue')
      x = 75
      points(x=x, y=al.perf[x], pch=1, col='blue')
      points(x=x, y=rs.perf[x], pch=1, col='blue')
    } else if (subject.number == 3) {
      lines(al.perf, lty='solid', col='purple')
      lines(rs.perf, lty='dotted', col='purple')
      x = 62
      points(x=x, y=al.perf[x], pch=15, col='purple')
      points(x=x, y=rs.perf[x], pch=15, col='purple')
    } else if (subject.number == 4) {
      lines(al.perf, lty='solid', col='orange')
      lines(rs.perf, lty='dotted', col='orange')
      x = 110
      points(x=x, y=al.perf[x], pch=19, col='orange')
      points(x=x, y=rs.perf[x], pch=19, col='orange')
    }
  }
  
  legend(x='bottomright', ncol=2, 
         legend=c('S1 AL            ', 'S2 AL            ', 'S3 AL            ', 'S4 AL            ',
                  'S1 RS            ', 'S2 RS            ', 'S3 RS            ', 'S4 RS            '), 
         col=c('red', 'blue', 'purple', 'orange'), 
         lty=c('solid', 'solid', 'solid', 'solid',
               'dashed', 'dashed', 'dashed', 'dashed'),
         pch=c(0, 1, 15, 19))
  
  al.mean = apply(X=all.al.perfs, MARGIN=2, FUN=mean)
  rs.mean = apply(X=all.rs.perfs, MARGIN=2, FUN=mean)
  plot(al.mean, xlab='Num. Annotations Provided', ylab='F-Score',
       type='l', lwd=2, ylim=c(0, 0.6), col='red')
  lines(rs.mean, lty='solid', col='blue')
  legend(x='bottomright', legend=c('Active Learning               ', 
                                   'Random Selection              '), 
         lwd=c(2, 1),
         col=c('red', 'blue'))
}

PlotAvgImprovement = function(k, filtered, metric, gamma) {
  
  get.single.performance = function(l.rep) {
    sapply(l.rep$cms, WeightedFMeasure)
  }
  
  all.al.perfs = NULL
  all.rs.perfs = NULL
  for (subject.number in seq_len(4)) {
    al.file = paste('../../output/opportunity.butterworth.dtw/subject_', subject.number, 
                    '/output/l.', filtered,
                    '.subject.', subject.number,
                    '.k.', k, 
                    '.', metric, 
                    '.gamma.', gamma, 
                    '.Rdata', sep='')
    load(file=al.file)
    al.data = l
    al.perfs = sapply(al.data, get.single.performance)
    if (is.null(all.al.perfs)) {
      all.al.perfs = al.perfs
    } else {
      all.al.perfs = cbind(all.al.perfs, al.perfs)
    }
    
    rs.file = paste('../../output/opportunity.butterworth.dtw/subject_', subject.number, 
                    '/output/l.', filtered,
                    '.subject.', subject.number,
                    '.k.', k, 
                    '.random.Rdata', sep='')
    load(file=rs.file)
    rs.data = l
    rs.perfs = sapply(rs.data, get.single.performance)
    if (is.null(all.rs.perfs)) {
      all.rs.perfs = rs.perfs 
    } else {
      all.rs.perfs = cbind(all.rs.perfs, rs.perfs)
    }
    
    improvement = al.perfs - rs.perfs
    
    improvement.mean = apply(X=improvement, MARGIN=1, FUN=mean)
    improvement.sd = apply(X=improvement, MARGIN=1, FUN=sd)
    
    mean.minus.sd = improvement.mean - improvement.sd
    mean.plus.sd = improvement.mean + improvement.sd
    
    ylim = c(min(mean.minus.sd), max(mean.plus.sd))
    #plot(improvement.mean, type='l', ylim=ylim, lwd=2, col='red', main=paste('Improvement for Subject', subject.number))
    #lines(mean.minus.sd, lty='dotted', col='red')
    #lines(mean.plus.sd, lty='dotted', col='red')
    #lines(x=c(1, length(improvement.mean)), y=c(0, 0))
  }
  
  all.improvement = all.al.perfs - all.rs.perfs
  all.improvement.mean = apply(X=all.improvement, MARGIN=1, FUN=mean)
  all.improvement.sd = apply(X=all.improvement, MARGIN=1, FUN=sd)
  
  all.mean.minus.sd = all.improvement.mean - all.improvement.sd
  all.mean.plus.sd = all.improvement.mean + all.improvement.sd
  
  ylim = c(min(all.mean.minus.sd), max(all.mean.plus.sd))
  #plot(all.improvement.mean, type='l', ylim=ylim, lwd=2, col='red', main='Improvement for all subjects')
  #lines(all.mean.minus.sd, lty='dotted', col='red')
  #lines(all.mean.plus.sd, lty='dotted', col='red')
  #lines(x=c(1, length(all.improvement.mean)), y=c(0, 0))
  
  #hist(all.improvement.mean)
  e = ecdf(all.improvement.mean)
  my.e = my.ecdf(all.improvement.mean)
  plot(y=my.e$y, x=my.e$x, ylim=c(0, 1), xlab='Improvement (F-Score)', ylab='ECDF', type='l', lwd=2)
  #plot(e, ylab="ECDF", xlab='F-Score', main='')
  
  x = 0.025
  cat('x=', x, '; e(x)=', e(x), '\n', sep='')
  cat('max improvement:', max(all.improvement), '\n')
  cat('max mean improvement:', max(all.improvement.mean), '\n')
  #lines(x=c(x, x), y=c(0, e(x)), col='blue', lty='solid', lwd=2)
  #lines(x=c(-1, x), y=c(e(x), e(x)), col='blue', lty='solid', lwd=2)
  
  #legend(x='topleft', legend=c('Positive Gains Threshold                ', 
  #                             'Gains over 2.5% Threshold                '),
  #       col=c('red', 'blue'), lwd=c(2, 2))
  
  #for (x in seq(from=0.02, to=0.03, by=0.001)) {
  #  cat('x=', x, '; e(x)=', e(x), '\n', sep='')
  #}
  
  x = 0
  cat('x=', x, '; e(x)=', e(x), '\n', sep='')
  lines(x=c(x, x), y=c(0, e(x)), lty='dashed')
  lines(x=c(-1, x), y=c(e(x), e(x)), lty='dashed')
}
##### ##### #####

DiffNumAnnotations = function(baseline.perf, al.perf) {
  x.baseline = baseline.perf
  y.baseline = seq_along(baseline.perf)
  
  x.al = al.perf
  y.al = seq_along(al.perf)
  
  y.approx = approx(x=x.baseline, y=y.baseline, xout=x.al)$y
  diff.num.annotations = y.approx - y.al
  percentage.diff = diff.num.annotations / y.approx
  
  return( list(diff.num.annotations=diff.num.annotations, percentage.diff=percentage.diff) )
}

#subject.number = 4
#k = 5
#ExplorationPlots(subject.number=subject.number, k=k)

par(mar=c(5, 5, 1, 0.1))
#par()
### TODO
# 1) bring xy values closer to axes
# 2) bring xlab and ylab closer to values

output.dir = '~/R/sf_ALTLAR/output/opportunity.butterworth.dtw'
interm.dir = 'output'
interm.redone.dir = 'output.redone'
al.file = 'k.3.confidence.gamma.6.Rdata'
rs.file = 'k.3.random.Rdata'

l.als = list()
l.rss = list()
l.baselines = list()
l.pops = list()
for (i in 1:4) {
  subj.dir = paste('subject', i, sep='_')
  
  al.file.name = paste('l.unfiltered.subject', i, al.file, sep='.')
  al.path = paste(output.dir, subj.dir, interm.dir, al.file.name, sep='/')
  load(al.path)
  l.als[[1 + length(l.als)]] = l
  
  rs.file.name = paste('l.unfiltered.subject', i, rs.file, sep='.')
  rs.path = paste(output.dir, subj.dir, interm.dir, rs.file.name, sep='/')
  load(rs.path)
  l.rss[[1 + length(l.rss)]] = l
  
  baseline.file.name = paste('l.unfiltered.subject', i, 'k.3.random.all.Rdata', sep='.')
  baseline.path = paste(output.dir, subj.dir, interm.redone.dir, baseline.file.name, sep='/')
  load(baseline.path)
  l.baselines[[1 + length(l.baselines)]] = l
  
  pop.file.name = paste('l.unfiltered.subject', i, 'k.3.random.pop.Rdata', sep='.')
  pop.path = paste(output.dir, subj.dir, interm.redone.dir, pop.file.name, sep='/')
  load(pop.path)
  l.pops[[1 + length(l.pops)]] = l
}

l.al.perfs = lapply(X=l.als, FUN=GetAvgPerformance)
l.rs.perfs = lapply(X=l.rss, FUN=GetAvgPerformance)
l.baseline.perfs = lapply(X=l.baselines, FUN=GetAvgPerformance)
l.pop.perfs = lapply(X=l.pops, FUN=GetAvgPerformance)

## individual plots
for (i in seq_along(l.al.perfs)) {
  al.perf = l.al.perfs[[i]]
  rs.perf = l.rs.perfs[[i]]
  pop.perf.end = tail(l.pop.perfs[[i]], n=1)
  baseline.perf.end = tail(l.baseline.perfs[[i]], n=1)
  
  ## NOTE TO FUTURE SELF:
  # the following overrides the RS learning curve with the the data published in the IUCC paper.
  # this is done in order to be able to exactly match that, besides the equi-perf stuff which was not done at that time and is now included there
  baseline.perf = l.baseline.perfs[[i]] 
  baseline.perf[seq_along(rs.perf)] = rs.perf
  
  ylim.max = max(c(al.perf, rs.perf, baseline.perf, pop.perf.end))
  ylim.min = min(c(al.perf, rs.perf, baseline.perf, pop.perf.end))
  
  plot(rs.perf, ylab='F-Score', xlab='Num. annotations', ylim=c(0, 1), 
       type='l', col='blue')
  lines(al.perf, col='red')
  lines(x=c(0, length(al.perf)), y=c(pop.perf.end, pop.perf.end), col='black', lty='dotted')
  lines(x=c(0, length(al.perf)), y=c(baseline.perf.end, baseline.perf.end), col='black', lty='dashed')
  
  legend(x='bottomright', legend=c('Random Selection', 'Online Active Learning', 'Population Model', 'All annotations'), 
         lty=c('solid', 'solid', 'dotted', 'dashed'), col=c('blue', 'red', 'black', 'black'))
  
  l.diff = DiffNumAnnotations(baseline.perf=baseline.perf, al.perf=al.perf)
  
  positive.diffs = length(which(l.diff$percentage.diff > 0))
  
  how.many.perc = positive.diffs / length(al.perf) # how many positive
  mean.perc = mean(l.diff$percentage.diff, na.rm=T)
  max.perc = max(l.diff$percentage.diff, na.rm=T)
  
  how.many.num = positive.diffs
  mean.num = mean(l.diff$diff.num.annotations, na.rm=T)
  max.num = max(l.diff$diff.num.annotations, na.rm=T)
  
  cat('subject', i, 'annotations:\n')
  cat(' - perc.difference: max=', max.perc, '; mean=', mean.perc, '; how.many=', how.many.perc, '\n\n', sep='')
  cat(' - num.difference: max=', max.num, '; mean=', mean.num, '; how.many=', how.many.num, '\n\n', sep='')
  
  x = seq_along(l.diff$diff.num.annotations)
  y = x + l.diff$diff.num.annotations
  y.al = x
  
  plot(x=x, y=y, type='l', lwd=2, xlab='Num. Annotations by OAL', ylab='Num. Annotations by RS')
  lines(x=x, y=y.al, lty='dotted')
  legend(x='bottomright', legend=c(
    'Equi-performance line                  ', 
    'Improvement boundary                   '), 
    lwd=c(2, 1), lty=c('solid', 'dotted'))
}
