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

GetSinglePerformance = function(l.single, perf.function) {
  sapply(X=l.single$cms, FUN=perf.function)
}

GetAvgPerformance = function(l, perf.function) {
  l.single.perfs = lapply(X=l, FUN=GetSinglePerformance, perf.function=perf.function)
  l.single.perfs.lengths = sapply(l.single.perfs, length)
  min.len = min(l.single.perfs.lengths)
  
  for (i in seq_along(l.single.perfs)) {
    l.single.perfs[[i]] = l.single.perfs[[i]][seq_len(min.len)]
  }
  
  avg.perf = rep(0, times=length(l.single.perfs[[1]]))
  
  N = length(l.single.perfs)
  for (l.single.perf in l.single.perfs) {
    avg.perf = avg.perf + (l.single.perf / N)
  }
  return( avg.perf )
}

perf.function = WeightedFMeasure

PlotALVersusRS = function(subject.number, al.file.name, rs.file.name) {
  al.file = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw/subject_', subject.number, 
                  '/output/modulated.beta/l.subject.', subject.number, '.', al.file.name, sep='')
  load(file=al.file)
  al.data = l
  al.perf = GetAvgPerformance(l=al.data, perf.function=perf.function)
  
  rs.file = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw/subject_', subject.number, 
                  '/output/modulated.beta/l.subject.', subject.number, '.', rs.file.name, sep='')
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
  min.len = min(length(al.perf), length(rs.perf))
  al.perf = al.perf[seq_len(min.len)]
  rs.perf = rs.perf[seq_len(min.len)]
  
  return( list(al.perf=al.perf, rs.perf=rs.perf) )
}

PlotAllSubjects = function(al.file.name, rs.file.name, i.bubbles) {
  min.len = Inf
  for (subject.number in seq_len(4)) {
    al.file = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw/subject_', subject.number, 
                    '/output/modulated.beta/l.subject.', subject.number, '.', al.file.name, sep='')
    load(file=al.file)
    al.data = l
    
    rs.file = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw/subject_', subject.number, 
                    '/output/modulated.beta/l.subject.', subject.number, '.', rs.file.name, sep='')
    load(file=rs.file)
    rs.data = l
    
    all.data = c(al.data, rs.data)
    
    for (i in seq_along(all.data)) {
      current.len = length(all.data[[i]]$cms)
      if (current.len < min.len) {
        min.len = current.len
      }
    }
  }  
  
  l.perfs = list()
  for (subject.number in seq_len(4)) {
    l.perf = PlotALVersusRS(subject.number=subject.number, al.file.name=al.file.name, rs.file.name=rs.file.name)
    l.perfs[[subject.number]] = l.perf
  }
  
  all.al.perfs = NULL
  all.rs.perfs = NULL
  
  for (subject.number in seq_len(4)) {
    al.perf = l.perfs[[subject.number]]$al.perf[seq_len(min.len)]
    rs.perf = l.perfs[[subject.number]]$rs.perf[seq_len(min.len)]
    
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
      #x = 140
      x = i.bubbles[subject.number]
      points(x=x, y=al.perf[x], pch=0, col='red')
      points(x=x, y=rs.perf[x], pch=0, col='red')
    } else if (subject.number == 2) {
      lines(al.perf, lty='solid', col='blue')
      lines(rs.perf, lty='dotted', col='blue')
      #x = 75
      x = i.bubbles[subject.number]
      points(x=x, y=al.perf[x], pch=1, col='blue')
      points(x=x, y=rs.perf[x], pch=1, col='blue')
    } else if (subject.number == 3) {
      lines(al.perf, lty='solid', col='purple')
      lines(rs.perf, lty='dotted', col='purple')
      #x = 62
      x = i.bubbles[subject.number]
      points(x=x, y=al.perf[x], pch=15, col='purple')
      points(x=x, y=rs.perf[x], pch=15, col='purple')
    } else if (subject.number == 4) {
      lines(al.perf, lty='solid', col='orange')
      lines(rs.perf, lty='dotted', col='orange')
      #x = 110
      x = i.bubbles[subject.number]
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

PlotAvgImprovement = function(al.file.name, rs.file.name) {
  
  get.single.performance = function(l.rep) {
    sapply(l.rep$cms, WeightedFMeasure)
  }
  
  min.len = Inf
  for (subject.number in seq_len(4)) {
    al.file = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw/subject_', subject.number, 
                    '/output/modulated.beta/l.subject.', subject.number, '.', al.file.name, sep='')
    load(file=al.file)
    al.data = l
    
    rs.file = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw/subject_', subject.number, 
                    '/output/modulated.beta/l.subject.', subject.number, '.', rs.file.name, sep='')
    load(file=rs.file)
    rs.data = l
    
    all.data = c(al.data, rs.data)
    
    for (i in seq_along(all.data)) {
      current.len = length(all.data[[i]]$cms)
      if (current.len < min.len) {
        min.len = current.len
      }
    }
  }
  
  all.al.perfs = NULL
  all.rs.perfs = NULL
  for (subject.number in seq_len(4)) {
    al.file = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw/subject_', subject.number, 
                    '/output/modulated.beta/l.subject.', subject.number, '.', al.file.name, sep='')
    load(file=al.file)
    al.data = l
    for (i in seq_along(al.data)) {
      al.data[[i]]$cms = al.data[[i]]$cms[seq_len(min.len)]
    }
    al.perfs = sapply(al.data, get.single.performance)
    if (is.null(all.al.perfs)) {
      all.al.perfs = al.perfs
    } else {
      all.al.perfs = cbind(all.al.perfs, al.perfs)
    }
    
    rs.file = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw/subject_', subject.number, 
                    '/output/modulated.beta/l.subject.', subject.number, '.', rs.file.name, sep='')
    load(file=rs.file)
    rs.data = l
    for (i in seq_along(rs.data)) {
      rs.data[[i]]$cms = rs.data[[i]]$cms[seq_len(min.len)]
    }
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
  plot(e, ylab="ECDF", xlab='F-Score', main='')
  x = 0.025
  cat('x=', x, '; e(x)=', e(x), '\n', sep='')
  cat('max improvement:', max(all.improvement), '\n')
  cat('max mean improvement:', max(all.improvement.mean), '\n')
  lines(x=c(x, x), y=c(0, e(x)), col='blue', lty='solid', lwd=2)
  lines(x=c(-1, x), y=c(e(x), e(x)), col='blue', lty='solid', lwd=2)
  
  legend(x='topleft', legend=c('Positive Gains Threshold                ', 
                               'Gains over 2.5% Threshold                '),
         col=c('red', 'blue'), lwd=c(2, 2))
  
  #for (x in seq(from=0.02, to=0.03, by=0.001)) {
  #  cat('x=', x, '; e(x)=', e(x), '\n', sep='')
  #}
  
  x = 0
  cat('x=', x, '; e(x)=', e(x), '\n', sep='')
  lines(x=c(x, x), y=c(0, e(x)), col='red', lty='solid', lwd=2)
  lines(x=c(-1, x), y=c(e(x), e(x)), col='red', lty='solid', lwd=2)
}

PlotDistributionCurve = function(al.file.name, exp.strategy=F, lambda, max.size=200) {
  all.data = list()
  for (subject.number in seq_len(4)) {
    al.file = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw/subject_', subject.number, 
                    '/output/modulated.beta/l.subject.', subject.number, '.', al.file.name, sep='')
    load(file=al.file)
    al.data = l
    all.data = append(all.data, l)
  }
  
  get.distribution = function(l.item) {
    annotations = rep(0, times=2000)
    ## remove timestamps past the horizon
    timestamps = l.item$timestamps - 9
    
    if (length(timestamps) > 200) {
      timestamps = timestamps[1:200]
    }
    
    annotations[timestamps] = 1
    return( annotations )
  }
  
  m.annotations = sapply(X=all.data, FUN=get.distribution)
  
  avg.annot = apply(X=m.annotations, MARGIN=1, FUN=mean)
  
  #al.ma = arima(x=avg.annot, order=c(4, 0, 5))
  al.ma = arima(x=avg.annot, order=c(5, 0, 5))
    
  if (exp.strategy) {
    x = seq(from=0, to=1, length.out=2000)
    
    corr.factor = (lambda / (1-exp(-lambda))) / 10
    
    y.ideal = exp(-lambda * x) * corr.factor
    x.ideal = x * 2000
  } else {
    lambda = max.size / 2000
    x.ideal = seq(2000)
    y.ideal = rep(lambda, times=2000)
  }
  al.ma.y = avg.annot - al.ma$residuals
  
  y.min = min(c(avg.annot, al.ma.y, y.ideal))
  y.max = max(c(avg.annot, al.ma.y, y.ideal))
  ylim = c(y.min, y.max)
  
  plot(avg.annot, type='l', col=rgb(red=0.7, green=0.7, blue=0.7, alpha=1),
       xlab='Timestamp', ylab='Num. Annotations', ylim=ylim)  
  lines(al.ma.y, col='red')  
  lines(x=x.ideal, y=y.ideal, col='blue')
  
  print(ks.test(x=al.ma.y, y=y.ideal, alternative='two.sided'))
  
  cat('Variance of spending deviations:', var(y.ideal - avg.annot), '\n')
  
  #### zoom-in:
  
  i = seq_len(50)
  
  plot(avg.annot[i], type='l', col=rgb(red=0.7, green=0.7, blue=0.7, alpha=1),
       xlab='Timestamp', ylab='Num. Annotations', ylim=ylim)
  lines(al.ma.y[i], col='red')
  lines(x=x.ideal[i], y=y.ideal[i], col='blue')
}

PlotDistributionCurve.F = function(al.file.name, exp.strategy=F, lambda, max.size=200) {
  all.data = list()
  for (subject.number in seq_len(4)) {
    al.file = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw/subject_', subject.number, 
                    '/output/modulated.beta/l.subject.', subject.number, '.', al.file.name, sep='')
    load(file=al.file)
    al.data = l
    all.data = append(all.data, l)
  }
  
  get.distribution = function(l.item) {
    annotations = rep(0, times=2000)
    ## remove timestamps past the horizon
    timestamps = l.item$timestamps - 9
    
    if (length(timestamps) > 200) {
      timestamps = timestamps[1:200]
    }
    
    annotations[timestamps] = 1
    return( annotations )
  }
  
  get.distribution.F = function(l.item) {
    annotations = rep(0, times=2000)
    timestamps = l.item$timestamps - 9
    
    if (length(timestamps) > 200) {
      timestamps = timestamps[1:200]
    }
    
    for (timestamp in timestamps) {
      annotations[timestamp:(length(annotations))] = 1 + annotations[timestamp:(length(annotations))]
    }
    
    return( annotations )
  }
  
  m.annotations = sapply(X=all.data, FUN=get.distribution.F)
  
  avg.annot = apply(X=m.annotations, MARGIN=1, FUN=mean)
  
  #al.ma = arima(x=avg.annot, order=c(4, 0, 5))
  #al.ma = arima(x=avg.annot, order=c(5, 0, 5))
  
  plot(avg.annot, type='l', col='black',
       xlab='Timestamp', ylab='Num. Annotations')
  #al.ma.y = avg.annot - al.ma$residuals
  #lines(al.ma.y, col='red')
  
  if (exp.strategy) {
    x = seq(from=0, to=1, length.out=2000)
    
    #corr.factor = (lambda / (1-exp(-lambda))) / 10    
    #y.ideal = exp(-lambda * x) * corr.factor
    
    y.ideal = max.size * (1 - exp(-lambda * x))
    y.ideal.max = y.ideal[length(y.ideal)]
    y.ideal = y.ideal / y.ideal.max * max.size    
    
    x.ideal = x * 2000
  } else {
    lambda = max.size / 2000
    x.ideal = seq(2000)
    y.start = 1
    y.end = 2000
    #y.ideal = rep(lambda, times=2000)
    y.ideal = max.size * x.ideal / 2000
  }
  
  lines(x=x.ideal, y=y.ideal, col='blue', lty='dashed', lwd=2)
  
  print(ks.test(x=avg.annot, y=y.ideal, alternative='two.sided'))
  
  cat('Variance of spending deviations:', var(y.ideal - avg.annot), '\n')
  
  #### zoom-in:
  
  i = seq_len(50)
  
  y.min = min(c(avg.annot[i], y.ideal[i]))
  y.max = max(c(avg.annot[i], y.ideal[i]))
  ylim = c(y.min, y.max)
  
  plot(avg.annot[i], type='l', col='black',
       xlab='Timestamp', ylab='Num. Annotations', ylim=ylim)
  lines(x=x.ideal[i], y=y.ideal[i], col='blue', lwd=2, lty='dashed')
}

PrintAvgSpentBudget = function(al.file.name) {
  all.data = list()
  for (subject.number in seq_len(4)) {
    al.file = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw/subject_', subject.number, 
                    '/output/modulated.beta/l.subject.', subject.number, '.', al.file.name, sep='')
    load(file=al.file)
    al.data = l
    all.data = append(all.data, l)
  }
  
  get.spent.budget = function(l.item) {
    timestamps = l.item$timestamps
    
    if (length(timestamps) > 200) {
      timestamps = timestamps[1:200]
    }
    
    length(timestamps)
  }
  
  spent.budgets = sapply(all.data, get.spent.budget)
  
  cat("spent budget =", mean(spent.budgets) )
}

#subject.number = 4
#k = 5
#ExplorationPlots(subject.number=subject.number, k=k)

par(mar=c(5, 5, 0.1, 0.1))

#al.file.name = 'k.3.unif.gamma.Rdata'
#rs.file.name = 'k.3.unif.random.Rdata'
#i.bubbles = c(65, 75, 90, 150)

#al.file.name = 'k.3.exp.gamma.lambda.2.Rdata'
#rs.file.name = 'k.3.exp.random.lambda.2.Rdata'
#i.bubbles = c(100, 130, 75, 145)

#al.file.name = 'k.3.exp.gamma.lambda.1.Rdata'
#rs.file.name = 'k.3.exp.random.lambda.1.Rdata'
#i.bubbles = c(100, 130, 20, 145)

#al.file.name = 'k.3.unif.gamma.max.size.150.Rdata'
#rs.file.name = 'k.3.unif.random.max.size.150.Rdata'
#i.bubbles = c(55, 50, 95, 45)

#al.file.name = 'k.3.unif.gamma.max.size.100.Rdata'
#rs.file.name = 'k.3.unif.random.max.size.100.Rdata'
#i.bubbles = c(90, 50, 95, 45)

#al.file.name = 'k.3.unif.gamma.max.size.50.Rdata'
#rs.file.name = 'k.3.unif.random.max.size.50.Rdata'
#i.bubbles = c(10, 20, 35, 45)

#### FROM HERE :

#al.file.name = 'k.3.unif.gamma.beta.0.1.Rdata'
#rs.file.name = 'k.3.exp.random.lambda.2.Rdata' # random
#i.bubbles = c(100, 120, 60, 140)

#al.file.name = 'k.3.unif.gamma.beta.0.5.Rdata'
#rs.file.name = 'k.3.exp.random.lambda.2.Rdata' # random
#i.bubbles = c(175, 120, 80, 140)

#al.file.name = 'k.3.unif.gamma.beta.1.Rdata'
#rs.file.name = 'k.3.exp.random.lambda.2.Rdata' # random
#i.bubbles = c(175, 120, 150, 140)

#al.file.name = 'k.3.exp.gamma.lambda.3.beta.0.1.Rdata'
#rs.file.name = 'k.3.exp.random.lambda.2.Rdata' # random
#i.bubbles = c(175, 120, 160, 140)

#al.file.name = 'k.3.exp.gamma.lambda.3.beta.0.5.Rdata'
#rs.file.name = 'k.3.exp.random.lambda.2.Rdata' # random
#i.bubbles = c(175, 120, 125, 140)

al.file.name = 'k.3.exp.gamma.lambda.3.beta.1.Rdata'
rs.file.name = 'k.3.exp.random.lambda.2.Rdata' # random
i.bubbles = c(175, 120, 150, 175)


#PlotAllSubjects(al.file.name=al.file.name, rs.file.name=rs.file.name, i.bubbles=i.bubbles)
#PlotAvgImprovement(al.file.name=al.file.name, rs.file.name=rs.file.name)

tokens = strsplit(x=al.file.name, split='\\.')[[1]]
exp.strategy = 'exp' %in% tokens
lambda = NA
max.size = 200
if (exp.strategy) {
  i.lambda = which(tokens == 'lambda')
  lambda = as.integer(tokens[i.lambda + 1])
} else {
  if (('max' %in% tokens) & ('size' %in% tokens)) {
    max.size = as.integer(tokens[7])
  }
}

#PlotDistributionCurve(al.file.name=al.file.name, exp.strategy=exp.strategy, lambda=lambda, max.size=max.size)

PlotDistributionCurve.F(al.file.name=al.file.name, exp.strategy=exp.strategy, lambda=lambda, max.size=max.size)

#PrintAvgSpentBudget(al.file.name=al.file.name)

cat('done\n')
