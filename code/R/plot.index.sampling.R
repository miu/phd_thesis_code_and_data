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

setwd('~/R/sf_ALTLAR')

source('R/file.utils.R')
source('R/ml.utils.R')
source('R/index.sampling.R')

GetMeanSegLength = function(l) {
  sum.len = 0
  num.segs = 0
  for (i in seq_along(l)) {
    num.segs = num.segs + length(l[[i]]$cms.data$seg.lengths)
    sum.len = sum.len + sum(l[[i]]$cms.data$seg.lengths)
  }
  
  return( sum.len / num.segs )
}

GetMeanMinConfCorrect = function(l) {
  correct = 0
  total = 0
  for (i in seq_along(l)) {
    correct = correct + sum(l[[i]]$cms.data$min.conf.correct)
    total = total + length(l[[i]]$cms.data$min.conf.correct)
  }
  return( correct / total )
}

GetSegmentationCM = function(l) {
  cm = 0
  for (i in seq_along(l)) {
    cms.seg = l[[i]]$cms.data$cms.seg
    for (cm.seg in cms.seg) {
      cm = cm + cm.seg
    }
  }
  return (cm)
}

GetSegmentationCM.normalized = function(l) {
  cm = GetSegmentationCM(l=l)
  n = ncol(cm)
  cm.norm = matrix(data=0, nrow=n, ncol=n)
  for (i.actual in seq_len(n)) {
    pred.sum = 0
    for (i.pred in seq_len(n)) {
      pred.sum = pred.sum + cm[i.actual, i.pred]
    }
    for (i.pred in seq_len(n)) {
      cm.norm[i.actual, i.pred] = cm[i.actual, i.pred] / pred.sum
    }
  }
  
  return( cm.norm )
}

GetMeanAUC = function(l) {
  num.reps = length(l)
  auc.mean = 0
  for (rep in l) {
    auc = 0
    ts.size = 0
    for (i in seq_along(rep$cms.data$cms)) {
      cm = rep$cms.data$cms[[i]]
      seg.length = rep$cms.data$seg.lengths[i]
      ts.size = ts.size + seg.length
      mfm = MeanFMeasure(cm=cm)
      for (k in seq_len(seg.length)) {
        auc = auc + mfm
      }
    }
    auc = auc / ts.size
    auc.mean = auc.mean + (auc / num.reps)
  }
  return( auc.mean )
}

GetBudgetSpendingCDF = function(l) {
  num.reps = length(l)
  x.max = -1
  for (rep in l) {
    this.x.max = rep$cms.data$x.max
    if (x.max < this.x.max) {
      x.max = this.x.max
    }
  }
  
  y = rep(0, times=(x.max-1))
  for (rep in l) {
    nqsf = 0 # number of questions so far
    q.i = c(rep$cms.data$question.i, x.max)
    q.i.rep = q.i[2: length(q.i)] - q.i[1: (length(q.i)-1)]
    q.i = rep$cms.data$question.i
    for (i in seq_along(q.i)) {
      nqsf = 1 + nqsf
      for (k in (seq_len(q.i.rep[i])-1) ) {
        y[q.i[i] + k] = y[q.i[i] + k] + (nqsf / num.reps)
      }
    }
  }
  return( y )
}

GetPerformanceCDF = function(l) {
  num.reps = length(l)
  x.max = -1
  for (rep in l) {
    this.x.max = rep$cms.data$x.max
    if (x.max < this.x.max) {
      x.max = this.x.max
    }
  }
  
  y = rep(0, times=(x.max-1))
  for (rep in l) {
    q.i = c(rep$cms.data$question.i, x.max)
    q.i.rep = q.i[2: length(q.i)] - q.i[1: (length(q.i)-1)]
    q.i = rep$cms.data$question.i
    for (i in seq_along(q.i)) {
      cm = rep$cms.data$cms[[i]]
      perf.incr = MeanFMeasure(cm=cm) / num.reps
      for (k in (seq_len(q.i.rep[i])-1) ) {
        y[q.i[i] + k] = y[q.i[i] + k] + perf.incr
      }
    }
  }
  return( y )
}

ElongateCDF = function(y, x.max) {
  times = x.max - length(y)
  y.last = y[length(y)]
  y.padding = rep(x=y.last, times=times)
  return( c(y, y.padding) )
}

output.dir = 'output/segmentation'
varying.f.dir = 'varying.f.mean'
varying.gamma.dir = 'varying.gamma.mean'
#varying.gamma.dir = 'varying.gamma.mean.excluding.min.conf'
#varying.gamma.dir = 'varying.gamma.mode'

graphics.dir = 'graphics/segmentation/averaging.confidence.over.segment'
#graphics.dir = 'graphics/segmentation/mode.conf'
max.ts.size = 200

################################################################################
## Single-Frame Baselines with fixed-t

GetPerformance.no.seg = function(l, PerformanceFunction) {
  #sapply(X=l$cms, FUN=PerformanceFunction)
  num.reps = length(l$all)
  y = rep(x=0, times=length(l$all[[1]]$cms.data$cms))
  for (rep in l$all) {
    this.y = sapply(X=rep$cms.data$cms, FUN=MeanFMeasure)
    y = y + (this.y / num.reps)
  }
  return( y )
}

no.seg.output.dir = '~/R/sf_ALTLAR/output/index.sampling/pamap.indoor.percom/bag.30.naive.bayes'
file.name = 'index.sampling.sample'
# exp(1)
single.base.exp.1.dir = paste(no.seg.output.dir, 'exp.distr.1', sep='/')
single.base.exp.1.l = LoadObject(file.name=file.name, dir=single.base.exp.1.dir)
single.base.exp.1.y = GetPerformance.no.seg(l=single.base.exp.1.l, PerformanceFunction=MeanFMeasure)

# exp(minus 3)
single.base.exp.m3.dir = paste(no.seg.output.dir, 'exp.distr.1e-3', sep='/')
single.base.exp.m3.l = LoadObject(file.name=file.name, dir=single.base.exp.m3.dir)
single.base.exp.m3.y = GetPerformance.no.seg(l=single.base.exp.m3.l, PerformanceFunction=MeanFMeasure)

# exp(3)
single.base.exp.3.dir = paste(no.seg.output.dir, 'exp.distr.1e3', sep='/')
single.base.exp.3.l = LoadObject(file.name=file.name, dir=single.base.exp.3.dir)
single.base.exp.3.y = GetPerformance.no.seg(l=single.base.exp.3.l, PerformanceFunction=MeanFMeasure)

# uniform
single.base.unif.dir = paste(no.seg.output.dir, 'unif.distr', sep='/')
single.base.unif.l = LoadObject(file.name=file.name, dir=single.base.unif.dir)
single.base.unif.y = GetPerformance.no.seg(l=single.base.unif.l, PerformanceFunction=MeanFMeasure)

# upfront
single.base.upfr.dir = paste(no.seg.output.dir, 'upfront.distr', sep='/')
single.base.upfr.l = LoadObject(file.name=file.name, dir=single.base.upfr.dir)
single.base.upfr.y = GetPerformance.no.seg(l=single.base.upfr.l, PerformanceFunction=MeanFMeasure)

SavePlotBegin(dir=graphics.dir, file.name='single.baselines.fixed.t')
line.colors = c(rgb(0, 1, 0), rgb(0.2, 0.6, 0.2), rgb(0, 0, 0),
                rgb(0.5, 0, 0.5), rgb(0, 0.5, 0.5))
plot(single.base.exp.3.y, ylim=c(0, 1), type='l', ylab='Mean F-Measure', 
     xlab='Number of Annotations = Size of training set', col=line.colors[1])
lines(single.base.exp.1.y, col=line.colors[2])
lines(single.base.exp.m3.y, col=line.colors[3])
lines(single.base.unif.y, col=line.colors[4])
lines(single.base.upfr.y, col=line.colors[5])
legend.text = c('exp; lambda=1', 'exp; lambda=-3', 'exp; lambda=3',
                'uniform', 'upfront')
legend(x='bottomright', legend=legend.text, col=line.colors, lwd=1)
SavePlotEnd()
################################################################################
## Single-Frame single.baselines with fixed-f and best fixed-t

single.base.f.0.01.dir = paste(no.seg.output.dir, 'fixed.f', 'f_0.01', sep='/')
single.base.f.0.01.l = LoadObject(file.name=file.name, dir=single.base.f.0.01.dir)
single.base.f.0.01.y = GetPerformance.no.seg(l=single.base.f.0.01.l, PerformanceFunction=MeanFMeasure)

single.base.f.0.05.dir = paste(no.seg.output.dir, 'fixed.f', 'f_0.05', sep='/')
single.base.f.0.05.l = LoadObject(file.name=file.name, dir=single.base.f.0.05.dir)
single.base.f.0.05.y = GetPerformance.no.seg(l=single.base.f.0.05.l, PerformanceFunction=MeanFMeasure)

single.base.f.0.1.dir = paste(no.seg.output.dir, 'fixed.f', 'f_0.1', sep='/')
single.base.f.0.1.l = LoadObject(file.name=file.name, dir=single.base.f.0.1.dir)
single.base.f.0.1.y = GetPerformance.no.seg(l=single.base.f.0.1.l, PerformanceFunction=MeanFMeasure)

single.base.f.0.5.dir = paste(no.seg.output.dir, 'fixed.f', 'f_0.5', sep='/')
single.base.f.0.5.l = LoadObject(file.name=file.name, dir=single.base.f.0.5.dir)
single.base.f.0.5.y = GetPerformance.no.seg(l=single.base.f.0.5.l, PerformanceFunction=MeanFMeasure)

SavePlotBegin(dir=graphics.dir, file.name='single.baselines.fixed.f')
line.colors = c(rgb(0, 1, 0), rgb(0, 0.7, 0), rgb(0, 0.3, 0), rgb(0, 0.1, 0), 
                rgb(0.5, 0, 0.5))
plot(single.base.f.0.01.y, ylim=c(0, 1), type='l', ylab='Mean F-Measure', 
     xlab='Number of Annotations = Size of training set', col=line.colors[1])
lines(single.base.f.0.05.y, col=line.colors[2])
lines(single.base.f.0.1.y, col=line.colors[3])
lines(single.base.f.0.5.y, col=line.colors[4])
lines(single.base.unif.y, col=line.colors[5])
legend.text = c('fixed prob; f=0.01', 'fixed prob; f=0.05', 
                'fixed prob; f=0.1', 'fixed prob; f=0.5', 
                'fixed time; uniform')
legend(x='bottomright', legend=legend.text, col=line.colors, lwd=1)
SavePlotEnd()

################################################################################
## Single-Frame AL with best baseline

no.seg.gamma.dir = '~/R/sf_ALTLAR/output/segmentation/varying.gamma.mean/with.recall'
file.name = 'index.sampling.sample'

single.al.gamma.6.dir = paste(no.seg.gamma.dir, 'gamma_6', sep='/')
single.al.gamma.6.l = LoadObject(file.name=file.name, dir=single.al.gamma.6.dir)
single.al.gamma.6.y = GetPerformance.no.seg(l=single.al.gamma.6.l, 
                                            PerformanceFunction=MeanFMeasure)

single.al.gamma.3.dir = paste(no.seg.gamma.dir, 'gamma_3', sep='/')
single.al.gamma.3.l = LoadObject(file.name=file.name, dir=single.al.gamma.3.dir)
single.al.gamma.3.y = GetPerformance.no.seg(l=single.al.gamma.3.l, 
                                            PerformanceFunction=MeanFMeasure)

single.al.gamma.1.dir = paste(no.seg.gamma.dir, 'gamma_1', sep='/')
single.al.gamma.1.l = LoadObject(file.name=file.name, dir=single.al.gamma.1.dir)
single.al.gamma.1.y = GetPerformance.no.seg(l=single.al.gamma.1.l, 
                                            PerformanceFunction=MeanFMeasure)

single.al.gamma.0.1.dir = paste(no.seg.gamma.dir, 'gamma_0.1', sep='/')
single.al.gamma.0.1.l = LoadObject(file.name=file.name, dir=single.al.gamma.0.1.dir)
single.al.gamma.0.1.y = GetPerformance.no.seg(l=single.al.gamma.0.1.l, 
                                            PerformanceFunction=MeanFMeasure)

line.colors = c(rgb(1, 0, 0), rgb(1, 0.5, 0.5), rgb(0.5, 0.5, 0.5), rgb(0.3, 0.3, 0.3), 
                rgb(0, 0, 1))
SavePlotBegin(dir=graphics.dir, file.name='single.al')
plot(single.al.gamma.6.y, ylim=c(0, 1), type='l', ylab='Mean F-Measure', 
     xlab='Number of Annotations = Size of training set', col=line.colors[1])
lines(single.al.gamma.3.y, col=line.colors[2])
lines(single.al.gamma.1.y, col=line.colors[3])
lines(single.al.gamma.0.1.y, col=line.colors[4])
lines(single.base.f.0.01.y, col=line.colors[5])
legend.text = c('gamma=6', 'gamma=3', 'gamma=1', 'gamma=0.1', 
                'fixed prob; f=0.01')
legend(x='bottomright', legend=legend.text, col=line.colors, lwd=1)
SavePlotEnd()

################################################################################
## Segment AL with best baseline

seg.gamma.dir = '~/R/sf_ALTLAR/output/segmentation/varying.gamma.mean'
seg.f.dir = '~/R/sf_ALTLAR/output/segmentation/varying.f.mean'
file.name = 'sample'

seg.al.gamma.6.dir = paste(seg.gamma.dir, 'gamma_6', sep='/')
seg.al.gamma.6.l = LoadObject(file.name=file.name, dir=seg.al.gamma.6.dir)
seg.al.gamma.6.ts = GetSegmentationLC.trainig_set_size(l=seg.al.gamma.6.l, 
                                                       max.ts.size=max.ts.size)
seg.al.gamma.6.int = GetSegmentationLC.interruptions(l=seg.al.gamma.6.l)

seg.al.gamma.3.dir = paste(seg.gamma.dir, 'gamma_3', sep='/')
seg.al.gamma.3.l = LoadObject(file.name=file.name, dir=seg.al.gamma.3.dir)
seg.al.gamma.3.ts = GetSegmentationLC.trainig_set_size(l=seg.al.gamma.3.l, 
                                                       max.ts.size=max.ts.size)
seg.al.gamma.3.int = GetSegmentationLC.interruptions(l=seg.al.gamma.3.l)

seg.al.gamma.1.dir = paste(seg.gamma.dir, 'gamma_1', sep='/')
seg.al.gamma.1.l = LoadObject(file.name=file.name, dir=seg.al.gamma.1.dir)
seg.al.gamma.1.ts = GetSegmentationLC.trainig_set_size(l=seg.al.gamma.1.l, 
                                                       max.ts.size=max.ts.size)
seg.al.gamma.1.int = GetSegmentationLC.interruptions(l=seg.al.gamma.1.l)

seg.al.gamma.0.1.dir = paste(seg.gamma.dir, 'gamma_0.1', sep='/')
seg.al.gamma.0.1.l = LoadObject(file.name=file.name, dir=seg.al.gamma.0.1.dir)
seg.al.gamma.0.1.ts = GetSegmentationLC.trainig_set_size(l=seg.al.gamma.0.1.l, 
                                                       max.ts.size=max.ts.size)
seg.al.gamma.0.1.int = GetSegmentationLC.interruptions(l=seg.al.gamma.0.1.l)

seg.f.0.01.dir = paste(seg.f.dir, 'f_0.01', sep='/')
seg.f.0.01.l = LoadObject(file.name=file.name, dir=seg.f.0.01.dir)
seg.f.0.01.ts = GetSegmentationLC.trainig_set_size(l=seg.f.0.01.l, max.ts.size=max.ts.size)
seg.f.0.01.int = GetSegmentationLC.interruptions(l=seg.f.0.01.l)

line.colors = c(rgb(0, 0, 1), rgb(0.3, 0.3, 1), rgb(0.6, 0.6, 1), rgb(0.7, 0.7, 0.7), 
                rgb(1, 0, 0))
SavePlotBegin(dir=graphics.dir, file.name='seg.al')
plot(seg.al.gamma.6.ts, type='l', ylim=c(0, 1), 
     ylab='Mean F-Measure', xlab='Size of training set',  col=line.colors[1])
lines(seg.al.gamma.3.ts, col=line.colors[2])
lines(seg.al.gamma.1.ts, col=line.colors[3])
lines(seg.al.gamma.0.1.ts, col=line.colors[4])
lines(seg.f.0.01.ts, col=line.colors[5])
legend.text = c('gamma=6', 'gamma=3', 'gamma=1', 'gamma=0.1', 'f=0.01')
legend(x='bottomright', legend=legend.text, col=line.colors, lwd=1)
SavePlotEnd()

################################################################################
## Segment vs Single-Frame learning
line.colors = c('red', 'blue', 'red', 'blue')
line.ltys = c('dashed', 'dashed', 'solid', 'solid')

SavePlotBegin(dir=graphics.dir, file.name='segment.vs.frame')
plot(single.al.gamma.6.y, type='l', ylim=c(0, 1), ylab='Mean F-Measure',
     xlab='Number of annotations', col=line.colors[1], lty=line.ltys[1])
lines(single.base.f.0.01.y, col=line.colors[2], lty=line.ltys[2])
lines(seg.al.gamma.6.int, col=line.colors[3], lty=line.ltys[3])
gamma.len = length(seg.al.gamma.6.int$y)
last.perf.gamma = seg.al.gamma.6.int$y[gamma.len]
remaining.x.gamma = (gamma.len+1): max.ts.size
lines(x=remaining.x.gamma, y=rep(last.perf.gamma, length(remaining.x.gamma)), 
      col=line.colors[3], lty='dotted')
lines(seg.f.0.01.int, col=line.colors[4], lty=line.ltys[4])
f.len = length(seg.f.0.01.int$y)
last.perf.f = seg.f.0.01.int$y[f.len]
remaining.x.f = (f.len+1): max.ts.size
lines(x=remaining.x.f, y=rep(last.perf.f, length(remaining.x.f)), 
      col=line.colors[4], lty='dotted')
legend.text = c('Single Frame AL', 'Single Frame RS', 
                'Segment AL', 'Segment RS')
legend(x='bottomright', legend=legend.text, col=line.colors, lty=line.ltys, lwd=1)
SavePlotEnd()

################################################################################
## Budget CDFs

line.colors = c(rgb(1, 0, 0), rgb(0.8, 0.4, 0), rgb(0.8, 0, 0.4),
                rgb(0, 1, 0), rgb(0, 0.8, 0.4), rgb(0, 0.4, 0.8))
line.ltys = c('solid', 'solid', 'solid',
              'solid', 'solid', 'solid')

GetBudgetSpendingCDF.no.seg = function(l) {
  num.reps = length(l$all)
  x.max = -1
  for (rep in l$all) {
    this.x.max = rep$cms.data$x.max
    if (x.max < this.x.max) {
      x.max = this.x.max
    }
  }
  
  y = rep(0, times=(x.max-1))
  for (rep in l$all) {
    nqsf = 0
    q.i = c(rep$cms.data$question.i, x.max)
    q.i.rep = q.i[2: length(q.i)] - q.i[1: (length(q.i)-1)]
    q.i = rep$cms.data$question.i
    for (i in seq_along(q.i)) {
      nqsf = 1 + nqsf
      for (k in (seq_len(q.i.rep[i])-1) ) {
        y[q.i[i] + k] = y[q.i[i] + k] + (nqsf / num.reps)
      }
    }
  }
  
  return( y )
}

seg.al.gamma.1.budget = GetBudgetSpendingCDF(l=seg.al.gamma.1.l)

seg.al.gamma.2.dir = paste(seg.gamma.dir, 'gamma_2', sep='/')
seg.al.gamma.2.l = LoadObject(file.name=file.name, dir=seg.al.gamma.2.dir)
seg.al.gamma.2.budget = GetBudgetSpendingCDF(l=seg.al.gamma.2.l)

seg.al.gamma.3.budget = GetBudgetSpendingCDF(l=seg.al.gamma.3.l)

single.al.gamma.1.budget = GetBudgetSpendingCDF.no.seg(l=single.al.gamma.1.l)

single.al.gamma.2.dir = paste(no.seg.gamma.dir, 'gamma_2', sep='/')
file.name = 'index.sampling.sample'
single.al.gamma.2.l = LoadObject(file.name=file.name, dir=single.al.gamma.2.dir)
single.al.gamma.2.budget = GetBudgetSpendingCDF.no.seg(l=single.al.gamma.2.l)

single.al.gamma.3.budget = GetBudgetSpendingCDF.no.seg(l=single.al.gamma.3.l)

seg.gamma.1.len = length(seg.al.gamma.1.budget)
seg.gamma.2.len = length(seg.al.gamma.2.budget)
seg.gamma.3.len = length(seg.al.gamma.3.budget)
single.gamma.1.len = length(single.al.gamma.1.budget)
single.gamma.2.len = length(single.al.gamma.2.budget)
single.gamma.3.len = length(single.al.gamma.3.budget)
cdf.x.max = max(seg.gamma.1.len, seg.gamma.2.len, seg.gamma.3.len, 
                single.gamma.1.len, single.gamma.2.len, single.gamma.3.len)
cdf.y.max = max(seg.al.gamma.1.budget, seg.al.gamma.2.budget, seg.al.gamma.3.budget,
                single.al.gamma.1.budget, single.al.gamma.2.budget, single.al.gamma.3.budget)

seg.al.gamma.1.budget.elon = ElongateCDF(y=seg.al.gamma.1.budget, x.max=cdf.x.max)
seg.al.gamma.2.budget.elon = ElongateCDF(y=seg.al.gamma.2.budget, x.max=cdf.x.max)
seg.al.gamma.3.budget.elon = ElongateCDF(y=seg.al.gamma.3.budget, x.max=cdf.x.max)
single.al.gamma.1.budget.elon = ElongateCDF(y=single.al.gamma.1.budget, x.max=cdf.x.max)
single.al.gamma.2.budget.elon = ElongateCDF(y=single.al.gamma.2.budget, x.max=cdf.x.max)
single.al.gamma.3.budget.elon = ElongateCDF(y=single.al.gamma.3.budget, x.max=cdf.x.max)

SavePlotBegin(dir=graphics.dir, file.name='budget.cdf')
plot(seg.al.gamma.1.budget.elon, type='l', ylim=c(0, cdf.y.max), xlim=c(0, cdf.x.max),
     xlab='Number of frames seen', ylab='Number of annotations', col=line.colors[1],
     lty=line.ltys[1])
lines(seg.al.gamma.2.budget.elon, col=line.colors[2], lty=line.ltys[2])
lines(seg.al.gamma.3.budget.elon, col=line.colors[3], lty=line.ltys[3])
lines(single.al.gamma.1.budget.elon, col=line.colors[4], lty=line.ltys[4])
lines(single.al.gamma.2.budget.elon, col=line.colors[5], lty=line.ltys[5])
lines(single.al.gamma.3.budget.elon, col=line.colors[6], lty=line.ltys[6])
legend.text = c('segment; gamma=1', 'segment; gamma=2', 'segment; gamma=3', 
                'frame; gamma=1', 'frame; gamma=2', 'frame; gamma=3')
legend(x='right', legend=legend.text, col=line.colors, lwd=1, lty=line.ltys)
SavePlotEnd()

################################################################################
## end performance gamma

file.name = 'sample'

seg.al.gamma.0.2.dir = paste(seg.gamma.dir, 'gamma_0.2', sep='/')
seg.al.gamma.0.2.l = LoadObject(file.name=file.name, dir=seg.al.gamma.0.2.dir)

seg.al.gamma.0.5.dir = paste(seg.gamma.dir, 'gamma_0.5', sep='/')
seg.al.gamma.0.5.l = LoadObject(file.name=file.name, dir=seg.al.gamma.0.5.dir)

seg.al.gamma.4.dir = paste(seg.gamma.dir, 'gamma_4', sep='/')
seg.al.gamma.4.l = LoadObject(file.name=file.name, dir=seg.al.gamma.4.dir)

seg.al.gamma.5.dir = paste(seg.gamma.dir, 'gamma_5', sep='/')
seg.al.gamma.5.l = LoadObject(file.name=file.name, dir=seg.al.gamma.5.dir)

seg.end.performances = c(GetEndPerformance(l=seg.al.gamma.0.1.l, max.ts.size=max.ts.size),
                         GetEndPerformance(l=seg.al.gamma.0.2.l, max.ts.size=max.ts.size),
                         GetEndPerformance(l=seg.al.gamma.0.5.l, max.ts.size=max.ts.size),
                         GetEndPerformance(l=seg.al.gamma.1.l, max.ts.size=max.ts.size),
                         GetEndPerformance(l=seg.al.gamma.2.l, max.ts.size=max.ts.size),
                         GetEndPerformance(l=seg.al.gamma.3.l, max.ts.size=max.ts.size),
                         GetEndPerformance(l=seg.al.gamma.4.l, max.ts.size=max.ts.size),
                         GetEndPerformance(l=seg.al.gamma.5.l, max.ts.size=max.ts.size),
                         GetEndPerformance(l=seg.al.gamma.6.l, max.ts.size=max.ts.size))
seg.end.x = c(0.1, 0.2, 0.5, 1, 2, 3, 4, 5, 6)

file.name = 'index.sampling.sample'

single.al.gamma.0.1.end = single.al.gamma.0.1.y[length(single.al.gamma.0.1.y)]

single.al.gamma.0.2.dir = paste(no.seg.gamma.dir, 'gamma_0.2', sep='/')
single.al.gamma.0.2.l = LoadObject(file.name=file.name, dir=single.al.gamma.0.2.dir)
single.al.gamma.0.2.y = GetPerformance.no.seg(l=single.al.gamma.0.2.l, 
                                              PerformanceFunction=MeanFMeasure)
single.al.gamma.0.2.end = single.al.gamma.0.2.y[length(single.al.gamma.0.2.y)]

single.al.gamma.0.5.dir = paste(no.seg.gamma.dir, 'gamma_0.5', sep='/')
single.al.gamma.0.5.l = LoadObject(file.name=file.name, dir=single.al.gamma.0.5.dir)
single.al.gamma.0.5.y = GetPerformance.no.seg(l=single.al.gamma.0.5.l, 
                                              PerformanceFunction=MeanFMeasure)
single.al.gamma.0.5.end = single.al.gamma.0.5.y[length(single.al.gamma.0.5.y)]

single.al.gamma.1.end = single.al.gamma.1.y[length(single.al.gamma.1.y)]

single.al.gamma.2.dir = paste(no.seg.gamma.dir, 'gamma_2', sep='/')
single.al.gamma.2.l = LoadObject(file.name=file.name, dir=single.al.gamma.2.dir)
single.al.gamma.2.y = GetPerformance.no.seg(l=single.al.gamma.2.l, 
                                            PerformanceFunction=MeanFMeasure)
single.al.gamma.2.end = single.al.gamma.2.y[length(single.al.gamma.2.y)]

single.al.gamma.3.end = single.al.gamma.3.y[length(single.al.gamma.3.y)]

single.al.gamma.4.dir = paste(no.seg.gamma.dir, 'gamma_4', sep='/')
single.al.gamma.4.l = LoadObject(file.name=file.name, dir=single.al.gamma.4.dir)
single.al.gamma.4.y = GetPerformance.no.seg(l=single.al.gamma.4.l, 
                                              PerformanceFunction=MeanFMeasure)
single.al.gamma.4.end = single.al.gamma.4.y[length(single.al.gamma.4.y)]                                              
                                              
single.al.gamma.5.dir = paste(no.seg.gamma.dir, 'gamma_5', sep='/')
single.al.gamma.5.l = LoadObject(file.name=file.name, dir=single.al.gamma.5.dir)
single.al.gamma.5.y = GetPerformance.no.seg(l=single.al.gamma.5.l, 
                                              PerformanceFunction=MeanFMeasure)
single.al.gamma.5.end = single.al.gamma.5.y[length(single.al.gamma.5.y)]
single.al.gamma.6.end = single.al.gamma.6.y[length(single.al.gamma.6.y)]

single.end.performances = c(single.al.gamma.0.1.end, single.al.gamma.0.2.end, 
                            single.al.gamma.0.5.end, single.al.gamma.1.end, 
                            single.al.gamma.2.end, single.al.gamma.3.end, 
                            single.al.gamma.4.end, single.al.gamma.5.end,
                            single.al.gamma.6.end)

#ylim = c(min(seg.end.performances, single.end.performances), 
#         max(seg.end.performances, single.end.performances))
ylim = c(0.75, 0.95)

SavePlotBegin(dir=graphics.dir, file.name='end.performance.gamma')
plot(x=seg.end.x, y=seg.end.performances, ylim=ylim, type='l', col='red',
     xlab='gamma', ylab='End Mean F-Measure')
points(x=seg.end.x, y=seg.end.performances, col='red')
lines(x=seg.end.x, y=single.end.performances, col='purple', lty='dashed')
points(x=seg.end.x, y=single.end.performances, col='purple')
SavePlotEnd()

################################################################################
## end performance f

file.name = 'index.sampling.sample'

single.base.f.0.01.end = single.base.f.0.01.y[length(single.base.f.0.01.y)]

single.base.f.0.02.dir = paste(no.seg.output.dir, 'fixed.f/f_0.02', sep='/')
single.base.f.0.02.l = LoadObject(file.name=file.name, dir=single.base.f.0.02.dir)
single.base.f.0.02.y = GetPerformance.no.seg(l=single.base.f.0.02.l, 
                                             PerformanceFunction=MeanFMeasure)
single.base.f.0.02.end = single.base.f.0.02.y[length(single.base.f.0.02.y)]

single.base.f.0.05.end = single.base.f.0.05.y[length(single.base.f.0.05.y)]

single.base.f.0.1.end = single.base.f.0.1.y[length(single.base.f.0.1.y)]

single.base.f.0.2.dir = paste(no.seg.output.dir, 'fixed.f/f_0.2', sep='/')
single.base.f.0.2.l = LoadObject(file.name=file.name, dir=single.base.f.0.2.dir)
single.base.f.0.2.y = GetPerformance.no.seg(l=single.base.f.0.2.l, 
                                             PerformanceFunction=MeanFMeasure)
single.base.f.0.2.end = single.base.f.0.2.y[length(single.base.f.0.2.y)]

single.base.f.0.3.dir = paste(no.seg.output.dir, 'fixed.f/f_0.3', sep='/')
single.base.f.0.3.l = LoadObject(file.name=file.name, dir=single.base.f.0.3.dir)
single.base.f.0.3.y = GetPerformance.no.seg(l=single.base.f.0.3.l, 
                                            PerformanceFunction=MeanFMeasure)
single.base.f.0.3.end = single.base.f.0.3.y[length(single.base.f.0.3.y)]

single.base.f.0.4.dir = paste(no.seg.output.dir, 'fixed.f/f_0.4', sep='/')
single.base.f.0.4.l = LoadObject(file.name=file.name, dir=single.base.f.0.4.dir)
single.base.f.0.4.y = GetPerformance.no.seg(l=single.base.f.0.4.l, 
                                            PerformanceFunction=MeanFMeasure)
single.base.f.0.4.end = single.base.f.0.4.y[length(single.base.f.0.4.y)]

single.base.f.0.5.end = single.base.f.0.5.y[length(single.base.f.0.5.y)]

single.base.end.performances = c(single.base.f.0.01.end, single.base.f.0.02.end, 
                                 single.base.f.0.05.end, single.base.f.0.1.end,
                                 single.base.f.0.2.end, single.base.f.0.3.end, 
                                 single.base.f.0.4.end, single.base.f.0.5.end)

single.base.x = c(0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5)

seg.f.0.01.end = seg.f.0.01.int$y[length(seg.f.0.01.int$y)]

file.name = 'sample'

seg.f.0.02.dir = paste(seg.f.dir, 'f_0.02', sep='/')
seg.f.0.02.l = LoadObject(file.name=file.name, dir=seg.f.0.02.dir)
seg.f.0.02.int = GetSegmentationLC.interruptions(l=seg.f.0.02.l)
seg.f.0.02.end = seg.f.0.02.int$y[length(seg.f.0.02.int$y)]

seg.f.0.05.dir = paste(seg.f.dir, 'f_0.05', sep='/')
seg.f.0.05.l = LoadObject(file.name=file.name, dir=seg.f.0.05.dir)
seg.f.0.05.int = GetSegmentationLC.interruptions(l=seg.f.0.05.l)
seg.f.0.05.end = seg.f.0.05.int$y[length(seg.f.0.05.int$y)]

seg.f.0.1.dir = paste(seg.f.dir, 'f_0.1', sep='/')
seg.f.0.1.l = LoadObject(file.name=file.name, dir=seg.f.0.1.dir)
seg.f.0.1.int = GetSegmentationLC.interruptions(l=seg.f.0.1.l)
seg.f.0.1.end = seg.f.0.1.int$y[length(seg.f.0.1.int$y)]

seg.f.0.2.dir = paste(seg.f.dir, 'f_0.2', sep='/')
seg.f.0.2.l = LoadObject(file.name=file.name, dir=seg.f.0.2.dir)
seg.f.0.2.int = GetSegmentationLC.interruptions(l=seg.f.0.2.l)
seg.f.0.2.end = seg.f.0.2.int$y[length(seg.f.0.2.int$y)]

seg.f.0.3.dir = paste(seg.f.dir, 'f_0.3', sep='/')
seg.f.0.3.l = LoadObject(file.name=file.name, dir=seg.f.0.3.dir)
seg.f.0.3.int = GetSegmentationLC.interruptions(l=seg.f.0.3.l)
seg.f.0.3.end = seg.f.0.3.int$y[length(seg.f.0.3.int$y)]

seg.f.0.4.dir = paste(seg.f.dir, 'f_0.4', sep='/')
seg.f.0.4.l = LoadObject(file.name=file.name, dir=seg.f.0.4.dir)
seg.f.0.4.int = GetSegmentationLC.interruptions(l=seg.f.0.4.l)
seg.f.0.4.end = seg.f.0.4.int$y[length(seg.f.0.4.int$y)]

seg.f.0.5.dir = paste(seg.f.dir, 'f_0.5', sep='/')
seg.f.0.5.l = LoadObject(file.name=file.name, dir=seg.f.0.5.dir)
seg.f.0.5.int = GetSegmentationLC.interruptions(l=seg.f.0.5.l)
seg.f.0.5.end = seg.f.0.5.int$y[length(seg.f.0.5.int$y)]

seg.base.end.performances = c(seg.f.0.01.end, seg.f.0.02.end, seg.f.0.05.end, seg.f.0.1.end,
                              seg.f.0.2.end, seg.f.0.3.end, seg.f.0.4.end, seg.f.0.5.end)


line.colors = c('blue', 'blue')
line.ltys = c('dashed', 'solid')
point.pchs = c(1, 5)

#ylim = c(min(single.base.end.performances, seg.base.end.performances),
#         max(single.base.end.performances, seg.base.end.performances))

ylim = c(0.7, 0.9)

plot(x=single.base.x, y=single.base.end.performances, type='l', ylim=ylim,
     xlab='f', ylab='End Mean F-Measure', col=line.colors[1], lty=line.ltys[1])
points(x=single.base.x, y=single.base.end.performances, col=line.colors[1],
       pch=point.pchs[1])
lines(x=single.base.x, y=seg.base.end.performances, col=line.colors[2])
points(x=single.base.x, y=seg.base.end.performances, col=line.colors[2], pch=point.pchs[2])
#stop('Graphs done!')

##### 
# learning curves
gamma = 6
gamma.dir = paste('gamma', gamma, sep='_')
gamma.path = paste(output.dir, varying.gamma.dir, gamma.dir, sep='/')
l.gamma = LoadObject(file.name='sample', dir=gamma.path)
l.gamma.6 = l.gamma
gamma.perf.ts = GetSegmentationLC.trainig_set_size(l=l.gamma, max.ts.size=max.ts.size)
gamma.perf.int = GetSegmentationLC.interruptions(l=l.gamma)

f = 0.01
f.dir = paste('f', f, sep='_')
f.path = paste(output.dir, varying.f.dir, f.dir, sep='/')
l.f = LoadObject(file.name='sample', dir=f.path)
f.perf.ts = GetSegmentationLC.trainig_set_size(l=l.f, max.ts.size=max.ts.size)
f.perf.int = GetSegmentationLC.interruptions(l=l.f)

gamma.y.ts = gamma.perf.ts$y
f.y.ts = f.perf.ts$y
x.ts = seq_len(max.ts.size)

SavePlotBegin(dir=graphics.dir, file.name='ALvsRS_training.set.size')
plot(x=x.ts, y=gamma.y.ts[x.ts], ylim=c(0,1), col='red', type='l', 
     xlab='Size of training set', ylab='Mean F-Measure')
lines(x=x.ts, y=f.y.ts[x.ts], col='blue')
legend(x='bottomright', legend=c('Active Learning', 'Random Selection'), 
       col=c('red', 'blue'), lwd=c(1, 1))
SavePlotEnd()

gamma.y.int = gamma.perf.int$y
f.y.int = f.perf.int$y
x.int = seq_len(min(length(gamma.y.int), length(f.y.int)))

SavePlotBegin(dir=graphics.dir, file.name='ALvsRS_annotations')
plot(x=x.int, y=gamma.y.int[x.int], ylim=c(0,1), col='red', type='l', 
     xlab='Number of annotations', ylab='Mean F-Measure')
lines(x=x.int, y=f.y.int[x.int], col='blue')
legend(x='bottomright', legend=c('Active Learning', 'Random Selection'), 
       col=c('red', 'blue'), lwd=c(1, 1))
SavePlotEnd()

## interruptions w/ segmentation vs. training set size w/o segmentation
dir='output/segmentation/varying.gamma.no.seg/without.recall/gamma_3'
file.name='index.sampling.sample'
l.gamma.no.seg = LoadObject(file.name=file.name, dir=dir)

dir='output/segmentation/varying.f.mean/without.recall/f_0.1'
file.name='index.sampling.sample'
l.f.no.seg = LoadObject(file.name=file.name, dir=dir)

GetPerformance.no.segmentation = function(l) {
  y = rep(0, times=max.ts.size)
  num.individuals = length(l$individuals)
  
  for (ind in l$individuals) {
    y.ind = GetPerformance(cms=ind$cms, PerformanceFunction=MeanFMeasure)
    y = y + (y.ind / num.individuals)
  }
  
  return( y )
}

gamma.no.seg.y = GetPerformance.no.segmentation(l=l.gamma.no.seg)
gamma.no.seg.y = gamma.no.seg.y[x.ts]
gamma.no.seg.y.last = gamma.no.seg.y[length(gamma.no.seg.y)]

f.no.seg.y = GetPerformance.no.segmentation(l=l.f.no.seg)
f.no.seg.y = f.no.seg.y[x.ts]
f.no.seg.y.last = f.no.seg.y[length(f.no.seg.y)]

gamma.y.int.last = gamma.y.int[length(gamma.y.int)]
f.y.int.last = f.y.int[length(f.y.int)]

SavePlotBegin(dir=graphics.dir, file.name='ALvsRS_both')
plot(x=x.ts, y=gamma.no.seg.y[x.ts], ylim=c(0, 1), col='red', type='l',
     lty='dashed', xlab='Number of annotation interruptions',
     ylab='Mean F-Measure')
lines(x=x.ts, y=f.no.seg.y[x.ts], col='blue', lty='dashed')
lines(x=seq_along(gamma.y.int), y=gamma.y.int, col='red')
lines(x=x.ts, y=rep(gamma.y.int.last, times=length(x.ts)), col='black', lty='dotted')
lines(x=seq_along(f.y.int), y=f.y.int, col='blue')
lines(x=x.ts, y=rep(f.y.int.last, times=length(x.ts)), col='black', lty='dotted')
legend.text = c('Active Learning without Segmentation', 
                'Random Selection without Segmentation',
                'Active Learning with Segmentation',
                'Random Selection with Segmentation')
legend(x='bottomright', legend=legend.text, col=c('red', 'blue', 'red', 'blue'),
       lwd=c(1, 1, 1, 1), lty=c('dashed', 'dashed', 'solid', 'solid'))
SavePlotEnd()

seg.acc = Accuracy(GetSegmentationCM(l=l.gamma))
seg.acc.str = paste('segmentation accuracy:', seg.acc)

min.conf.correct = GetMeanMinConfCorrect(l=l.gamma)
min.conf.correct.str = paste('min. conf correctly annotated:', min.conf.correct)

seg.len = GetMeanSegLength(l.gamma)
seg.len.str = paste('average segment length:', seg.len)

effort.ratio = length(gamma.y.int) / length(gamma.no.seg.y)
effort.ratio.str = paste('annotation effort ratio:', effort.ratio)

performance.ratio = gamma.y.int.last / gamma.no.seg.y.last
performance.ratio.str = paste('performance ratio:', performance.ratio)

text.path = paste(graphics.dir, 'text.output.txt', sep='/')
file.create(text.path)

file.con = file(text.path)
text = c(seg.acc.str, min.conf.correct.str, seg.len.str, 
         effort.ratio.str, performance.ratio.str)
writeLines(text=text, con=file.con)
close(file.con)

## segmentation cm
seg.cm.file = paste(graphics.dir, 'seg.cm.txt', sep='/')
seg.cm = GetSegmentationCM.normalized(l=l.gamma.6)
seg.cm = round(seg.cm, digits=3)
file.con = file(seg.cm.file)
write(x=seg.cm, file=file.con)
close(file.con)

#####
# varying gamma and f
gammas = c(0.1, 0.2, 0.5, 1, 2, 3, 4, 5, 6)
gamma.perf.last = c()
gamma.ttsb = c()
gamma.mean.auc = c()
gamma.budget.spending.cdfs = list()
gamma.perf.cdfs = list()
for (gamma in gammas) {
  gamma.dir = paste('gamma', gamma, sep='_')
  gamma.path = paste(output.dir, varying.gamma.dir, gamma.dir, sep='/')
  l.gamma = LoadObject(file.name='sample', dir=gamma.path)
  
  gamma.last.y = GetEndPerformance(l=l.gamma, max.ts.size=max.ts.size)
  gamma.perf.last = c(gamma.perf.last, gamma.last.y)
  
  ttsb = GetMeanTTSB(l=l.gamma)
  gamma.ttsb = c(gamma.ttsb, ttsb)
  
  auc = GetMeanAUC(l=l.gamma)
  gamma.mean.auc = c(gamma.mean.auc, auc)
  
  budget.cdf = GetBudgetSpendingCDF(l=l.gamma)
  gamma.budget.spending.cdfs[[1+length(gamma.budget.spending.cdfs)]] = budget.cdf
  
  perf.cdf = GetPerformanceCDF(l=l.gamma)
  gamma.perf.cdfs[[1+length(gamma.perf.cdfs)]] = perf.cdf
}

gamma.colours = list(rgb(0.7, 0.7, 0.0), rgb(0.3, 0.3, 0.1), rgb(0.1, 0.6, 0.8),
                     rgb(0.0, 0.0, 1.0), rgb(0, 0.5, 1), rgb(0.4, 0.0, 0.4),                     
                     rgb(0.6, 0.6, 0.3), rgb(0.4, 0.4, 0.3), rgb(0.2, 0.2, 0.3))

gamma.budget.length = 0
max.q = 0
restrict.i = 4: 6

old.gamma.colours = gamma.colours
old.gamma.budget.spending.cdfs = gamma.budget.spending.cdfs
old.gamma.perf.cdfs = gamma.perf.cdfs

gamma.colours = gamma.colours[restrict.i]
gamma.budget.spending.cdfs = gamma.budget.spending.cdfs[restrict.i]
gamma.perf.cdfs = gamma.perf.cdfs[restrict.i]

for (i in seq_along(gamma.budget.spending.cdfs)) {
  budget.len = length(gamma.budget.spending.cdfs[[i]])
  if (gamma.budget.length < budget.len) {
    gamma.budget.length = budget.len
  }
  this.len = length(gamma.budget.spending.cdfs[[i]])
  this.q = gamma.budget.spending.cdfs[[i]][this.len]
  if (max.q < this.q) {
    max.q = this.q
  }
}

for (i in seq_along(gamma.budget.spending.cdfs)) {
  gamma.budget.spending.cdfs[[i]] = ElongateCDF(y=gamma.budget.spending.cdfs[[i]], 
                                                x.max=gamma.budget.length)
  gamma.perf.cdfs[[i]] = ElongateCDF(y=gamma.perf.cdfs[[i]], 
                                     x.max=gamma.budget.length)
}

SavePlotBegin(dir=graphics.dir, file.name='gamma.perf.cdf')
plot(gamma.perf.cdfs[[1]], type='l', col=gamma.colours[[1]], ylim=c(0, 1),
     xlab='Number of frames seen', ylab='Mean F-Measure')
for (i in 2: length(gamma.perf.cdfs)) {
  lines(gamma.perf.cdfs[[i]], col=gamma.colours[[i]])
}
legend.text = c()
legend.col = c()
for (i in seq_along(gamma.perf.cdfs)) {
  this.text = paste('gamma', gammas[restrict.i][i], sep=' = ')
  legend.text = c(legend.text, this.text)
  legend.col = c(legend.col, gamma.colours[[i]])
}
legend(x='bottomright', legend=legend.text, col=legend.col, lwd=1)
SavePlotEnd()

SavePlotBegin(dir=graphics.dir, file.name='gamma.budget.cdf')
plot(gamma.budget.spending.cdfs[[1]], type='l', col=gamma.colours[[1]], 
     ylim=c(0, max.q),
     xlab='Number of frames seen', ylab='Number of annotations')
for (i in 2: length(gamma.budget.spending.cdfs)) {
  lines(gamma.budget.spending.cdfs[[i]], col=gamma.colours[[i]])
}
legend.text = c()
legend.col = c()
for (i in seq_along(gamma.budget.spending.cdfs)) {
  this.text = paste('gamma', gammas[restrict.i][i], sep=' = ')
  legend.text = c(legend.text, this.text)
  legend.col = c(legend.col, gamma.colours[[i]])
}
legend(x='bottomright', legend=legend.text, col=legend.col, lwd=1)
SavePlotEnd()

fs = c(0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5)
f.perf.last = c()
f.ttsb = c()
f.mean.auc = c()
f.budget.spending.cdfs = list()
f.perf.cdfs = list()
for (f in fs) {
  f.dir = paste('f', f, sep='_')
  f.path = paste(output.dir, varying.f.dir, f.dir, sep='/')
  l.f = LoadObject(file.name='sample', dir=f.path)
  
  f.last.y = GetEndPerformance(l=l.f, max.ts.size=max.ts.size)
  f.perf.last = c(f.perf.last, f.last.y)
  
  ttsb = GetMeanTTSB(l=l.f)
  f.ttsb = c(f.ttsb, ttsb)
  
  auc = GetMeanAUC(l=l.f)
  f.mean.auc = c(f.mean.auc, auc)
  
  budget.cdf = GetBudgetSpendingCDF(l=l.f)
  f.budget.spending.cdfs[[1 + length(f.budget.spending.cdfs)]] = budget.cdf
  
  perf.cdf = GetPerformanceCDF(l=l.f)
  f.perf.cdfs[[1 + length(f.perf.cdfs)]] = perf.cdf
}

f.colours = list(rgb(0.7, 0.7, 0.0), rgb(0.3, 0.3, 0.1), rgb(0.1, 0.6, 0.8),
                 rgb(1, 0.0, 0.0), rgb(1, 0.5, 0), rgb(0.7, 0.2, 0.7),                     
                 rgb(0.6, 0.6, 0.3), rgb(0.4, 0.4, 0.3), rgb(0.2, 0.2, 0.3))

f.budget.length = 0
max.q = 0
restrict.i = 4:6

old.f.colours = f.colours
old.f.budget.spending.cdfs = f.budget.spending.cdfs
old.f.perf.cdfs = f.perf.cdfs

f.colours = f.colours[restrict.i]
f.budget.spending.cdfs = f.budget.spending.cdfs[restrict.i]
f.perf.cdfs = f.perf.cdfs[restrict.i]

for (i in seq_along(f.budget.spending.cdfs)) {
  budget.len = length(f.budget.spending.cdfs[[i]])
  if (f.budget.length < budget.len) {
    f.budget.length = budget.len
  }
  this.len = length(f.budget.spending.cdfs[[i]])
  this.q = f.budget.spending.cdfs[[i]][this.len]
  if (max.q < this.q) {
    max.q = this.q
  }
}

#for (i in seq_along(f.budget.spending.cdfs)) {
for (i in seq_along(f.budget.spending.cdfs)) {
  f.budget.spending.cdfs[[i]] = ElongateCDF(y=f.budget.spending.cdfs[[i]], 
                                                x.max=f.budget.length)
  f.perf.cdfs[[i]] = ElongateCDF(y=f.perf.cdfs[[i]], 
                                     x.max=f.budget.length)
}

SavePlotBegin(dir=graphics.dir, file.name='f.perf.cdf')
plot(f.perf.cdfs[[1]], type='l', col=f.colours[[1]], ylim=c(0, 1),
     xlab='Number of frames seen', ylab='Mean F-Measure')
for (i in 2: length(f.perf.cdfs)) {
  lines(f.perf.cdfs[[i]], col=f.colours[[i]])
}
legend.text = c()
legend.col = c()
for (i in seq_along(f.perf.cdfs)) {
  this.text = paste('f', fs[restrict.i][i], sep=' = ')
  legend.text = c(legend.text, this.text)
  legend.col = c(legend.col, f.colours[[i]])
}
legend(x='bottomright', legend=legend.text, col=legend.col, lwd=1)
SavePlotEnd()

SavePlotBegin(dir=graphics.dir, file.name='f.budget.cdf')
plot(f.budget.spending.cdfs[[1]], type='l', col=f.colours[[1]], ylim=c(0, max.q),
     xlab='Number of frames seen', ylab='Number of annotations')
for (i in 2: length(f.budget.spending.cdfs)) {
  lines(f.budget.spending.cdfs[[i]], col=f.colours[[i]])
}
legend.text = c()
legend.col = c()
for (i in seq_along(f.budget.spending.cdfs)) {
  this.text = paste('f', fs[restrict.i][i], sep=' = ')
  legend.text = c(legend.text, this.text)
  legend.col = c(legend.col, f.colours[[i]])
}
legend(x='bottomright', legend=legend.text, col=legend.col, lwd=1)
SavePlotEnd()

SavePlotBegin(dir=graphics.dir, file.name='budget')
old.par = par(mfrow=c(3, 2))
plot(x=gammas, y=gamma.perf.last, type='l', ylim=c(0.7, 0.9), col='red',
     xlab='gamma', ylab='Top Classification Performance')
points(x=gammas, y=gamma.perf.last)
plot(x=fs, y=f.perf.last, type='l', ylim=c(0.7, 0.9), col='blue', 
     xlab='f', ylab='Top Classification Performance')
points(x=fs, y=f.perf.last)

plot(x=gammas, y=gamma.mean.auc, type='l', ylim=c(0.5, 0.7), col='red', 
     xlab='gamma', ylab='Mean AUC')
points(x=gammas, y=gamma.mean.auc)
plot(x=fs, y=f.mean.auc, type='l', ylim=c(0.5, 0.7), col='blue', 
     xlab='f', ylab='Mean AUC')
points(x=fs, y=f.mean.auc)

plot(x=gammas, y=gamma.ttsb, type='l', col='red',
     xlab='gamma', ylab='Time to spend budget')
points(x=gammas, y=gamma.ttsb)
plot(x=fs, y=f.ttsb, type='l', col='blue', 
     xlab='f', ylab='Time to spend budget')
points(x=fs, y=f.ttsb)
par(mfrow=c(1, 1))
SavePlotEnd()



#####
# dummy acceleration figure
source('R/load.R')

setwd("/home/tudor/R/sf_ALTLAR/input")
s = ReadSubjectsData()
cat('Loaded data for', length(s), 'subjects\n')
setwd("/home/tudor/R/sf_ALTLAR")
dummy.data = s[[1]]$data[[4]]$acc
i = 5001:5500
acc.x = dummy.data$x[i]
acc.y = dummy.data$y[i]
acc.z = dummy.data$z[i]
ylim = c(min(acc.x, acc.y, acc.z), max(acc.x, acc.y, acc.z))

SavePlotBegin(dir=graphics.dir, file.name='dummy.acceleration')
plot(acc.x, type='l', col='red', ylim=ylim, ylab='', xlab='', xaxt='n', yaxt='n', ann=F)
lines(acc.y, col='green')
lines(acc.z, col='blue')
legend(x='topright', legend=c('x', 'y', 'z'), col=c('red', 'green', 'blue'), lwd=1, 
       x.intersp=0.2, y.intersp=0.7, seg.len=1.2)
SavePlotEnd()
