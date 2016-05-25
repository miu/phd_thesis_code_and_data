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

library(RWeka)

source('R/ml.utils.R')
source('R/subjects.search.R')
source('R/organize.R')

######################
## RS within a fixed time frame

UniformGenerator = function(N, Nb.trial, ...) {
  i.distr = runif(Nb.trial)
  # standardize to [0, 1]
  i.distr = (i.distr - min(i.distr)) / (max(i.distr) - min(i.distr))
  i.distr = as.integer(i.distr * N)
  i.distr = i.distr[-which(i.distr == 0)]
  i.distr = i.distr[-which(i.distr == max(i.distr))]
  
  return( sort(i.distr) )
}

ExponentialGenerator = function(N, Nb.trial, rate, ...) {
  i.distr = rexp(Nb.trial, rate=rate)
  # standardize to [0, 1]
  i.distr = (i.distr - min(i.distr)) / (max(i.distr) - min(i.distr))
  i.distr = as.integer(i.distr * N)
  i.distr = i.distr[-which(i.distr == 0)]
  i.distr = i.distr[-which(i.distr == max(i.distr))]
  
  return( sort(i.distr) )
}

LogNormalGenerator = function(N, Nb.trial, meanlog, sdlog, ...) {
  i.distr = rlnorm(Nb.trial, meanlog=meanlog, sdlog=sdlog)
  # standardize to [0, 1]
  i.distr = (i.distr - min(i.distr)) / (max(i.distr) - min(i.distr))
  i.distr = as.integer(i.distr * N)
  i.distr = i.distr[-which(i.distr == 0)]
  i.distr = i.distr[-which(i.distr == max(i.distr))]
  
  return( sort(i.distr) )
}

NormalGenerator = function(N, Nb.trial, mean, sd, ...) {
  i.distr = rnorm(Nb.trial, mean=mean, sd=sd)
  # standardize to [0, 1]
  i.distr = (i.distr - min(i.distr)) / (max(i.distr) - min(i.distr))
  i.distr = as.integer(i.distr * N)
  i.distr = i.distr[-which(i.distr == 0)]
  i.distr = i.distr[-which(i.distr == max(i.distr))]
  
  return( sort(i.distr) )
}

UpfrontGenerator = function(...) {
  
}

SampleIndices = function(N, Nb, Nb.trial, generator, ...) {
  # N: upper limit of indices (1 to N)
  # Nb: number of returned sample points
  # Nb.trialed: number sampled points from which to choose Nb unique ones, per iteration
  # rand.generator: random number generator according to a distribution
  #                 (dnorm, dunif, dpois, etc.)
  # ... parameters passed to rand.generator
  
  i.distr.so.far = NULL
  if (identical(generator, UpfrontGenerator)) {
    return( 1:Nb )
  }
  
  while (T) {
    #cat('length(table(i.distr.so.far)) =', length(table(i.distr.so.far)), '\n')
    #print(table(i.distr.so.far))
    #cat('\n')
    i.distr = generator(N=N, Nb.trial=Nb.trial, ...)
    i.distr.so.far = c(i.distr.so.far, i.distr)    
    
    i.distr.table = table(i.distr.so.far)
    #print(i.distr.table)
    len = length(i.distr.table)
    #cat('len =', len, '\n')
    if (len >= Nb) {
      i.distr.so.far = sort(i.distr.table, decreasing=T)
      return( as.integer(names(i.distr.so.far[1:Nb])) )
    } 
  }  
}

SimulateSamplingForSubject.budget = function(subject.data, Nb, Nb.trial, generator, 
                                             classifier.name, weka.control, min.train.size, 
                                             class.labels, num.folds, ...) {
  # sample from the entire subject's pool of data
  N = nrow(subject.data)
  
  question.i = SampleIndices(N=N, Nb=Nb, Nb.trial=Nb.trial, 
                             generator=generator, ...)
  question.i = sort(question.i)
  zeror.name = 'weka/classifiers/rules/ZeroR'
  training.set = subject.data[0, ]
  training.set.compl = subject.data
  cms = list()
  for (i in question.i) {
    training.set = rbind(training.set, subject.data[i, ])
    training.set.compl = training.set.compl[-(i-nrow(training.set)+1), ]
    #cat(nrow(training.set) + nrow(training.set.compl), '')
    if (nrow(training.set) >= min.train.size) {
      c.name = classifier.name
    } else {
      c.name = zeror.name
    }
    
    conf.matrix = CrossValidateWithSubsample(classifier.name=c.name, 
                                             weka.control=weka.control, 
                                             data.subsample=training.set, 
                                             data.complementary=training.set.compl, 
                                             class.labels=class.labels, 
                                             num.folds=num.folds)
    cms[[1 + length(cms)]] = conf.matrix
  }
  return( list(cms=cms, question.i=question.i, 
               x.max=nrow(subject.data)) )
}

SimulateSamplingForSample.budget = function(subjects.features, repetitions, Nb, 
                                            SubjectSamplingFunction, to.stream, 
                                            max.episode.length, ...) {
  num.subjects = length(subjects.features)
  c.p = expand.grid(subject.i=seq_len(num.subjects), rep=seq_len(repetitions))
  l = foreach(row.i = seq_len(nrow(c.p))) %dopar% {
    source('R/index.sampling.R')
    #cat('.')
    row = c.p[row.i, ]
    subject.i = row$subject.i
    subject.data = subjects.features[[subject.i]]$data
    if (to.stream) {
      subject.data = TurnPoolIntoStream(subject.data=subject.data, 
                                        max.episode.length=max.episode.length)
    }
    #cms.data = SimulateSamplingForSubject(subject.data=subject.data, Nb=Nb, ...)
    cms.data = SubjectSamplingFunction(subject.data=subject.data, Nb=Nb, 
                                       max.episode.length, ...)
    #print(cms.data)
    list( subject.i=subject.i, cms.data=cms.data )
  }
  all = l
  #cat('---- 0 \n')
  # average over repetitions, but keeping subjects intact
  # compute normalized AUC for each rep and compute its average for each subject
  cms.sample = list()
  for (i in seq_along(subjects.features)) {
    cms.sample[[i]] = list()
    #cat('---- 1 \n')
    cms.sample[[i]]$cms = list()
    for (k in seq_len(Nb)) {
      cms.sample[[i]]$cms[[k]] = 0
    }
    #cat('---- 2 \n')
    cms.sample[[i]]$auc.norm = 0
  }
  #cat('---- 3 \n')
  for (i in seq_along(l)) {
    subject.i = l[[i]]$subject.i
    cms = l[[i]]$cms.data$cms
    y = sapply(cms, MeanFMeasure)
    #print(y)
    x = l[[i]]$cms.data$question.i
    x.max = l[[i]]$cms.data$x.max # nrow(subjects.features[[subject.i]]$data)
    cat(' x.max =', x.max, '\n')
    auc.norm = NormalizedAUC(x=x, x.max=x.max, y=y)
    
    cms.sample[[subject.i]]$auc.norm = cms.sample[[subject.i]]$auc.norm + 
      (auc.norm / repetitions)
    
    for (k in seq_len(Nb)) {
      cms.sample[[subject.i]]$cms[[k]] = cms.sample[[subject.i]]$cms[[k]] + cms[[k]]
    }
  }
  
  # now average over all subjects
  l = list()
  l$auc.norm = 0
  l$cms = list()
  for (k in seq_len(Nb)) {
    l$cms[[k]] = 0
  }
  
  for (i in seq_along(cms.sample)) {
    subject = cms.sample[[i]]
    for (k in seq_len(Nb)) {
      l$cms[[k]] = l$cms[[k]] + subject$cms[[k]]
    }
    l$auc.norm = l$auc.norm + (subject$auc.norm / length(subjects.features))
  }
  
  l$individuals = cms.sample
  l$all = all
  
  return( l )
}

RunSimulationForSample = function(output.dir, graphics.dir, ...) {
  l = SimulateSamplingForSample(...)
  # save l to file
  var.name = 'index.sampling.sample'
  SaveObject(obj=l, var.name=var.name, dir=output.dir)
  
  #cms = l$cms
  ##print(cms)
  ##print(names(cms))
  #
  #x = seq_along(cms)
  #y.mfm = sapply(cms, MeanFMeasure)
  #y.acc = sapply(cms, Accuracy)
  #
  ## make plots
  ## MFM vs. training set size
  #SavePlotBegin(dir=graphics.dir, file.name='sample.mfm')
  #plot(x=x, y=y.mfm, ylim=c(0, 1), xlab='Training set size', 
  #     ylab='Mean F-Measure', type='l')
  #SavePlotEnd()
  #
  #SavePlotBegin(dir=graphics.dir, file.name='sample.acc')
  #plot(x=x, y=y.acc, ylim=c(0, 1), xlab='Training set size', 
  #     ylab='Accuracy', type='l')
  #SavePlotEnd()
}

RunSimulationForSample.pop = function(output.dir, graphics.dir, ...) {
  l = SimulateSamplingForSample.pop(...)
  # save l to file
  var.name = 'index.sampling.sample'
  SaveObject(obj=l, var.name=var.name, dir=output.dir)
  
  cms = l$cms
  #print(cms)
  #print(names(cms))
  
  x = seq_along(cms)
  y.mfm = sapply(cms, MeanFMeasure)
  y.acc = sapply(cms, Accuracy)
  
  # make plots
  # MFM vs. training set size
  SavePlotBegin(dir=graphics.dir, file.name='sample.mfm')
  plot(x=x, y=y.mfm, ylim=c(0, 1), xlab='Training set size', 
       ylab='Mean F-Measure', type='l')
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name='sample.acc')
  plot(x=x, y=y.acc, ylim=c(0, 1), xlab='Training set size', 
       ylab='Accuracy', type='l')
  SavePlotEnd()
}

RunSimulationForSample.budget = function(output.dir, graphics.dir, ...) {
  l = SimulateSamplingForSample.budget(...)
  # save l to file
  var.name = 'index.sampling.sample'
  SaveObject(obj=l, var.name=var.name, dir=output.dir)
  
  cms = l$cms
  #print(cms)
  #print(names(cms))
  
  x = seq_along(cms)
  y.mfm = sapply(cms, MeanFMeasure)
  y.acc = sapply(cms, Accuracy)
  
  # make plots
  # MFM vs. training set size
  SavePlotBegin(dir=graphics.dir, file.name='sample.mfm')
  plot(x=x, y=y.mfm, ylim=c(0, 1), xlab='Training set size', 
       ylab='Mean F-Measure', type='l')
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name='sample.acc')
  plot(x=x, y=y.acc, ylim=c(0, 1), xlab='Training set size', 
       ylab='Accuracy', type='l')
  SavePlotEnd()
}

##############################
## AL and RS on a potentially infinite activity stream

## The Online.* functions return the probability of sampling the instance
### whose offline AL metric is offline.metric
Online.BMargin = function(offline.metric, gamma, ...) {
  #cat(' >>> gamma =', gamma, '\n')
  return( exp(-gamma*offline.metric) )
}

Online.FixedRate = function(f, ...) {
  #cat(' >>> f =', f, '\n')
  return( f )
}

Online.Always = function(...) {
  return( 2 ) # greater than any probability
}

Online.Unskewed = function(offline.metric, unskewed.threshold, ...) {
  p = 1 - offline.metric
  return( min(p, unskewed.threshold) )
}

## The Offline.* functions quantify the offline AL confusion 
### they operate on a vector of probabilities corresponding to the model estimation
Offline.Confidence = function(p) {
  return( max(p) )
}

Offline.Margin = function(p) {
  p = sort(p, decreasing=T)
  return( p[1] - p[2] )
}

Offline.Random = function(p) {
  return( 0.5 ) 
  # all instances receive equal confusion metrics
  ## ties are broken randomly
}

## Weighting functions that attribute different weights to instances
### according to their position in the recall vector
Batch.Confidence.Factor.end = function(position, recall.size, max.factor=1) {
  middle.point = (recall.size + 1) / 2
  v = (tanh(position - middle.point)/2) + 0.5
  return( v / max.factor )
}

Batch.Confidence.Factor.begining = function(position, recall.size, max.factor=1) {
  middle.point = (recall.size + 1) / 2
  v = (tanh(middle.point - position)/2) + 0.5
  return( v / max.factor )
}

Batch.Confidence.Factor.middle = function(position, recall.size, max.factor=1) {
  middle.point = (recall.size + 1) / 2
  sigma = middle.point / 3
  v = dnorm(position, mean=middle.point, sd=sigma)
  return( v / max.factor )
}

Batch.Confidence.Factor.unif = function(position, recall.size, max.factor=1) {
  return( 1 )
}

GetNewActivitySubstream = function(subject.data, max.size) {
  if (nrow(subject.data) == 0) {
    stop('subject.data has 0 rows. Cannot generate activity stream.')
  }
  while (T) {
    activity.label = as.character(sample(x=unique(subject.data$activity), size=1))
    activity.i = which(subject.data$activity == activity.label)
    activity.data = subject.data[activity.i, ]
    compl.data = subject.data[-activity.i, ]
    
    n = nrow(activity.data)
    if (n > 0) {
      n.min = min(max.size, n)
      num.instances = sample(x=n.min, size=1)
      i = sample(x=nrow(activity.data), size=num.instances)
      
      activity.substream = activity.data[i, ]
      complementary.pool = rbind(compl.data, activity.data[-i, ])
      
      return( list(activity.substream=activity.substream,
                   complementary.pool=complementary.pool) )
    }
  }
}

GetNewActivitySubstream.min = function(subject.data, min.size, max.size) {
  if (nrow(subject.data) == 0) {
    stop('subject.data has 0 rows. Cannot generate activity stream.')
  }
  while (T) {
    activity.label = as.character(sample(x=unique(subject.data$activity), size=1))
    activity.i = which(subject.data$activity == activity.label)
    activity.data = subject.data[activity.i, ]
    compl.data = subject.data[-activity.i, ]
    
    n = nrow(activity.data)
    if (n < min.size) {
      no.instances = T
      num.insts = c()
      for (act.char in as.character(unique(subject.data$activity))) {
        activity.k = which(subject.data$activity == act.char)
        activity.k.data = subject.data[activity.k, ]
        if (nrow(activity.k.data) >= min.size) {
          no.instances = F
          num.insts = c(num.insts, nrow(activity.k.data))
          break
        }
      }
      if (no.instances) {
        cat('  instances: '); print(num.insts)
        stop(' !! Not enough instances in pool to sample !!\n')
      } else {
        next # while loop
      }
    }
    if (n > 0) {
      n.min = min(max.size, n) - min.size + 1
      num.instances = sample(x=n.min, size=1) + min.size - 1
      i = sample(x=nrow(activity.data), size=num.instances)
      
      activity.substream = activity.data[i, ]
      complementary.pool = rbind(compl.data, activity.data[-i, ])
      
      return( list(activity.substream=activity.substream,
                   complementary.pool=complementary.pool) )
    }
  }
}

TurnPoolIntoStream = function(subject.data, max.episode.length) {
  activity.stream = subject.data[0, ]
  while (nrow(subject.data) > 0) {
    l = GetNewActivitySubstream(subject.data=subject.data, 
                                max.size=max.episode.length)
    activity.stream = rbind(activity.stream, l$activity.substream)
    subject.data = l$complementary.pool
  }
  return( activity.stream )
}

## with segmentation
SimululateSegmentationSamplingForSubject = function(subjects.features,
                                                    subject.i, Nb, classifier.name,
                                                    weka.control, min.train.size, 
                                                    class.labels, num.folds,
                                                    max.episode.length,
                                                    online.confusion.metric, 
                                                    offline.confusion.metric, 
                                                    kernel.width, threshold, ...) {
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  GetKernel = function(L) {
    m = matrix(0, 2*L, 2*L)
    for (i in seq_len(nrow(m))) {
      for (j in seq_len(ncol(m))) {
        if (((i <= L) & (j > L)) |
              ((j <= L) & (i > L))) {
          m[i, j] = 1 / (2*L^2)
        }
      }
    }
    return( m )
  }
  kernel.m = GetKernel(L=kernel.width)
  
  GetDist = function(row.1, row.2) {
    sqrt(sum((row.1-row.2)^2))
    #sum(row.1-row.2)
  }
  
  subject.data = subjects.features[[subject.i]]$data
  initial.pool.size = nrow(subject.data)
  
  mean.xs = c()
  mean.ys = c()
  mean.zs = c()
  
  var.xs = c()
  var.ys = c()
  var.zs = c()
  
  cor.xys = c()
  cor.yzs = c()
  cor.zxs = c()
  
  subject.is = seq_along(subjects.features)
  for (s.i in subject.is[-which(subject.is == subject.i)]) {
    s.data = subjects.features[[s.i]]$data
    
    mean.xs = c(mean.xs, s.data$mean.x)
    mean.ys = c(mean.ys, s.data$mean.y)
    mean.zs = c(mean.zs, s.data$mean.z)
    
    var.xs = c(var.xs, s.data$var.x)
    var.ys = c(var.ys, s.data$var.y)
    var.zs = c(var.zs, s.data$var.z)
    
    cor.xys = c(cor.xys, s.data$cor.xy)
    cor.yzs = c(cor.yzs, s.data$cor.yz)
    cor.zxs = c(cor.zxs, s.data$cor.zx)
  }
  
  quant.min = 0.01
  quant.max = 0.99
  
  mean.x.min = quantile(x=mean.xs, probs=quant.min)
  mean.x.max = quantile(x=mean.xs, probs=quant.max)
  mean.x.den = mean.x.max - mean.x.min
  
  mean.y.min = quantile(x=mean.ys, probs=quant.min)
  mean.y.max = quantile(x=mean.ys, probs=quant.max)
  mean.y.den = mean.y.max - mean.y.min
  
  mean.z.min = quantile(x=mean.zs, probs=quant.min)
  mean.z.max = quantile(x=mean.zs, probs=quant.max)
  mean.z.den = mean.z.max - mean.z.min
  
  var.x.min = quantile(x=var.xs, probs=quant.min)
  var.x.max = quantile(x=var.xs, probs=quant.max)
  var.x.den = var.x.max - var.x.min
  
  var.y.min = quantile(x=var.ys, probs=quant.min)
  var.y.max = quantile(x=var.ys, probs=quant.max)
  var.y.den = var.y.max - var.y.min
  
  var.z.min = quantile(x=var.zs, probs=quant.min)
  var.z.max = quantile(x=var.zs, probs=quant.max)
  var.z.den = var.z.max - var.z.min
  
  cor.xy.min = quantile(x=cor.xys, probs=quant.min)
  cor.xy.max = quantile(x=cor.xys, probs=quant.max)
  cor.xy.den = cor.xy.max - cor.xy.min
  
  cor.yz.min = quantile(x=cor.yzs, probs=quant.min)
  cor.yz.max = quantile(x=cor.yzs, probs=quant.max)
  cor.yz.den = cor.yz.max - cor.yz.min
  
  cor.zx.min = quantile(x=cor.zxs, probs=quant.min)
  cor.zx.max = quantile(x=cor.zxs, probs=quant.max)
  cor.zx.den = cor.zx.max - cor.zx.min
  
  recall.buffer = list()
  recall.buffer$w = subject.data[0, ]
  recall.buffer$timestamp = c()
  recall.buffer$predicted = list()
  recall.buffer$metric = c()
  recall.buffer$w.norm = subject.data[0, 1:9]
  recall.buffer$dists = c()
  recall.buffer$seg.begin = 1
  
  activity.stream = subject.data[0, ]
  
  ann.set.clean = subject.data[0, ]
  ann.set.noisy = subject.data[0, ]
  
  zeror.name = 'weka.classifiers.rules.ZeroR'
  zeror = make_Weka_classifier(name=zeror.name, class=class.labels)
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  model = zeror(formula=activity~., data=ann.set.noisy)
  
  question.i = c()  
  cms.noisy = list()
  seg.lengths = c()
  cms.seg = list()
  peak.heights = c()
  min.conf.correct = c()
  
  timestamp = 1
  while (nrow(ann.set.noisy) < Nb) {    
    cat('t =', timestamp, '... ')
    
    if (nrow(activity.stream) == 0) {
      cat('  generating new activity stream... ')
      l = GetNewActivitySubstream.min(subject.data=subject.data, 
                                      min.size=kernel.width,
                                      max.size=max.episode.length)
      activity.stream = l$activity.substream
      cat('Activity_', activity.stream$activity[1], ' ', sep='')
      cat(nrow(activity.stream), 'activity windows \n')
      subject.data = l$complementary.pool
    }
    
    w.act.stream = activity.stream[1, ]
    activity.stream = activity.stream[-1, ]
    #cat('  window pulled from activity stream\n')
    
    recall.buffer$w = rbind(recall.buffer$w, w.act.stream)
    #cat('   added to recall buffer\n')
    recall.buffer$timestamp = c(recall.buffer$timestamp, timestamp)
    #cat('   timestamp\n')
    predicted = predict(model, newdata=w.act.stream, type='probability')
    recall.buffer$predicted[[1+length(recall.buffer$predicted)]] = predicted
    #cat('   predicted\n')
    metric = offline.confusion.metric(predicted)
    recall.buffer$metric = c(recall.buffer$metric, metric)
    #cat('   metric\n')
    
    w.norm.new = subject.data[0, 1:9]
    w.norm.new[1, ]$mean.x = (mean.x.max - w.act.stream$mean.x) / mean.x.den
    w.norm.new$mean.y = (mean.y.max - w.act.stream$mean.y) / mean.y.den
    w.norm.new$mean.z = (mean.z.max - w.act.stream$mean.z) / mean.z.den
    w.norm.new$var.x = (var.x.max - w.act.stream$var.x) / var.x.den
    w.norm.new$var.y = (var.y.max - w.act.stream$var.y) / var.y.den
    w.norm.new$var.z = (var.z.max - w.act.stream$var.z) / var.z.den
    w.norm.new$cor.xy = (cor.xy.max - w.act.stream$cor.xy) / cor.xy.den
    w.norm.new$cor.yz = (cor.yz.max - w.act.stream$cor.yz) / cor.yz.den
    w.norm.new$cor.zx = (cor.zx.max - w.act.stream$cor.zx) / cor.zx.den
    recall.buffer$w.norm = rbind(recall.buffer$w.norm, w.norm.new)   
    #cat('   normalized\n')
        
    #cat('  finished adding new window to recall.buffer\n')
    
    ## detect change
    dist.m = matrix(data=0, nrow=2*kernel.width, ncol=2*kernel.width)
    rb.len = nrow(recall.buffer$w.norm)
    if (rb.len >= 2*kernel.width) {      
      i.start = rb.len - 2*kernel.width + 1
      i.end = rb.len
      i.seq = i.start: i.end
      #print(i.seq)
      for (i in i.seq) {
        for (k in i.seq) {
          row.1 = recall.buffer$w.norm[i, ]
          row.2 = recall.buffer$w.norm[k, ]
          d.i = 1 + i - i.start
          d.k = 1 + k - i.start
          #cat('  d.i=', d.i, ' d.k=', d.k, 
          #    ' (i.start=', i.start, ', i.end=', i.end, ')\n', sep='')
          dist.m[d.i, d.k] = GetDist(row.1=row.1, row.2=row.2)
        }
      }
    }    
    recall.buffer$dists = c(recall.buffer$dists, sum(dist.m * kernel.m))
    #cat('  computed aggregate distance\n')
    
    activity.changed = F
    d.prev = 0
    if (rb.len >= 3) {
      d.last = recall.buffer$dists[rb.len]
      d.prev = recall.buffer$dists[rb.len-1]
      d.prev.2 = recall.buffer$dists[rb.len-2]
      if ((d.prev >= d.prev.2) & (d.prev >= d.last) & (d.prev > threshold)) {
        activity.changed = T
        cat('  detected activity change at t =', timestamp - kernel.width, 
            '(now t =', timestamp, ')\n')
      }
    }
    
    if (activity.changed) {
      #cat('length(recall.buffer$metric) =', length(recall.buffer$metric), '\n')
      cat('nrow(recall.buffer$w) =', nrow(recall.buffer$w), '\n')
      cat('length(recall.buffer$dists) =', length(recall.buffer$dists), '\n')
      #cat('length(recall.buffer$predicted) =', length(recall.buffer$predicted), '\n')
      #cat('nrow(recall.buffer$w.norm) =', nrow(recall.buffer$w.norm), '\n')
      #print(recall.buffer$metric)
      
      i.begin = recall.buffer$seg.begin
      i.end = rb.len - kernel.width - 1
      if (i.begin > i.end) {
        stop('i.begin > i.end')
      }
      ann.set.i = i.begin: i.end
      
      avg.conf = recall.buffer$metric[i.begin]
      min.conf = recall.buffer$metric[i.begin]
      min.conf.i = i.begin
      #cat('    rb.len =', rb.len, '\n')
      #cat('    length(metric) =', length(recall.buffer$metric))
      for (i in (i.begin+1): i.end) {
        if (recall.buffer$metric[i] < min.conf) {
          min.conf = recall.buffer$metric[i]
          min.conf.i = i
        }
        avg.conf = avg.conf + (recall.buffer$metric[i])
      }
      avg.conf = avg.conf / length(ann.set.i)
      
      ## AVERAGE CONFIDENCE
      min.conf = avg.conf
      
      min.conf.act = recall.buffer$w$activity[min.conf.i]
      seg.mode.act = Mode(recall.buffer$w$activity[ann.set.i])   
      min.conf.correct = c(min.conf.correct, min.conf.act == seg.mode.act)
      
      cat('  min.conf =', min.conf, '\n')
      ask.prob = online.confusion.metric(offline.metric=min.conf, ...)
      
      r = runif(1)
      ask = r < ask.prob
      if (ask) {
        cat('   asking...\n')
      } else {
        cat('   not asking...\n')
      }
      
      recycle.ds = subject.data[0, ]
      
      if (ask) {
        question.i = c(question.i, timestamp)
        peak.heights = c(peak.heights, d.prev)
        cat('   annotating instances from', i.begin, 'to', i.end, '\n')
        
        ann.subset.clean = recall.buffer$w[ann.set.i, ]
        ann.set.clean = rbind(ann.set.clean, ann.subset.clean)
        cat('    - clean set\n')
        
        act.mode = Mode(recall.buffer$w$activity[ann.set.i])
        ann.subset.noisy = recall.buffer$w[ann.set.i, ]
        ann.subset.noisy$activity = act.mode
        ann.set.noisy = rbind(ann.set.noisy, ann.subset.noisy)
        cat('    - noisy set\n')
        
        cat('  nrow(ann.subset.clean) =', nrow(ann.subset.clean), '\n')
        cat('  nrow(ann.subset.noisy) =', nrow(ann.subset.noisy), '\n')
        cm.seg = ConfusionMatrix(actual=ann.subset.clean$activity,
                                 predicted=ann.subset.noisy$activity, 
                                 class.labels=class.labels)
        cat('    - segmentation cms\n')
        cms.seg[[1+length(cms.seg)]] = cm.seg
    
        seg.lengths = c(seg.lengths, nrow(ann.subset.noisy) )
        
        cm.noisy = CrossValidateWithNoisyTrainingSet(classifier.name=classifier.name, 
                                                     weka.control=weka.control, 
                                                     data.subsample.noisy=ann.set.noisy, 
                                                     data.subsample.clean=ann.set.clean, 
                                                     data.complementary=subject.data, 
                                                     class.labels=class.labels, 
                                                     num.folds=num.folds)
        cms.noisy[[1+length(cms.noisy)]] = cm.noisy
        
        # update AL model
        if (nrow(ann.set.noisy) >= min.train.size) {
          model = classifier(formula=activity~., data=ann.set.noisy)
        } else {
          model = zeror(formula=activity~., data=ann.set.noisy)
        }       
      } else {
        recycle.ds = recall.buffer$w[ann.set.i, ]
      } # if(ask)
      
      ## remove previous segment from recall buffer
      #cat('discarding instances ')
      #print(ann.set.i)
      #recall.buffer$w = recall.buffer$w[-ann.set.i, ]
      #recall.buffer$w.norm = recall.buffer$w.norm[-ann.set.i, ]
      #recall.buffer$timestamp = recall.buffer$timestamp[-ann.set.i]
      #recall.buffer$predicted = recall.buffer$predicted[-ann.set.i]
      #recall.buffer$metric = recall.buffer$metric[-ann.set.i]
      #for (i in seq_len(nrow(recall.buffer$w))) {          
      #  newdata = recall.buffer$w[i, ]
      #  predicted = predict(model, newdata=newdata, type='probability')
      #  recall.buffer$predicted[[i]] = predicted
      #  #print(predicted)
      #  recall.buffer$metric[i] = offline.confusion.metric(predicted)
      #  #cat('    -->', recall.buffer$metric[i], '\n')
      #}
      ## recycle un-annotated instances
      #subject.data = rbind(subject.data, recycle.ds)
      
      cat(' discarding instances. ');
      keep.end.i = rb.len
      keep.begin.i = rb.len - 2*kernel.width + 2 
      keep.i = keep.begin.i: keep.end.i
      cat('keeping everything from', keep.begin.i, 'to', keep.end.i, '\n')
      # The buffer is NOW short of one activity point to calculate the distances.
      # The next activity point in the stream will complete the buffer to
      #  its minimum size
      
      recall.buffer$seg.begin = kernel.width - 1
      recall.buffer$w = recall.buffer$w[keep.i, ]
      recall.buffer$w.norm = recall.buffer$w.norm[keep.i, ]
      recall.buffer$timestamp = recall.buffer$timestamp[keep.i]
      recall.buffer$dists = recall.buffer$dists[keep.i]
      
      recall.buffer$predicted = recall.buffer$predicted[keep.i]
      recall.buffer$metric = recall.buffer$metric[keep.i]
      for (i in seq_along(nrow(recall.buffer$w))) {
        newdata = recall.buffer$w[i, ]
        predicted = predict(model, newdata=newdata, type='probability')
        recall.buffer$predicted[[i]] = predicted
        recall.buffer$metric[i] = offline.confusion.metric(predicted)        
      }
      # recycle un-annotated instances
      subject.data = rbind(subject.data, recycle.ds)
      
      ## audit
      training.set.size = nrow(ann.set.noisy)
      recall.buffer.size = nrow(recall.buffer$w)
      pool.size = nrow(subject.data)
      activity.stream.size = nrow(activity.stream)
      cat('  training set size    =', training.set.size, '\n')
      cat('  recall.buffer size   =', recall.buffer.size, '\n')
      cat('  pool size            =', pool.size, '\n')
      cat('  activity stream size =', activity.stream.size, '\n')
      sum.sizes = training.set.size + recall.buffer.size + pool.size + activity.stream.size
      cat(' sum =', sum.sizes, '\n')
      cat(' initial =', initial.pool.size, '\n')
      #if (sum.sizes != initial.pool.size) {
      #  stop('some pool instances not used or reused')
      #}
    }
    
    timestamp = 1 + timestamp
  }  
  
  return( list(cms=cms.noisy, cms.seg=cms.seg, question.i=question.i, 
               x.max=question.i[length(question.i)],
               seg.lengths=seg.lengths, peak.heights=peak.heights,
               min.conf.correct=min.conf.correct) )
}

SimululateSegmentationSamplingForSubject.mode.conf = function(subjects.features,
                                                              subject.i, Nb, 
                                                              classifier.name,
                                                              weka.control, 
                                                              min.train.size, 
                                                              class.labels, num.folds,
                                                              max.episode.length,
                                                              online.confusion.metric, 
                                                              offline.confusion.metric, 
                                                              kernel.width, 
                                                              threshold, ...) {
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  GetKernel = function(L) {
    m = matrix(0, 2*L, 2*L)
    for (i in seq_len(nrow(m))) {
      for (j in seq_len(ncol(m))) {
        if (((i <= L) & (j > L)) |
              ((j <= L) & (i > L))) {
          m[i, j] = 1 / (2*L^2)
        }
      }
    }
    return( m )
  }
  kernel.m = GetKernel(L=kernel.width)
  
  GetDist = function(row.1, row.2) {
    sqrt(sum((row.1-row.2)^2))
    #sum(row.1-row.2)
  }
  
  subject.data = subjects.features[[subject.i]]$data
  initial.pool.size = nrow(subject.data)
  
  mean.xs = c()
  mean.ys = c()
  mean.zs = c()
  
  var.xs = c()
  var.ys = c()
  var.zs = c()
  
  cor.xys = c()
  cor.yzs = c()
  cor.zxs = c()
  
  subject.is = seq_along(subjects.features)
  for (s.i in subject.is[-which(subject.is == subject.i)]) {
    s.data = subjects.features[[s.i]]$data
    
    mean.xs = c(mean.xs, s.data$mean.x)
    mean.ys = c(mean.ys, s.data$mean.y)
    mean.zs = c(mean.zs, s.data$mean.z)
    
    var.xs = c(var.xs, s.data$var.x)
    var.ys = c(var.ys, s.data$var.y)
    var.zs = c(var.zs, s.data$var.z)
    
    cor.xys = c(cor.xys, s.data$cor.xy)
    cor.yzs = c(cor.yzs, s.data$cor.yz)
    cor.zxs = c(cor.zxs, s.data$cor.zx)
  }
  
  quant.min = 0.01
  quant.max = 0.99
  
  mean.x.min = quantile(x=mean.xs, probs=quant.min)
  mean.x.max = quantile(x=mean.xs, probs=quant.max)
  mean.x.den = mean.x.max - mean.x.min
  
  mean.y.min = quantile(x=mean.ys, probs=quant.min)
  mean.y.max = quantile(x=mean.ys, probs=quant.max)
  mean.y.den = mean.y.max - mean.y.min
  
  mean.z.min = quantile(x=mean.zs, probs=quant.min)
  mean.z.max = quantile(x=mean.zs, probs=quant.max)
  mean.z.den = mean.z.max - mean.z.min
  
  var.x.min = quantile(x=var.xs, probs=quant.min)
  var.x.max = quantile(x=var.xs, probs=quant.max)
  var.x.den = var.x.max - var.x.min
  
  var.y.min = quantile(x=var.ys, probs=quant.min)
  var.y.max = quantile(x=var.ys, probs=quant.max)
  var.y.den = var.y.max - var.y.min
  
  var.z.min = quantile(x=var.zs, probs=quant.min)
  var.z.max = quantile(x=var.zs, probs=quant.max)
  var.z.den = var.z.max - var.z.min
  
  cor.xy.min = quantile(x=cor.xys, probs=quant.min)
  cor.xy.max = quantile(x=cor.xys, probs=quant.max)
  cor.xy.den = cor.xy.max - cor.xy.min
  
  cor.yz.min = quantile(x=cor.yzs, probs=quant.min)
  cor.yz.max = quantile(x=cor.yzs, probs=quant.max)
  cor.yz.den = cor.yz.max - cor.yz.min
  
  cor.zx.min = quantile(x=cor.zxs, probs=quant.min)
  cor.zx.max = quantile(x=cor.zxs, probs=quant.max)
  cor.zx.den = cor.zx.max - cor.zx.min
  
  recall.buffer = list()
  recall.buffer$w = subject.data[0, ]
  recall.buffer$timestamp = c()
  recall.buffer$predicted = list()
  recall.buffer$metric = c()
  recall.buffer$w.norm = subject.data[0, 1:9]
  recall.buffer$dists = c()
  recall.buffer$seg.begin = 1
  
  activity.stream = subject.data[0, ]
  
  ann.set.clean = subject.data[0, ]
  ann.set.noisy = subject.data[0, ]
  
  zeror.name = 'weka.classifiers.rules.ZeroR'
  zeror = make_Weka_classifier(name=zeror.name, class=class.labels)
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  model = zeror(formula=activity~., data=ann.set.noisy)
  
  question.i = c()  
  cms.noisy = list()
  seg.lengths = c()
  cms.seg = list()
  peak.heights = c()
  min.conf.correct = c()
  
  timestamp = 1
  while (nrow(ann.set.noisy) < Nb) {    
    cat('t =', timestamp, '... ')
    
    if (nrow(activity.stream) == 0) {
      cat('  generating new activity stream... ')
      l = GetNewActivitySubstream.min(subject.data=subject.data, 
                                      min.size=kernel.width,
                                      max.size=max.episode.length)
      activity.stream = l$activity.substream
      cat('Activity_', activity.stream$activity[1], ' ', sep='')
      cat(nrow(activity.stream), 'activity windows \n')
      subject.data = l$complementary.pool
    }
    
    w.act.stream = activity.stream[1, ]
    activity.stream = activity.stream[-1, ]
    #cat('  window pulled from activity stream\n')
    
    recall.buffer$w = rbind(recall.buffer$w, w.act.stream)
    #cat('   added to recall buffer\n')
    recall.buffer$timestamp = c(recall.buffer$timestamp, timestamp)
    #cat('   timestamp\n')
    predicted = predict(model, newdata=w.act.stream, type='probability')
    recall.buffer$predicted[[1+length(recall.buffer$predicted)]] = predicted
    #cat('   predicted\n')
    metric = offline.confusion.metric(predicted)
    recall.buffer$metric = c(recall.buffer$metric, metric)
    #cat('   metric\n')
    
    w.norm.new = subject.data[0, 1:9]
    w.norm.new[1, ]$mean.x = (mean.x.max - w.act.stream$mean.x) / mean.x.den
    w.norm.new$mean.y = (mean.y.max - w.act.stream$mean.y) / mean.y.den
    w.norm.new$mean.z = (mean.z.max - w.act.stream$mean.z) / mean.z.den
    w.norm.new$var.x = (var.x.max - w.act.stream$var.x) / var.x.den
    w.norm.new$var.y = (var.y.max - w.act.stream$var.y) / var.y.den
    w.norm.new$var.z = (var.z.max - w.act.stream$var.z) / var.z.den
    w.norm.new$cor.xy = (cor.xy.max - w.act.stream$cor.xy) / cor.xy.den
    w.norm.new$cor.yz = (cor.yz.max - w.act.stream$cor.yz) / cor.yz.den
    w.norm.new$cor.zx = (cor.zx.max - w.act.stream$cor.zx) / cor.zx.den
    recall.buffer$w.norm = rbind(recall.buffer$w.norm, w.norm.new)   
    #cat('   normalized\n')
    
    #cat('  finished adding new window to recall.buffer\n')
    
    ## detect change
    dist.m = matrix(data=0, nrow=2*kernel.width, ncol=2*kernel.width)
    rb.len = nrow(recall.buffer$w.norm)
    if (rb.len >= 2*kernel.width) {      
      i.start = rb.len - 2*kernel.width + 1
      i.end = rb.len
      i.seq = i.start: i.end
      #print(i.seq)
      for (i in i.seq) {
        for (k in i.seq) {
          row.1 = recall.buffer$w.norm[i, ]
          row.2 = recall.buffer$w.norm[k, ]
          d.i = 1 + i - i.start
          d.k = 1 + k - i.start
          #cat('  d.i=', d.i, ' d.k=', d.k, 
          #    ' (i.start=', i.start, ', i.end=', i.end, ')\n', sep='')
          dist.m[d.i, d.k] = GetDist(row.1=row.1, row.2=row.2)
        }
      }
    }    
    recall.buffer$dists = c(recall.buffer$dists, sum(dist.m * kernel.m))
    #cat('  computed aggregate distance\n')
    
    activity.changed = F
    d.prev = 0
    if (rb.len >= 3) {
      d.last = recall.buffer$dists[rb.len]
      d.prev = recall.buffer$dists[rb.len-1]
      d.prev.2 = recall.buffer$dists[rb.len-2]
      if ((d.prev >= d.prev.2) & (d.prev >= d.last) & (d.prev > threshold)) {
        activity.changed = T
        cat('  detected activity change at t =', timestamp - kernel.width, 
            '(now t =', timestamp, ')\n')
      }
    }
    
    if (activity.changed) {
      #cat('length(recall.buffer$metric) =', length(recall.buffer$metric), '\n')
      #cat('nrow(recall.buffer$w) =', nrow(recall.buffer$w), '\n')
      #cat('length(recall.buffer$dists) =', length(recall.buffer$dists), '\n')
      #cat('length(recall.buffer$predicted) =', length(recall.buffer$predicted), '\n')
      #cat('nrow(recall.buffer$w.norm) =', nrow(recall.buffer$w.norm), '\n')
      #print(recall.buffer$metric)
      
      i.begin = recall.buffer$seg.begin
      i.end = rb.len - kernel.width - 1
      if (i.begin > i.end) {
        stop('i.begin > i.end')
      }
      ann.set.i = i.begin: i.end
      
      avg.conf = 0
      min.conf = 0
      min.conf.i = i.begin
      #cat('    rb.len =', rb.len, '\n')
      #cat('    length(metric) =', length(recall.buffer$metric))
      i.hist = i.begin: i.end
      hist.predicted = recall.buffer$predicted[i.hist]
      class.table = vector(mode='integer', length=length(class.labels))
      for (i in i.hist) {
        pred.probs = recall.buffer$predicted[[i]]
        i.class = which(pred.probs == max(pred.probs))
        for (i.c in i.class) {
          class.table[i.c] = 1 + class.table[i.c]
        }
      }
      
      i.c.max = which(class.table == max(class.table))[1]
      #print(class.table)
      #cat('    going for', i.c.max, '\n')
      
      if (length(i.c.max) > 1) {
        stop('i.c.max is a vector')
      }
      
      avg.conf = 0
      for (i in i.hist) {
        conf = recall.buffer$predicted[[i]][i.c.max]
        avg.conf = avg.conf + conf
      }
      
      avg.conf = avg.conf / length(ann.set.i)
      
      ## AVERAGE CONFIDENCE
      min.conf = avg.conf
      
      min.conf.act = recall.buffer$w$activity[min.conf.i]
      seg.mode.act = Mode(recall.buffer$w$activity[ann.set.i])   
      min.conf.correct = c(min.conf.correct, min.conf.act == seg.mode.act)
      
      cat('  min.conf =', min.conf, '\n')
      ask.prob = online.confusion.metric(offline.metric=min.conf, ...)
      
      r = runif(1)
      ask = r < ask.prob
      if (ask) {
        cat('   asking...\n')
      } else {
        cat('   not asking...\n')
      }
      
      recycle.ds = subject.data[0, ]
      
      if (ask) {
        question.i = c(question.i, timestamp)
        peak.heights = c(peak.heights, d.prev)
        cat('   annotating instances from', i.begin, 'to', i.end, '\n')
        
        ann.subset.clean = recall.buffer$w[ann.set.i, ]
        ann.set.clean = rbind(ann.set.clean, ann.subset.clean)
        #cat('    - clean set\n')
        
        act.mode = Mode(recall.buffer$w$activity[ann.set.i])
        ann.subset.noisy = recall.buffer$w[ann.set.i, ]
        ann.subset.noisy$activity = act.mode
        ann.set.noisy = rbind(ann.set.noisy, ann.subset.noisy)
        #cat('    - noisy set\n')
        
        #cat('  nrow(ann.subset.clean) =', nrow(ann.subset.clean), '\n')
        #cat('  nrow(ann.subset.noisy) =', nrow(ann.subset.noisy), '\n')
        cm.seg = ConfusionMatrix(actual=ann.subset.clean$activity,
                                 predicted=ann.subset.noisy$activity, 
                                 class.labels=class.labels)
        #cat('    - segmentation cms\n')
        cms.seg[[1+length(cms.seg)]] = cm.seg
        
        seg.lengths = c(seg.lengths, nrow(ann.subset.noisy) )
        
        cm.noisy = CrossValidateWithNoisyTrainingSet(classifier.name=classifier.name, 
                                                     weka.control=weka.control, 
                                                     data.subsample.noisy=ann.set.noisy, 
                                                     data.subsample.clean=ann.set.clean, 
                                                     data.complementary=subject.data, 
                                                     class.labels=class.labels, 
                                                     num.folds=num.folds)
        cms.noisy[[1+length(cms.noisy)]] = cm.noisy
        
        # update AL model
        if (nrow(ann.set.noisy) >= min.train.size) {
          model = classifier(formula=activity~., data=ann.set.noisy)
        } else {
          model = zeror(formula=activity~., data=ann.set.noisy)
        }       
      } else {
        recycle.ds = recall.buffer$w[ann.set.i, ]
      } # if(ask)
      
      ## remove previous segment from recall buffer
      #cat('discarding instances ')
      #print(ann.set.i)
      #recall.buffer$w = recall.buffer$w[-ann.set.i, ]
      #recall.buffer$w.norm = recall.buffer$w.norm[-ann.set.i, ]
      #recall.buffer$timestamp = recall.buffer$timestamp[-ann.set.i]
      #recall.buffer$predicted = recall.buffer$predicted[-ann.set.i]
      #recall.buffer$metric = recall.buffer$metric[-ann.set.i]
      #for (i in seq_len(nrow(recall.buffer$w))) {          
      #  newdata = recall.buffer$w[i, ]
      #  predicted = predict(model, newdata=newdata, type='probability')
      #  recall.buffer$predicted[[i]] = predicted
      #  #print(predicted)
      #  recall.buffer$metric[i] = offline.confusion.metric(predicted)
      #  #cat('    -->', recall.buffer$metric[i], '\n')
      #}
      ## recycle un-annotated instances
      #subject.data = rbind(subject.data, recycle.ds)
      
      cat(' discarding instances. ');
      keep.end.i = rb.len
      keep.begin.i = rb.len - 2*kernel.width + 2 
      keep.i = keep.begin.i: keep.end.i
      cat('keeping everything from', keep.begin.i, 'to', keep.end.i, '\n')
      # The buffer is NOW short of one activity point to calculate the distances.
      # The next activity point in the stream will complete the buffer to
      #  its minimum size
      
      recall.buffer$seg.begin = kernel.width - 1
      recall.buffer$w = recall.buffer$w[keep.i, ]
      recall.buffer$w.norm = recall.buffer$w.norm[keep.i, ]
      recall.buffer$timestamp = recall.buffer$timestamp[keep.i]
      recall.buffer$dists = recall.buffer$dists[keep.i]
      
      recall.buffer$predicted = recall.buffer$predicted[keep.i]
      recall.buffer$metric = recall.buffer$metric[keep.i]
      for (i in seq_along(nrow(recall.buffer$w))) {
        newdata = recall.buffer$w[i, ]
        predicted = predict(model, newdata=newdata, type='probability')
        recall.buffer$predicted[[i]] = predicted
        recall.buffer$metric[i] = offline.confusion.metric(predicted)        
      }
      # recycle un-annotated instances
      subject.data = rbind(subject.data, recycle.ds)
      
      ## audit
      training.set.size = nrow(ann.set.noisy)
      recall.buffer.size = nrow(recall.buffer$w)
      pool.size = nrow(subject.data)
      activity.stream.size = nrow(activity.stream)
      cat(' >>> training set size    =', training.set.size, '\n')
      #cat('  recall.buffer size   =', recall.buffer.size, '\n')
      #cat('  pool size            =', pool.size, '\n')
      #cat('  activity stream size =', activity.stream.size, '\n')
      sum.sizes = training.set.size + recall.buffer.size + pool.size + activity.stream.size
      #cat(' sum =', sum.sizes, '\n')
      #cat(' initial =', initial.pool.size, '\n')
      #if (sum.sizes != initial.pool.size) {
      #  stop('some pool instances not used or reused')
      #}
    }
    
    timestamp = 1 + timestamp
  }  
  
  return( list(cms=cms.noisy, cms.seg=cms.seg, question.i=question.i, 
               x.max=question.i[length(question.i)],
               seg.lengths=seg.lengths, peak.heights=peak.heights,
               min.conf.correct=min.conf.correct) )
}

SimululateSegmentationSamplingForSubject.mode.conf.cls = function(subjects.features,
                                                                  subject.i, Nb, 
                                                                  classifier.name,
                                                                  weka.control, 
                                                                  min.train.size, 
                                                                  class.labels, num.folds,
                                                                  max.episode.length,
                                                                  online.confusion.metric, 
                                                                  offline.confusion.metric, 
                                                                  kernel.width, 
                                                                  threshold, ...) {
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  GetKernel = function(L) {
    m = matrix(0, 2*L, 2*L)
    for (i in seq_len(nrow(m))) {
      for (j in seq_len(ncol(m))) {
        if (((i <= L) & (j > L)) |
              ((j <= L) & (i > L))) {
          m[i, j] = 1 / (2*L^2)
        }
      }
    }
    return( m )
  }
  kernel.m = GetKernel(L=kernel.width)
  
  GetDist = function(row.1, row.2) {
    sqrt(sum((row.1-row.2)^2))
    #sum(row.1-row.2)
  }
  
  subject.data = subjects.features[[subject.i]]$data
  initial.pool.size = nrow(subject.data)
  
  mean.xs = c()
  mean.ys = c()
  mean.zs = c()
  
  var.xs = c()
  var.ys = c()
  var.zs = c()
  
  cor.xys = c()
  cor.yzs = c()
  cor.zxs = c()
  
  subject.is = seq_along(subjects.features)
  for (s.i in subject.is[-which(subject.is == subject.i)]) {
    s.data = subjects.features[[s.i]]$data
    
    mean.xs = c(mean.xs, s.data$mean.x)
    mean.ys = c(mean.ys, s.data$mean.y)
    mean.zs = c(mean.zs, s.data$mean.z)
    
    var.xs = c(var.xs, s.data$var.x)
    var.ys = c(var.ys, s.data$var.y)
    var.zs = c(var.zs, s.data$var.z)
    
    cor.xys = c(cor.xys, s.data$cor.xy)
    cor.yzs = c(cor.yzs, s.data$cor.yz)
    cor.zxs = c(cor.zxs, s.data$cor.zx)
  }
  
  quant.min = 0.01
  quant.max = 0.99
  
  mean.x.min = quantile(x=mean.xs, probs=quant.min)
  mean.x.max = quantile(x=mean.xs, probs=quant.max)
  mean.x.den = mean.x.max - mean.x.min
  
  mean.y.min = quantile(x=mean.ys, probs=quant.min)
  mean.y.max = quantile(x=mean.ys, probs=quant.max)
  mean.y.den = mean.y.max - mean.y.min
  
  mean.z.min = quantile(x=mean.zs, probs=quant.min)
  mean.z.max = quantile(x=mean.zs, probs=quant.max)
  mean.z.den = mean.z.max - mean.z.min
  
  var.x.min = quantile(x=var.xs, probs=quant.min)
  var.x.max = quantile(x=var.xs, probs=quant.max)
  var.x.den = var.x.max - var.x.min
  
  var.y.min = quantile(x=var.ys, probs=quant.min)
  var.y.max = quantile(x=var.ys, probs=quant.max)
  var.y.den = var.y.max - var.y.min
  
  var.z.min = quantile(x=var.zs, probs=quant.min)
  var.z.max = quantile(x=var.zs, probs=quant.max)
  var.z.den = var.z.max - var.z.min
  
  cor.xy.min = quantile(x=cor.xys, probs=quant.min)
  cor.xy.max = quantile(x=cor.xys, probs=quant.max)
  cor.xy.den = cor.xy.max - cor.xy.min
  
  cor.yz.min = quantile(x=cor.yzs, probs=quant.min)
  cor.yz.max = quantile(x=cor.yzs, probs=quant.max)
  cor.yz.den = cor.yz.max - cor.yz.min
  
  cor.zx.min = quantile(x=cor.zxs, probs=quant.min)
  cor.zx.max = quantile(x=cor.zxs, probs=quant.max)
  cor.zx.den = cor.zx.max - cor.zx.min
  
  recall.buffer = list()
  recall.buffer$w = subject.data[0, ]
  recall.buffer$timestamp = c()
  recall.buffer$predicted = list()
  recall.buffer$metric = c()
  recall.buffer$w.norm = subject.data[0, 1:9]
  recall.buffer$dists = c()
  recall.buffer$seg.begin = 1
  
  activity.stream = subject.data[0, ]
  
  ann.set.clean = subject.data[0, ]
  ann.set.noisy = subject.data[0, ]
  
  zeror.name = 'weka.classifiers.rules.ZeroR'
  zeror = make_Weka_classifier(name=zeror.name, class=class.labels)
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  model = zeror(formula=activity~., data=ann.set.noisy)
  
  question.i = c()  
  cms.noisy = list()
  seg.lengths = c()
  cms.seg = list()
  peak.heights = c()
  min.conf.correct = c()
  
  timestamp = 1
  while (nrow(ann.set.noisy) < Nb) {    
    cat('t =', timestamp, '... ')
    
    if (nrow(activity.stream) == 0) {
      cat('  generating new activity stream... ')
      l = GetNewActivitySubstream.min(subject.data=subject.data, 
                                      min.size=kernel.width,
                                      max.size=max.episode.length)
      activity.stream = l$activity.substream
      cat('Activity_', activity.stream$activity[1], ' ', sep='')
      cat(nrow(activity.stream), 'activity windows \n')
      subject.data = l$complementary.pool
    }
    
    w.act.stream = activity.stream[1, ]
    activity.stream = activity.stream[-1, ]
    #cat('  window pulled from activity stream\n')
    
    recall.buffer$w = rbind(recall.buffer$w, w.act.stream)
    #cat('   added to recall buffer\n')
    recall.buffer$timestamp = c(recall.buffer$timestamp, timestamp)
    #cat('   timestamp\n')
    predicted = predict(model, newdata=w.act.stream, type='probability')
    recall.buffer$predicted[[1+length(recall.buffer$predicted)]] = predicted
    #cat('   predicted\n')
    metric = offline.confusion.metric(predicted)
    recall.buffer$metric = c(recall.buffer$metric, metric)
    #cat('   metric\n')
    
    w.norm.new = subject.data[0, 1:9]
    w.norm.new[1, ]$mean.x = (mean.x.max - w.act.stream$mean.x) / mean.x.den
    w.norm.new$mean.y = (mean.y.max - w.act.stream$mean.y) / mean.y.den
    w.norm.new$mean.z = (mean.z.max - w.act.stream$mean.z) / mean.z.den
    w.norm.new$var.x = (var.x.max - w.act.stream$var.x) / var.x.den
    w.norm.new$var.y = (var.y.max - w.act.stream$var.y) / var.y.den
    w.norm.new$var.z = (var.z.max - w.act.stream$var.z) / var.z.den
    w.norm.new$cor.xy = (cor.xy.max - w.act.stream$cor.xy) / cor.xy.den
    w.norm.new$cor.yz = (cor.yz.max - w.act.stream$cor.yz) / cor.yz.den
    w.norm.new$cor.zx = (cor.zx.max - w.act.stream$cor.zx) / cor.zx.den
    recall.buffer$w.norm = rbind(recall.buffer$w.norm, w.norm.new)   
    #cat('   normalized\n')
    
    #cat('  finished adding new window to recall.buffer\n')
    
    ## detect change
    dist.m = matrix(data=0, nrow=2*kernel.width, ncol=2*kernel.width)
    rb.len = nrow(recall.buffer$w.norm)
    if (rb.len >= 2*kernel.width) {      
      i.start = rb.len - 2*kernel.width + 1
      i.end = rb.len
      i.seq = i.start: i.end
      #print(i.seq)
      for (i in i.seq) {
        for (k in i.seq) {
          row.1 = recall.buffer$w.norm[i, ]
          row.2 = recall.buffer$w.norm[k, ]
          d.i = 1 + i - i.start
          d.k = 1 + k - i.start
          #cat('  d.i=', d.i, ' d.k=', d.k, 
          #    ' (i.start=', i.start, ', i.end=', i.end, ')\n', sep='')
          dist.m[d.i, d.k] = GetDist(row.1=row.1, row.2=row.2)
        }
      }
    }    
    recall.buffer$dists = c(recall.buffer$dists, sum(dist.m * kernel.m))
    #cat('  computed aggregate distance\n')
    
    activity.changed = F
    d.prev = 0
    if (rb.len >= 3) {
      d.last = recall.buffer$dists[rb.len]
      d.prev = recall.buffer$dists[rb.len-1]
      d.prev.2 = recall.buffer$dists[rb.len-2]
      if ((d.prev >= d.prev.2) & (d.prev >= d.last) & (d.prev > threshold)) {
        activity.changed = T
        cat('  detected activity change at t =', timestamp - kernel.width, 
            '(now t =', timestamp, ')\n')
      }
    }
    
    if (activity.changed) {
      #cat('length(recall.buffer$metric) =', length(recall.buffer$metric), '\n')
      #cat('nrow(recall.buffer$w) =', nrow(recall.buffer$w), '\n')
      #cat('length(recall.buffer$dists) =', length(recall.buffer$dists), '\n')
      #cat('length(recall.buffer$predicted) =', length(recall.buffer$predicted), '\n')
      #cat('nrow(recall.buffer$w.norm) =', nrow(recall.buffer$w.norm), '\n')
      #print(recall.buffer$metric)
      
      i.begin = recall.buffer$seg.begin
      i.end = rb.len - kernel.width - 1
      if (i.begin > i.end) {
        stop('i.begin > i.end')
      }
      ann.set.i = i.begin: i.end
      
      avg.conf = 0
      min.conf = 0
      min.conf.i = i.begin
      #cat('    rb.len =', rb.len, '\n')
      #cat('    length(metric) =', length(recall.buffer$metric))
      i.hist = i.begin: i.end
      hist.predicted = recall.buffer$predicted[i.hist]
      class.table = vector(mode='integer', length=length(class.labels))
      for (i in i.hist) {
        pred.probs = recall.buffer$predicted[[i]]
        i.class = which(pred.probs == max(pred.probs))
        for (i.c in i.class) {
          class.table[i.c] = 1 + class.table[i.c]
        }
      }
      
      i.c.max = which(class.table == max(class.table))[1]
      #print(class.table)
      #cat('    going for', i.c.max, '\n')
      
      if (length(i.c.max) > 1) {
        stop('i.c.max is a vector')
      }
      
      avg.conf = 0
      for (i in i.hist) {
        conf = recall.buffer$predicted[[i]][i.c.max]
        avg.conf = avg.conf + conf
      }
      
      avg.conf = avg.conf / length(ann.set.i)
      
      ## AVERAGE CONFIDENCE
      min.conf = avg.conf
      
      min.conf.act = recall.buffer$w$activity[min.conf.i]
      seg.mode.act = Mode(recall.buffer$w$activity[ann.set.i])   
      min.conf.correct = c(min.conf.correct, min.conf.act == seg.mode.act)
      
      cat('  min.conf =', min.conf, '\n')
      ask.prob = online.confusion.metric(offline.metric=min.conf, ...)
      
      r = runif(1)
      ask = r < ask.prob
      if (ask) {
        cat('   asking...\n')
      } else {
        cat('   not asking...\n')
      }
      
      recycle.ds = subject.data[0, ]
      
      if (ask) {
        question.i = c(question.i, timestamp)
        peak.heights = c(peak.heights, d.prev)
        cat('   annotating instances from', i.begin, 'to', i.end, '\n')
        
        ann.subset.clean = recall.buffer$w[ann.set.i, ]
        ann.set.clean = rbind(ann.set.clean, ann.subset.clean)
        #cat('    - clean set\n')
        
        act.mode = Mode(recall.buffer$w$activity[ann.set.i])
        ann.subset.noisy = recall.buffer$w[ann.set.i, ]
        ann.subset.noisy$activity = act.mode
        ann.set.noisy = rbind(ann.set.noisy, ann.subset.noisy)
        #cat('    - noisy set\n')
        
        #cat('  nrow(ann.subset.clean) =', nrow(ann.subset.clean), '\n')
        #cat('  nrow(ann.subset.noisy) =', nrow(ann.subset.noisy), '\n')
        cm.seg = ConfusionMatrix(actual=ann.subset.clean$activity,
                                 predicted=ann.subset.noisy$activity, 
                                 class.labels=class.labels)
        #cat('    - segmentation cms\n')
        cms.seg[[1+length(cms.seg)]] = cm.seg
        
        seg.lengths = c(seg.lengths, nrow(ann.subset.noisy) )
        
        cm.noisy = CrossValidateWithNoisyTrainingSet(classifier.name=classifier.name, 
                                                     weka.control=weka.control, 
                                                     data.subsample.noisy=ann.set.noisy, 
                                                     data.subsample.clean=ann.set.clean, 
                                                     data.complementary=subject.data, 
                                                     class.labels=class.labels, 
                                                     num.folds=num.folds)
        cms.noisy[[1+length(cms.noisy)]] = cm.noisy
        
        # update AL model
        if (nrow(ann.set.noisy) >= min.train.size) {
          model = classifier(formula=activity~., data=ann.set.noisy)
        } else {
          model = zeror(formula=activity~., data=ann.set.noisy)
        }       
      } else {
        recycle.ds = recall.buffer$w[ann.set.i, ]
      } # if(ask)
      
      ## remove previous segment from recall buffer
      #cat('discarding instances ')
      #print(ann.set.i)
      #recall.buffer$w = recall.buffer$w[-ann.set.i, ]
      #recall.buffer$w.norm = recall.buffer$w.norm[-ann.set.i, ]
      #recall.buffer$timestamp = recall.buffer$timestamp[-ann.set.i]
      #recall.buffer$predicted = recall.buffer$predicted[-ann.set.i]
      #recall.buffer$metric = recall.buffer$metric[-ann.set.i]
      #for (i in seq_len(nrow(recall.buffer$w))) {          
      #  newdata = recall.buffer$w[i, ]
      #  predicted = predict(model, newdata=newdata, type='probability')
      #  recall.buffer$predicted[[i]] = predicted
      #  #print(predicted)
      #  recall.buffer$metric[i] = offline.confusion.metric(predicted)
      #  #cat('    -->', recall.buffer$metric[i], '\n')
      #}
      ## recycle un-annotated instances
      #subject.data = rbind(subject.data, recycle.ds)
      
      cat(' discarding instances. ');
      keep.end.i = rb.len
      keep.begin.i = rb.len - 2*kernel.width + 2 
      keep.i = keep.begin.i: keep.end.i
      cat('keeping everything from', keep.begin.i, 'to', keep.end.i, '\n')
      # The buffer is NOW short of one activity point to calculate the distances.
      # The next activity point in the stream will complete the buffer to
      #  its minimum size
      
      recall.buffer$seg.begin = kernel.width - 1
      recall.buffer$w = recall.buffer$w[keep.i, ]
      recall.buffer$w.norm = recall.buffer$w.norm[keep.i, ]
      recall.buffer$timestamp = recall.buffer$timestamp[keep.i]
      recall.buffer$dists = recall.buffer$dists[keep.i]
      
      recall.buffer$predicted = recall.buffer$predicted[keep.i]
      recall.buffer$metric = recall.buffer$metric[keep.i]
      for (i in seq_along(nrow(recall.buffer$w))) {
        newdata = recall.buffer$w[i, ]
        predicted = predict(model, newdata=newdata, type='probability')
        recall.buffer$predicted[[i]] = predicted
        recall.buffer$metric[i] = offline.confusion.metric(predicted)        
      }
      # recycle un-annotated instances
      subject.data = rbind(subject.data, recycle.ds)
      
      ## audit
      training.set.size = nrow(ann.set.noisy)
      recall.buffer.size = nrow(recall.buffer$w)
      pool.size = nrow(subject.data)
      activity.stream.size = nrow(activity.stream)
      cat(' >>> training set size    =', training.set.size, '\n')
      #cat('  recall.buffer size   =', recall.buffer.size, '\n')
      #cat('  pool size            =', pool.size, '\n')
      #cat('  activity stream size =', activity.stream.size, '\n')
      sum.sizes = training.set.size + recall.buffer.size + pool.size + activity.stream.size
      #cat(' sum =', sum.sizes, '\n')
      #cat(' initial =', initial.pool.size, '\n')
      #if (sum.sizes != initial.pool.size) {
      #  stop('some pool instances not used or reused')
      #}
    }
    
    timestamp = 1 + timestamp
  }  
  
  return( list(cms=cms.noisy, cms.seg=cms.seg, question.i=question.i, 
               x.max=question.i[length(question.i)],
               seg.lengths=seg.lengths, peak.heights=peak.heights,
               min.conf.correct=min.conf.correct) )
}

SimululateSegmentationSamplingForSubject.exclude.min.conf = function(subjects.features,
                                                                     subject.i, Nb, 
                                                                     classifier.name,
                                                                     weka.control, 
                                                                     min.train.size, 
                                                                     class.labels, 
                                                                     num.folds,
                                                                     max.episode.length,
                                                                     online.confusion.metric, 
                                                                     offline.confusion.metric, 
                                                                     kernel.width, 
                                                                     threshold, ...) {
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  GetKernel = function(L) {
    m = matrix(0, 2*L, 2*L)
    for (i in seq_len(nrow(m))) {
      for (j in seq_len(ncol(m))) {
        if (((i <= L) & (j > L)) |
              ((j <= L) & (i > L))) {
          m[i, j] = 1 / (2*L^2)
        }
      }
    }
    return( m )
  }
  kernel.m = GetKernel(L=kernel.width)
  
  GetDist = function(row.1, row.2) {
    sqrt(sum((row.1-row.2)^2))
    #sum(row.1-row.2)
  }
  
  subject.data = subjects.features[[subject.i]]$data
  initial.pool.size = nrow(subject.data)
  
  mean.xs = c()
  mean.ys = c()
  mean.zs = c()
  
  var.xs = c()
  var.ys = c()
  var.zs = c()
  
  cor.xys = c()
  cor.yzs = c()
  cor.zxs = c()
  
  subject.is = seq_along(subjects.features)
  for (s.i in subject.is[-which(subject.is == subject.i)]) {
    s.data = subjects.features[[s.i]]$data
    
    mean.xs = c(mean.xs, s.data$mean.x)
    mean.ys = c(mean.ys, s.data$mean.y)
    mean.zs = c(mean.zs, s.data$mean.z)
    
    var.xs = c(var.xs, s.data$var.x)
    var.ys = c(var.ys, s.data$var.y)
    var.zs = c(var.zs, s.data$var.z)
    
    cor.xys = c(cor.xys, s.data$cor.xy)
    cor.yzs = c(cor.yzs, s.data$cor.yz)
    cor.zxs = c(cor.zxs, s.data$cor.zx)
  }
  
  quant.min = 0.01
  quant.max = 0.99
  
  mean.x.min = quantile(x=mean.xs, probs=quant.min)
  mean.x.max = quantile(x=mean.xs, probs=quant.max)
  mean.x.den = mean.x.max - mean.x.min
  
  mean.y.min = quantile(x=mean.ys, probs=quant.min)
  mean.y.max = quantile(x=mean.ys, probs=quant.max)
  mean.y.den = mean.y.max - mean.y.min
  
  mean.z.min = quantile(x=mean.zs, probs=quant.min)
  mean.z.max = quantile(x=mean.zs, probs=quant.max)
  mean.z.den = mean.z.max - mean.z.min
  
  var.x.min = quantile(x=var.xs, probs=quant.min)
  var.x.max = quantile(x=var.xs, probs=quant.max)
  var.x.den = var.x.max - var.x.min
  
  var.y.min = quantile(x=var.ys, probs=quant.min)
  var.y.max = quantile(x=var.ys, probs=quant.max)
  var.y.den = var.y.max - var.y.min
  
  var.z.min = quantile(x=var.zs, probs=quant.min)
  var.z.max = quantile(x=var.zs, probs=quant.max)
  var.z.den = var.z.max - var.z.min
  
  cor.xy.min = quantile(x=cor.xys, probs=quant.min)
  cor.xy.max = quantile(x=cor.xys, probs=quant.max)
  cor.xy.den = cor.xy.max - cor.xy.min
  
  cor.yz.min = quantile(x=cor.yzs, probs=quant.min)
  cor.yz.max = quantile(x=cor.yzs, probs=quant.max)
  cor.yz.den = cor.yz.max - cor.yz.min
  
  cor.zx.min = quantile(x=cor.zxs, probs=quant.min)
  cor.zx.max = quantile(x=cor.zxs, probs=quant.max)
  cor.zx.den = cor.zx.max - cor.zx.min
  
  recall.buffer = list()
  recall.buffer$w = subject.data[0, ]
  recall.buffer$timestamp = c()
  recall.buffer$predicted = list()
  recall.buffer$metric = c()
  recall.buffer$w.norm = subject.data[0, 1:9]
  recall.buffer$dists = c()
  recall.buffer$seg.begin = 1
  
  activity.stream = subject.data[0, ]
  
  ann.set.clean = subject.data[0, ]
  ann.set.noisy = subject.data[0, ]
  
  zeror.name = 'weka.classifiers.rules.ZeroR'
  zeror = make_Weka_classifier(name=zeror.name, class=class.labels)
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  model = zeror(formula=activity~., data=ann.set.noisy)
  
  question.i = c()  
  cms.noisy = list()
  seg.lengths = c()
  cms.seg = list()
  peak.heights = c()
  min.conf.correct = c()
  
  timestamp = 1
  while (nrow(ann.set.noisy) < Nb) {    
    cat('t =', timestamp, '... ')
    
    if (nrow(activity.stream) == 0) {
      cat('  generating new activity stream... ')
      l = GetNewActivitySubstream.min(subject.data=subject.data, 
                                      min.size=kernel.width,
                                      max.size=max.episode.length)
      activity.stream = l$activity.substream
      cat('Activity_', activity.stream$activity[1], ' ', sep='')
      cat(nrow(activity.stream), 'activity windows \n')
      subject.data = l$complementary.pool
    }
    
    w.act.stream = activity.stream[1, ]
    activity.stream = activity.stream[-1, ]
    #cat('  window pulled from activity stream\n')
    
    recall.buffer$w = rbind(recall.buffer$w, w.act.stream)
    #cat('   added to recall buffer\n')
    recall.buffer$timestamp = c(recall.buffer$timestamp, timestamp)
    #cat('   timestamp\n')
    predicted = predict(model, newdata=w.act.stream, type='probability')
    recall.buffer$predicted[[1+length(recall.buffer$predicted)]] = predicted
    #cat('   predicted\n')
    metric = offline.confusion.metric(predicted)
    recall.buffer$metric = c(recall.buffer$metric, metric)
    #cat('   metric\n')
    
    w.norm.new = subject.data[0, 1:9]
    w.norm.new[1, ]$mean.x = (mean.x.max - w.act.stream$mean.x) / mean.x.den
    w.norm.new$mean.y = (mean.y.max - w.act.stream$mean.y) / mean.y.den
    w.norm.new$mean.z = (mean.z.max - w.act.stream$mean.z) / mean.z.den
    w.norm.new$var.x = (var.x.max - w.act.stream$var.x) / var.x.den
    w.norm.new$var.y = (var.y.max - w.act.stream$var.y) / var.y.den
    w.norm.new$var.z = (var.z.max - w.act.stream$var.z) / var.z.den
    w.norm.new$cor.xy = (cor.xy.max - w.act.stream$cor.xy) / cor.xy.den
    w.norm.new$cor.yz = (cor.yz.max - w.act.stream$cor.yz) / cor.yz.den
    w.norm.new$cor.zx = (cor.zx.max - w.act.stream$cor.zx) / cor.zx.den
    recall.buffer$w.norm = rbind(recall.buffer$w.norm, w.norm.new)   
    #cat('   normalized\n')
    
    #cat('  finished adding new window to recall.buffer\n')
    
    ## detect change
    dist.m = matrix(data=0, nrow=2*kernel.width, ncol=2*kernel.width)
    rb.len = nrow(recall.buffer$w.norm)
    if (rb.len >= 2*kernel.width) {      
      i.start = rb.len - 2*kernel.width + 1
      i.end = rb.len
      i.seq = i.start: i.end
      #print(i.seq)
      for (i in i.seq) {
        for (k in i.seq) {
          row.1 = recall.buffer$w.norm[i, ]
          row.2 = recall.buffer$w.norm[k, ]
          d.i = 1 + i - i.start
          d.k = 1 + k - i.start
          #cat('  d.i=', d.i, ' d.k=', d.k, 
          #    ' (i.start=', i.start, ', i.end=', i.end, ')\n', sep='')
          dist.m[d.i, d.k] = GetDist(row.1=row.1, row.2=row.2)
        }
      }
    }    
    recall.buffer$dists = c(recall.buffer$dists, sum(dist.m * kernel.m))
    #cat('  computed aggregate distance\n')
    #cat('   rb.len =', rb.len, '\n')
    #cat('   length(recall.buffer$dists)', length(recall.buffer$dists), '\n')
    
    activity.changed = F
    d.prev = 0
    if (rb.len >= 3) {
      d.last = recall.buffer$dists[rb.len]
      d.prev = recall.buffer$dists[rb.len-1]
      d.prev.2 = recall.buffer$dists[rb.len-2]
      #cat('    d.last =', d.last, '\n')
      #cat('    d.prev =', d.prev, '\n')
      #cat('    d.prev.2 =', d.prev.2, '\n')
      #cat('    threshold =', threshold, '\n')
      #cond = (d.prev >= d.prev.2) & (d.prev >= d.last) & (d.prev > threshold)
      #cat('    condition =', cond, '\n')
      #print(recall.buffer$w)
      #print(recall.buffer$w.no)
      if ((d.prev >= d.prev.2) & (d.prev >= d.last) & (d.prev > threshold)) {
        activity.changed = T
        cat('  detected activity change at t =', timestamp - kernel.width, 
            '(now t =', timestamp, ')\n')
      }
    }
    
    #cat(' ==========================\n')
    if (activity.changed) {
      #cat('length(recall.buffer$metric) =', length(recall.buffer$metric), '\n')
      #cat('nrow(recall.buffer$w) =', nrow(recall.buffer$w), '\n')
      #cat('length(recall.buffer$dists) =', length(recall.buffer$dists), '\n')
      #cat('length(recall.buffer$predicted) =', length(recall.buffer$predicted), '\n')
      #cat('nrow(recall.buffer$w.norm) =', nrow(recall.buffer$w.norm), '\n')
      #print(recall.buffer$metric)
      
      i.begin = recall.buffer$seg.begin
      i.end = rb.len - kernel.width - 1
      if (i.begin > i.end) {
        stop('i.begin > i.end')
      }
      ann.set.i = i.begin: i.end
      
      avg.conf = recall.buffer$metric[i.begin]
      min.conf = recall.buffer$metric[i.begin]
      min.conf.i = i.begin
      #cat('    rb.len =', rb.len, '\n')
      #cat('    length(metric) =', length(recall.buffer$metric))
      for (i in (i.begin+1): i.end) {
        if (recall.buffer$metric[i] < min.conf) {
          min.conf = recall.buffer$metric[i]
          min.conf.i = i
        }
        avg.conf = avg.conf + (recall.buffer$metric[i])
      }
      cat('  avg.conf computed\n')
      
      avg.conf = avg.conf - min.conf # exclude min.conf from segment conf calculation
      avg.conf = avg.conf / (length(ann.set.i) - 1)
      # exclude min conf frame from segment
      #cat('  ann.set.i before:')
      #print(ann.set.i)
      #cat('  min.conf.i:', min.conf.i, '\n')
      ann.set.i = ann.set.i[-which(ann.set.i == min.conf.i)]
      #cat('ann.set.i after:')
      #print(ann.set.i)
      
      subject.data = rbind(subject.data, recall.buffer$w[min.conf.i, ])
      i.end = i.end - 1
      ann.set.i = i.begin: i.end
      
      #cat('   recall.buffer$w BEFORE:\n')
      #print(recall.buffer$w)
      recall.buffer$w = recall.buffer$w[-min.conf.i, ]
      #cat('   recall.buffer$w AFTER:\n')
      #print(recall.buffer$w)
      
      recall.buffer$w.norm = recall.buffer$w.norm[-min.conf.i, ]
      recall.buffer$timestamp = recall.buffer$timestamp[-min.conf.i]
      recall.buffer$dists = recall.buffer$dists[-min.conf.i]
      recall.buffer$predicted = recall.buffer$predicted[-min.conf.i]
      recall.buffer$metric = recall.buffer$metric[-min.conf.i]
      recall.buffer$seg.begin = recall.buffer$seg.begin - 1
      
      ## AVERAGE CONFIDENCE
      min.conf = avg.conf
      
      min.conf.act = recall.buffer$w$activity[min.conf.i]
      seg.mode.act = Mode(recall.buffer$w$activity[ann.set.i])   
      min.conf.correct = c(min.conf.correct, min.conf.act == seg.mode.act)
      
      cat('  min.conf =', min.conf, '\n')
      ask.prob = online.confusion.metric(offline.metric=min.conf, ...)
      
      r = runif(1)
      ask = r < ask.prob
      if (ask) {
        cat('   asking...\n')
      } else {
        cat('   not asking...\n')
      }
      
      recycle.ds = subject.data[0, ]
      
      if (ask) {
        question.i = c(question.i, timestamp)
        peak.heights = c(peak.heights, d.prev)
        cat('   annotating instances from', i.begin, 'to', i.end, '\n')
        
        #cat('    recall.buffer$w:\n')
        #print(recall.buffer$w)
        
        ann.subset.clean = recall.buffer$w[ann.set.i, ]
        ann.set.clean = rbind(ann.set.clean, ann.subset.clean)
        #cat('    - clean set\n')
        
        act.mode = Mode(recall.buffer$w$activity[ann.set.i])
        ann.subset.noisy = recall.buffer$w[ann.set.i, ]
        ann.subset.noisy$activity = act.mode
        ann.set.noisy = rbind(ann.set.noisy, ann.subset.noisy)
        #cat('    - noisy set\n')
        
        #cat('  nrow(ann.subset.clean) =', nrow(ann.subset.clean), '\n')
        #cat('  nrow(ann.subset.noisy) =', nrow(ann.subset.noisy), '\n')
        cm.seg = ConfusionMatrix(actual=ann.subset.clean$activity,
                                 predicted=ann.subset.noisy$activity, 
                                 class.labels=class.labels)
        #cat('    - segmentation cms\n')
        cms.seg[[1+length(cms.seg)]] = cm.seg
        
        seg.lengths = c(seg.lengths, nrow(ann.subset.noisy) )
        
        cm.noisy = CrossValidateWithNoisyTrainingSet(classifier.name=classifier.name, 
                                                     weka.control=weka.control, 
                                                     data.subsample.noisy=ann.set.noisy, 
                                                     data.subsample.clean=ann.set.clean, 
                                                     data.complementary=subject.data, 
                                                     class.labels=class.labels, 
                                                     num.folds=num.folds)
        cms.noisy[[1+length(cms.noisy)]] = cm.noisy
        
        # update AL model
        if (nrow(ann.set.noisy) >= min.train.size) {
          model = classifier(formula=activity~., data=ann.set.noisy)
        } else {
          model = zeror(formula=activity~., data=ann.set.noisy)
        }       
      } else {
        recycle.ds = recall.buffer$w[ann.set.i, ]
      } # if(ask)
      
      ## remove previous segment from recall buffer
      #cat('discarding instances ')
      #print(ann.set.i)
      #recall.buffer$w = recall.buffer$w[-ann.set.i, ]
      #recall.buffer$w.norm = recall.buffer$w.norm[-ann.set.i, ]
      #recall.buffer$timestamp = recall.buffer$timestamp[-ann.set.i]
      #recall.buffer$predicted = recall.buffer$predicted[-ann.set.i]
      #recall.buffer$metric = recall.buffer$metric[-ann.set.i]
      #for (i in seq_len(nrow(recall.buffer$w))) {          
      #  newdata = recall.buffer$w[i, ]
      #  predicted = predict(model, newdata=newdata, type='probability')
      #  recall.buffer$predicted[[i]] = predicted
      #  #print(predicted)
      #  recall.buffer$metric[i] = offline.confusion.metric(predicted)
      #  #cat('    -->', recall.buffer$metric[i], '\n')
      #}
      ## recycle un-annotated instances
      #subject.data = rbind(subject.data, recycle.ds)
      
      cat(' discarding instances. ');
      rb.len = nrow(recall.buffer$w)
      keep.end.i = rb.len
      keep.begin.i = rb.len - 2*kernel.width + 2 
      keep.i = keep.begin.i: keep.end.i
      cat('keeping everything from', keep.begin.i, 'to', keep.end.i, '\n')
      # The buffer is NOW short of one activity point to calculate the distances.
      # The next activity point in the stream will complete the buffer to
      #  its minimum size
      
      recall.buffer$seg.begin = kernel.width - 1
      
      #cat('-- keep.i:')
      #print(keep.i)
      
      #cat('-- recall.buffer$w BEFORE:\n')
      #print(recall.buffer$w)
      recall.buffer$w = recall.buffer$w[keep.i, ]
      #cat('-- recall.buffer$w AFTER:\n')
      #print(recall.buffer$w)
      
      recall.buffer$w.norm = recall.buffer$w.norm[keep.i, ]
      recall.buffer$timestamp = recall.buffer$timestamp[keep.i]
      recall.buffer$dists = recall.buffer$dists[keep.i]
      
      recall.buffer$predicted = recall.buffer$predicted[keep.i]
      recall.buffer$metric = recall.buffer$metric[keep.i]
      for (i in seq_along(nrow(recall.buffer$w))) {
        newdata = recall.buffer$w[i, ]
        predicted = predict(model, newdata=newdata, type='probability')
        recall.buffer$predicted[[i]] = predicted
        recall.buffer$metric[i] = offline.confusion.metric(predicted)        
      }
      # recycle un-annotated instances
      subject.data = rbind(subject.data, recycle.ds)
      
      ## audit
      #training.set.size = nrow(ann.set.noisy)
      #recall.buffer.size = nrow(recall.buffer$w)
      #pool.size = nrow(subject.data)
      #activity.stream.size = nrow(activity.stream)
      #cat('  training set size    =', training.set.size, '\n')
      #cat('  recall.buffer size   =', recall.buffer.size, '\n')
      #cat('  pool size            =', pool.size, '\n')
      #cat('  activity stream size =', activity.stream.size, '\n')
      #sum.sizes = training.set.size + recall.buffer.size + pool.size + activity.stream.size
      #cat(' sum =', sum.sizes, '\n')
      #cat(' initial =', initial.pool.size, '\n')
      #if (sum.sizes != initial.pool.size) {
      #  stop('some pool instances not used or reused')
      #}
      
      cat('>>>> Training set size:', nrow(ann.set.noisy), '\n')
    }
    
    timestamp = 1 + timestamp
    #cat('\n\n\n\n\n')
  }  
  
  return( list(cms=cms.noisy, cms.seg=cms.seg, question.i=question.i, 
               x.max=question.i[length(question.i)],
               seg.lengths=seg.lengths, peak.heights=peak.heights,
               min.conf.correct=min.conf.correct) )
}

SimulateSegmentationForSample = function(subjects.features, repetitions, 
                                         SubjectSamplingFunction, ...) {
  num.subjects = length(subjects.features)
  c.p = expand.grid(subject.i=seq_len(num.subjects), rep=seq_len(repetitions))
  l.sub.rep = foreach(row.i = seq_len(nrow(c.p))) %dopar% {
    source('R/index.sampling.R')
    #cat('.')
    row = c.p[row.i, ]
    subject.i = row$subject.i
    
    cms.data = SubjectSamplingFunction(subjects.features=subjects.features, 
                                       subject.i=subject.i, ...)
    #print(cms.data)
    list( subject.i=subject.i, cms.data=cms.data )
  }
  
  return(l.sub.rep)
}

GetPerformance = function(cms, PerformanceFunction) {
  sapply(X=cms, FUN=PerformanceFunction)
}

GetSegmentationLC.trainig_set_size = function(l, max.ts.size) {
  x = seq_len(max.ts.size)
  y = 0
  num.curves = length(l)
  for (i in seq_along(l)) {
    this.y.int = GetPerformance(cms=l[[i]]$cms.data$cms, PerformanceFunction=MeanFMeasure)
    this.y = c()
    for (k in seq_along(l[[i]]$cms.data$seg.lengths)) {
      seg.len = l[[i]]$cms.data$seg.lengths[k]
      this.y = c(this.y, rep(x=this.y.int[k], times=seg.len))
    } 
    
    y = y + (this.y[x] / num.curves)
  }
  
  return( list(x=x, y=y) )
}

GetSegmentationLC.interruptions = function(l) {
  min.num.interruptions = Inf
  for (i in seq_along(l)) {
    len = length(l[[i]]$cms.data$cms)
    if (len < min.num.interruptions) {
      min.num.interruptions = len
    }
  }
  
  num.curves = length(l)
  x = seq_len(min.num.interruptions)
  y = rep(x=0, times=min.num.interruptions)
  for (i in seq_along(l)) {
    this.y = GetPerformance(cms=l[[i]]$cms.data$cms, PerformanceFunction=MeanFMeasure)
    y = y + (this.y[x] / num.curves)
  }
  
  return( list(x=x, y=y) )
}

GetEndPerformance = function(l, max.ts.size) {
  y = GetSegmentationLC.trainig_set_size(l=l, max.ts.size=max.ts.size)$y
  return( y[length(y)] )
}

# TTSB = time-to-spend budget
GetMeanTTSB = function(l) {
  ttsb = rep(x=0, times=length(l))
  for (i in seq_along(l)) {
    ttsb[i] = l[[i]]$cms.data$x.max
  }
  
  return( mean(ttsb) )
}

SimulateHybridSamplingForSubject = function(subject.data, Nb, classifier.name, 
                                            weka.control, min.train.size, 
                                            class.labels, num.folds, recall.size,
                                            max.episode.length,
                                            min.episode.length,
                                            online.confusion.metric,
                                            offline.confusion.metric,
                                            ...) {
  zeror.name = 'weka.classifiers.rules.ZeroR'
  zeror = make_Weka_classifier(name=zeror.name, class=class.labels)
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  activity.stream = subject.data[0, ]
  
  recall.buffer = list()
  recall.buffer$w = subject.data[0, ]
  recall.buffer$timestamp = c()
  recall.buffer$predicted = list()
  recall.buffer$metric = c()
  
  ann.set = subject.data[0, ]
  model = zeror(formula=activity~., data=ann.set)
  
  question.i = c()
  
  cms = list()
  timestamp = 1
  while (nrow(ann.set) < Nb) {
    
    cat('t =', timestamp, '...\n')
    
    if (nrow(activity.stream) == 0) {
      cat('  generating new activity stream... ')
      l = GetNewActivitySubstream.min(subject.data=subject.data, 
                                      max.size=max.episode.length,
                                      min.size=min.episode.length)
      activity.stream = l$activity.substream
      cat(nrow(activity.stream), 'activity windows \n')
      subject.data = l$complementary.pool
    }
    
    w.act.stream = activity.stream[1, ]
    activity.stream = activity.stream[-1, ]
    cat('  window pulled from activity stream\n')
    
    recall.buffer$w = rbind(recall.buffer$w, w.act.stream)
    recall.buffer$timestamp = c(recall.buffer$timestamp, timestamp)
    predicted = predict(model, newdata=w.act.stream, type='probability')
    recall.buffer$predicted[[1+length(recall.buffer$predicted)]] = predicted
    metric = offline.confusion.metric(predicted)
    recall.buffer$metric = c(recall.buffer$metric, metric)
    cat('  updated recall.buffer\n')
    
    if (timestamp - recall.buffer$timestamp[1] > recall.size) {
      subject.data = rbind(subject.data, recall.buffer$w[1, ])
      recall.buffer$w = recall.buffer$w[-1, ]
      recall.buffer$timestamp = recall.buffer$timestamp[-1]
      recall.buffer$predicted = recall.buffer$predicted[-1]
      recall.buffer$metric = recall.buffer$metric[-1]
      cat('  deleted because of old timestamp\n')
    }
    
    # offline AL
    min.conf = min(recall.buffer$metric)
    min.conf.i = which(recall.buffer$metric == min.conf)
    if (length(min.conf.i) > 1) {
      # break ties randomly
      min.conf.i = sample(min.conf.i, 1)
    }
    cat('  min.conf computed\n')
    
    min.conf.w = recall.buffer$w[min.conf.i, ]
    ask.prob = online.confusion.metric(offline.metric=min.conf, ...)
    
    r = runif(1)
    ask = r < ask.prob
    if (ask) {
      cat('  asking: r =', r, ' ask.prob =', ask.prob, '\n')
      # remove window from recall.buffer
      recall.buffer$w = recall.buffer$w[-min.conf.i, ]
      recall.buffer$timestamp = recall.buffer$timestamp[-min.conf.i]
      recall.buffer$predicted = recall.buffer$predicted[-min.conf.i]
      recall.buffer$metric = recall.buffer$metric[-min.conf.i]
      
      ann.set = rbind(ann.set, min.conf.w)
      cat('  added to training set. New size =', nrow(ann.set), '\n')
      
      if (nrow(ann.set) < min.train.size) {
        cls.name = zeror.name
        cls = zeror
        control = Weka_control()
        cat('   choosing ZeroR (', nrow(ann.set), ',', min.train.size, ')\n')
      } else {
        cls.name = classifier.name
        cls = classifier
        control = weka.control
        cat('   choosing', classifier.name, '\n')
      }
      
      model = cls(formula=activity~., data=ann.set, control=control)
      
      # update recall.buffer estimated probabilities and offline metrics
      for (i in seq_along(recall.buffer$timestamp)) {
        p = predict(model, newdata=recall.buffer$w[i, ], type='probability')
        recall.buffer$predicted[[i]] = p
        recall.buffer$metric[i] = offline.confusion.metric(p)
      }
      cat('   updated estimates in the recall buffer\n')
      
      question.i = c(question.i, timestamp)
      
      # data not present in ann.set
      data.compl = rbind(subject.data, recall.buffer$w, activity.stream)
      
      #print(classifier.name)
      #print(weka.control)
      #print(nrow(ann.set))
      #print(nrow(data.compl))
      #print(class.labels)
      #print(num.folds)
      
      cat('  computing confusion matrix...\n')
      #print(classifier.name)
      #print(nrow(ann.set))
      #print(ann.set$activity)
      #print(nrow(data.compl))
      #print(data.compl$activity)
      #print(num.folds)
      conf.matrix = CrossValidateWithSubsample(classifier.name=cls.name, 
                                               weka.control=weka.control, 
                                               data.subsample=ann.set, 
                                               data.complementary=data.compl, 
                                               class.labels=class.labels, 
                                               num.folds=num.folds)
      #print(cms.selectively)
      
      cms[[1 + length(cms)]] = conf.matrix
      cat('  added cms to list\n\n')
    }
    
    timestamp = 1 + timestamp
  }
  
  return( list(cms=cms, question.i=question.i, 
               x.max=question.i[length(question.i)]) )
}

SimulateHybridSamplingForSubject.batch = function(subject.data, Nb, classifier.name, 
                                                  weka.control, min.train.size, 
                                                  class.labels, num.folds, 
                                                  recall.size, max.episode.length,
                                                  online.confusion.metric,
                                                  offline.confusion.metric,
                                                  batch.factor.f,
                                                  ...) {
  
  Max.For.Batch.Confidence.Factor = function(recall.size, f) {
    max = f(position=1, recall.size=recall.size)
    for (position in seq_len(recall.size)) {
      v = f(position=position, recall.size=recall.size)
      if (v > max) {
        max = v
      }
    }
    return( max )
  }
  
  max.factor = Max.For.Batch.Confidence.Factor(recall.size=recall.size, f=batch.factor.f)
    
  zeror.name = 'weka.classifiers.rules.ZeroR'
  zeror = make_Weka_classifier(name=zeror.name, class=class.labels)
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  activity.stream = subject.data[0, ]
  
  recall.buffer = list()
  recall.buffer$w = subject.data[0, ]
  recall.buffer$timestamp = c()
  recall.buffer$predicted = list()
  recall.buffer$metric = c()
  
  ann.set = subject.data[0, ]
  model = zeror(formula=activity~., data=ann.set)
  
  question.i = c()
  
  cms = list()
  timestamp = 1
  while (nrow(ann.set) < Nb) {
    
    cat('t =', timestamp, '...\n')
    
    if (nrow(activity.stream) == 0) {
      cat('  generating new activity stream... ')
      l = GetNewActivitySubstream(subject.data=subject.data, 
                                  max.size=max.episode.length)
      activity.stream = l$activity.substream
      cat(nrow(activity.stream), 'activity windows \n')
      subject.data = l$complementary.pool
    }
    
    w.act.stream = activity.stream[1, ]
    activity.stream = activity.stream[-1, ]
    cat('  window pulled from activity stream\n')
    
    recall.buffer$w = rbind(recall.buffer$w, w.act.stream)
    recall.buffer$timestamp = c(recall.buffer$timestamp, timestamp)
    predicted = predict(model, newdata=w.act.stream, type='probability')
    recall.buffer$predicted[[1+length(recall.buffer$predicted)]] = predicted
    metric = offline.confusion.metric(predicted)
    recall.buffer$metric = c(recall.buffer$metric, metric)
    cat('  updated recall.buffer\n')
    
    if (timestamp - recall.buffer$timestamp[1] > recall.size) {
      subject.data = rbind(subject.data, recall.buffer$w[1, ])
      recall.buffer$w = recall.buffer$w[-1, ]
      recall.buffer$timestamp = recall.buffer$timestamp[-1]
      recall.buffer$predicted = recall.buffer$predicted[-1]
      recall.buffer$metric = recall.buffer$metric[-1]
      cat('  deleted because of old timestamp\n')
    }
    
    # offline AL
    min.conf = min(recall.buffer$metric)
    min.conf.i = which(recall.buffer$metric == min.conf)
    if (length(min.conf.i) > 1) {
      # break ties randomly
      min.conf.i = sample(min.conf.i, 1)
    }
    cat('  min.conf computed\n')
    
    min.conf.w = recall.buffer$w[min.conf.i, ]
    online.metric = online.confusion.metric(offline.metric=min.conf, ...)
    
    ask.prob = online.metric
    
    if (identical(online.confusion.metric, Online.BMargin) |
          identical(online.confusion.metric, Online.Unskewed)) {
      ask.prob = ask.prob * Batch.Confidence.Factor.middle(position=min.conf.i, 
                                                           recall.size=recall.size,
                                                           max.factor=max.factor) 
    } 
    
    cat('  ask.prob =', ask.prob, '\n')
    r = runif(1)
    ask = r < ask.prob
    if (ask) {
      cat('  asking: r =', r, ' ask.prob =', ask.prob, '\n')
      # remove window from recall.buffer
      
      batch = min.conf.w
      batch.indices = c(min.conf.i)
      cat('  min.conf.i =', min.conf.i, '\n')
      
      k.start = 1 + min.conf.i
      k.end = nrow(recall.buffer$w)
      cat('   (', k.start, ',', k.end, ')\n')
      if (k.start <= k.end) {
        #print(recall.buffer$w)
        for (k in k.start:k.end) {          
          win = recall.buffer$w[k, ]
          cat('  win$activity =', as.character(win$activity), '\n')
          cat('  min.conf.w$activity =', as.character(min.conf.w$activity), '\n')
          cat('  condition:', win$activity == min.conf.w$activity, '\n')
          if (win$activity == min.conf.w$activity) {
            batch = rbind(batch, win)
            batch.indices = c(batch.indices, k)
          } else {
            break
          }
        }
      }
      cat('  searched for subsequent adjacent instances\n')
      
      k.start = 1
      k.end = min.conf.i - 1
      cat('   (', k.start, ',', k.end, ')\n')
      if (k.start <= k.end) {
        for (k in k.start:k.end) {          
          win = recall.buffer$w[k, ]
          cat('  win$activity =', as.character(win$activity), '\n')
          cat('  min.conf.w$activity =', as.character(min.conf.w$activity), '\n')
          cat('  condition:', win$activity == min.conf.w$activity, '\n')
          if (win$activity == min.conf.w$activity) {
            batch = rbind(batch, win)
            batch.indices = c(batch.indices, k)
          } else {
            break
          }
        }
      }
      cat('  searched for prior adjacent instances\n')
      
      cat('  batch size =', nrow(batch), '\n')
      cat('  num activities in batch =', length(unique(batch$activity)), '\n')
      
      recall.buffer$w = recall.buffer$w[-batch.indices, ]
      recall.buffer$timestamp = recall.buffer$timestamp[-batch.indices]
      recall.buffer$predicted = recall.buffer$predicted[-batch.indices]
      recall.buffer$metric = recall.buffer$metric[-batch.indices]
      
      ann.set = rbind(ann.set, batch)
      cat('  added to training set. New size =', nrow(ann.set), '\n')
      
      if (nrow(ann.set) < min.train.size) {
        cls.name = zeror.name
        cls = zeror
        control = Weka_control()
        cat('   choosing ZeroR (', nrow(ann.set), ',', min.train.size, ')\n')
      } else {
        cls.name = classifier.name
        cls = classifier
        control = weka.control
        cat('   choosing', classifier.name, '\n')
      }
      
      model = cls(formula=activity~., data=ann.set, control=control)
      
      # update recall.buffer estimated probabilities and offline metrics
      for (i in seq_along(recall.buffer$timestamp)) {
        p = predict(model, newdata=recall.buffer$w[i, ], type='probability')
        recall.buffer$predicted[[i]] = p
        recall.buffer$metric[i] = offline.confusion.metric(p)
      }
      cat('   updated estimates in the recall buffer\n')
      
      # data not present in ann.set
      data.compl = rbind(subject.data, recall.buffer$w, activity.stream)
      
      #print(classifier.name)
      #print(weka.control)
      #print(nrow(ann.set))
      #print(nrow(data.compl))
      #print(class.labels)
      #print(num.folds)
      
      cat('  computing confusion matrix...\n')
      #print(classifier.name)
      #print(nrow(ann.set))
      #print(ann.set$activity)
      #print(nrow(data.compl))
      #print(data.compl$activity)
      #print(num.folds)
      conf.matrix = CrossValidateWithSubsample(classifier.name=cls.name, 
                                               weka.control=weka.control, 
                                               data.subsample=ann.set, 
                                               data.complementary=data.compl, 
                                               class.labels=class.labels, 
                                               num.folds=num.folds)
      #print(cms.selectively)
      
      old.cms.len = length(cms)
      for (i in seq_len(nrow(batch))) {
        cms[[1 + length(cms)]] = conf.matrix
        cat('  added cms to list\n\n')
        
        question.i = c(question.i, timestamp)
      }
      new.cms.len = length(cms)
      if ((new.cms.len - nrow(batch)) != old.cms.len) {
        cat()
        stop(' --- Did not add the correct number of confusion matrices\n')
      }
    }
    
    timestamp = 1 + timestamp
  }
  
  return( list(cms=cms, question.i=question.i, 
               x.max=question.i[length(question.i)]) )
}

Pop.Select.Random = function(pop.activity.subset, sample.size, ...) {
  n = nrow(pop.activity.subset)
  i = sample(n, sample.size)
  return( pop.activity.subset[i, ] )
}

Pop.Select.RevTL = function(ann.set, pop.activity.subset, sample.size, 
                            class.labels, label.i, ...) {
  
}

Decay.exp = function(nu, ne, np) {
  # nu: number of user's instances
  # ne: point of equal influence 
  # np: number of population\{user} instances
  return( exp(nu/ne * log(ne/np)) )
  ##return( (ne/np)^(nu/ne) )
}

Decay.inv = function(nu, ne, np) {
  return( ne^2 / (ne^2 + nu*(np-ne)) )
}

SimulateHybridSamplingForSubject.pop = function(subject.data, Nb, classifier.name, 
                                                weka.control, min.train.size, 
                                                class.labels, num.folds, recall.size,
                                                max.episode.length,
                                                online.confusion.metric,
                                                offline.confusion.metric,
                                                other.subjects.data,
                                                decay.f, ne,
                                                pop.select.f,
                                                ...) {
  num.activities = length(class.labels)
  pop = other.subjects.data
  
  # label proportions estimated from the rest of the population
  ## NOT from the current subject's data
  n.pop = c()
  for (i in seq_along(class.labels)) {
    pop.subset = pop[pop$activity == class.labels[i], ]
    n.pop[i] = nrow(pop.subset)
  }
  
  # initial weights
  wp = rep(1, times=length(class.labels))
  
  zeror.name = 'weka.classifiers.rules.ZeroR'
  zeror = make_Weka_classifier(name=zeror.name, class=class.labels)
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  activity.stream = subject.data[0, ]
  
  recall.buffer = list()
  recall.buffer$w = subject.data[0, ]
  recall.buffer$timestamp = c()
  recall.buffer$predicted = list()
  recall.buffer$metric = c()
  
  train.pop = pop
  ann.set = subject.data[0, ]
  
  train.set = rbind(ann.set, train.pop)
  if (nrow(train.set) < min.train.size) {
    model = zeror(formula=activity~., data=train.set)
  } else {
    model = classifier(formula=activity~., data=train.set)
  }
  
  question.i = c()
  
  cms = list()
  timestamp = 1
  while (nrow(ann.set) < Nb) {
    
    cat('t =', timestamp, '...\n')
    
    if (nrow(activity.stream) == 0) {
      cat('  generating new activity stream... ')
      l = GetNewActivitySubstream(subject.data=subject.data, 
                                  max.size=max.episode.length)
      activity.stream = l$activity.substream
      cat(nrow(activity.stream), 'activity windows \n')
      subject.data = l$complementary.pool
    }
    
    w.act.stream = activity.stream[1, ]
    activity.stream = activity.stream[-1, ]
    cat('  window pulled from activity stream\n')
    
    recall.buffer$w = rbind(recall.buffer$w, w.act.stream)
    recall.buffer$timestamp = c(recall.buffer$timestamp, timestamp)
    predicted = predict(model, newdata=w.act.stream, type='probability')
    recall.buffer$predicted[[1+length(recall.buffer$predicted)]] = predicted
    metric = offline.confusion.metric(predicted)
    recall.buffer$metric = c(recall.buffer$metric, metric)
    cat('  updated recall.buffer\n')
    
    if (timestamp - recall.buffer$timestamp[1] > recall.size) {
      subject.data = rbind(subject.data, recall.buffer$w[1, ])
      recall.buffer$w = recall.buffer$w[-1, ]
      recall.buffer$timestamp = recall.buffer$timestamp[-1]
      recall.buffer$predicted = recall.buffer$predicted[-1]
      recall.buffer$metric = recall.buffer$metric[-1]
      cat('  deleted because of old timestamp\n')
    }
    
    # offline AL
    min.conf = min(recall.buffer$metric)
    min.conf.i = which(recall.buffer$metric == min.conf)
    if (length(min.conf.i) > 1) {
      # break ties randomly
      min.conf.i = sample(min.conf.i, 1)
    }
    cat('  min.conf computed\n')
    
    min.conf.w = recall.buffer$w[min.conf.i, ]
    ask.prob = online.confusion.metric(offline.metric=min.conf, ...)
    
    r = runif(1)
    ask = r < ask.prob
    if (ask) {
      cat('  asking: r =', r, ' ask.prob =', ask.prob, '\n')
      # remove window from recall.buffer
      recall.buffer$w = recall.buffer$w[-min.conf.i, ]
      recall.buffer$timestamp = recall.buffer$timestamp[-min.conf.i]
      recall.buffer$predicted = recall.buffer$predicted[-min.conf.i]
      recall.buffer$metric = recall.buffer$metric[-min.conf.i]
      
      ann.set = rbind(ann.set, min.conf.w)
      cat('  added to training set. New size =', nrow(ann.set), '\n')
      
      # removing default labels from train.set
      label.i = which(class.labels == min.conf.w$activity)
      #nu = nrow(ann.set)
      nu = nrow(ann.set[ann.set$activity == min.conf.w$activity, ])
      wp[label.i] = decay.f(nu=nu, ne=ne, np=n.pop[label.i])
      
      # 2) sample which instances to keep in population set
      sample.size = wp[label.i] * n.pop[label.i]
      activity.i = which(train.pop$activity == min.conf.w$activity)
      pop.act = train.pop[activity.i, ]
      #cat('old nrow(pop.act) =', nrow(pop.act), '\n')
      new.pop.act = pop.select.f(ann.set=ann.set, pop.activity.subset=pop.act, 
                                 sample.size=sample.size, class.labels=class.labels, 
                                 label.i=label.i)
      train.pop = train.pop[-activity.i, ]
      train.pop = rbind(train.pop, new.pop.act)
      
      train.set = rbind(train.pop, ann.set)
      
      if (nrow(train.set) < min.train.size) {
        cls.name = zeror.name
        cls = zeror
        control = Weka_control()
        #cat('   choosing ZeroR (', nrow(ann.set), ',', min.train.size, ')\n')
      } else {
        cls.name = classifier.name
        cls = classifier
        control = weka.control
        #cat('   choosing', classifier.name, '\n')
      }
      
      model = cls(formula=activity~., data=train.set, control=control)
      
      # update recall.buffer estimated probabilities and offline metrics
      for (i in seq_along(recall.buffer$timestamp)) {
        p = predict(model, newdata=recall.buffer$w[i, ], type='probability')
        recall.buffer$predicted[[i]] = p
        recall.buffer$metric[i] = offline.confusion.metric(p)
      }
      cat('   updated estimates in the recall buffer\n')
      
      question.i = c(question.i, timestamp)
      
      # data not present in ann.set
      data.compl = rbind(subject.data, recall.buffer$w, activity.stream)
      
      #print(classifier.name)
      #print(weka.control)
      #print(nrow(ann.set))
      #print(nrow(data.compl))
      #print(class.labels)
      #print(num.folds)
      
      cat('  computing confusion matrix...\n')
      #print(classifier.name)
      #print(nrow(ann.set))
      #print(ann.set$activity)
      #print(nrow(data.compl))
      #print(data.compl$activity)
      #print(num.folds)
      conf.matrix = CrossValidateWithPopulationData(classifier.name=classifier.name, 
                                                    weka.control=weka.control, 
                                                    train.user=ann.set, 
                                                    train.pop=train.pop,
                                                    test.user=data.compl, 
                                                    class.labels=class.labels, 
                                                    num.folds=num.folds)
      #print(cms.selectively)
      
      cms[[1 + length(cms)]] = conf.matrix
      cat('  added cms to list\n\n')
    }
    
    timestamp = 1 + timestamp
  }
  
  return( list(cms=cms, question.i=question.i, 
               x.max=question.i[length(question.i)]) )
}

SimulateSamplingForSample.pop = function(subjects.features, repetitions, Nb, 
                                         SubjectSamplingFunction, to.stream=F, 
                                         max.episode.length, ...) {
  num.subjects = length(subjects.features)
  c.p = expand.grid(subject.i=seq_len(num.subjects), rep=seq_len(repetitions))
  l.sub.rep = foreach(row.i = seq_len(nrow(c.p))) %dopar% {
    source('R/index.sampling.R')
    #cat('.')
    row = c.p[row.i, ]
    subject.i = row$subject.i
    subject.data = subjects.features[[subject.i]]$data
    
    other.subjects.features = subjects.features[-subject.i]
    other.subjects.data = CollapseToTable(other.subjects.features)
    
    if (to.stream) {
      subject.data = TurnPoolIntoStream(subject.data=subject.data, 
                                        max.episode.length=max.episode.length)
    }
    #cms.data = SimulateSamplingForSubject(subject.data=subject.data, Nb=Nb, ...)
    cms.data = SubjectSamplingFunction(subject.data=subject.data, Nb=Nb, 
                                       max.episode.length=max.episode.length, 
                                       other.subjects.data=other.subjects.data, ...)
    #print(cms.data)
    list( subject.i=subject.i, cms.data=cms.data )
  }
  #cat('---- 0 \n')
  # average over repetitions, but keeping subjects intact
  # compute normalized AUC for each rep and compute its average for each subject
  cms.sample = list()
  for (i in seq_along(subjects.features)) {
    cms.sample[[i]] = list()
    #cat('---- 1 \n')
    cms.sample[[i]]$cms = list()
    for (k in seq_len(Nb)) {
      cms.sample[[i]]$cms[[k]] = 0
    }
    #cat('---- 2 \n')
    cms.sample[[i]]$auc.norm = 0
    cms.sample[[i]]$num.questions = 0
  }
  #cat('---- 3 \n')
  for (i in seq_along(l.sub.rep)) {
    subject.i = l.sub.rep[[i]]$subject.i
    cms = l.sub.rep[[i]]$cms.data$cms
    y = sapply(cms, MeanFMeasure)
    #print(y)
    x = l.sub.rep[[i]]$cms.data$question.i
    x.max = l.sub.rep[[i]]$cms.data$x.max # nrow(subjects.features[[subject.i]]$data)
    #cat(' x.max =', x.max, '\n')
    auc.norm = NormalizedAUC(x=x, x.max=x.max, y=y)
    num.questions = length(unique(x))
    
    cms.sample[[subject.i]]$auc.norm = cms.sample[[subject.i]]$auc.norm + 
      (auc.norm / repetitions)
    
    cms.sample[[subject.i]]$num.questions = cms.sample[[subject.i]]$num.questions +
      (num.questions / repetitions)
    
    for (k in seq_len(Nb)) {
      cms.sample[[subject.i]]$cms[[k]] = cms.sample[[subject.i]]$cms[[k]] + cms[[k]]
    }
  }
  
  # now average over all subjects
  l = list()
  l$auc.norm = 0
  l$num.questions = 0
  l$cms = list()
  for (k in seq_len(Nb)) {
    l$cms[[k]] = 0
  }
  
  #cat('  averaging over', length(cms.sample), 'subjects...\n')
  for (i in seq_along(cms.sample)) {
    subject = cms.sample[[i]]
    for (k in seq_len(Nb)) {
      l$cms[[k]] = l$cms[[k]] + subject$cms[[k]]
    }
    l$auc.norm = l$auc.norm + (subject$auc.norm / length(subjects.features))
    l$num.questions = l$num.questions + 
      (subject$num.questions / length(subjects.features))
  }
  
  l$individuals = cms.sample
  
  # if the subject simulation sample is running in batch mode,
  # then also average over the number of interruptions
  min.num.questions = Nb
  for (i in seq_along(l.sub.rep)) {
    num.questions = length(unique(l.sub.rep[[i]]$cms.data$question.i))
    min.num.questions = min(min.num.questions, num.questions)
  }
  #cat('   min.num.questions =', min.num.questions, '\n')
  
  cms.q = list()
  for (k in seq_len(min.num.questions)) {
    cms.q[[k]] = 0
  }
  
  #cat('------ A\n')
  for (i in seq_along(l.sub.rep)) {
    cms = l.sub.rep[[i]]$cms.data$cms
    #cat('  length(cms) =', length(cms), '\n')
    question.i = l.sub.rep[[i]]$cms.data$question.i
    #print(question.i)
    q.unique = sort(unique(question.i))
    #print(q.unique)
    for (k in seq_len(min.num.questions)) {
      q = q.unique[k]
      #cat('----\n')
      #print(q.unique)
      #print(q)
      #cat(' which:\n')
      #print(which(q == question.i))
      i.q = which(q == q.unique)[1]
      #cat('   i.q =', i.q, '\n')
      #print(cms[[i.q]])
      cms.q[[k]] = cms.q[[k]] + cms[[i.q]]
    }
  }
  
  l$cms.q = cms.q
  
  return( l )
}

SimulateSamplingForSample = function(subjects.features, repetitions, Nb, 
                                     SubjectSamplingFunction, to.stream=F, 
                                     max.episode.length, ...) {
  num.subjects = length(subjects.features)
  c.p = expand.grid(subject.i=seq_len(num.subjects), rep=seq_len(repetitions))
  l.sub.rep = foreach(row.i = seq_len(nrow(c.p))) %dopar% {
    source('R/index.sampling.R')
    #cat('.')
    row = c.p[row.i, ]
    subject.i = row$subject.i
    subject.data = subjects.features[[subject.i]]$data
    if (to.stream) {
      subject.data = TurnPoolIntoStream(subject.data=subject.data, 
                                        max.episode.length=max.episode.length)
    }
    #cms.data = SimulateSamplingForSubject(subject.data=subject.data, Nb=Nb, ...)
    cms.data = SubjectSamplingFunction(subject.data=subject.data, Nb=Nb, 
                                       max.episode.length=max.episode.length, ...)
    #print(cms.data)
    list( subject.i=subject.i, cms.data=cms.data )
  }
  all = l.sub.rep
  
  #cat('---- 0 \n')
  # average over repetitions, but keeping subjects intact
  # compute normalized AUC for each rep and compute its average for each subject
  cms.sample = list()
  for (i in seq_along(subjects.features)) {
    cms.sample[[i]] = list()
    #cat('---- 1 \n')
    cms.sample[[i]]$cms = list()
    for (k in seq_len(Nb)) {
      cms.sample[[i]]$cms[[k]] = 0
    }
    #cat('---- 2 \n')
    cms.sample[[i]]$auc.norm = 0
    cms.sample[[i]]$num.questions = 0
  }
  #cat('---- 3 \n')
  for (i in seq_along(l.sub.rep)) {
    subject.i = l.sub.rep[[i]]$subject.i
    cms = l.sub.rep[[i]]$cms.data$cms
    y = sapply(cms, MeanFMeasure)
    #print(y)
    x = l.sub.rep[[i]]$cms.data$question.i
    x.max = l.sub.rep[[i]]$cms.data$x.max # nrow(subjects.features[[subject.i]]$data)
    cat(' x.max =', x.max, '\n')
    auc.norm = NormalizedAUC(x=x, x.max=x.max, y=y)
    num.questions = length(unique(x))
    
    cms.sample[[subject.i]]$auc.norm = cms.sample[[subject.i]]$auc.norm + 
      (auc.norm / repetitions)
    
    cms.sample[[subject.i]]$num.questions = cms.sample[[subject.i]]$num.questions +
      (num.questions / repetitions)
    
    for (k in seq_len(Nb)) {
      cms.sample[[subject.i]]$cms[[k]] = cms.sample[[subject.i]]$cms[[k]] + cms[[k]]
    }
  }
  
  # now average over all subjects
  l = list()
  l$l.sub.rep = l.sub.rep
  l$auc.norm = 0
  l$num.questions = 0
  l$cms = list()
  for (k in seq_len(Nb)) {
    l$cms[[k]] = 0
  }
  
  cat('  averaging over', length(cms.sample), 'subjects...\n')
  for (i in seq_along(cms.sample)) {
    subject = cms.sample[[i]]
    for (k in seq_len(Nb)) {
      l$cms[[k]] = l$cms[[k]] + subject$cms[[k]]
    }
    l$auc.norm = l$auc.norm + (subject$auc.norm / length(subjects.features))
    l$num.questions = l$num.questions + 
      (subject$num.questions / length(subjects.features))
  }
  
  l$individuals = cms.sample
  
  # if the subject simulation sample is running in batch mode,
  # then also average over the number of interruptions
  min.num.questions = Nb
  for (i in seq_along(l.sub.rep)) {
    num.questions = length(unique(l.sub.rep[[i]]$cms.data$question.i))
    min.num.questions = min(min.num.questions, num.questions)
  }
  cat('   min.num.questions =', min.num.questions, '\n')
  
  cms.q = list()
  for (i in seq_len(min.num.questions)) {
    cms.q[[i]] = 0
    for (s in l.sub.rep) {
      question.i = s$cms.data$question.i
      cms = s$cms.data$cms
      
      question.i.unique = sort(unique(question.i))
      q.i = question.i.unique[i]
      cms.i = which(q.i == question.i)[1]
      cms.q[[i]] = cms.q[[i]] + cms[[cms.i]]
    }
  }
  
  l$all = all
  l$cms.q = cms.q
  
  return( l )
}
