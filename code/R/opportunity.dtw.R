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

library(dtw)
library(doMC)

GetLabels = function(data.set) {
  labels = c()
  for (seg in data.set) {
    labels = c(labels, as.character(seg$activity[1]))
  }
  return( labels )
}

SplitDataSet = function(data.set) {
  # drop timestamps
  # separate signals from labels
  n = length(data.set)
  labels = rep(NA, n)
  trimmed.data.set = list()
  for (i in seq_along(data.set)) {
    seg = data.set[[i]]
    labels[i] = as.character(seg$activity[1])
    seg = seg[, -1]
    seg = seg[, -which(names(seg) == 'activity')]
    trimmed.data.set[[i]] = seg
  }
  return( list(labels=labels, signal.only=trimmed.data.set) )
}

DTW.distance.matrix = function(test.signal, train.signal, mc.cores=4) {
  #l = lapply(X=test.signal, FUN=DTW.distance.vector, 
  #           train.signal=train.signal)
  l = mclapply(X=test.signal, FUN=DTW.distance.vector, 
               train.signal=train.signal, mc.cores=mc.cores)
  return( do.call(cbind, args=l) )
}

DTW.distance.elem = function(train.segment, test.segment) {
  euclidean.dist = proxy::dist(x=test.segment, y=train.segment)
  alignment = dtw(x=euclidean.dist, distance.only=T)
  return( alignment$normalizedDistance )
}

DTW.distance.vector = function(test.segment, train.signal) {
  n.train.segs = length(train.signal)
  dtw.dists = sapply(X=train.signal, FUN=DTW.distance.elem, 
                     test.segment=test.segment)
  return( dtw.dists )
}


collapse.probabilities = function(nn.probs) {
  unique.names = unique(names(nn.probs))
  probs = rep(0, length(unique.names))
  names(probs) = unique.names
  
  for (i in seq_along(unique.names)) {
    u.name = unique.names[i]
    for (k in seq_along(nn.probs)) {
      if (u.name == names(nn.probs)[k]) {
        probs[i] = probs[i] + nn.probs[k]
      }
    }
  }
  return( sort(probs, decreasing=T) )
}

knn.probability = function(dist.vector, labels, k) {
  # adjust k if too little 'training' instances
  k = min(k, length(dist.vector))
  
  best.dists = sort(dist.vector, decreasing=F)[seq_len(k)]
  
  nn.i = rep(NA, k)
  for (i in seq_len(k)) {
    nn.i[i] = which(best.dists[i] == dist.vector)
  }
  nn.probs = (1/best.dists) / sum(1/best.dists)
  names(nn.probs) = labels[nn.i]
   
  return( collapse.probabilities(nn.probs=nn.probs) )
}

knn.class = function(dist.vector, labels, k) {
  nn.probs = knn.probability(dist.vector=dist.vector, 
                             labels=labels, k=k)
  return( names(nn.probs)[1] )
}

# ms = list()
# for (s.i in seq_along(train.sets)) {
#   cat('Subject', s.i, '...\n')
#   train.set = train.sets[[s.i]]$data
#   test.set = test.sets[[s.i]]$data
#   
#   l = SplitDataSet(data.set=train.set)
#   train.signal = l$signal.only
#   train.labels = l$labels
#   
#   l = SplitDataSet(data.set=test.set)
#   test.signal = l$signal.only
#   test.labels = l$labels
#   
#   cat('\tComputing distance matrix...\n')
#   m = DTW.distance.matrix(test.signal=test.signal, 
#                           train.signal=train.signal, 
#                           mc.cores=4)
#   ms[[s.i]] = m
#   actual.labels = test.labels
#   
#   ks = 1:10
#   for (k in ks) {
#     cat('\tk = ', k, '; ', sep='')
#     pred.labels = apply(X=m, MARGIN=2, FUN=knn.class, 
#                         labels=train.labels, k=k)
#     cm = ConfusionMatrix(actual=actual.labels, predicted=pred.labels, 
#                          class.labels=unique(c(test.labels, train.labels)))
#     cat('F-score =', WeightedFMeasure(cm), '\n')
#   }
# }
# 

UpdateDistanceMatrix = function(old.matrix, train.segment, 
                                test.signal) {
  distance.vector = DTW.distance.vector(test.segment=train.segment, 
                                        train.signal=test.signal)
  
  return( rbind(old.matrix, distance.vector) )
}

Offline.Confidence = function(prediction.probs) {
  return( max(prediction.probs) )
}

Online.BMargin = function(metric, gamma) {
  return( exp(-gamma*metric) )
}

Online.Random = function(metric, f) {
  return( f )
}


SimulateOpportunityDTW = function(train.set, test.set, k, 
                                  metric.function, 
                                  selection.function, 
                                  max.size, ...) {
  l = SplitDataSet(data.set=train.set)
  train.signal = l$signal.only
  train.labels = l$labels
  
  l = SplitDataSet(data.set=test.set)
  test.signal = l$signal.only
  test.labels = l$labels
  
  class.labels = unique(test.labels)
  
  ann.signal = list()
  ann.labels = c()
  q.i = c()
  cms = list()
  distance.matrix = NULL
  predicteds = list()
  
  if (is.null(max.size)) {
    max.size = length(train.signal)
  }
  
  timestamp = 1
  while (length(ann.signal) < max.size) {
    cat(timestamp, ' (', length(ann.signal), '/', 
        length(train.signal), ')', sep='')
    i.segment = sample(length(train.signal), 1)    
    segment = train.signal[[i.segment]]
    train.signal = train.signal[-i.segment]
    label = train.labels[i.segment]
    train.labels = train.labels[-i.segment]
    
    if (length(ann.signal) < 1) {
      prediction.probs = 1
    } else {
      dist.vector = DTW.distance.vector(test.segment=segment, 
                                        train.signal=ann.signal)
      prediction.probs = knn.probability(dist.vector=dist.vector, 
                                         labels=ann.labels, 
                                         k=k)
    }
    
    #print(prediction.probs)
    
    # decide whether to annotate the segment
    metric = metric.function(prediction.probs=prediction.probs)
    #cat('\nmetric =', metric, '\n')
    selection.thresh = selection.function(metric=metric, ...)
    #cat('thresh =', selection.thresh, '\n')
    
    rand.attempt = runif(1)
    ask = rand.attempt < selection.thresh
    if (ask) {
      cat('\n\tasking (', metric, ' prob => ', selection.thresh, 
          ' thresh <= ', rand.attempt, ')\n', sep='')
      ann.set.next.i = 1 + length(ann.signal)
      ann.signal[[ann.set.next.i]] = segment
      ann.labels[ann.set.next.i] = label
      
      # update DTW distance matrix
      cat('\tupdating distance matrix... ')
      time.start = Sys.time()
      distance.matrix = UpdateDistanceMatrix(old.matrix=distance.matrix, 
                                             train.segment=segment, 
                                             test.signal=test.signal)
      time.end = Sys.time()
      cat(round(time.end-time.start, 2), 's\n', sep='')
      
      # compute confusion matrix
      cat('\testimating labels...\n')
      pred.labels = apply(X=distance.matrix, MARGIN=2, FUN=knn.class, 
                          labels=ann.labels, k=k)
      cat('\tconfusion matrix...\n')
      #print(actual)
      #print(predicted)
      #print(class.labels)
      cm = ConfusionMatrix(actual=test.labels, 
                           predicted=pred.labels, 
                           class.labels=class.labels)
      cat('-')
      cms.next.i = 1 + length(cms)
      cms[[cms.next.i]] = cm
      cat('-')
      q.i[cms.next.i] = timestamp
      cat('-')
      predicteds[[cms.next.i]] = pred.labels
      cat('-\n')
      cat('\tF-score =', WeightedFMeasure(cm), '\n')
    } else {
      # put it back
      train.signal.next.i = 1+length(train.signal)
      train.signal[[train.signal.next.i]] = segment
      train.labels[train.signal.next.i] = label
      cat(' ')
    }
    
    timestamp = 1 + timestamp
  }
  
  return( list(actual=test.labels, cms=cms, q.i=q.i, 
               predicteds=predicteds) )
}

SimulateOpportunityDTW.sample = function(train.sets, test.sets, 
                                         repetitions, 
                                         opportunity.dir, 
                                         destination.dir,
                                         file.name,
                                         ...) {
  ts.ts = list()
  for (test.set in test.sets) {
    train.set = train.sets[[1]]
    subject.dir = paste('subject', test.set$subject.number, sep='_')
    for (ts in train.sets) {
      #cat(' - train.set$subject.number =', ts$subject.number, '\n')
      #cat(' - test.set$subject.number =', test.set$subject.number, '\n')
      if (ts$subject.number == test.set$subject.number) {
        train.set = ts
        #cat('using this train/test combination\n\n')
        l = list(train.set=train.set, test.set=test.set, 
                 subject.number=ts$subject.number)
        ts.ts[[1 + length(ts.ts)]] = l
        break
      } else {
        #cat('trying other train/test combination\n\n')
      }
    }
    cat('running... (', train.set$subject.number, ', ', test.set$subject.number, ')\n', sep='')
    cat(' train.set size:', length(train.set$data), '\n')
    cat(' test.set  size:', length(test.set$data), '\n')
  }
  
  grid = expand.grid(comb.i=seq_along(ts.ts), rep=seq_len(repetitions))
  i.grid = sample(x=nrow(grid), size=nrow(grid))
  grid = grid[i.grid, ]
  
  all.res = foreach (i.row = seq_len(nrow(grid))) %dopar% {
    row = grid[i.row, ]
    print(row)
    comb = ts.ts[[row$comb.i]]
    
    train.set = comb$train.set
    cat('length(train.set$data) =', length(train.set$data), '\n')
    
    test.set = comb$test.set
    cat('length(test.set$data) =', length(test.set$data), '\n')
    
    subject.number = comb$subject.number
    cat('subject.number =', subject.number, '\n')
    
    source('R/opportunity.dtw.R')      
    
    res = SimulateOpportunityDTW(train.set=train.set$data, 
                       test.set=test.set$data,  ...)
    cat('done...')
    list(res=res, subject.number=subject.number)
  }
  
  SaveObject(obj=all.res, var.name='all.res', 
             dir=paste(opportunity.dir, destination.dir, sep='/'))
  #stop('saved all.res')
  l = list()
  for (i in seq_along(test.sets)) {
    l[[i]] = list()
    l[[i]]$reps = list()
    l[[i]]$subject.number = i
  }
  
  for (elem in all.res) {
    cat('++ elem$subject.number =', elem$subject.number, '\n')
    cat('++++', length(l[[elem$subject.number]]$reps), '->')
    len = length(l[[elem$subject.number]]$reps)
    l[[elem$subject.number]]$reps[[1+len]] = elem$res
    cat(length(l[[elem$subject.number]]$reps), '\n')
  }
  
  for (elem in l) {
    subject.dir = paste('subject', elem$subject.number, sep='_')
    cat('---- Saving')
    dir = paste(opportunity.dir, destination.dir, 
                subject.dir, sep='/')
    SaveObject(obj=elem$reps, var.name=file.name, dir=dir)
  }
}
