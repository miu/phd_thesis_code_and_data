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

library(dtw)
source('~/R/sf_ALTLAR/R/ml.utils.R')

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

UpdateDistanceMatrix = function(old.matrix, train.segment, 
                                test.signal) {
  distance.vector = DTW.distance.vector(test.segment=train.segment, 
                                        train.signal=test.signal)
  
  return( rbind(old.matrix, distance.vector) )
}

SimulateOpportunityDTW.all = function(train.set, test.set, k, 
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
  while (length(train.signal) > 0) {
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
    cat('metric =', metric, '\n')
    #cat('\nmetric =', metric, '\n')
    selection.thresh = selection.function(metric=metric, ...)
    cat('selection.thresh =', selection.thresh, '\n')
    #cat('thresh =', selection.thresh, '\n')
    
    rand.attempt = runif(1)
    ask = rand.attempt < selection.thresh
    cat('ask =', ask, '\n')
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
    cat('metric =', metric, '\n')
    #cat('\nmetric =', metric, '\n')
    selection.thresh = selection.function(metric=metric, ...)
    cat('selection.thresh =', selection.thresh, '\n')
    #cat('thresh =', selection.thresh, '\n')
    
    rand.attempt = runif(1)
    ask = rand.attempt < selection.thresh
    cat('ask =', ask, '\n')
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

SimulateOpportunityDTW.rep = function(num.reps, ...) {
  foreach(rep = seq_len(num.reps)) %dopar% {
    source('simulation.functions.R')
    SimulateOpportunityDTW(...)
  }
}

SimulateOpportunityDTW.all.rep = function(num.reps, ...) {
  foreach(rep = seq_len(num.reps)) %dopar% {
    source('simulation.functions.R')
    SimulateOpportunityDTW.all(...)
  }
}

SimulateOpportunityDTW.modulated.unif = function(train.set, test.set, k, 
                                                 metric.function, 
                                                 selection.function, 
                                                 max.size, horizon.size,
                                                 history.size, num.gamma.iterations,
                                                 monotone.increasing) {
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
  timestamps = c()
  gammas = c()
  
  ## assume one initial activity per label
  unique.labels = sort(unique(train.labels))
  for (unique.label in unique.labels) {
    i.seg = which(unique.label == train.labels)
    
    if (length(i.seg) > 0) {
      i.seg = sample(i.seg, 1)
    }
    
    segment = train.signal[[i.seg]]
    train.signal = train.signal[-i.seg]
    
    label = train.labels[i.seg]
    train.labels = train.labels[-i.seg]
    
    distance.matrix = UpdateDistanceMatrix(old.matrix=distance.matrix, 
                                           train.segment=segment, 
                                           test.signal=test.signal)
    
    ann.signal[[1 + length(ann.signal)]] = segment
    ann.labels[[1 + length(ann.labels)]] = label
  }
  
  
  if (is.null(max.size)) {
    max.size = length(train.signal)
  }
  
  conf.history = rep(x=NA, times=history.size)
  conf.history.i = 1
  
  
  
  timestamp = 1
  #max.size = max.size + length(ann.signal)
  num.annotations.so.far = 0
  #while ((num.annotations.so.far < max.size) & (timestamp < (horizon.size + history.size))) {
  while (timestamp < (horizon.size + history.size) ) {
    cat('[', timestamp, ' / ', horizon.size, '] (', 
        num.annotations.so.far, ' / ', max.size, ') ', sep='')
    
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
    cat('metric =', metric, '\n')
    #cat('\nmetric =', metric, '\n')
    
    conf.history[conf.history.i] = metric
    conf.history.i = (conf.history.i + 1) %% (1 + length(conf.history))
    
    #cat('conf.history: '); print(conf.history)
    
    num.segments.left = horizon.size - (timestamp - history.size) + 1
    
    gamma = 0.5
    best.gamma = gamma
    best.diff = Inf
    best.expected = Inf
    target = max.size - num.annotations.so.far
    
    if (is.na(sum(conf.history))) {
      #cat(' ::: history unusable\n')
      if (monotone.increasing) {
        gamma = 0
        best.gamma = gamma
      } else {
        gamma = Inf
        best.gamma = gamma
      }
      #cat(' ::: (history check) gamma =', gamma, '\n')
    } else {
      gamma.min = 0
      gamma.max = Inf
      
      for (j in seq_len(num.gamma.iterations)) {
        #cat('conf.history:'); print(conf.history)
        #cat('gamma =', gamma, '\n')
        p.ask.history = selection.function(metric=conf.history, gamma=gamma)
        #cat('a\n')
        
        num.annot.expected = mean(p.ask.history) * num.segments.left
        #cat('b\n')
        
        #cat(' ::: target =', target, '\n')
        #cat(' ::: expect =', num.annot.expected, '\n')
        
        # calculate expectation
        diff.expected = abs(num.annot.expected - target)
        #cat('c\n')
        
        if (diff.expected < best.diff) {
          best.gamma = gamma
          best.diff = diff.expected
          best.expected = num.annot.expected
        }
        
        # seek better gamma
        if ((!monotone.increasing & (num.annot.expected < target)) |
              (monotone.increasing & (num.annot.expected > target))) {
          gamma.max = gamma
          gamma = (gamma + gamma.min) / 2
          #cat(' ::: new gamma =', gamma, ' (gan doon)\n')
        }
        
        if ((!monotone.increasing & (num.annot.expected > target)) | 
              (monotone.increasing & (num.annot.expected < target))) {
          gamma.min = gamma
          
          if (gamma.max == Inf) {
            gamma = gamma * 2
            #cat(' ::: new gamma =', gamma, ' (doubled) \n')
          } else {
            gamma = (gamma + gamma.max) / 2
            #cat(' ::: new gamma =', gamma, ' (gan oop) \n')
          }
          
        }
        
      }
    }
    #cat('d\n')
    
    gamma = best.gamma
    gammas = c(gammas, gamma)
    
    #cat('\tgamma =', gamma, '\n')
    #cat('\tbest.expected =', best.expected, '(target=', target, ')\n')
    #cat('\tnum.segments.left =', num.segments.left, '\n')
    
    selection.thresh = selection.function(metric=metric, gamma=gamma)
    cat('selection.thresh =', selection.thresh, '\n')
    #cat('thresh =', selection.thresh, '\n')
    #cat('e\n')
    
    rand.attempt = runif(1)
    ask = rand.attempt < selection.thresh
    cat('ask =', ask, '\n')
    if (ask) {
      
      num.annotations.so.far = 1 + num.annotations.so.far
      
      cat('\n\tasking (', metric, ' prob => ', selection.thresh, 
          ' thresh <= ', rand.attempt, ')\n', sep='')
      ann.set.next.i = 1 + length(ann.signal)
      ann.signal[[ann.set.next.i]] = segment
      ann.labels[ann.set.next.i] = label
      
      timestamps = c(timestamps, timestamp)
      
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
               predicteds=predicteds, timestamps=timestamps,
               gammas=gammas) )
}

SimulateOpportunityDTW.modulated.beta.unif = function(train.set, test.set, k, 
                                                      metric.function, 
                                                      selection.function, 
                                                      max.size, horizon.size,
                                                      history.size, num.gamma.iterations,
                                                      monotone.increasing, beta) {
  
  half.sigmoid = function(B, beta) {
    if (B < 0) {
      return( 0 )
    } else {
      return( 2 * (1 / (1 + exp(-1/beta * B)) - 0.5) )
    }
  }
  
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
  timestamps = c()
  gammas = c()
  
  ## assume one initial activity per label
  unique.labels = sort(unique(train.labels))
  for (unique.label in unique.labels) {
    i.seg = which(unique.label == train.labels)
    
    if (length(i.seg) > 0) {
      i.seg = sample(i.seg, 1)
    }
    
    segment = train.signal[[i.seg]]
    train.signal = train.signal[-i.seg]
    
    label = train.labels[i.seg]
    train.labels = train.labels[-i.seg]
    
    distance.matrix = UpdateDistanceMatrix(old.matrix=distance.matrix, 
                                           train.segment=segment, 
                                           test.signal=test.signal)
    
    ann.signal[[1 + length(ann.signal)]] = segment
    ann.labels[[1 + length(ann.labels)]] = label
  }
  
  
  if (is.null(max.size)) {
    max.size = length(train.signal)
  }
  
  conf.history = rep(x=NA, times=history.size)
  conf.history.i = 1
  
  
  
  timestamp = 1
  #max.size = max.size + length(ann.signal)
  num.annotations.so.far = 0
  #while ((num.annotations.so.far < max.size) & (timestamp < (horizon.size + history.size))) {
  while (timestamp < (horizon.size + history.size) ) {
    cat('[', timestamp, ' / ', horizon.size, '] (', 
        num.annotations.so.far, ' / ', max.size, ') ', sep='')
    
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
    cat('metric =', metric, '\n')
    #cat('\nmetric =', metric, '\n')
    
    conf.history[conf.history.i] = metric
    conf.history.i = (conf.history.i + 1) %% (1 + length(conf.history))
    
    #cat('conf.history: '); print(conf.history)
    
    num.segments.left = horizon.size - (timestamp - history.size) + 1
    
    gamma = 0.5
    best.gamma = gamma
    best.diff = Inf
    best.expected = Inf
    target = max.size - num.annotations.so.far
    
    if (is.na(sum(conf.history))) {
      #cat(' ::: history unusable\n')
      if (monotone.increasing) {
        gamma = 0
        best.gamma = gamma
      } else {
        gamma = Inf
        best.gamma = gamma
      }
      #cat(' ::: (history check) gamma =', gamma, '\n')
    } else {
      gamma.min = 0
      gamma.max = Inf
      
      for (j in seq_len(num.gamma.iterations)) {
        #cat('conf.history:'); print(conf.history)
        #cat('gamma =', gamma, '\n')
        p.ask.history = selection.function(metric=conf.history, gamma=gamma)
        #cat('a\n')
        
        num.annot.expected = mean(p.ask.history) * num.segments.left
        #cat('b\n')
        
        #cat(' ::: target =', target, '\n')
        #cat(' ::: expect =', num.annot.expected, '\n')
        
        # calculate expectation
        diff.expected = abs(num.annot.expected - target)
        #cat('c\n')
        
        if (diff.expected < best.diff) {
          best.gamma = gamma
          best.diff = diff.expected
          best.expected = num.annot.expected
        }
        
        # seek better gamma
        if ((!monotone.increasing & (num.annot.expected < target)) |
              (monotone.increasing & (num.annot.expected > target))) {
          gamma.max = gamma
          gamma = (gamma + gamma.min) / 2
          #cat(' ::: new gamma =', gamma, ' (gan doon)\n')
        }
        
        if ((!monotone.increasing & (num.annot.expected > target)) | 
              (monotone.increasing & (num.annot.expected < target))) {
          gamma.min = gamma
          
          if (gamma.max == Inf) {
            gamma = gamma * 2
            #cat(' ::: new gamma =', gamma, ' (doubled) \n')
          } else {
            gamma = (gamma + gamma.max) / 2
            #cat(' ::: new gamma =', gamma, ' (gan oop) \n')
          }
          
        }
        
      }
    }
    #cat('d\n')
    
    gamma = best.gamma
    gammas = c(gammas, gamma)
    
    #cat('\tgamma =', gamma, '\n')
    #cat('\tbest.expected =', best.expected, '(target=', target, ')\n')
    #cat('\tnum.segments.left =', num.segments.left, '\n')
    
    selection.thresh = selection.function(metric=metric, gamma=gamma)
    cat('selection.thresh =', selection.thresh, '\n')
    #cat('thresh =', selection.thresh, '\n')
    #cat('e\n')
    
    B.theoretical = max.size * ((timestamp-history.size) / horizon.size)
    B.actual = num.annotations.so.far
    B.deviation = B.theoretical - B.actual
    
    p.budget = half.sigmoid(B=abs(B.deviation), beta=beta)
    
    if (B.deviation > 0) {
      p.ask = (1 - p.budget) * selection.thresh + p.budget  
    } else {
      #p.ask = 1 - (1 - selection.thresh) * p.budget
      p.ask = selection.thresh * (1 - p.budget)
    }
    
    cat('B.deviation =', B.deviation, '\n')
    cat('p.budget =', p.budget, '\n')
    cat('p.ask =', p.ask, '\n')
    
    rand.attempt = runif(1)
    ask = rand.attempt < p.ask
    cat('ask =', ask, '\n')
    if (ask) {
      
      num.annotations.so.far = 1 + num.annotations.so.far
      
      cat('\n\tasking (', metric, ' prob => ', selection.thresh, 
          ' thresh <= ', rand.attempt, ')\n', sep='')
      ann.set.next.i = 1 + length(ann.signal)
      ann.signal[[ann.set.next.i]] = segment
      ann.labels[ann.set.next.i] = label
      
      timestamps = c(timestamps, timestamp)
      
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
               predicteds=predicteds, timestamps=timestamps,
               gammas=gammas) )
}

SimulateOpportunityDTW.modulated.exp = function(train.set, test.set, k, 
                                                metric.function, 
                                                selection.function, 
                                                max.size, horizon.size,
                                                history.size, num.gamma.iterations,
                                                lambda, monotone.increasing) {
  
  exp.distr.scaled.grid = function(lambda, B) {
    # B asymptotic budget




    t = seq(from=0, to=1, length.out=100)
    y = 1 - exp(-lambda * t)  
    
    #y = y * B #* (1 - exp(-lambda))
    y = y * B #/ (1 - exp(-lambda))
    
    return( y )
  }
  
  exp.distr.deriv.grid = function(lambda, B) {
    t = seq(from=0, to=1, length.out=100)
    y = lambda * exp(-lambda * t)
    
    #y = y * B #* (1 - exp(-lambda))
    y = y * B #/ (1 - exp(-lambda))
    
    return( y )
  }
  
  
  exp.distr.scaled = function(lambda, B, t0) {
    # t0 fraction of budget horizon elapsed
    y0 = 1 - exp(-lambda * t0)
    
    #y0 = y0 * B #* (1 - exp(-lambda))
    y0 = y0 * B  / (1 - exp(-lambda))
    
    return( y0 )
  }
  
  
  exp.distr.deriv = function(lambda, B, t0) {
    y0 = lambda * exp(-lambda * t0)
    
    #y0 = y0 * B #* (1 - exp(-lambda))
    y0 = y0 * B / (1 - exp(-lambda))
    
    return( y0 )
  }
  
  B.target = function(lambda, B, t0, B.spent) {
    slope = exp.distr.deriv(lambda, B, t0)
    
    B.theoretical = exp.distr.scaled(lambda=lambda, B=B, t0=t0)
    
    slope * (1 - t0) + (B.theoretical - B.spent)
  }
  
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
  timestamps = c()
  gammas = c()
  
  ## assume one initial activity per label
  unique.labels = sort(unique(train.labels))
  for (unique.label in unique.labels) {
    i.seg = which(unique.label == train.labels)
    
    if (length(i.seg) > 0) {
      i.seg = sample(i.seg, 1)
    }
    
    segment = train.signal[[i.seg]]
    train.signal = train.signal[-i.seg]
    
    label = train.labels[i.seg]
    train.labels = train.labels[-i.seg]
    
    distance.matrix = UpdateDistanceMatrix(old.matrix=distance.matrix, 
                                           train.segment=segment, 
                                           test.signal=test.signal)
    
    ann.signal[[1 + length(ann.signal)]] = segment
    ann.labels[[1 + length(ann.labels)]] = label
  }
  
  
  if (is.null(max.size)) {
    max.size = length(train.signal)
  }
  
  conf.history = rep(x=NA, times=history.size)
  conf.history.i = 1
  
  timestamp = 1
  num.annotations.so.far = 0
  
  
  t0 = 0
  B = max.size
  
  target = B.target(lambda=lambda, B=B, t0=t0, B.spent=num.annotations.so.far)
  
  
  #max.size = max.size + length(ann.signal)
  
  #while ((num.annotations.so.far < max.size) & (timestamp < (horizon.size + history.size))) {
  while (timestamp < (horizon.size + history.size)) {
    cat('[', timestamp, ' / ', horizon.size, '] (', 
        num.annotations.so.far, ' / ', max.size, ') ', sep='')
    
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
    cat('metric =', metric, '\n')
    #cat('\nmetric =', metric, '\n')
    
    conf.history[conf.history.i] = metric
    conf.history.i = (conf.history.i + 1) %% (1 + length(conf.history))
    
    #cat('conf.history: '); print(conf.history)
    
    num.segments.left = horizon.size - timestamp + 1
    
    gamma = 0.5
    best.gamma = gamma
    best.diff = Inf
    best.expected = Inf
    
    ############################################################################################
    ## old version
    #
    t0 = (timestamp-history.size) / horizon.size
    B = max.size
    # 
    # target = B.target(lambda=lambda, B=B, t0=t0, B.spent=num.annotations.so.far)
    ############################################################################################
    
    cat(' -- B.spent  =', num.annotations.so.far, '\n')
    cat(' -- B.theor  =', exp.distr.scaled(lambda=lambda, B=B, t0=t0), '\n')
    cat(' -- B.target =', target, '\n')
    
    if (is.na(sum(conf.history))) {
      #cat(' ::: history unusable\n')
      if (monotone.increasing) {
        gamma = 0
        best.gamma = gamma
      } else {
        gamma = Inf
        best.gamma = gamma
      }
      #cat(' ::: (history check) gamma =', gamma, '\n')
    } else {
      gamma.min = 0
      gamma.max = Inf
      
      for (j in seq_len(num.gamma.iterations)) {
        #cat('conf.history:'); print(conf.history)
        #cat('gamma =', gamma, '\n')
        p.ask.history = selection.function(metric=conf.history, gamma=gamma)
        #cat('a\n')
        
        num.annot.expected = mean(p.ask.history) * num.segments.left
        #cat('b\n')
        
        #cat(' ::: target =', target, '\n')
        #cat(' ::: expect =', num.annot.expected, '\n')
        
        # calculate expectation
        diff.expected = abs(num.annot.expected - target)
        #cat('c\n')
        
        if (diff.expected < best.diff) {
          best.gamma = gamma
          best.diff = diff.expected
          best.expected = num.annot.expected
        }
        
        # seek better gamma
        if ((!monotone.increasing & (num.annot.expected < target)) |
              (monotone.increasing & (num.annot.expected > target))) {
          gamma.max = gamma
          gamma = (gamma + gamma.min) / 2
          #cat(' ::: new gamma =', gamma, ' (gan doon)\n')
        }
        
        if ((!monotone.increasing & (num.annot.expected > target)) | 
              (monotone.increasing & (num.annot.expected < target))) {
          gamma.min = gamma
          
          if (gamma.max == Inf) {
            gamma = gamma * 2
            #cat(' ::: new gamma =', gamma, ' (doubled) \n')
          } else {
            gamma = (gamma + gamma.max) / 2
            #cat(' ::: new gamma =', gamma, ' (gan oop) \n')
          }
          
        }
        
      }
    }
    #cat('d\n')
    
    gamma = best.gamma
    gammas = c(gammas, gamma)
    
    cat('\tgamma =', gamma, '\n')
    cat('\tbest.expected =', best.expected, '(target=', target, ')\n')
    cat('\tnum.segments.left =', num.segments.left, '\n')
    
    selection.thresh = selection.function(metric=metric, gamma=gamma)
    cat('selection.thresh =', selection.thresh, '\n')
    #cat('thresh =', selection.thresh, '\n')
    
    rand.attempt = runif(1)
    ask = rand.attempt < selection.thresh
    cat('ask =', ask, '\n')
    if (ask) {
      
      num.annotations.so.far = 1 + num.annotations.so.far
      
      
      t0 = ((1+timestamp) - history.size) / horizon.size
      B = max.size
      target = B.target(lambda=lambda, B=B, t0=t0, B.spent=num.annotations.so.far)      
          
      
      cat('\n\tasking (', metric, ' prob => ', selection.thresh, 
          ' thresh <= ', rand.attempt, ')\n', sep='')
      ann.set.next.i = 1 + length(ann.signal)
      ann.signal[[ann.set.next.i]] = segment
      ann.labels[ann.set.next.i] = label
      
      timestamps = c(timestamps, timestamp)
      
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
               predicteds=predicteds, timestamps=timestamps,
               gammas=gammas) )
}

SimulateOpportunityDTW.modulated.beta.exp = function(train.set, test.set, k, 
                                                metric.function, 
                                                selection.function, 
                                                max.size, horizon.size,
                                                history.size, num.gamma.iterations,
                                                lambda, monotone.increasing, beta) {
  
  half.sigmoid = function(B, beta) {
    if (B < 0) {
      return( 0 )
    } else {
      return( 2 * (1 / (1 + exp(-1/beta * B)) - 0.5) )
    }
  }
  
  exp.distr.scaled.grid = function(lambda, B) {
    # B asymptotic budget
    
    
    
    
    t = seq(from=0, to=1, length.out=100)
    y = 1 - exp(-lambda * t)  
    
    #y = y * B #* (1 - exp(-lambda))
    y = y * B #/ (1 - exp(-lambda))
    
    return( y )
  }
  
  exp.distr.deriv.grid = function(lambda, B) {
    t = seq(from=0, to=1, length.out=100)
    y = lambda * exp(-lambda * t)
    
    #y = y * B #* (1 - exp(-lambda))
    y = y * B #/ (1 - exp(-lambda))
    
    return( y )
  }
  
  
  exp.distr.scaled = function(lambda, B, t0) {
    # t0 fraction of budget horizon elapsed
    y0 = 1 - exp(-lambda * t0)
    
    #y0 = y0 * B #* (1 - exp(-lambda))
    y0 = y0 * B  / (1 - exp(-lambda))
    
    return( y0 )
  }
  
  
  exp.distr.deriv = function(lambda, B, t0) {
    y0 = lambda * exp(-lambda * t0)
    
    #y0 = y0 * B #* (1 - exp(-lambda))
    y0 = y0 * B / (1 - exp(-lambda))
    
    return( y0 )
  }
  
  B.target = function(lambda, B, t0, B.spent) {
    slope = exp.distr.deriv(lambda, B, t0)
    
    B.theoretical = exp.distr.scaled(lambda=lambda, B=B, t0=t0)
    
    slope * (1 - t0) + (B.theoretical - B.spent)
  }
  
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
  timestamps = c()
  gammas = c()
  
  ## assume one initial activity per label
  unique.labels = sort(unique(train.labels))
  for (unique.label in unique.labels) {
    i.seg = which(unique.label == train.labels)
    
    if (length(i.seg) > 0) {
      i.seg = sample(i.seg, 1)
    }
    
    segment = train.signal[[i.seg]]
    train.signal = train.signal[-i.seg]
    
    label = train.labels[i.seg]
    train.labels = train.labels[-i.seg]
    
    distance.matrix = UpdateDistanceMatrix(old.matrix=distance.matrix, 
                                           train.segment=segment, 
                                           test.signal=test.signal)
    
    ann.signal[[1 + length(ann.signal)]] = segment
    ann.labels[[1 + length(ann.labels)]] = label
  }
  
  
  if (is.null(max.size)) {
    max.size = length(train.signal)
  }
  
  conf.history = rep(x=NA, times=history.size)
  conf.history.i = 1
  
  timestamp = 1
  num.annotations.so.far = 0
  
  
  t0 = 0
  B = max.size
  
  target = B.target(lambda=lambda, B=B, t0=t0, B.spent=num.annotations.so.far)
  
  
  #max.size = max.size + length(ann.signal)
  
  #while ((num.annotations.so.far < max.size) & (timestamp < (horizon.size + history.size))) {
  while (timestamp < (horizon.size + history.size)) {
    cat('[', timestamp, ' / ', horizon.size, '] (', 
        num.annotations.so.far, ' / ', max.size, ') ', sep='')
    
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
    cat('metric =', metric, '\n')
    #cat('\nmetric =', metric, '\n')
    
    conf.history[conf.history.i] = metric
    conf.history.i = (conf.history.i + 1) %% (1 + length(conf.history))
    
    #cat('conf.history: '); print(conf.history)
    
    num.segments.left = horizon.size - timestamp + 1
    
    gamma = 0.5
    best.gamma = gamma
    best.diff = Inf
    best.expected = Inf
    
    ############################################################################################
    ## old version
    #
    t0 = (timestamp-history.size) / horizon.size
    B = max.size
    # 
    # target = B.target(lambda=lambda, B=B, t0=t0, B.spent=num.annotations.so.far)
    ############################################################################################
    
    cat(' -- B.spent  =', num.annotations.so.far, '\n')
    cat(' -- B.theor  =', exp.distr.scaled(lambda=lambda, B=B, t0=t0), '\n')
    cat(' -- B.target =', target, '\n')
    
    if (is.na(sum(conf.history))) {
      #cat(' ::: history unusable\n')
      if (monotone.increasing) {
        gamma = 0
        best.gamma = gamma
      } else {
        gamma = Inf
        best.gamma = gamma
      }
      #cat(' ::: (history check) gamma =', gamma, '\n')
    } else {
      gamma.min = 0
      gamma.max = Inf
      
      for (j in seq_len(num.gamma.iterations)) {
        #cat('conf.history:'); print(conf.history)
        #cat('gamma =', gamma, '\n')
        p.ask.history = selection.function(metric=conf.history, gamma=gamma)
        #cat('a\n')
        
        num.annot.expected = mean(p.ask.history) * num.segments.left
        #cat('b\n')
        
        #cat(' ::: target =', target, '\n')
        #cat(' ::: expect =', num.annot.expected, '\n')
        
        # calculate expectation
        diff.expected = abs(num.annot.expected - target)
        #cat('c\n')
        
        if (diff.expected < best.diff) {
          best.gamma = gamma
          best.diff = diff.expected
          best.expected = num.annot.expected
        }
        
        # seek better gamma
        if ((!monotone.increasing & (num.annot.expected < target)) |
              (monotone.increasing & (num.annot.expected > target))) {
          gamma.max = gamma
          gamma = (gamma + gamma.min) / 2
          #cat(' ::: new gamma =', gamma, ' (gan doon)\n')
        }
        
        if ((!monotone.increasing & (num.annot.expected > target)) | 
              (monotone.increasing & (num.annot.expected < target))) {
          gamma.min = gamma
          
          if (gamma.max == Inf) {
            gamma = gamma * 2
            #cat(' ::: new gamma =', gamma, ' (doubled) \n')
          } else {
            gamma = (gamma + gamma.max) / 2
            #cat(' ::: new gamma =', gamma, ' (gan oop) \n')
          }
          
        }
        
      }
    }
    #cat('d\n')
    
    gamma = best.gamma
    gammas = c(gammas, gamma)
    
    cat('\tgamma =', gamma, '\n')
    cat('\tbest.expected =', best.expected, '(target=', target, ')\n')
    cat('\tnum.segments.left =', num.segments.left, '\n')
    
    selection.thresh = selection.function(metric=metric, gamma=gamma)
    cat('selection.thresh =', selection.thresh, '\n')
    #cat('thresh =', selection.thresh, '\n')
    
    
    ## todo
    t0 = ((1+timestamp) - history.size) / horizon.size
    
    B.theoretical = exp.distr.scaled(lambda=lambda, B=max.size, t0=t0)
    B.actual = num.annotations.so.far
    B.deviation = B.theoretical - B.actual
    
    p.budget = half.sigmoid(B=abs(B.deviation), beta=beta)
    
    if (B.deviation > 0) {
      p.ask = (1 - p.budget) * selection.thresh + p.budget  
    } else {
      #p.ask = 1 - (1 - selection.thresh) * p.budget
      p.ask = selection.thresh * (1 - p.budget)
    }
    
    cat('B.deviation =', B.deviation, '\n')
    cat('p.budget =', p.budget, '\n')
    cat('p.ask =', p.ask, '\n')    
    
    rand.attempt = runif(1)
    ask = rand.attempt < p.ask
    cat('ask =', ask, '\n')
    if (ask) {
      
      num.annotations.so.far = 1 + num.annotations.so.far
      
      
      t0 = ((1+timestamp) - history.size) / horizon.size
      B = max.size
      target = B.target(lambda=lambda, B=B, t0=t0, B.spent=num.annotations.so.far)      
      
      
      cat('\n\tasking (', metric, ' prob => ', selection.thresh, 
          ' thresh <= ', rand.attempt, ')\n', sep='')
      ann.set.next.i = 1 + length(ann.signal)
      ann.signal[[ann.set.next.i]] = segment
      ann.labels[ann.set.next.i] = label
      
      timestamps = c(timestamps, timestamp)
      
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
               predicteds=predicteds, timestamps=timestamps,
               gammas=gammas) )
}

SimulateOpportunityDTW.modulated.unif.rep = function(num.reps, ...) {
  foreach(rep = seq_len(num.reps)) %dopar% {
    source('simulation.functions.R')
    SimulateOpportunityDTW.modulated.unif(...)
  }
}

SimulateOpportunityDTW.modulated.beta.unif.rep = function(num.reps, ...) {
  foreach(rep = seq_len(num.reps)) %dopar% {
    source('simulation.functions.R')
    SimulateOpportunityDTW.modulated.beta.unif(...)
  }
}

SimulateOpportunityDTW.modulated.exp.rep = function(num.reps, ...) {
  foreach(rep = seq_len(num.reps)) %dopar% {
    source('simulation.functions.R')
    SimulateOpportunityDTW.modulated.exp(...)
  }
}

SimulateOpportunityDTW.modulated.beta.exp.rep = function(num.reps, ...) {
  foreach(rep = seq_len(num.reps)) %dopar% {
    source('simulation.functions.R')
    SimulateOpportunityDTW.modulated.beta.exp(...)
  }
}

Offline.Margin = function(prediction.probs) {
  if (length(prediction.probs) == 1) {
    return( 1 )
  } else {
    prediction.probs = sort(prediction.probs, decreasing=T)
    #cat(prediction.probs[1], '-', prediction.probs[2], '=', prediction.probs[1] - prediction.probs[2])
    return( prediction.probs[1] - prediction.probs[2] )
  }
}

Offline.Confidence = function(prediction.probs) {
  return( max(prediction.probs) )
}

Online.BMargin = function(metric, gamma) {
  return( exp(-gamma*metric) )
}

Online.Random = function(metric, gamma) {
  return( rep(x=gamma, length.out=length(metric)) )
}
