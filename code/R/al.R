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

GetMaxEntropyIndex = function(predicted) {
  index = 0
  max.entropy = -Inf
  
  for (i in seq_len(nrow(predicted))) {
    row = predicted[i, ]
    
    entropy = 0
    #cat(' ----- row ')
    #print(row)
    #cat('\n')
    for (p in row) {
      if (p != 0) {
        entropy = entropy + p * log(p)
      } else {
        entropy = 0;
        break;
      }
    }
    entropy = -entropy
    #cat(' --- entropy:', entropy, '\n\n', file=stderr())
    if (entropy > max.entropy) {
      index = i
      max.entropy = entropy
    }
  }
  
  return( list(index=index, metric=max.entropy) )
}

GetLeastMarginIndex = function(predicted) {
  index = 1
  least.margin =  1 # maximum margin is 1
  for (i in seq_len(nrow(predicted))) {
    row.sorted = sort(predicted[i, ], decreasing=T) # in decreasing order; most confident predictions first
    margin = row.sorted[1] - row.sorted[2]
    if (margin < least.margin) {
      index = i
      least.margin = margin
    }
  }
  return( list(index=index, metric=least.margin) )
}

GetLeastConfidentPredictionIndex = function(predicted) {
  best.predictions = apply(predicted, 1, max)
  worst.confidence = GetLeastConfidentPredictionConf(predicted)
  index = which(worst.confidence == best.predictions)[1]
  return( list(index=index, metric=worst.confidence) )
}

GetLeastConfidentPredictionConf = function(predicted) {
  best.predictions = apply(predicted, 1, max)
  return( min(best.predictions) )
}

GetBMargin = function(predicted, b) {
  p = max(predicted)
  return( b/(p+b) )
}

GetPredictedLabels = function(predicted, classes) {
  i = which(predicted == max(predicted))
  return( classes[i] )
}

# heuristics
al.heuristic.lc = GetLeastConfidentPredictionIndex
al.heuristic.lm = GetLeastMarginIndex
al.heuristic.en = GetMaxEntropyIndex

AverageParallelChainsAL = function(cms.list) {
  cms = list()
  metrics = c()
  sizes = c()
  for (i in seq_along(cms.list[[1]]$cms)) {
    cm = 0
    metric = 0
    size = 0
    for (k in seq_along(cms.list)) {
      cm = cm + cms.list[[k]]$cms[[i]]
      metric = metric + cms.list[[k]]$metrics[[i]]
      size = cms.list[[k]]$sizes[[i]]
    }
    last.index = length(cms)
    cms[[last.index + 1]] = cm
    metrics = c(metrics, metric / length(cms.list))
    sizes = c(sizes, size)
  }
  
  return( list(cms=cms, metrics=metrics, sizes=sizes) )
}

# pollute all labels in a dataset with a probability of gamma
PolluteLabels = function(dataset, gamma, class.labels) {
  
  pollute = function(current.label, possible.labels, gamma) {
    if (runif(1) > gamma) {
      remaining.labels = setdiff(possible.labels, current.label)
      return (sample(remaining.labels, 1))
    } else {
      return (current.label)
    }
  }
  
  dataset$activity.polluted = factor(sapply(dataset$activity, 
                                            pollute, 
                                            gamma=gamma, 
                                            possible.labels=class.labels))
  levels(dataset$activity.polluted) = levels(class.labels)
  return (dataset)
}

################################################################################
# Subject AL
SimulateALForSubjectLimited = function(classifier.name, weka.control, 
                                       subjects.features, subject.number, 
                                       class.labels, num.folds, max.instances,
                                       al.heuristic, min.instances) {
  source('R/al.R')
  source('R/ml.utils.R')
  
  cms = list() # chain of confusion matrices from classifier evaluation
  metrics = c() # sequence of corresponding metrics that drove the AL decision
  sizes = c() # sizes of the training sets in the chain
  
  s.f = FindSubjectByNumber(subjects.features, subject.number)
  data.other = s.f$data
  train.i = c()
  # sample random initial data point in training set
  #i = sample(seq_len(nrow(data.other)), 1)
  i = sample(seq_len(nrow(data.other)), min.instances) # for boosting/bagging num.instances >= num.folds
  size = min.instances
  
  #cat('First random index for AL: ', i, ' (label=', data.other[i, 'activity'], ')\n', sep='', file=stderr())
  
  #train.i = c(train.i, i)
  
  data.train = data.other[i, ]
  data.other = data.other[-i, ]
  
  classifier = make_Weka_classifier(classifier.name, class.labels)
  repeat {
    model = classifier(activity ~ ., data.train, control=weka.control)
    cm = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                    weka.control=weka.control,
                                    data.subsample=data.train, 
                                    data.complementary=data.other, 
                                    class.labels=class.labels, num.folds=num.folds)
    
    sizes = c(sizes, size)
    size = size + 1
    
    next.i = 1 + length(cms)
    cms[[next.i]] = cm
    
    other.data.predicted = predict(model, data.other, type='probability')
    
    al.decision = al.heuristic(other.data.predicted)
    new.row.index = al.decision$index
    metrics = c(metrics, al.decision$metric)
    
    new.row = data.other[new.row.index, ]
        
    data.train = rbind(data.train, new.row)
    data.other = data.other[-new.row.index, ]
    
    if ((nrow(data.train) > max.instances) | (nrow(data.other) == 0)) {
      break
    }
  }
  
  return( list(cms=cms, metrics=metrics, sizes=sizes) )
}

SimulateALForSubjectLimited.b = function(classifier.name, weka.control, 
                                         subjects.features, subject.number, 
                                         class.labels, num.folds, max.instances,
                                         al.heuristic, min.instances, b) {
  source('R/al.R')
  source('R/ml.utils.R')
  
  cms = list() # chain of confusion matrices from classifier evaluation
  metrics = c() # sequence of corresponding metrics that drove the AL decision
  sizes = c() # sizes of the training sets in the chain
  
  s.f = FindSubjectByNumber(subjects.features, subject.number)
  data.other = s.f$data
  train.i = c()
  # sample random initial data point in training set
  #i = sample(seq_len(nrow(data.other)), 1)
  i = sample(seq_len(nrow(data.other)), min.instances) # for boosting/bagging num.instances >= num.folds
  size = min.instances
  
  #cat('First random index for AL: ', i, ' (label=', data.other[i, 'activity'], ')\n', sep='', file=stderr())
  
  #train.i = c(train.i, i)
  
  data.train = data.other[i, ]
  data.other = data.other[-i, ]
  positives = data.other[0, ]
  ignored = data.other[0, ]
  
  classifier = make_Weka_classifier(classifier.name, class.labels)
  repeat {
    model = classifier(activity ~ ., data.train, control=weka.control)
    cm = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                    weka.control=weka.control,
                                    data.subsample=data.train, 
                                    data.complementary=rbind(data.other, positives, ignored), 
                                    class.labels=class.labels, num.folds=num.folds)
    
    sizes = c(sizes, size)
    size = size + 1
    
    next.i = 1 + length(cms)
    cms[[next.i]] = cm
    
    other.data.predicted = predict(model, data.other, type='probability')
    
    al.decision = al.heuristic(other.data.predicted)
    new.row.index = al.decision$index
    metrics = c(metrics, al.decision$metric)
    
    new.row = data.other[new.row.index, ]
    
    actual.label = new.row$activity
    label.probabilities = other.data.predicted[new.row.index, ]
    predicted.label = GetPredictedLabels(label.probabilities, class.labels)
    b.margin = GetBMargin(predicted=max(label.probabilities), b=b)
    
    p.rand = runif(1)
    ask = 
      (length(predicted.label) > 1) | 
      ((length(predicted.label) == 1) & (p.rand < b.margin))
    
    #cat('len.p:', length(predicted.label), '\n')
    #cat('actual:', actual.label, '\n')
    #cat('predicted.label:', predicted.label, '\n')
    #cat('p.rand:', p.rand, '\n')
    #cat('b.margin:', b.margin, '\n')
    if (ask) {
      if ( (length(predicted.label) > 1) | 
            ((length(predicted.label) == 1) & (actual.label != predicted.label)) ) {
        #cat(' -- data.train\n')
        data.train = rbind(data.train, new.row)
      } else {
        #cat(' -- positives\n')
        positives = rbind(positives, new.row)
      } 
    } else {
      #cat(' -- ignored\n')
      ignored = rbind(ignored, new.row)
    }
    #cat('\n')
    data.other = data.other[-new.row.index, ]
    
    n.inst = nrow(data.train) + nrow(positives) + nrow(ignored)
    if ((n.inst > max.instances) | (nrow(data.other) == 0)) {
      break
    }
  }
  
  return( list(cms=cms, metrics=metrics, sizes=sizes) )
}

SimulateRandomSamplingForSubjectLimited = function(classifier.name, weka.control,
                                                   subjects.features, subject.number, 
                                                   class.labels, num.folds, max.instances,
                                                   min.instances) {
  source('R/al.R')
  source('R/ml.utils.R')
  
  s.f = FindSubjectByNumber(subjects.features=subjects.features,
                            subject.number=subject.number)
  subject.data = s.f$data
  
  classifier = make_Weka_classifier(classifier.name, class.labels)
  
  n = nrow(subject.data)
  min.n = min(n, max.instances)
  #cat('=========== min.n:', min.n, '\n')
  #foreach (i = seq_len(min.n)) %do% {
  cms = list()
  metrics = c()
  sizes = c()
  
  for (i in seq(from=min.instances, to=min.n)) { # boosting/bagging
    train.i = sample(seq_len(n), i)
    #cat('random indices:', sort(train.i), '\n', file=stderr())
    train.data = subject.data[train.i, ]
    test.data = subject.data[-train.i, ]
    
    model = classifier(activity ~ ., train.data, control=weka.control)
    
    cm = CrossValidateWithSubsample(classifier.name=classifier.name, 
                               weka.control=weka.control,
                               data.subsample=train.data, 
                               data.complementary=test.data, 
                               class.labels=class.labels, num.folds=num.folds)
    #cat('nrow(train.data):', nrow(train.data), '\n', file=stderr())
    #cat('nrow(test.data):', nrow(test.data), '\n', file=stderr())
    #cat('cm:\n')
    #print(cm, file=stderr())
    #cat('\n\n')
    last.index = length(cms)
    cms[[last.index + 1]] = cm
    metrics = c(metrics, 0)
    sizes = c(sizes, i)
    #stop()
  }
  return( list(cms=cms, metrics=metrics, sizes=sizes) )
}


SimulateALForSubjectRepeatedly = function(f, repetitions, ...) {
  #cat('\n\n --------- cms.list ... --------- \n\n')
  
  cms.list = 
    foreach(i = seq_len(repetitions), .inorder=F,
            .multicombine=T, .packages='RWeka') %dopar% {
      source('R/al.R')
      source('R/tl.R')
      source('R/ml.utils.R')
      f(...)
    }
  
  # now combine CMs from parallel simulations:
  return ( AverageParallelChainsAL(cms.list) )
  
  #cat('\n\n --------- cms.list done --------- \n\n')
  
  # sequence of a simulation chain
  #seq.chain = seq_along(cms.list[[1]]$cms)
  
  #cms = list()
  #metrics = c()
  
  #for (i in seq.chain) {
  #  cm = 0
  #  metric = c()
  #  for (k in seq_along(cms.list)) {
  #    cm = cm + cms.list[[k]]$cms[[i]]
  #    metric = c(metric, cms.list[[k]]$metrics[[i]])
  #  }
  #  last.index = length(cms)
  #  cms[[last.index + 1]] = cm
  #  metrics = c(metrics, mean(metric))
  #}
  #
  #return( list(cms=cms, metrics=metrics))
}

################################################################################
# Sample AL
SimulateRandomSamplingForSampleLimited = function(classifier.name, weka.control, subjects.features, 
                                                  class.labels, num.folds, max.instances,
                                                  min.instances) {
  source('R/al.R')
  source('R/ml.utils.R')
  
  cms.list = foreach (s.f = subjects.features, .packages='RWeka') %dopar% {
    source('R/al.R')
    source('R/ml.utils.R')
    subject.number = s.f$number
    SimulateRandomSamplingForSubjectLimited(classifier.name=classifier.name, 
    #SimulateRandomSamplingForSubjectLimited.noisy(classifier.name=classifier.name, 
                                            weka.control=weka.control,
                                            subjects.features=subjects.features, 
                                            subject.number=subject.number,
                                            class.labels=class.labels, num.folds=num.folds,
                                            max.instances=max.instances,
                                            min.instances=min.instances)
  }
  
  return( AverageParallelChainsAL(cms.list) )
}

SimulateALForSampleLimited = function(classifier.name, weka.control, subjects.features,
                                      class.labels, num.folds, max.instances, al.heuristic, 
                                      min.instances) {
  source('R/al.R')
  source('R/ml.utils.R')
  
  cms.l = foreach (s.f = subjects.features, .packages='RWeka') %do% {
    source('R/al.R')
    source('R/ml.utils.R')
    subject.number = s.f$number
    SimulateALForSubjectLimited(classifier.name=classifier.name, 
                                weka.control=weka.control,
                                subjects.features=subjects.features, 
                                subject.number=subject.number, 
                                class.labels=class.labels, num.folds=num.folds, 
                                max.instances=max.instances,
                                al.heuristic=al.heuristic, min.instances=min.instances)
  }

  return( AverageParallelChainsAL(cms.l) )
}

SimulateALForSampleRepeatedly = function(f, repetitions, ...) {
  source('R/al.R')
  source('R/ml.utils.R')
  
  cms.l = foreach(i = seq_len(repetitions), .packages='RWeka') %dopar% {
    source('R/al.R')
    source('R/ml.utils.R')
    f(...)
  }
  
  return ( AverageParallelChainsAL(cms.l) )
}


################################################################################
# plotting functions
GetPerformanceEvolutionForSubjectAL = function(cms, PerformanceFunction, ...) {
  sapply(cms, PerformanceFunction, ...)
}

GetPerformanceEvolutionForSampleAL = function(cms.list, PerformanceFunction, ...) {
  # for each sample length, average out results over all subjects
  min.length = .Machine$integer.max
  x.list = list()
  y.list = list()
  
  foreach(cms = cms.list) %do% {
    next.i = 1 + length(y.list)
    perf = GetPerformanceEvolutionForSubjectAL(cms, PerformanceFunction, ...)
    x.list[[next.i]] = seq_along(perf)
    y.list[[next.i]] = perf
    len = length(perf)
    if (len < min.length) {
      min.length = len
    }
  }
  
  x = x.list[[1]][seq_len(len)]
  
  y = matrix(0, nrow=length(y.list), ncol=len)
  foreach(i = seq_along(y.list)) %do% {
    y[i, ] = y.list[[i]][seq_len(len)]
  }
  
  y = apply(y, 2, mean)
  return( list(x=x, y=y) )
}

PlotSampleALPerformanceEvolution = function(cms.list, PerformanceFunction, ...) {
  l = GetPerformanceEvolutionForSampleAL(cms.list, PerformanceFunction, ...)
  x = l$x
  y = l$y
  plot(y ~ x, type='l', ylim=c(0, 1))
}

PlotSubjectALPerformanceEvolution = function(cms.list, PerformanceFunction, ...) {
  # remove 'label' and 'first.x' from ellispis
  label = NULL
  first.x = 1
  args = list(...)
  i.label = which(names(args) == 'label')
  if (length(i.label) > 0) {
    label = args[[i.label]]
  }
  
  i.first.x = which(names(args) == 'first.x')
  if (length(i.first.x) > 0) {
    first.x = args[[i.first.x]]      
  }
  
  perf = GetPerformanceEvolutionForSubjectAL(cms=cms.list, PerformanceFunction, label=label)
  x = seq_along(perf) + first.x - 1
  
  plot(x, perf, type='l', ylim=c(0, 1), ylab='', xlab='Size of Training Set')
}

PlotConfidenceEvolutionForSubjectAL = function(confs, ...) {
  plot(confs, type='l', ...)
}

GetConfidenceEvolutionForSampleAL = function(confs) {
  cfs = 0
  for (i in seq_along(confs)) {
    chain = confs[[i]]
    for (k in seq_along(chain)) {
      if (is.na(cfs[k])) {
        cfs[k] = 0
      }
      cfs[k] = cfs[k] + chain[[k]]
    }
  }
  cfs = cfs / length(confs)
  
  return( cfs )
}

PlotConfidenceEvolutionForSampleAL = function(confs) {
  cfs = GetConfidenceEvolutionForSampleAL(confs)
  
  plot(cfs, type='l', ylim=c(0, 1))
}

################################################################################
#
DoPlotALPerformanceForSubject = function(obj, prefix, graphics.dir, first.x) {
  # LABEL_WALK
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, '.walk.FMeasure', sep=''))
  PlotSubjectALPerformanceEvolution(obj, FMeasure, label=LABEL_WALK, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, '.walk.Precision', sep=''))
  PlotSubjectALPerformanceEvolution(obj, Precision, label=LABEL_WALK, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, '.walk.Recall', sep=''))
  PlotSubjectALPerformanceEvolution(obj, Recall, label=LABEL_WALK, first.x=first.x)
  SavePlotEnd()
  
  # LABEL_RUN
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, '.run.FMeasure', sep=''))
  PlotSubjectALPerformanceEvolution(obj, FMeasure, label=LABEL_RUN, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, '.run.Precision', sep=''))
  PlotSubjectALPerformanceEvolution(obj, Precision, label=LABEL_RUN, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, '.run.Recall', sep=''))
  PlotSubjectALPerformanceEvolution(obj, Recall, label=LABEL_RUN, first.x=first.x)
  SavePlotEnd()
  
  # LABEL_UP
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, '.up.FMeasure', sep=''))
  PlotSubjectALPerformanceEvolution(obj, FMeasure, label=LABEL_UP, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, '.up.Precision', sep=''))
  PlotSubjectALPerformanceEvolution(obj, Precision, label=LABEL_UP, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, '.up.Recall', sep=''))
  PlotSubjectALPerformanceEvolution(obj, Recall, label=LABEL_UP, first.x=first.x)
  SavePlotEnd()
  
  # LABEL_DOWN
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, '.down.FMeasure', sep=''))
  PlotSubjectALPerformanceEvolution(obj, FMeasure, label=LABEL_DOWN, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, '.down.Precision', sep=''))
  PlotSubjectALPerformanceEvolution(obj, Precision, label=LABEL_DOWN, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, '.down.Recall', sep=''))
  PlotSubjectALPerformanceEvolution(obj, Recall, label=LABEL_DOWN, first.x=first.x)
  SavePlotEnd()
  
  # LABEL_STILL
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, '.still.FMeasure', sep=''))
  PlotSubjectALPerformanceEvolution(obj, FMeasure, label=LABEL_STILL, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, '.still.Precision', sep=''))
  PlotSubjectALPerformanceEvolution(obj, Precision, label=LABEL_STILL, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, '.still.Recall', sep=''))
  PlotSubjectALPerformanceEvolution(obj, Recall, label=LABEL_STILL, first.x=first.x)
  SavePlotEnd()
  
  # Accuracy
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, '.Accuracy', sep=''))
  PlotSubjectALPerformanceEvolution(obj, Accuracy, first.x=first.x)
  SavePlotEnd()
}

DoPlotALMetricForSubject = function(obj, prefix, graphics.dir, first.x) {
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, '.conf', sep='')) 
  PlotConfidenceEvolutionForSubjectAL(confs=obj, ylim=c(0, max(1, max(obj))))
  SavePlotEnd()
}


DoPlotSimulateALForSample = function(obj, prefix, first.x=first.x) {
  # LABEL_WALK
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'walk.FMeasure', sep=''))
  PlotSampleALPerformanceEvolution(obj, FMeasure, label=LABEL_WALK, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'walk.Precision', sep=''))
  PlotSampleALPerformanceEvolution(obj, Precision, label=LABEL_WALK, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'walk.Recall', sep=''))
  PlotSampleALPerformanceEvolution(obj, Recall, label=LABEL_WALK, first.x=first.x)
  SavePlotEnd()
  
  # LABEL_RUN
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'run.FMeasure', sep=''))
  PlotSampleALPerformanceEvolution(obj, FMeasure, label=LABEL_RUN, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'run.Precision', sep=''))
  PlotSampleALPerformanceEvolution(obj, Precision, label=LABEL_RUN, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'run.Recall', sep=''))
  PlotSampleALPerformanceEvolution(obj, Recall, label=LABEL_RUN, first.x=first.x)
  SavePlotEnd()
  
  # LABEL_UP
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'up.FMeasure', sep=''))
  PlotSampleALPerformanceEvolution(obj, FMeasure, label=LABEL_UP, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'up.Precision', sep=''))
  PlotSampleALPerformanceEvolution(obj, Precision, label=LABEL_UP, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'up.Recall', sep=''))
  PlotSampleALPerformanceEvolution(obj, Recall, label=LABEL_UP, first.x=first.x)
  SavePlotEnd()
  
  # LABEL_DOWN
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'down.FMeasure', sep=''))
  PlotSampleALPerformanceEvolution(obj, FMeasure, label=LABEL_DOWN, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'down.Precision', sep=''))
  PlotSampleALPerformanceEvolution(obj, Precision, label=LABEL_DOWN, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'down.Recall', sep=''))
  PlotSampleALPerformanceEvolution(obj, Recall, label=LABEL_DOWN, first.x=first.x)
  SavePlotEnd()
  
  # LABEL_STILL
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'still.FMeasure', sep=''))
  PlotSampleALPerformanceEvolution(obj, FMeasure, label=LABEL_STILL, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'still.Precision', sep=''))
  PlotSampleALPerformanceEvolution(obj, Precision, label=LABEL_STILL, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'still.Recall', sep=''))
  PlotSampleALPerformanceEvolution(obj, Recall, label=LABEL_STILL, first.x=first.x)
  SavePlotEnd()
  
  # Accuracy
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'Accuracy', sep=''))
  PlotSampleALPerformanceEvolution(obj, Accuracy, first.x=first.x)
  SavePlotEnd()
}


DoPlotDifferenceForSubject = function(obj1, obj2, PerformanceFunction, ...) {
  # remove 'label' from ellispis
  label = NULL
  plot.args = NULL
  args = list(...)
  i.label = which(names(args) == 'label')
  if (length(i.label) > 0) {
    label = args[[i.label]]
    plot.args = args[-i.label]
  }
  
  obj1.perf = GetPerformanceEvolutionForSubjectAL(cms=obj1, PerformanceFunction, 
                                                  label=label)
  obj2.perf = GetPerformanceEvolutionForSubjectAL(cms=obj2, PerformanceFunction, 
                                                  label=label)
  plot(obj1.perf - obj2.perf, type='l', ylim=c(-1, 1))
}

DoPlotVersusForSubject = function(obj1, obj2, PerformanceFunction, ...) {
  # remove 'label' from ellispis
  label = NULL
  first.x = 1
  
  args = list(...)
  
  i.label = which(names(args) == 'label')
  if (length(i.label) > 0) {
    label = args[[i.label]]
  }
  
  i.first.x = which(names(args) == 'first.x')
  if (length(i.first.x) > 0) {
    first.x = args[[i.first.x]]      
  }
  
  obj1.perf = GetPerformanceEvolutionForSubjectAL(cms=obj1, PerformanceFunction, 
                                                  label=label)
  obj2.perf = GetPerformanceEvolutionForSubjectAL(cms=obj2, PerformanceFunction, 
                                                  label=label)
  x = seq_along(obj1.perf) + first.x - 1
  plot(x, obj1.perf, type='l', ylim=c(0, 1), ylab='', xlab='Size of Training Set')
  lines(x, obj2.perf, ylim=c(-1, 1), lty=3)
}

DoPlotDifferencesForSubject = function(obj1.name, obj2.name, prefix) {
  obj1.file = paste(obj1.name, 'Rdata', sep='.')
  obj1.path = paste(output.dir, obj1.file, sep='/')
  load(obj1.path)
  obj1 = obj
  
  obj2.file = paste(obj2.name, 'Rdata', sep='.')
  obj2.path = paste(output.dir, obj2.file, sep='/')
  load(obj2.path)
  obj2 = obj
  
  # LABEL_WALK
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'walk.FMeasure', sep=''))
  DoPlotDifferenceForSubject(obj1, obj2, FMeasure, label=LABEL_WALK)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'walk.Precision', sep=''))
  DoPlotDifferenceForSubject(obj1, obj2, Precision, label=LABEL_WALK)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'walk.Recall', sep=''))
  DoPlotDifferenceForSubject(obj1, obj2, Recall, label=LABEL_WALK)
  SavePlotEnd()
  
  # LABEL_RUN
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'run.FMeasure', sep=''))
  DoPlotDifferenceForSubject(obj1, obj2, FMeasure, label=LABEL_RUN)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'run.Precision', sep=''))
  DoPlotDifferenceForSubject(obj1, obj2, Precision, label=LABEL_RUN)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'run.Recall', sep=''))
  DoPlotDifferenceForSubject(obj1, obj2, Recall, label=LABEL_RUN)
  SavePlotEnd()
  
  # LABEL_UP
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'up.FMeasure', sep=''))
  DoPlotDifferenceForSubject(obj1, obj2, FMeasure, label=LABEL_UP)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'up.Precision', sep=''))
  DoPlotDifferenceForSubject(obj1, obj2, Precision, label=LABEL_UP)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'up.Recall', sep=''))
  DoPlotDifferenceForSubject(obj1, obj2, Recall, label=LABEL_UP)
  SavePlotEnd()
  
  # LABEL_DOWN
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'down.FMeasure', sep=''))
  DoPlotDifferenceForSubject(obj1, obj2, FMeasure, label=LABEL_DOWN)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'down.Precision', sep=''))
  DoPlotDifferenceForSubject(obj1, obj2, Precision, label=LABEL_DOWN)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'down.Recall', sep=''))
  DoPlotDifferenceForSubject(obj1, obj2, Recall, label=LABEL_DOWN)
  SavePlotEnd()
  
  # LABEL_STILL
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'still.FMeasure', sep=''))
  DoPlotDifferenceForSubject(obj1, obj2, FMeasure, label=LABEL_STILL)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'still.Precision', sep=''))
  DoPlotDifferenceForSubject(obj1, obj2, Precision, label=LABEL_STILL)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'still.Recall', sep=''))
  DoPlotDifferenceForSubject(obj1, obj2, Recall, label=LABEL_STILL)
  SavePlotEnd()
  
  # Accuracy
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'Accuracy', sep=''))
  DoPlotDifferenceForSubject(obj1, obj2, Accuracy)
  SavePlotEnd()
}

DoPlotVersiForSubject = function(obj1.dir, obj1.name, obj2.dir, obj2.name, prefix, 
                                 graphics.dir, first.x) {
  obj1.file = paste(obj1.name, 'Rdata', sep='.')
  obj1.path = paste(obj1.dir, obj1.file, sep='/')
  load(obj1.path)
  obj1 = obj$cms
  
  obj2.file = paste(obj2.name, 'Rdata', sep='.')
  obj2.path = paste(obj2.dir, obj2.file, sep='/')
  load(obj2.path)
  obj2 = obj$cms
  
  # LABEL_WALK
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'walk.FMeasure', sep=''))
  DoPlotVersusForSubject(obj1, obj2, FMeasure, label=LABEL_WALK, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'walk.Precision', sep=''))
  DoPlotVersusForSubject(obj1, obj2, Precision, label=LABEL_WALK, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'walk.Recall', sep=''))
  DoPlotVersusForSubject(obj1, obj2, Recall, label=LABEL_WALK, first.x=first.x)
  SavePlotEnd()
  
  # LABEL_RUN
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'run.FMeasure', sep=''))
  DoPlotVersusForSubject(obj1, obj2, FMeasure, label=LABEL_RUN, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'run.Precision', sep=''))
  DoPlotVersusForSubject(obj1, obj2, Precision, label=LABEL_RUN, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'run.Recall', sep=''))
  DoPlotVersusForSubject(obj1, obj2, Recall, label=LABEL_RUN, first.x=first.x)
  SavePlotEnd()
  
  # LABEL_UP
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'up.FMeasure', sep=''))
  DoPlotVersusForSubject(obj1, obj2, FMeasure, label=LABEL_UP, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'up.Precision', sep=''))
  DoPlotVersusForSubject(obj1, obj2, Precision, label=LABEL_UP, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'up.Recall', sep=''))
  DoPlotVersusForSubject(obj1, obj2, Recall, label=LABEL_UP, first.x=first.x)
  SavePlotEnd()
  
  # LABEL_DOWN
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'down.FMeasure', sep=''))
  DoPlotVersusForSubject(obj1, obj2, FMeasure, label=LABEL_DOWN, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'down.Precision', sep=''))
  DoPlotVersusForSubject(obj1, obj2, Precision, label=LABEL_DOWN, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'down.Recall', sep=''))
  DoPlotVersusForSubject(obj1, obj2, Recall, label=LABEL_DOWN, first.x=first.x)
  SavePlotEnd()
  
  # LABEL_STILL
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'still.FMeasure', sep=''))
  DoPlotVersusForSubject(obj1, obj2, FMeasure, label=LABEL_STILL, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'still.Precision', sep=''))
  DoPlotVersusForSubject(obj1, obj2, Precision, label=LABEL_STILL, first.x=first.x)
  SavePlotEnd()
  
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'still.Recall', sep=''))
  DoPlotVersusForSubject(obj1, obj2, Recall, label=LABEL_STILL, first.x=first.x)
  SavePlotEnd()
  
  # Accuracy
  SavePlotBegin(dir=graphics.dir, file.name=paste(prefix, 'Accuracy', sep=''))
  DoPlotVersusForSubject(obj1, obj2, Accuracy, first.x=first.x)
  SavePlotEnd()
}

################################################################################
# Driver functions
RunALForSubject = function(repetitions, classifier.name, weka.control, subjects.features,
                           subject.number, class.labels, num.folds, output.dir, graphics.dir, 
                           object.name, max.instances, min.instances=1, b) {
  subj.dir = paste('subject', subject.number, sep='.')
  
  ## Least Confidence
  output.dir.lc = paste(output.dir, 'least.conf', subj.dir, sep='/')
  dir.create(path=output.dir, showWarnings=F, recursive=T)
  
  graphics.dir.lc = paste(graphics.dir, 'least.conf', subj.dir, sep='/')
  dir.create(path=graphics.dir, showWarnings=F, recursive=T)
  
  DoSimulateALForSubjectLimitedRepeatedly(repetitions=repetitions, classifier.name=classifier.name, 
                                          weka.control=weka.control, 
                                          subjects.features=subjects.features, 
                                          subject.number=subject.number, class.labels=class.labels, 
                                          num.folds=num.folds, max.instances=max.instances,
                                          al.heuristic=al.heuristic.lc, output.dir=output.dir.lc, 
                                          graphics.dir=graphics.dir.lc, object.name=object.name, 
                                          min.instances=min.instances, b=b)  
  
  ## Least Margin
  #output.dir.lm = paste(output.dir, 'least.marg', subj.dir, sep='/')
  #dir.create(path=output.dir, showWarnings=F, recursive=T)
  #
  #graphics.dir.lm = paste(graphics.dir, 'least.marg', subj.dir, sep='/')
  #dir.create(path=graphics.dir, showWarnings=F, recursive=T)
  #
  #DoSimulateALForSubjectLimitedRepeatedly(repetitions=repetitions, classifier.name=classifier.name, weka.control=weka.control, 
  #                                        subjects.features=subjects.features, subject.number=subject.number,
  #                                        class.labels=class.labels, num.folds=num.folds, max.instances=max.instances,
  #                                        al.heuristic=al.heuristic.lm, output.dir=output.dir.lm, graphics.dir=graphics.dir.lm, 
  #                                        object.name=object.name, min.instances=min.instances)
  
  ## Entropy
  #output.dir.en = paste(output.dir, 'entropy', subj.dir, sep='/')
  #dir.create(path=output.dir, showWarnings=F, recursive=T)
  #
  #graphics.dir.en = paste(graphics.dir, 'entropy', subj.dir, sep='/')
  #dir.create(path=graphics.dir, showWarnings=F, recursive=T)
  #
  #DoSimulateALForSubjectLimitedRepeatedly(repetitions=repetitions, classifier.name=classifier.name, weka.control=weka.control, 
  #                                        subjects.features=subjects.features, subject.number=subject.number,
  #                                        class.labels=class.labels, num.folds=num.folds, max.instances=max.instances,
  #                                        al.heuristic=al.heuristic.en, output.dir=output.dir.en, graphics.dir=graphics.dir.en, 
  #                                        object.name=object.name, min.instances=min.instances)
  
  ## Random Sampling
  #output.dir.rand = paste(output.dir, 'random', subj.dir, sep='/')
  #dir.create(path=output.dir, showWarnings=F, recursive=T)
  #
  #graphics.dir.rand = paste(graphics.dir, 'random', subj.dir, sep='/')
  #dir.create(path=graphics.dir, showWarnings=F, recursive=T)
  #
  #DoSimulateRandomSamplingForSubjectLimitedRepeatedly(repetitions=repetitions, classifier.name=classifier.name, weka.control=weka.control, 
  #                                                    subjects.features=subjects.features, subject.number=subject.number,
  #                                                    class.labels=class.labels, num.folds=num.folds, max.instances=max.instances,
  #                                                    output.dir=output.dir.rand, graphics.dir=graphics.dir.rand, 
  #                                                    object.name=object.name, min.instances=min.instances)
  #
  #prefix = paste('versus', object.name, '', sep='.')
  
  ## Least Confidence vs. Random
  #cat('-- Plotting LC vs. Random...\n')
  #dir.create(path=graphics.dir, showWarnings=F, recursive=T)
  #DoPlotVersiForSubject(obj1.dir=output.dir.lc,
  #                      obj1.name=object.name, 
  #                      obj2.dir=output.dir.rand,
  #                      obj2.name=object.name,
  #                      prefix=prefix,
  #                      graphics.dir=graphics.dir.lc,
  #                     first.x=min.instances)
  
  ## Least Margin vs. Random
  #cat('-- Plotting LM vs. Random...\n')
  #dir.create(path=graphics.dir, showWarnings=F, recursive=T)
  #DoPlotVersiForSubject(obj1.dir=output.dir.lm,
  #                      obj1.name=object.name, 
  #                      obj2.dir=output.dir.rand,
  #                      obj2.name=object.name,
  #                      prefix=prefix,
  #                      graphics.dir=graphics.dir.lm,
  #                      first.x=min.instances)
  
  ## Entropy vs. Random
  #cat('-- Plotting Entropy vs. Random...\n')
  #dir.create(path=graphics.dir, showWarnings=F, recursive=T)
  #DoPlotVersiForSubject(obj1.dir=output.dir.en,
  #                      obj1.name=object.name, 
  #                      obj2.dir=output.dir.rand,
  #                      obj2.name=object.name,
  #                      prefix=prefix,
  #                      graphics.dir=graphics.dir.en,
  #                      first.x=min.instances)
}

DoSimulateALForSubjectLimitedRepeatedly = function(repetitions, classifier.name, weka.control, 
                                                   subjects.features, subject.number, class.labels, 
                                                   num.folds, max.instances, al.heuristic, 
                                                   output.dir, graphics.dir, object.name,
                                                   min.instances, b) {
  cat("-- AL for subject", subject.number, "limited & repeatedly...\n")
  cms = SimulateALForSubjectRepeatedly(f=SimulateALForSubjectLimited.b, b=b,
                                       repetitions=repetitions,
                                       classifier.name=classifier.name, 
                                       weka.control=weka.control,
                                       subjects.features=subjects.features, 
                                       subject.number=subject.number, 
                                       class.labels=class.labels, 
                                       num.folds=num.folds, max.instances=max.instances,
                                       al.heuristic=al.heuristic,
                                       min.instances=min.instances)  
  SaveObject(obj=cms, var.name=object.name, dir=output.dir)
  
  DoPlotALPerformanceForSubject(obj=cms$cms, prefix=object.name, graphics.dir=graphics.dir, first.x=min.instances)
  DoPlotALMetricForSubject(obj=cms$metric, prefix=object.name, graphics.dir=graphics.dir, first.x=min.instances)
}

DoSimulateRandomSamplingForSubjectLimitedRepeatedly = function(repetitions, classifier.name, 
                                                               weka.control, subjects.features,
                                                               subject.number, class.labels, 
                                                               num.folds, max.instances,
                                                               output.dir, graphics.dir, 
                                                               object.name, min.instances) {
  cat('-- Random Sampling for subject', subject.number, ' limited repeatedly...\n')
  cms = SimulateALForSubjectRepeatedly(f=SimulateRandomSamplingForSubjectLimited,
                                       repetitions=repetitions,
                                       classifier.name=classifier.name, 
                                       weka.control=weka.control,
                                       subjects.features=subjects.features, 
                                       subject.number=subject.number, 
                                       class.labels=class.labels, 
                                       max.instances=max.instances,
                                       num.folds=num.folds,
                                       min.instances=min.instances)
  
  SaveObject(obj=cms, var.name=object.name, dir=output.dir)
  
  DoPlotALPerformanceForSubject(obj=cms$cms, prefix=object.name, graphics.dir=graphics.dir, 
                                first.x=min.instances)
}

RunALForSample = function(repetitions, classifier.name, weka.control, 
                          subjects.features, class.labels, num.folds,
                          output.dir, graphics.dir, object.name, 
                          max.instances, min.instances=1) {
  sample.dir = 'sample'
  
  ## Least Confidence
  output.dir.lc = paste(output.dir, 'least.conf', sample.dir, sep='/')
  dir.create(path=output.dir, showWarnings=F, recursive=T)
  
  graphics.dir.lc = paste(graphics.dir, 'least.conf', sample.dir, sep='/')
  dir.create(path=graphics.dir, showWarnings=F, recursive=T)
  
  DoSimulateALForSampleLimitedRepeatedly(repetitions=repetitions, 
                                         classifier.name=classifier.name, 
                                         weka.control=weka.control, 
                                         subjects.features=subjects.features,
                                         class.labels=class.labels, num.folds=num.folds, 
                                         max.instances=max.instances,
                                         al.heuristic=al.heuristic.lc, output.dir=output.dir.lc, 
                                         graphics.dir=graphics.dir.lc, object.name=object.name, 
                                         min.instances=min.instances)
  
  ## Least Margin
  #output.dir.lm = paste(output.dir, 'least.margin', sample.dir, sep='/')
  #dir.create(path=output.dir, showWarnings=F, recursive=T)
  #
  #graphics.dir.lm = paste(graphics.dir, 'least.margin', sample.dir, sep='/')
  #dir.create(path=graphics.dir, showWarnings=F, recursive=T)
  #
  #DoSimulateALForSampleLimitedRepeatedly(repetitions=repetitions, classifier.name=classifier.name, 
  #                                       weka.control=weka.control, 
  #                                       subjects.features=subjects.features,
  #                                       class.labels=class.labels, num.folds=num.folds, 
  #                                       max.instances=max.instances,
  #                                       al.heuristic=al.heuristic.lm, output.dir=output.dir.lm, 
  #                                       graphics.dir=graphics.dir.lm, object.name=object.name, 
  #                                       min.instances=min.instances)
  
  ## Entropy
  #output.dir.en = paste(output.dir, 'entropy', sample.dir, sep='/')
  #dir.create(path=output.dir, showWarnings=F, recursive=T)
  #
  #graphics.dir.en = paste(graphics.dir, 'entropy', sample.dir, sep='/')
  #dir.create(path=graphics.dir, showWarnings=F, recursive=T)
  #
  #DoSimulateALForSampleLimitedRepeatedly(repetitions=repetitions, classifier.name=classifier.name, 
  #                                       weka.control=weka.control, 
  #                                       subjects.features=subjects.features,
  #                                       class.labels=class.labels, num.folds=num.folds, 
  #                                       max.instances=max.instances,
  #                                       al.heuristic=al.heuristic.en, output.dir=output.dir.en, 
  #                                       graphics.dir=graphics.dir.en, object.name=object.name, 
  #                                       min.instances=min.instances)
  
  ## Random Sampling
  output.dir.rand = paste(output.dir, 'random', sample.dir, sep='/')
  dir.create(path=output.dir, showWarnings=F, recursive=T)
  
  graphics.dir.rand = paste(graphics.dir, 'random', sample.dir, sep='/')
  dir.create(path=graphics.dir, showWarnings=F, recursive=T)
  
  DoSimulateRandomSamplingForSampleLimitedRepeatedly(repetitions=repetitions, 
                                                     classifier.name=classifier.name, 
                                                     weka.control=weka.control, 
                                                     subjects.features=subjects.features,
                                                     class.labels=class.labels, 
                                                     num.folds=num.folds, 
                                                     max.instances=max.instances, 
                                                     output.dir=output.dir.rand, 
                                                     graphics.dir=graphics.dir.rand, 
                                                     object.name=object.name, 
                                                     min.instances=min.instances)
  
  prefix = paste('versus', object.name, sep='.')  
    
  ## Least Confidence vs. Random
  cat('-- Plotting LC vs. Random...\n')
  dir.create(path=graphics.dir, showWarnings=F, recursive=T)
  DoPlotVersiForSubject(obj1.dir=output.dir.lc,
                        obj1.name=object.name, 
                        obj2.dir=output.dir.rand,
                        obj2.name=object.name,
                        prefix=prefix,
                        graphics.dir=graphics.dir.lc,
                        first.x=min.instances)
  
  ## Least Margin vs. Random
  #cat('-- Plotting LM vs. Random...\n')
  #dir.create(path=graphics.dir, showWarnings=F, recursive=T)
  #DoPlotVersiForSubject(obj1.dir=output.dir.lm,
  #                      obj1.name=object.name, 
  #                      obj2.dir=output.dir.rand,
  #                      obj2.name=object.name,
  #                      prefix=prefix,
  #                      graphics.dir=graphics.dir.lm,
  #first.x=min.instances)
  
  ## Entropy vs. Random
  #cat('-- Plotting Entropy vs. Random...\n')
  #dir.create(path=graphics.dir, showWarnings=F, recursive=T)
  #DoPlotVersiForSubject(obj1.dir=output.dir.en,
  #                      obj1.name=object.name, 
  #                      obj2.dir=output.dir.rand,
  #                      obj2.name=object.name,
  #                      prefix=prefix,
  #                      graphics.dir=graphics.dir.en,
  #                      first.x=min.instances)
}

DoSimulateALForSampleLimitedRepeatedly = function(repetitions, classifier.name, weka.control, 
                                                  subjects.features, class.labels, num.folds, 
                                                  max.instances, al.heuristic, output.dir, 
                                                  graphics.dir, object.name, min.instances) {
  cat('-- AL for sample limited & repeatedly...\n')
  cms.list = SimulateALForSampleRepeatedly(f=SimulateALForSampleLimited, 
                                           repetitions=repetitions,
                                           classifier.name=classifier.name,
                                           weka.control=weka.control,
                                           subjects.features=subjects.features, 
                                           class.labels=class.labels, 
                                           num.folds=num.folds, max.instances=max.instances,
                                           al.heuristic=al.heuristic,
                                           min.instances=min.instances)
  SaveObject(obj=cms.list, var.name=object.name, dir=output.dir)
  
  DoPlotALPerformanceForSubject(obj=cms.list$cms, prefix=object.name, 
                                graphics.dir=graphics.dir, first.x=min.instances)
  DoPlotALMetricForSubject(obj=cms.list$metric, prefix=object.name, 
                           graphics.dir=graphics.dir, first.x=min.instances)
}

DoSimulateRandomSamplingForSampleLimitedRepeatedly = function(repetitions, classifier.name, 
                                                              weka.control, subjects.features,
                                                              class.labels, 
                                                              num.folds, max.instances,
                                                              output.dir, graphics.dir, 
                                                              object.name, min.instances) {
  cat('-- Random Sampling for sample limited & repeatedly...\n')
  cms.list = SimulateALForSampleRepeatedly(f=SimulateRandomSamplingForSampleLimited, 
                                           repetitions=repetitions,
                                           classifier.name=classifier.name, 
                                           weka.control=weka.control,
                                           subjects.features=subjects.features, 
                                           class.labels=class.labels, 
                                           max.instances=max.instances,
                                           num.folds=num.folds,
                                           min.instances=min.instances)
  SaveObject(obj=cms.list, var.name=object.name, dir=output.dir)
  
  DoPlotALPerformanceForSubject(obj=cms.list$cms, prefix=object.name, graphics.dir=graphics.dir, 
                                first.x=min.instances)
}
