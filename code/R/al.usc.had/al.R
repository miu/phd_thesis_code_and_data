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

source('../ml.utils.R')

B.margin = function(probs, gamma) {
  return ( exp(-gamma * max(probs)) )
}

B.margin.modulated = function(probs, gamma) {
  return ( exp(-gamma * probs) )
}

Random.modulated = function(probs, gamma) {
  return( rep(x=gamma, length.out=length(probs)) )
}

Random = function(...) {
  return( 1 )
}

SimulateRepetitions = function(num.repetitions, ...) {
  foreach (i = seq_len(num.repetitions)) %dopar% {
    source('al.R')
    SimulateOnlineALForSubject(...)
  }
}

SimulateRepetitions.budget.unif = function(num.repetitions, ...) {
  foreach (i = seq_len(num.repetitions)) %dopar% {
    source('al.R')
    SimulateOnlineALForSubject.budget.unif(...)
  }
}

SimulateRepetitions.budget.beta.unif = function(num.repetitions, ...) {
  foreach (i = seq_len(num.repetitions)) %dopar% {
    source('al.R')
    SimulateOnlineALForSubject.budget.beta.unif(...)
  }
}

SimulateRepetitions.budget.exp = function(num.repetitions, ...) {
  foreach (i = seq_len(num.repetitions)) %dopar% {
    source('al.R')
    SimulateOnlineALForSubject.budget.exp(...)
  }
}

SimulateRepetitions.budget.beta.exp = function(num.repetitions, ...) {
  foreach (i = seq_len(num.repetitions)) %dopar% {
    source('al.R')
    SimulateOnlineALForSubject.budget.beta.exp(...)
  }
}

SimulateRepetitions.seg = function(num.repetitions, ...) {
  foreach (i = seq_len(num.repetitions)) %dopar% {
    source('al.R')
    SimulateOnlineALForSubject.seg(...)
  }
}

SimulateRepetitions.generated.seg = function(num.repetitions, ...) {
  foreach (i = seq_len(num.repetitions)) %dopar% {
    source('al.R')
    SimulateOnlineALForSubject.generated.seg(...)
  }
}


SimulateForAllSubjects = function(num.repetitions, subjects.features, ...) {
  l = list()
  for (subject.features in subjects.features) {
    cat('Subject ', subject.features$number, '...\n', sep='')
    subject.data = subject.features$data
    
    class.labels = as.character(unique(sort(subject.data$activity, decreasing=F)))
    #print(class.labels)
    #l = SimulateOnlineALForSubject(subject.data=subject.data, 
    #                               class.labels=class.labels, ...)
    l[[1 + length(l)]] = SimulateRepetitions(num.repetitions=num.repetitions, 
                                             subject.data=subject.data, 
                                             class.labels=class.labels, ...)
  }
  cat('\n')
  
  return( list(data=l, subject.number=subject.features$number) )
}

SimulateForAllSubjects.unif = function(num.repetitions, subjects.features, ...) {
  l = list()
  for (subject.features in subjects.features) {
    cat('Subject ', subject.features$number, '...\n', sep='')
    subject.data = subject.features$data
    
    class.labels = as.character(unique(sort(subject.data$activity, decreasing=F)))
    #print(class.labels)
    #l = SimulateOnlineALForSubject(subject.data=subject.data, 
    #                               class.labels=class.labels, ...)
    l[[1 + length(l)]] = SimulateRepetitions.budget.unif(num.repetitions=num.repetitions, 
                                                         subject.data=subject.data, 
                                                         class.labels=class.labels, ...)
  }
  cat('\n')
  
  return( list(data=l, subject.number=subject.features$number) )
}

SimulateForAllSubjects.unif.beta = function(num.repetitions, subjects.features, ...) {
  l = list()
  for (subject.features in subjects.features) {
    cat('Subject ', subject.features$number, '...\n', sep='')
    subject.data = subject.features$data
    
    class.labels = as.character(unique(sort(subject.data$activity, decreasing=F)))
    #print(class.labels)
    #l = SimulateOnlineALForSubject(subject.data=subject.data, 
    #                               class.labels=class.labels, ...)
    
    #l[[1 + length(l)]] = SimulateRepetitions.budget.unif(num.repetitions=num.repetitions, 
    #                                                     subject.data=subject.data, 
    #                                                     class.labels=class.labels, ...)
    
    l[[1 + length(l)]] = SimulateRepetitions.budget.beta.unif(num.repetitions=num.repetitions, 
                                                              subject.data=subject.data, 
                                                              class.labels=class.labels, ...)
  }
  cat('\n')
  
  return( list(data=l, subject.number=subject.features$number) )
}

SimulateForAllSubjects.exp = function(num.repetitions, subjects.features, ...) {
  l = list()
  for (subject.features in subjects.features) {
    cat('Subject ', subject.features$number, '...\n', sep='')
    subject.data = subject.features$data
    
    class.labels = as.character(unique(sort(subject.data$activity, decreasing=F)))
    #print(class.labels)
    #l = SimulateOnlineALForSubject(subject.data=subject.data, 
    #                               class.labels=class.labels, ...)
    l[[1 + length(l)]] = SimulateRepetitions.budget.exp(num.repetitions=num.repetitions, 
                                                        subject.data=subject.data, 
                                                        class.labels=class.labels, ...)
  }
  cat('\n')
  
  return( list(data=l, subject.number=subject.features$number) )
}

SimulateForAllSubjects.exp.beta = function(num.repetitions, subjects.features, ...) {
  l = list()
  for (subject.features in subjects.features) {
    cat('Subject ', subject.features$number, '...\n', sep='')
    subject.data = subject.features$data
    
    class.labels = as.character(unique(sort(subject.data$activity, decreasing=F)))
    #print(class.labels)
    #l = SimulateOnlineALForSubject(subject.data=subject.data, 
    #                               class.labels=class.labels, ...)
    
    #l[[1 + length(l)]] = SimulateRepetitions.budget.exp(num.repetitions=num.repetitions, 
    #                                                    subject.data=subject.data, 
    #                                                    class.labels=class.labels, ...)
    
    l[[1 + length(l)]] = SimulateRepetitions.budget.beta.exp(num.repetitions=num.repetitions, 
                                                             subject.data=subject.data, 
                                                             class.labels=class.labels, ...)
  }
  cat('\n')
  
  return( list(data=l, subject.number=subject.features$number) )
}

SimulateForAllSubjects.seg = function(num.repetitions, subjects.features, ...) {
  l = list()
  for (subject.features in subjects.features) {
    cat('Subject ', subject.features$number, '...\n', sep='')
    subject.data = subject.features$data
    
    class.labels = as.character(unique(sort(subject.data$activity, decreasing=F)))
    #print(class.labels)
    #l = SimulateOnlineALForSubject(subject.data=subject.data, 
    #                               class.labels=class.labels, ...)
    l[[1 + length(l)]] = SimulateRepetitions.seg(num.repetitions=num.repetitions, 
                                                 subject.data=subject.data, 
                                                 class.labels=class.labels, ...)
  }
  cat('\n')
  
  return( list(data=l, subject.number=subject.features$number) )
}

SimulateForAllSubjects.generated.seg = function(num.repetitions, subjects.features, ...) {
  l = list()
  for (subject.features in subjects.features) {
    cat('Subject ', subject.features$number, '...\n', sep='')
    subject.data = subject.features$data
    
    class.labels = as.character(unique(sort(subject.data$activity, decreasing=F)))
    #print(class.labels)
    #l = SimulateOnlineALForSubject(subject.data=subject.data, 
    #                               class.labels=class.labels, ...)
    l[[1 + length(l)]] = SimulateRepetitions.generated.seg(num.repetitions=num.repetitions, 
                                                           subject.data=subject.data, 
                                                           class.labels=class.labels, ...)
  }
  cat('\n')
  
  return( list(data=l, subject.number=subject.features$number) )
}


SimulateForAllSubjects.amnesic = function(num.repetitions, subjects.features, ...) {
  l = list()
  
  ## TODO: foreach
  for (rep in seq_len(num.repetitions)) {
    i.subjects = sample(length(subjects.features))
    subjects.features.shuffled = subjects.features[i.subjects]    
    
    subject.data = subject.features$data
    
    class.labels = as.character(unique(sort(subject.data$activity, decreasing=F)))
    #print(class.labels)
    #l = SimulateOnlineALForSubject(subject.data=subject.data, 
    #                               class.labels=class.labels, ...)
    l[[1 + length(l)]] = SimulateRepetitions.generated.seg(num.repetitions=num.repetitions, 
                                                           subject.data=subject.data, 
                                                           class.labels=class.labels, ...)
  }
  cat('\n')
  
  return( list(data=l, subject.number=subject.features$number) )
}


SimulateOnlineALForSubject.old = function(subject.data, classifier.name, gamma,
                                          online.heuristic, 
                                          weka.control, class.labels, num.folds, 
                                          min.instances, max.instances) {  
  #s.f = FindSubjectByNumber(subjects.features=subjects.features,
  #                          subject.number=subject.number)
  
  stream = subject.data
  
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  i.row = sample(x=nrow(stream), size=min.instances)
  cat('nrow(stream) =', nrow(stream), '\n')
  cat('min.instances =', min.instances, '\n')
  
  cat('i.row: '); print(i.row)
  train.data = stream[i.row, ]
  stream = stream[-i.row, ]
  model = classifier(activity ~., train.data)
  
  #zeror.name = 'weka.classifiers.rules.ZeroR'
  #zeror.classifier = make_Weka_classifier(name=zeror.name, class=class.labels)
  #model = zeror.classifier(activity~., train.data)
  
  ## lists to track progress
  cms.selectively.myopic = list()
  cms.selectively.full = list()
  #sizes = c()
  instances = list()
  
  #cat(nrow(train.data), ' / ', max.instances, '\n', sep='')
  timestamp = 0
  timestamps = c()
  while (nrow(train.data) < max.instances) {
    timestamp = timestamp + 1
    cat(nrow(train.data), '/', max.instances, '; ', sep='')
    #for (size in nrow(init.sample):min(max.instances, n) ) {
    #cat(size, '...\n', sep='')
    #sizes = c(sizes, size)
    #window = stream[1, ]
    #stream = stream[-1, ]
    
    i.row = sample(x=nrow(stream), 1)
    
    instance = stream[i.row, ]
    
    # 'selectively ask' confusion
    predicted.prob = predict(model, instance, type='probability')
    #predicted.prob = max(predicted.prob) # take the confidence
    confusion = online.heuristic(predicted.prob, gamma)
    #cat('\tasking.probability =', confusion, '\n')
    #cat('    ', confusion.selectively, '\n')
    #confusions = c(confusions, confusion)
    
    confusion.thresh = runif(1)
    
    #ask = F
    #if (confusion > confusion.thresh) {
    #  ask = T
    #}
    ask = confusion > confusion.thresh
    
    
    # retrain model(s)
    if (ask) {
      #cat('  asking...\n\n')
      train.data = rbind(train.data, instance)
      stream = stream[-i.row, ]
      ## retrain model
      model = classifier(activity ~., train.data)
      #interrupts = c(interrupts, T)    
      timestamps = c(timestamps, timestamp)
      
      # full model evaluations
      i = length(cms.selectively.myopic) + 1
      cms.selectively.myopic[[i]] = CrossValidate(classifier.name=classifier.name, 
                                                  weka.control=weka.control, 
                                                  data=train.data, 
                                                  class.labels=class.labels, 
                                                  num.folds=num.folds)
      cms.selectively.full[[i]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                             weka.control=weka.control, 
                                                             data.subsample=train.data, 
                                                             data.complementary=stream, 
                                                             class.labels=class.labels, 
                                                             num.folds=num.folds)
    } 
  }
  
  l = list(cms.myopic=cms.selectively.myopic,
           cms.full=cms.selectively.full,
           timestamps=timestamps)
  #print(l)
  return( l )
}

SimulateOnlineALForSubject = function(subject.data, classifier.name, gamma,
                                      online.heuristic, 
                                      weka.control, class.labels, num.folds, 
                                      min.instances, max.instances) {  
  #s.f = FindSubjectByNumber(subjects.features=subjects.features,
  #                          subject.number=subject.number)
  
  stream = subject.data
  
  zeror = make_Weka_classifier(name='weka/classifiers/rules/ZeroR', class=class.labels)
  
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  ## old code
  #   i.row = sample(x=nrow(stream), size=min.instances)
  #   cat('nrow(stream) =', nrow(stream), '\n')
  #   cat('min.instances =', min.instances, '\n')
  #   
  #   cat('i.row: '); print(i.row)
  #   train.data = stream[i.row, ]
  #   stream = stream[-i.row, ]
  #   model = classifier(activity ~., train.data)
  ##################
  
  train.data = stream[0, ]
  model = zeror(activity ~., train.data)
  
  #zeror.name = 'weka.classifiers.rules.ZeroR'
  #zeror.classifier = make_Weka_classifier(name=zeror.name, class=class.labels)
  #model = zeror.classifier(activity~., train.data)
  
  ## lists to track progress
  cms.selectively.myopic = list()
  cms.selectively.full = list()
  #sizes = c()
  instances = list()
  
  #cat(nrow(train.data), ' / ', max.instances, '\n', sep='')
  timestamp = 0
  timestamps = c()
  while (nrow(train.data) < max.instances) {
    timestamp = timestamp + 1
    cat(nrow(train.data), '/', max.instances, '; ', sep='')
    #for (size in nrow(init.sample):min(max.instances, n) ) {
    #cat(size, '...\n', sep='')
    #sizes = c(sizes, size)
    #window = stream[1, ]
    #stream = stream[-1, ]
    
    i.row = sample(x=nrow(stream), 1)
    
    instance = stream[i.row, ]
    
    # 'selectively ask' confusion
    predicted.prob = predict(model, instance, type='probability')
    #predicted.prob = max(predicted.prob) # take the confidence
    confusion = online.heuristic(predicted.prob, gamma)
    #cat('\tasking.probability =', confusion, '\n')
    #cat('    ', confusion.selectively, '\n')
    #confusions = c(confusions, confusion)
    
    confusion.thresh = runif(1)
    
    #ask = F
    #if (confusion > confusion.thresh) {
    #  ask = T
    #}
    ask = confusion > confusion.thresh
    
    
    # retrain model(s)
    if (ask) {
      #cat('  asking...\n\n')
      train.data = rbind(train.data, instance)
      stream = stream[-i.row, ]
      ## retrain model
      
      if (length(unique(train.data$activity)) > 2) {
        model = classifier(activity ~., train.data)
      } else {
        model = zeror(activity ~., train.data)
      }
      #interrupts = c(interrupts, T)    
      timestamps = c(timestamps, timestamp)
      
      # full model evaluations
      i = length(cms.selectively.myopic) + 1
      cms.selectively.myopic[[i]] = CrossValidate(classifier.name=classifier.name, 
                                                  weka.control=weka.control, 
                                                  data=train.data, 
                                                  class.labels=class.labels, 
                                                  num.folds=num.folds)
      cms.selectively.full[[i]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                             weka.control=weka.control, 
                                                             data.subsample=train.data, 
                                                             data.complementary=stream, 
                                                             class.labels=class.labels, 
                                                             num.folds=num.folds)
    } 
  }
  
  l = list(cms.myopic=cms.selectively.myopic,
           cms.full=cms.selectively.full,
           timestamps=timestamps)
  #print(l)
  return( l )
}

## TODO
SimulateOnlineALForSubject.budget.unif = function(subject.data, classifier.name, gamma,
                                                  selection.function, 
                                                  weka.control, class.labels, num.folds, horizon.size,
                                                  history.size, num.gamma.iterations,
                                                  max.size, monotone.increasing) {  
  #s.f = FindSubjectByNumber(subjects.features=subjects.features,
  #                          subject.number=subject.number)
  
  
  cat('SimulateOnlineALForSubject.budget.unif...\n')
  
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  #i.row = sample(x=nrow(stream), size=min.instances)
  #cat('nrow(stream) =', nrow(stream), '\n')
  #cat('min.instances =', min.instances, '\n')
  
  #cat('i.row: '); print(i.row)
  #train.data = stream[i.row, ]
  #stream = stream[-i.row, ]
  #model = classifier(activity ~., train.data)
  
  #zeror.name = 'weka.classifiers.rules.ZeroR'
  #zeror.classifier = make_Weka_classifier(name=zeror.name, class=class.labels)
  #model = zeror.classifier(activity~., train.data)
  
  train.data = subject.data[0, ]
  
  ## assume one initial activity per label
  unique.labels = sort(unique(as.character(subject.data$activity)))
  for (unique.label in unique.labels) {
    i.row = which(unique.label == as.character(subject.data$activity))
    
    if (length(i.row) > 0) {
      i.row = sample(i.row, 1)
    }
    
    example = subject.data[i.row, ]
    subject.data = subject.data[-i.row, ]
    
    train.data = rbind(train.data, example)
  }
  
  model = classifier(activity ~ ., train.data)
  
  stream = subject.data
  
  ## lists to track progress
  cms.selectively.myopic = list()
  cms.selectively.full = list()
  #sizes = c()
  instances = list()
  gammas = c()
  
  conf.history = rep(x=NA, times=history.size)
  conf.history.i = 1
  
  #cat(nrow(train.data), ' / ', max.instances, '\n', sep='')
  timestamp = 1
  timestamps = c()
  
  num.annotations.so.far = 0
  
  while (timestamp < (history.size + horizon.size) ) {
    cat(num.annotations.so.far, '/', (timestamp - history.size), '; ', sep='')
    #for (size in nrow(init.sample):min(max.instances, n) ) {
    #cat(size, '...\n', sep='')
    #sizes = c(sizes, size)
    #window = stream[1, ]
    #stream = stream[-1, ]
    
    i.row = sample(x=nrow(stream), 1)
    
    instance = stream[i.row, ]
    
    # 'selectively ask' confusion
    predicted.prob = max(predict(model, instance, type='probability'))
    #cat('    ', confusion.selectively, '\n')
    #confusions = c(confusions, confusion)
    
    conf.history[conf.history.i] = predicted.prob
    conf.history.i = (conf.history.i + 1) %% (1 + history.size)
    cat('\t\tmean(conf.history) =', mean(conf.history), '\n')
    
    num.segments.left = horizon.size - (timestamp - history.size) + 1
    
    ## find the best gamma
    gamma = 0.5
    best.gamma = gamma
    best.diff = Inf
    best.expected = Inf
    target = max.size - num.annotations.so.far
    
    cat('\tmean.ask.prob =', mean(selection.function(probs=conf.history, gamma=gamma)), '\n')
    cat('\tmax.size =', max.size, '\n')
    cat('\tnum.annotations.so.far =', num.annotations.so.far, '\n')
    cat('\tnum.segments.left =', num.segments.left, '\n')
    cat('\ttarget =', target, '\n')
    
    
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
        p.ask.history = selection.function(probs=conf.history, gamma=gamma)
        #cat('a\n')
        
        #cat('\t\tmean(p.ask.history) =', mean(p.ask.history), '\n')
        
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
    
    gamma = best.gamma
    gammas = c(gammas, gamma)
    
    cat('\tgamma =', gamma, '\n')
    
    cat('\tbest.expected =', best.expected, '\n')
    cat('\t\tconf.history: '); print(conf.history)
    cat('\t\tmean(p.ask.history) =', mean(selection.function(conf.history, gamma)), '\n')
    cat('\t\tp.ask.history: '); print(selection.function(conf.history, gamma))
    
    
    cat('\n*** calculating asking prob:\n')
    cat(' - predicted.prob =', predicted.prob, '\n')
    cat(' - gamma =', gamma, '\n')
    confusion = selection.function(predicted.prob, gamma)
    cat(' - asking prob =', confusion, '\n')
    
    confusion.thresh = runif(1)
    
    #ask = F
    #if (confusion > confusion.thresh) {
    #  ask = T
    #}
    ask = confusion > confusion.thresh
    if (is.na(sum(conf.history))) {
      ask = F
    }
    
    # retrain model(s)
    if (ask) {
      num.annotations.so.far = num.annotations.so.far + 1
      
      #cat('  asking...\n\n')
      train.data = rbind(train.data, instance)
      stream = stream[-i.row, ]
      ## retrain model
      model = classifier(activity ~., train.data)
      #interrupts = c(interrupts, T)    
      timestamps = c(timestamps, timestamp)
      
      # full model evaluations
      i = length(cms.selectively.full) + 1
      #       cms.selectively.myopic[[i]] = CrossValidate(classifier.name=classifier.name, 
      #                                                   weka.control=weka.control, 
      #                                                   data=train.data, 
      #                                                   class.labels=class.labels, 
      #                                                   num.folds=num.folds)
      cms.selectively.full[[i]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                             weka.control=weka.control, 
                                                             data.subsample=train.data, 
                                                             data.complementary=stream, 
                                                             class.labels=class.labels, 
                                                             num.folds=num.folds)
      cat('F-Measure =', WeightedFMeasure(cm=cms.selectively.full[[i]]), '\n')
    } 
    
    timestamp = timestamp + 1
  }
  
  l = list(cms.myopic=cms.selectively.myopic,
           cms.full=cms.selectively.full,
           timestamps=timestamps,
           gammas=gammas)
  #print(l)
  return( l )
}

SimulateOnlineALForSubject.budget.beta.unif = function(subject.data, classifier.name, gamma,
                                                       selection.function, 
                                                       weka.control, class.labels, num.folds, horizon.size,
                                                       history.size, num.gamma.iterations,
                                                       max.size, monotone.increasing, beta) {  
  
  half.sigmoid = function(B, beta) {
    if (B < 0) {
      return( 0 )
    } else {
      return( 2 * (1 / (1 + exp(-1/beta * B)) - 0.5) )
    }
  }  
  
  #s.f = FindSubjectByNumber(subjects.features=subjects.features,
  #                          subject.number=subject.number)
  
  
  cat('SimulateOnlineALForSubject.budget.unif...\n')
  
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  #i.row = sample(x=nrow(stream), size=min.instances)
  #cat('nrow(stream) =', nrow(stream), '\n')
  #cat('min.instances =', min.instances, '\n')
  
  #cat('i.row: '); print(i.row)
  #train.data = stream[i.row, ]
  #stream = stream[-i.row, ]
  #model = classifier(activity ~., train.data)
  
  #zeror.name = 'weka.classifiers.rules.ZeroR'
  #zeror.classifier = make_Weka_classifier(name=zeror.name, class=class.labels)
  #model = zeror.classifier(activity~., train.data)
  
  train.data = subject.data[0, ]
  
  ## assume one initial activity per label
  unique.labels = sort(unique(as.character(subject.data$activity)))
  for (unique.label in unique.labels) {
    i.row = which(unique.label == as.character(subject.data$activity))
    
    if (length(i.row) > 0) {
      i.row = sample(i.row, 1)
    }
    
    example = subject.data[i.row, ]
    subject.data = subject.data[-i.row, ]
    
    train.data = rbind(train.data, example)
  }
  
  model = classifier(activity ~ ., train.data)
  
  stream = subject.data
  
  ## lists to track progress
  cms.selectively.myopic = list()
  cms.selectively.full = list()
  #sizes = c()
  instances = list()
  gammas = c()
  
  conf.history = rep(x=NA, times=history.size)
  conf.history.i = 1
  
  #cat(nrow(train.data), ' / ', max.instances, '\n', sep='')
  timestamp = 1
  timestamps = c()
  
  num.annotations.so.far = 0
  
  while (timestamp < (history.size + horizon.size) ) {
    cat(num.annotations.so.far, '/', (timestamp - history.size), '; ', sep='')
    #for (size in nrow(init.sample):min(max.instances, n) ) {
    #cat(size, '...\n', sep='')
    #sizes = c(sizes, size)
    #window = stream[1, ]
    #stream = stream[-1, ]
    
    i.row = sample(x=nrow(stream), 1)
    
    instance = stream[i.row, ]
    
    # 'selectively ask' confusion
    predicted.prob = max(predict(model, instance, type='probability'))
    #cat('    ', confusion.selectively, '\n')
    #confusions = c(confusions, confusion)
    
    conf.history[conf.history.i] = predicted.prob
    conf.history.i = (conf.history.i + 1) %% (1 + history.size)
    cat('\t\tmean(conf.history) =', mean(conf.history), '\n')
    
    num.segments.left = horizon.size - (timestamp - history.size) + 1
    
    ## find the best gamma
    gamma = 0.5
    best.gamma = gamma
    best.diff = Inf
    best.expected = Inf
    target = max.size - num.annotations.so.far
    
    cat('\tmean.ask.prob =', mean(selection.function(probs=conf.history, gamma=gamma)), '\n')
    cat('\tmax.size =', max.size, '\n')
    cat('\tnum.annotations.so.far =', num.annotations.so.far, '\n')
    cat('\tnum.segments.left =', num.segments.left, '\n')
    cat('\ttarget =', target, '\n')
    
    
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
        p.ask.history = selection.function(probs=conf.history, gamma=gamma)
        #cat('a\n')
        
        #cat('\t\tmean(p.ask.history) =', mean(p.ask.history), '\n')
        
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
    
    gamma = best.gamma
    gammas = c(gammas, gamma)
    
    cat('\tgamma =', gamma, '\n')
    
    cat('\tbest.expected =', best.expected, '\n')
    cat('\t\tconf.history: '); print(conf.history)
    cat('\t\tmean(p.ask.history) =', mean(selection.function(conf.history, gamma)), '\n')
    cat('\t\tp.ask.history: '); print(selection.function(conf.history, gamma))
    
    
    cat('\n*** calculating asking prob:\n')
    cat(' - predicted.prob =', predicted.prob, '\n')
    cat(' - gamma =', gamma, '\n')
    confusion = selection.function(predicted.prob, gamma)
    cat(' - confusion =', confusion, '\n')
    
    B.theoretical = max.size * ((timestamp-history.size) / horizon.size)
    B.actual = num.annotations.so.far
    B.deviation = B.theoretical - B.actual
    
    p.budget = half.sigmoid(B=abs(B.deviation), beta=beta)    
    
    if (B.deviation > 0) {
      p.ask = (1 - p.budget) * confusion + p.budget  
    } else {
      #p.ask = 1 - (1 - selection.thresh) * p.budget
      p.ask = confusion * (1 - p.budget)
    }
    
    cat('B.deviation =', B.deviation, '\n')
    cat('p.budget =', p.budget, '\n')
    cat('p.ask =', p.ask, '\n')
    
    confusion.thresh = runif(1)
    
    #ask = F
    #if (confusion > confusion.thresh) {
    #  ask = T
    #}
    ask = p.ask > confusion.thresh
    if (is.na(sum(conf.history))) {
      ask = F
    }
    
    # retrain model(s)
    if (ask) {
      num.annotations.so.far = num.annotations.so.far + 1
      
      #cat('  asking...\n\n')
      train.data = rbind(train.data, instance)
      stream = stream[-i.row, ]
      ## retrain model
      model = classifier(activity ~., train.data)
      #interrupts = c(interrupts, T)    
      timestamps = c(timestamps, timestamp)
      
      # full model evaluations
      i = length(cms.selectively.full) + 1
      #       cms.selectively.myopic[[i]] = CrossValidate(classifier.name=classifier.name, 
      #                                                   weka.control=weka.control, 
      #                                                   data=train.data, 
      #                                                   class.labels=class.labels, 
      #                                                   num.folds=num.folds)
      cms.selectively.full[[i]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                             weka.control=weka.control, 
                                                             data.subsample=train.data, 
                                                             data.complementary=stream, 
                                                             class.labels=class.labels, 
                                                             num.folds=num.folds)
      cat('F-Measure =', WeightedFMeasure(cm=cms.selectively.full[[i]]), '\n')
    } 
    
    timestamp = timestamp + 1
  }
  
  l = list(cms.myopic=cms.selectively.myopic,
           cms.full=cms.selectively.full,
           timestamps=timestamps,
           gammas=gammas)
  #print(l)
  return( l )
}

## TODO
SimulateOnlineALForSubject.budget.exp = function(subject.data, classifier.name, gamma,
                                                 selection.function, 
                                                 weka.control, class.labels, num.folds, 
                                                 max.size, horizon.size,
                                                 history.size, num.gamma.iterations,
                                                 lambda, monotone.increasing) {  
  #s.f = FindSubjectByNumber(subjects.features=subjects.features,
  #                          subject.number=subject.number)
  
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
  
  
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  #i.row = sample(x=nrow(stream), size=min.instances)
  #cat('nrow(stream) =', nrow(stream), '\n')
  #cat('min.instances =', min.instances, '\n')
  
  #cat('i.row: '); print(i.row)
  #train.data = stream[i.row, ]
  #stream = stream[-i.row, ]
  #model = classifier(activity ~., train.data)
  
  #zeror.name = 'weka.classifiers.rules.ZeroR'
  #zeror.classifier = make_Weka_classifier(name=zeror.name, class=class.labels)
  #model = zeror.classifier(activity~., train.data)
  
  train.data = subject.data[0, ]
  
  ## assume one initial activity per label
  unique.labels = sort(unique(as.character(subject.data$activity)))
  for (unique.label in unique.labels) {
    i.row = which(unique.label == as.character(subject.data$activity))
    
    if (length(i.row) > 0) {
      i.row = sample(i.row, 1)
    }
    
    example = subject.data[i.row, ]
    subject.data = subject.data[-i.row, ]
    
    train.data = rbind(train.data, example)
  }
  
  model = classifier(activity ~ ., train.data)
  
  stream = subject.data
  
  ## lists to track progress
  cms.selectively.myopic = list()
  cms.selectively.full = list()
  #sizes = c()
  instances = list()
  gammas = c()
  
  conf.history = rep(x=NA, times=history.size)
  conf.history.i = 1
  
  #cat(nrow(train.data), ' / ', max.instances, '\n', sep='')
  timestamp = 1
  timestamps = c()
  
  num.annotations.so.far = 0
  
  
  t0 = 0
  B = max.size
  
  target = B.target(lambda=lambda, B=B, t0=t0, B.spent=num.annotations.so.far)
  
  
  while (timestamp < (history.size + horizon.size) ) {
    cat(num.annotations.so.far, '/', (timestamp-history.size), '; ', sep='')
    #for (size in nrow(init.sample):min(max.instances, n) ) {
    #cat(size, '...\n', sep='')
    #sizes = c(sizes, size)
    #window = stream[1, ]
    #stream = stream[-1, ]
    
    i.row = sample(x=nrow(stream), 1)
    
    instance = stream[i.row, ]
    
    # 'selectively ask' confusion
    predicted.prob = max(predict(model, instance, type='probability'))
    #cat('    ', confusion.selectively, '\n')
    #confusions = c(confusions, confusion)
    
    conf.history[conf.history.i] = max(predicted.prob)
    conf.history.i = (conf.history.i + 1) %% (1 + history.size)
    
    num.segments.left = horizon.size - (timestamp - history.size) + 1
    
    ## find the best gamma
    gamma = 0.5
    best.gamma = gamma
    best.diff = Inf
    best.expected = Inf
    
    t0 = (timestamp-history.size) / horizon.size
    B = max.size
    
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
        p.ask.history = selection.function(probs=conf.history, gamma=gamma)
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
    
    gamma = best.gamma
    gammas = c(gammas, gamma)
    
    confusion = selection.function(predicted.prob, gamma)
    
    confusion.thresh = runif(1)
    
    #ask = F
    #if (confusion > confusion.thresh) {
    #  ask = T
    #}
    ask = confusion > confusion.thresh
    if (is.na(sum(conf.history))) {
      ask = F
    }
    
    # retrain model(s)
    if (ask) {
      #cat('  asking...\n\n')
      
      num.annotations.so.far = 1 + num.annotations.so.far
      
      t0 = ((1+timestamp) - history.size) / horizon.size
      B = max.size
      target = B.target(lambda=lambda, B=B, t0=t0, B.spent=num.annotations.so.far)   
      
      
      
      train.data = rbind(train.data, instance)
      stream = stream[-i.row, ]
      ## retrain model
      model = classifier(activity ~., train.data)
      #interrupts = c(interrupts, T)    
      timestamps = c(timestamps, timestamp)
      
      # full model evaluations
      i = length(cms.selectively.full) + 1
      #       cms.selectively.myopic[[i]] = CrossValidate(classifier.name=classifier.name, 
      #                                                   weka.control=weka.control, 
      #                                                   data=train.data, 
      #                                                   class.labels=class.labels, 
      #                                                   num.folds=num.folds)
      cms.selectively.full[[i]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                             weka.control=weka.control, 
                                                             data.subsample=train.data, 
                                                             data.complementary=stream, 
                                                             class.labels=class.labels, 
                                                             num.folds=num.folds)
      cat('F-Measure =', WeightedFMeasure(cm=cms.selectively.full[[i]]))
    } 
    
    timestamp = timestamp + 1
  }
  
  l = list(cms.myopic=cms.selectively.myopic,
           cms.full=cms.selectively.full,
           timestamps=timestamps,
           gammas=gammas)
  #print(l)
  return( l )
}

SimulateOnlineALForSubject.budget.beta.exp = function(subject.data, classifier.name, gamma,
                                                      selection.function, 
                                                      weka.control, class.labels, num.folds, 
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
  
  #s.f = FindSubjectByNumber(subjects.features=subjects.features,
  #                          subject.number=subject.number)
  
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
  
  
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  #i.row = sample(x=nrow(stream), size=min.instances)
  #cat('nrow(stream) =', nrow(stream), '\n')
  #cat('min.instances =', min.instances, '\n')
  
  #cat('i.row: '); print(i.row)
  #train.data = stream[i.row, ]
  #stream = stream[-i.row, ]
  #model = classifier(activity ~., train.data)
  
  #zeror.name = 'weka.classifiers.rules.ZeroR'
  #zeror.classifier = make_Weka_classifier(name=zeror.name, class=class.labels)
  #model = zeror.classifier(activity~., train.data)
  
  train.data = subject.data[0, ]
  
  ## assume one initial activity per label
  unique.labels = sort(unique(as.character(subject.data$activity)))
  for (unique.label in unique.labels) {
    i.row = which(unique.label == as.character(subject.data$activity))
    
    if (length(i.row) > 0) {
      i.row = sample(i.row, 1)
    }
    
    example = subject.data[i.row, ]
    subject.data = subject.data[-i.row, ]
    
    train.data = rbind(train.data, example)
  }
  
  model = classifier(activity ~ ., train.data)
  
  stream = subject.data
  
  ## lists to track progress
  cms.selectively.myopic = list()
  cms.selectively.full = list()
  #sizes = c()
  instances = list()
  gammas = c()
  
  conf.history = rep(x=NA, times=history.size)
  conf.history.i = 1
  
  #cat(nrow(train.data), ' / ', max.instances, '\n', sep='')
  timestamp = 1
  timestamps = c()
  
  num.annotations.so.far = 0
  
  
  t0 = 0
  B = max.size
  
  target = B.target(lambda=lambda, B=B, t0=t0, B.spent=num.annotations.so.far)
  
  
  while (timestamp < (history.size + horizon.size) ) {
    cat(num.annotations.so.far, '/', (timestamp-history.size), '; ', sep='')
    #for (size in nrow(init.sample):min(max.instances, n) ) {
    #cat(size, '...\n', sep='')
    #sizes = c(sizes, size)
    #window = stream[1, ]
    #stream = stream[-1, ]
    
    i.row = sample(x=nrow(stream), 1)
    
    instance = stream[i.row, ]
    
    # 'selectively ask' confusion
    predicted.prob = max(predict(model, instance, type='probability'))
    #cat('    ', confusion.selectively, '\n')
    #confusions = c(confusions, confusion)
    
    conf.history[conf.history.i] = max(predicted.prob)
    conf.history.i = (conf.history.i + 1) %% (1 + history.size)
    
    num.segments.left = horizon.size - (timestamp - history.size) + 1
    
    ## find the best gamma
    gamma = 0.5
    best.gamma = gamma
    best.diff = Inf
    best.expected = Inf
    
    t0 = (timestamp-history.size) / horizon.size
    B = max.size
    
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
        p.ask.history = selection.function(probs=conf.history, gamma=gamma)
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
    
    gamma = best.gamma
    gammas = c(gammas, gamma)
    
    confusion = selection.function(predicted.prob, gamma)
    
    t0 = ((1+timestamp) - history.size) / horizon.size
    
    B.theoretical = exp.distr.scaled(lambda=lambda, B=max.size, t0=t0)
    B.actual = num.annotations.so.far
    B.deviation = B.theoretical - B.actual
    
    p.budget = half.sigmoid(B=abs(B.deviation), beta=beta)
    
    if (B.deviation > 0) {
      p.ask = (1 - p.budget) * confusion + p.budget  
    } else {
      #p.ask = 1 - (1 - selection.thresh) * p.budget
      p.ask = confusion * (1 - p.budget)
    }
    
    cat('B.deviation =', B.deviation, '\n')
    cat('p.budget =', p.budget, '\n')
    cat('p.ask =', p.ask, '\n') 
    
    
    
    confusion.thresh = runif(1)
    
    #ask = F
    #if (confusion > confusion.thresh) {
    #  ask = T
    #}
    ask = p.ask > confusion.thresh
    if (is.na(sum(conf.history))) {
      ask = F
    }
    
    # retrain model(s)
    if (ask) {
      #cat('  asking...\n\n')
      
      num.annotations.so.far = 1 + num.annotations.so.far
      
      t0 = ((1+timestamp) - history.size) / horizon.size
      B = max.size
      target = B.target(lambda=lambda, B=B, t0=t0, B.spent=num.annotations.so.far)   
      
      
      
      train.data = rbind(train.data, instance)
      stream = stream[-i.row, ]
      ## retrain model
      model = classifier(activity ~., train.data)
      #interrupts = c(interrupts, T)    
      timestamps = c(timestamps, timestamp)
      
      # full model evaluations
      i = length(cms.selectively.full) + 1
      #       cms.selectively.myopic[[i]] = CrossValidate(classifier.name=classifier.name, 
      #                                                   weka.control=weka.control, 
      #                                                   data=train.data, 
      #                                                   class.labels=class.labels, 
      #                                                   num.folds=num.folds)
      cms.selectively.full[[i]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                             weka.control=weka.control, 
                                                             data.subsample=train.data, 
                                                             data.complementary=stream, 
                                                             class.labels=class.labels, 
                                                             num.folds=num.folds)
      cat('F-Measure =', WeightedFMeasure(cm=cms.selectively.full[[i]]))
    } 
    
    timestamp = timestamp + 1
  }
  
  l = list(cms.myopic=cms.selectively.myopic,
           cms.full=cms.selectively.full,
           timestamps=timestamps,
           gammas=gammas)
  #print(l)
  return( l )
}


SimulateOnlineALForSubject.seg = function(subject.data, classifier.name, gamma,
                                          online.heuristic, 
                                          weka.control, class.labels, num.folds, 
                                          min.instances, max.instances) {
  l.segs = SegmentData(subject.data=subject.data)
  
  
  i.seg = sample(x=length(l.segs), 1)
  seg = l.segs[[i.seg]]
  
  cms.myopic = list()
  cms.full = list()
  
  cms.seg.myopic = list()
  cms.seg.full = list()
  
  while (nrow(seg) < min.instances) {
    i.seg = sample(x=length(l.segs), 1)
    seg = l.segs[[i.seg]]
  }  
  l.segs = l.segs[-i.seg]
  
  l.train = list()
  l.train[[1]] = seg
  
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  model = classifier(formula=activity~., data=l.train[[1]], control=weka.control)
  
  timestamps = c()
  timestamp = 0
  while (length(l.segs) > 0) {
    timestamp = timestamp + 1
    cat(length(l.segs), ' ')
    i.seg = sample(x=length(l.segs), 1)
    seg = l.segs[[i.seg]]
    
    # average confidence
    avg.predicted.probs = rep(0, times=length(class.labels))
    for (i.row in seq_len(nrow(seg))) {
      row = seg[i.row, ]
      predicted.probs = predict(object=model, newdata=row, type='probability')
      avg.predicted.probs = avg.predicted.probs + predicted.probs / nrow(seg)
    }
    
    ask.prob = online.heuristic(probs=avg.predicted.probs, gamma=gamma)
    
    ask.thresh = runif(1)
    
    ask = ask.prob > ask.thresh
    if (ask) {
      l.segs = l.segs[-i.seg]
      l.train[[1 + length(l.train)]] = seg      
      
      train.data = do.call(rbind, l.train)
      
      i = length(cms.myopic) + 1
      cms.myopic[[i]] = CrossValidate(classifier.name=classifier.name, 
                                      weka.control=weka.control, 
                                      data=train.data, 
                                      class.labels=class.labels, 
                                      num.folds=num.folds)
      
      data.complementary = do.call(rbind, l.segs)
      cms.full[[i]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                 weka.control=weka.control, 
                                                 data.subsample=train.data, 
                                                 data.complementary=data.complementary, 
                                                 class.labels=class.labels, 
                                                 num.folds=num.folds)
      
      cms.seg.myopic[[i]] = CrossValidateWithSubsample.segments(classifier.name=classifier.name,
                                                                weka.control=weka.control,
                                                                l.data.subsample=l.train,
                                                                l.data.complementary=list(),
                                                                class.labels=class.labels,
                                                                num.folds=num.folds)
      
      cms.seg.full[[i]] = CrossValidateWithSubsample.segments(classifier.name=classifier.name,
                                                              weka.control=weka.control,
                                                              l.data.subsample=l.train,
                                                              l.data.complementary=l.segs,
                                                              class.labels=class.labels,
                                                              num.folds=num.folds)
      
      timestamps = c(timestamps, timestamp)
    }
  }
  
  return( list(cms.myopic=cms.myopic, cms.full=cms.full, timestamps=timestamps,
               cms.seg.myopic=cms.seg.myopic, cms.seg.full=cms.seg.full) )
}

SimulateOnlineALForSubject.generated.seg = function(subject.data, classifier.name, gamma,
                                                    online.heuristic, 
                                                    weka.control, class.labels, num.folds, 
                                                    min.instances, max.instances,
                                                    seg.size) {
  generate.segment = function(data, seg.size, class.labels) {
    class.label = sample(x=class.labels, size=1)
    i.subset = which(as.character(data$activity) == class.label)
    data.subset = data[i.subset, ]
    while (nrow(data.subset) < seg.size) {
      cat(' subset... ')
      class.label = sample(x=class.labels, size=1)
      i.subset = which(as.character(data$activity) == class.label)
      data.subset = data[i.subset, ]
    }
    #cat('\n')
    
    i.seg = sample(x=nrow(data.subset), size=seg.size)
    seg = data.subset[i.seg, ]
    
    #data = data[-i.subset, ]
    data = rbind(data[-i.subset, ], data.subset[-i.seg, ])
    
    return( list(seg=seg, data.complementary=data) )
  }
  
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  i.row = sample(x=nrow(subject.data), size=min.instances)
  cat('nrow(subject.data) =', nrow(subject.data), '\n')
  cat('min.instances =', min.instances, '\n')
  
  cat('i.row: '); print(i.row)
  train.data = subject.data[i.row, ]
  subject.data = subject.data[-i.row, ]
  model = classifier(activity ~., train.data)
  
  cms.selectively.myopic = list()
  cms.selectively.full = list()
  
  timestamp = 0
  timestamps = c()
  while (nrow(train.data) < max.instances) {
    timestamp = timestamp + 1
    cat(nrow(train.data), '/', max.instances, '; ', sep='')
    
    l.data = generate.segment(data=subject.data, seg.size=seg.size, class.labels=class.labels)
    seg = l.data$seg
    #cat('nrow(seg) =', nrow(seg), '')
    # average confidence
    avg.predicted.probs = rep(0, times=length(class.labels))
    for (i.row in seq_len(nrow(seg))) {
      row = seg[i.row, ]
      predicted.probs = predict(object=model, newdata=row, type='probability')
      avg.predicted.probs = avg.predicted.probs + predicted.probs / nrow(seg)
    }
    
    confusion = online.heuristic(avg.predicted.probs, gamma)
    
    confusion.thresh = runif(1)
    ask = confusion > confusion.thresh
    
    if (ask) {
      subject.data = l.data$data.complementary
      
      train.data = rbind(train.data, seg)
      model = classifier(activity ~., train.data)
      
      timestamps = c(timestamps, timestamp)
      
      # full model evaluations
      i = length(cms.selectively.myopic) + 1
      cms.selectively.myopic[[i]] = CrossValidate(classifier.name=classifier.name, 
                                                  weka.control=weka.control, 
                                                  data=train.data, 
                                                  class.labels=class.labels, 
                                                  num.folds=num.folds)
      cms.selectively.full[[i]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                             weka.control=weka.control, 
                                                             data.subsample=train.data, 
                                                             data.complementary=subject.data, 
                                                             class.labels=class.labels, 
                                                             num.folds=num.folds)
    }
  }
  
  return( list(cms.myopic=cms.selectively.myopic,
               cms.full=cms.selectively.full,
               timestamps=timestamps) )
}


GetPerformance = function(l, subject.index) {
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

SegmentData = function(subject.data) {
  act = as.character(subject.data$activity)
  i1 = 1: (length(act) - 1)
  i2 = i1 + 1
  
  i.seg = which(act[i1] != act[i2])
  i.seg = c(i.seg, length(act))
  
  l.segs = list()
  i.start = 1
  for (i.end in i.seg) {
    l.segs[[1 + length(l.segs)]] = subject.data[i.start:i.end, ]
    i.start = i.end + 1
  }
  
  return( l.segs )
}
