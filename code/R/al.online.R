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
source('R/file.utils.R')
source('R/labels.R')
source('R/organize.R')

Entropy = function(probs) {
  en = 0
  for(p in probs) {
    if (p == 0) {
      return( 0 )
    } 
    en = en + p * log(p)
  }
  return( en )
}

Confidence = function(probs) {
  return( max(probs) )
}

Margin = function(probs) {
  probs = sort(probs, decreasing=T)
  return( probs[1] - probs[2] )
}

B = function(probs, b) {
  p = max(probs)
  return( b/(b+p) )
}

#B.margin = function(probs, b) {
#  return ( 1 / (1 + exp(-b * max(probs))) )
#}

B.margin = function(probs, b) {
  return ( exp(-b * max(probs)) )
}

#B.margin = function(probs, b) {
#  return (1 / (1 + exp(-b * max(probs))))
#}

AverageChainsOnlineAL = function(l) {
  #cat(' ------ a\n')
  m = list()
  m$cms.always.myopic = list()
  m$cms.always.full = list()
  m$cms.selectively.myopic = list()
  m$cms.selectively.full = list()
  m$sizes = c()
  m$interrupts = c()
  m$interrupts.neg = c()
  m$confusions.always = c()
  m$confusions.selectively = c()
  
  m$rand.cms.selectively.myopic = list()
  m$rand.cms.selectively.full = list()
  
  #cat(' ------ b\n')
  n = length(l)
  #cat(' :::: n =', n)
  for(i in seq_along(l)) {
    chain = l[[i]]
    for (k in seq_along(chain$cms.always.myopic)) {
      if (length(is.na(m$cms.always.myopic)) == 0) {
        m$cms.always.mypoic[[1]] = 0
        m$cms.always.full[[1]] = 0
        m$cms.selectively.myopic[[1]] = 0
        m$cms.selectively.full[[1]] = 0
        m$rand.cms.selectively.myopic[[1]] = 0
        m$rand.cms.selectively.full[[1]] = 0
        m$sizes[1] = 0
        m$interrupts[1] = 0
        m$interrupts.neg[1] = 0
        m$confusions.always[1] = 0
        m$confusions.selectively[1] = 0
      }
      
      if (length(m$cms.always.myopic) < k) {
        m$cms.always.myopic[[k]] = 0
        m$cms.always.full[[k]] = 0
        m$cms.selectively.myopic[[k]] = 0
        m$cms.selectively.full[[k]] = 0
        m$rand.cms.selectively.myopic[[k]] = 0
        m$rand.cms.selectively.full[[k]] = 0
        m$sizes[k] = 0
        m$interrupts[k] = 0
        m$interrupts.neg[k] = 0
        m$confusions.always[k] = 0
        m$confusions.selectively[k] = 0
      }
      
      #cat(' ------ c\n')
      #cat(' === ', length(m$cms.always.myopic), length(chain$cms.always.myopic), '\n')
      m$cms.always.myopic[[k]] = m$cms.always.myopic[[k]] + chain$cms.always.myopic[[k]]
      m$cms.always.full[[k]] = m$cms.always.full[[k]] + chain$cms.always.full[[k]]
      m$cms.selectively.myopic[[k]] = m$cms.selectively.myopic[[k]] + chain$cms.selectively.myopic[[k]]
      m$cms.selectively.full[[k]] = m$cms.selectively.full[[k]] + chain$cms.selectively.full[[k]]
      
      #cat(' === ', length(m$rand.cms.selectively.myopic), length(chain$rand.cms.selectively.myopic), '\n')
      m$rand.cms.selectively.myopic[[k]] = m$rand.cms.selectively.myopic[[k]] + chain$rand.cms.selectively.myopic[[k]]
      
      #cat(' === ', length(m$rand.cms.selectively.full), length(chain$rand.cms.selectively.full), '\n')
      m$rand.cms.selectively.full[[k]] = m$rand.cms.selectively.full[[k]] + chain$rand.cms.selectively.full[[k]] 
      
      m$sizes[k] = m$sizes[k] + chain$sizes[k]
      m$interrupts[k] = m$interrupts[k] + chain$interrupts[k]
      m$interrupts.neg[k] = m$interrupts.neg[k] + chain$interrupts.neg[k]
      m$confusions.always[k] = m$confusions.always[k] + chain$confusions.always[k]
      m$confusions.selectively[k] = m$confusions.selectively[k] + chain$confusions.selectively[k]
      #cat(' ------ d\n')
    }
  }
  #cat(' ------ e\n')
  for (k in seq_along(m$cms.always.myopic)) {
    m$cms.always.myopic[[k]] = m$cms.always.myopic[[k]] / n
    m$cms.always.full[[k]] = m$cms.always.full[[k]] / n
    m$cms.selectively.myopic[[k]] = m$cms.selectively.myopic[[k]] / n
    m$cms.selectively.full[[k]] = m$cms.selectively.full[[k]] / n
  }
  #cat(' ------ f\n')
  m$sizes = m$sizes / n
  m$interrupts = m$interrupts / n
  m$interrupts.neg = m$interrupts.neg / n
  m$confusions.always = m$confusions.always / n
  m$confusions.selectively = m$confusions.selectively / n
  
  #cat(' ------ g\n')
  return(m)
}

ReorderSubjectTemporally = function(s.f.table, num.vectors.per.window) {
  #cat('ReorderSubjectTemporally\n')
  #cat(' ::: ', nrow(s.f.table), num.vectors.per.window, '\n')
  # generate tables for each activity
  num.activities = length(levels(s.f.table$activity))
  #cat(' +++ num.activities', num.activities, '\n')
  
  activity.tables = list()
  for (act in seq_len(num.activities)) {
    activity.tables[[act]] = s.f.table[s.f.table$activity == act, ]
  }  
  #cat('activity tables ok...\n')
  
  # sample initial dataset containing one point for each activity
  #init.sample = list()
  #for (act in seq_len(num.activities)) {
  #  i = sample(nrow(activity.tables[[act]]), 1)
  #  init.sample = rbind(init.sample, activity.tables[[act]][i, ])
  #  activity.tables[[act]] = activity.tables[[act]][-i, ]
  #}
  #init.sample = data.frame(init.sample)
  
  # how many windows per activity  
  #num.vectors = floor(nrow(s.f.table)-nrow(init.sample)) / num.activities
  num.vectors = floor(nrow(s.f.table)) / num.activities
  num.windows = floor(num.vectors / num.vectors.per.window)  
  
  # generate random sequence of activities
  ordered.activities = rep(seq_len(num.activities), num.windows)
  ordered.activities = sample(ordered.activities, length(ordered.activities))
  
  ordered.act.table = list()
  for (act in ordered.activities) {
    if (nrow(activity.tables[[act]]) >= num.vectors.per.window) {
      i = sample(nrow(activity.tables[[act]]), num.vectors.per.window)
      ordered.act.table = rbind(ordered.act.table, activity.tables[[act]][i, ])
      activity.tables[[act]] = activity.tables[[act]][-i, ]
    }
  }
  
  ordered.act.table.c = list()
  for (act.table in activity.tables) {
    ordered.act.table.c = rbind(ordered.act.table.c, act.table)
  }
  
  l = list()
  l$data.ordered = data.frame(ordered.act.table)
  l$data.ordered.complement = ordered.act.table.c
  
  return( l )
}

ReorderSubjectTemporally.rare = function(s.f.table, num.instances) {
  walk.table = s.f.table[s.f.table$activity==LABEL_WALK, ]
  still.table = s.f.table[s.f.table$activity==LABEL_STILL, ]
  up.table = s.f.table[s.f.table$activity==LABEL_UP, ]
  down.table = s.f.table[s.f.table$activity==LABEL_DOWN, ]
  run.table = s.f.table[s.f.table$activity==LABEL_RUN, ]
  
  rare.labels = 3
  rare.instances = 2
  
  walk.still.table = rbind(walk.table, still.table)
  walk.still.i = sample(nrow(walk.still.table), num.instances - (rare.instances*rare.labels))
  
  up.i = sample(nrow(up.table), rare.instances)
  down.i = sample(nrow(down.table), rare.instances)
  run.i = sample(nrow(run.table), rare.instances)
  
  data.ordered = rbind(walk.still.table[walk.still.i, ],
                       up.table[up.i, ],
                       down.table[down.i, ],
                       run.table[run.i, ])
  
  data.ordered.c = rbind(walk.still.table[-walk.still.i, ],
                         up.table[-up.i, ],
                         down.table[-down.i, ],
                         run.table[-run.i, ])
  
  #i = sample(nrow(data.ordered), nrow(data.ordered))
  #data.ordered = data.ordered[i, ]
  
  
  
  #cat(' *** ', nrow(data.ordered), 'instances in stream\n')
  
  l = list()
  l$data.init = list()
  l$data.ordered = data.ordered
  l$data.ordered.complement = data.ordered.c
  
  return(l)
}

ReorderSubjectTemporally.rare.2 = function(s.f.table, still.table,
                                           limit.walk, limit.still, num.instances) {
  walk.table = s.f.table[s.f.table$activity==LABEL_WALK, ]
  up.table = s.f.table[s.f.table$activity==LABEL_UP, ]
  down.table = s.f.table[s.f.table$activity==LABEL_DOWN, ]
  run.table = s.f.table[s.f.table$activity==LABEL_RUN, ]
  
  #cat(':', nrow(walk.table), nrow(up.table), nrow(down.table), nrow(run.table), '\n')
  
  rare.instances = 2
  
  #walk.i = sample(nrow(walk.table), rare.instances)
  walk.i = sample(nrow(walk.table), min(limit.walk, nrow(walk.table)) )
  #cat('length(walk.i) =', length(walk.i), '\n')
  
  up.i = sample(nrow(up.table), rare.instances)
  #cat('length(up.i) =', length(up.i), '\n')
  
  down.i = sample(nrow(down.table), rare.instances)
  #cat('length(down.i) =', length(down.i), '\n')
  
  run.i = sample(nrow(run.table), rare.instances)
  #cat('length(run.i) =', length(run.i), '\n')
  
  n = length(walk.i) + length(up.i) + length(down.i) + length(run.i)
  #cat('n =', n, '\n')
  #cat('num.instances =', num.instances, '\n')
  if (num.instances > n) {
    still.i = sample(nrow(still.table), min(nrow(still.table), limit.still,
                                            (num.instances - n)) )
  } else {
    still.i = 0
  }
  #cat('still.i\n')
  
  data.ordered = rbind(still.table[still.i, ],
                       walk.table[walk.i, ],
                       up.table[up.i, ],
                       down.table[down.i, ],
                       run.table[run.i, ])
  
  data.ordered.c = rbind(still.table[-still.i, ],
                         walk.table[-walk.i, ],
                         up.table[-up.i, ],
                         down.table[-down.i, ],
                         run.table[-run.i, ])
  
  i = sample(nrow(data.ordered), nrow(data.ordered))
  data.ordered = data.ordered[i, ]
  
  #cat(' *** ', nrow(data.ordered), 'instances in stream\n')
  
  l = list()
  l$data.init = list()
  l$data.ordered = data.ordered
  l$data.ordered.complement = data.ordered.c
  
  return(l)
}


SimulateOnlineALForSubject = function(subjects.features, subject.number, 
                                      confusion.quantifier, confusion.thresh,
                                      increasing, classifier.name, 
                                      weka.control, class.labels, num.folds, 
                                      max.instances, num.vectors.per.window) {  
  s.f = FindSubjectByNumber(subjects.features=subjects.features,
                            subject.number=subject.number)
  l = ReorderSubjectTemporally(s.f.table=s.f$data, 
                               num.vectors.per.window=num.vectors.per.window)
  
  stream = l$data.ordered
  init.sample = l$data.init
  
  classifier = make_Weka_classifier(name=classifier.name, 
                                    class=class.labels)
  
  train.always = init.sample
  train.selectively = init.sample
  
  # set of instances ignored by selective model
  ignored = stream[0, ]
  
  model.always = classifier(activity ~ ., train.always)
  model.selectively = classifier(activity ~., train.selectively)
  
  ## lists to track progress
  cms.always.myopic = list()
  cms.always.full = list()
  cms.selectively.myopic = list()
  cms.selectively.full = list()
  sizes = c()
  interrupts = c()
  confusions.always = c()
  confusions.selectively = c()
  
  n = nrow(stream)
  #while (nrow(stream > 0)) {
  for (size in nrow(init.sample):min(max.instances, n) ) {
    #cat(size, '...\n', sep='')
    sizes = c(sizes, size)
    window = stream[1, ]
    stream = stream[-1, ]
    
    # 'always ask' confusion
    predicted.always = predict(model.always, window, type='probability')
    confusion.always = confusion.quantifier(predicted.always)
    #cat('    ', confusion.always, '')
    confusions.always = c(confusions.always, confusion.always)
    
    # 'selectively ask' confusion
    predicted.selectively = predict(model.selectively, window, type='probability')
    confusion.selectively = confusion.quantifier(predicted.selectively)
    #cat('    ', confusion.selectively, '\n')
    confusions.selectively = c(confusions.selectively, confusion.selectively)
    
    ask = (increasing & (confusion.selectively < confusion.thresh)) | 
      (!increasing & (confusion.selectively > confusion.thresh))
    
    # retrain model(s)
    if (ask) {
      #cat('  asking...\n\n')
      train.selectively = rbind(train.selectively, window)
      ## retrain model
      model.selectively = classifier(activity ~., train.selectively)
      interrupts = c(interrupts, T)
    } else {
      # add instance to ignored list
      ignored = rbind(ignored, window)
      interrupts = c(interrupts, F)
    }
    train.always = rbind(train.always, window)
    model.always = classifier(activity ~ ., train.always)
    
    i = 1 + length(cms.always.myopic)
    #cat('   training set size:', size, '\n')
    # mypoic model evaluations
    cms.always.myopic[[i]] = CrossValidate(classifier.name=classifier.name, 
                                           weka.control=weka.control, 
                                           data=train.always, 
                                           class.labels=class.labels, 
                                           num.folds=num.folds)
    cms.always.full[[i]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                      weka.control=weka.control, 
                                                      data.subsample=train.always, 
                                                      data.complementary=stream, 
                                                      class.labels=class.labels, 
                                                      num.folds=num.folds)
    
    # full model evaluations
    cms.selectively.myopic[[i]] = CrossValidate(classifier.name=classifier.name, 
                                                weka.control=weka.control, 
                                                data=train.selectively, 
                                                class.labels=class.labels, 
                                                num.folds=num.folds)
    cms.selectively.full[[i]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                           weka.control=weka.control, 
                                                           data.subsample=train.selectively, 
                                                           data.complementary=rbind(stream, ignored), 
                                                           class.labels=class.labels, 
                                                           num.folds=num.folds)
  }
  
  l = list(cms.always.myopic=cms.always.myopic,
           cms.always.full=cms.always.full,
           confusions.always=confusions.always,
           cms.selectively.myopic=cms.selectively.myopic,
           cms.selectively.full=cms.selectively.full,
           confusions.selectively=confusions.selectively,
           sizes=sizes, 
           interrupts=interrupts, 
           subject.number=subject.number)
  return( l )
}

SimulateOnlineALForSubject.neg = function(subjects.features, subject.number, 
                                          confusion.quantifier, confusion.thresh, 
                                          increasing, classifier.name, 
                                          weka.control, class.labels, num.folds, 
                                          max.instances, num.vectors.per.window) {  
  s.f = FindSubjectByNumber(subjects.features=subjects.features,
                            subject.number=subject.number)
  l = ReorderSubjectTemporally(s.f.table=s.f$data, 
                               num.vectors.per.window=num.vectors.per.window)
  
  stream = l$data.ordered
  init.sample = l$data.init
  
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  train.always = init.sample
  train.selectively = init.sample
  
  # set of instances ignored by selective model
  ignored = stream[0, ]
  
  # set of correctly classified instances
  correct = stream[0, ]
  
  model.always = classifier(activity ~ ., train.always)
  model.selectively = classifier(activity ~., train.selectively)
  
  ## lists to track progress
  cms.always.myopic = list()
  cms.always.full = list()
  cms.selectively.myopic = list()
  cms.selectively.full = list()
  sizes = c()
  interrupts = c()
  interrupts.neg = c()
  confusions.always = c()
  confusions.selectively = c()
  
  n = nrow(stream)
  #while (nrow(stream > 0)) {
  for (size in nrow(init.sample):min(max.instances, n) ) {
    #cat(size, '...\n', sep='')
    sizes = c(sizes, size)
    window = stream[1, ]
    stream = stream[-1, ]
    
    # 'always ask' confusion
    predicted.always = predict(model.always, window, type='probability')
    c.always = confusion.quantifier(predicted.always)
    #cat('    ', confusion.always, '')
    confusions.always = c(confusions.always, c.always)
    
    # 'selectively ask' confusion
    predicted.selectively = predict(model.selectively, window, type='probability')
    c.selectively = confusion.quantifier(predicted.selectively)
    #cat('    ', confusion.selectively, '\n')
    confusions.selectively = c(confusions.selectively, c.selectively)
    
    ask = (increasing & (c.selectively < confusion.thresh)) | 
      (!increasing & (c.selectively > confusion.thresh))
    
    # retrain model(s)
    if (ask) {
      actual.label = window$activity
      predicted.label = class.labels[which(max(predicted.selectively) == predicted.selectively)]
      
      if ( ((length(predicted.label) == 1) & actual.label != predicted.label) | 
             (length(predicted.label) > 1) ) {
        # update model only on negative feedback or on tie with another prediction
        train.selectively = rbind(train.selectively, window)
        interrupts.neg = c(interrupts.neg, T)
        ## retrain model
        model.selectively = classifier(activity ~., train.selectively)
      } else {
        correct = rbind(correct, window)
        interrupts.neg = c(interrupts.neg, F)
      }
      
      interrupts = c(interrupts, T)
    } else {
      # add instance to ignored list
      ignored = rbind(ignored, window)
      interrupts = c(interrupts, F)
      interrupts.neg = c(interrupts.neg, F)
    }
    
    # update 'always ask' model
    train.always = rbind(train.always, window)
    model.always = classifier(activity ~ ., train.always)
    
    i = 1 + length(cms.always.myopic)
    #cat('   training set size:', size, '\n')
    # mypoic model evaluations
    cms.always.myopic[[i]] = CrossValidate(classifier.name=classifier.name, 
                                           weka.control=weka.control, 
                                           data=train.always, 
                                           class.labels=class.labels, 
                                           num.folds=num.folds)
    cms.always.full[[i]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                      weka.control=weka.control, 
                                                      data.subsample=train.always, 
                                                      data.complementary=stream, 
                                                      class.labels=class.labels, 
                                                      num.folds=num.folds)
    
    # full model evaluations
    cms.selectively.myopic[[i]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                             weka.control=weka.control, 
                                                             data.subsample=train.selectively, 
                                                             data.complementary=correct,
                                                             class.labels=class.labels, 
                                                             num.folds=num.folds)
    cms.selectively.full[[i]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                           weka.control=weka.control, 
                                                           data.subsample=train.selectively, 
                                                           data.complementary=rbind(stream, ignored, correct), 
                                                           class.labels=class.labels, 
                                                           num.folds=num.folds)
  }
  
  l = list(cms.always.myopic=cms.always.myopic,
           cms.always.full=cms.always.full,
           confusions.always=confusions.always,
           cms.selectively.myopic=cms.selectively.myopic,
           cms.selectively.full=cms.selectively.full,
           confusions.selectively=confusions.selectively,
           sizes=sizes, 
           interrupts=interrupts,
           interrupts.neg=interrupts.neg,
           subject.number=subject.number)
  return( l )
}

SimulateOnlineALForSubject.neg.b = function(subjects.features, subject.number, b, gamma,
                                            confusion.quantifier, classifier.name, 
                                            weka.control, class.labels, num.folds, 
                                            min.instances,
                                            max.instances, num.vectors.per.window) {  
  s.f = FindSubjectByNumber(subjects.features=subjects.features,
                            subject.number=subject.number)
  
  #cat( ' +++ 1\n')
  s.f = BalanceLabelsForOneSubject(s.f=s.f)
  cat(' -- subject', subject.number, nrow(s.f$data), '\n')
  l = ReorderSubjectTemporally(s.f.table=s.f$data, 
                               num.vectors.per.window=num.vectors.per.window)
  #cat(' --    reordered\n')
  
  #still.table = s.f$data[s.f$data$activity == LABEL_STILL, ]
  #other.subjects = ExcludeSubjectByNumber(subjects.features=subjects.features, 
  #                                        subject.number=subject.number)
  #for(s in other.subjects) {
  #  still.table = rbind(still.table, s$data[s$data$activity == LABEL_STILL, ])
  #}
  #
  #s.f = BalanceLabelsForOneSubject(s.f=s.f)
  #l = ReorderSubjectTemporally.rare.2(s.f.table=s.f$data, still.table=still.table,
  #                                    num.instances=max.instances,
  #                                    limit.walk=500, limit.still=500)
  
  stream = l$data.ordered
  stream.c = l$data.ordered.complement
  
  #cat(' ::: ', nrow(stream), nrow(stream.c), '\n')
  
  original.stream = l$data.ordered
  
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  ## first two samples gathered automatically
  init.i = seq_len(min.instances)
  train.always = stream[init.i, ]
  train.selectively = stream[init.i, ]
  stream = stream[-init.i, ]
  
  ## one sample of each class
  #init.i = seq_along(l$data.init)
  #train.always = l$data.init
  #train.selectively = l$data.init
  #print(l$data.init)
  #stop()
  
  # set of instances ignored by selective model
  ignored = stream[0, ]
  
  # set of correctly classified instances
  correct = stream[0, ]
  
  model.always = classifier(activity ~ ., train.always)
  model.selectively = classifier(activity ~., train.selectively)
  
  ## lists to track progress
  cms.always.myopic = list()
  cms.always.full = list()
  cms.selectively.myopic = list()
  cms.selectively.full = list()
  sizes = c()
  interrupts = c()
  interrupts.neg = c()
  confusions.always = c()
  confusions.selectively = c()
  
  n = nrow(stream) + length(init.i)
  #while (nrow(stream > 0)) {
  stream.sizes = (1+length(init.i)):min(max.instances, n)
  original.stream = l$data.ordered[seq_len(min(max.instances, n)), ]
  complement.stream = l$data.ordered[-seq_len(min(max.instances, n)), ]
  #cat(' length of stream in AL:', nrow(original.stream), '\n')
  
  #cat(' *** ', 2 + length(stream.sizes), 'instances played back\n')
  #cat('     nrow(stream) =', nrow(stream), '\n')
  #cat('     max.instances =', max.instances, '\n')
  #cat('     (1+length(init.i)) =', (1+length(init.i)), '\n')
  #cat('     max(stream.sizes) =', max(stream.sizes), '\n')
  for (size in stream.sizes) {
    #cat(size, '...\n', sep='')
    sizes = c(sizes, size)
    window = stream[1, ]
    stream = stream[-1, ]
    
    # 'always ask' confusion
    predicted.always = predict(model.always, window, type='probability')
    #c.always = confusion.quantifier(predicted.always, b)
    c.always = max(predicted.always)
    #cat('    ', confusion.always, '')
    confusions.always = c(confusions.always, c.always)
    
    # 'selectively ask' confusion
    predicted.selectively = predict(model.selectively, window, type='probability')
    
    c.selectively = confusion.quantifier(predicted.selectively, b)
    #c.selectively1 = B(predicted.selectively, b)
    #c.selectively2 = B.margin(predicted.selectively, gamma)
    
    conf.selectively = max(predicted.selectively)
    #cat('    ', confusion.selectively, '\n')
    confusions.selectively = c(confusions.selectively, conf.selectively)
    
    #ask.conf = (runif(1) < c.selectively1) | (runif(1) < c.selectively2)
    ask.conf = runif(1) < c.selectively
    
    #ask = (increasing & (c.selectively < confusion.thresh)) | 
    #  (!increasing & (c.selectively > confusion.thresh))
    
    # retrain model(s)
    if (ask.conf) {
      actual.label = window$activity
      predicted.label = class.labels[which(max(predicted.selectively) == predicted.selectively)]
      
      #if ( ((length(predicted.label) == 1) & actual.label != predicted.label) | 
      #       (length(predicted.label) > 1) ) {
      #  # update model only on negative feedback or on tie with another prediction
      train.selectively = rbind(train.selectively, window)
      interrupts.neg = c(interrupts.neg, T)
      #  ## retrain model
      model.selectively = classifier(activity ~., train.selectively)
      #} else {
      #  correct = rbind(correct, window)
      #  interrupts.neg = c(interrupts.neg, F)
      #}
      
      interrupts = c(interrupts, T)
    } else {
      # add instance to ignored list
      ignored = rbind(ignored, window)
      interrupts = c(interrupts, F)
      interrupts.neg = c(interrupts.neg, F)
    }
    
    # update 'always ask' model
    train.always = rbind(train.always, window)
    model.always = classifier(activity ~ ., train.always)
    
    i = 1 + length(cms.always.myopic)
    #cat('   training set size:', size, '\n')
    # mypoic model evaluations
    cms.always.myopic[[i]] = CrossValidate(classifier.name=classifier.name, 
                                           weka.control=weka.control, 
                                           data=train.always, 
                                           class.labels=class.labels, 
                                           num.folds=num.folds)
    cms.always.full[[i]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                      weka.control=weka.control, 
                                                      data.subsample=train.always, 
                                                      data.complementary=rbind(stream, stream.c), 
                                                      class.labels=class.labels, 
                                                      num.folds=num.folds)
    
    # full model evaluations
    cms.selectively.myopic[[i]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                             weka.control=weka.control, 
                                                             data.subsample=train.selectively, 
                                                             data.complementary=correct,
                                                             class.labels=class.labels, 
                                                             num.folds=num.folds)
    cms.selectively.full[[i]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                           weka.control=weka.control, 
                                                           data.subsample=train.selectively, 
                                                           data.complementary=rbind(stream, stream.c, ignored, correct), 
                                                           class.labels=class.labels, 
                                                           num.folds=num.folds)
  }
  
  l = list(cms.always.myopic=cms.always.myopic,
           cms.always.full=cms.always.full,
           confusions.always=confusions.always,
           cms.selectively.myopic=cms.selectively.myopic,
           cms.selectively.full=cms.selectively.full,
           confusions.selectively=confusions.selectively,
           sizes=sizes, 
           interrupts=interrupts,
           interrupts.neg=interrupts.neg,
           subject.number=subject.number,
           stream=original.stream,
           stream.complement=rbind(complement.stream, stream.c))
  return( l )
}

SimulateOnlineRSForSubject.neg = function(stream, stream.complement, min.instances,
                                          num.interruptions, classifier.name, 
                                          weka.control, class.labels, num.folds) {
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  #cat(' length of stream in RS:', nrow(stream), '\n\n')
  
  i = seq_len(min.instances)
  train.always = stream[i, ]
  train.selectively = stream[i, ]
  stream = stream[-i, ]
  
  k.int = sort(sample(nrow(stream), num.interruptions))
  
  # set of instances ignored by selective model
  ignored = stream[0, ]
  
  # set of correctly classified instances
  correct = stream[0, ]
  
  model.selectively = classifier(activity ~., train.selectively)
  
  ## lists to track progress
  cms.selectively.myopic = list()
  cms.selectively.full = list()
  
  #while (nrow(stream > 0)) {
  for (k in seq_len(nrow(stream))) {
    #cat(size, '...\n', sep='')
    window = stream[1, ]
    stream = stream[-1, ]
    
    # 'selectively ask' confusion
    predicted.selectively = predict(model.selectively, window, type='probability')
    
    ask = k %in% k.int
    
    # retrain model(s)
    if (ask) {
      actual.label = window$activity
      predicted.label = class.labels[which(max(predicted.selectively) == predicted.selectively)]
      
      #if ( ((length(predicted.label) == 1) & actual.label != predicted.label) | 
      #       (length(predicted.label) > 1) ) {
      #  # update model only on negative feedback or on tie with another prediction
      train.selectively = rbind(train.selectively, window)
      ## retrain model
      model.selectively = classifier(activity ~., train.selectively)
      #} else {
      #  correct = rbind(correct, window)
      #}
    } else {
      # add instance to ignored list
      ignored = rbind(ignored, window)
    }
    
    i = 1 + length(cms.selectively.myopic)
    #cat('   training set size:', size, '\n')
    
    cms.selectively.myopic[[k]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                             weka.control=weka.control, 
                                                             data.subsample=train.selectively, 
                                                             data.complementary=correct,
                                                             class.labels=class.labels, 
                                                             num.folds=num.folds)
    cms.selectively.full[[k]] = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                                           weka.control=weka.control, 
                                                           data.subsample=train.selectively, 
                                                           data.complementary=rbind(stream, ignored, 
                                                                                    correct, stream.complement), 
                                                           class.labels=class.labels, 
                                                           num.folds=num.folds)
  }
  
  l = list(cms.selectively.myopic=cms.selectively.myopic,
           cms.selectively.full=cms.selectively.full,
           subject.number=subject.number)
  return( l )
}

SimulateOnlineALForAllSubjects = function(subjects.features, gamma, 
                                          confusion.quantifier, confusion.thresh, 
                                          increasing, classifier.name, 
                                          weka.control, class.labels, num.folds,
                                          max.instances, num.vectors.per.window, 
                                          graphics.root, output.root) {  
  l = foreach(s.f = subjects.features, .packages='RWeka') %dopar% {
    source('R/al.online.R')
    #l = SimulateOnlineALForSubject(subjects.features=subjects.features, 
    #l = SimulateOnlineALForSubject.neg(subjects.features=subjects.features, 
    l = SimulateOnlineALForSubject.neg.b(subjects.features=subjects.features, b=b, gamma=gamma, 
                                         subject.number=subject.number, 
                                         confusion.quantifier=confusion.quantifier, 
                                         confusion.thresh=confusion.thresh, 
                                         increasing=increasing, 
                                         classifier.name=classifier.name, 
                                         weka.control=weka.control, 
                                         class.labels=class.labels, 
                                         num.folds=num.folds,
                                         max.instances=max.instances, 
                                         num.vectors.per.window=num.vectors.per.window)
    subj.folder = paste('subj', s.f$number, sep='.')
    
    graphics.dir = paste(graphics.root, subj.folder, sep='/')
    PlotAll(l=l, graphics.dir=graphics.dir)
    
    output.dir = paste(output.root, subj.folder, sep='/')
    SaveObject(obj=l, var.name='al.online.subject', dir=output.dir)
    
    l
  }
  SaveObject(obj=l, var.name='al.online.all', dir=output.root)
}

SimulateOnlineALForAllSubjects.b = function(subjects.features, b, gamma, 
                                            confusion.quantifier, classifier.name, 
                                            weka.control, class.labels, num.folds,
                                            max.instances, num.vectors.per.window, 
                                            graphics.root, output.root) {  
  l = foreach(s.f = subjects.features, .packages='RWeka') %dopar% {
    source('R/al.online.R')
    #l = SimulateOnlineALForSubject(subjects.features=subjects.features, 
    #l = SimulateOnlineALForSubject.neg(subjects.features=subjects.features, 
    l = SimulateOnlineALForSubject.neg.b(subjects.features=subjects.features, b=b, gamma=gamma, , 
                                         subject.number=subject.number, 
                                         confusion.quantifier=confusion.quantifier, 
                                         confusion.thresh=confusion.thresh, 
                                         increasing=increasing, 
                                         classifier.name=classifier.name, 
                                         weka.control=weka.control, 
                                         class.labels=class.labels, 
                                         num.folds=num.folds,
                                         max.instances=max.instances, 
                                         num.vectors.per.window=num.vectors.per.window)
    subj.folder = paste('subj', s.f$number, sep='.')
    
    graphics.dir = paste(graphics.root, subj.folder, sep='/')
    PlotAll(l=l, graphics.dir=graphics.dir)
    
    output.dir = paste(output.root, subj.folder, sep='/')
    SaveObject(obj=l, var.name='al.online.subject', dir=output.dir)
    
    l
  }
  SaveObject(obj=l, var.name='al.online.all', dir=output.root)
}

SimulateOnlineALForAllSubjects.repeatedly.b = function(subjects.features, b, gamma, 
                                                       min.instances,
                                                       repetitions, confusion.quantifier, 
                                                       classifier.name, weka.control, 
                                                       class.labels, num.folds,
                                                       max.instances, num.vectors.per.window, 
                                                       graphics.root, output.root) {
  
  l = foreach(s.f = subjects.features, .packages='RWeka') %dopar% {
    subject.number = s.f$number
    m = foreach (r = seq_len(repetitions)) %do% {
      source('R/al.online.R')
      #cat(' --- 1\n')
      chain = SimulateOnlineALForSubject.neg.b(subjects.features=subjects.features, b=b, gamma=gamma, 
                                               min.instances=min.instances,
                                               subject.number=subject.number, 
                                               confusion.quantifier=confusion.quantifier,
                                               classifier.name=classifier.name, 
                                               weka.control=weka.control, 
                                               class.labels=class.labels, 
                                               num.folds=num.folds,
                                               max.instances=max.instances, 
                                               num.vectors.per.window=num.vectors.per.window)
      stream = chain$stream
      stream.c = chain$stream.complement
      num.interruptions = sum(chain$interrupts)
      #cat(' --- 2\n')
      l.rand = SimulateOnlineRSForSubject.neg(stream=stream, stream.complement=stream.c, 
                                              min.instances,
                                              num.interruptions=num.interruptions, 
                                              classifier.name=classifier.name, 
                                              weka.control=weka.control, 
                                              class.labels=class.labels, num.folds=num.folds)
      #cat(' --- 3\n')
      chain$rand.cms.selectively.myopic = l.rand$cms.selectively.myopic
      chain$rand.cms.selectively.full = l.rand$cms.selectively.full
      
      #cat(' !! ', length(chain$cms.selectively.myopic), length(chain$rand.cms.selectively.full), '\n')
      #stop()
      
      chain
    }
    #cat(' ---- 4\n')
    m = AverageChainsOnlineAL(m)
    #cat(' ---- 5\n')
    subj.folder = paste('subj', s.f$number, sep='.')
    
    graphics.dir = paste(graphics.root, subj.folder, sep='/')
    PlotAll(l=m, graphics.dir=graphics.dir)
    
    output.dir = paste(output.root, subj.folder, sep='/')
    SaveObject(obj=m, var.name='al.online.subject', dir=output.dir)
    
    m
  }
  #cat(' ----- 6\n')
  
  SaveObject(obj=l, var.name='al.online.all', dir=output.root)
}

## TODO
GetThresholdForNumQuestions = function(num.questions, subject.number, pool.al.output.dir) {
  
}

GetPerformanceXY = function(cms, sizes, PerformanceFunction, label) {
  x = sizes
  y = sapply(X=cms, FUN=PerformanceFunction, label=label)
  
  return( list(x=x, y=y) )
}

GetConfusionXY = function(confusions, sizes) {
  return( list(x=sizes, y=confusions) )
}

GetInterruptsXY = function(interrupts, sizes) {
  questions = c()
  q = 0
  for (i in seq_along(interrupts)) {
    q = q + interrupts[i]
    questions[i] = q
  }
  return( list(x=sizes, y=questions) )
}

PlotAll = function(l, graphics.dir) {
  ## accuracy
  SavePlotBegin(dir=graphics.dir, file.name='accuracy')
  
  lst = GetPerformanceXY(cms=l$cms.always.myopic, sizes=l$sizes, 
                         PerformanceFunction=Accuracy, label=NULL)
  StartOnlineALPlot(l=lst, ylim=c(0, 1))
  
  lst = GetPerformanceXY(cms=l$cms.always.full, sizes=l$sizes, 
                         PerformanceFunction=Accuracy, label=NULL)
  ContinueOnlineALPlot(l=lst, col='red')
  
  lst = GetPerformanceXY(cms=l$cms.selectively.full, sizes=l$sizes, 
                         PerformanceFunction=Accuracy, label=NULL)
  ContinueOnlineALPlot(l=lst, col='blue')
  
  lst = GetPerformanceXY(cms=l$cms.selectively.myopic, sizes=l$sizes, 
                         PerformanceFunction=Accuracy, label=NULL)
  ContinueOnlineALPlot(l=lst, col='green')
  
  lst = GetPerformanceXY(cms=l$rand.cms.selectively.full, sizes=l$sizes,
                         PerformanceFunction=Accuracy, label=NULL)
  #cat(length(lst$x), length(lst$y), '\n')
  ContinueOnlineALPlot(l=lst, col='orange')
  
  lst = GetPerformanceXY(cms=l$rand.cms.selectively.myopic, sizes=l$sizes,
                         PerformanceFunction=Accuracy, label=NULL)
  #cat(length(lst$x), length(lst$y), '\n\n')
  ContinueOnlineALPlot(l=lst, col='orange', lty=3)
  
  legend(legend=c('always.myopic', 'always.full', 
                  'selectively.full', 'selectively.myopic',
                  'random.full', 'random.myopic'),
         x='bottomright', 
         col=c('black', 'red', 'blue', 'green', 'orange', 'orange'), 
         lty=c(1, 1, 1, 1, 1, 3))
  
  SavePlotEnd()
  
  ## confusion metric
  SavePlotBegin(dir=graphics.dir, file.name='confusion')
  
  lst.1 = GetConfusionXY(confusions=l$confusions.always, sizes=l$sizes)
  lst.2 = GetConfusionXY(confusions=l$confusions.selectively, sizes=l$sizes)
  #ylim = c(0, max(lst.1$y, lst.2$y))
  ylim = c(0, 1)
  
  StartOnlineALPlot(l=lst.1, ylim=ylim)
  ContinueOnlineALPlot(l=lst.2, col='red')
  legend(legend=c('always', 'selectively'), col=c('black', 'red'),
         x='bottomright', lty=1)
  
  SavePlotEnd()
  
  ## distribution of interruptions
  SavePlotBegin(dir=graphics.dir, file.name='interruptions')
  
  lst = GetInterruptsXY(interrupts=l$interrupts, sizes=l$sizes)
  StartOnlineALPlot(l=lst)  
  
  SavePlotEnd()
  
  ## distribution of negative feedback interruptions
  SavePlotBegin(dir=graphics.dir, file.name='interruptions.negative')
  
  lst = GetInterruptsXY(interrupts=l$interrupts.neg, sizes=l$sizes)
  StartOnlineALPlot(l=lst)  
  
  SavePlotEnd()
}

StartOnlineALPlot = function(l, ...) {
  plot(x=l$x, y=l$y, type='l', ...)
}

ContinueOnlineALPlot = function(l, ...) {
  lines(x=l$x, y=l$y, ...)
}

RunSimulateOnlineALForSubject = function(subjects.features, subject.number, 
                                         confusion.quantifier, confusion.thresh, 
                                         increasing, classifier.name, 
                                         weka.control, class.labels, num.folds) {
  l = SimulateOnlineALForSubject(subjects.features=subjects.features, 
                                 subject.number=subject.number, 
                                 confusion.quantifier=confusion.quantifier, 
                                 confusion.thresh=confusion.thresh, 
                                 increasing=increasing, classifier.name=classifier.name, 
                                 weka.control=weka.control, class.labels=class.labels, 
                                 num.folds=num.folds)
}
