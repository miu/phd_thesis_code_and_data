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
library(doRedis)

source('R/file.utils.R')
source('R/ml.utils.R')
source('R/index.sampling.R')
source('R/hmm.utils.R')

ApplyPCA = function(new.data, pca.m, min.sdev.ratio=0.95) {
  if (!is.null(pca.m)) {
    act.col = new.data$activity
    new.data = new.data[, -which(names(new.data) == 'activity')]
    
    pca.data = as.matrix(new.data)
    pca.center = matrix(t(as.matrix(pca.m$center)), nrow=nrow(pca.data), ncol=ncol(pca.data), byrow=T)
    pca.scale = matrix(t(as.matrix(pca.m$scale)), nrow=nrow(pca.data), ncol=ncol(pca.data), byrow=T)
    pca.data.scaled = (pca.data - pca.center) / pca.scale
    pca.rot = GetRotationMatrix(pca.m=pca.m, min.sdev.ratio=min.sdev.ratio)
    pca.data.rotated = data.frame(pca.data.scaled %*% pca.rot)
    
    return( cbind(pca.data.rotated, act.col) )  
  } else {
    return( new.data )
  }
}

ComputePCAModel = function(new.data) {
  new.data = new.data[, -which(names(new.data) == 'activity')]
  pca.data = as.matrix(new.data)
  pca.m = prcomp(formula=~., scale.=T, data=pca.data)
  
  return( pca.m )
}

GetRotationMatrix = function(pca.m, min.sdev.ratio=0.95) {
  i = length(pca.m$sdev)
  sum.sdev = sum(pca.m$sdev)
  while (i > 0) {
    this.sdev = sum(pca.m$sdev[seq_len(i)])
    this.ratio = this.sdev / sum.sdev
    if (this.ratio < min.sdev.ratio) {
      break
    }
    i = i - 1
  }
  valid.i = seq_len(i + 1)
  
  return( pca.m$rotation[, valid.i] )
}

SimulateOpportunityAL.pca = function(subject.data, Nb, classifier.name, 
                                     weka.control, min.train.size, 
                                     class.labels, num.folds, recall.size,
                                     max.episode.length,
                                     min.episode.length,
                                     online.confusion.metric,
                                     offline.confusion.metric,
                                     min.sdev.ratio,
                                     ...) {
  ## not tested yet
  zeror.name = 'weka.classifiers.rules.ZeroR'
  zeror = make_Weka_classifier(name=zeror.name, class=class.labels)
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  activity.stream = subject.data[0, ]
  
  recall.buffer = list()
  recall.buffer$w = subject.data[0, ]
  recall.buffer$timestamp = c()
  recall.buffer$predicted = list()
  recall.buffer$metric = c()
  recall.buffer$pca.m = NULL
  
  ann.set.unaltered = subject.data[0, ]
  model = zeror(formula=activity~., data=ann.set.unaltered)
  
  question.i = c()
  
  cms = list()
  timestamp = 1
  
  while (nrow(ann.set.unaltered) < Nb) {
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
    
    w.act.stream.projected = ApplyPCA(new.data=w.act.stream, 
                                      pca.m=recall.buffer$pca.m, 
                                      min.sdev.ratio=min.sdev.ratio)
    
    recall.buffer$w = rbind(recall.buffer$w, w.act.stream) # unprojected instance
    recall.buffer$timestamp = c(recall.buffer$timestamp, timestamp)
    predicted = predict(model, newdata=w.act.stream.projected, type='probability')
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
      
      ann.set.unaltered = rbind(ann.set.unaltered, min.conf.w)
      cat('  added to training set. New size =', nrow(ann.set.unaltered), '\n')
      
      # re-projecting training set
      ann.set.act.col = ann.set.unaltered$activity
      recall.buffer$pca.m = ComputePCAModel(new.data=ann.set.unaltered)
      ann.set.projected = cbind(data.frame(pca.m$x), ann.set.act.col)
      
      if (nrow(ann.set.projected) < min.train.size) {
        cls.name = zeror.name
        cls = zeror
        control = Weka_control()
        cat('   choosing ZeroR (', nrow(ann.set.unaltered), ',', min.train.size, ')\n')
      } else {
        cls.name = classifier.name
        cls = classifier
        control = weka.control
        cat('   choosing', classifier.name, '\n')
      }
      
      model = cls(formula=activity~., data=ann.set.projected, control=control)      
      
      # update recall.buffer estimated probabilities and offline metrics
      for (i in seq_along(recall.buffer$timestamp)) {
        newdata = ApplyPCA(new.data=recall.buffer$w[i, ],
                           pca.m=recall.buffer$pca.m, 
                           min.sdev.ratio=min.sdev.ratio)
        p = predict(model, newdata=newdata, type='probability')
        recall.buffer$predicted[[i]] = p
        recall.buffer$metric[i] = offline.confusion.metric(p)
      }
      cat('   updated estimates in the recall buffer\n')
      
      question.i = c(question.i, timestamp)
      
      ### TODO
      ### project data.compl
      
      data.compl = rbind(subject.data, recall.buffer$w, activity.stream)
      
      cat('  computing confusion matrix...\n')
      conf.matrix = CrossValidateWithSubsample(classifier.name=cls.name, 
                                               weka.control=weka.control, 
                                               data.subsample=ann.set.projected, 
                                               data.complementary=data.compl, 
                                               class.labels=class.labels, 
                                               num.folds=num.folds)
      
      cms[[1 + length(cms)]] = conf.matrix
      cat('  added cms to list\n\n')
    }
    
    timestamp = 1 + timestamp
  }
  
  return( list(cms=cms, question.i=question.i, 
               x.max=question.i[length(question.i)]) )
}

SimultateOpportunityAL.hmm = function(subject.data, Nb, class.labels, 
                                      num.folds, 
                                      online.confusion.metric,
                                      offline.confusion.metric,
                                      hmm.nStates, hmm.dis, output.dir,
                                      ...) {
  cat(' Initializing hmm... ')
  hmm.model = InitHMM(class.labels=class.labels)
  cat('done\n')
  cat(' classes: ')
  print(class.labels)
  
  ann.set.size = 0
  
  #question.i = c()
  #cms = list()
  timestamp = 1
  
  while (ann.set.size < Nb) {
    cat('timestamp =', timestamp, '\n')
    next.gesture.i = sample(x=length(subject.data), size=1)
    next.gesture = subject.data[[next.gesture.i]]
    subject.data = subject.data[-next.gesture.i]
    
    #print(next.gesture)
    #cat(' next.gesture.i =', next.gesture.i, '\n')
    
    gesture.pred.prob = PredictHMM.probability(segment=next.gesture, 
                                               model=hmm.model)
    metric = offline.confusion.metric(gesture.pred.prob)
    
    ask.prob = online.confusion.metric(metric, ...)
    
    r = runif(1)
    ask = r < ask.prob
    cat('  metric =', metric, '\n')
    cat('  ask.prob =', ask.prob, '\n')
    cat('  ask =', ask, '\n')
    if (ask) {
      cat(' asking (conf = ', metric, ')...\n', sep='')
      #question.i = c(question.i, timestamp)
      
      # update model
      cat('  updating data...\n')
      hmm.model = UpdateHMMData(model=hmm.model, segment=next.gesture)
      #print(hmm.model)
      cat('  retraining model... ')
      hmm.model = TrainHMM(model=hmm.model, hmm.dis=hmm.dis,
                           hmm.nStates=hmm.nStates)
      ann.set.size = 1 + ann.set.size
      cat('done (size = ', ann.set.size, ')\n', sep='')
      
      # model diagnostics
      #cat(' model diagnostics:\n')
      #for (m in hmm.model) {
      #  cat(' -', m$class.label, ';')
      #  cond.null = !is.null(m$hmm)
      #  if (cond.null) {
      #    cat('model; ')
      #  } else {
      #    cat('NULL; ')
      #  }
      #  num.segs = length(m$data)
      #  cat(num.segs, 'seg ')
      #  if (!cond.null & (num.segs > 0)) {
      #    cat('*')
      #  }
      #  cat('\n')
      #}
      
      # evaluate new model
      n = 0
      for (m in hmm.model) {
        n = n + length(m$data)
      }
      cat(' model size =', ann.set.size, '\n')
      cat(' test only size =', length(subject.data), n, '\n')
      
      cat('  x-validation...\n')
      cm = CrossValidateWithTestHMM(model=hmm.model, 
                                    test.only.segments=subject.data, 
                                    num.folds=num.folds, 
                                    class.labels=class.labels, 
                                    hmm.dis=hmm.dis, 
                                    hmm.nStates=hmm.nStates)
      #cms[[1 + length(cms)]] = cm
      
      var.name = paste('cm', ann.set.size, sep='_')
      SaveObject(obj=cm, var.name=var.name, dir=output.dir)
      
      var.name = paste('question', ann.set.size, sep='_')
      SaveObject(obj=timestamp, var.name=var.name, dir=output.dir)
      
      #cat('  added cm to list\n')
      cat('  * mfm =', MeanFMeasure(cm), '\n')
    } else {
      # put gesture back in pool
      subject.data[[1 + length(subject.data)]] = next.gesture
      cat(' not asking...\n')
    }
    
    timestamp = 1 + timestamp
  }
}

SimulateOpportinityAL = function(subject.data, Nb, classifier.name, 
                                 weka.control, class.labels, 
                                 min.train.size,
                                 num.folds, online.confusion.metric,
                                 ...) {
  zeror.name = 'weka.classifiers.rules.ZeroR'
  zeror = make_Weka_classifier(name=zeror.name, class=class.labels)
  zeror.control = Weka_control()
  
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  CollapseSubjectData = function(subject.data) {
    do.call(what=rbind, args=subject.data)
  }
  
  subject.data.collapsed = CollapseSubjectData(subject.data=subject.data)
  
  ann.set = subject.data.collapsed[0, ]
  ann.set.size = 0
  model = zeror(formula=activity~., data=ann.set)
  
  question.i = c()
  
  cms = list()
  timestamp = 1
  
  while (ann.set.size < Nb) {
    cat('timestamp =', timestamp, '\n')
    # generate next gesture
    next.gesture.i = sample(x=length(subject.data), size=1)
    next.gesture = subject.data[[next.gesture.i]]
    subject.data = subject.data[-next.gesture.i]
    
    ## work out segment confidence - min confidence in segment
    predicted.class = predict(object=model, newdata=next.gesture, 
                              type='class')
    mode.class = names(sort(table(predicted.class), decreasing=T))[1]
    predicted.prob = predict(object=model, newdata=next.gesture, 
                             type='probability')    
    predicted.col.names = colnames(predicted.prob)
    mode.class.i = which(predicted.col.names == mode.class)
    mode.probs = predicted.prob[, mode.class.i]    
    avg.conf = min(mode.probs)
    
    ## work out segment confidence - confidence of the mode
    #predicted.class = predict(object=model, newdata=next.gesture, 
    #                          type='class')
    #mode.class = names(sort(table(predicted.class), decreasing=T))[1]
    #predicted.prob = predict(object=model, newdata=next.gesture, 
    #                         type='probability')    
    #predicted.col.names = colnames(predicted.prob)
    #mode.class.i = which(predicted.col.names == mode.class)
    #mode.probs = predicted.prob[, mode.class.i]    
    #avg.conf = mean(mode.probs)
    
    
    ## work out segment confidence - average confidence
    #predicted.prob = predict(object=model, newdata=next.gesture, 
    #                         type='probability')
    #avg.conf = 0
    #nrow.pred = nrow(predicted.prob)
    #for (i in seq_len(nrow.pred)) {
    #  prob.row = predicted.prob[i, ]
    #  avg.conf = avg.conf + (max(prob.row) / nrow.pred)
    #}
    
    #cat('predicted.prob:\n')
    #print(predicted.prob)
    #
    #cat('mode.class.i:', mode.class.i, '\n')
    #
    #cat('mode probs:')
    #print(mode.probs)
    #
    #cat('avg.conf:', avg.conf, '\n')
    
    # annotation probability
    ask.prob = online.confusion.metric(offline.metric=avg.conf, ...)
    
    r = runif(1)
    ask = r < ask.prob
    if (ask) {
      cat('   asking (min.conf = ', avg.conf, ')...\n', sep='')
      question.i = c(question.i, timestamp)
      
      # evaluate
      ann.set = rbind(ann.set, next.gesture)
      ann.set$activity = factor(x=ann.set$activity, 
                                levels=levels(subject.data.collapsed$activity))
      #cat('print(levels(ann.set$activity)):\n')
      #print(levels(ann.set$activity))
      
      data.compl = CollapseSubjectData(subject.data=subject.data)
      data.compl$activity = factor(x=data.compl$activity, 
                                   levels(subject.data.collapsed$activity))
      #cat('print(levels(data.compl$activity)):\n')
      #print(levels(data.compl$activity))
      
      cat('   cross validation...\n')
      cm = CrossValidateWithSubsample(classifier.name=classifier.name, 
                                      weka.control=weka.control, 
                                      data.subsample=ann.set,
                                      data.complementary=data.compl, 
                                      class.labels=class.labels, 
                                      num.folds=num.folds)
      
      cms[[1 + length(cms)]] = cm
      
      ann.set.size = 1 + ann.set.size
      cat('    new ann.set.size:', ann.set.size, '/', Nb, '\n')
      
      # rebuild model
      cat('   rebuilding model\n')
      if (nrow(ann.set) < min.train.size) {
        cls = zeror
        control = zeror.control
        cat('    using ZeroR\n')
      } else {
        cls = classifier
        control = weka.control
        cat('    using', classifier.name, '\n')
      }
      model = cls(formula=activity~., data=ann.set, control=control)
      
    } else {
      cat('   not asking...\n')
      
      # put back gesture data
      subject.data[[1 + length(subject.data)]] = next.gesture
    }
    
    timestamp = 1 + timestamp
  }
  
  return( list(cms=cms, question.i=question.i, 
               x.max=question.i[length(question.i)]) )
}

SimulateOpportunityALSample = function(subjects.features, 
                                       repetitions, 
                                       SubjectSamplingFunction,
                                       ...) {
  reps = seq_len(repetitions)
  subs = seq_along(subjects.features)
  
  c.p = expand.grid(rep=reps, s.f.i=subs)
  
  foreach (row.i = seq_len(nrow(c.p))) %dopar% {
    source('R/opportunity.al.R')
    row = c.p[row.i, ]
    s.f.i = row$s.f.i
    
    subject.data = subjects.features[[s.f.i]]$data
    
    output.folder = paste('subject', s.f.i, 'rep', row$rep, sep='_')
    opportunity.base = '~/R/sf_ALTLAR/output/opportunity/al/simulation'
    output.dir = paste(opportunity.base, output.folder, sep='/')
    
    SubjectSamplingFunction(subject.data=subject.data, 
                            output.dir=output.dir, ...)
  }
}

GetOpportunityClassLabels = function(subjects.features) {
  class.labels = c()
  for (s.f in subjects.features) {
    c.l = c()
    for (seg in s.f$data) {
      c.l = unique(c(c.l, unique(as.character(seg$activity))))
    }
    class.labels = unique(c(class.labels, c.l))
  }
  return( class.labels )
}
