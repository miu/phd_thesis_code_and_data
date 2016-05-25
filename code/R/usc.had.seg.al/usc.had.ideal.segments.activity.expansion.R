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

source('../ml.utils.R')

GetSegmentsIndicesForSubject = function(subject.features, class.labels.expand=c(), expand.factor=10,
                                        max.seg.size=6) {
  sf = subject.features$data  
  
  separate.activities = function(sf) {
    class.labels = sort(unique(as.character(sf$activity)))
    
    l.activities.frames = list()
    for (cl in class.labels) {
      i.frames = which(cl == as.character(sf$activity))
      l.activities.frames[[1 + length(l.activities.frames)]] = list(activity=cl, i.frames=i.frames)      
    }
    
    return( l.activities.frames )
  }
  
  expand.activities = function(l.activities.frames, class.labels.expand, expand.factor, max.seg.size) {
    for (i in seq_along(l.activities.frames)) {
      if (l.activities.frames[[i]]$activity %in% class.labels.expand) {
        x = l.activities.frames[[i]]$i.frames
        size = length(x) * expand.factor
        i.frames = sample(x=x, size=size, replace=T)
        
        #cat('old size:', length(x), '\n')
        #cat('new size:', length(i.frames), '\n\n')
        
        l.activities.frames[[i]]$i.frames = i.frames
      }
    }
    
    return( l.activities.frames )
  }
  
  get.stream.of.segments = function(l.activities.frames, max.seg.size) {
    l.segs = list()
    
    # get segments for each activity in turn
    for (af in l.activities.frames) {
      i.frames = af$i.frames
      
      while( length(i.frames) > 0) {
        n = min(c(length(i.frames), max.seg.size))
        seg.size = sample(x=seq_len(n), size=1)
        
        if (length(i.frames) > 1) {
          ii = seq_along(i.frames)
          seg.ii = sample(x=ii, size=seg.size, replace=F)
        } else {
          seg.ii = 1
        }
        seg.i = i.frames[seg.ii]
        
        l.segs[[1 + length(l.segs)]] = seg.i
        i.frames = i.frames[-seg.ii]
      }
    }
      
    # shuffle order of segments
    i.order = sample(x=seq_along(l.segs))
    l.segs = l.segs[i.order]
    
    return( l.segs )
  }  
  
  l.activities = separate.activities(sf=sf)
  l.activities = expand.activities(l.activities.frames=l.activities, 
                                   class.labels.expand=class.labels.expand, 
                                   expand.factor=expand.factor)
  l.seg.indices = get.stream.of.segments(l.activities.frames=l.activities, max.seg.size=max.seg.size)
  
  return( l.seg.indices )
}

Online.BMargin = function(conf, gamma) {
  return( exp(-gamma * conf) )
}

Online.Random = function(conf, gamma) {
  return( gamma )
}

GetSegmentsIndicesForSample = function(subjects.features, class.labels.expand, expand.factor, max.seg.size) {
  lapply(X=subjects.features, FUN=GetSegmentsIndicesForSubject, class.labels.expand=class.labels.expand, 
         expand.factor=expand.factor, max.seg.size=max.seg.size)
}

SimulateALForSample = function(subjects.features, l.seg.sample, min.train.size, max.train.size, classifier.name, 
                               weka.control, num.folds, num.reps, online.heuristic, gamma, num.days) {
  l = list()
  for (i.subject in seq_along(subjects.features)) {
    subject.features = subjects.features[[i.subject]]$data
    l.seg.indices = l.seg.sample[[i.subject]]
    
    cms.data = foreach(i.rep = seq_len(num.reps)) %dopar% {
      source('usc.had.ideal.segments.activity.expansion.R')
      
      SimulateALForSubject(subject.features=subject.features, l.seg.indices=l.seg.indices, 
                           max.train.size=max.train.size, min.train.size=min.train.size,
                           classifier.name=classifier.name, weka.control=weka.control, 
                           num.folds=num.folds, online.heuristic=online.heuristic, 
                           gamma=gamma, num.days=num.days)
    }
    l[[1 + length(l)]] = list(subject.number=i.subject, cms.data=cms.data)
  }
  return( l )
}

SimulateALForSubject = function(subject.features, l.seg.indices, min.train.size, max.train.size, classifier.name, 
                                weka.control, num.folds, online.heuristic, gamma, num.days) {
    
  class.labels = sort(unique(as.character(subject.features$activity)))
  
  num.total.segs = length(l.seg.indices)
  
  num.day = 1
  k.seg = 0
  timestamp = 0
  i.train = c()
  
  zeror.name = 'weka.classifiers.rules.ZeroR'
  zeror = make_Weka_classifier(name=zeror.name, class=class.labels)
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  model = zeror(formula=activity~., data=subject.features[i.train, ])
  
  cms = list()
  q.timestamps = c()
  q.days = c()
  q.k.segs = c()
  act.labels = c()
  while ( (length(i.train) < max.train.size) & (num.day < num.days) ) {
    
    #cat('length(i.train) =', length(i.train), 
    #    '; (length(i.train) < max.train.size) =', (length(i.train) < max.train.size))
    #cat('num.day =', num.day, 
    #    '; (num.day > num.days) =', (num.day > num.days), 
    #    '; overall condition =', ((length(i.train) < max.train.size) | (num.day > num.days)), 
    #    '\n')    
    
    timestamp = timestamp + 1
    if (k.seg == 0) {
      k.seg = 1
    }
    
    i.seg = l.seg.indices[[k.seg]]
    seg = subject.features[i.seg, ]
    
    k.seg = k.seg + 1
    if (k.seg > num.total.segs) {
      k.seg = k.seg %% (num.total.segs + 1)
      num.day = num.day + 1
    }
        
    predicted.probs = predict(model, seg, type='probability')
    confs = apply(X=predicted.probs, MARGIN=1, FUN=max)
    avg.conf = mean(confs)
    ask.prob = online.heuristic(avg.conf, gamma=gamma)
    
    thresh = runif(1)
    ask = thresh < ask.prob  
    
    if (ask) {
      cat('t=', timestamp, ' day=', num.day, ' k.seg=', k.seg, '\n', sep='')
      cat('labels: '); print(sort(unique(as.character(subject.features[i.train, ]$activity))))
      
      i.train = c(i.train, i.seg)
      train.set = subject.features[i.train, ]
      
      cat(' - asking... ')
      cat(' train.size=', nrow(train.set), '', sep='')
      
      if (length(unique(as.character(train.set$activity))) >= min.train.size) {
        model = classifier(formula=activity~., data=train.set, control=weka.control)
        cat(' B.NB ')
      } else {
        cat(' ZeroR ')
        model = zeror(formula=activity~., data=train.set)
      }
      
      cm = CrossValidateWithIndices(classifier.name=classifier.name, weka.control=weka.control, 
                                    subject.features=subject.features, i.train=i.train, 
                                    class.labels=class.labels, num.folds=num.folds, min.train.size=min.train.size)
      cat('F-Score =', WeightedFMeasure(cm), '\n')
      
      cms[[1 + length(cms)]] = cm
      q.timestamps = c(q.timestamps, timestamp)
      q.days = c(q.days, num.day)
      
      k.seg.rec = k.seg-1
      if (k.seg.rec == 0) {
        k.seg.rec = length(l.seg.indices)
      }
      q.k.segs = c(q.k.segs, k.seg.rec)
                   
      act.labels = c(act.labels, as.character(subject.features[i.seg, ]$activity)[1] )
    }
  }
  
  return( list(cms=cms, q.timestamps=q.timestamps, q.days=q.days, q.k.segs=q.k.segs, act.labels=act.labels) )
}

