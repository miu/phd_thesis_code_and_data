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

# ML model building and evaluation functions

library(RWeka)

EmptyConfusionMatrix = function(class.labels) {
  dim = length(class.labels)
  cm = matrix(data=0, nrow=dim, ncol=dim)
  rownames(cm) = as.character(class.labels)
  colnames(cm) = as.character(class.labels)
  
  return( cm )
}

ConfusionMatrix = function(actual, predicted, class.labels) {
  #print(actual)
  #cat('-----------------\n')
  #print(predicted)
  #cat('-----------------\n')
  cm = EmptyConfusionMatrix(class.labels)
  
  la = length(actual)
  lp = length(predicted)
  if (la != lp) {
    msg = paste('length(actual) != length(predicted):', la, lp, '\n')
    warning(msg)
  }
  
  #foreach(i = seq_len(lp)) %do% {
  for (i in seq_len(lp)) {
    #cat('(', i, ' / ', la, ') ', sep='')
    cla = actual[i]
    clp = predicted[i]
    cm[cla, clp] = 1 + cm[cla, clp]
  }
  #cat('\n')
  
  # cm[actual, predicted]
  return(cm)
}

TP = function(cm, label) {
  return( cm[label,label] )
}

FP = function(cm, label) {
  return( sum(cm[-label, label]) )
}

FN = function(cm, label) {
  return( sum(cm[label, -label]) )
}

Precision = function(cm, label) {
  tp = TP(cm, label)
  fp = FP(cm, label)
  
  return( Fraction(tp, tp+fp) )
}

Recall = function(cm, label) {
  tp = TP(cm, label)
  fn = FN(cm, label)
  
  return( Fraction(tp, tp+fn) )
}

FMeasure = function(cm, label) {
  p = Precision(cm, label)
  r = Recall(cm, label)
  
  return( Fraction(2*p*r, p+r) )
}

Accuracy = function(cm, label) {
  correct = sum(diag(cm))
  all = sum(cm)
  
  return( Fraction(correct, all) )
}

MeanFMeasure = function(cm) {
  fms = c()
  for (label in seq_len(ncol(cm))) {
    fms = c(fms, FMeasure(cm=cm, label=label))
  }
  return( mean(fms) )
}

WeightedFMeasure = function(cm) {
  fms = rep(0, nrow(cm)) # F-Measure
  w = rep(0, nrow(cm)) # label weights
  for (i.actual in seq_len(nrow(cm))) {
    fms[i.actual] = FMeasure(cm=cm, label=i.actual)
    w[i.actual] = sum(cm[i.actual, ]) / sum(cm)
  }
  return( sum(fms * w) )
}

Fraction = function(numerator, denominator) {
  if (denominator != 0) {
    return( numerator/denominator )
  } else {
    return( 0 )
  }
}

EvaluateModel = function(classifier.name, weka.control, train.data, 
                         test.data, class.labels) {
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  model = classifier(activity~., data=train.data, control=weka.control)
  
  actual = test.data$activity
  predicted = predict(model, test.data, type='class')
  
  return( ConfusionMatrix(actual=actual, predicted=predicted, 
                          class.labels=class.labels) )
}

CrossValidateWithIndices = function(classifier.name, weka.control, subject.features,
                                    i.train, class.labels, num.folds, min.train.size) {
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  zeror.name = 'weka/classifiers/rules/ZeroR'
  zeror = make_Weka_classifier(name=zeror.name, class=class.labels)
  
  all.i = seq_len(nrow(subject.features))
  
  partitions.indices = StrataIndices.indices(indices=i.train, n=num.folds)
  cm = EmptyConfusionMatrix(class.labels=class.labels)
  
  all.i.pi = seq_along(partitions.indices)
  for (i.pi in all.i.pi) {
    train.i.pi = setdiff(all.i.pi, i.pi)
    train.fold.i = i.train[unlist(partitions.indices[train.i.pi])]
    
    test.fold.i = setdiff(all.i, train.fold.i)
    
    train.set = subject.features[train.fold.i, ]
    test.set = subject.features[test.fold.i, ]
    
    #cat(' --- xval (', length(unlist(partitions.indices)), ')\n')
    #cat(' ---- nrow(train.set) =', nrow(train.set), '\n')
    #cat(' ---- nrow(test.set)  =', nrow(test.set), '\n')
    #
    #cat('train.fold.i:', train.fold.i, '\n')
    
    actual.labels = as.character(test.set$activity)
    predicted.labels = NULL
    
    if (length(unique(train.set$activity)) > 1) {
      #cat('+')
      model = classifier(activity ~ ., data=train.set, control=weka.control) 
    } else {
      #cat('-')
      model = zeror(activity ~ ., data=train.set, control=weka.control)
    }
    predicted.labels = as.character(predict(model, test.set, type='class'))
    
    cm = cm + ConfusionMatrix(actual=actual.labels, predicted=predicted.labels, 
                              class.labels=class.labels)
  }
  
  return( cm )
}

CrossValidateWithPopulationData = function(classifier.name, weka.control,
                                           train.user, train.pop, test.user, 
                                           class.labels, num.folds) {
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  partitions.indices = StrataIndices(train.user, num.folds)
  
  cm = EmptyConfusionMatrix(class.labels=class.labels)
  for (pi in partitions.indices) {
    test.set = rbind(train.user[pi, ], test.user)
    train.set = rbind(train.user[-pi, ], train.pop)
    model = classifier(activity ~ ., data=train.set, control=weka.control)
    
    actual.labels = test.set$activity    
    predicted.labels = predict(model, test.set, type='class')
    
    cm = cm + ConfusionMatrix(actual=actual.labels, predicted=predicted.labels, 
                              class.labels=class.labels)
  }
  
  return( cm )
}

# create n fairly balanced partitions of indices
StrataIndices = function(data, n) {
  x = seq_len(nrow(data))
  split(sample(x), sort(rank(x) %% n))
}

# create n fairly balanced partitions of indices
StrataIndices.indices = function(indices, n) {
  x = seq_along(indices)
  split(sample(x), sort(rank(x) %% n))
}

CrossValidate = function(classifier.name, weka.control, 
                         data, class.labels, num.folds) {
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  partitions.indices = StrataIndices(data, num.folds)
  
  cm = EmptyConfusionMatrix(class.labels=class.labels)
  #foreach (pi = partitions.indices) %do% {
  for (pi in partitions.indices) {
    test.set = data[pi,]
    train.set = data[-pi,]
    model = classifier(activity ~ ., data=train.set, control=weka.control)
    
    actual.labels = test.set$activity    
    predicted.labels = predict(model, test.set, type='class')
    
    cm = cm + ConfusionMatrix(actual=actual.labels, predicted=predicted.labels, 
                              class.labels=class.labels)
  }
  
  return( cm )
}

StrataIndices.random.segments = function(l.data, n) {
  x = seq_len(length(l.data))
  
  #print(split(sample(x), sort(rank(x) %% n)))
  
  split(sample(x), sort(rank(x) %% n))
}

CrossValidateWithSubsample.segments = function(classifier.name, weka.control,
                                               l.data.subsample, l.data.complementary,
                                               class.labels, num.folds) {
  classifier.informed = make_Weka_classifier(name=classifier.name, class=class.labels)
  weka.control.informed = weka.control
  
  zeror.name = 'weka/classifiers/rules/ZeroR'
  weka.control.zeror = Weka_control()
  zeror = make_Weka_classifier(name=zeror.name, class=class.labels)
  
  partitions.indices = StrataIndices.random.segments(l.data.subsample, num.folds)
  cm = EmptyConfusionMatrix(class.labels=class.labels)
  
  data.complementary = do.call(rbind, l.data.complementary)
  
  for (pi in partitions.indices) {
    #cat('.')
    #cat('*1')
    data.subsample = do.call(rbind, l.data.subsample[pi])
    #cat('*2')
    test.set = rbind(data.subsample, data.complementary)
    #cat('*3')
    
    #cat('length(l.data.subsample) =', length(l.data.subsample), '\n')
    #print('pi'); print(pi)
    
    train.set = do.call(rbind, l.data.subsample[-pi])
      
    #cat('*4')
    if (is.null(train.set)) {
      classifier = zeror
      weka.control = weka.control.zeror
    } else {
      classifier = classifier.informed
      weka.control = weka.control.informed
    }
    #cat('*5')
    
    #print(classifier.name)
    #print(weka.control)
    #print(classifier)
    
    #cat('print(levels(train.set$activity))\n')
    #print(levels(train.set$activity))
    #
    #cat('print(levels(test.set$activity))\n')
    #print(levels(test.set$activity))
    #
    #SaveObject(obj=train.set, var.name='xval.train.set', 
    #           dir='~/R/sf_ALTLAR/output/debug')
    
    actual.labels = as.character(test.set$activity)
    predicted.labels = NULL
    
    if (length(unique(train.set$activity)) > 1) {
      #cat(' >> length(unique(train.set$activity)) > 1\n')
      model = classifier(activity ~ ., data=train.set, control=weka.control) 
      predicted.labels = as.character(predict(model, test.set, type='class'))
    } else {
      #cat('-')
      #cat(' >> NOT length(unique(train.set$activity)) > 1\n')
      #print('train.set'); print(train.set)
      train.labels = as.character(train.set$activity)
      #print('train.labels:'); print(train.labels)
      
      #cat('nrow(train.set) =', nrow(train.set), '\n')
      
      train.labels.table = table(train.labels)
      mode.label.i = which(train.labels.table == max(train.labels.table))
      mode.label = names(mode.label.i)
      
      if (length(mode.label.i) > 1) {
        #cat('a1')
        mode.label = names(train.labels.table)[sample(x=mode.label.i, 1)]
        #cat('a2')
      } 
      if (length(mode.label.i) == 0) {
        mode.label = sample(x=class.labels, size=1)
      }
      #cat('b')
      
      #cat('c')
      #cat('mode.label =', mode.label, '\n')
      #cat('length(actual.labels) =', length(actual.labels), '\n')
      
      predicted.labels = rep(x=mode.label, times=length(actual.labels))
      #cat('d')
    }
    
    if (length(predicted.labels) != length(actual.labels)) {
      print(predicted.labels); print(actual.labels)
      cat('length(predicted.labels) =', length(predicted.labels), '\n')
      cat('length(actual.labels) =', length(actual.labels), '\n')
      stop('!!!')
    }
    
    #cat('e')
    #print(paste('actual.labels', length(actual.labels)))
    #print(actual.labels)
    
    #print(paste('predicted.labels', length(predicted.labels)))
    #print(predicted.labels)
    cm = cm + ConfusionMatrix(actual=actual.labels, predicted=predicted.labels, 
                              class.labels=class.labels)
    #cat('\nALL RIGHT\n')
  }
  #cat('\n')
  
  return( cm )
}

LeaveOneOutCrossValidate = function(classifier.name, weka.control, 
                                    data, class.labels) {
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  cm = EmptyConfusionMatrix(class.labels=class.labels)
  for (i in seq_len(nrow(data))) {
    test.set = data[i, ]
    train.set = data[-i, ]
    model = classifier(activity ~ ., data=train.set, control=weka.control)
    
    actual.labels = test.set$activity    
    predicted.labels = predict(model, test.set, type='class')
    
    cm = cm + ConfusionMatrix(actual=actual.labels, predicted=predicted.labels, 
                              class.labels=class.labels)
  }
  return(cm)
}

CrossValidateWithSubsample.old = function(classifier.name, weka.control,
                                          data.subsample, data.complementary,
                                          class.labels, num.folds) {
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  zeror.name = 'weka/classifiers/rules/ZeroR'
  zeror = make_Weka_classifier(name=zeror.name, class=class.labels)
  
  partitions.indices = StrataIndices(data.subsample, num.folds)
  cm = EmptyConfusionMatrix(class.labels=class.labels)

  for (pi in partitions.indices) {
    #cat('.')
    test.set = rbind(data.subsample[pi,], data.complementary)
    train.set = data.subsample[-pi,]
    
    #print(classifier.name)
    #print(weka.control)
    #print(classifier)
    
    #cat('print(levels(train.set$activity))\n')
    #print(levels(train.set$activity))
    #
    #cat('print(levels(test.set$activity))\n')
    #print(levels(test.set$activity))
    #
    #SaveObject(obj=train.set, var.name='xval.train.set', 
    #           dir='~/R/sf_ALTLAR/output/debug')
    
    actual.labels = as.character(test.set$activity)
    predicted.labels = NULL
    
    if (length(unique(train.set$activity)) > 1) {
      #cat('+')
      model = classifier(activity ~ ., data=train.set, control=weka.control) 
      predicted.labels = as.character(predict(model, test.set, type='class'))
    } else {
      #cat('-')
      train.labels = as.character(train.set$activity)
      train.labels.table = table(train.labels)
      mode.label.i = which(train.labels.table == max(train.labels.table))
      if (length(mode.label.i) > 1) {
        mode.label = names(train.labels.table)[sample(x=mode.label.i, 1)]
      }
      mode.label = names(mode.label.i)
      predicted.labels = rep(x=mode.label, times=length(actual.labels))
    }
    cm = cm + ConfusionMatrix(actual=actual.labels, predicted=predicted.labels, 
                              class.labels=class.labels)
  }
  #cat('\n')
  
  return( cm )
}

CrossValidateWithSubsample = function(classifier.name, weka.control,
                                          data.subsample, data.complementary,
                                          class.labels, num.folds) {
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  zeror.name = 'weka/classifiers/rules/ZeroR'
  zeror = make_Weka_classifier(name=zeror.name, class=class.labels)
  
  partitions.indices = StrataIndices(data.subsample, num.folds)
  cm = EmptyConfusionMatrix(class.labels=class.labels)
  
  for (pi in partitions.indices) {
    #cat('.')
    test.set = rbind(data.subsample[pi,], data.complementary)
    train.set = data.subsample[-pi,]
    
    #print(classifier.name)
    #print(weka.control)
    #print(classifier)
    
    #cat('print(levels(train.set$activity))\n')
    #print(levels(train.set$activity))
    #
    #cat('print(levels(test.set$activity))\n')
    #print(levels(test.set$activity))
    #
    #SaveObject(obj=train.set, var.name='xval.train.set', 
    #           dir='~/R/sf_ALTLAR/output/debug')
    
    actual.labels = as.character(test.set$activity)
    predicted.labels = NULL
    
    if (length(unique(train.set$activity)) > 2) {
      #cat('+')
      model = classifier(activity ~ ., data=train.set, control=weka.control) 
      
    } else {
      model = zeror(activity ~ ., data=train.set, control=weka.control)
    }
    predicted.labels = as.character(predict(model, test.set, type='class'))
    cm = cm + ConfusionMatrix(actual=actual.labels, predicted=predicted.labels, 
                              class.labels=class.labels)
  }
  #cat('\n')
  
  return( cm )
}

CrossValidateWithSubsample.noisy = function(classifier.name, weka.control,
                                            data.subsample, data.complementary,
                                            class.labels, num.folds) {
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  partitions.indices = StrataIndices(data.subsample, num.folds)
  cm = EmptyConfusionMatrix(class.labels=class.labels)
  
  for (pi in partitions.indices) {
    #print(names(data.subsample))
    #print(names(data.complementary))
    #cat('      -- OK1\n')
    test.set = rbind(data.subsample[pi,], data.complementary)
    # 'de-pollute' test set
    #cat('      -- OK2\n')
    test.set$activity.polluted = NULL
    
    #cat('      -- OK3\n')
    train.set = data.subsample[-pi,]
    # 'pollute' train set
    #cat('      -- OK4\n')
    train.set$activity = train.set$activity.polluted
    #cat('      -- OK5\n')
    train.set$activity.polluted = NULL
    
    #cat('      -- OK6\n')
    model = classifier(activity ~ ., data=train.set, control=weka.control)
    #cat('      -- OK7\n')
    actual.labels = as.character(test.set$activity)
    predicted.labels = as.character(predict(model, test.set, type='class'))
    cm = cm + ConfusionMatrix(actual=actual.labels, predicted=predicted.labels, 
                              class.labels=class.labels)
  }
  #cat(' -- OK8\n')
  return( cm )
}

NormalizedAUC = function(x, x.max, y) {
  auc = 0
  lx = length(x)
  x.diffs = c(x[2:lx], x.max) - x
  x.diffs[length(x.diffs)] = 1 + x.diffs[length(x.diffs)]
  for (i in seq_along(y)) {
    #cat('        x.diffs[i] =', x.diffs[i], '\n')
    for (rep in seq_len(x.diffs[i])) {
      auc = auc + y[i]
    }
  }
  #cat('auc =', auc, '\n')
  #cat('x.max =', x.max, '\n')  
  
  return( auc / x.max )
}

CrossValidateWithNoisyTrainingSet = function(classifier.name, weka.control,
                                             data.subsample.noisy, 
                                             data.subsample.clean, 
                                             data.complementary,
                                             class.labels, num.folds) {
  classifier = make_Weka_classifier(name=classifier.name, class=class.labels)
  
  partitions.indices = StrataIndices(data.subsample.noisy, num.folds)
  cm = EmptyConfusionMatrix(class.labels=class.labels)
  
  for (pi in partitions.indices) {
    test.set = rbind(data.subsample.clean[pi,], data.complementary)
    train.set = data.subsample.noisy[-pi,]
    model = classifier(activity ~ ., data=train.set, control=weka.control)
    
    actual.labels = as.character(test.set$activity)
    predicted.labels = as.character(predict(model, test.set, type='class'))
    cm = cm + ConfusionMatrix(actual=actual.labels, 
                              predicted=predicted.labels, 
                              class.labels=class.labels)
  }
  return( cm )
}
