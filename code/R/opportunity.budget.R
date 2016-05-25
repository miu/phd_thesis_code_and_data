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

setwd('~/R/sf_ALTLAR')

source('R/ml.utils.R')

GenerateQuestionIndices.uniform = function(train.set, percentage, ...) {
  n.total = length(train.set)
  n.questions = as.integer(percentage * n.total)
  if (n.questions > n.total) {
    msg = paste('Cannot ask', n.questions, 'from', 
                n.total, 'segments!')
    stop(msg)
  }
  
  q.i = sort(sample(x=n.total, size=n.questions), decreasing=F)
  
  return( q.i )
}

GenerateQuestionIndices.upfront = function(train.set, percentage, ...) {
  n.total = length(train.set)
  n.questions = as.integer(percentage * n.total)
  if (n.questions > n.total) {
    msg = paste('Cannot ask', n.questions, 'from', 
                n.total, 'segments!')
    stop(msg)
  }
  
  return( seq_len(n.questions) )
}

GenerateQuestionIndices.equidistant = function(train.set, percentage,
                                               ...) {
  n.total = length(train.set)
  n.questions = as.integer(percentage * n.total)
  if (n.questions > n.total) {
    msg = paste('Cannot ask', n.questions, 'from', 
                n.total, 'segments!')
    stop(msg)
  }
  
  q.i = as.integer(seq(from=1, to=n.total, length.out=n.questions))
  return( q.i )
}

GenerateQuestionIndices.exponential = function(train.set, percentage,
                                               lambda, ...) {
  n.total = length(train.set)
  n.questions = as.integer(percentage * n.total)
  if (n.questions > n.total) {
    msg = paste('Cannot ask', n.questions, 'from', 
                n.total, 'segments!')
    stop(msg)
  }
  
  fractional.seq = seq(from=0, to=1, length.out=n.total)
  unitary.sampling.prob = exp(-lambda * fractional.seq)
  
  if (n.questions <= n.total/2) {
    normalized.sampling.prob = unitary.sampling.prob / sum(unitary.sampling.prob)
    q.i = c()
    
    while (length(q.i) < n.questions) {
      i = sample(x=n.total, size=1)
      thresh = normalized.sampling.prob[i]
      include.prob = runif(n=1, min=0, max=1)
      if (include.prob < thresh) {
        if (!(i %in% q.i)) {
          q.i = c(q.i, i)
        }
      }
    }
  } else {
    unitary.exclusion.prob = 1 - unitary.sampling.prob
    normalized.exclusion.prob = unitary.exclusion.prob / sum(unitary.exclusion.prob)
    e.i = c()
    
    while (n.questions + length(e.i) < n.total) {
      i = sample(x=n.total, size=1)
      thresh = normalized.exclusion.prob[i]
      exclude.prob = runif(n=1, min=0, max=1)
      if (exclude.prob < thresh) {
        if (!(i %in% e.i)) {
          e.i = c(e.i, i)
        }
      }
    }    
    q.i = setdiff(seq_len(n.total), e.i)
  }
  
  return( sort(q.i, decreasing=F))
}

GenerateQuestionIndices.baseline = function(train.set, ...) {
  n.total = length(train.set)
  return( seq_len(n.total) )
}

GetSetClassLabels = function(data.set) {
  class.labels = c()
  for (seg in data.set) {
    c.l = unique(as.character(seg$activity))
    class.labels = unique(c(class.labels, c.l))
  }
  class.labels = sort(class.labels)
  return( class.labels )
}

CollapseTestSet = function(test.set) {
  as.data.frame(do.call(what=rbind, args=test.set))
}

CollapseTrainSet = function(train.set) {
  ts = train.set$data[[1]][0, ]
  for (seg in train.set$data) {
    ts = rbind(ts, seg)
  }
  return( as.data.frame(ts) )
}

Mode = function(v) {
  t.v = table(v)
  i.max = which(t.v == max(t.v))
  if (length(i.max) > 1) {
    i.max = sample(i.max, 1)
  }
  return( names(i.max) )
}

ClassifySegment = function(segment, model) {
  predicted.classes = as.character(predict(object=model, 
                                           newdata=segment, 
                                           type='class'))
  return( Mode(predicted.classes) )
}

TrainTestEval.Opportunity = function(train.set, test.set, actual,
                                     classifier, weka.control) {
  class.labels = GetSetClassLabels(data.set=test.set)
  #test.set.collapsed = CollapseTestSet(test.set)
  
  #print(train.set)
  #cat('a\n')
  #cat('nrow(trian.set) =', nrow(train.set))
  model = classifier(formula=activity~., data=train.set, 
                     control=weka.control)
  #print(model)
  #cat('b\n')
  ##actual = as.character(test.set.collapsed$activity)
  
  ### compute actual & predictions
  n = length(test.set)
  actual = rep('', n)
  predicted = rep('', n)
  for (i in seq_along(test.set)) {
    # actual
    actual[i] = as.character(test.set[[i]]$activity[1])
  }
  predicted = sapply(X=test.set, FUN=ClassifySegment, model=model)
  
  #cat('c\n')
  #predicted = as.character(predict(object=model, newdata=test.set.collapsed, 
  #                                 type='class'))
  #cat('d\n')
  cm = ConfusionMatrix(actual=actual, predicted=predicted, 
                       class.labels=class.labels)
  #cat('e\n')
  return( list(cm=cm, predicted=predicted, actual=actual) )
}

SimulateTest = function(train.set, test.set, percentage, QIGenerator,
                        classifier.name, weka.control, ...) {
  class.labels = GetSetClassLabels(data.set=test.set)
  classifier = make_Weka_classifier(name=classifier.name, 
                                    class=class.labels)
  
  q.i = QIGenerator(train.set=train.set, percentage=percentage, ...)
  #print(q.i) 
  
  n = length(test.set)
  actual = rep('', n)
  predicted = rep('', n)
  for (i in seq_along(test.set)) {
    # actual
    actual[i] = as.character(test.set[[i]]$activity[1])
  }
  
  cms = list()
  ann.set = list()
  predicteds = list()
  for (i in q.i) {
    clock.start = Sys.time()
    cat(i, '')
    ann.set = rbind(ann.set, train.set[[i]])
    levels(ann.set$activity) = class.labels
    #cat('nrow(ann.set) =', nrow(ann.set), '\n')
    l = TrainTestEval.Opportunity(train.set=ann.set, 
                                  test.set=test.set, actual=actual,
                                  classifier=classifier,
                                  weka.control=weka.control)
    #print(l$cm)
    cms[[1 + length(cms)]] = l$cm
    predicteds[[1 + length(predicteds)]] = l$predicted
    clock.end = Sys.time()
    cat('(', WeightedFMeasure(l$cm), ' [', 
        round(clock.end - clock.start, 1), 's]) ', sep='')
  }
  cat('\n')
  print(warnings())
  return( list(cms=cms, actual=l$actual, 
               predicteds=predicteds, q.i=q.i) )
}

SimulateTestSuite = function(train.sets, test.sets, percentage, 
                             QIGenerator, repetitions, file.name, destination.dir, 
                             train.set.ratio, opportunity.dir, ...) {
  perc.dir = paste('perc', percentage, sep='_')
  
  ts.ts = list()
  
  ## per subject evaluation  
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
    
    source('R/opportunity.budget.R')      
    n.seg = length(train.set$data)
    i.seg = seq_len(n.seg * train.set.ratio)
    print(head(i.seg))
    cat(length(i.seg), 'segments\n')
    
    #l = foreach (rep = seq_len(repetitions)) %dopar% {
    
    res = SimulateTest(train.set=train.set$data[i.seg], 
                       test.set=test.set$data, 
                       percentage=percentage,
                       QIGenerator=QIGenerator, ...)
    list(res=res, subject.number=subject.number)
  }
  
  SaveObject(obj=all.res, var.name='all.res', 
             dir=paste(opportunity.dir, destination.dir, sep='/'))
  
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
    dir = paste(opportunity.dir, destination.dir, perc.dir, 
                subject.dir, sep='/')
    SaveObject(obj=elem$reps, var.name=file.name, dir=dir)
  }
  
  ## all subjects evaluation
  #test.set = NULL
  #for (ts in test.sets) {
  #  test.set = c(test.set, ts$data)
  #}
  #
  #train.set = NULL
  #for (ts in train.sets) {
  #  train.set = c(train.set, ts$data)
  #}
  #
  #cat('All subjects evaluation\n')
  #cat(' train.set size:', length(train.set), '\n')
  #cat(' test.set  size:', length(test.set), '\n')
  #l = foreach (rep = seq_len(repetitions)) %dopar% {
  #  source('R/opportunity.budget.R')
  #  
  #  SimulateTest(train.set=train.set, 
  #               test.set=test.set, 
  #               percentage=percentage,
  #               QIGenerator=QIGenerator, ...)
  #}
  #subject.dir = 'subject.both'
  #dir = paste(opportunity.dir, destination.dir, perc.dir, subject.dir, 
  #            sep='/')
  #SaveObject(obj=l, var.name=file.name, dir=dir)
    
  return( NULL )
}

SimulateTestSuite.shuffle = function(train.sets, test.sets, percentage, 
                                     QIGenerator, repetitions, file.name, 
                                     destination.dir, train.set.ratio, 
                                     opportunity.dir, ...) {
  
  perc.dir = paste('perc', percentage, sep='_')
  
  ## per subject evaluation  
  for (test.set in test.sets) {
    train.set = train.sets[[1]]
    subject.dir = paste('subject', test.set$subject.number, sep='_')
    for (ts in train.sets) {
      #cat(' - train.set$subject.number =', ts$subject.number, '\n')
      #cat(' - test.set$subject.number =', test.set$subject.number, '\n')
      if (ts$subject.number == test.set$subject.number) {
        train.set = ts
        #cat('using this train/test combination\n\n')
        break
      } else {
        #cat('trying other train/test combination\n\n')
      }
    }
    cat('running... (', train.set$subject.number, ', ', test.set$subject.number, ')\n', sep='')
    cat(' train.set size:', length(train.set$data), '\n')
    cat(' test.set  size:', length(test.set$data), '\n')
    
    n.seg = length(train.set$data)
    i.seg = sample(n.seg)[seq_len(n.seg * train.set.ratio)]
    print(head(i.seg))
    cat(length(i.seg), 'segments\n')
    
    l = foreach (rep = seq_len(repetitions)) %dopar% {
      source('R/opportunity.budget.R')      
      SimulateTest(train.set=train.set$data[i.seg], 
                   test.set=test.set$data, 
                   percentage=percentage,
                   QIGenerator=QIGenerator, ...)
    }
    dir = paste(opportunity.dir, destination.dir, perc.dir, subject.dir, 
                sep='/')
    SaveObject(obj=l, var.name=file.name, dir=dir)
  }
  
  ## all subjects evaluation
  #test.set = NULL
  #for (ts in test.sets) {
  #  test.set = c(test.set, ts$data)
  #}
  #
  #train.set = NULL
  #for (ts in train.sets) {
  #  train.set = c(train.set, ts$data)
  #}
  #
  #cat('All subjects evaluation\n')
  #cat(' train.set size:', length(train.set), '\n')
  #cat(' test.set  size:', length(test.set), '\n')  
  #  n.seg = length(train.set)
  #  i.seg = sample(n.seg)[seq_len(n.seg * train.set.ratio)]
  #  print(head(i.seg))
  #  cat(length(i.seg), 'segments\n')  
  #l = foreach (rep = seq_len(repetitions)) %dopar% {
  #  source('R/opportunity.budget.R')
  #    
  #  SimulateTest(train.set=train.set[i.seg], 
  #               test.set=test.set, 
  #               percentage=percentage,
  #               QIGenerator=QIGenerator, ...)
  #}
  #subject.dir = 'subject_both'
  #dir = paste(opportunity.dir, destination.dir, perc.dir, subject.dir, 
  #            sep='/')
  #SaveObject(obj=l, var.name=file.name, dir=dir)

  return( NULL )
}
