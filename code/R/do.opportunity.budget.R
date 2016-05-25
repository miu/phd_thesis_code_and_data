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

library(RWeka)
library(doRedis)

setwd('~/R/sf_ALTLAR')

source('R/opportunity.budget.R')
source('R/file.utils.R')
source('R/ml.utils.R')

#ReplaceLabel = function(label) {
#  if ((label == 506616) | (label == 506617)) {
#    return( '506616' )
#  } else if ((label == 504616) | (label == 504617)) {
#    return( '504616' )
#  } else if ((label == 506619) | (label == 506611) | (label == 506608)) {
#    return( '506619' )
#  } else if ((label == 504619) | (label == 504611) | (label == 504608)) {
#    return( '504619' )
#  } else {
#    return( label )
#  }
#}

ReplaceLabel = function(label) {
  if ((label == 506616) | (label == 506617) |
        (label == 504616) | (label == 504617)) { # 2 doors
    return( '506616' )
  } else if ((label == 506619) | (label == 506611) | (label == 506608) | 
               (label == 504619) | (label == 504611) | (label == 504608)) { # 3 drawers
    return( '506619' )
  } else if ((label == 506620) | (label == 504620)) { # fridge
    return( '506620' )
  } else if ((label == 506605) | (label == 504605)) { # dishwasher
    return( '506605' )
  } else {
    return( label )
  }
}

ReplaceLabels = function(dataset) {
  for (i in seq_along(dataset)) {
    data = dataset[[i]]$data
    for (i.seg in seq_along(data)) {
      seg = data[[i.seg]]
      label = ReplaceLabel(as.character(seg$activity[1]))
      dataset[[i]]$data[[i.seg]]$activity = as.factor(label)
    }
  }
  return( dataset )
}

subjects.analyzed = c(1, 2, 3, 4)
#subjects.analyzed = 4

opportunity.dir ='output/opportunity/budget.5.mcnemar.segment-level.re'
file.name = 'opportunity.subjects.features.train.250.incr'
train.sets = LoadObject(dir=opportunity.dir, file.name=file.name)
train.sets = ReplaceLabels(train.sets)
train.sets = train.sets[subjects.analyzed]

train.sets.shuffled = list()
for (i in seq_along(train.sets)) {
  ts = list()
  
  ts$subject.number = train.sets[[i]]$subject.number
  
  segments = train.sets[[i]]$data
  i.segments = sample(x=length(segments))
  ts$data = segments[i.segments]
  
  train.sets.shuffled[[i]] = ts
}

file.name = 'opportunity.subjects.features.test.250.incr'
test.sets = LoadObject(dir=opportunity.dir, file.name=file.name)
test.sets = ReplaceLabels(test.sets)
test.sets = test.sets[subjects.analyzed]

#classifier.name = 'weka.classifiers.lazy.IBk'
#weka.control = Weka_control(K=3)

classifier.name = 'weka.classifiers.trees.J48'
weka.control = Weka_control()

#classifier.name = 'weka/classifiers/meta/Bagging'
#num.iterations = 30
#weak.classifier.name = 'weka.classifiers.trees.J48'
#weka.control = Weka_control(W=weak.classifier.name, 
#                            I=num.iterations)
queue.name='opportunity.budget.queue'
registerDoRedis(queue=queue.name)

repetitions = 10

#percs = c(0.05, 0.1, 0.15, 0.5, # critical ones
#          0.2, 0.25, 0.3, 0.35, 0.4, 0.45, # the rest
#          0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 
#          0.9, 0.95) #...
#percs = c(0.1, 0.25, 0.5, 0.75)
percs = 0.75

lambdas = c(1, 5)
#lambdas = 1

train.set.ratio = 1

file.name = 'c4.5'

## unshuffled

#destination.dir = 'baseline.unshuffled'
#l = SimulateTestSuite(train.sets=train.sets, 
#                      test.sets=test.sets, 
#                      percentage=1, 
#                      QIGenerator=GenerateQuestionIndices.baseline, 
#                      classifier.name=classifier.name, 
#                      weka.control=weka.control,
#                      file.name=file.name,
#                      repetitions=4, 
#                      destination.dir=destination.dir,
#                      train.set.ratio=train.set.ratio,
#                      opportunity.dir=opportunity.dir)

for (percentage in percs) {
#   for (lambda in lambdas) {
#     destination.dir = paste('exp', lambda, sep='_')
#     destination.dir = paste(destination.dir, 'unshuffled', sep='.')
#     l = SimulateTestSuite(train.sets=train.sets, 
#                           test.sets=test.sets, 
#                           percentage=percentage, 
#                           QIGenerator=GenerateQuestionIndices.exponential, 
#                           classifier.name=classifier.name, 
#                           weka.control=weka.control,
#                           file.name=file.name,
#                           repetitions=repetitions,
#                           lambda=lambda,
#                           destination.dir=destination.dir,
#                           train.set.ratio=train.set.ratio,
#                           opportunity.dir=opportunity.dir)
#   }
#     
#   destination.dir = 'unif'
#   destination.dir = paste(destination.dir, 'unshuffled', sep='.')
#   l = SimulateTestSuite(train.sets=train.sets, 
#                         test.sets=test.sets, 
#                         percentage=percentage, 
#                         QIGenerator=GenerateQuestionIndices.uniform, 
#                         classifier.name=classifier.name, 
#                         weka.control=weka.control,
#                         file.name=file.name,
#                         repetitions=repetitions,
#                         lambda=lambda,
#                         destination.dir=destination.dir,
#                         train.set.ratio=train.set.ratio,
#                         opportunity.dir=opportunity.dir)
#    
#   destination.dir = 'equi'
#   destination.dir = paste(destination.dir, 'unshuffled', sep='.')
#   l = SimulateTestSuite(train.sets=train.sets, 
#                         test.sets=test.sets, 
#                         percentage=percentage, 
#                         QIGenerator=GenerateQuestionIndices.equidistant, 
#                         classifier.name=classifier.name, 
#                         weka.control=weka.control,
#                         file.name=file.name,
#                         repetitions=1,
#                         destination.dir=destination.dir,
#                         train.set.ratio=train.set.ratio,
#                         opportunity.dir=opportunity.dir)
  
  destination.dir = 'upfront'
  destination.dir = paste(destination.dir, 'unshuffled', sep='.')
  l = SimulateTestSuite(train.sets=train.sets, 
                        test.sets=test.sets, 
                        percentage=percentage, 
                        QIGenerator=GenerateQuestionIndices.upfront, 
                        classifier.name=classifier.name, 
                        weka.control=weka.control,
                        file.name=file.name,
                        repetitions=1,
                        destination.dir=destination.dir,
                        train.set.ratio=train.set.ratio,
                        opportunity.dir=opportunity.dir)
}

#stop('DONE')

#print(warnings())
#stop('okay')
#
#train.set.ratios = percs
#for (train.set.ratio in train.set.ratios) {
#  partial.dir = paste('partial', train.set.ratio, sep='_')
#  for (percentage in percs) {
#    for (lambda in lambdas) {
#      exp.dir = paste('exp', lambda, sep='_')
#      exp.dir = paste(exp.dir, 'shuffled', sep='.')
#      destination.dir = paste(partial.dir, exp.dir, sep='/')
#      l = SimulateTestSuite(train.sets=train.sets.shuffled, 
#                            test.sets=test.sets, 
#                            percentage=percentage, 
#                            QIGenerator=GenerateQuestionIndices.exponential, 
#                            classifier.name=classifier.name, 
#                            weka.control=weka.control,
#                            file.name=file.name,
#                            repetitions=repetitions,
#                            lambda=lambda,
#                            destination.dir=destination.dir,
#                            train.set.ratio=train.set.ratio,
#                            opportunity.dir=opportunity.dir)
#      
#      exp.dir = paste('exp', lambda, sep='_')
#      exp.dir = paste(exp.dir, 'unshuffled', sep='.')
#      destination.dir = paste(partial.dir, exp.dir, sep='/')
#      #l = SimulateTestSuite(train.sets=train.sets, 
#      #                      test.sets=test.sets, 
#      #                      percentage=percentage, 
#      #                      QIGenerator=GenerateQuestionIndices.exponential, 
#      #                      classifier.name=classifier.name, 
#      #                      weka.control=weka.control,
#      #                      file.name=file.name,
#      #                      repetitions=repetitions,
#      #                      lambda=lambda,
#      #                      destination.dir=destination.dir,
#      #                      train.set.ratio=train.set.ratio,
#      #                      opportunity.dir=opportunity.dir)
#    }
#  }
#}

stop('Done!')

###### shuffled
destination.dir = 'baseline.shuffled'
l = SimulateTestSuite(train.sets=train.sets.shuffled, 
                      test.sets=test.sets, 
                      percentage=1, 
                      QIGenerator=GenerateQuestionIndices.baseline, 
                      classifier.name=classifier.name, 
                      weka.control=weka.control,
                      file.name=file.name,
                      repetitions=1, 
                      destination.dir=destination.dir,
                      train.set.ratio=train.set.ratio,
                      opportunity.dir=opportunity.dir)

for (percentage in percs) {
  for (lambda in lambdas) {
    destination.dir = paste('exp', lambda, sep='_')
    destination.dir = paste(destination.dir, 'shuffled', sep='.')
    l = SimulateTestSuite(train.sets=train.sets.shuffled, 
                          test.sets=test.sets, 
                          percentage=percentage, 
                          QIGenerator=GenerateQuestionIndices.exponential, 
                          classifier.name=classifier.name, 
                          weka.control=weka.control,
                          file.name=file.name,
                          repetitions=repetitions,
                          lambda=lambda,
                          destination.dir=destination.dir,
                          train.set.ratio=train.set.ratio,
                          opportunity.dir=opportunity.dir)
  }
  
  destination.dir = 'unif'
  destination.dir = paste(destination.dir, 'shuffled', sep='.')
  l = SimulateTestSuite(train.sets=train.sets.shuffled, 
                        test.sets=test.sets, 
                        percentage=percentage, 
                        QIGenerator=GenerateQuestionIndices.uniform, 
                        classifier.name=classifier.name, 
                        weka.control=weka.control,
                        file.name=file.name,
                        repetitions=repetitions,
                        lambda=lambda,
                        destination.dir=destination.dir,
                        train.set.ratio=train.set.ratio,
                        opportunity.dir=opportunity.dir)
  
  destination.dir = 'equi'
  destination.dir = paste(destination.dir, 'shuffled', sep='.')
  l = SimulateTestSuite(train.sets=train.sets.shuffled, 
                        test.sets=test.sets, 
                        percentage=percentage, 
                        QIGenerator=GenerateQuestionIndices.equidistant, 
                        classifier.name=classifier.name, 
                        weka.control=weka.control,
                        file.name=file.name,
                        repetitions=1,
                        destination.dir=destination.dir,
                        train.set.ratio=train.set.ratio,
                        opportunity.dir=opportunity.dir)
    
  destination.dir = 'upfront'
  destination.dir = paste(destination.dir, 'shuffled', sep='.')
  l = SimulateTestSuite(train.sets=train.sets.shuffled, 
                        test.sets=test.sets, 
                        percentage=percentage, 
                        QIGenerator=GenerateQuestionIndices.upfront, 
                        classifier.name=classifier.name, 
                        weka.control=weka.control,
                        file.name=file.name,
                        repetitions=1,
                        destination.dir=destination.dir,
                        train.set.ratio=train.set.ratio,
                        opportunity.dir=opportunity.dir)
}
