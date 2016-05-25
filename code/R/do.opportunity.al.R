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

library(RWeka)
library(doRedis)

source('R/file.utils.R')
source('R/ml.utils.R')
source('R/index.sampling.R')
source('R/opportunity.al.R')

TrimSubjectsFeatures = function(subjects.features, col.i) {
  for (i in seq_along(subjects.features)) {
    for (k in seq_along(subjects.features[[i]]$data)) {
      seg = subjects.features[[i]]$data[[k]]
      subjects.features[[i]]$data[[k]] = seg[, col.i]
    }
  }
  return( subjects.features )
}

subjects.features = LoadObject(dir='output/opportunity', 
                               file.name='opportunity.subjects.features.collapsed.250.incr')

subjects.features = CleanSubjectsFeatures(subjects.features)

#col.i = c(1:3, 10)
#subjects.features = TrimSubjectsFeatures(subjects.features, col.i)
subjects.features = subjects.features[2:4]

queue.name='opportunity.queue.2'
registerDoRedis(queue=queue.name)
cat('Waiting for workers...\n')

model.builders = list()

mb = list()
mb$name = 'weka/classifiers/meta/Bagging'
bagging.iterations = 30
weak.classifier.name = 'weka.classifiers.trees.J48'
mb$weka.control = Weka_control(W=weak.classifier.name, 
                               I=bagging.iterations)
mb$dir.name = 'b.30.j48'
#model.builders[[1 + length(model.builders)]] = mb

mb = list()
mb$name = 'weka/classifiers/meta/Bagging'
bagging.iterations = 50
weak.classifier.name = 'weka.classifiers.trees.J48'
mb$weka.control = Weka_control(W=weak.classifier.name, 
                               I=bagging.iterations)
mb$dir.name = 'b.50.j48'
model.builders[[1 + length(model.builders)]] = mb

mb = list()
mb$name = 'weka/classifiers/meta/AdaBoostM1'
bagging.iterations = 100
weak.classifier.name = 'weka.classifiers.trees.J48'
mb$weka.control = Weka_control(W=weak.classifier.name, 
                               I=bagging.iterations)
mb$dir.name = 'boost.100.j48'
model.builders[[1 + length(model.builders)]] = mb

mb = list()
mb$name = 'weka/classifiers/meta/Bagging'
bagging.iterations = 30
weak.classifier.name = 'weka.classifiers.bayes.NaiveBayes'
mb$weka.control = Weka_control(W=weak.classifier.name, 
                               I=bagging.iterations)
mb$dir.name = 'b.30.nb'
model.builders[[1 + length(model.builders)]] = mb




mb = list()
mb$name = 'weka/classifiers/bayes/NaiveBayes'
mb$weka.control = Weka_control()
mb$dir.name = 'nb'
model.builders[[1 + length(model.builders)]] = mb

mb = list()
mb$name = 'weka/classifiers/lazy/IBk'
mb$weka.control = Weka_control(K=3)
mb$dir.name = 'knn.3'
model.builders[[1 + length(model.builders)]] = mb

mb = list()
mb$name = 'weka/classifiers/meta/Bagging'
bagging.iterations = 30
weak.classifier.name = 'weka.classifiers.trees.J48'
mb$weka.control = Weka_control(W=weak.classifier.name, 
                               I=bagging.iterations)
mb$dir.name = 'b.30.j48'
model.builders[[1 + length(model.builders)]] = mb

mb = list()
mb$name = 'weka/classifiers/meta/Bagging'
bagging.iterations = 30
weak.classifier.name = 'weka.classifiers.bayes.NaiveBayes'
mb$weka.control = Weka_control(W=weak.classifier.name, 
                               I=bagging.iterations)
mb$dir.name = 'b.30.nb'
model.builders[[1 + length(model.builders)]] = mb

mb = list()
mb$name = 'weka/classifiers/meta/AdaBoostM1'
bagging.iterations = 100
weak.classifier.name = 'weka.classifiers.trees.J48'
mb$weka.control = Weka_control(W=weak.classifier.name, 
                               I=bagging.iterations)
mb$dir.name = 'boost.100.j48'
model.builders[[1 + length(model.builders)]] = mb


mb = list()
mb$name = 'weka/classifiers/meta/Bagging'
bagging.iterations = 20
weak.classifier.name = 'weka.classifiers.trees.J48'
mb$weka.control = Weka_control(W=weak.classifier.name, 
                               I=bagging.iterations)
mb$dir.name = 'b.20.j48'
model.builders[[1 + length(model.builders)]] = mb

mb = list()
mb$name = 'weka/classifiers/meta/Bagging'
bagging.iterations = 10
weak.classifier.name = 'weka.classifiers.trees.J48'
mb$weka.control = Weka_control(W=weak.classifier.name, 
                               I=bagging.iterations)
mb$dir.name = 'b.10.j48'
model.builders[[1 + length(model.builders)]] = mb

mb = list()
mb$name = 'weka/classifiers/functions/MultilayerPerceptron'
hidden.nodes = 18
mb$weka.control = Weka_control(H=hidden.nodes)
mb$dir.name = 'nn'
model.builders[[1 + length(model.builders)]] = mb

mb = list()
mb$name = 'weka/classifiers/lazy/IBk'
mb$weka.control = Weka_control(K=10)
mb$dir.name = 'knn.10'
model.builders[[1 + length(model.builders)]] = mb



mb = list()
mb$name = 'weka/classifiers/meta/AdaBoostM1'
bagging.iterations = 20
weak.classifier.name = 'weka.classifiers.trees.J48'
mb$weka.control = Weka_control(W=weak.classifier.name, 
                               I=bagging.iterations)
mb$dir.name = 'boost.20.j48'
model.builders[[1 + length(model.builders)]] = mb

mb = list()
mb$name = 'weka/classifiers/meta/AdaBoostM1'
bagging.iterations = 10
weak.classifier.name = 'weka.classifiers.trees.J48'
mb$weka.control = Weka_control(W=weak.classifier.name, 
                               I=bagging.iterations)
mb$dir.name = 'boost.10.j48'
model.builders[[1 + length(model.builders)]] = mb

mb = list()
mb$name = 'weka/classifiers/meta/Bagging'
bagging.iterations = 30
weak.classifier.name = 'weka.classifiers.trees.J48'
mb$weka.control = Weka_control(I=bagging.iterations,
                               W=weak.classifier.name, '--', U=T, M=2)
mb$dir.name = 'b.30.j48.highvar'
model.builders[[1 + length(model.builders)]] = mb

mb = list()
mb$name = 'weka/classifiers/lazy/IBk'
mb$weka.control = Weka_control(K=2)
mb$dir.name = 'knn.2'
model.builders[[1 + length(model.builders)]] = mb

mb = list()
mb$name = 'weka/classifiers/lazy/IBk'
mb$weka.control = Weka_control(K=5)
mb$dir.name = 'knn.5'
model.builders[[1 + length(model.builders)]] = mb

mb = list()
mb$name = 'weka/classifiers/meta/Bagging'
bagging.iterations = 20
weak.classifier.name = 'weka.classifiers.bayes.NaiveBayes'
mb$weka.control = Weka_control(W=weak.classifier.name, 
                               I=bagging.iterations)
mb$dir.name = 'b.20.nb'
model.builders[[1 + length(model.builders)]] = mb

mb = list()
mb$name = 'weka/classifiers/meta/Bagging'
bagging.iterations = 10
weak.classifier.name = 'weka.classifiers.bayes.NaiveBayes'
mb$weka.control = Weka_control(W=weak.classifier.name, 
                               I=bagging.iterations)
mb$dir.name = 'b.10.nb'
model.builders[[1 + length(model.builders)]] = mb

mb = list()
mb$name = 'weka/classifiers/functions/Logistic'
mb$weka.control = Weka_control()
mb$dir.name = 'logistic'
model.builders[[1 + length(model.builders)]] = mb










min.train.size = 4
class.labels = GetOpportunityClassLabels(subjects.features)
num.folds = 10
repetitions = 1
Nb = 200 # max 200

gammas = c(6)
#gammas = 1
fs = 0.4
#fs = c()

SubjectSamplingFunction = SimulateOpportinityAL
root.dir = '~/R/sf_ALTLAR/output/opportunity/mb.trials.min'
for (mb in model.builders) {
  for (gamma in gammas) {
    l = SimulateOpportunityALSample(subjects.features=subjects.features,
                                    repetitions=repetitions, 
                                    SubjectSamplingFunction=SubjectSamplingFunction,
                                    Nb=Nb, 
                                    classifier.name=mb$name, 
                                    weka.control=mb$weka.control, 
                                    min.train.size=min.train.size, 
                                    class.labels=class.labels, 
                                    num.folds=num.folds, 
                                    online.confusion.metric=Online.BMargin, 
                                    gamma=gamma)
    gamma.dir = paste('gamma', gamma, sep='.')
    output.dir = paste(root.dir, mb$dir.name, gamma.dir, sep='/')
    SaveObject(obj=l, var.name='sample', dir=output.dir)
  }
  
  for (f in fs) {
    l = SimulateOpportunityALSample(subjects.features=subjects.features,
                                    repetitions=repetitions, 
                                    SubjectSamplingFunction=SubjectSamplingFunction,
                                    Nb=Nb, 
                                    classifier.name=mb$name, 
                                    weka.control=mb$weka.control, 
                                    min.train.size=min.train.size, 
                                    class.labels=class.labels, 
                                    num.folds=num.folds, 
                                    online.confusion.metric=Online.FixedRate,
                                    f=f)
    f.dir = paste('f', f, sep='.')
    output.dir = paste(root.dir, mb$dir.name, f.dir, sep='/')
    SaveObject(obj=l, var.name='sample', dir=output.dir)
  }
  
}

stop('-- Done!')

#l = SimulateOpportinityAL(subject.data=subjects.features[[1]]$data, 
#                          Nb=Nb, 
#                          classifier.name=classifier.name, 
#                          weka.control=Weka_control(), 
#                          min.train.size=min.train.size, 
#                          class.labels=class.labels, 
#                          num.folds=num.folds, 
#                          online.confusion.metric=online.confusion.metric, 
#                          gamma=1)

#SubjectSamplingFunction = SimulateOpportinityAL
#l = SimulateOpportunityALSample(subjects.features=subjects.features,
#                                repetitions=repetitions, 
#                                SubjectSamplingFunction=SubjectSamplingFunction,
#                                Nb=Nb, 
#                                classifier.name=classifier.name, 
#                                weka.control=weka.control, 
#                                min.train.size=min.train.size, 
#                                class.labels=class.labels, 
#                                num.folds=num.folds, 
#                                online.confusion.metric=online.confusion.metric, 
#                                #gamma=gamma)
#                                f=f)
#
#SaveObject(obj=l, dir='output/opportunity/rs', var.name='sample')

#subject.data=subjects.features[[2]]$data
#for (i in seq_along(subject.data)) {
#  seg = subject.data[[i]]
#  subject.data[[i]] = seg[, c(1:3, 10)]
#}
#



#Nb = 100
#num.folds = 10
#online.confusion.metric = Online.BMargin
#offline.confusion.metric = Offline.Confidence
#hmm.nStates = 3
#hmm.dis = 'NORMAL'
#class.labels = GetOpportunityClassLabels(subjects.features)
#subject.data = subjects.features[[2]]$data
#gamma = 3
#f = 0.2
#l = SimultateOpportunityAL.hmm(subject.data=subject.data, 
#                               Nb=Nb, class.labels=class.labels, 
#                               num.folds=num.folds, 
#                               online.confusion.metric=online.confusion.metric, 
#                               offline.confusion.metric=offline.confusion.metric, 
#                               hmm.nStates=hmm.nStates, 
#                               hmm.dis=hmm.dis, gamma=gamma)
#SaveObject(obj=l, var.name='silly.l', dir='output/opportunity/debug')
#stop('okay')

SubjectSamplingFunction = SimultateOpportunityAL.hmm
online.confusion.metric = Online.BMargin
offline.confusion.metric = Offline.Confidence
class.labels = GetOpportunityClassLabels(subjects.features)
num.folds = 10
gamma = 3
f = 0.2
hmm.nStates = 5
hmm.dis = 'NORMAL'
repetitions = 1
Nb = 20

cat('Opportunity HMM\n')
cat(' >> '); print(class.labels)
cat(' >> '); print(names(subjects.features[[1]]$data[[1]]))

l = SimulateOpportunityALSample(subjects.features=subjects.features,
                                repetitions=repetitions, 
                                SubjectSamplingFunction=SubjectSamplingFunction,
                                Nb=Nb, class.labels=class.labels, 
                                num.folds=num.folds, 
                                online.confusion.metric=online.confusion.metric, 
                                offline.confusion.metric=offline.confusion.metric, 
                                hmm.nStates=hmm.nStates, 
                                hmm.dis=hmm.dis, 
                                gamma=gamma)
SaveObject(obj=l, dir='output/opportunity/hmm/gamma_3', var.name='sample')

online.confusion.metric = Online.FixedRate
l = SimulateOpportunityALSample(subjects.features=subjects.features,
                                repetitions=repetitions, 
                                SubjectSamplingFunction=SubjectSamplingFunction,
                                Nb=Nb, class.labels=class.labels, 
                                num.folds=num.folds, 
                                online.confusion.metric=online.confusion.metric, 
                                offline.confusion.metric=offline.confusion.metric, 
                                hmm.nStates=hmm.nStates, 
                                hmm.dis=hmm.dis, 
                                f=f)
SaveObject(obj=l, dir='output/opportunity/hmm/f_0.2', var.name='sample')

online.confusion.metric = Online.BMargin
gamma = 1
l = SimulateOpportunityALSample(subjects.features=subjects.features,
                                repetitions=repetitions, 
                                SubjectSamplingFunction=SubjectSamplingFunction,
                                Nb=Nb, class.labels=class.labels, 
                                num.folds=num.folds, 
                                online.confusion.metric=online.confusion.metric, 
                                offline.confusion.metric=offline.confusion.metric, 
                                hmm.nStates=hmm.nStates, 
                                hmm.dis=hmm.dis, 
                                gamma=gamma)
SaveObject(obj=l, dir='output/opportunity/hmm/gamma_1', var.name='sample')

gamma = 6
l = SimulateOpportunityALSample(subjects.features=subjects.features,
                                repetitions=repetitions, 
                                SubjectSamplingFunction=SubjectSamplingFunction,
                                Nb=Nb, class.labels=class.labels, 
                                num.folds=num.folds, 
                                online.confusion.metric=online.confusion.metric, 
                                offline.confusion.metric=offline.confusion.metric, 
                                hmm.nStates=hmm.nStates, 
                                hmm.dis=hmm.dis, 
                                gamma=gamma)
SaveObject(obj=l, dir='output/opportunity/hmm/gamma_6', var.name='sample')
