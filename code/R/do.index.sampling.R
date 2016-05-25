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

source('R/index.sampling.R')

library(doRedis)

source('R/do.organize.R')

#load(file='output/organize/subjects.features.Rdata')
#subjects.features = obj

#NUM_WORKERS = 4

queue.name='index.sampling.queue'
registerDoRedis(queue=queue.name)
#startLocalWorkers(n=NUM_WORKERS, queue=queue.name, timeout=1)
#CHUNK_SIZE = 1
#setChunkSize(CHUNK_SIZE)

###########################################################################
## Index Sampling
Nb = 200
Nb.trial = 5 * Nb
min.train.size = 3  
class.labels = GetAllLabels(subjects.features=subjects.features) 
num.folds = 10
repetitions = 10
rate = 1
SubjectSamplingFunction = SimulateSamplingForSubject.budget
max.episode.length = 10

model.builders = list()

#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/bayes/NaiveBayes',
#       weka.control=Weka_control(),
#       short.name='naive.bayes')
#
#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/functions/Logistic',
#       weka.control=Weka_control(),
#       short.name='logistic')
#
#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/trees/J48',
#       weka.control=Weka_control(),
#       short.name='c4.5')
#
weak.classifier.name = 'weka.classifiers.bayes.NaiveBayes'
bagging.iterations = 30
model.builders[[1 + length(model.builders)]] = 
  list(classifier.name='weka/classifiers/meta/Bagging',
       weka.control=Weka_control(W=weak.classifier.name, 
                                 I=bagging.iterations),
       short.name='bag.30.naive.bayes')

#weak.classifier.name = 'weka.classifiers.bayes.NaiveBayes'
#bagging.iterations = 20
#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/meta/Bagging',
#       weka.control=Weka_control(W=weak.classifier.name, 
#                                 I=bagging.iterations),
#       short.name='bag.20.naive.bayes')
#
#weak.classifier.name = 'weka.classifiers.bayes.NaiveBayes'
#bagging.iterations = 10
#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/meta/Bagging',
#       weka.control=Weka_control(W=weak.classifier.name, 
#                                 I=bagging.iterations),
#       short.name='bag.10.naive.bayes')
#
#weak.classifier.name = 'weka.classifiers.trees.J48'
#bagging.iterations = 30
#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/meta/Bagging',
#       weka.control=Weka_control(W=weak.classifier.name, 
#                                 I=bagging.iterations),
#       short.name='bag.30.c4.5')
#
#weak.classifier.name = 'weka.classifiers.trees.J48'
#bagging.iterations = 20
#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/meta/Bagging',
#       weka.control=Weka_control(W=weak.classifier.name, 
#                                 I=bagging.iterations),
#       short.name='bag.20.c4.5')
#
#weak.classifier.name = 'weka.classifiers.trees.J48'
#bagging.iterations = 10
#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/meta/Bagging',
#       weka.control=Weka_control(W=weak.classifier.name, 
#                                 I=bagging.iterations),
#       short.name='bag.10.c4.5')
#
#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/lazy/IBk',
#       weka.control=Weka_control(K=1),
#       short.name='knn.1')
#
#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/lazy/IBk',
#       weka.control=Weka_control(K=3),
#       short.name='knn.3')
#
#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/lazy/IBk',
#       weka.control=Weka_control(K=5),
#       short.name='knn.5')
#
#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/lazy/IBk',
#       weka.control=Weka_control(K=10),
#       short.name='knn.10')
#
#subjecst.features = subjects.features[1:2]
for (i in seq_along(model.builders)) {
  mb = model.builders[[i]]
  classifier.name = mb$classifier.name
  weka.control = mb$weka.control
  
  output.dir = paste('output/index.sampling/pamap.indoor.percom', mb$short.name,
                     sep='/')
  graphics.dir = paste('graphics/index.sampling/pamap.indoor.percom', mb$short.name,
                       sep='/')
    
  this.output.dir = paste(output.dir, 'exp.distr.1e-3', sep='/')
  this.graphics.dir = paste(graphics.dir, 'exp.distr.1e-3', sep='/')
  RunSimulationForSample(output.dir=this.output.dir, graphics.dir=this.graphics.dir, 
                         repetitions=repetitions, subjects.features=subjects.features, 
                         Nb=Nb, Nb.trial=Nb.trial, generator=ExponentialGenerator, 
                         classifier.name=classifier.name, 
                         weka.control=weka.control, min.train.size=min.train.size, 
                         class.labels=class.labels, num.folds=num.folds,
                         rate=1e-3, SubjectSamplingFunction=SubjectSamplingFunction,
                         max.episode.length=max.episode.length, to.stream=T)
  
  this.output.dir = paste(output.dir, 'exp.distr.1e3', sep='/')
  this.graphics.dir = paste(graphics.dir, 'exp.distr.1e3', sep='/')
  RunSimulationForSample(output.dir=this.output.dir, graphics.dir=this.graphics.dir, 
                         repetitions=repetitions, subjects.features=subjects.features, 
                         Nb=Nb, Nb.trial=Nb.trial, generator=ExponentialGenerator, 
                         classifier.name=classifier.name, 
                         weka.control=weka.control, min.train.size=min.train.size, 
                         class.labels=class.labels, num.folds=num.folds,
                         rate=1e3, SubjectSamplingFunction=SubjectSamplingFunction,
                         max.episode.length=max.episode.length, to.stream=T)
  
  this.output.dir = paste(output.dir, 'exp.distr.1', sep='/')
  this.graphics.dir = paste(graphics.dir, 'exp.distr.1', sep='/')
  RunSimulationForSample(output.dir=this.output.dir, graphics.dir=this.graphics.dir, 
                         repetitions=repetitions, subjects.features=subjects.features, 
                         Nb=Nb, Nb.trial=Nb.trial, generator=ExponentialGenerator, 
                         classifier.name=classifier.name, 
                         weka.control=weka.control, min.train.size=min.train.size, 
                         class.labels=class.labels, num.folds=num.folds,
                         rate=1, SubjectSamplingFunction=SubjectSamplingFunction,
                         max.episode.length=max.episode.length, to.stream=T)
  
  this.output.dir = paste(output.dir, 'unif.distr', sep='/')
  this.graphics.dir = paste(graphics.dir, 'unif.distr', sep='/')
  RunSimulationForSample(output.dir=this.output.dir, graphics.dir=this.graphics.dir, 
                         repetitions=repetitions, subjects.features=subjects.features, 
                         Nb=Nb, Nb.trial=Nb.trial, generator=UniformGenerator, 
                         classifier.name=classifier.name, 
                         weka.control=weka.control, min.train.size=min.train.size, 
                         class.labels=class.labels, num.folds=num.folds,
                         SubjectSamplingFunction=SubjectSamplingFunction,
                         max.episode.length=max.episode.length, to.stream=T)
  
  this.output.dir = paste(output.dir, 'upfront.distr', sep='/')
  this.graphics.dir = paste(graphics.dir, 'upfront.distr', sep='/')
  RunSimulationForSample(output.dir=this.output.dir, graphics.dir=this.graphics.dir, 
                         repetitions=1, subjects.features=subjects.features, 
                         Nb=Nb, Nb.trial=Nb.trial, generator=UpfrontGenerator, 
                         classifier.name=classifier.name, 
                         weka.control=weka.control, min.train.size=min.train.size, 
                         class.labels=class.labels, num.folds=num.folds,
                         SubjectSamplingFunction=SubjectSamplingFunction,
                         max.episode.length=max.episode.length, to.stream=T)
  
  fs = c(0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.02, 0.01)
  kernel.width = 3
  min.episode.length = kernel.width
  for (f in fs) {
    this.output.dir = paste('fixed.f/f', f, sep='_')
    path.no.seg = paste(output.dir, this.output.dir, sep='/')
    RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
                           output.dir=path.no.seg, 
                           graphics.dir=NULL,
                           repetitions=repetitions, classifier.name=classifier.name,
                           weka.control=weka.control, min.train.size=min.train.size, 
                           class.labels=class.labels, num.folds=num.folds, 
                           recall.size=1, 
                           max.episode.length=max.episode.length, 
                           min.episode.length=min.episode.length,
                           online.confusion.metric=Online.FixedRate, 
                           offline.confusion.metric=Offline.Random,
                           unskewed.threshold=NULL,
                           SubjectSamplingFunction=SimulateHybridSamplingForSubject,
                           f=f)
  }
  
}

#stop('  Fixed-time budget ended successfully.')

#####################################################################
## Active Learning
#subjects.features = subjects.features[1:2]
#Nb = 200
Nb.trial = 5 * Nb
min.train.size = 3  
class.labels = GetAllLabels(subjects.features=subjects.features) 
num.folds = 10
#repetitions = 10
max.episode.length = 10

model.builders = list()

weak.classifier.name = 'weka.classifiers.bayes.NaiveBayes'
bagging.iterations = 30
model.builders[[1 + length(model.builders)]] = 
  list(classifier.name='weka/classifiers/meta/Bagging',
       weka.control=Weka_control(W=weak.classifier.name, 
                                 I=bagging.iterations),
       short.name='bag.30.naive.bayes')

model.builders[[1 + length(model.builders)]] = 
  list(classifier.name='weka/classifiers/bayes/NaiveBayes',
       weka.control=Weka_control(),
       short.name='naive.bayes')

#weak.classifier.name = 'weka.classifiers.trees.J48'
#bagging.iterations = 30
#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/meta/Bagging',
#       weka.control=Weka_control(W=weak.classifier.name, 
#                                 I=bagging.iterations),
#       short.name='bag.30.c4.5')

model.builders[[1 + length(model.builders)]] = 
  list(classifier.name='weka/classifiers/lazy/IBk',
       weka.control=Weka_control(K=3),
       short.name='knn.3')

model.builders[[1 + length(model.builders)]] = 
  list(classifier.name='weka/classifiers/lazy/IBk',
       weka.control=Weka_control(K=5),
       short.name='knn.5')

#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/lazy/IBk',
#       weka.control=Weka_control(K=10),
#       short.name='knn.10')

#weak.classifier.name = 'weka.classifiers.trees.J48'
#bagging.iterations = 20
#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/meta/Bagging',
#       weka.control=Weka_control(W=weak.classifier.name, 
#                                 I=bagging.iterations),
#       short.name='bag.20.c4.5')

#weak.classifier.name = 'weka.classifiers.trees.J48'
#bagging.iterations = 10
#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/meta/Bagging',
#       weka.control=Weka_control(W=weak.classifier.name, 
#                                 I=bagging.iterations),
#       short.name='bag.10.c4.5')

#weak.classifier.name = 'weka.classifiers.bayes.NaiveBayes'
#bagging.iterations = 20
#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/meta/Bagging',
#       weka.control=Weka_control(W=weak.classifier.name, 
#                                 I=bagging.iterations),
#       short.name='bag.20.naive.bayes')

#weak.classifier.name = 'weka.classifiers.bayes.NaiveBayes'
#bagging.iterations = 10
#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/meta/Bagging',
#       weka.control=Weka_control(W=weak.classifier.name, 
#                                 I=bagging.iterations),
#       short.name='bag.10.naive.bayes')

#model.builders[[1 + length(model.builders)]] = 
#  list(classifier.name='weka/classifiers/functions/Logistic',
#       weka.control=Weka_control(),
#       short.name='logistic')

#subjects.features = subjects.features[1]

kernel.width = 3
min.train.size = 3
#threshold = 0.2
#subject.i = 1
#classifier.name = 'weka/classifiers/bayes/NaiveBayes'
#weka.control = Weka_control()

classifier.name='weka/classifiers/meta/Bagging'
weak.classifier.name = 'weka.classifiers.bayes.NaiveBayes'
bagging.iterations = 30
weka.control = Weka_control(W=weak.classifier.name, I=bagging.iterations)

online.confusion.metric = Online.BMargin
offline.confusion.metric = Offline.Confidence
gamma = 1
#l = SimululateSegmentationSamplingForSubject(subjects.features=subjects.features, 
#                                             subject.i=subject.i, Nb=Nb, 
#                                             classifier.name=classifier.name, 
#                                             weka.control=weka.control, 
#                                             min.train.size=min.train.size, 
#                                             class.labels=class.labels, 
#                                             num.folds=num.folds, 
#                                             max.episode.length=max.episode.length, 
#                                             online.confusion.metric=online.confusion.metric, 
#                                             offline.confusion.metric=offline.confusion.metric, 
#                                             kernel.width=kernel.width, threshold=threshold,
#                                             gamma=gamma)

#SubjectSamplingFunction=SimululateSegmentationSamplingForSubject
#l = SimulateSegmentationForSample(subjects.features=subjects.features, 
#                                  repetitions=repetitions,
#                                  SubjectSamplingFunction=SubjectSamplingFunction,
#                                  Nb=Nb, 
#                                  classifier.name=classifier.name, 
#                                  weka.control=weka.control, 
#                                  min.train.size=min.train.size, 
#                                  class.labels=class.labels, 
#                                  num.folds=num.folds, 
#                                  max.episode.length=max.episode.length, 
#                                  online.confusion.metric=online.confusion.metric, 
#                                  offline.confusion.metric=offline.confusion.metric, 
#                                  kernel.width=kernel.width, threshold=threshold,
#                                  gamma=gamma)

root.path = 'output/segmentation'
SubjectSamplingFunction=SimululateSegmentationSamplingForSubject
#SubjectSamplingFunction=SimululateSegmentationSamplingForSubject.exclude.min.conf
#SubjectSamplingFunction = SimululateSegmentationSamplingForSubject.mode.conf
#SubjectSamplingFunction = SimululateSegmentationSamplingForSubject.mode.conf.cls

gammas = c(0.1, 0.2, 0.5, 1, 2, 3, 4, 5, 6)
#gammas = c(1, 2, 3, 0.1, 0.2, 0.5, 4, 5, 6)
#gammas = c(1, 2, 3)
#gammas = 0.1
threshold = 0.2
online.confusion.metric = Online.BMargin
offline.confusion.metric = Offline.Confidence
#offline.confusion.metric = Offline.Margin
abs.path = paste(root.path, 'varying.gamma.mean', sep='/')
recall.size = 6
min.episode.length = kernel.width
for (gamma in gammas) {
  #this.dir = paste('gamma', gamma, sep='_')
  #path = paste(abs.path, this.dir, sep='/')
  #l = SimulateSegmentationForSample(subjects.features=subjects.features, 
  #                                  repetitions=repetitions,
  #                                  SubjectSamplingFunction=SubjectSamplingFunction,
  #                                  Nb=Nb, 
  #                                  classifier.name=classifier.name, 
  #                                  weka.control=weka.control, 
  #                                  min.train.size=min.train.size, 
  #                                  class.labels=class.labels, 
  #                                  num.folds=num.folds, 
  #                                  max.episode.length=max.episode.length, 
  #                                  online.confusion.metric=online.confusion.metric, 
  #                                  offline.confusion.metric=offline.confusion.metric, 
  #                                  kernel.width=kernel.width, threshold=threshold,
  #                                  gamma=gamma)
  #SaveObject(obj=l, var.name='sample', dir=path)
  
  this.dir = paste('with.recall/gamma', gamma, sep='_')
  path.no.seg = paste(abs.path, this.dir, sep='/')
  RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
                         output.dir=path.no.seg, 
                         graphics.dir=NULL,
                         repetitions=repetitions, classifier.name=classifier.name,
                         weka.control=weka.control, min.train.size=min.train.size, 
                         class.labels=class.labels, num.folds=num.folds, 
                         recall.size=recall.size, 
                         max.episode.length=max.episode.length, 
                         min.episode.length=min.episode.length,
                         online.confusion.metric=online.confusion.metric, 
                         offline.confusion.metric=offline.confusion.metric,
                         unskewed.threshold=NULL,
                         SubjectSamplingFunction=SimulateHybridSamplingForSubject,
                         gamma=gamma)
  
  this.dir = paste('without.recall/gamma', gamma, sep='_')
  path.no.seg = paste(abs.path, this.dir, sep='/')
  RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
                         output.dir=path.no.seg, 
                         graphics.dir=NULL,
                         repetitions=repetitions, classifier.name=classifier.name,
                         weka.control=weka.control, min.train.size=min.train.size, 
                         class.labels=class.labels, num.folds=num.folds, 
                         recall.size=1, 
                         max.episode.length=max.episode.length, 
                         min.episode.length=min.episode.length,
                         online.confusion.metric=online.confusion.metric, 
                         offline.confusion.metric=offline.confusion.metric,
                         unskewed.threshold=NULL,
                         SubjectSamplingFunction=SimulateHybridSamplingForSubject,
                         gamma=gamma)
}

stop('Enough! (nothing to worry)')

thresholds = seq(from=0.4, to=1, by=0.2)
#thresholds = seq(from=0.1, to=0.5, by=0.1)
#thresholds = c(0.1, 0.2)
gamma = 1
online.confusion.metric = Online.BMargin
offline.confusion.metric = Offline.Confidence
abs.path = paste(root.path, 'varying.threshold', sep='/')
#for (threshold in thresholds) {
#  this.dir = paste('threshold', threshold, sep='_')
#  path = paste(abs.path, this.dir, sep='/')
#  l = SimulateSegmentationForSample(subjects.features=subjects.features, 
#                                    repetitions=repetitions,
#                                    SubjectSamplingFunction=SubjectSamplingFunction,
#                                    Nb=Nb, 
#                                    classifier.name=classifier.name, 
#                                    weka.control=weka.control, 
#                                    min.train.size=min.train.size, 
#                                    class.labels=class.labels, 
#                                    num.folds=num.folds, 
#                                    max.episode.length=max.episode.length, 
#                                    online.confusion.metric=online.confusion.metric, 
#                                    offline.confusion.metric=offline.confusion.metric, 
#                                    kernel.width=kernel.width, threshold=threshold,
#                                    gamma=gamma)
#  SaveObject(obj=l, var.name='sample', dir=path)
#}

fs = c(0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.02, 0.01)
#fs = c(0.9, 0.8)
threshold = 0.2
online.confusion.metric = Online.FixedRate
offline.confusion.metric = Offline.Confidence
abs.path = paste(root.path, 'varying.f.mode', sep='/')
for (f in fs) {
  this.dir = paste('f', f, sep='_')
  path = paste(abs.path, this.dir, sep='/')
  l = SimulateSegmentationForSample(subjects.features=subjects.features, 
                                    repetitions=repetitions,
                                    SubjectSamplingFunction=SubjectSamplingFunction,
                                    Nb=Nb, 
                                    classifier.name=classifier.name, 
                                    weka.control=weka.control, 
                                    min.train.size=min.train.size, 
                                    class.labels=class.labels, 
                                    num.folds=num.folds, 
                                    max.episode.length=max.episode.length, 
                                    online.confusion.metric=online.confusion.metric, 
                                    offline.confusion.metric=offline.confusion.metric, 
                                    kernel.width=kernel.width, threshold=threshold,
                                    f=f)
  SaveObject(obj=l, var.name='sample', dir=path)
  
  #this.dir = paste('without.recall/f', f, sep='_')
  #path.no.seg = paste(abs.path, this.dir, sep='/')
  #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
  #                       output.dir=path.no.seg, 
  #                       graphics.dir=NULL,
  #                       repetitions=repetitions, classifier.name=classifier.name,
  #                       weka.control=weka.control, min.train.size=min.train.size, 
  #                       class.labels=class.labels, num.folds=num.folds, 
  #                       recall.size=1, 
  #                       max.episode.length=max.episode.length, 
  #                       min.episode.length=min.episode.length,
  #                       online.confusion.metric=online.confusion.metric, 
  #                       offline.confusion.metric=offline.confusion.metric,
  #                       unskewed.threshold=NULL,
  #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject,
  #                       f=f)
}

gammas = c(4, 5, 6)
threshold = 0.2
online.confusion.metric = Online.BMargin
offline.confusion.metric = Offline.Confidence
#offline.confusion.metric = Offline.Margin
abs.path = paste(root.path, 'varying.gamma.min', sep='/')
#for (gamma in gammas) {
#  this.dir = paste('gamma', gamma, sep='_')
#  path = paste(abs.path, this.dir, sep='/')
#  l = SimulateSegmentationForSample(subjects.features=subjects.features, 
#                                    repetitions=repetitions,
#                                    SubjectSamplingFunction=SubjectSamplingFunction,
#                                    Nb=Nb, 
#                                    classifier.name=classifier.name, 
#                                    weka.control=weka.control, 
#                                    min.train.size=min.train.size, 
#                                    class.labels=class.labels, 
#                                    num.folds=num.folds, 
#                                    max.episode.length=max.episode.length, 
#                                    online.confusion.metric=online.confusion.metric, 
#                                    offline.confusion.metric=offline.confusion.metric, 
#                                    kernel.width=kernel.width, threshold=threshold,
#                                    gamma=gamma)
#  SaveObject(obj=l, var.name='sample', dir=path)
#}

stop('Stopping before loop. Nothing to worry about\n')

for (i in seq_along(model.builders)) {
  mb = model.builders[[i]]
  classifier.name = mb$classifier.name
  weka.control = mb$weka.control
  
  output.dir = paste('output/index.sampling.active.fixed/pamap.indoor', mb$short.name,
                     sep='/')
  graphics.dir = paste('graphics/index.sampling.active.fixed/pamap.indoor', mb$short.name,
                       sep='/')
  
  unskewed.dir = paste('unskewed', unskewed.threshold, sep='.')
  
  this.output.dir = paste(output.dir, unskewed.dir, sep='/')
  this.graphics.dir = paste(graphics.dir, unskewed.dir, sep='/')
  #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
  #                       output.dir=this.output.dir, 
  #                       graphics.dir=this.graphics.dir,
  #                       repetitions=repetitions, classifier.name=classifier.name,
  #                       weka.control=weka.control, min.train.size=min.train.size, 
  #                       class.labels=class.labels, num.folds=num.folds, 
  #                       recall.size=recall.size, 
  #                       max.episode.length=max.episode.length, 
  #                       online.confusion.metric=Online.Unskewed, 
  #                       offline.confusion.metric=Offline.Confidence,
  #                       unskewed.threshold=unskewed.threshold,
  #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject)
    
  this.output.dir = paste(output.dir, 'b.margin', sep='/')
  this.graphics.dir = paste(graphics.dir, 'b.margin', sep='/')
  for (gamma in gammas) {
    dir.name = paste('gamma', gamma, sep='.')
    gamma.output.dir = paste(this.output.dir, dir.name, sep='/')
    gamma.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=gamma.output.dir, 
    #                       graphics.dir=gamma.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=recall.size, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.BMargin, 
    #                       offline.confusion.metric=Offline.Confidence,
    #                       gamma=gamma,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject)
    
    dir.name = paste('batch', 'gamma', gamma, 'middle', sep='.')
    batch.output.dir = paste(this.output.dir, dir.name, sep='/')
    batch.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=batch.output.dir, 
    #                       graphics.dir=batch.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=recall.size, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.BMargin, 
    #                       offline.confusion.metric=Offline.Confidence,
    #                       gamma=gamma,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject.batch,
    #                       batch.factor.f=Batch.Confidence.Factor.middle)
    
    dir.name = paste('batch', 'gamma', gamma, 'end', sep='.')
    batch.output.dir = paste(this.output.dir, dir.name, sep='/')
    batch.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=batch.output.dir, 
    #                       graphics.dir=batch.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=recall.size, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.BMargin, 
    #                       offline.confusion.metric=Offline.Confidence,
    #                       gamma=gamma,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject.batch,
    #                       batch.factor.f=Batch.Confidence.Factor.end)
    
    dir.name = paste('batch', 'gamma', gamma, 'beginning', sep='.')
    batch.output.dir = paste(this.output.dir, dir.name, sep='/')
    batch.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=batch.output.dir, 
    #                       graphics.dir=batch.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=recall.size, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.BMargin, 
    #                       offline.confusion.metric=Offline.Confidence,
    #                       gamma=gamma,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject.batch,
    #                       batch.factor.f=Batch.Confidence.Factor.begining)
    
    dir.name = paste('batch', 'gamma', gamma, 'unif', sep='.')
    batch.output.dir = paste(this.output.dir, dir.name, sep='/')
    batch.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=batch.output.dir, 
    #                       graphics.dir=batch.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=recall.size, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.BMargin, 
    #                       offline.confusion.metric=Offline.Confidence,
    #                       gamma=gamma,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject.batch,
    #                       batch.factor.f=Batch.Confidence.Factor.unif)
    
    ## population data
    dir.name = paste('batch', 'gamma', gamma, 'pop', 'random', sep='.')
    batch.output.dir = paste(this.output.dir, dir.name, sep='/')
    batch.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample.pop(subjects.features=subjects.features, Nb=Nb, 
    #                           output.dir=batch.output.dir, 
    #                           graphics.dir=batch.graphics.dir,
    #                           repetitions=repetitions, classifier.name=classifier.name,
    #                           weka.control=weka.control, min.train.size=min.train.size, 
    #                           class.labels=class.labels, num.folds=num.folds, 
    #                           recall.size=recall.size, 
    #                           max.episode.length=max.episode.length, 
    #                           online.confusion.metric=Online.BMargin, 
    #                           offline.confusion.metric=Offline.Confidence,
    #                           gamma=gamma,
    #                           SubjectSamplingFunction=SimulateHybridSamplingForSubject.pop,
    #                           pop.select.f=Pop.Select.Random, decay.f=Decay.exp, 
    #                           ne=5)
  }
  
  ## minimum memory: 1
  this.output.dir = paste(output.dir, 'b.margin.min.mem', sep='/')
  this.graphics.dir = paste(graphics.dir, 'b.margin.min.mem', sep='/')
  for (gamma in gammas) {
    dir.name = paste('gamma', gamma, sep='.')
    gamma.output.dir = paste(this.output.dir, dir.name, sep='/')
    gamma.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
                           output.dir=gamma.output.dir, 
                           graphics.dir=gamma.graphics.dir,
                           repetitions=repetitions, classifier.name=classifier.name,
                           weka.control=weka.control, min.train.size=min.train.size, 
                           class.labels=class.labels, num.folds=num.folds, 
                           recall.size=1, 
                           max.episode.length=max.episode.length, 
                           online.confusion.metric=Online.BMargin, 
                           offline.confusion.metric=Offline.Confidence,
                           gamma=gamma,
                           SubjectSamplingFunction=SimulateHybridSamplingForSubject)
    
    dir.name = paste('batch', 'gamma', gamma, 'middle', sep='.')
    batch.output.dir = paste(this.output.dir, dir.name, sep='/')
    batch.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=batch.output.dir, 
    #                       graphics.dir=batch.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=recall.size, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.BMargin, 
    #                       offline.confusion.metric=Offline.Confidence,
    #                       gamma=gamma,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject.batch,
    #                       batch.factor.f=Batch.Confidence.Factor.middle)
    
    dir.name = paste('batch', 'gamma', gamma, 'end', sep='.')
    batch.output.dir = paste(this.output.dir, dir.name, sep='/')
    batch.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=batch.output.dir, 
    #                       graphics.dir=batch.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=recall.size, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.BMargin, 
    #                       offline.confusion.metric=Offline.Confidence,
    #                       gamma=gamma,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject.batch,
    #                       batch.factor.f=Batch.Confidence.Factor.end)
    
    dir.name = paste('batch', 'gamma', gamma, 'beginning', sep='.')
    batch.output.dir = paste(this.output.dir, dir.name, sep='/')
    batch.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=batch.output.dir, 
    #                       graphics.dir=batch.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=recall.size, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.BMargin, 
    #                       offline.confusion.metric=Offline.Confidence,
    #                       gamma=gamma,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject.batch,
    #                       batch.factor.f=Batch.Confidence.Factor.begining)
    
    dir.name = paste('batch', 'gamma', gamma, 'unif', sep='.')
    batch.output.dir = paste(this.output.dir, dir.name, sep='/')
    batch.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=batch.output.dir, 
    #                       graphics.dir=batch.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=recall.size, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.BMargin, 
    #                       offline.confusion.metric=Offline.Confidence,
    #                       gamma=gamma,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject.batch,
    #                       batch.factor.f=Batch.Confidence.Factor.unif)
  }
  
  this.output.dir = paste(output.dir, 'fixed', sep='/')
  this.graphics.dir = paste(graphics.dir, 'fixed', sep='/')
  for (f in freqs) {
    dir.name = paste('f', f, sep='.')
    fixed.f.output.dir = paste(this.output.dir, dir.name, sep='/')
    fixed.f.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=fixed.f.output.dir, 
    #                       graphics.dir=fixed.f.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=recall.size, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.FixedRate, 
    #                       offline.confusion.metric=Offline.Random,
    #                       f=f,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject)
    
    dir.name = paste('batch', 'f', f, 'middle', sep='.')
    fixed.f.batch.output.dir = paste(this.output.dir, dir.name, sep='/')
    fixed.f.batch.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=fixed.f.batch.output.dir, 
    #                       graphics.dir=fixed.f.batch.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=recall.size, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.FixedRate, 
    #                       offline.confusion.metric=Offline.Random,
    #                       f=f,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject.batch,
    #                       batch.factor.f=Batch.Confidence.Factor.middle)
    
    dir.name = paste('batch', 'f', f, 'beginning', sep='.')
    fixed.f.batch.output.dir = paste(this.output.dir, dir.name, sep='/')
    fixed.f.batch.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=fixed.f.batch.output.dir, 
    #                       graphics.dir=fixed.f.batch.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=recall.size, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.FixedRate, 
    #                       offline.confusion.metric=Offline.Random,
    #                       f=f,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject.batch,
    #                       batch.factor.f=Batch.Confidence.Factor.begining)
    
    dir.name = paste('batch', 'f', f, 'end', sep='.')
    fixed.f.batch.output.dir = paste(this.output.dir, dir.name, sep='/')
    fixed.f.batch.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=fixed.f.batch.output.dir, 
    #                       graphics.dir=fixed.f.batch.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=recall.size, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.FixedRate, 
    #                       offline.confusion.metric=Offline.Random,
    #                       f=f,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject.batch,
    #                       batch.factor.f=Batch.Confidence.Factor.end)
    
    dir.name = paste('batch', 'f', f, 'unif', sep='.')
    fixed.f.batch.output.dir = paste(this.output.dir, dir.name, sep='/')
    fixed.f.batch.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=fixed.f.batch.output.dir, 
    #                       graphics.dir=fixed.f.batch.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=recall.size, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.FixedRate, 
    #                       offline.confusion.metric=Offline.Random,
    #                       f=f,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject.batch,
    #                       batch.factor.f=Batch.Confidence.Factor.unif)
  }
  
  ## minimum memory: 1
  this.output.dir = paste(output.dir, 'fixed.min.mem', sep='/')
  this.graphics.dir = paste(graphics.dir, 'fixed.min.mem', sep='/')
  for (f in freqs) {
    dir.name = paste('f', f, sep='.')
    fixed.f.output.dir = paste(this.output.dir, dir.name, sep='/')
    fixed.f.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
                           output.dir=fixed.f.output.dir, 
                           graphics.dir=fixed.f.graphics.dir,
                           repetitions=repetitions, classifier.name=classifier.name,
                           weka.control=weka.control, min.train.size=min.train.size, 
                           class.labels=class.labels, num.folds=num.folds, 
                           recall.size=1, 
                           max.episode.length=max.episode.length, 
                           online.confusion.metric=Online.FixedRate, 
                           offline.confusion.metric=Offline.Random,
                           f=f,
                           SubjectSamplingFunction=SimulateHybridSamplingForSubject)
    
    dir.name = paste('batch', 'f', f, 'middle', sep='.')
    fixed.f.batch.output.dir = paste(this.output.dir, dir.name, sep='/')
    fixed.f.batch.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=fixed.f.batch.output.dir, 
    #                       graphics.dir=fixed.f.batch.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=1, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.FixedRate, 
    #                       offline.confusion.metric=Offline.Random,
    #                       f=f,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject.batch,
    #                       batch.factor.f=Batch.Confidence.Factor.middle)
    
    dir.name = paste('batch', 'f', f, 'beginning', sep='.')
    fixed.f.batch.output.dir = paste(this.output.dir, dir.name, sep='/')
    fixed.f.batch.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=fixed.f.batch.output.dir, 
    #                       graphics.dir=fixed.f.batch.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=1, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.FixedRate, 
    #                       offline.confusion.metric=Offline.Random,
    #                       f=f,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject.batch,
    #                       batch.factor.f=Batch.Confidence.Factor.begining)
    
    dir.name = paste('batch', 'f', f, 'end', sep='.')
    fixed.f.batch.output.dir = paste(this.output.dir, dir.name, sep='/')
    fixed.f.batch.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=fixed.f.batch.output.dir, 
    #                       graphics.dir=fixed.f.batch.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=1, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.FixedRate, 
    #                       offline.confusion.metric=Offline.Random,
    #                       f=f,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject.batch,
    #                       batch.factor.f=Batch.Confidence.Factor.end)
    
    dir.name = paste('batch', 'f', f, 'unif', sep='.')
    fixed.f.batch.output.dir = paste(this.output.dir, dir.name, sep='/')
    fixed.f.batch.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')
    #RunSimulationForSample(subjects.features=subjects.features, Nb=Nb, 
    #                       output.dir=fixed.f.batch.output.dir, 
    #                       graphics.dir=fixed.f.batch.graphics.dir,
    #                       repetitions=repetitions, classifier.name=classifier.name,
    #                       weka.control=weka.control, min.train.size=min.train.size, 
    #                       class.labels=class.labels, num.folds=num.folds, 
    #                       recall.size=1, 
    #                       max.episode.length=max.episode.length, 
    #                       online.confusion.metric=Online.FixedRate, 
    #                       offline.confusion.metric=Offline.Random,
    #                       f=f,
    #                       SubjectSamplingFunction=SimulateHybridSamplingForSubject.batch,
    #                       batch.factor.f=Batch.Confidence.Factor.unif)
  }
  
  ## fixed budget sampling
  this.output.dir = paste(output.dir, 'budget', sep='/')
  this.graphics.dir = paste(graphics.dir, 'budget', sep='/')
  
  dir.name = paste('exp', 'distr', 1e-3, sep='.')
  budget.output.dir = paste(this.output.dir, dir.name, sep='/')
  budget.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')  
  #RunSimulationForSample.budget(output.dir=budget.output.dir, 
  #                              graphics.dir=budget.graphics.dir, 
  #                              repetitions=repetitions, subjects.features=subjects.features, 
  #                              Nb=Nb, Nb.trial=Nb.trial, generator=ExponentialGenerator, 
  #                              classifier.name=classifier.name, 
  #                              weka.control=weka.control, min.train.size=min.train.size, 
  #                              class.labels=class.labels, num.folds=num.folds,
  #                              rate=1e-3, 
  #                              SubjectSamplingFunction=SimulateSamplingForSubject.budget, 
  #                              to.stream=T, max.episode.length=max.episode.length)
  
  dir.name = paste('exp', 'distr', 1, sep='.')
  budget.output.dir = paste(this.output.dir, dir.name, sep='/')
  budget.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')  
  #RunSimulationForSample.budget(output.dir=budget.output.dir, 
  #                              graphics.dir=budget.graphics.dir, 
  #                              repetitions=repetitions, subjects.features=subjects.features, 
  #                              Nb=Nb, Nb.trial=Nb.trial, generator=ExponentialGenerator, 
  #                              classifier.name=classifier.name, 
  #                              weka.control=weka.control, min.train.size=min.train.size, 
  #                              class.labels=class.labels, num.folds=num.folds,
  #                              rate=1, 
  #                              SubjectSamplingFunction=SimulateSamplingForSubject.budget, 
  #                              to.stream=T, max.episode.length=max.episode.length)
  
  dir.name = paste('exp', 'distr', 1e+3, sep='.')
  budget.output.dir = paste(this.output.dir, dir.name, sep='/')
  budget.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')  
  #RunSimulationForSample.budget(output.dir=budget.output.dir, 
  #                              graphics.dir=budget.graphics.dir, 
  #                              repetitions=repetitions, subjects.features=subjects.features, 
  #                              Nb=Nb, Nb.trial=Nb.trial, generator=ExponentialGenerator, 
  #                              classifier.name=classifier.name, 
  #                              weka.control=weka.control, min.train.size=min.train.size, 
  #                              class.labels=class.labels, num.folds=num.folds,
  #                              rate=1e+3, 
  #                              SubjectSamplingFunction=SimulateSamplingForSubject.budget, 
  #                              to.stream=T, max.episode.length=max.episode.length)
  
  dir.name = paste('unif', 'distr', sep='.')
  budget.output.dir = paste(this.output.dir, dir.name, sep='/')
  budget.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')  
  #RunSimulationForSample.budget(output.dir=budget.output.dir, 
  #                              graphics.dir=budget.graphics.dir, 
  #                              repetitions=repetitions, subjects.features=subjects.features, 
  #                              Nb=Nb, Nb.trial=Nb.trial, generator=UniformGenerator, 
  #                              classifier.name=classifier.name, 
  #                              weka.control=weka.control, min.train.size=min.train.size, 
  #                              class.labels=class.labels, num.folds=num.folds,
  #                              SubjectSamplingFunction=SimulateSamplingForSubject.budget, 
  #                              to.stream=T, max.episode.length=max.episode.length)
  
  dir.name = paste('upfront', 'distr', sep='.')
  budget.output.dir = paste(this.output.dir, dir.name, sep='/')
  budget.graphics.dir = paste(this.graphics.dir, dir.name, sep='/')  
  #RunSimulationForSample.budget(output.dir=budget.output.dir, 
  #                              graphics.dir=budget.graphics.dir, 
  #                              repetitions=repetitions, subjects.features=subjects.features, 
  #                              Nb=Nb, Nb.trial=Nb.trial, generator=UpfrontGenerator, 
  #                              classifier.name=classifier.name, 
  #                              weka.control=weka.control, min.train.size=min.train.size, 
  #                              class.labels=class.labels, num.folds=num.folds,
  #                              SubjectSamplingFunction=SimulateSamplingForSubject.budget, 
  #                              to.stream=T, max.episode.length=max.episode.length)
}
