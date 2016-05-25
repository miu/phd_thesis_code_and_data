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
setwd('~/R/sf_ALTLAR/R/al.usc.had')

source('al.R')

library(doRedis)


## TODO
# load preprocessed data from disk

queue.name='al.online.queue'
registerDoRedis(queue=queue.name)

## B.NB
classifier.name = 'weka/classifiers/meta/Bagging'
weak.classifier.name = 'weka.classifiers.bayes.NaiveBayes'
num.iterations = 30
weka.control = Weka_control(W=weak.classifier.name, 
                            I=num.iterations)

#load(file='~/R/sf_ALTLAR/output/organize.usc.had/subjects.features.Rdata')
load(file='~/R/sf_ALTLAR/output/organize.pamap.2.protocol/subjects.features.Rdata')
subjects.features = obj

#class.labels = GetAllLabels(subjects.features=subjects.features)

num.folds = 10


#max.instances = 2000
#max.instances = 10
min.instances = 3

#repetitions = 20
#num.repetitions = 10
#num.repetitions = 20
num.repetitions = 10

gamma = 6
#subjects.features = subjects.features[-2] ## missing data

horizon.size = 2000
#horizon.size = 10
max.size = 200

history.size = 10
num.gamma.iterations = 50

max.sizes = c(200, 150, 100, 50, 25)
#max.sizes = c(150, 100, 50, 25)
#max.sizes = 10
#max.sizes = 2
for (max.size in max.sizes) {
  selection.function = B.margin.modulated
  l.al = SimulateForAllSubjects.unif(subjects.features=subjects.features, gamma=gamma,
                                     classifier.name=classifier.name, 
                                     weka.control=weka.control, num.folds=num.folds,
                                     num.repetitions=num.repetitions,
                                     selection.function=selection.function, max.size=max.size,
                                     horizon.size=horizon.size, history.size=history.size,
                                     num.gamma.iterations=num.gamma.iterations,
                                     monotone.increasing=F)
  #file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/l.al.unif.size', 
  #                  max.size, 'frame.Rdata', sep='.')
  #file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/l.al.unif.frame.Rdata'
  
  file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated/pamap/l.al.unif.size', 
                    max.size, 'frame.Rdata', sep='.')  
  
  save(l.al, file=file.name)
  cat(file.name, 'done\n')
  
  
  selection.function = Random.modulated
  l.rs = SimulateForAllSubjects.unif(subjects.features=subjects.features, gamma=gamma,
                                     classifier.name=classifier.name, 
                                     weka.control=weka.control, num.folds=num.folds,
                                     num.repetitions=num.repetitions,
                                     selection.function=selection.function, max.size=max.size,
                                     horizon.size=horizon.size, history.size=history.size,
                                     num.gamma.iterations=num.gamma.iterations,
                                     monotone.increasing=T)
#   file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/retry/l.rs.unif.size', 
#                     max.size, 'frame.Rdata', sep='.')
  #file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/retry/l.rs.unif.frame.Rdata'
  
  
  file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated/pamap/l.rs.unif.size', 
                    max.size, 'frame.Rdata', sep='.')
  
  save(l.rs, file=file.name)
  cat(file.name, 'done\n')
}

max.size = 200
#max.size = 2

#lambda = 3
lambdas = c(3, 2, 1, 0.5, 2.5, 1.5, 3.5)
#lambdas = 3
for (lambda in lambdas) {
  selection.function = B.margin.modulated
  l.al = SimulateForAllSubjects.exp(subjects.features=subjects.features, gamma=gamma,
                                    classifier.name=classifier.name, 
                                    weka.control=weka.control, num.folds=num.folds,
                                    num.repetitions=num.repetitions,
                                    selection.function=selection.function, max.size=max.size,
                                    horizon.size=horizon.size, history.size=history.size,
                                    num.gamma.iterations=num.gamma.iterations, lambda=lambda,
                                    monotone.increasing=F)
#   file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/retry/l.al.exp.lambda', 
#                     lambda, 'frame.Rdata', sep='.')
  #file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/l.al.exp.frame.Rdata'
  
  
  file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated/pamap/l.al.exp.lambda', 
                    lambda, 'frame.Rdata', sep='.')
  
  save(l.al, file=file.name)
  cat(file.name, 'done\n')
  
  
  selection.function = Random.modulated
  l.rs = SimulateForAllSubjects.exp(subjects.features=subjects.features, gamma=gamma,
                                    classifier.name=classifier.name, 
                                    weka.control=weka.control, num.folds=num.folds,
                                    num.repetitions=num.repetitions,
                                    selection.function=selection.function, max.size=max.size,
                                    horizon.size=horizon.size, history.size=history.size,
                                    num.gamma.iterations=num.gamma.iterations, lambda=lambda,
                                    monotone.increasing=T)
#   file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/retry/l.rs.exp.lambda', 
#                     lambda, 'frame.Rdata', sep='.')
  #file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/l.rs.exp.frame.Rdata'
  
  
  file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated/pamap/l.rs.exp.lambda', 
                    lambda, 'frame.Rdata', sep='.')  
  
  save(l.rs, file=file.name)
  cat(file.name, 'done\n')
}
