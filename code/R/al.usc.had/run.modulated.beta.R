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

load(file='~/R/sf_ALTLAR/output/organize.usc.had/subjects.features.Rdata')
#load(file='~/R/sf_ALTLAR/output/organize.pamap.2.protocol/subjects.features.Rdata')
subjects.features = obj

#class.labels = GetAllLabels(subjects.features=subjects.features)

num.folds = 10


#max.instances = 2000
#max.instances = 10
min.instances = 3

#repetitions = 20
num.repetitions = 10
#num.repetitions = 20
#num.repetitions = 3

gamma = 6
#subjects.features = subjects.features[-2] ## missing data

horizon.size = 2000
#horizon.size = 10

max.size = 200
#max.size = 3

history.size = 10
num.gamma.iterations = 50

betas = c(0.1, 0.5, 1, 2.5, 5, 10, 50, 100, 500, 1000, 5000, 1e4)

for (beta in betas) {
  selection.function = B.margin.modulated
  l.al = SimulateForAllSubjects.unif.beta(subjects.features=subjects.features, gamma=gamma,
                                          classifier.name=classifier.name, 
                                          weka.control=weka.control, num.folds=num.folds,
                                          num.repetitions=num.repetitions,
                                          selection.function=selection.function, max.size=max.size,
                                          horizon.size=horizon.size, history.size=history.size,
                                          num.gamma.iterations=num.gamma.iterations,
                                          monotone.increasing=F, beta=beta)
  #file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/l.al.unif.size', 
  #                  max.size, 'frame.Rdata', sep='.')
  #file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/l.al.unif.frame.Rdata'
  
  #file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/pamap/l.al.unif.size', 
  #                  max.size, 'beta', beta, 'frame.Rdata', sep='.')  
  
  file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/usc-had/l.al.unif.size', 
                    max.size, 'beta', beta, 'frame.Rdata', sep='.')  
  
  save(l.al, file=file.name)
  cat(file.name, 'done\n')
  
  
  #selection.function = Random.modulated
  #l.rs = SimulateForAllSubjects.unif.beta(subjects.features=subjects.features, gamma=gamma,
  #                                        classifier.name=classifier.name, 
  #                                        weka.control=weka.control, num.folds=num.folds,
  #                                        num.repetitions=num.repetitions,
  #                                        selection.function=selection.function, max.size=max.size,
  #                                        horizon.size=horizon.size, history.size=history.size,
  #                                        num.gamma.iterations=num.gamma.iterations,
  #                                        monotone.increasing=T, beta=beta)
  #   file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/retry/l.rs.unif.size', 
  #                     max.size, 'frame.Rdata', sep='.')
  #file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/retry/l.rs.unif.frame.Rdata'
  
  
  #file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/pamap/l.rs.unif.size', 
  #                  max.size, 'frame.Rdata', sep='.')
  #
  #save(l.rs, file=file.name)
  #cat(file.name, 'done\n')

  lambda = 3
  
  selection.function = B.margin.modulated
  l.al = SimulateForAllSubjects.exp.beta(subjects.features=subjects.features, gamma=gamma,
                                         classifier.name=classifier.name, 
                                         weka.control=weka.control, num.folds=num.folds,
                                         num.repetitions=num.repetitions,
                                         selection.function=selection.function, max.size=max.size,
                                         horizon.size=horizon.size, history.size=history.size,
                                         num.gamma.iterations=num.gamma.iterations, lambda=lambda,
                                         monotone.increasing=F, beta=beta)
  #   file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/retry/l.al.exp.lambda', 
  #                     lambda, 'frame.Rdata', sep='.')
  #file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/l.al.exp.frame.Rdata'
  
  
  #file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/pamap/l.al.exp.lambda', 
  #                  lambda, 'beta', beta, 'frame.Rdata', sep='.')
  
  file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/usc-had/l.al.exp.lambda', 
                    lambda, 'beta', beta, 'frame.Rdata', sep='.')
  
  save(l.al, file=file.name)
  cat(file.name, 'done\n')
  
  
  #selection.function = Random.modulated
  #l.rs = SimulateForAllSubjects.exp.beta(subjects.features=subjects.features, gamma=gamma,
  #                                       classifier.name=classifier.name, 
  #                                       weka.control=weka.control, num.folds=num.folds,
  #                                       num.repetitions=num.repetitions,
  #                                       selection.function=selection.function, max.size=max.size,
  #                                       horizon.size=horizon.size, history.size=history.size,
  #                                       num.gamma.iterations=num.gamma.iterations, lambda=lambda,
  #                                       monotone.increasing=T, beta=beta)
  #   file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/retry/l.rs.exp.lambda', 
  #                     lambda, 'frame.Rdata', sep='.')
  #file.name = '~/R/sf_ALTLAR/output/al.usc.had.modulated/usc-had/l.rs.exp.frame.Rdata'
  
  
  #file.name = paste('~/R/sf_ALTLAR/output/al.usc.had.modulated.beta/pamap/l.rs.exp.lambda', 
  #                  lambda, 'frame.Rdata', sep='.')  
  #
  #save(l.rs, file=file.name)
  cat(file.name, 'done\n')
  
  #stop('one iteration done')
}
