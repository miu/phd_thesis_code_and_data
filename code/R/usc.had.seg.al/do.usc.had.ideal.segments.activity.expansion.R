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

setwd('~/R/sf_ALTLAR/R/usc.had.seg.al')

source('usc.had.ideal.segments.activity.expansion.R')

library(RWeka)
library(doRedis)

queue.name='index.sampling.queue'
registerDoRedis(queue=queue.name)


## PAMAP
load(file='~/R/sf_ALTLAR/output/organize.pamap.2.protocol/subjects.features.Rdata')
subjects.features = obj
class.labels.expand = c('1', '2') # 1-lie; 2-sit
#class.labels.expand = c()



max.seg.size = 6
gamma = 6
online.heuristic = Online.BMargin

classifier.name='weka/classifiers/meta/Bagging'
weak.classifier.name = 'weka.classifiers.bayes.NaiveBayes'
bagging.iterations = 30
weka.control = Weka_control(W=weak.classifier.name, I=bagging.iterations)

min.train.size = 3

#expand.factor = 10 # for PAMAP
#max.train.size = 800 

expand.factor = 50
max.train.size = 1000

#expand.factor = 1
#max.train.size = 1000

num.folds = 10
num.days = Inf
num.reps = 10

output.dir = '~/R/sf_ALTLAR/output/usc.had.expanded/pamap/'
if (!file.exists(output.dir)) {
  dir.create(path=output.dir, showWarnings=F, recursive=T)
}

l.seg.sample.file.name = 'l.seg.sample.Rdata'
l.seg.sample.file.path = paste(output.dir, l.seg.sample.file.name, sep='/')
if (file.exists(l.seg.sample.file.path)) {
  load(l.seg.sample.file.path)
} else {
  l.seg.sample = GetSegmentsIndicesForSample(subjects.features=subjects.features, 
                                             class.labels.expand=class.labels.expand, 
                                             expand.factor=expand.factor, 
                                             max.seg.size=max.seg.size)
  save(l.seg.sample, file=l.seg.sample.file.path)
}

cat('OAL...\n')
l.al = SimulateALForSample(subjects.features=subjects.features, l.seg.sample=l.seg.sample, 
                           min.train.size=min.train.size, max.train.size=max.train.size, 
                           classifier.name=classifier.name, weka.control=weka.control, 
                           num.folds=num.folds, online.heuristic=online.heuristic, gamma=gamma, 
                           num.days=num.days, num.reps=num.reps)
output.file.name = paste('l.al', 'gamma', gamma, 'expand,factor', expand.factor, 'Rdata', sep='.')
output.file.path = paste(output.dir, output.file.name, sep='/')
save(l.al, file=output.file.path)

cat('RS 0.1...\n')
online.heuristic = Online.Random
gamma = 0.1
l.rs = SimulateALForSample(subjects.features=subjects.features, l.seg.sample=l.seg.sample, 
                           min.train.size=min.train.size, max.train.size=max.train.size, 
                           classifier.name=classifier.name, weka.control=weka.control, 
                           num.folds=num.folds, online.heuristic=online.heuristic, gamma=gamma, 
                           num.days=num.days, num.reps=num.reps)
output.file.name = paste('l.rs', 'f', gamma, 'expand,factor', expand.factor, 'Rdata', sep='.')
output.file.path = paste(output.dir, output.file.name, sep='/')
save(l.rs, file=output.file.path)

cat('RS 0.01...\n')
online.heuristic = Online.Random
gamma = 0.01
l.rs = SimulateALForSample(subjects.features=subjects.features, l.seg.sample=l.seg.sample, 
                           min.train.size=min.train.size, max.train.size=max.train.size, 
                           classifier.name=classifier.name, weka.control=weka.control, 
                           num.folds=num.folds, online.heuristic=online.heuristic, gamma=gamma, 
                           num.days=num.days, num.reps=num.reps)
output.file.name = paste('l.rs', 'f', gamma, 'expand,factor', expand.factor, 'Rdata', sep='.')
output.file.path = paste(output.dir, output.file.name, sep='/')
save(l.rs, file=output.file.path)

cat('\n\nDone!\n\n')
