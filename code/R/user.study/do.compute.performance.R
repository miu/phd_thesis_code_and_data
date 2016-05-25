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

setwd('~/R/sf_ALTLAR/R/user.study')

library(RWeka)
library(doRedis)

source('../ml.utils.R')
source('plot.R')

SortUsersBySize = function(datasets.segments) {
  sizes = rep(NA, length(datasets.segments))
  for (i in seq_along(datasets.segments)) {
    sizes[i] = length(datasets.segments[[i]]$l.segments)
  }
  
  i.order = order(sizes, decreasing=T)
  return( datasets.segments[i.order] )
}

queue.name='user.study.queue'
registerDoRedis(queue=queue.name)

if (!file.exists('~/R/sf_ALTLAR/output/user.study/datasets.segments.annotated.Rdata')) {
  cat('Loading datasets...\n')
  datasets.instances = LoadDatasets()
  #dataset.instances = datasets.instances[[1]]
  
  cat('Splitting into segments...\n')
  datasets.segments = lapply(X=datasets.instances, FUN=SplitIntoSegments)
  #dataset.segments = datasets.segments[[1]]
  
  exclude.labels = c(-1, -2)
  #dataset.segments.old = dataset.segments
  #dataset.segments.new = FilterSegments(dataset.segments=dataset.segments, exclude.labels=exclude.labels)
  
  datasets.segments.annotated = lapply(X=datasets.segments, FUN=FilterSegments, exclude.labels=exclude.labels)
  
  datasets.segments.annotated = SortUsersBySize(datasets.segments.annotated)
  
  save(datasets.segments.annotated, file='~/R/sf_ALTLAR/output/user.study/datasets.segments.annotated.Rdata')
  save(datasets.segments, file='~/R/sf_ALTLAR/output/user.study/datasets.segments.Rdata')
} else {
  load(file='~/R/sf_ALTLAR/output/user.study/datasets.segments.annotated.Rdata')
  load(file='~/R/sf_ALTLAR/output/user.study/datasets.segments.Rdata')
}
annotation.ratios = rep(NA, length(datasets.segments.annotated))
for (i in seq_along(datasets.segments.annotated)) {
  annotation.ratios[i] = GetRatioAnnotated(datasets.segments[[i]], datasets.segments.annotated[[i]])
}

num.reps = 20
#noise.levels = c(0, 0.01, 0.05, 0.1)
noise.levels = c(0, 0.1)
#noise.levels = c(0.2, 0.3, 0.4, 0.5)
#noise.levels = c(0.1)

#datasets.segments.annotated = datasets.segments.annotated[length(datasets.segments.annotated)]

# cat('Computing performance (with sitting)...\n')
# for (i in seq_len(14)) {
#   l.perfs.with.sitting = PerformanceMetrics(datasets.segments.annotated=datasets.segments.annotated, num.reps=1)
#   
#   file.name = paste('l.perfs.with.sitting.rep', i, '.Rdata', sep='')
#   file.dir = '~/R/sf_ALTLAR/output/user.study'
#   file.path = paste(file.dir, file.name, sep='/')
#   
#   #save(l.perfs.with.sitting, file='~/R/sf_ALTLAR/output/user.study/l.perfs.with.sitting.Rdata')
#   save(l.perfs.with.sitting, file=file.path)
#   cat('\nSaved! (', i, ')\n', sep='')
# }

#datasets.segments.annotated=datasets.segments.annotated[-c(1, 2, 3, 4, 5, 6, 7)]

#datasets.segments.annotated=datasets.segments.annotated[6:10]

# classifier.name = 'weka/classifiers/meta/Bagging'
# weak.classifier.name = 'weka.classifiers.bayes.NaiveBayes'
# weka.control = Weka_control(W=weak.classifier.name, 
#                             I=30)

classifier.name = 'weka/classifiers/rules/ZeroR'
weka.control = Weka_control()

for (subject.i in seq_along(datasets.segments.annotated)) {
 l.perfs.with.sitting.noisy = PerformanceMetrics.noise(datasets.segments.annotated=datasets.segments.annotated[subject.i], 
                                                       num.reps=num.reps, noise.levels=noise.levels,
                                                       classifier.name=classifier.name, weka.control=weka.control)
 file.name = paste('~/R/sf_ALTLAR/output/user.study/noisy/zeror/l.perfs.with.sitting.noisy.subject', subject.i, 
                   'Rdata', sep='.')
 save(l.perfs.with.sitting.noisy, file=file.name)
 cat('\nSaved!', file.name)
 
 print(warnings())
}

#stop('enough')

exclude.labels = 1 # sitting
datasets.segments.annotated = lapply(X=datasets.segments.annotated, FUN=FilterSegments, exclude.labels=exclude.labels)
datasets.segments.annotated = SortUsersBySize(datasets.segments.annotated)

for (subject.i in seq_along(datasets.segments.annotated)) {
  l.perfs.without.sitting.noisy = PerformanceMetrics.noise(datasets.segments.annotated=datasets.segments.annotated[subject.i], 
                                                        num.reps=num.reps, noise.levels=noise.levels,
                                                        classifier.name=classifier.name, weka.control=weka.control)
  file.name = paste('~/R/sf_ALTLAR/output/user.study/noisy/zeror/l.perfs.without.sitting.noisy.subject', subject.i, 
                    'Rdata', sep='.')
  save(l.perfs.without.sitting.noisy, file=file.name)
  cat('\nSaved!', file.name)
  
  print(warnings())
}

stop('enough')

#cat('Computin performance (without sitting)')
#l.perfs.without.sitting = PerformanceMetrics(datasets.segments.annotated=datasets.segments.annotated, num.reps=num.reps)
#save(l.perfs.without.sitting, file='~/R/sf_ALTLAR/output/user.study/l.perfs.without.sitting.Rdata')
#cat('\nSaved!')

# for (i in seq_len(num.reps)) {
#   l.perfs.with.sitting = PerformanceMetrics(datasets.segments.annotated=datasets.segments.annotated, num.reps=1)
#   
#   file.name = paste('l.perfs.with.sitting.rep', i, '.Rdata', sep='')
#   file.dir = '~/R/sf_ALTLAR/output/user.study'
#   file.path = paste(file.dir, file.name, sep='/')
#   
#   #save(l.perfs.with.sitting, file='~/R/sf_ALTLAR/output/user.study/l.perfs.with.sitting.Rdata')
#   save(l.perfs.with.sitting, file=file.path)
#   cat('\nSaved! (', i, ')\n', sep='')
# }

# l.perfs.without.sitting.noisy = PerformanceMetrics.noise(datasets.segments.annotated=datasets.segments.annotated, 
#                                                          num.reps=num.reps, noise.levels=noise.levels,
#                                                          classifier.name=classifier.name, weka.control=weka.control)
# save(l.perfs.without.sitting.noisy, file='~/R/sf_ALTLAR/output/user.study/noisy/l.perfs.without.sitting.noisy.Rdata')
# cat('\nSaved!')
# 
# cat('\n\n============ Done B.NB.30 ============\n\n')

load(file='~/R/sf_ALTLAR/output/user.study/datasets.segments.annotated.Rdata')
load(file='~/R/sf_ALTLAR/output/user.study/datasets.segments.Rdata')

classifier.name = 'weka/classifiers/rules/ZeroR'
weka.control = Weka_control()

l.perfs.with.sitting.noisy = PerformanceMetrics.noise(datasets.segments.annotated=datasets.segments.annotated, 
                                                      num.reps=num.reps, noise.levels=noise.levels,
                                                      classifier.name=classifier.name, weka.control=weka.control)
save(l.perfs.with.sitting.noisy, 
     file='~/R/sf_ALTLAR/output/user.study/noisy/l.perfs.with.sitting.noisy.ZeroR.Rdata')
cat('\nSaved!')

print(warnings())

#stop('enough')

exclude.labels = 1 # sitting
datasets.segments.annotated = lapply(X=datasets.segments.annotated, FUN=FilterSegments, exclude.labels=exclude.labels)
datasets.segments.annotated = SortUsersBySize(datasets.segments.annotated)

cat('Computin performance (without sitting)')
#l.perfs.without.sitting = PerformanceMetrics(datasets.segments.annotated=datasets.segments.annotated, num.reps=num.reps)
#save(l.perfs.without.sitting, file='~/R/sf_ALTLAR/output/user.study/l.perfs.without.sitting.Rdata')
#cat('\nSaved!')

# for (i in seq_len(num.reps)) {
#   l.perfs.with.sitting = PerformanceMetrics(datasets.segments.annotated=datasets.segments.annotated, num.reps=1)
#   
#   file.name = paste('l.perfs.with.sitting.rep', i, '.Rdata', sep='')
#   file.dir = '~/R/sf_ALTLAR/output/user.study'
#   file.path = paste(file.dir, file.name, sep='/')
#   
#   #save(l.perfs.with.sitting, file='~/R/sf_ALTLAR/output/user.study/l.perfs.with.sitting.Rdata')
#   save(l.perfs.with.sitting, file=file.path)
#   cat('\nSaved! (', i, ')\n', sep='')
# }

l.perfs.without.sitting.noisy = PerformanceMetrics.noise(datasets.segments.annotated=datasets.segments.annotated, 
                                                         num.reps=num.reps, noise.levels=noise.levels,
                                                         classifier.name=classifier.name, weka.control=weka.control)
save(l.perfs.without.sitting.noisy, 
     file='~/R/sf_ALTLAR/output/user.study/noisy/l.perfs.without.sitting.noisy.ZeroR.Rdata')
cat('\nSaved!')

cat('\n\n============ Done ZeroR ============\n\n')
