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

source('../file.utils.R')
source('index.sampling.R')

library(doRedis)

queue.name='index.sampling.queue'
registerDoRedis(queue=queue.name)

### USC-HAD
load(file='~/R/sf_ALTLAR/output/organize.usc.had/subjects.features.Rdata')
subjects.features = obj
subjects.features = subjects.features[-2]
out.dir = '~/R/sf_ALTLAR/output/usc.had.seg.al/seg.accuracy/usc-had/'

### PAMAP
#load(file='~/R/sf_ALTLAR/output/organize.pamap.2.protocol/subjects.features.Rdata')
#subjects.features = obj
#out.dir = '~/R/sf_ALTLAR/output/usc.had.seg.al/seg.accuracy/pamap/'

kernel.width = 3 
min.seg.len = 3 
max.seg.len = 6 
num.segments = 1000
quantile.min = 0.01
quantile.max = 0.99
threshold = 0.2

l.seg = SimulateSegmentationForSample.seg.acc(subjects.features=subjects.features,
                                              kernel.width=kernel.width, 
                                              min.seg.len=min.seg.len, 
                                              max.seg.len=max.seg.len, 
                                              num.segments=num.segments,
                                              quantile.min=quantile.min,
                                              quantile.max=quantile.max,
                                              threshold=threshold)

dir.create(path=out.dir, showWarnings=F, recursive=T)
SaveObject(obj=l.seg, var.name='l.seg-0.2', dir=out.dir)

#stop('Enough. No need to worry')

seg.accs = rep(NA, times=length(l.seg))
for (i.subject in seq_along(l.seg)) {  
  l.rep = l.seg[[i.subject]]
  
  gt.ends = c(0, l.rep$gt.ends)
  gt.diffs = gt.ends[2: length(gt.ends)] - gt.ends[1: (length(gt.ends)-1)]
  gt.labels = l.rep$gt.labels
  gt.labelled.frames = rep(NA, max(gt.ends))
  k = 1
  for (i.gt in seq_along(gt.diffs)) {
    gt.label = gt.labels[i.gt]
    num.reps = gt.diffs[i.gt]
    for (i in seq_len(num.reps)) {
      gt.labelled.frames[k] = gt.label
      k = k+1
    }
  }
  
  est.ends = c(0, l.rep$est.ends)
  est.diffs = est.ends[2: length(est.ends)] - est.ends[1: (length(est.ends)-1)]
  est.labels = l.rep$est.labels
  est.labelled.frames = rep(NA, max(est.ends))
  k = 1
  for (i.gt in seq_along(est.diffs)) {
    est.label = est.labels[i.gt]
    num.reps = est.diffs[i.gt]
    for (i in seq_len(num.reps)) {
      est.labelled.frames[k] = est.label
      k = k+1
    }
  }
  
  i.est.subset = seq_along(est.labelled.frames)
  num.correct = sum(est.labelled.frames == gt.labelled.frames[i.est.subset])
  num.total = length(est.labelled.frames)
  segmentation.accuracy = num.correct / num.total
  cat('  Subject', i.subject, ':');
  cat('Segmentation accuracy =', segmentation.accuracy, '\n')
  seg.accs[i.subject] = segmentation.accuracy
}

cat('Overall segmentation accuracy:', mean(seg.accs))
