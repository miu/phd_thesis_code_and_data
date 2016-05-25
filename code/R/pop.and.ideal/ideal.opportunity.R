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

setwd('~/R/sf_ALTLAR/R/opportunity.butterworth.dtw')

source('~/R/sf_ALTLAR/R/ml.utils.R')
source('simulation.functions.R')

library(multicore)

# Load data
subject.ns = seq_len(4)

train.dataset.name = 'train.dataset.data'
test.dataset.name = 'test.dataset.data'
train.dataset.filtered.name = 'train.dataset.filtered.data'
test.dataset.filtered.name = 'test.dataset.filtered.data'

train.datasets = list()
test.datasets = list()

for (subject.number in subject.ns) {
  subject.dir = paste('subject', subject.number, sep='_')
  dataset.path = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw', subject.dir, sep='/')
  
  train.dataset.file.path = paste(dataset.path, train.dataset.name, sep='/')
  load(file=train.dataset.file.path)
  
  test.dataset.file.path = paste(dataset.path, test.dataset.name, sep='/')
  load(file=test.dataset.file.path)
  
  train.datasets[[subject.number]] = train.dataset
  test.datasets[[subject.number]] = test.dataset
}

final.fms = c()
subject.ns = c(4, 3)
for (subject.number in subject.ns) {
  
  cat('subject.number =', subject.number, '\n')
  
  train.dataset = train.datasets[[subject.number]]
  test.dataset = test.datasets[[subject.number]]
  
  # build distance matrices
  n.col = length(train.dataset)
  n.row = length(test.dataset)
  dist.matrix = matrix(data=NA, nrow=n.row, ncol=n.col)
  
  subject.dir = paste('subject', subject.number, sep='_')
  output.file.name = 'dist.matrix.ideal.R'
  output.path = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw', subject.dir, output.file.name, sep='/')
  
  
  #     for (i in seq_len(n.row)) {
  #       if (i %% 1 == 0) {
  #         cat(i, '')
  #       }
  #       seg.test = test.dataset[[i]]
  #       for (k in seq_len(n.col)) {
  #         seg.train = train.dataset.pop[[k]]
  #         dist.matrix[i, k] = DTW.distance.elem(train.segment=seg.train, test.segment=seg.test)
  #       }
  #     }
  #    
  get.pred.label = function(dist.vector, k, train.labels) {
    i.rows = order(dist.vector, decreasing=F)[seq_len(k)]
    labels = rep(x=NA, times=k)
    for (i in seq_along(i.rows)) {
      i.row = i.rows[i]
      labels[i] = train.labels[i.row]
    }
    
    labels.table = table(labels)
    label = names(which(labels.table == max(labels.table)))
    if (length(label > 1)) {
      label = sample(x=label, size=1)
    }
    
    return( label )
  }
  
  DTW.distance.vector.parallel = function(test.segment, train.signal) {
    n.train.segs = length(train.signal)
    dtw.dists = unlist(mclapply(X=train.signal, FUN=DTW.distance.elem, 
                                test.segment=test.segment, mc.cores=4))
    return( dtw.dists )
  }
  
  collapse.probabilities = function(nn.probs) {
    unique.names = unique(names(nn.probs))
    probs = rep(0, length(unique.names))
    names(probs) = unique.names
    
    for (i in seq_along(unique.names)) {
      u.name = unique.names[i]
      for (k in seq_along(nn.probs)) {
        if (u.name == names(nn.probs)[k]) {
          probs[i] = probs[i] + nn.probs[k]
        }
      }
    }
    return( sort(probs, decreasing=T) )
  }
  
  knn.probability = function(dist.vector, labels, k) {
    # adjust k if too little 'training' instances
    k = min(k, length(dist.vector))
    
    best.dists = sort(dist.vector, decreasing=F)[seq_len(k)]
    
    nn.i = rep(NA, k)
    for (i in seq_len(k)) {
      nn.i[i] = which(best.dists[i] == dist.vector)
    }
    nn.probs = (1/best.dists) / sum(1/best.dists)
    names(nn.probs) = labels[nn.i]
    
    return( collapse.probabilities(nn.probs=nn.probs) )
  }
  
  knn.class = function(dist.vector, labels, k) {
    nn.probs = knn.probability(dist.vector=dist.vector, 
                               labels=labels, k=k)
    return( names(nn.probs)[1] )
  }
  
  l.dtw.dists = lapply(X=test.dataset, FUN=DTW.distance.vector.parallel, train.signal=train.dataset)
  dist.matrix = do.call(rbind, l.dtw.dists)
  
  save(dist.matrix, file=output.path)
  
  train.labels = rep(x=NA, times=length(train.dataset))
  for (i in seq_along(train.dataset)) {
    train.labels[i] = as.character(train.dataset[[i]]$activity[1])
  }
  
  pred.labels = apply(X=dist.matrix, MARGIN=1, FUN=get.pred.label, k=3, train.labels=train.labels)
  #pred.labels = apply(X=dist.matrix, MARGIN=1, FUN=knn.class, k=3, labels=actual.labels)
  
  actual.labels = rep(x=NA, times=length(test.dataset))
  for (i in seq_along(test.dataset)) {
    actual.labels[i] = as.character(test.dataset[[i]]$activity[1])
  }
  
  class.labels = sort(unique(actual.labels))
  
  cm = ConfusionMatrix(actual=actual.labels, predicted=pred.labels, class.labels=class.labels)
  
  final.fm = WeightedFMeasure(cm=cm)
  
  cat('ideal final.fm =', final.fm, '\n\n')
  
  final.fms = c(final.fms, final.fm)
}
