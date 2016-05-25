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

source('R/opportunity.dtw.R')
source('R/file.utils.R')

library(doRedis)

opportunity.dir = '~/R/sf_ALTLAR/output/opportunity/dtw.2'

train.file.name = 'opportunity.subjects.dtw.train.250.incr'
train.sets.original = LoadObject(file.name=train.file.name, 
                                 dir=opportunity.dir)

test.file.name = 'opportunity.subjects.dtw.test.250.incr'
test.sets.original = LoadObject(file.name=test.file.name,
                                dir=opportunity.dir)

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

DiminishDataSets = function(data.sets, size) {
  for (i in seq_along(data.sets)) {
    data.sets[[i]]$data = data.sets[[i]]$data[seq_len(size)]
  }
  return( data.sets )
}

NormalizeDataSets = function(data.sets) {
  for (i in seq_along(data.sets)) {
    collapsed.ds = do.call(what=rbind, args=data.sets[[i]]$data)
    i.col.seq = 2: (ncol(collapsed.ds) - 1)
    mins = rep(NA, length(i.col.seq))
    maxs = rep(NA, length(i.col.seq))
    for (k in seq_along(i.col.seq)) {
      i.col = i.col.seq[k]
      mins[k] = min(collapsed.ds[, i.col])
      maxs[k] = max(collapsed.ds[, i.col])
    }
    
    for (i.seg in seq_along(data.sets[[i]]$data)) {
      seg = data.sets[[i]]$data[[i.seg]]
      for (k in seq_along(i.col.seq)) {
        i.col = i.col.seq[k]
        seg[, i.col] = (maxs[k]-seg[, i.col]) / (maxs[k]-mins[k])
      }
      data.sets[[i]]$data[[i.seg]] = seg
    }
  }
  return( data.sets )
}

NormalizeDataSets.perc = function(data.sets) {
  for (i in seq_along(data.sets)) {
    collapsed.ds = do.call(what=rbind, args=data.sets[[i]]$data)
    i.col.seq = 2: (ncol(collapsed.ds) - 1)
    mins = rep(NA, length(i.col.seq))
    maxs = rep(NA, length(i.col.seq))
    for (k in seq_along(i.col.seq)) {
      i.col = i.col.seq[k]
      mins[k] = quantile(x=collapsed.ds[, i.col], probs=0.01)
      maxs[k] = quantile(x=collapsed.ds[, i.col], probs=0.99)
    }
    
    for (i.seg in seq_along(data.sets[[i]]$data)) {
      seg = data.sets[[i]]$data[[i.seg]]
      for (k in seq_along(i.col.seq)) {
        i.col = i.col.seq[k]
        seg[, i.col] = (maxs[k]-seg[, i.col]) / (maxs[k]-mins[k])
      }
      data.sets[[i]]$data[[i.seg]] = seg
    }
  }
  return( data.sets )
}

NormalizeDataSets.seg = function(data.sets) {
  for (i in seq_along(data.sets)) {
    for (i.seg in seq_along(data.sets[[i]]$data)) {
      seg = data.sets[[i]]$data[[i.seg]]
      
      i.col.seq = 2: (ncol(seg) - 1)
      mins = rep(NA, length(i.col.seq))
      maxs = rep(NA, length(i.col.seq))
      for (k in seq_along(i.col.seq)) {
        i.col = i.col.seq[k]
        mins[k] = min(seg[, i.col])
        maxs[k] = max(seg[, i.col])
      }
      
      for (k in seq_along(i.col.seq)) {
        i.col = i.col.seq[k]
        seg[, i.col] = (maxs[k]-seg[, i.col]) / (maxs[k]-mins[k])
      }
      data.sets[[i]]$data[[i.seg]] = seg
    }
  }
  return( data.sets )
}

NormalizeDataSets.transfer = function(data.sets) {
  collapsed.dss = list()
  for (i in seq_along(data.sets)) {
    collapsed.dss[[i]] = do.call(what=rbind, args=data.sets[[i]]$data)
  }
  
  for (i in seq_along(data.sets)) {
    i.col.seq = 2: (ncol(collapsed.dss[[1]]) - 1)
    mins = rep(NA, length(i.col.seq))
    maxs = rep(NA, length(i.col.seq))
    
    #transfer.ds = do.call(what=rbind, args=collapsed.dss[-i]$data)
    transfer.ds = collapsed.dss[[1]][0, ]
    for (k in setdiff(seq_along(data.sets), i)) {
      transfer.ds = rbind(transfer.ds, 
                          do.call(what=rbind, 
                                  args=data.sets[[k]]$data))
    }
    
    for (k in seq_along(i.col.seq)) {
      i.col = i.col.seq[k]
      mins[k] = quantile(x=transfer.ds[, i.col], probs=0.01)
      maxs[k] = quantile(x=transfer.ds[, i.col], probs=0.99)
    }
    
    for (i.seg in seq_along(data.sets[[i]]$data)) {
      seg = data.sets[[i]]$data[[i.seg]]
      for (k in seq_along(i.col.seq)) {
        i.col = i.col.seq[k]
        seg[, i.col] = (maxs[k]-seg[, i.col]) / (maxs[k]-mins[k])
      }
      data.sets[[i]]$data[[i.seg]] = seg
    }
  }
  return( data.sets )
}

#train.sets.original = DiminishDataSets(train.sets.original, size=20)
#test.sets.original = DiminishDataSets(test.sets.original, size=30)

queue.name='opportunity.dtw.queue'
registerDoRedis(queue=queue.name)

k = 4
metric.function = Offline.Confidence
max.size = 200

repetitions = 10
#gammas = c(1, 2, 3)
gammas = c(4, 5)
file.name = '3-nn'

f = 1


## raw 17
destination.folder = paste('f', f, sep='_')
destination.dir = paste('raw_17', destination.folder, sep='/')
selection.function = Online.Random
# SimulateOpportunityDTW.sample(train.sets=train.sets.original, 
#                               test.sets=test.sets.original, 
#                               repetitions=repetitions, 
#                               opportunity.dir=opportunity.dir, 
#                               metric.function=metric.function, 
#                               selection.function=selection.function,
#                               k=k, 
#                               f=f, 
#                               destination.dir=destination.dir,
#                               file.name=file.name, 
#                               max.size=max.size)

for (gamma in gammas) {
  destination.folder = paste('gamma', gamma, sep='_')
  destination.dir = paste('raw_17', destination.folder, sep='/')
  selection.function = Online.BMargin
  SimulateOpportunityDTW.sample(train.sets=train.sets.original, 
                                test.sets=test.sets.original, 
                                repetitions=repetitions, 
                                opportunity.dir=opportunity.dir, 
                                metric.function=metric.function, 
                                selection.function=selection.function,
                                k=k, 
                                gamma=gamma, 
                                destination.dir=destination.dir,
                                file.name=file.name,
                                max.size=max.size)
}

## norm 17
train.sets.norm = NormalizeDataSets(train.sets.original)
test.sets.norm = NormalizeDataSets(test.sets.original)

destination.folder = paste('f', f, sep='_')
destination.dir = paste('norm_17', destination.folder, sep='/')
selection.function = Online.Random
# SimulateOpportunityDTW.sample(train.sets=train.sets.norm, 
#                               test.sets=test.sets.norm, 
#                               repetitions=repetitions, 
#                               opportunity.dir=opportunity.dir, 
#                               metric.function=metric.function, 
#                               selection.function=selection.function,
#                               k=k, 
#                               f=f, 
#                               destination.dir=destination.dir,
#                               file.name=file.name, 
#                               max.size=max.size)

for (gamma in gammas) {
  destination.folder = paste('gamma', gamma, sep='_')
  destination.dir = paste('norm_17', destination.folder, sep='/')
  selection.function = Online.BMargin
#   SimulateOpportunityDTW.sample(train.sets=train.sets.norm, 
#                                 test.sets=test.sets.norm, 
#                                 repetitions=repetitions, 
#                                 opportunity.dir=opportunity.dir, 
#                                 metric.function=metric.function, 
#                                 selection.function=selection.function,
#                                 k=k, 
#                                 gamma=gamma, 
#                                 destination.dir=destination.dir,
#                                 file.name=file.name,
#                                 max.size=max.size)
}

## norm transfer 17
train.sets.norm = NormalizeDataSets.transfer(train.sets.original)
test.sets.norm = NormalizeDataSets.transfer(test.sets.original)

destination.folder = paste('f', f, sep='_')
destination.dir = paste('norm.tr_17', destination.folder, sep='/')
selection.function = Online.Random
# SimulateOpportunityDTW.sample(train.sets=train.sets.norm, 
#                               test.sets=test.sets.norm, 
#                               repetitions=repetitions, 
#                               opportunity.dir=opportunity.dir, 
#                               metric.function=metric.function, 
#                               selection.function=selection.function,
#                               k=k, 
#                               f=f, 
#                               destination.dir=destination.dir,
#                               file.name=file.name, 
#                               max.size=max.size)

for (gamma in gammas) {
  destination.folder = paste('gamma', gamma, sep='_')
  destination.dir = paste('norm.tr_17', destination.folder, sep='/')
  selection.function = Online.BMargin
#   SimulateOpportunityDTW.sample(train.sets=train.sets.norm, 
#                                 test.sets=test.sets.norm, 
#                                 repetitions=repetitions, 
#                                 opportunity.dir=opportunity.dir, 
#                                 metric.function=metric.function, 
#                                 selection.function=selection.function,
#                                 k=k, 
#                                 gamma=gamma, 
#                                 destination.dir=destination.dir,
#                                 file.name=file.name,
#                                 max.size=max.size)
}

############# compressing labels

train.sets.original = ReplaceLabels(train.sets.original)
test.sets.original = ReplaceLabels(test.sets.original)

## raw 7
destination.folder = paste('f', f, sep='_')
destination.dir = paste('raw_7', destination.folder, sep='/')
selection.function = Online.Random
# SimulateOpportunityDTW.sample(train.sets=train.sets.original, 
#                               test.sets=test.sets.original, 
#                               repetitions=repetitions, 
#                               opportunity.dir=opportunity.dir, 
#                               metric.function=metric.function, 
#                               selection.function=selection.function,
#                               k=k, 
#                               f=f, 
#                               destination.dir=destination.dir,
#                               file.name=file.name, 
#                               max.size=max.size)

for (gamma in gammas) {
  destination.folder = paste('gamma', gamma, sep='_')
  destination.dir = paste('raw_7', destination.folder, sep='/')
  selection.function = Online.BMargin
  SimulateOpportunityDTW.sample(train.sets=train.sets.original, 
                                test.sets=test.sets.original, 
                                repetitions=repetitions, 
                                opportunity.dir=opportunity.dir, 
                                metric.function=metric.function, 
                                selection.function=selection.function,
                                k=k, 
                                gamma=gamma, 
                                destination.dir=destination.dir,
                                file.name=file.name,
                                max.size=max.size)
}

## norm 7
train.sets.norm = NormalizeDataSets(train.sets.original)
test.sets.norm = NormalizeDataSets(test.sets.original)

destination.folder = paste('f', f, sep='_')
destination.dir = paste('norm_7', destination.folder, sep='/')
selection.function = Online.Random
SimulateOpportunityDTW.sample(train.sets=train.sets.norm, 
                              test.sets=test.sets.norm, 
                              repetitions=repetitions, 
                              opportunity.dir=opportunity.dir, 
                              metric.function=metric.function, 
                              selection.function=selection.function,
                              k=k, 
                              f=f, 
                              destination.dir=destination.dir,
                              file.name=file.name, 
                              max.size=max.size)

for (gamma in gammas) {
  destination.folder = paste('gamma', gamma, sep='_')
  destination.dir = paste('norm_7', destination.folder, sep='/')
  selection.function = Online.BMargin
  SimulateOpportunityDTW.sample(train.sets=train.sets.norm, 
                                test.sets=test.sets.norm, 
                                repetitions=repetitions, 
                                opportunity.dir=opportunity.dir, 
                                metric.function=metric.function, 
                                selection.function=selection.function,
                                k=k, 
                                gamma=gamma, 
                                destination.dir=destination.dir,
                                file.name=file.name,
                                max.size=max.size)
}

## norm transfer 7
train.sets.norm = NormalizeDataSets.transfer(train.sets.original)
test.sets.norm = NormalizeDataSets.transfer(test.sets.original)

destination.folder = paste('f', f, sep='_')
destination.dir = paste('norm.tr_7', destination.folder, sep='/')
selection.function = Online.Random
SimulateOpportunityDTW.sample(train.sets=train.sets.norm, 
                              test.sets=test.sets.norm, 
                              repetitions=repetitions, 
                              opportunity.dir=opportunity.dir, 
                              metric.function=metric.function, 
                              selection.function=selection.function,
                              k=k, 
                              f=f, 
                              destination.dir=destination.dir,
                              file.name=file.name, 
                              max.size=max.size)

for (gamma in gammas) {
  destination.folder = paste('gamma', gamma, sep='_')
  destination.dir = paste('norm.tr_7', destination.folder, sep='/')
  selection.function = Online.BMargin
  SimulateOpportunityDTW.sample(train.sets=train.sets.norm, 
                                test.sets=test.sets.norm, 
                                repetitions=repetitions, 
                                opportunity.dir=opportunity.dir, 
                                metric.function=metric.function, 
                                selection.function=selection.function,
                                k=k, 
                                gamma=gamma, 
                                destination.dir=destination.dir,
                                file.name=file.name,
                                max.size=max.size)
}
