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

library(RWeka)
library(dtw)
library(multicore)
library(signal)

source('../ml.utils.R')

input.columns.i = c(8:10, # LUA^
                    11:13, # RUA_
                    17:19, # BACK
                    26:28, # RUA^
                    29:31) # LUA_
class.column.i = 116

column.names = c('lua.up.x', 'lua.up.y', 'lua.up.z',
                 'rua.down.x', 'rua.down.y', 'rua.down.z',
                 'back.x', 'back.y', 'back.z',
                 'rua.up.x', 'rua.up.y', 'rua.up.z',
                 'lua.down.x', 'lua.down.y', 'lua.down.z',
                 'activity')

LoadOpportunityDataFiles = function(subject.number, dataset.dir, input.columns.i, class.column.i, column.names, str.adls) {
  
  file.names = paste('S', subject.number, '-', str.adls, '.dat', sep='')
  file.paths = paste(dataset.dir, file.names, sep='/')
  file.paths.standardized = c()
  
  for (i in seq_along(file.paths)) {
    file.path = file.paths[i]
    file.paths.standardized[i] = paste(file.path, 'new', sep='_')
    standardize.file(file.path.original=file.path, file.path.new=file.paths.standardized[i])
  }
  
  signal.datasets = lapply(X=file.paths.standardized, FUN=read.csv, na.strings="NaN", check.names=F)
  lapply(X=file.paths.standardized, file.remove)
  
  signal.datasets = lapply(X=signal.datasets, FUN=keep.columns, c(input.columns.i, class.column.i))
  signal.datasets = lapply(X=signal.datasets, FUN=name.columns, column.names)
  original.signal.datasets = signal.datasets
  
  scaled.datasets = scale.dataset(signal.datasets=signal.datasets)
  
  segments = list()
  
  class.labels = GetClassLabels(scaled.datasets)
  class.labels.no.null = class.labels[which(class.labels != 0)]
  for (i in seq_along(scaled.datasets)) {
    scaled.datasets[[i]]$activity = factor(scaled.datasets[[i]]$activity, levels=class.labels)
    
    segment = scaled.datasets[[i]][0, ]
    seg.activity = as.character(scaled.datasets[[i]]$activity[1])
    for (i.row in seq_len(nrow(scaled.datasets[[i]]))) {
      row = scaled.datasets[[i]][i.row, ]
      curr.activity = as.character(row$activity)
      
      if ((curr.activity == 0) && (seg.activity == 0)) {
        segment = scaled.datasets[[i]][0, ]
        next
      }
      
      if ((curr.activity > 0) && (seg.activity == 0)) {
        segment = row
        seg.activity = curr.activity
      }
      
      if ((curr.activity > 0) && (seg.activity == curr.activity)) {
        segment = rbind(segment, row)
      }
      
      if ((curr.activity == 0) && (seg.activity > 0)) {
        i.col = 1 : (ncol(segment) - 1)
        readings.sum = sum(segment[, i.col])
        
        if (!is.na(readings.sum)) {
          segment$activity = factor(segment$activity, class.labels.no.null)
          segments[[1 + length(segments)]] = segment
        }
        segment = scaled.datasets[[i]][0, ]
        seg.activity = 0        
      }
      
    }
  }
  
  return( segments )
}

LoadOpportunityDataFiles.filtered = function(subject.number, dataset.dir, input.columns.i, class.column.i, 
                                             column.names, str.adls, bw.filter, delay) {
  file.names = paste('S', subject.number, '-', str.adls, '.dat', sep='')
  file.paths = paste(dataset.dir, file.names, sep='/')
  file.paths.standardized = c()
  
  for (i in seq_along(file.paths)) {
    file.path = file.paths[i]
    file.paths.standardized[i] = paste(file.path, 'new', sep='_')
    standardize.file(file.path.original=file.path, file.path.new=file.paths.standardized[i])
  }
  
  signal.datasets = lapply(X=file.paths.standardized, FUN=read.csv, na.strings="NaN", check.names=F)
  lapply(X=file.paths.standardized, file.remove)
  
  signal.datasets = lapply(X=signal.datasets, FUN=keep.columns, c(input.columns.i, class.column.i))
  signal.datasets = lapply(X=signal.datasets, FUN=name.columns, column.names)
  original.signal.datasets = signal.datasets
  
  # shift labels according to delay
  for (i in seq_along(signal.datasets)) {
    len = nrow(signal.datasets[[i]])
    print(range(delay: len))
    print(range(1: (len-delay + 1)))
    
    #print(range((delay+1): len))
    #print(range(1: (len-delay)))
    signal.datasets[[i]]$activity[delay: len] = signal.datasets[[i]]$activity[1: (len-delay + 1)]
  }
  
  non.na.segments = list()
  segment = signal.datasets[[1]][0, ]
  i.col = 1 : (ncol(segment) - 1)
  for (i in seq_along(signal.datasets)) {
    #segment.i.start = NA
    #segment.i.end = NA
    #prev.was.na = F
    for (i.row in seq_len(nrow(signal.datasets[[i]]))) {
      row = signal.datasets[[i]][i.row, ]
      
      sum.row = sum(row[, i.col])
      if (!is.na(sum.row)) {
        segment = rbind(segment, row)
        #         if (is.na(segment.i.start)) {
        #           segment.i.start = i.row
        #         } else {
        #           segment.i.end = i.row
        #         }
      } else {
        #if (!is.na(segment.i.start) & !is.na(segment.i.end)) {
        #  segment = signal.datasets[[i]][segment.i.start: segment.i.end, ]
        #}
        
        #segment.i.start = NA
        #segment.i.end = NA
        
        if (nrow(segment) > 0) {
          # apply filter
          cat('Applying filter [', nrow(segment), '] ...', sep='')
          segment.filtered = ApplyFilter(segment.signals=segment, bw.filter=bw.filter, ignore.class=T)
          cat(' done...\n')
          #stop()
          non.na.segments[[1 + length(non.na.segments)]] = segment.filtered          
          segment = signal.datasets[[1]][0, ]
          
          #if (sum(as.integer(as.character(segment.filtered$activity))) > 0) {
          #  break
          #}
        }
      }
    }
    
    # last segment
    if (nrow(segment) > 0) {
      # apply filter
      cat('Applying filter [', nrow(segment), '] ...', sep='')
      segment.filtered = ApplyFilter(segment.signals=segment, bw.filter=bw.filter, ignore.class=T)
      cat(' done...\n')
      #stop()
      non.na.segments[[1 + length(non.na.segments)]] = segment.filtered          
      segment = signal.datasets[[1]][0, ]
    }
  }
  
  scaled.datasets = scale.dataset(signal.datasets=non.na.segments)
  
  ##
  ## TODO: shift class labels according to filter delay
  ##
  
  segments = list()
  
  class.labels = GetClassLabels(scaled.datasets)
  class.labels.no.null = class.labels[which(class.labels != 0)]
  for (i in seq_along(scaled.datasets)) {
    scaled.datasets[[i]]$activity = factor(scaled.datasets[[i]]$activity, levels=class.labels)
    
    segment = scaled.datasets[[i]][0, ]
    i.col = 1 : (ncol(segment) - 1)
    seg.activity = as.character(scaled.datasets[[i]]$activity[1])
    for (i.row in seq_len(nrow(scaled.datasets[[i]]))) {
      row = scaled.datasets[[i]][i.row, ]
      curr.activity = as.character(row$activity)
      
      if ((curr.activity == 0) && (seg.activity == 0)) {
        segment = scaled.datasets[[i]][0, ]
        next
      }
      
      if ((curr.activity > 0) && (seg.activity == 0)) {
        segment = row
        seg.activity = curr.activity
      }
      
      if ((curr.activity > 0) && (seg.activity == curr.activity)) {
        segment = rbind(segment, row)
      }
      
      if ((curr.activity == 0) && (seg.activity > 0)) {        
        readings.sum = sum(segment[, i.col])
        
        if (!is.na(readings.sum)) {
          segment$activity = factor(segment$activity, class.labels.no.null)
          segments[[1 + length(segments)]] = segment
        }
        segment = scaled.datasets[[i]][0, ]
        seg.activity = 0        
      }
      
    }
  }
  
  return( segments )
}

LoadOpportunityDataFiles.unfiltered = function(subject.number, dataset.dir, input.columns.i, class.column.i, 
                                               column.names, str.adls, bw.filter, delay) {
  file.names = paste('S', subject.number, '-', str.adls, '.dat', sep='')
  file.paths = paste(dataset.dir, file.names, sep='/')
  file.paths.standardized = c()
  
  for (i in seq_along(file.paths)) {
    file.path = file.paths[i]
    file.paths.standardized[i] = paste(file.path, 'new', sep='_')
    standardize.file(file.path.original=file.path, file.path.new=file.paths.standardized[i])
  }
  
  signal.datasets = lapply(X=file.paths.standardized, FUN=read.csv, na.strings="NaN", check.names=F)
  lapply(X=file.paths.standardized, file.remove)
  
  signal.datasets = lapply(X=signal.datasets, FUN=keep.columns, c(input.columns.i, class.column.i))
  signal.datasets = lapply(X=signal.datasets, FUN=name.columns, column.names)
  original.signal.datasets = signal.datasets
  
  # shift labels according to delay
  for (i in seq_along(signal.datasets)) {
    len = nrow(signal.datasets[[i]])
    print(range(delay: len))
    print(range(1: (len-delay + 1)))
    signal.datasets[[i]]$activity[delay: len] = signal.datasets[[i]]$activity[1: (len-delay + 1)]
  }
  
  non.na.segments = list()
  segment = signal.datasets[[1]][0, ]
  i.col = 1 : (ncol(segment) - 1)
  for (i in seq_along(signal.datasets)) {
    segment.i.start = NA
    segment.i.end = NA
    prev.was.na = F
    for (i.row in seq_len(nrow(signal.datasets[[i]]))) {
      row = signal.datasets[[i]][i.row, ]
      
      sum.row = sum(row[, i.col])
      if (!is.na(sum.row)) {
        segment = rbind(segment, row)
        #         if (is.na(segment.i.start)) {
        #           segment.i.start = i.row
        #         } else {
        #           segment.i.end = i.row
        #         }
      } else {
        if (!is.na(segment.i.start) & !is.na(segment.i.end)) {
          segment = signal.datasets[[i]][segment.i.start: segment.i.end, ]
        }
        
        segment.i.start = NA
        segment.i.end = NA
        
        if (nrow(segment) > 0) {
          # apply filter
          cat('Applying filter [', nrow(segment), '] ...', sep='')
          #segment.filtered = ApplyFilter(segment.signals=segment, bw.filter=bw.filter, ignore.class=T)
          segment.filtered = segment
          cat(' done...\n')
          #stop()
          non.na.segments[[1 + length(non.na.segments)]] = segment.filtered          
          segment = signal.datasets[[1]][0, ]
        }
      }
    }
  }
  
  scaled.datasets = scale.dataset(signal.datasets=non.na.segments)
  
  ##
  ## TODO: shift class labels according to filter delay
  ##
  
  segments = list()
  
  class.labels = GetClassLabels(scaled.datasets)
  class.labels.no.null = class.labels[which(class.labels != 0)]
  for (i in seq_along(scaled.datasets)) {
    scaled.datasets[[i]]$activity = factor(scaled.datasets[[i]]$activity, levels=class.labels)
    
    segment = scaled.datasets[[i]][0, ]
    i.col = 1 : (ncol(segment) - 1)
    seg.activity = as.character(scaled.datasets[[i]]$activity[1])
    for (i.row in seq_len(nrow(scaled.datasets[[i]]))) {
      row = scaled.datasets[[i]][i.row, ]
      curr.activity = as.character(row$activity)
      
      if ((curr.activity == 0) && (seg.activity == 0)) {
        segment = scaled.datasets[[i]][0, ]
        next
      }
      
      if ((curr.activity > 0) && (seg.activity == 0)) {
        segment = row
        seg.activity = curr.activity
      }
      
      if ((curr.activity > 0) && (seg.activity == curr.activity)) {
        segment = rbind(segment, row)
      }
      
      if ((curr.activity == 0) && (seg.activity > 0)) {        
        readings.sum = sum(segment[, i.col])
        
        if (!is.na(readings.sum)) {
          segment$activity = factor(segment$activity, class.labels.no.null)
          segments[[1 + length(segments)]] = segment
        }
        segment = scaled.datasets[[i]][0, ]
        seg.activity = 0        
      }
      
    }
  }
  
  return( segments )
}

standardize.file = function(file.path.original, file.path.new) {
  file.con = file(file.path.original, open="r")
  lines = readLines(con=file.con)
  close(file.con)
  
  lines.standardized = lapply(X=lines, FUN=gsub, pattern=' +', 
                              replacement=',')
  file.create(file.path.new)
  file.con.new = file(file.path.new, open="w")
  for (line in lines.standardized) {
    writeLines(text=line, con=file.con.new)
  }
  close(file.con.new)
}

keep.columns = function(signal.dataset, columns.i) {
  signal.dataset[, columns.i]
}

name.columns = function(signal.dataset, column.names) {
  names(signal.dataset) = column.names
  signal.dataset
}

GetClassLabels = function(datasets) {
  sort(as.character(unique(do.call(rbind, datasets)$activity)))
}

scale.dataset = function(signal.datasets, min.quantile.p=0.001, max.quantile.p=0.999) {
  signal.datasets.scaled = signal.datasets
  #for (i.dataset in seq_len(signal.datasets)) {
  #  signal.datasets.scaled[[i.dataset]] = signal.datasets[[i]][0, ]
  #}
  
  for (i.col in seq_len(ncol(signal.datasets[[1]]) - 1)) {
    values = c()
    
    min.quantile = NA
    max.quantile = NA
    
    for (i.dataset in seq_along(signal.datasets)) {
      i.non.na = which(!is.na(signal.datasets[[i.dataset]][, i.col]))
      values.non.na = signal.datasets[[i.dataset]][i.non.na, i.col]
      values = c(values, values.non.na)
    }
    
    min.quantile = quantile(x=values, probs=min.quantile.p)
    max.quantile = quantile(x=values, probs=max.quantile.p)
    
    for (i.dataset in seq_along(signal.datasets)) {
      values = signal.datasets.scaled[[i.dataset]][, i.col]
      
      values.scaled = (values - min.quantile) / (max.quantile - min.quantile)
      
      i.greater.than.one = which(values.scaled > 1)
      values.scaled[i.greater.than.one] = 1
      
      i.less.than.zero = which(values.scaled < 0)
      values.scaled[i.less.than.zero] = 0
      
      signal.datasets.scaled[[i.dataset]][, i.col] = values.scaled
    }
  }
  
  return( signal.datasets.scaled )  
}


SplitDataSet = function(data.set) {
  # drop timestamps
  # separate signals from labels
  n = length(data.set)
  labels = rep(NA, n)
  trimmed.data.set = list()
  for (i in seq_along(data.set)) {
    seg = data.set[[i]]
    labels[i] = as.character(seg$activity[1])
    seg = seg[, -1]
    seg = seg[, -which(names(seg) == 'activity')]
    trimmed.data.set[[i]] = seg
  }
  return( list(labels=labels, signal.only=trimmed.data.set) )
}

DTW.distance.matrix = function(test.signal, train.signal, mc.cores=4) {
  #l = lapply(X=test.signal, FUN=DTW.distance.vector, 
  #           train.signal=train.signal)
  l = mclapply(X=test.signal, FUN=DTW.distance.vector, 
               train.signal=train.signal, mc.cores=mc.cores)
  return( do.call(cbind, args=l) )
}

DTW.distance.elem = function(train.segment, test.segment) {
  euclidean.dist = proxy::dist(x=test.segment, y=train.segment)
  alignment = dtw(x=euclidean.dist, distance.only=T)
  return( alignment$normalizedDistance )
}

DTW.distance.vector = function(test.segment, train.signal) {
  n.train.segs = length(train.signal)
  dtw.dists = sapply(X=train.signal, FUN=DTW.distance.elem, 
                     test.segment=test.segment)
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

ApplyFilter = function(segment.signals, bw.filter, ignore.class=F) {
  segment.signals.filtered = segment.signals
  
  if (ignore.class) {
    i.col = seq_len(ncol(segment.signals) - 1)
  } else {
    i.col = seq_len(ncol(segment.signals))
  }
  
  for (i in i.col) {
    #cat('\tApply filter, i =', i, 'OK\n')
    signal = segment.signals[, i]
    #print(signal)
    #print(as.numeric(filter(bw.filter, signal)))
    #print(segment.signals.filtered[, i])
    segment.signals.filtered[, i] = as.numeric(filter(bw.filter, signal))
  }
  
  plot(segment.signals[, 13], type='l')
  lines(segment.signals.filtered[, 13][4: length(segment.signals.filtered[, 13])], col='orange')
  #stop()
  
  return( segment.signals.filtered )
}

GetFilterDelay = function(segment.signals, bw.filter, ignore.class=F) {
  signal = segment.signals[, 1]
  ts.signal.filtered = filter(bw.filter, signal)
  
  return (start(ts.signal.filtered)[1])
}

#####
## running code

dataset.dir = '~/opportunity.dataset'
str.adls.train = c('ADL1', 'ADL2', 'ADL3', 'Drill')
#str.adls.train = c('ADL1')
str.adls.test = c('ADL4', 'ADL5')
#str.adls.test = c('ADL4')

subject.number = 1

subject.ns = 1:4
for (subject.number in subject.ns) {
  cat('Loading training set without filtering...\n')
  train.dataset = LoadOpportunityDataFiles(subject.number=subject.number, dataset.dir=dataset.dir, input.columns.i=input.columns.i, 
                                           class.column.i=class.column.i, column.names=column.names, str.adls=str.adls.train)
  
  cat('Loading testing set without filtering...\n')
  test.dataset = LoadOpportunityDataFiles(subject.number=subject.number, dataset.dir=dataset.dir, input.columns.i=input.columns.i, 
                                          class.column.i=class.column.i, column.names=column.names, str.adls=str.adls.test)
  
  l = SplitDataSet(data.set=train.dataset)
  train.dataset.labels = l$labels
  train.dataset.signals = l$signal.only
  
  l = SplitDataSet(data.set=test.dataset)
  test.dataset.labels = l$labels
  test.dataset.signals = l$signal.only
  
  
#   bw.filter = butter(2, W=0.2, type='low')
#   delay = 4
#   
#   cat('Loading training set with filtering...\n')
#   train.dataset.filtered = LoadOpportunityDataFiles.filtered(subject.number=subject.number, dataset.dir=dataset.dir, 
#                                                              input.columns.i=input.columns.i, class.column.i=class.column.i, 
#                                                              column.names=column.names, str.adls=str.adls.train, bw.filter=bw.filter,
#                                                              delay=delay)
#   
#   cat('Loading testing set with filtering...\n')
#   test.dataset.filtered = LoadOpportunityDataFiles.filtered(subject.number=subject.number, dataset.dir=dataset.dir, 
#                                                             input.columns.i=input.columns.i, class.column.i=class.column.i, 
#                                                             column.names=column.names, str.adls=str.adls.test, bw.filter=bw.filter,
#                                                             delay=delay)
#   
#   l = SplitDataSet(data.set=train.dataset.filtered)
#   train.dataset.filtered.labels = l$labels
#   train.dataset.filtered.signals = l$signal.only
#   
#   l = SplitDataSet(data.set=test.dataset.filtered)
#   test.dataset.filtered.labels = l$labels
#   test.dataset.filtered.signals = l$signal.only
#   
#   #bw.filter = butter(n=2, W=0.35)
#   
#   segment = train.dataset.signals[[1]]
#   segment.filtered = ApplyFilter(segment.signals=segment, bw.filter=bw.filter)
#   index.offset = GetFilterDelay(segment.signals=segment, bw.filter=bw.filter)
#   
#   #signal.filtered.trimmed = segment.filtered[, 13]
#   #signal.filtered.trimmed = signal.filtered.trimmed[1: length(signal.filtered.trimmed)]
#   
#   signal.filtered.trimmed = segment.filtered[(index.offset+1) : nrow(segment.filtered), 13]
#   
#   #x = (1 - index.offset) : (length(signal.filtered.trimmed) - index.offset)
#   x = (1 - index.offset) : (length(signal.filtered.trimmed) - index.offset)
#   plot(segment[, 13], type='l', ylim=c(0, 1))
#   lines(signal.filtered.trimmed, col='red', lty='dotted') # ok
#   #lines(segment.filtered[, 13], col='red')
#   lines(x=c(0, 0), y=c(0, 1), col='black')
#   
#   bw.filter2 = butter(n=4, W=0.3, type='low')
#   segment.unfiltered = train.dataset.signals[[1]]
#   segment.filtered = ApplyFilter(segment.signals=segment.unfiltered, bw.filter=bw.filter2, ignore.class=F)
#   plot(segment.unfiltered[, 13], type='l')
#   lines(segment.filtered[4:length(segment.unfiltered[, 13]), 13], col='orange')
#   
#   cat('Subject', subject.number, ':')
#   
#   ## with filtering
#   dist.matrix = DTW.distance.matrix(test.signal=test.dataset.filtered.signals, train.signal=train.dataset.filtered.signals, mc.cores=3)
#   k = 6
#   pred.labels = apply(X=dist.matrix, MARGIN=2, FUN=knn.class, labels=train.dataset.filtered.labels, k=k)
#   accuracy = length(which(pred.labels == test.dataset.filtered.labels)) / length(test.dataset.filtered.labels)
#   cat('Accuracy (with filtering):', accuracy, '\n')
#   
#   ## without filtering
#   dist.matrix = DTW.distance.matrix(test.signal=test.dataset.signals, train.signal=train.dataset.signals, mc.cores=3)
#   k = 3
#   pred.labels = apply(X=dist.matrix, MARGIN=2, FUN=knn.class, labels=train.dataset.labels, k=k)
#   accuracy = length(which(pred.labels == test.dataset.labels)) / length(test.dataset.labels)
#   cat('Accuracy (without filtering):', accuracy, '\n')
#   
  subject.dir = paste('subject', subject.number, sep='_')
#   #output.dir = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw', subject.dir, sep='/')
  output.dir = paste('~/R/sf_ALTLAR/output/opportunity.new.dtw', subject.dir, sep='/')
  
  dir.create(path=output.dir, showWarnings=T, recursive=T)
  
  save(train.dataset, file=paste(output.dir, 'train.dataset.data', sep='/'))
  save(test.dataset, file=paste(output.dir, 'test.dataset.data', sep='/'))
  #save(train.dataset.filtered, file=paste(output.dir, 'train.dataset.filtered.data', sep='/'))
  #save(test.dataset.filtered, file=paste(output.dir, 'test.dataset.filtered.data', sep='/'))
}
stop('enough')

##### simulation functions
UpdateDistanceMatrix = function(old.matrix, train.segment, 
                                test.signal) {
  distance.vector = DTW.distance.vector(test.segment=train.segment, 
                                        train.signal=test.signal)
  
  return( rbind(old.matrix, distance.vector) )
}

SimulateOpportunityDTW = function(train.set, test.set, k, 
                                  metric.function, 
                                  selection.function, 
                                  max.size, ...) {
  l = SplitDataSet(data.set=train.set)
  train.signal = l$signal.only
  train.labels = l$labels
  
  l = SplitDataSet(data.set=test.set)
  test.signal = l$signal.only
  test.labels = l$labels
  
  class.labels = unique(test.labels)
  
  ann.signal = list()
  ann.labels = c()
  q.i = c()
  cms = list()
  distance.matrix = NULL
  predicteds = list()
  
  if (is.null(max.size)) {
    max.size = length(train.signal)
  }
  
  timestamp = 1
  while (length(ann.signal) < max.size) {
    cat(timestamp, ' (', length(ann.signal), '/', 
        length(train.signal), ')', sep='')
    i.segment = sample(length(train.signal), 1)    
    segment = train.signal[[i.segment]]
    train.signal = train.signal[-i.segment]
    label = train.labels[i.segment]
    train.labels = train.labels[-i.segment]
    
    if (length(ann.signal) < 1) {
      prediction.probs = 1
    } else {
      dist.vector = DTW.distance.vector(test.segment=segment, 
                                        train.signal=ann.signal)
      prediction.probs = knn.probability(dist.vector=dist.vector, 
                                         labels=ann.labels, 
                                         k=k)
    }
    
    #print(prediction.probs)
    
    # decide whether to annotate the segment
    metric = metric.function(prediction.probs=prediction.probs)
    #cat('\nmetric =', metric, '\n')
    selection.thresh = selection.function(metric=metric, ...)
    #cat('thresh =', selection.thresh, '\n')
    
    rand.attempt = runif(1)
    ask = rand.attempt < selection.thresh
    if (ask) {
      cat('\n\tasking (', metric, ' prob => ', selection.thresh, 
          ' thresh <= ', rand.attempt, ')\n', sep='')
      ann.set.next.i = 1 + length(ann.signal)
      ann.signal[[ann.set.next.i]] = segment
      ann.labels[ann.set.next.i] = label
      
      # update DTW distance matrix
      cat('\tupdating distance matrix... ')
      time.start = Sys.time()
      distance.matrix = UpdateDistanceMatrix(old.matrix=distance.matrix, 
                                             train.segment=segment, 
                                             test.signal=test.signal)
      time.end = Sys.time()
      cat(round(time.end-time.start, 2), 's\n', sep='')
      
      # compute confusion matrix
      cat('\testimating labels...\n')
      pred.labels = apply(X=distance.matrix, MARGIN=2, FUN=knn.class, 
                          labels=ann.labels, k=k)
      cat('\tconfusion matrix...\n')
      #print(actual)
      #print(predicted)
      #print(class.labels)
      cm = ConfusionMatrix(actual=test.labels, 
                           predicted=pred.labels, 
                           class.labels=class.labels)
      cat('-')
      cms.next.i = 1 + length(cms)
      cms[[cms.next.i]] = cm
      cat('-')
      q.i[cms.next.i] = timestamp
      cat('-')
      predicteds[[cms.next.i]] = pred.labels
      cat('-\n')
      cat('\tF-score =', WeightedFMeasure(cm), '\n')
    } else {
      # put it back
      train.signal.next.i = 1+length(train.signal)
      train.signal[[train.signal.next.i]] = segment
      train.labels[train.signal.next.i] = label
      cat(' ')
    }
    
    timestamp = 1 + timestamp
  }
  
  return( list(actual=test.labels, cms=cms, q.i=q.i, 
               predicteds=predicteds) )
}

k=4
metric.function = Offline.Confidence
selection.function = Online.BMargin
max.size = 200
gamma = 2
l = SimulateOpportunityDTW(train.set=train.dataset.filtered, test.set=test.dataset.filtered, k=k, metric.function=metric.function, 
                           selection.function=selection.function, max.size=max.size, gamma=gamma)

SimulateOpportunityDTW.sample = function(train.sets, test.sets, 
                                         repetitions, 
                                         opportunity.dir, 
                                         destination.dir,
                                         file.name,
                                         ...) {
  ts.ts = list()
  for (test.set in test.sets) {
    train.set = train.sets[[1]]
    subject.dir = paste('subject', test.set$subject.number, sep='_')
    for (ts in train.sets) {
      #cat(' - train.set$subject.number =', ts$subject.number, '\n')
      #cat(' - test.set$subject.number =', test.set$subject.number, '\n')
      if (ts$subject.number == test.set$subject.number) {
        train.set = ts
        #cat('using this train/test combination\n\n')
        l = list(train.set=train.set, test.set=test.set, 
                 subject.number=ts$subject.number)
        ts.ts[[1 + length(ts.ts)]] = l
        break
      } else {
        #cat('trying other train/test combination\n\n')
      }
    }
    cat('running... (', train.set$subject.number, ', ', test.set$subject.number, ')\n', sep='')
    cat(' train.set size:', length(train.set$data), '\n')
    cat(' test.set  size:', length(test.set$data), '\n')
  }
  
  grid = expand.grid(comb.i=seq_along(ts.ts), rep=seq_len(repetitions))
  i.grid = sample(x=nrow(grid), size=nrow(grid))
  grid = grid[i.grid, ]
  
  all.res = foreach (i.row = seq_len(nrow(grid))) %dopar% {
    row = grid[i.row, ]
    print(row)
    comb = ts.ts[[row$comb.i]]
    
    train.set = comb$train.set
    cat('length(train.set$data) =', length(train.set$data), '\n')
    
    test.set = comb$test.set
    cat('length(test.set$data) =', length(test.set$data), '\n')
    
    subject.number = comb$subject.number
    cat('subject.number =', subject.number, '\n')
    
    source('R/opportunity.dtw.R')      
    
    res = SimulateOpportunityDTW(train.set=train.set$data, 
                                 test.set=test.set$data,  ...)
    cat('done...')
    list(res=res, subject.number=subject.number)
  }
  
  SaveObject(obj=all.res, var.name='all.res', 
             dir=paste(opportunity.dir, destination.dir, sep='/'))
  #stop('saved all.res')
  l = list()
  for (i in seq_along(test.sets)) {
    l[[i]] = list()
    l[[i]]$reps = list()
    l[[i]]$subject.number = i
  }
  
  for (elem in all.res) {
    cat('++ elem$subject.number =', elem$subject.number, '\n')
    cat('++++', length(l[[elem$subject.number]]$reps), '->')
    len = length(l[[elem$subject.number]]$reps)
    l[[elem$subject.number]]$reps[[1+len]] = elem$res
    cat(length(l[[elem$subject.number]]$reps), '\n')
  }
  
  for (elem in l) {
    subject.dir = paste('subject', elem$subject.number, sep='_')
    cat('---- Saving')
    dir = paste(opportunity.dir, destination.dir, 
                subject.dir, sep='/')
    SaveObject(obj=elem$reps, var.name=file.name, dir=dir)
  }
}

Offline.Margin = function(prediction.probs) {
  prediction.probs = sort(prediction.probs, decreasing=T)
  return( prediction.probs[1] - prediction.probs[2] )
}

Offline.Confidence = function(prediction.probs) {
  return( max(prediction.probs) )
}

Online.BMargin = function(metric, gamma) {
  return( exp(-gamma*metric) )
}

Online.Random = function(metric, f) {
  return( f )
}
