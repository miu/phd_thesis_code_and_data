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

setwd('~/R/sf_ALTLAR/')

library('RWeka')
library('doRedis')

source('R/subjects.search.R')
source('R/file.utils.R')

dataset.dir = '~/opportunity.dataset'

ExtractFeatures = function(frame) {
  x = frame[, 2]
  y = frame[, 3]
  z = frame[, 4]
  act = frame$activity[1]
  
  f = c(mean(x), mean(y), mean(z),
        var(x), var(y), var(z),
        cor(x, y), cor(y, z), cor(z, x),
        act)
  names(f) = c('mean.x', 'mean.y', 'mean.z', 
               'var.x', 'var.y', 'var.z',
               'cor.xy', 'cor.yz', 'cor.zx',
               'activity')
  
  return( f )
}

SlidingWindow = function(segment, win.len, win.incr) {
  init.timestamp = segment[1, 'timestamp']
  i = 1
  diff.timestamp = 0
  
  win.start.i = 1
  
  while ((i < nrow(segment)) & (diff.timestamp < win.incr)) {
    i = 1 + i
    this.timestamp = segment[i, 'timestamp']
    diff.timestamp = this.timestamp - init.timestamp
  }   
  win.incr.i = i
  
  while ((i < nrow(segment)) & (diff.timestamp < win.len)) {
    i = 1 + i
    this.timestamp = segment[i, 'timestamp']
    diff.timestamp = this.timestamp - init.timestamp
  }  
  win.end.i = i  
    
  frames = list()
  
  this.win = segment[win.start.i: win.end.i, ]
  frames[[1 + length(frames)]] = this.win
  
  i = win.end.i + 1
  incr.timestamp = segment[win.incr.i, 'timestamp']
  next.incr.i = NULL
  while ((i < nrow(segment))) {
    i = 1 + i
    #cat('i =', i, '\n')
    this.timestamp = segment[i, 'timestamp']
    diff.timestamp = this.timestamp - incr.timestamp
    
    if (is.null(next.incr.i)) {
      if (diff.timestamp >= win.incr) {
        next.incr.i = i - 1
      }
    }
    
    if (diff.timestamp >= win.len) {
      this.win = segment[win.incr.i: i, ]
      frames[[1 + length(frames)]] = this.win
      
      #cat(' new window', segment[win.incr.i, 'timestamp'], segment[i, 'timestamp'], '\n')
      
      win.incr.i = next.incr.i
      #cat(' next incr.i', segment[win.incr.i, 'timestamp'], '\n')
      
      incr.timestamp = segment[win.incr.i, 'timestamp']
      
      next.incr.i = NULL
      i = win.incr.i + 1
      #cat(' next i', segment[i, 'timestamp'], '\n\n')
      
    }    
  }
  
  return( frames )
}

#OrganizeOpportunityDS = function(dataset.dir) {
#  old.wd = getwd()
setwd(dataset.dir)
pattern = '^S[1-5].*$'

subjects.features = list()

#input.columns.i = 1: 114
timestamp.column.i = 1
input.columns.i = 23:25
# input.columns = c(23:25, 32:34)
class.column.i = 116
keep.column.i = c(timestamp.column.i, input.columns.i, class.column.i)

NUM_WORKERS = 8

queue.name='opportunity.queue'
registerDoRedis(queue=queue.name)
startLocalWorkers(n=NUM_WORKERS, queue=queue.name, timeout=1)

### NOTE TO SELF
## I need to change *2* files
win.len = 500
win.incr = 250

subject.file.names = dir(pattern=pattern)
subjects.features = foreach (file.name = subject.file.names) %dopar% {
#for (file.name in subject.file.names[2]) {
  split.pattern = '-'
  s = strsplit(x=file.name, split=split.pattern)[[1]][1]
  split.pattern = 'S'
  subject.number = as.integer(strsplit(x=s, split=split.pattern)[[1]][2])
  subject.data = list()
  
  cat('reading...\n')
  
  file.text = readChar(file.name, file.info(file.name)$size)
  file.text = gsub(pattern='[ ]+', replacement=',', x=file.text)
  file.name.new = paste('new', file.name, sep='.')
  writeChar(object=file.text, con=file.name.new, nchars=nchar(file.text))
  
  file.data = read.csv(file=file.name.new, header=F, sep=',', na.strings='NaN')
  file.remove(file.name.new)
  file.data = file.data[, keep.column.i]
  column.names = colnames(file.data)
  column.names[length(column.names)] = 'activity'
  column.names[1] = 'timestamp'
  names(file.data) = column.names
  
  i = 1
  
  last.na.row = rep(1, times=ncol(file.data))
  
  cat('finding segments...\n')
  min.activity = 500000
  while (i <= nrow(file.data)) {          
    segment = file.data[0, ]
    current.row = file.data[i, ]
    if (is.na(current.row$activity)) {
      i = 1 + i
      next
    }
    if (current.row$activity > min.activity) {
      cat('(', i, ',\n', sep='')
      for (col.i in seq_len(ncol(file.data))) {
        if (is.na(file.data[i, col.i])) {
          row.i = i - 1
          while (is.na(file.data[row.i, col.i]) & (row.i > last.na.row[col.i])) {
            #cat(file.data[row.i, col.i])
            row.i = row.i - 1
          }       
          last.na.row[col.i] = i
          cat('  NA:', col.i, ':', i, '<-', row.i, '\n')
          file.data[i, col.i] = file.data[row.i, col.i]
        }
      }
      
      #cat(' new segment\n')
      
      segment = current.row
      k = i + 1
      while (k <= nrow(file.data)) {
        next.row = file.data[k, ]
        if (is.na(next.row$activity)) {
          k = k + 1
          next
        }
        if (next.row$activity == current.row$activity) {
          segment = rbind(segment, next.row)
        } else {
          i = k - 1
          break
        }
        k = k + 1
      }
      cat(i, ')\n', sep='')
      
      cat(' sliding window...\n')
      frames = SlidingWindow(segment=segment, win.len=win.len, win.incr=win.incr)
      
      cat(' extracting feature vectors from segment frames...\n')
      l = lapply(X=frames, FUN=ExtractFeatures)
      segment.feature.vectors = do.call(what=rbind, args=l)
      
      segment.feature.vectors = data.frame(segment.feature.vectors)
      segment.feature.vectors$activity = factor(segment.feature.vectors$activity)
      
      subject.data[[1 + length(subject.data)]] = data.frame(segment.feature.vectors)
      
      ### TODO
      ## extract features
      #num.cols = ncol(file.data) - 1
      #num.features = 2 * num.cols
      #f.vector = vector(mode='double', length=num.features + 1)
      #for (col.i in seq_len(num.cols)) {
      #  seg.col = segment[, col.i]
      #  f.vector[2*col.i - 1] = mean(seg.col)
      #  f.vector[2*col.i] = var(seg.col)
      #}
      #if (is.na(f.vector[90])) {
      #  print(segment)
      #  stop('problem at 90')
      #}
      #f.vector[length(f.vector)] = segment$activity[1] 
      #subject.data = rbind(subject.data, f.vector)
    }
    
    i = i + 1
  }
  
  #num.cols = ncol(subject.data)
  #column.names = vector(mode='character', length=num.cols)
  #
  #for (i in seq_len(num.cols)) {
  #  column.names[i] = paste('f', i, sep='')
  #}
  #column.names[num.cols] = 'activity'
  #rownames(subject.data) = seq_len(nrow(subject.data))
  #subject.data = data.frame(subject.data)
  #names(subject.data) = column.names
  #
  #subject.data$activity = as.factor(unlist(subject.data$activity))
  #for (col.i in 1: (ncol(subject.data)-1) ) {
  #  subject.data[, col.i] = unlist(subject.data[, col.i])
  #}
  
  list( subject.number=subject.number, data=subject.data)
}

#SaveObject(obj=subjects.features, var.name='opportunity.subjects.features',
#           dir='~/R/sf_ALTLAR/output/opportunity')
SaveObject(obj=subjects.features, var.name='opportunity.subjects.features.250.incr',
           dir='~/R/sf_ALTLAR/output/opportunity')

#stop('okay')

cat('collapsing subjects.features...')

subjects.features.old = subjects.features

subjects.features.collapsed = list()

for (s.f in subjects.features) {
  data.appended = F
  for (s.f.c.i in seq_along(subjects.features.collapsed)) {
    s.f.c = subjects.features.collapsed[[s.f.c.i]]
    if (s.f.c$subject.number == s.f$subject.number) {
      subjects.features.collapsed[[s.f.c.i]]$data = c(s.f.c$data, s.f$data)
      data.appended = T
      break
    }
  }
  
  if (!data.appended) {
    next.i = 1 + length(subjects.features.collapsed)
    subjects.features.collapsed[[next.i]] = list()
    subjects.features.collapsed[[next.i]]$subject.number = s.f$subject.number
    subjects.features.collapsed[[next.i]]$data = s.f$data
  }
}
SaveObject(obj=subjects.features.collapsed, 
           var.name='opportunity.subjects.features.collapsed.250.incr',
           dir='~/R/sf_ALTLAR/output/opportunity')

subjects.features = subjects.features.collapsed

stop('okay')

#  setwd(old.wd)
#  return( subjects.features )
#}

################################

class.labels = GetAllLabels(subjects.features=subjects.features)

pca.filter.name = 'weka/filters/unsupervised/attribute/PrincipalComponents'
pca.filter = make_Weka_filter(name=pca.filter.name, class=class.labels)

s.f = LoadObject(dir='~/R/sf_ALTLAR/output/organize', file.name='subjects.features')

data.prev = s.f[[1]]$data
data.now = subjects.features[[1]]$data

na.cols = rep(0, ncol(data.now))
na.rows = rep(0, nrow(data.now))
for (col.i in seq_along(data.now)) {
  sum.na = sum(is.na(data.now[, col.i]))
  if (sum.na > 0) {
    na.cols[col.i] = sum.na
  }
}
keep.col.i = which(na.cols != nrow(data.now))
data.now.clean.col = data.now[, keep.col.i]

for (row.i in seq_len(nrow(data.now.clean.col))) {
  sum.na = sum(is.na(data.now.clean.col[row.i, ]))
  if (sum.na > 0) {
    na.rows[row.i] = sum.na
  }
}
keep.row.i = which(na.rows == 0)
data.now.clean = data.now.clean.col[keep.row.i, ]

pca.data = data.now.clean[, 1: (ncol(data.now.clean)-1)]
pca.m = prcomp(formula=~., scale.=T, data=pca.data)

row.1 = pca.data[1, ]
row.1.scaled = as.matrix((row.1-m$center) / m$scale)

v = row.1.scaled %*% m$rotation # === m$x

# generate empty PCA model m.pca

# while training set size not big enough
## sample random segment s from pool
## pass s through m.pca
## generate predictions for s using m.pred
## if s should be annotated
### add s to training set
### update m.pca
### pass training set through m.pca
### update m.pred
### evaluate m.pred
## else
### put back s in pool
## end if
# end while
