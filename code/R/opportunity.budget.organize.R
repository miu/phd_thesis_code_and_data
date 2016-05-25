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

ExtractFeatures.3.axis.acc.only = function(frame) {
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

ExtractFeatures.6.axis.acc.only = function(frame) {
  x.r = frame[, 2]
  y.r = frame[, 3]
  z.r = frame[, 4]
  x.l = frame[, 5]
  y.l = frame[, 6]
  z.l = frame[, 7]
  
  act = frame$activity[1]
  
  f = c(mean(x.r), mean(y.r), mean(z.r), mean(x.l), mean(y.l), mean(z.l),
        var(x.r), var(y.r), var(z.r), var(x.l), var(y.l), var(z.l),
        cor(x.r, y.r), cor(y.r, z.r), cor(z.r, x.r), 
        cor(x.l, y.l), cor(y.l, z.l), cor(z.l, x.l),
        act)
  names(f) = c('mean.x.r', 'mean.y.r', 'mean.z.r', 'mean.x.l', 'mean.y.l', 'mean.z.l', 
               'var.x.r', 'var.y.r', 'var.z.r', 'var.x.l', 'var.y.l', 'var.z.l',
               'cor.xy.r', 'cor.yz.r', 'cor.zx.r', 'cor.xy.l', 'cor.yz.l', 'cor.zx.l',
               'activity')
  
  return( f )
}

ExtractFeatures.all = function(frame) {
  i.col.signals = 2: (ncol(frame)-1)
  act = frame$activity[1]
  
  f = rep(0, length(i.col.signals) + 1)  
  f.names = rep('', length(i.col.signals) + 1)
  
  for (i in seq_along(i.col.signals)) {
    signal.i = i.col.signals[i]
    signal = frame[, signal.i]
    f[i] = mean(signal)
    f.names[i] = paste('mean', i, sep='.')
  }
  
  f[length(f)] = act
  f.names[length(f)] = 'activity'
  names(f) = f.names  
  
  #print(f)  
  #stop()
  
  return( f )
}

#ExtractFeatures = ExtractFeatures.6.axis.acc.only
ExtractFeatures = ExtractFeatures.all

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

setwd(dataset.dir)
pattern = '^S[1-5].*$'

#input.columns.i = 1: 114
timestamp.column.i = 1
#input.columns.i = 23:25
#input.columns.i = 2:114
#input.columns.i = 23:25 # right wrist
#input.columns.i = 32:34 # left wrist
#input.columns.i = c(23:25, 32:34) # both wrists
input.columns.i = c(8:10, # LUA^
                    11:13, # RUA_
                    17:19, # BACK
                    26:28, # RUA^
                    29:31) # LUA_

class.column.i = 116
keep.column.i = c(timestamp.column.i, input.columns.i, class.column.i)

NUM_WORKERS = 8

queue.name='opportunity.budget.queue'
registerDoRedis(queue=queue.name)
startLocalWorkers(n=NUM_WORKERS, queue=queue.name, timeout=1)

### NOTE TO SELF
## I need to change *2* files
win.len = 500
win.incr = 250

budget.subjects = c(1, 2, 3, 4)
train.task.numbers = c(0, 1, 2, 3) # task 0 is Drill.dat
test.task.numbers = c(4, 5)

subject.file.names = dir(pattern=pattern)
subjects.features = foreach(file.name = subject.file.names) %dopar% {
  #for (file.name in subject.file.names[2]) {
  split.pattern = '-'
  atoms = strsplit(x=file.name, split=split.pattern)[[1]]
  s = atoms[1]
  split.pattern = 'S'
  subject.number = as.integer(strsplit(x=s, split=split.pattern)[[1]][2])
  if (!(subject.number %in% budget.subjects)) {
    cat('skipping subject', subject.number, '\n')
    NULL
  } else {  
    subject.data = list()
    
    task = atoms[2]
    task.number = 0 # drill
    if (length(grep(pattern='Drill\\.dat', x=task)) == 0) {
      task.number = as.integer(strsplit(x=task, split='')[[1]][4])
    }  
    
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
    
    segments = list()
    
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
        segments[[1 + length(segments)]] = segment
      }
      
      i = i + 1
    }
    
    list( subject.number=subject.number, data=segments, 
          task.number=task.number)
  }
}

opportunity.dir = '~/R/sf_ALTLAR/output/opportunity/dtw'

#SaveObject(obj=subjects.features, var.name='opportunity.subjects.features',
#           dir='~/R/sf_ALTLAR/output/opportunity')
SaveObject(obj=subjects.features, var.name='opportunity.subjects.features.250.incr',
           dir=opportunity.dir)

stop('okay')

cat('making training and testing sets...\n')
subjects.features.train = list()
subjects.features.test = list()
for (s.f in subjects.features) {
  if (is.null(s.f)) {
    next
  }
  data.appended = F
  if (s.f$task.number %in% test.task.numbers) {
    for (s.f.t.i in seq_along(subjects.features.test)) {
      s.f.t = subjects.features.test[[s.f.t.i]]
      if (s.f.t$subject.number == s.f$subject.number) {
        subjects.features.test[[s.f.t.i]]$data = c(s.f.t$data, s.f$data)
        data.appended = T
        break
      }
    }
    
    if (!data.appended) {
      next.i = 1 + length(subjects.features.test)
      subjects.features.test[[next.i]] = list()
      subjects.features.test[[next.i]]$subject.number = s.f$subject.number
      subjects.features.test[[next.i]]$data = s.f$data
    }
  } 
  
  ## keep separate training sets for ADL1, ADL2, ADL3, Drill
  #if (s.f$task.number %in% train.task.numbers) {
  #  current.len = length(subjects.features.train)
  #  subjects.features.train[[1 + current.len]] = s.f
  #}
  
  ## concatenate TRAINING set
  if (s.f$task.number %in% train.task.numbers) {
    for (s.f.t.i in seq_along(subjects.features.train)) {
      s.f.t = subjects.features.train[[s.f.t.i]]
      if (s.f.t$subject.number == s.f$subject.number) {
        subjects.features.train[[s.f.t.i]]$data = c(s.f.t$data, s.f$data)
        data.appended = T
        break
      }
    }
    
    if (!data.appended) {
      next.i = 1 + length(subjects.features.train)
      subjects.features.train[[next.i]] = list()
      subjects.features.train[[next.i]]$subject.number = s.f$subject.number
      subjects.features.train[[next.i]]$data = s.f$data
    }
  }
}

#stop('before cleaning')

##cleaning data
for (i in seq_along(subjects.features.test)) {
  s.data = subjects.features.test[[i]]$data
  i.null = c()
  for (k in seq_along(s.data)) {
    seg = s.data[[k]]
    if (sum(is.na(seg)) > 0) {
      i.null = c(i.null, k)
    }
  }
  if (length(i.null) > 0) {
    subjects.features.test[[i]]$data = subjects.features.test[[i]]$data[-i.null]
  }
}

for (i in seq_along(subjects.features.train)) {
  s.data = subjects.features.train[[i]]$data
  i.null = c()
  for (k in seq_along(s.data)) {
    seg = s.data[[k]]
    if (sum(is.na(seg)) > 0) {
      i.null = c(i.null, k)
    }
  }
  if (length(i.null) > 0) {
    subjects.features.train[[i]]$data = subjects.features.train[[i]]$data[-i.null]
  }
}

SaveObject(obj=subjects.features.test, 
           var.name='opportunity.subjects.features.test.250.incr',
           dir=opportunity.dir)

SaveObject(obj=subjects.features.train, 
           var.name='opportunity.subjects.features.train.250.incr',
           dir=opportunity.dir)
