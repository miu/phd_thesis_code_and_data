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

library(RWeka)

source('../ml.utils.R')
source('../file.utils.R')

LoadDatasets = function() {
  dataset.dir = '~/activity_data'
  
  subject.dirs = c(
    'participant_1',
    'participant_2',
    'participant_3',
    'participant_4',
    'participant_5',
    'participant_6',
    'participant_7',
    'participant_8',
    'participant_9',
    'participant_10'
  )
  
  instances.file.paths = rep(x=NA, times=length(subject.dirs))
  datasets = list()
  for (i in seq_along(subject.dirs)) {
    instances.file.paths[i] = paste(dataset.dir, subject.dirs[i], 'instances.csv', sep='/')
    
    if (!file.exists(instances.file.paths[i])) {
      msg = paste(instances.file.paths[i], 'does not exist.')
      stop(msg)
    }
    
    instances = read.csv(file=instances.file.paths[i], header=T)
    act = as.character(instances$activity)
    class.labels = sort(unique(act))
    instances$activity = factor(x=act, levels=class.labels)
    
    datasets[[i]] = list(subject.dir=subject.dirs[i], instances=instances)
  }
  
  return( datasets )
}

AggregatePerformance = function(l.perfs) {
  noise.levels = c()
  subject.dirs = c()
  
  for (l.perf in l.perfs) {
    noise.levels = c(noise.levels, l.perf$noise.level)
    subject.dirs = c(subject.dirs, l.perf$subject.dir)
  }
  
  noise.levels.unique = sort(unique(noise.levels))
  subject.dirs.unique = sort(unique(subject.dirs))
  
  l.perfs.reorg = list()  
  for (noise.level in noise.levels.unique) {
    for (subject.dir in subject.dirs.unique) {
      m.perf.frame = NULL
      m.perf.seg = NULL
      subject.i = NULL
      for (l.perf in l.perfs) {
        if ((l.perf$subject.dir == subject.dir) & (l.perf$noise.level == noise.level)) {
          m.perf.frame = rbind(m.perf.frame, l.perf$perfs.frame)
          m.perf.seg = rbind(m.perf.seg, l.perf$perfs.seg)
          subject.i = l.perf$subject.i
        }
      }
      
      avg.perf.frame = apply(X=m.perf.frame, MARGIN=2, FUN=mean)
      avg.perf.seg = apply(X=m.perf.seg, MARGIN=2, FUN=mean)
      
      l = list()
      l$perfs.frame = avg.perf.frame
      l$perfs.seg = avg.perf.seg
      l$subject.dir = subject.dir
      l$subject.i = subject.i
      l$noise.level = noise.level
      l.perfs.reorg[[1 + length(l.perfs.reorg)]] = l
    }
  }
  
  return( l.perfs.reorg )
}

FilterBySubjectDir = function(l.perfs, include.subject.dirs) {
  l = list()
  for (l.perf in l.perfs) {
    if (l.perf$subject.dir %in% include.subject.dirs) {
      l[[1 + length(l)]] = l.perf
    }
  }
  return( l )
}

SplitIntoSegments = function(dataset.instances) {
  l.segments = list()
  insts = dataset.instances$instances
  
  i.non.ml.col = which(names(insts) %in% c("segment_number", "first_timestamp", "last_timestamp"))
  
  i = 1
  seg.num = insts[i, ]$segment_number
  segment = insts[0, ]
  seg.timestamps = c()
  
  while (i <= nrow(insts)) {
    row = insts[i, ]
    current.seg.num = row$segment_number
    row = row[, -i.non.ml.col]
    
    if (current.seg.num == seg.num) {
      segment = rbind(segment, row)
    } else {
      l.segments[[1 + length(l.segments)]] = segment
      segment = row
      seg.num = current.seg.num
    }
    
    i = i + 1
  }
  l.segments[[1 + length(l.segments)]] = segment
  
  return( list(subject.dir=dataset.instances$subject.dir, l.segments=l.segments) )
}

FilterSegments = function(dataset.segments, exclude.labels) {
  i = 1
  while( i <= length(dataset.segments$l.segments)) {
    act = dataset.segments$l.segments[[i]]$activity
    act = as.integer(as.character(act))
    
    act = unique(act)
    if (length(act) != 1) {
      msg = paste('More than one activity in segment:', paste(act, collapse=' '))
      stop(msg)
    }
    
    if (act %in% exclude.labels) {
      dataset.segments$l.segments[[i]] = NULL
    } else {
      i = i + 1
    }
  }
  
  remaining.class.labels = sort(unique(do.call(rbind, dataset.segments$l.segments)$activity))
  remaining.class.labels = as.character(remaining.class.labels)
  for (i in seq_along(dataset.segments$l.segments)) {
    act = as.character(dataset.segments$l.segments[[i]]$activity)
    act = factor(x=act, levels=remaining.class.labels)
    dataset.segments$l.segments[[i]]$activity = act
  }
  
  return( dataset.segments )
}

GetRatioAnnotated = function(dataset.segments.all, dataset.segments.annotated) {
  length(dataset.segments.annotated$l.segments) / length(dataset.segments.all$l.segments)
}

PerformanceMetrics = function(datasets.segments.annotated, num.reps) {
  
  classifier.name = 'weka/classifiers/meta/Bagging'
  weak.classifier.name = 'weka.classifiers.bayes.NaiveBayes'
  weka.control = Weka_control(W=weak.classifier.name, 
                              I=30)
  
  #classifier.name = 'weka/classifiers/rules/ZeroR'
  #weka.control = Weka_control()
  
  class.labels = sort(unique(as.character(do.call(rbind, datasets.segments.annotated[[1]]$l.segments)$activity)))
  
  l.perfs = foreach(i.subject = seq_along(datasets.segments.annotated)) %dopar% {
    #for (i.subject in seq_along(datasets.segments.annotated)) {
    
    source('~/R/sf_ALTLAR/R/user.study/plot.R')
    
    cat('i.subject =', i.subject, '\n')
    l.segs = datasets.segments.annotated[[i.subject]]$l.segments
    perfs.frame = rep(0, length(l.segs))
    perfs.seg = rep(0, length(l.segs))
    
    
    #for (size in seq_along(l.segs)) {
    for (size in seq_len(4)) {
      #foreach (size = seq_along(l.segs)) %dopar% {
      
      cat('\t', size, '/', length(l.segs), ' ', sep='')
      for (i.rep in seq_len(num.reps)) {
        cat('.')
        i.train = sample(x=length(l.segs))
        train.data = do.call(what=rbind, args=l.segs[i.train])
        test.data = do.call(what=rbind, args=l.segs[-i.train])
        
        cm.frame = CrossValidateWithSubsample(classifier.name=classifier.name, weka.control=weka.control, 
                                              data.subsample=train.data, data.complementary=test.data, 
                                              class.labels=class.labels, num.folds=10)
        perfs.frame[size] = perfs.frame[size] + (WeightedFMeasure(cm=cm.frame) / num.reps)
        
        cat('*')
        train.data = l.segs[i.train]        
        test.data = l.segs[-i.train]
        
        cm.seg = CrossValidateWithSubsample.segments(classifier.name=classifier.name, weka.control=weka.control, 
                                                     l.data.subsample=train.data, l.data.complementary=test.data, 
                                                     class.labels=class.labels, num.folds=10)
        perfs.seg[size] = perfs.seg[size] + (WeightedFMeasure(cm=cm.seg) / num.reps)
      }
      cat('\n')
    }    
    
    list(perfs.frame=perfs.frame, perfs.seg=perfs.seg, 
         subject.dir=datasets.segments.annotated[[i.subject]]$subject.dir,
         subject.i=i.subject)
  }  
  
  return( l.perfs )
}

PerformanceMetrics.noise = function(datasets.segments.annotated, num.reps, noise.levels,
                                    classifier.name, weka.control) {
  
  i.subjects = seq_along(datasets.segments.annotated)
  reps = seq_len(num.reps)
  grid = expand.grid(rep.no=reps, noise.level=noise.levels, i.subject=i.subjects)
  
  i.row = seq_len(nrow(grid))
  i.row.s = as.character(i.row)
  
  order.i = order(i.row.s, decreasing=F)
  order.i.flip = rep(NaN, length(order.i))
  
  for (i in seq_along(order.i)) {
    index = order.i[i]
    value = i
    order.i.flip[index] = value
  }
  grid = grid[order.i.flip, ]
  
  cat('Total # of jobs:', nrow(grid), '\n')
  
  l.perfs = foreach(i.row = seq_len(nrow(grid))) %dopar% {
    source('~/R/sf_ALTLAR/R/user.study/plot.R')
    
    i.subject = grid$i.subject[i.row]
    noise.level = grid$noise.level[i.row]
    
    cat('i.subject =', i.subject, '\n')
    l.segs = datasets.segments.annotated[[i.subject]]$l.segments
    
    class.labels = sort(unique(as.character(do.call(rbind, l.segs)$activity)))
    #print(class.labels); stop();
    
    perfs.frame = rep(0, length(l.segs))
    perfs.seg = rep(0, length(l.segs))
    
    ### TODO 
    ## introduce noise
    introduce.noise = function(l.segs, noise.level, class.labels) {
      n = length(l.segs)
      num.noisy.segs = ceiling(noise.level * n)
      
      #cat('noise level =', noise.level, '\n')
      #cat('#noisy segs =', num.noisy.segs, '\n')
      
      i.noisy.segs = sample(x=n, size=num.noisy.segs)
      
      for (i.noisy in i.noisy.segs) {
        actual.label = as.character(l.segs[[i.noisy]]$activity[1])
        all.other.labels = setdiff(class.labels, actual.label)
        
        noisy.label = sample(x=all.other.labels, size=1)
        #print('before'); print(l.segs[[i.noisy]]$activity)
        for (i in seq_len(nrow(l.segs[[i.noisy]]))) {
          l.segs[[i.noisy]]$activity[i] = noisy.label
        }
        #print('after'); print(l.segs[[i.noisy]]$activity);# stop()
      }
      
      return( l.segs )
    }
    
    l.segs = introduce.noise(l.segs=l.segs, noise.level=noise.level, class.labels=class.labels)
    
    for (size in seq_along(l.segs)) {
      #for (size in seq_len(5)) {
      cat(size, '/', length(l.segs), ' ', sep='')
      
      i.train = sample(x=length(l.segs), size=size)
      #cat('i.train =', i.train, '\n')
      #cat('size =', size, '\n')
      
      #cat('(frame eval', datasets.segments.annotated[[i.subject]]$subject.dir, ')' )
      train.data = do.call(what=rbind, args=l.segs[i.train])
      test.data = do.call(what=rbind, args=l.segs[-i.train])
      
      #cat('CrossValidateWithSubsample...')
      cm.frame = CrossValidateWithSubsample(classifier.name=classifier.name, weka.control=weka.control, 
                                            data.subsample=train.data, data.complementary=test.data, 
                                            class.labels=class.labels, num.folds=10)
      #cat(' OK!\n')
      perfs.frame[size] = WeightedFMeasure(cm=cm.frame)
      
      
      #cat('(seg eval', datasets.segments.annotated[[i.subject]]$subject.dir, ')' )
      train.data = l.segs[i.train]        
      test.data = l.segs[-i.train]
      
      #cat('CrossValidateWithSubsample.segments...')
      cm.seg = CrossValidateWithSubsample.segments(classifier.name=classifier.name, weka.control=weka.control, 
                                                   l.data.subsample=train.data, l.data.complementary=test.data, 
                                                   class.labels=class.labels, num.folds=10)
      #cat(' OK!\n')
      perfs.seg[size] = WeightedFMeasure(cm=cm.seg)
    }
    cat(' *** OK *** \n')
    
    print(warnings())
    
    list(perfs.frame=perfs.frame, perfs.seg=perfs.seg, 
         subject.dir=datasets.segments.annotated[[i.subject]]$subject.dir,
         subject.i=i.subject,
         noise.level=noise.level)
  }
  
  return( l.perfs )
}

ElongatePerformance = function(l.perfs) {
  l.perfs.frame = list()
  max.len = 0
  for (i in seq_along(l.perfs)) {
    l.perfs.frame[[i]] = l.perfs[[i]]$perfs.frame
    this.len = length(l.perfs.frame[[i]])
    if (max.len < this.len) {
      max.len = this.len
    }
  }
  
  for (i in seq_along(l.perfs.frame)) {
    this.len = length(l.perfs.frame[[i]])
    if (this.len < max.len) {
      for (k in (this.len+1):max.len) {
        l.perfs.frame[[i]][[k]] = l.perfs.frame[[i]][[this.len]]
      }
    }
    
    l.perfs[[i]]$perfs.frame = l.perfs.frame[[i]]
  }
  
  return( l.perfs )
}

PlotAllUsers = function(l.perfs) {
  
  l.perfs = ElongatePerformance(l.perfs)
  
  colours = c('red', 'green', 'black', 
              'blue', 'orange', 'purple', 
              'deeppink2', 'darkseagreen4', 'goldenrod3',
              'bisque3', 'cyan3', 'slateblue3')
  
  for (i in seq_along(l.perfs)) {
    y = l.perfs[[i]]$perfs.frame
    if (i == 1) {
      plot(y, type='l', ylim=c(0, 1), col=colours[i], lty='dashed')
    } else {
      lines(y, col=colours[i], lty='dashed')
    }
  }  
  
  # TODO
  # plot $perfs.seg
}

PlotAvgUsers = function(l.perfs) {
  l.perfs = ElongatePerformance(l.perfs)
  
  num.users = length(l.perfs)
  
  y.avg = rep(0, length(l.perfs[[1]]$perfs.frame))
  
  for (i in seq_len(num.users)) {
    y.avg = y.avg + (l.perfs[[i]]$perfs.frame / num.users) 
  }
  
  main=''
  #main='Average performance across all users'
  plot(y.avg, ylim=c(0, 1), main=main, type='l')
}

PlotAllUsers.noisy = function(l.perfs, noise.level, graphics.dir) {
  subject.dirs = rep(NA, times=length(l.perfs))
  
  for (i in seq_along(l.perfs)) {
    l.perf = l.perfs[[i]]
    subject.dirs[i] = l.perf$subject.dir
  }
  subject.dirs = sort(unique(subject.dirs))
  
  subject.is = rep(NA, times=length(subject.dirs))
  for (i in seq_along(subject.dirs)) {
    subject.dir = subject.dirs[i]
    subject.i = as.integer(strsplit(x=subject.dir, split='_')[[1]][2])
    subject.is[i] = subject.i
  }
  subject.dirs = subject.dirs[order(subject.is)]
  subject.is = sort(subject.is)
  
  l.ys = list()
  for (i in seq_along(subject.dirs)) {
    l = list()
    l$subject.i = subject.is[i]
    l$subject.dir = subject.dirs[i]
    l$m.frame = NULL
    l$m.seg = NULL
    l.ys[[i]] = l    
  }
  
  for (l.perf in l.perfs) {
    if (l.perf$noise.level == noise.level) {
      y.frame = l.perf$perfs.frame      
      y.seg = l.perf$perfs.seg
      
      subject.dir = l.perf$subject.dir
      
      for (i.l.y in seq_along(l.ys)) {
        if (subject.dir == l.ys[[i.l.y]]$subject.dir) {
          l.ys[[i.l.y]]$m.frame = rbind(l.ys[[i.l.y]]$m.frame, y.frame)
          l.ys[[i.l.y]]$m.seg = rbind(l.ys[[i.l.y]]$m.seg, y.seg)
          break
        }
      }
    }
  }
  
  for (i.l.y in seq_along(l.ys)) {
    l.ys[[i.l.y]]$avg.frame = apply(X=l.ys[[i.l.y]]$m.frame, MARGIN=2, FUN=mean)
    l.ys[[i.l.y]]$avg.seg = apply(X=l.ys[[i.l.y]]$m.seg, MARGIN=2, FUN=mean)
  }
  
  for (l.y in l.ys) {
    #main = paste('Subject ', l.y$subject.i, '; ', noise.level*100, '% noise; frame-based x-val', sep='')
    main=''
    file.name = paste('subject', l.y$subject.i, 'noise', paste(noise.level*100, 'p', sep=''), 'frame-based', sep='-')
    SavePlotBegin(dir=graphics.dir, file.name=file.name)
    plot(l.y$avg.frame, type='l', col='red', main=main, ylim=c(0, 1), 
         ylab='F-Score', xlab='Number of Annotations Given')
    SavePlotEnd()
    
    #main = paste('Subject ', l.y$subject.i, '; ', noise.level*100, '% noise; segment-based x-val', sep='')
    main=''
    file.name = paste('subject', l.y$subject.i, 'noise', paste(noise.level*100, 'p', sep=''), 'seg-based', sep='-')
    SavePlotBegin(dir=graphics.dir, file.name=file.name)
    plot(l.y$avg.seg, type='l', col='red', main=main, ylim=c(0, 1),
         ylab='F-Score', xlab='Number of Annotations Given')
    SavePlotEnd()
  }
}

#ZeroR
PlotAllUsers.noisy.zeror = function(l.perfs, l.perfs.zeror, noise.level, noise.level.zeror, graphics.dir) {
  subject.dirs = rep(NA, times=length(l.perfs))
  subject.dirs.zeror = rep(NA, times=length(l.perfs.zeror))
  
  for (i in seq_along(l.perfs)) {
    l.perf = l.perfs[[i]]
    subject.dirs[i] = l.perf$subject.dir
  }
  subject.dirs = sort(unique(subject.dirs))
  
  for (i in seq_along(l.perfs.zeror)) {
    l.perf = l.perfs.zeror[[i]]
    subject.dirs.zeror[i] = l.perf$subject.dir
  }
  subject.dirs.zeror = sort(unique(subject.dirs.zeror))
  
  subject.is = rep(NA, times=length(subject.dirs))
  for (i in seq_along(subject.dirs)) {
    subject.dir = subject.dirs[i]
    subject.i = as.integer(strsplit(x=subject.dir, split='_')[[1]][2])
    subject.is[i] = subject.i
  }
  subject.dirs = subject.dirs[order(subject.is)]
  subject.is = sort(subject.is)
  
  subject.is.zeror = rep(NA, times=length(subject.dirs.zeror))
  for (i in seq_along(subject.dirs.zeror)) {
    subject.dir = subject.dirs.zeror[i]
    subject.i = as.integer(strsplit(x=subject.dir, split='_')[[1]][2])
    subject.is.zeror[i] = subject.i
  }
  subject.dirs.zeror = subject.dirs[order(subject.is.zeror)]
  subject.is.zeror = sort(subject.is.zeror)
  
  l.ys = list()
  for (i in seq_along(subject.dirs)) {
    l = list()
    l$subject.i = subject.is[i]
    l$subject.dir = subject.dirs[i]
    l$m.frame = NULL
    l$m.seg = NULL
    l.ys[[i]] = l    
  }
  
  l.ys.zeror = list()
  for (i in seq_along(subject.dirs.zeror)) {
    l = list()
    l$subject.i = subject.is.zeror[i]
    l$subject.dir = subject.dirs.zeror[i]
    l$m.frame = NULL
    l$m.seg = NULL
    l.ys.zeror[[i]] = l    
  }
  
  for (l.perf in l.perfs) {
    if (l.perf$noise.level == noise.level) {
      y.frame = l.perf$perfs.frame      
      y.seg = l.perf$perfs.seg
      
      subject.dir = l.perf$subject.dir
      
      for (i.l.y in seq_along(l.ys)) {
        if (subject.dir == l.ys[[i.l.y]]$subject.dir) {
          l.ys[[i.l.y]]$m.frame = rbind(l.ys[[i.l.y]]$m.frame, y.frame)
          l.ys[[i.l.y]]$m.seg = rbind(l.ys[[i.l.y]]$m.seg, y.seg)
          break
        }
      }
    }
  }
  
  for (l.perf in l.perfs.zeror) {
    if (l.perf$noise.level == noise.level.zeror) {
      y.frame = l.perf$perfs.frame      
      y.seg = l.perf$perfs.seg
      
      subject.dir = l.perf$subject.dir
      
      for (i.l.y in seq_along(l.ys.zeror)) {
        if (subject.dir == l.ys.zeror[[i.l.y]]$subject.dir) {
          l.ys.zeror[[i.l.y]]$m.frame = rbind(l.ys.zeror[[i.l.y]]$m.frame, y.frame)
          l.ys.zeror[[i.l.y]]$m.seg = rbind(l.ys.zeror[[i.l.y]]$m.seg, y.seg)
          break
        }
      }
    }
  }
  
  for (i.l.y in seq_along(l.ys)) {
    l.ys[[i.l.y]]$avg.frame = apply(X=l.ys[[i.l.y]]$m.frame, MARGIN=2, FUN=mean)
    l.ys[[i.l.y]]$avg.seg = apply(X=l.ys[[i.l.y]]$m.seg, MARGIN=2, FUN=mean)
  }
  
  for (i.l.y in seq_along(l.ys.zeror)) {
    l.ys.zeror[[i.l.y]]$avg.frame = apply(X=l.ys.zeror[[i.l.y]]$m.frame, MARGIN=2, FUN=mean)
    l.ys.zeror[[i.l.y]]$avg.seg = apply(X=l.ys.zeror[[i.l.y]]$m.seg, MARGIN=2, FUN=mean)
  }
  
  for (i.l.y in seq_along(l.ys)) {
    l.y = l.ys[[i.l.y]]
    l.y.zeror = l.ys.zeror[[i.l.y]]
    
    #main = paste('Subject ', l.y$subject.i, '; ', noise.level*100, '% noise; frame-based x-val', sep='')
    main=''
    file.name = paste('subject', l.y$subject.i, 'noise', paste(noise.level*100, 'p', sep=''), 'frame-based', 
                      'zeror', sep='-')
    SavePlotBegin(dir=graphics.dir, file.name=file.name)
    plot(l.y$avg.frame, type='l', col='red', main=main, ylim=c(0, 1), 
         ylab='F-Score', xlab='Number of Annotations Given')
    #lines(l.y.zeror$avg.frame)
    SavePlotEnd()
    
    #main = paste('Subject ', l.y$subject.i, '; ', noise.level*100, '% noise; segment-based x-val', sep='')
    main=''
    file.name = paste('subject', l.y$subject.i, 'noise', paste(noise.level*100, 'p', sep=''), 'seg-based', 
                      'zeror', sep='-')
    SavePlotBegin(dir=graphics.dir, file.name=file.name)
    plot(l.y$avg.seg, type='l', col='red', main=main, ylim=c(0, 1),
         ylab='F-Score', xlab='Number of Annotations Given')
    SavePlotEnd()
  }
}

PlotAvgUsers.noisy = function(l.perfs, noise.level, graphics.dir) {
  subject.dirs = rep(NA, times=length(l.perfs))
  
  for (i in seq_along(l.perfs)) {
    l.perf = l.perfs[[i]]
    subject.dirs[i] = l.perf$subject.dir
  }
  subject.dirs = sort(unique(subject.dirs))
  
  subject.is = rep(NA, times=length(subject.dirs))
  for (i in seq_along(subject.dirs)) {
    subject.dir = subject.dirs[i]
    subject.i = as.integer(strsplit(x=subject.dir, split='_')[[1]][2])
    subject.is[i] = subject.i
  }
  subject.dirs = subject.dirs[order(subject.is)]
  subject.is = sort(subject.is)
  
  l.ys = list()
  for (i in seq_along(subject.dirs)) {
    l = list()
    l$subject.i = subject.is[i]
    l$subject.dir = subject.dirs[i]
    l$m.frame = NULL
    l$m.seg = NULL
    l.ys[[i]] = l    
  }
  
  m.perfs.frame.interp = NULL
  m.perfs.seg.interp = NULL
  
  n.interp.points = 1000
  x.interp = seq(from=0, to=1, length.out=n.interp.points)
  
  m.avg.frame = NULL
  m.avg.seg = NULL
  
  for (l.perf in l.perfs) {
    if (l.perf$noise.level == noise.level) {
      y.frame = l.perf$perfs.frame      
      x = seq(from=0, to=1, length.out=length(y.frame))
      perf.fun = approxfun(x=x, y=y.frame, method='linear')      
      y.frame.interp = perf.fun(x.interp)
      
      y.seg = l.perf$perfs.seg
      x = seq(from=0, to=1, length.out=length(y.seg))
      perf.fun = approxfun(x=x, y=y.seg, method='linear')      
      y.seg.interp = perf.fun(x.interp)
      
      subject.dir = l.perf$subject.dir
      
      for (i.l.y in seq_along(l.ys)) {
        if (subject.dir == l.ys[[i.l.y]]$subject.dir) {
          m.avg.frame = rbind(m.avg.frame, y.frame.interp)
          m.avg.seg = rbind(m.avg.seg, y.seg.interp)
          break
        }
      }
    }
  }
  
  y.avg.frame = apply(X=m.avg.frame, MARGIN=2, FUN=mean)
  y.avg.seg = apply(X=m.avg.seg, MARGIN=2, FUN=mean)
  
  x = seq(from=0, to=100, length.out=length(y.avg.frame))
  
  #main = paste('All Subjects; frame-based x-val; ', noise.level*100, '% noise level', sep='')
  main=''
  
  file.name = paste('avg', 'noise', paste(noise.level*100, 'p', sep=''), 'frame-based', sep='-')
  SavePlotBegin(dir=graphics.dir, file.name=file.name)
  plot(x=x, y=y.avg.frame, type='l', ylim=c(0, 1), main=main, col='red',
       xlab='Percentage of Annotations Given', ylab='F-Score')
  SavePlotEnd()
  
  #main = paste('All Subjects; segment-based x-val; ', noise.level*100, '% noise level', sep='')
  main=''
  file.name = paste('avg', 'noise', paste(noise.level*100, 'p', sep=''), 'seg-based', sep='-')
  SavePlotBegin(dir=graphics.dir, file.name=file.name)
  plot(x=x, y=y.avg.seg, type='l', ylim=c(0, 1), main=main, col='red',
       xlab='Percentage of Annotations Given', ylab='F-Score')
  SavePlotEnd()
}

PlotAllUsers.noisy.versus.noise.level = function(l.perfs, noise.levels, graphics.dir) {
  subject.dirs = rep(NA, times=length(l.perfs))
  
  for (i in seq_along(l.perfs)) {
    l.perf = l.perfs[[i]]
    subject.dirs[i] = l.perf$subject.dir
  }
  subject.dirs = sort(unique(subject.dirs))
  
  subject.is = rep(NA, times=length(subject.dirs))
  for (i in seq_along(subject.dirs)) {
    subject.dir = subject.dirs[i]
    subject.i = as.integer(strsplit(x=subject.dir, split='_')[[1]][2])
    subject.is[i] = subject.i
  }
  subject.dirs = subject.dirs[order(subject.is)]
  subject.is = sort(subject.is)
  
  l.noise = list()
  for (noise.level in noise.levels) {
    
    l.ys = list()
    for (i in seq_along(subject.dirs)) {
      l = list()
      l$subject.i = subject.is[i]
      l$subject.dir = subject.dirs[i]
      l$m.frame = NULL
      l$m.seg = NULL
      l.ys[[i]] = l    
    }
    
    for (l.perf in l.perfs) {
      if (l.perf$noise.level == noise.level) {
        y.frame = l.perf$perfs.frame      
        y.seg = l.perf$perfs.seg
        
        subject.dir = l.perf$subject.dir
        subject.i = subject.is[which(subject.dirs == subject.dir)]
        
        for (i.l.y in seq_along(l.ys)) {
          if (subject.dir == l.ys[[i.l.y]]$subject.dir) {
            l.ys[[i.l.y]]$m.frame = rbind(l.ys[[i.l.y]]$m.frame, y.frame)
            l.ys[[i.l.y]]$m.seg = rbind(l.ys[[i.l.y]]$m.seg, y.seg)
            l.ys[[i.l.y]]$subject.dir = subject.dir
            l.ys[[i.l.y]]$subject.i = subject.i
            break
          }
        }
      }
    }
    
    for (i.l.y in seq_along(l.ys)) {
      l.ys[[i.l.y]]$avg.frame = apply(X=l.ys[[i.l.y]]$m.frame, MARGIN=2, FUN=mean)
      l.ys[[i.l.y]]$avg.seg = apply(X=l.ys[[i.l.y]]$m.seg, MARGIN=2, FUN=mean)
    }
    
    l = list()
    l$noise.level = noise.level
    l$l.ys = l.ys
    l.noise[[1 + length(l.noise)]] = l
  }
  
  for (subject.i in subject.is) {
    m.perfs.frame = NULL
    m.perfs.seg = NULL
    
    for (noise.level in noise.levels) {
      for (l.n in l.noise) {
        if (l.n$noise.level == noise.level) {
          for (l.y in l.n$l.ys) {
            if (l.y$subject.i == subject.i) {
              m.perfs.frame = rbind(m.perfs.frame, l.y$avg.frame)
              m.perfs.seg = rbind(m.perfs.seg, l.y$avg.seg)
              #cat('&')
              break
            }
          } 
        }
      }
    }
    
    main.perf = NULL
    
    for (i.row in seq_len(nrow(m.perfs.frame))) {
      if (i.row == 1) {
        #main = paste('Subject ', subject.i, '; frame-based x-val', sep='')
        
        main.perf = m.perfs.frame[i.row, ]
        
        main=''
        file.name = paste('subject', subject.i, 'noise', 'versus', 'frame-based', sep='-')
        SavePlotBegin(dir=graphics.dir, file.name=file.name)
        plot(m.perfs.frame[i.row, ], type='l', ylim=c(0, 1), main=main, col='red', 
             xlab='Number of Annotations Given', ylab='F-Score', lwd=2)
      } else {
        lines(m.perfs.frame[i.row, ], lty='dotted', col='red')
      } 
    }    
    legend(x='bottomright', legend=c('no noise', '10% noise'), col=c('red', 'red'), 
           lty=c('solid', 'dotted'), lwd=c(2, 1) )
    SavePlotEnd()
    
    for (i.row in seq_len(nrow(m.perfs.seg))) {
      #cat('i.row =', i.row, '\n')
      if (i.row == 1) {
        #main = paste('Subject', subject.i, '; segment-based x-val', sep='')
        main=''
        file.name = paste('subject', subject.i, 'noise', 'versus', 'seg-based', sep='-')
        SavePlotBegin(dir=graphics.dir, file.name=file.name)
        plot(m.perfs.seg[i.row, ], type='l', ylim=c(0, 1), main=main, col='red', 
             xlab='Number of Annotations Given', ylab='F-Score', lwd=2)
      } else {
        lines(m.perfs.seg[i.row, ], lty='dotted', col='red')
      } 
    }    
    legend(x='bottomright', legend=c('no noise', '10% noise'), col=c('red', 'red'), 
           lty=c('solid', 'dotted'), lwd=c(2, 1) )
    SavePlotEnd()
  }
}

# ZeroR
PlotAllUsers.noisy.versus.noise.level.zeror = function(l.perfs, noise.levels, 
                                                       l.perfs.zeror, noise.level.zeror, 
                                                       graphics.dir) {
  subject.dirs = rep(NA, times=length(l.perfs))
  subject.dirs.zeror = rep(NA, times=length(l.perfs.zeror))
  
  for (i in seq_along(l.perfs)) {
    l.perf = l.perfs[[i]]
    subject.dirs[i] = l.perf$subject.dir
  }
  subject.dirs = sort(unique(subject.dirs))
  
  for (i in seq_along(l.perfs.zeror)) {
    l.perf = l.perfs.zeror[[i]]
    subject.dirs.zeror[i] = l.perf$subject.dir
  }
  subject.dirs.zeror = sort(unique(subject.dirs.zeror))
  
  subject.is = rep(NA, times=length(subject.dirs))
  for (i in seq_along(subject.dirs)) {
    subject.dir = subject.dirs[i]
    subject.i = as.integer(strsplit(x=subject.dir, split='_')[[1]][2])
    subject.is[i] = subject.i
  }
  subject.dirs = subject.dirs[order(subject.is)]
  subject.is = sort(subject.is)
  
  subject.is.zeror = rep(NA, times=length(subject.dirs.zeror))
  for (i in seq_along(subject.dirs.zeror)) {
    subject.dir = subject.dirs.zeror[i]
    subject.i = as.integer(strsplit(x=subject.dir, split='_')[[1]][2])
    subject.is.zeror[i] = subject.i
  }
  subject.dirs.zeror = subject.dirs[order(subject.is.zeror)]
  subject.is.zeror = sort(subject.is.zeror)
  
  l.ys.zeror = list()
  for (i in seq_along(subject.dirs.zeror)) {
    l = list()
    l$subject.i = subject.is.zeror[i]
    l$subject.dir = subject.dirs.zeror[i]
    l$m.frame = NULL
    l$m.seg = NULL
    l.ys.zeror[[i]] = l    
  }
  
  for (l.perf in l.perfs.zeror) {
    if (l.perf$noise.level == noise.level.zeror) {
      y.frame = l.perf$perfs.frame      
      y.seg = l.perf$perfs.seg
      
      subject.dir = l.perf$subject.dir
      
      for (i.l.y in seq_along(l.ys.zeror)) {
        if (subject.dir == l.ys.zeror[[i.l.y]]$subject.dir) {
          l.ys.zeror[[i.l.y]]$m.frame = rbind(l.ys.zeror[[i.l.y]]$m.frame, y.frame)
          l.ys.zeror[[i.l.y]]$m.seg = rbind(l.ys.zeror[[i.l.y]]$m.seg, y.seg)
          break
        }
      }
    }
  }
  
  for (i.l.y in seq_along(l.ys.zeror)) {
    l.ys.zeror[[i.l.y]]$avg.frame = apply(X=l.ys.zeror[[i.l.y]]$m.frame, MARGIN=2, FUN=mean)
    l.ys.zeror[[i.l.y]]$avg.seg = apply(X=l.ys.zeror[[i.l.y]]$m.seg, MARGIN=2, FUN=mean)
  }
  
  l.noise = list()
  for (noise.level in noise.levels) {
    
    l.ys = list()
    for (i in seq_along(subject.dirs)) {
      l = list()
      l$subject.i = subject.is[i]
      l$subject.dir = subject.dirs[i]
      l$m.frame = NULL
      l$m.seg = NULL
      l.ys[[i]] = l    
    }
    
    for (l.perf in l.perfs) {
      if (l.perf$noise.level == noise.level) {
        y.frame = l.perf$perfs.frame      
        y.seg = l.perf$perfs.seg
        
        subject.dir = l.perf$subject.dir
        subject.i = subject.is[which(subject.dirs == subject.dir)]
        
        for (i.l.y in seq_along(l.ys)) {
          if (subject.dir == l.ys[[i.l.y]]$subject.dir) {
            l.ys[[i.l.y]]$m.frame = rbind(l.ys[[i.l.y]]$m.frame, y.frame)
            l.ys[[i.l.y]]$m.seg = rbind(l.ys[[i.l.y]]$m.seg, y.seg)
            l.ys[[i.l.y]]$subject.dir = subject.dir
            l.ys[[i.l.y]]$subject.i = subject.i
            break
          }
        }
      }
    }
    
    for (i.l.y in seq_along(l.ys)) {
      l.ys[[i.l.y]]$avg.frame = apply(X=l.ys[[i.l.y]]$m.frame, MARGIN=2, FUN=mean)
      l.ys[[i.l.y]]$avg.seg = apply(X=l.ys[[i.l.y]]$m.seg, MARGIN=2, FUN=mean)
    }
    
    l = list()
    l$noise.level = noise.level
    l$l.ys = l.ys
    l.noise[[1 + length(l.noise)]] = l
  }
  
  for (subject.i in subject.is) {
    m.perfs.frame = NULL
    m.perfs.seg = NULL
    
    for (noise.level in noise.levels) {
      for (l.n in l.noise) {
        if (l.n$noise.level == noise.level) {
          #for (l.y in l.n$l.ys) {
          for (i.l.y in seq_along(l.n$l.ys)) {
            l.y = l.n$l.ys[[i.l.y]]
            
            #l.y.zeror = l.ys.zeror[[i.l.y]]
            
            if (l.y$subject.i == subject.i) {
              m.perfs.frame = rbind(m.perfs.frame, l.y$avg.frame)
              m.perfs.seg = rbind(m.perfs.seg, l.y$avg.seg)
              #cat('&')
              l.y.zeror = l.ys.zeror[[i.l.y]]
              
              if(l.y$subject.i != l.y.zeror$subject.i) {
                stop('BAAAAAD !!!')
              }
              
              break
            }
          } 
        }
      }
    }
    
    main.perf = NULL
    zeror.frame = NULL
    
    for (i.row in seq_len(nrow(m.perfs.seg))) {
      #print(m.perfs.frame[i.row, ])
      #cat('i.row =', i.row, '\n')
      if (i.row == 1) {
        #main = paste('Subject', subject.i, '; segment-based x-val', sep='')
        
        main=''
        file.name = paste('subject', subject.i, 'noise', 'versus', 'seg-based', 'zeror', sep='-')
        SavePlotBegin(dir=graphics.dir, file.name=file.name)
        plot(m.perfs.seg[i.row, ], type='l', ylim=c(0, 1), main=main, col='red', 
             xlab='Number of Annotations Given', ylab='F-Score', lwd=2)
        lines(l.y.zeror$avg.seg, col='black')
      } else {
        lines(m.perfs.seg[i.row, ], lty='dotted', col='red')
      } 
    }    
    legend(x='bottomright', legend=c('no noise', 'strawman', '10% noise'), col=c('red', 'black', 'red'), 
           lty=c('solid', 'solid', 'dotted'), lwd=c(2, 1, 2) )
    SavePlotEnd()
    
    for (i.row in seq_len(nrow(m.perfs.frame))) {
      #print(m.perfs.frame[i.row, ])
      if (i.row == 1) {
        
        main.perf = m.perfs.frame[i.row, ]
        zeror.perf = l.y.zeror$avg.frame
        
        #main = paste('Subject ', subject.i, '; frame-based x-val', sep='')
        main=''
        file.name = paste('subject', subject.i, 'noise', 'versus', 'frame-based', 'zeror', sep='-')
        SavePlotBegin(dir=graphics.dir, file.name=file.name)
        plot(m.perfs.frame[i.row, ], type='l', ylim=c(0, 1), main=main, col='red', 
             xlab='Number of Annotations Given', ylab='F-Score', lwd=2)
        lines(l.y.zeror$avg.frame, col='black')
      } else {
        lines(m.perfs.frame[i.row, ], lty='dotted', col='red', lwd=2)
      } 
    }    
    legend(x='bottomright', legend=c('no noise', 'strawman', '10% noise'), col=c('red', 'black', 'red'), 
           lty=c('solid', 'solid', 'dotted'), lwd=c(2, 1, 2) )
    SavePlotEnd()
    
    cat('Performance difference (annot - zeror) =', max(main.perf-zeror.perf))
  }
}

PlotAvgUsers.noisy.versus.noise.level = function(l.perfs, noise.levels, graphics.dir) {
  subject.dirs = rep(NA, times=length(l.perfs))
  
  n.interp.points = 1000
  x.interp = seq(from=0, to=1, length.out=n.interp.points)
  
  for (i in seq_along(l.perfs)) {
    l.perf = l.perfs[[i]]
    subject.dirs[i] = l.perf$subject.dir
  }
  subject.dirs = sort(unique(subject.dirs))
  
  subject.is = rep(NA, times=length(subject.dirs))
  for (i in seq_along(subject.dirs)) {
    subject.dir = subject.dirs[i]
    subject.i = as.integer(strsplit(x=subject.dir, split='_')[[1]][2])
    subject.is[i] = subject.i
  }
  subject.dirs = subject.dirs[order(subject.is)]
  subject.is = sort(subject.is)
  
  l.noise = list()
  for (noise.level in noise.levels) {
    
    l.ys = list()
    for (i in seq_along(subject.dirs)) {
      l = list()
      l$subject.i = subject.is[i]
      l$subject.dir = subject.dirs[i]
      l$m.frame = NULL
      l$m.seg = NULL
      l.ys[[i]] = l    
    }
    
    for (l.perf in l.perfs) {
      if (l.perf$noise.level == noise.level) {
        y.frame = l.perf$perfs.frame
        x = seq(from=0, to=1, length.out=length(y.frame))
        perf.fun = approxfun(x=x, y=y.frame, method='linear')      
        y.frame.interp = perf.fun(x.interp)
        
        y.seg = l.perf$perfs.seg
        x = seq(from=0, to=1, length.out=length(y.seg))
        perf.fun = approxfun(x=x, y=y.seg, method='linear')      
        y.seg.interp = perf.fun(x.interp)
        
        subject.dir = l.perf$subject.dir
        subject.i = subject.is[which(subject.dirs == subject.dir)]
        
        for (i.l.y in seq_along(l.ys)) {
          if (subject.dir == l.ys[[i.l.y]]$subject.dir) {
            l.ys[[i.l.y]]$m.frame = rbind(l.ys[[i.l.y]]$m.frame, y.frame.interp)
            l.ys[[i.l.y]]$m.seg = rbind(l.ys[[i.l.y]]$m.seg, y.seg.interp)
            l.ys[[i.l.y]]$subject.dir = subject.dir
            l.ys[[i.l.y]]$subject.i = subject.i
            break
          }
        }
      }
    }
    
    for (i.l.y in seq_along(l.ys)) {
      l.ys[[i.l.y]]$avg.frame = apply(X=l.ys[[i.l.y]]$m.frame, MARGIN=2, FUN=mean)
      l.ys[[i.l.y]]$avg.seg = apply(X=l.ys[[i.l.y]]$m.seg, MARGIN=2, FUN=mean)
    }
    
    l = list()
    l$noise.level = noise.level
    l$l.ys = l.ys
    l.noise[[1 + length(l.noise)]] = l
  }
  
  l.perfs.frame = list()
  l.perfs.seg = list()
  
  for (noise.level in noise.levels) {
    for (subject.i in subject.is) {
      m.perfs.frame = NULL
      m.perfs.seg = NULL
      
      for (l.n in l.noise) {
        if (l.n$noise.level == noise.level) {
          for (l.y in l.n$l.ys) {
            if (l.y$subject.i == subject.i) {
              m.perfs.frame = rbind(m.perfs.frame, l.y$avg.frame)
              m.perfs.seg = rbind(l.y$avg.seg)
              #cat('&')
              break
            }
          } 
        }
      }
    }
    
    l.perfs.frame[[1 + length(l.perfs.frame)]] = apply(X=m.perfs.frame, MARGIN=2, FUN=mean)
    l.perfs.seg[[1 + length(l.perfs.seg)]] = apply(X=m.perfs.seg, MARGIN=2, FUN=mean)
    
  }  
  
  for (i in seq_along(l.perfs.frame)) {
    if (i == 1) {
      #main = paste('All Subjects; frame-based x-val')
      main=''
      x = seq(from=0, to=100, length.out=length(l.perfs.frame[[1]]))
      file.name = paste('avg', 'noise', 'versus', 'frame-based', 'zeror', sep='-')
      SavePlotBegin(dir=graphics.dir, file.name=file.name)
      plot(y=l.perfs.frame[[i]], x=x, main=main, col='red', lwd=2, type='l', ylim=c(0, 1), 
           ylab='F-Score', xlab='Percentage of Annotations Given')
      #lines(avg.perf.frame.zeror, col='black')
    } else {
      lines(y=l.perfs.frame[[i]], x=x, col='red', lwd=2, lty='dotted')
    }
  }
  legend(x='bottomright', legend=c('no noise', '10% noise'), col=c('red', 'red'), 
         lty=c('solid', 'dotted'), lwd=c(2, 2) )
  SavePlotEnd()
  
  for (i in seq_along(l.perfs.seg)) {
    if (i == 1) {
      #main = paste('All Subjects; segment-based x-val')
      main=''
      x = seq(from=0, to=100, length.out=length(l.perfs.seg[[1]]))
      file.name = paste('avg', 'noise', 'versus', 'seg-based', 'zeror', sep='-')
      SavePlotBegin(dir=graphics.dir, file.name=file.name)
      plot(y=l.perfs.seg[[i]], x=x, main=main, col='red', lwd=2, type='l', ylim=c(0, 1), 
           ylab='F-Score', xlab='Percentage of Annotations Given')
      #lines(avg.perf.seg.zeror, col='black')
    } else {
      lines(y=l.perfs.seg[[i]], x=x, col='red', lwd=2, lty='dotted')
    }
  }  
  legend(x='bottomright', legend=c('no noise', '10% noise'), col=c('red', 'red'), 
         lty=c('solid', 'dotted'), lwd=c(2, 2) )
  SavePlotEnd()
}

# Zeror 
PlotAvgUsers.noisy.versus.noise.level.zeror = function(l.perfs, noise.levels, 
                                                       l.perfs.zeror, noise.level.zeror,
                                                       graphics.dir) {
  subject.dirs = rep(NA, times=length(l.perfs))
  subject.dirs.zeror = rep(NA, times=length(l.perfs.zeror))
  
  n.interp.points = 1000
  x.interp = seq(from=0, to=1, length.out=n.interp.points)
  
  for (i in seq_along(l.perfs)) {
    l.perf = l.perfs[[i]]
    subject.dirs[i] = l.perf$subject.dir
  }
  subject.dirs = sort(unique(subject.dirs))
  
  for (i in seq_along(l.perfs.zeror)) {
    l.perf = l.perfs.zeror[[i]]
    subject.dirs.zeror[i] = l.perf$subject.dir
  }
  subject.dirs.zeror = sort(unique(subject.dirs.zeror))
  
  subject.is = rep(NA, times=length(subject.dirs))
  for (i in seq_along(subject.dirs)) {
    subject.dir = subject.dirs[i]
    subject.i = as.integer(strsplit(x=subject.dir, split='_')[[1]][2])
    subject.is[i] = subject.i
  }
  subject.dirs = subject.dirs[order(subject.is)]
  subject.is = sort(subject.is)
  
  subject.is.zeror = rep(NA, times=length(subject.dirs.zeror))
  for (i in seq_along(subject.dirs.zeror)) {
    subject.dir = subject.dirs.zeror[i]
    subject.i = as.integer(strsplit(x=subject.dir, split='_')[[1]][2])
    subject.is.zeror[i] = subject.i
  }
  subject.dirs.zeror = subject.dirs[order(subject.is.zeror)]
  subject.is.zeror = sort(subject.is.zeror)
  
  l.ys.zeror = list()
  for (i in seq_along(subject.dirs.zeror)) {
    l = list()
    l$subject.i = subject.is.zeror[i]
    l$subject.dir = subject.dirs.zeror[i]
    l$m.frame = NULL
    l$m.seg = NULL
    l.ys.zeror[[i]] = l    
  }
  
  
  #   for (l.perf in l.perfs.zeror) {
  #     if (l.perf$noise.level == noise.level.zeror) {
  #       y.frame = l.perf$perfs.frame      
  #       y.seg = l.perf$perfs.seg
  #       
  #       subject.dir = l.perf$subject.dir
  #       
  #       for (i.l.y in seq_along(l.ys.zeror)) {
  #         if (subject.dir == l.ys.zeror[[i.l.y]]$subject.dir) {
  #           l.ys.zeror[[i.l.y]]$m.frame = rbind(l.ys.zeror[[i.l.y]]$m.frame, y.frame)
  #           l.ys.zeror[[i.l.y]]$m.seg = rbind(l.ys.zeror[[i.l.y]]$m.seg, y.seg)
  #           break
  #         }
  #       }
  #     }
  #   }
  
  for (l.perf in l.perfs.zeror) {
    y.frame = l.perf$perfs.frame
    x = seq(from=0, to=1, length.out=length(y.frame))
    perf.fun = approxfun(x=x, y=y.frame, method='linear')      
    y.frame.interp = perf.fun(x.interp)
    
    y.seg = l.perf$perfs.seg
    x = seq(from=0, to=1, length.out=length(y.seg))
    perf.fun = approxfun(x=x, y=y.seg, method='linear')      
    y.seg.interp = perf.fun(x.interp)
    
    subject.dir = l.perf$subject.dir
    subject.i = subject.is[which(subject.dirs == subject.dir)]
    
    for (i.l.y in seq_along(l.ys.zeror)) {
      if (subject.dir == l.ys.zeror[[i.l.y]]$subject.dir) {
        l.ys.zeror[[i.l.y]]$m.frame = rbind(l.ys.zeror[[i.l.y]]$m.frame, y.frame.interp)
        l.ys.zeror[[i.l.y]]$m.seg = rbind(l.ys.zeror[[i.l.y]]$m.seg, y.seg.interp)
        l.ys.zeror[[i.l.y]]$subject.dir = subject.dir
        l.ys.zeror[[i.l.y]]$subject.i = subject.i
        break
      }
    }
  }
  
  for (i.l.y in seq_along(l.ys.zeror)) {
    l.ys.zeror[[i.l.y]]$avg.frame = apply(X=l.ys.zeror[[i.l.y]]$m.frame, MARGIN=2, FUN=mean)
    l.ys.zeror[[i.l.y]]$avg.seg = apply(X=l.ys.zeror[[i.l.y]]$m.seg, MARGIN=2, FUN=mean)
  }
  
  l.noise = list()
  for (noise.level in noise.levels) {
    
    l.ys = list()
    for (i in seq_along(subject.dirs)) {
      l = list()
      l$subject.i = subject.is[i]
      l$subject.dir = subject.dirs[i]
      l$m.frame = NULL
      l$m.seg = NULL
      l.ys[[i]] = l    
    }
    
    for (l.perf in l.perfs) {
      if (l.perf$noise.level == noise.level) {
        y.frame = l.perf$perfs.frame
        x = seq(from=0, to=1, length.out=length(y.frame))
        perf.fun = approxfun(x=x, y=y.frame, method='linear')      
        y.frame.interp = perf.fun(x.interp)
        
        y.seg = l.perf$perfs.seg
        x = seq(from=0, to=1, length.out=length(y.seg))
        perf.fun = approxfun(x=x, y=y.seg, method='linear')      
        y.seg.interp = perf.fun(x.interp)
        
        subject.dir = l.perf$subject.dir
        subject.i = subject.is[which(subject.dirs == subject.dir)]
        
        for (i.l.y in seq_along(l.ys)) {
          if (subject.dir == l.ys[[i.l.y]]$subject.dir) {
            l.ys[[i.l.y]]$m.frame = rbind(l.ys[[i.l.y]]$m.frame, y.frame.interp)
            l.ys[[i.l.y]]$m.seg = rbind(l.ys[[i.l.y]]$m.seg, y.seg.interp)
            l.ys[[i.l.y]]$subject.dir = subject.dir
            l.ys[[i.l.y]]$subject.i = subject.i
            break
          }
        }
      }
    }
    
    for (i.l.y in seq_along(l.ys)) {
      l.ys[[i.l.y]]$avg.frame = apply(X=l.ys[[i.l.y]]$m.frame, MARGIN=2, FUN=mean)
      l.ys[[i.l.y]]$avg.seg = apply(X=l.ys[[i.l.y]]$m.seg, MARGIN=2, FUN=mean)
    }
    
    l = list()
    l$noise.level = noise.level
    l$l.ys = l.ys
    l.noise[[1 + length(l.noise)]] = l
  }
  
  l.perfs.frame = list()
  l.perfs.seg = list()
  
  for (noise.level in noise.levels) {
    for (subject.i in subject.is) {
      m.perfs.frame = NULL
      m.perfs.seg = NULL
      
      for (l.n in l.noise) {
        if (l.n$noise.level == noise.level) {
          for (l.y in l.n$l.ys) {
            if (l.y$subject.i == subject.i) {
              m.perfs.frame = rbind(m.perfs.frame, l.y$avg.frame)
              m.perfs.seg = rbind(l.y$avg.seg)
              #cat('&')
              break
            }
          } 
        }
      }
    }
    
    l.perfs.frame[[1 + length(l.perfs.frame)]] = apply(X=m.perfs.frame, MARGIN=2, FUN=mean)
    l.perfs.seg[[1 + length(l.perfs.seg)]] = apply(X=m.perfs.seg, MARGIN=2, FUN=mean)
    
  }  
  
  m.perfs.frames.zeror = NULL
  m.perfs.segs.zeror = NULL
  for (l.y.zeror in l.ys.zeror) {
    m.perfs.frames.zeror = rbind(m.perfs.frames.zeror, l.y.zeror$avg.frame)
    m.perfs.segs.zeror = rbind(m.perfs.segs.zeror, l.y.zeror$avg.seg)
  }
  
  avg.perf.frame.zeror = apply(X=m.perfs.frames.zeror, MARGIN=2, FUN=mean)
  avg.perf.seg.zeror = apply(X=m.perfs.segs.zeror, MARGIN=2, FUN=mean)
  
  cat('\n\n')
  cat('max improvement (frame):', max(l.perfs.frame[[1]] - avg.perf.frame.zeror, na.rm=T))
  cat('\n\n')
  
  main.perf = NULL
  zeror.perf = NULL
  
  for (i in seq_along(l.perfs.frame)) {
    if (i == 1) {
      
      main.perf = l.perfs.frame[[i]]
      zeror.perf = avg.perf.frame.zeror
      
      #main = paste('All Subjects; frame-based x-val')
      main=''
      x = seq(from=0, to=100, length.out=length(l.perfs.frame[[1]]))
      file.name = paste('avg', 'noise', 'versus', 'frame-based', 'zeror', sep='-')
      SavePlotBegin(dir=graphics.dir, file.name=file.name)
      plot(y=l.perfs.frame[[i]], x=x, main=main, col='red', lwd=2, type='l', ylim=c(0, 1), 
           ylab='F-Score', xlab='Percentage of Annotations Given')
      lines(y=avg.perf.frame.zeror, x=x, col='black')
      #print(avg.perf.frame.zeror)
      #print(l.perfs.frame[[i]])
    } else {
      lines(y=l.perfs.frame[[i]], x=x, col='red', lwd=2, lty='dotted')
    }
  }
  #   x.legend = 'bottomright'
  x.legend = 'topleft'
  legend(x=x.legend, legend=c('no noise', 'strawman', '10% noise'), col=c('red', 'black', 'red'), 
         lty=c('solid', 'solid', 'dotted'), lwd=c(2, 1, 2) )
  SavePlotEnd()
  #stop('Enough!!')
  
  for (i in seq_along(l.perfs.seg)) {
    if (i == 1) {
      #main = paste('All Subjects; segment-based x-val')
      main=''
      x = seq(from=0, to=100, length.out=length(l.perfs.seg[[1]]))
      file.name = paste('avg', 'noise', 'versus', 'seg-based', 'zeror', sep='-')
      SavePlotBegin(dir=graphics.dir, file.name=file.name)
      plot(y=l.perfs.seg[[i]], x=x, main=main, col='red', lwd=2, type='l', ylim=c(0, 1), 
           ylab='F-Score', xlab='Percentage of Annotations Given')
      lines(y=avg.perf.seg.zeror, x=x, col='black')
    } else {
      lines(y=l.perfs.seg[[i]], x=x, col='red', lwd=2, lty='dotted')
    }
  }  
  legend(x=x.legend, legend=c('no noise', 'strawman', '10% noise'), col=c('red', 'black', 'red'), 
         lty=c('solid', 'solid', 'dotted'), lwd=c(2, 1, 2) )
  SavePlotEnd()
  
  #print(main.perf)
  #print(zeror.perf)
  
  cat('Performance diff (annot - zeror) =', max(main.perf - zeror.perf, na.rm=T), '\n')
}

PlotSingle = function(l.perf) {
  #main = paste(l.perf$subject.dir, 'noise level', l.perf$noise.level, 'frame')
  main=''
  plot(l.perf$perfs.frame, type='l', main=main, ylim=c(0, 1))
  
  #main = paste(l.perf$subject.dir, 'noise level', l.perf$noise.level, 'seg')
  main=''
  plot(l.perf$perfs.seg, type='l', main=main, ylim=c(0, 1))
}

PlotHistAllUsers = function(datasets.segments.annotated, graphics.dir) {
  
  class.labels = c()
  for (dataset.segments in datasets.segments.annotated) {
    for (seg in dataset.segments$l.segments) {
      activity = as.character(seg$activity[1])
      class.labels = c(class.labels, activity)
    }
  }
  class.labels = sort(unique(class.labels))
  
  m.hists = NULL
  for (dataset.segments in datasets.segments.annotated) {
    table.activities = rep(0, length(class.labels))
    names(table.activities) = class.labels
    
    for (seg in dataset.segments$l.segments) {
      activity = as.character(seg$activity)
      table.activities[activity] = 1 + table.activities[activity] 
    }
    
    m.hists = rbind(m.hists, table.activites)
  }
  
  str.activities = c(
    'walking', # 0
    'sitting', # 1
    'standing', # 2
    'sitting knee raises', # 3
    'calf raises', # 4
    'squats', # 5
    'torso side to side', # 6
    'torso forward backward', # 7
    'torso twists' # 8
  )
  
}

PlotHistAvgUsers = function(datasets.segments.annotated, ylim, ylim.norm, graphics.dir) {
  class.labels = c()
  for (dataset.segments in datasets.segments.annotated) {
    for (seg in dataset.segments$l.segments) {
      activity = as.character(seg$activity[1])
      class.labels = c(class.labels, activity)
    }
  }
  class.labels = sort(unique(class.labels))
  
  m.hists = NULL
  for (dataset.segments in datasets.segments.annotated) {
    table.activities = rep(0, length(class.labels))
    names(table.activities) = class.labels
    
    for (seg in dataset.segments$l.segments) {
      activity = as.character(seg$activity)
      table.activities[activity] = 1 + table.activities[activity] 
    }
    
    m.hists = rbind(m.hists, table.activities)
  }
  
  str.activities = c(
    'walking', # 0
    'sitting', # 1
    'standing', # 2
    'sitting knee\nraises', # 3
    'calf raises', # 4
    'squats', # 5
    'torso side\nto side', # 6
    'torso forward\nbackward', # 7
    'torso twists' # 8
  )
  
  avg.hist = apply(X=m.hists, MARGIN=2, FUN=mean)
  for (i in seq_along(avg.hist)) {
    names(avg.hist)[i] = str.activities[as.integer(names(avg.hist)[i]) + 1]
  }
  
  file.name = paste('barplot', 'avg', sep='-')
  SavePlotBegin(dir=graphics.dir, file.name=file.name)
  bplot = barplot(avg.hist, ylim=ylim, axisnames=F)
  text(bplot, par("usr")[3], labels=names(avg.hist), srt=45, adj=c(1, 1, 1, 1), xpd=T, cex=.9)
  SavePlotEnd()
  
  file.name = paste('barplot', 'avg', 'norm', sep='-')
  SavePlotBegin(dir=graphics.dir, file.name=file.name)
  bplot=barplot(avg.hist / (sum(avg.hist)), ylim=ylim.norm, axisnames=F)
  text(bplot, par("usr")[3], labels=names(avg.hist), srt=45, adj=c(1, 1, 1, 1), xpd=T, cex=.9)
  SavePlotEnd()
}
