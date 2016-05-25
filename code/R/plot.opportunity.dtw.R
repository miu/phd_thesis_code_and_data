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

source('R/file.utils.R')
source('R/ml.utils.R')


GetLC = function(cm.data, x.max, PerfFunction) {
  cms = cm.data$cms
  q.i = cm.data$q.i
  q.i.ext = c(q.i, (x.max+1))
  q.diff = q.i.ext[2: length(q.i.ext)] - q.i.ext[1: (length(q.i.ext)-1)]
  y.sparse = sapply(cms, PerfFunction)
  y.dense = c()
  for (i in seq_along(q.diff)) {
    y.dense = c(y.dense, rep(y.sparse[i], times=q.diff[i]))
  }
  
  q.i.start = q.i[1]
  zero.pad = rep(0, times=(q.i.start-1))
  
  return( c(zero.pad, y.dense) )
}

GetAverageLC = function(cm.data, x.max, PerfFunction) {
  ys = lapply(X=cm.data, FUN=GetLC, x.max=x.max, 
              PerfFunction=PerfFunction)
  y = rep(0, times=x.max)
  ys.len = length(ys)
  
  for (i in seq_len(x.max)) {
    for (k in seq_along(ys)) {
      y[i] = y[i] + (ys[[k]][i] / ys.len)
    }
  }
  return( y )
}

GetLC.effort = function(cm.data, PerfFunction) {
  sapply(cm.data$cms, PerfFunction)
}

GetAverageLC.effort = function(cm.data, PerfFunction) {
  ys = lapply(cm.data, GetLC.effort, PerfFunction=PerfFunction)
  n.curve = length(ys[[1]])
  y.curve = rep(0, n.curve)
  n.reps = length(ys)
  for (y in ys) {
    y.curve = y.curve + (y / n.reps)
  }
  return( y.curve )
}

root.dir = 'output/opportunity/dtw.2'
graphics.dir = 'graphics/opportunity/dtw.2'

gamma = 2
preproc.dir = 'raw_7'

i.subs = 1:4
graphics.file.name = paste('gamma', gamma, preproc.dir, sep='_')
SavePlotBegin(dir=graphics.dir, file.name=graphics.file.name)
par(mfrow=c(2, 2))
for (i.sub in i.subs) {
  data.file.name = '3-nn'
  PerfFunction = WeightedFMeasure
  
  subject.dir = paste('subject', i.sub, sep='_')
  
  baseline.folder = 'f_1'
  baseline.dir = paste(root.dir, preproc.dir, baseline.folder, 
                       subject.dir, sep='/')
  baseline.l = LoadObject(file.name=data.file.name, dir=baseline.dir)
  baseline.x.max = 0
  for (rep in baseline.l) {
    this.x.max = rep$q.i[length(rep$q.i)]
    if (this.x.max > baseline.x.max) {
      baseline.x.max = this.x.max
    }
  }
  baseline.y = GetAverageLC(cm.data=baseline.l, x.max=baseline.x.max, 
                            PerfFunction=PerfFunction)
  baseline.y.effort = GetAverageLC.effort(cm.data=baseline.l, 
                                          PerfFunction=PerfFunction)
  
  gamma.folder = paste('gamma', gamma, sep='_')
  gamma.dir = paste(root.dir, preproc.dir, gamma.folder,
                    subject.dir, sep='/')
  gamma.l = LoadObject(file.name=data.file.name, dir=gamma.dir)
  gamma.x.max = 0
  for (rep in gamma.l) {
    this.x.max = rep$q.i[length(rep$q.i)]
    if (this.x.max > gamma.x.max) {
      gamma.x.max = this.x.max
    }
  }
  gamma.y = GetAverageLC(cm.data=gamma.l, x.max=gamma.x.max, 
                         PerfFunction=PerfFunction)
  gamma.y.effort = GetAverageLC.effort(cm.data=gamma.l, 
                                       PerfFunction=PerfFunction)
  
  #x.max = max(baseline.x.max, gamma.x.max)
  #baseline.y.stretched = GetAverageLC(cm.data=baseline.l, x.max=x.max, 
  #                                    PerfFunction=PerfFunction)
  #gamma.y.stretched = GetAverageLC(cm.data=gamma.l, x.max=x.max, 
  #                                 PerfFunction=PerfFunction)
  #plot()
  
  ylim = c(0, 0.8)
  main = paste('subject', i.sub, preproc.dir)
  plot(baseline.y.effort, type='l', ylim=ylim, main=main, col='blue')
  lines(gamma.y.effort, col='red')
}
SavePlotEnd()
