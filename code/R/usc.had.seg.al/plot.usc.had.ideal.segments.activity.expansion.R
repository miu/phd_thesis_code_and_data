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

setwd('~/R/sf_ALTLAR/R/usc.had.seg.al/')

source('../ml.utils.R')

GetLabels = function(l) {
  colnames(l[[1]]$cms.data[[1]]$cms[[1]])
}

GetLabelSequence.proportions = function(l.rep) {
  l.props = list()
  class.labels = colnames(l.rep$cms[[1]])
  
  counts = rep(0, times=length(class.labels))
  names(counts) = class.labels
  
  for (label in l.rep$act.labels) {
    counts[label] = 1 + counts[label]
    l.props[[1 + length(l.props)]] = counts / sum(counts)
  }
  
  return( l.props )
}

GetLabelSequence.all = function(l) {
  l.l.props = list()
  
  for (l.subj in l) {
    for (l.rep in l.subj$cms.data) {
      l.l.props[[1 + length(l.l.props)]] = GetLabelSequence.proportions(l.rep)
    }
  }
  
  return( l.l.props )
}

Entropy = function(p, exclude.labels=NA, ...) {
  p[exclude.labels] = 0
  p = p[which(p != 0)]
  return( sum(-p * log(p)) )
}

ExcludeLabels = function(props, exclude.labels=NA) {
  p = props[which(!(names(props) %in% exclude.labels))]
  p = p / sum(p)
  
  return( p )
}

RunningEntropy = function(l.props, exclude.labels=NA) {
  e = rep(x=NA, times=length(l.props))
  for (i in seq_along(l.props)) {
    e[i] = Entropy(ExcludeLabels(l.props[[i]], exclude.labels=exclude.labels))
  }
  return( e )
}

RunningVariance = function(l.props, exclude.labels=NA) {
  v = rep(x=NA, times=length(l.props))
  for (i in seq_along(l.props)) {
    v[i] = var(ExcludeLabels(l.props[[i]], exclude.labels=exclude.labels))
  }
  return( v )
}

RunningProportion = function(l.props, exclude.labels=NA) {
  
}

GetAvgPerformance = function(l) {
  
  max.len = -1
  for (l.subj in l) {
    for (l.rep in l.subj$cms.data) {
      len = length(l.rep$cms)
      if (len > max.len) {
        max.len = len
      }
    }
  }
  
  m.perf = c()
  for (l.subj in l) {
    for (l.rep in l.subj$cms.data) {
      v.perf = sapply(l.rep$cms, WeightedFMeasure)
      num.padding = max.len - length(v.perf)
      v.perf = c(v.perf, rep(x=NA, times=num.padding))
      m.perf = rbind(m.perf, v.perf)
    }
  }
  
  mean.perf = apply(X=m.perf, MARGIN=2, FUN=mean, na.rm=T)
  
  return( mean.perf )
}

## TODO
## entropy / variance / num.activities.discovered:
## - with all activities
## - without sedentary activities

#### PAMAP with expansion (10)
al.path = '~/R/sf_ALTLAR/desktop.output/usc.had.expanded/pamap/l.al.gamma.6.expand,factor.10.Rdata'
rs.path = '~/R/sf_ALTLAR/desktop.output/usc.had.expanded/pamap/l.rs.f.0.1.expand.factor.10.Rdata'

#### PAMAP without expansion (1)
al.path = '~/R/sf_ALTLAR/desktop.output/usc.had.expanded/pamap/l.al.gamma.6.expand,factor.1.Rdata'
rs.path = '~/R/sf_ALTLAR/desktop.output/usc.had.expanded/pamap/l.rs.f.0.1.expand.factor.1.Rdata'

#### USC-HAD - class imbalance not right
#al.path = '~/R/sf_ALTLAR/desktop.output/usc.had.expanded/usc.had/l.al.gamma.6.expand,factor.10.Rdata'
#rs.path = '~/R/sf_ALTLAR/desktop.output/usc.had.expanded/usc.had/l.rs.f.0.1.expand.factor.10.Rdata'

x.limit = 300
seq.limit = seq_len(x.limit)

load(al.path) # l.al
load(rs.path) # l.rs
al.perf = GetAvgPerformance(l=l.al)
rs.perf = GetAvgPerformance(l=l.rs)

plot(al.perf[seq.limit], type='l', col='red', lwd=2, ylab='F-Score', xlab='Num. Annotations')
lines(rs.perf[seq.limit], col='blue', lwd=2)
legend(x='bottomright', legend=c('Online Active Learning              ', 
                                 'Random Selection                    '), 
       col=c('red', 'blue'), lwd=c(2, 2))


l.l.props.al = GetLabelSequence.all(l.al)
l.l.props.rs = GetLabelSequence.all(l.rs)


DoMean = function(l.running) {
  len = max(sapply(l.running, length))
  m = c()
  for (l.run in l.running) {
    num.padding = len - length(l.run)
    m = rbind(m, c(l.run, rep(x=NA, times=num.padding)))
  }
  
  return( apply(X=m, MARGIN=2, FUN=mean, na.rm=T) )
}

#### running entropy
#l.entropy.al = lapply(X=l.l.props.al, FUN=RunningEntropy, exclude.labels=c('1', '2'))
l.entropy.al = lapply(X=l.l.props.al, FUN=RunningEntropy)
entropy.al = DoMean(l.running=l.entropy.al)

#l.entropy.rs = lapply(X=l.l.props.rs, FUN=RunningEntropy, exclude.labels=c('1', '2'))
l.entropy.rs = lapply(X=l.l.props.rs, FUN=RunningEntropy)
entropy.rs = DoMean(l.running=l.entropy.rs)

plot(entropy.al[seq.limit], type='l', col='red', ylab='Label Entropy', xlab='Num. Annotations')
lines(entropy.rs[seq.limit], col='blue')
legend(x='bottomright', legend=c(
  'Online Active Learning              ', 
  'Random Selection                    '), 
  col=c('red', 'blue'), lwd=c(1, 1))
