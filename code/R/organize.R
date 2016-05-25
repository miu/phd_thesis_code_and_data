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

## organize.R
# split data into windows
# extract features from window
# associate feature set to its corresponding subject
#library(doMC)
source('R/subjects.search.R')

BalanceLabelsForOneSubject = function(s.f) {
  l = list()
  new.f.table = list()
  f.table = s.f$data
  min.count = min(table(f.table$activity))
  
  for (act in unique(f.table$activity)) {
    f.table.act = f.table[f.table$activity == act, ]
    i = sample(seq_len(nrow(f.table.act)), min.count)
    f.table.act.sub = f.table.act[i, ]
    new.f.table = rbind(new.f.table, f.table.act.sub)
  }
  l$number = s.f$number
  l$data = new.f.table
  
  return( l )
} 

BalanceLabelsForSubjects = function(subjects.features) {
  lapply(subjects.features, BalanceLabelsForOneSubject)
}

CollapseToTable = function(subjects.features) {
  s.f.table = subjects.features[[1]]$data[0, ]
  
  for (s.f.i in seq_along(subjects.features)) {
    s.f = subjects.features[[s.f.i]]
    s.f.table = rbind(s.f.table, s.f$data)    
  }
  
  return( s.f.table )
}

GetFeaturesForSubjects = function(s) {
  s2 = GetWindowsForSubjects(s)
  s3 = list()
  
  list.append = function(l, e) {
    l[[1+length(l)]] = e
    return( l )
  }
  
  #s3 = foreach(i = seq_along(s), .init=s3, .combine=list.append, .inorder=T, 
  #        .multicombine=F, .errorhandling='stop') %dopar% {
  FUN = function(i) {
    l = list()
    l$number = s2[[i]]$number
    
    d = NULL
    #foreach(h = seq_along(s2[[i]]$data)) %do% {
    for (h in seq_along(s2[[i]]$data)) {
      #cat('(i,h) = (', i, ',', h, ')\n')
      f.row = ExtractFeaturesFromWindow(s2[[i]]$data[[h]])
      d = rbind(d, f.row, deparse.level=0)
    }
    d = as.data.frame(d)
    d$activity = as.factor(d$activity)
    l$data = d
    
    l
  }
  lapply(seq_along(s), FUN)    
}

GetWindowsForSubjects = function(s) {
  s2 = list()
  #foreach(i = seq_along(s)) %do% {
  for (i in seq_along(s)) {
    s2[[i]] = list()
    s2[[i]]$number = s[[i]]$number
    windows = list()
    
    d = s[[i]]$data
    #foreach(h = seq_along(d)) %do% {
    for (h in seq_along(d)) {
      w = SplitIntoWindows(d[[h]])
      windows = c(windows, w)
    }
    
    s2[[i]]$data = windows
  }
  return( s2 )
}

SplitIntoWindows = function(d) {
  t = d$acc$t
  x = d$acc$x
  y = d$acc$y
  z = d$acc$z
  label = d$label
  
  lt = length(t)
  lx = length(x)
  ly = length(y)
  lz = length(z)
  if ((lx != ly) | (ly != lz) | (lz != lt)) {
    msg = paste("In splitting function: t x y z do not have the same lengths:",
                lt, lx, ly, lz, '\n')
    stop(msg)
  }
  
  windows = list()
  i.seq = seq(1, lx+1, WINDOW_LENGTH)  
  
  for (k in seq_along(i.seq[1: (length(i.seq)-1)])) {
    w = list()
    i.start = i.seq[k]
    i.end = i.seq[k+1]
    
    if (is.na(i.end)) {
      break
    }
    
    range = i.start: (i.end-1)
    w$x = x[range]
    w$y = y[range]
    w$z = z[range]
    next.index = 1 + length(windows)
    windows[[next.index]] = list()
    windows[[next.index]]$acc = as.data.frame(w)
    windows[[next.index]]$label = label
    #cat(label, '')
  }
  #cat('\n')
  
  return( windows )
}

ExtractFeaturesFromWindow = function(w) {
  #ExtractFeaturesFromWindow.3d.plus.a(w)
  ExtractFeaturesFromWindow.3d(w)
  #ExtractFeaturesFromWindow.a(w)
  #ExtractFeaturesFromWindow.a.fft(w)
}

ExtractFeaturesFromWindow.3d = function(w) {
  x = w$acc$x
  y = w$acc$y
  z = w$acc$z
  
  f = c()
  
  # axis means
  mean.x = mean(x)
  names(mean.x) = 'mean.x'
  mean.y = mean(y)
  names(mean.y) = 'mean.y'
  mean.z = mean(z)
  names(mean.z) = 'mean.z'
  f = c(f, mean.x, mean.y, mean.z)
  
  # axis variances
  var.x = var(x)
  names(var.x) = 'var.x'
  var.y = var(y)
  names(var.y) = 'var.y'
  var.z = var(z)
  names(var.z) = 'var.z'
  f = c(f, var.x, var.y, var.z)
  
  # inter-axis correlations
  options(warn=-1)
  
  cor.xy = cor(x, y)
  if (is.na(cor.xy)) {
    cor.xy = 0
  }
  names(cor.xy) = 'cor.xy'
  
  cor.yz = cor(y, z)
  if (is.na(cor.yz)) {
    cor.yz = 0
  }
  names(cor.yz) = 'cor.yz'
  
  cor.zx = cor(z, x)
  if (is.na(cor.zx)) {
    cor.zx = 0
  }
  names(cor.zx) = 'cor.zx'
  f = c(f, cor.xy, cor.yz, cor.zx)
  
  options(warn=1)
  
  # activity label
  label = w$label
  names(label) = 'activity'
  f = c(f, label)
  
  return( f )
}

ExtractFeaturesFromWindow.a.fft = function(w) {
  x = w$acc$x
  y = w$acc$y
  z = w$acc$z
  
  # amplitude
  a = sqrt(x^2 + y^2 + z^2)
  
  f = c()
  
  # mean
  m = mean(a)
  names(m) = 'mean'
  f = c(f, m)
  
  # var
  var.a = var(a)
  names(var.a) = 'var'
  f = c(f, var.a)

  ## FFT coefficients
  fft.a = abs(fft(a))
  fft.a = fft.a[2:(length(fft.a)/2)] # exclude DC and mirrorring coefficients past half
  
  # spectral energy
  spectral.energy = sum(fft.a^2) / length(fft.a)
  names(spectral.energy) = 'spectral.energy'
  f = c(f, spectral.energy)
  
  ## Function to compute the entropy of a vector
  Entropy <- function(p) {
    p.norm <- p / sum(p) # normalize
    return( -sum(log(p.norm) * p.norm) )
  }
  
  # spectral entropy
  spectral.entropy = Entropy(fft.a) / length(fft.a)
  names(spectral.entropy) = 'spectral.entropy'
  f = c(f, spectral.entropy)
  
  # activity label
  label = w$label
  names(label) = 'activity'
  f = c(f, label)
    
  return( f )
}

ExtractFeaturesFromWindow.a = function(w) {
  x = w$acc$x
  y = w$acc$y
  z = w$acc$z
  
  # amplitude
  a = sqrt(x^2 + y^2 + z^2)
  
  f = c()
  
  # mean
  m = mean(a)
  names(m) = 'mean'
  f = c(f, m)
  
  # var
  var.a = var(a)
  names(var.a) = 'var'
  f = c(f, var.a)
  
  # activity label
  label = w$label
  names(label) = 'activity'
  f = c(f, label)
  
  return( f )
}

ExtractFeaturesFromWindow.3d.plus.a = function(w) {
  f.3d = ExtractFeaturesFromWindow.3d(w)
  f.3d = f.3d[-which(names(f.3d) == 'activity')]
  
  f.a = ExtractFeaturesFromWindow.a(w)
  
  return( c(f.3d, f.a) )
}

