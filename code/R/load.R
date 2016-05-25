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

## load.R
# read input data for each user

library(foreach)

# ReadSubjectData should be defined as one of the following:
# - ReadSubjectData.pamap
# - ReadSubjectData.usc.had

ReadSubjectsData = function() {
  s = list()
  
  subject.dirs = GetSubjectDirs()
  foreach (s.dir = subject.dirs) %do% {
    s.no = GetSubjectNumber(s.dir)
    s.data = ReadSubjectData(s.dir)
    #s.data = ReadSubjectData.usc.had(s.dir)
    next.index = 1 + length(s)
    
    s[[next.index]] = list()
    s[[next.index]]$number = s.no
    s[[next.index]]$data = s.data
  }
  
  return(s)
}

GetSubjectDirs = function() {
  #setwd('C:/Users/tudor/Documents/ALTLAR/input')
  #setwd('~/R/sf_ALTLAR/input')
  return( dir(pattern='^[sS]ubject[[:digit:]]+$') )
}

GetSubjectNumber = function(subject.dir) {
  return( strsplit(subject.dir, "^[sS]ubject")[[1]][2] )
}

ReadSubjectData.pamap = function(subject.dir) {
  acc.files = paste(subject.dir, dir(subject.dir, "^[[:digit:]]+\\.csv"),
                    sep='/')
  label.file = paste(subject.dir, dir(subject.dir, "^labels.csv$"), sep='/')
  
  labels = read.csv(label.file, header=F, sep=',', stringsAsFactors=T)
  names(labels) <- c('file.number', 'label')
  
  d = list()
  
  for (i in seq_along(acc.files)) {
    acc.file = acc.files[i]
    
    acc.data = read.csv(acc.file, header=T, sep=',', stringsAsFactors=F)
    acc.data$x = as.double(acc.data$x)
    acc.data$y = as.double(acc.data$y)
    acc.data$z = as.double(acc.data$z)
        
    #acc.data = read.csv(acc.file, header=F, sep=',')
    #names(acc.data) <- c('t', 'x', 'y', 'z')
    d[[i]] = list()
    d[[i]]$acc = acc.data
    
    acc.file.name.no.dir = strsplit(acc.file, '/')[[1]][2]
    file.number = as.integer(strsplit(acc.file.name.no.dir, "\\.csv$"))[[1]][1]
    d[[i]]$label = labels[labels$file.number == file.number, 'label']
  }
  
  return( d )
}

ReadSubjectData.usc.had = function(subject.dir) {
  style.i = seq(from=0, to=55, by=5)
  activity.i = c()
  for (j in 1:5) {
    style.i = style.i + 1
    activity.i = c(activity.i, style.i)
  }
  
  label.file = paste(subject.dir, dir(subject.dir, "^labels.csv$"), sep='/')
  labels = read.csv(label.file, header=F, sep=',', stringsAsFactors=T)
  names(labels) <- c('file.number', 'label')
  
  d = list()
  
  #cat(subject.dir, '\n')
  for (k in seq_along(activity.i)) {
    i = activity.i[k]
    acc.file = paste(i, 'csv', sep='.')
    #cat('   ', acc.file, '->')
    acc.file = paste(subject.dir, acc.file, sep='/')
    
    acc.data = read.csv(acc.file, header=T, sep=',', stringsAsFactors=F)
    acc.data$x = as.double(acc.data$x)
    acc.data$y = as.double(acc.data$y)
    acc.data$z = as.double(acc.data$z)
    
    #acc.data = read.csv(acc.file, header=F, sep=',')
    #names(acc.data) <- c('t', 'x', 'y', 'z')
    d[[k]] = list()
    d[[k]]$acc = acc.data
    
    acc.file.name.no.dir = strsplit(acc.file, '/')[[1]][2]
    file.number = as.integer(strsplit(acc.file.name.no.dir, "\\.csv$"))[[1]][1]
    d[[k]]$label = labels[labels$file.number == file.number, 'label']
    #cat('', d[[k]]$label, '\n')
  }
  
  return( d )
}
