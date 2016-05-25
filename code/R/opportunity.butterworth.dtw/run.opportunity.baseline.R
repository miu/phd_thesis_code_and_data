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

library(doRedis)


###############################################################################

queue.name='al.online.dtw.queue'
registerDoRedis(queue=queue.name)

subject.ns = seq_len(4)

train.dataset.name = 'train.dataset.data'
test.dataset.name = 'test.dataset.data'


## random baseline
for (subject.number in subject.ns) {
  subject.dir = paste('subject', subject.number, sep='_')
  dataset.path = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw', subject.dir, sep='/')
  
  train.dataset.file.path = paste(dataset.path, train.dataset.name, sep='/')
  load(file=train.dataset.file.path)
  
  test.dataset.file.path = paste(dataset.path, test.dataset.name, sep='/')
  load(file=test.dataset.file.path)
  
  output.dir = paste(dataset.path, 'output.redone', sep='/')
  dir.create(path=output.dir, showWarnings=T, recursive=T)
  
  num.reps = 10
  #ks = c(1, 2, 3)
  ks = c(3)
  
  #gammas = c(1, 2, 3, 5, 6)
  gammas = c(6)
  #gammas = c(1)
  max.size = 200
  #max.size = 2
  
  ## unfiltered datasets
  for (k in ks) {
    
    selection.function = Online.BMargin
    for (gamma in gammas) {
      
      #
      #metric.function = Offline.Confidence
      #l = SimulateOpportunityDTW.rep(num.reps=num.reps, train.set=train.dataset, test.set=test.dataset, k=k, 
      #                               metric.function=metric.function, selection.function=selection.function, 
      #                               max.size=max.size, gamma=gamma)
      #file.name = paste('l', 'unfiltered', 'subject', subject.number, 'k', k, 'confidence', 'gamma', gamma, 'Rdata', sep='.')
      #
      #file.path = paste(output.dir, file.name, sep='/')
      #save(l, file=file.path)
      #l = NULL
      
      #
      #metric.function = Offline.Margin
      #l = SimulateOpportunityDTW.rep(num.reps=num.reps, train.set=train.dataset, test.set=test.dataset, k=k, 
      #                               metric.function=metric.function, selection.function=selection.function, 
      #                               max.size=max.size, gamma=gamma)
      #file.name = paste('l', 'unfiltered', 'subject', subject.number, 'k', k, 'margin', 'gamma', gamma, 'Rdata', sep='.')
      #
      #file.path = paste(output.dir, file.name, sep='/')
      #save(l, file=file.path)
      #l = NULL
    }
    
    #
#     selection.function = Online.Random
#     metric.function = Offline.Confidence
#     l = SimulateOpportunityDTW.all.rep(num.reps=num.reps, train.set=train.dataset, test.set=test.dataset, k=k, 
#                                    metric.function=metric.function, selection.function=selection.function, 
#                                    max.size=max.size, gamma=1)
#     file.name = paste('l', 'unfiltered', 'subject', subject.number, 'k', k, 'random.all', 'Rdata', sep='.')
#     file.path = paste(output.dir, file.name, sep='/')
#     save(l, file=file.path)
#     l = NULL
  }
  
  
}

## population baseline
for (subject.number in subject.ns) {
  subject.dir = paste('subject', subject.number, sep='_')
  dataset.path = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw', subject.dir, sep='/')
  
  test.dataset.file.path = paste(dataset.path, test.dataset.name, sep='/')
  load(file=test.dataset.file.path)
  
  train.ds = list()
  for (non.subject.number in setdiff(subject.ns, subject.number)) {
    subject.dir = paste('subject', non.subject.number, sep='_')
    non.dataset.path = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw', subject.dir, sep='/')
    
    train.dataset.file.path = paste(non.dataset.path, train.dataset.name, sep='/')
    load(file=train.dataset.file.path)  
    
    train.ds = c(train.ds, train.dataset)
  }
  train.dataset = train.ds
  
  output.dir = paste(dataset.path, 'output.redone', sep='/')
  dir.create(path=output.dir, showWarnings=T, recursive=T)
  
  num.reps = 10
  #ks = c(1, 2, 3)
  ks = c(3)
  
  #gammas = c(1, 2, 3, 5, 6)
  gammas = c(6)
  #gammas = c(1)
  max.size = 200
  #max.size = 2
  
  ## unfiltered datasets
  for (k in ks) {
    
    selection.function = Online.BMargin
    for (gamma in gammas) {
      
      #
      #metric.function = Offline.Confidence
      #l = SimulateOpportunityDTW.rep(num.reps=num.reps, train.set=train.dataset, test.set=test.dataset, k=k, 
      #                               metric.function=metric.function, selection.function=selection.function, 
      #                               max.size=max.size, gamma=gamma)
      #file.name = paste('l', 'unfiltered', 'subject', subject.number, 'k', k, 'confidence', 'gamma', gamma, 'Rdata', sep='.')
      #
      #file.path = paste(output.dir, file.name, sep='/')
      #save(l, file=file.path)
      #l = NULL
      
      #
      #metric.function = Offline.Margin
      #l = SimulateOpportunityDTW.rep(num.reps=num.reps, train.set=train.dataset, test.set=test.dataset, k=k, 
      #                               metric.function=metric.function, selection.function=selection.function, 
      #                               max.size=max.size, gamma=gamma)
      #file.name = paste('l', 'unfiltered', 'subject', subject.number, 'k', k, 'margin', 'gamma', gamma, 'Rdata', sep='.')
      #
      #file.path = paste(output.dir, file.name, sep='/')
      #save(l, file=file.path)
      #l = NULL
    }
    
    #
    selection.function = Online.Random
    metric.function = Offline.Confidence
    l = SimulateOpportunityDTW.all.rep(num.reps=num.reps, train.set=train.dataset, test.set=test.dataset, k=k, 
                                       metric.function=metric.function, selection.function=selection.function, 
                                       max.size=max.size, gamma=1)
    file.name = paste('l', 'unfiltered', 'subject', subject.number, 'k', k, 'random.pop', 'Rdata', sep='.')
    file.path = paste(output.dir, file.name, sep='/')
    save(l, file=file.path)
    l = NULL
  }
  
  
}
