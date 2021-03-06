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
train.dataset.filtered.name = 'train.dataset.filtered.data'
test.dataset.filtered.name = 'test.dataset.filtered.data'

for (subject.number in subject.ns) {
  
  betas = c(0.1, 0.5, 1, 2.5, 5, 10)
  #betas = c(0.1, 5)
  
  for (beta in betas) {
    
    subject.dir = paste('subject', subject.number, sep='_')
    dataset.path = paste('~/R/sf_ALTLAR/output/opportunity.butterworth.dtw', subject.dir, sep='/')
    
    train.dataset.file.path = paste(dataset.path, train.dataset.name, sep='/')
    load(file=train.dataset.file.path)
    
    test.dataset.file.path = paste(dataset.path, test.dataset.name, sep='/')
    load(file=test.dataset.file.path)
    
    train.dataset.filtered.file.path = paste(dataset.path, train.dataset.filtered.name, sep='/')
    load(file=train.dataset.filtered.file.path)
    
    test.dataset.filtered.file.path = paste(dataset.path, test.dataset.filtered.name, sep='/')
    load(file=test.dataset.filtered.file.path)
    
    
    output.dir = paste(dataset.path, 'output/modulated.beta', sep='/')
    dir.create(path=output.dir, showWarnings=T, recursive=T)
    
    history.size = 10
    
    num.reps = 10
    #num.reps = 1
    
    k = 3
    
    max.size = 200
    #max.size = 10
    
    horizon.size = 2000
    #horizon.size = 50
    
    num.gamma.iterations = 50
    
    metric.function = Offline.Confidence
    
    selection.function = Online.BMargin
    l = SimulateOpportunityDTW.modulated.beta.unif.rep(num.reps=num.reps, train.set=train.dataset, 
                                                       test.set=test.dataset, k=k, 
                                                       metric.function=metric.function, 
                                                       selection.function=selection.function, 
                                                       max.size=max.size, horizon.size=horizon.size, 
                                                       history.size=history.size, 
                                                       num.gamma.iterations=num.gamma.iterations, 
                                                       monotone.increasing=F, beta=beta)
    file.name = paste('l', 'subject', subject.number, 'k', k, 'unif', 'gamma', 'beta', beta, 'Rdata', sep='.')        
    file.path = paste(output.dir, file.name, sep='/')
    save(l, file=file.path)
    l = NULL
    cat('saved', file.path, '\n')
    
    lambda = 3
    selection.function = Online.BMargin
    l = SimulateOpportunityDTW.modulated.beta.exp.rep(num.reps=num.reps, train.set=train.dataset, 
                                                      test.set=test.dataset, k=k, 
                                                      metric.function=metric.function, 
                                                      selection.function=selection.function, 
                                                      max.size=max.size, horizon.size=horizon.size, 
                                                      history.size=history.size,
                                                      num.gamma.iterations=num.gamma.iterations, 
                                                      lambda=lambda, monotone.increasing=F, beta=beta)
    file.name = paste('l', 'subject', subject.number, 'k', k, 'exp', 'gamma', 'lambda', lambda, 'beta', beta, 
                      'Rdata', sep='.')    
    file.path = paste(output.dir, file.name, sep='/')
    save(l, file=file.path)
    l = NULL
    cat('saved', file.path, '\n')
  }
}
