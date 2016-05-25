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

source('R/ml.utils.R')
source('R/file.utils.R')

MovingAverage = function(y, kernel.width) {
  i.seq = kernel.width : (length(y) - kernel.width + 1)
  y.ma = rep(NA, length(i.seq))
  for (i.ma in seq_along(i.seq)) {
    i.y = i.seq[i.ma]
    i.kernel = (i.y - kernel.width + 1) : i.y
    y.ma[i.ma] = mean(y[i.kernel])
  }
  padding = rep(NA, kernel.width)
  return( c(padding, y.ma, padding) )
}

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
  ys = lapply(X=cm.data, FUN=GetLC, x.max=x.max, PerfFunction=PerfFunction)
  y = rep(0, times=x.max)
  ys.len = length(ys)
  
  for (i in seq_len(x.max)) {
    for (k in seq_along(ys)) {
      y[i] = y[i] + (ys[[k]][i] / ys.len)
    }
  }
  return( y )
}

PlotAnnotationTimes = function(cm.data.1.rep, x.max, PerfFunction,
                               col, main, file.name, dir, ylim,
                               height=height, width=width) {
  perf.y = GetLC(cm.data=cm.data.1.rep, x.max=x.max,
                 PerfFunction=PerfFunction)
  SavePlotBegin(dir=dir, file.name=file.name,
                height=height, width=width)
  plot(perf.y, type='l', xlab='Number of segments seen', 
       ylab='F-Score', col=col, main=main, ylim=ylim)
  for (i in seq_along(cm.data.1.rep$q.i)) {
    question.i = cm.data.1.rep$q.i[i]
    value = perf.y[question.i]
    x = c(question.i, question.i)
    y = c(0, value)
    lines(x=x, y=y, col='grey50', lty='dotted')
  }
  SavePlotEnd()
}

MakeMcNemarMatrix = function(predicted.1, predicted.2, actual) {
  correct.1.i = which(predicted.1 == actual)
  incorrect.1.i = which(predicted.1 != actual)
  correct.2.i = which(predicted.2 == actual)
  incorrect.2.i = which(predicted.2 != actual)
  
  mcn = matrix(0, nrow=2, ncol=2)
  mcn[1, 1] = length(intersect(correct.1.i, correct.2.i))
  mcn[1, 2] = length(intersect(correct.1.i, incorrect.2.i))
  mcn[2, 1] = length(intersect(incorrect.1.i, correct.2.i))
  mcn[2, 2] = length(intersect(incorrect.1.i, incorrect.2.i))
  
  return( mcn )
}

McNemar.within.index = function(cm.data) {
  l.pred = cm.data$predicteds
  actual = cm.data$actual
  n = length(l.pred)
  last.predicted = l.pred[[n]]
  first.insignificant.index = 0
  for (i in seq_len(n)) {
    current.predicted = l.pred[[i]]
    mcn = MakeMcNemarMatrix(predicted.1=last.predicted, 
                            predicted.2=current.predicted,
                            actual=actual)
    #print(mcn)
    test = mcnemar.test(mcn)
    #print(test)
    if (is.na(test$p.value)) {
      next
    }
    if (test$p.value > 0.05) {
      #cat('i: ', i, ' is insignificant (', test$p.value, ')\n', sep='')
      first.insignificant.index = i
      break
    } else {
      #cat('i: ', i, ' is significant (', test$p.value, ')\n', sep='')
    }
  }
  
  return( cm.data$q.i[first.insignificant.index] )
}

McNemar.within.avg.index = function(cms) {
  sig.i = rep(0, length(cms))
  for (i in seq_along(cms)) {
    sig.i[i] = McNemar.within.index(cm.data=cms[[i]])
  }
  return( mean(sig.i) )
}

McNemar.end = function(cm.data.1, cm.data.2) {
  if (sum(cm.data.1$actual != cm.data.2$actual)) {
    stop('Different actuals')
  }
  
  n1 = length(cm.data.1$predicteds)
  predicted.1 = cm.data.1$predicteds[[n1]]
  
  n2 = length(cm.data.2$predicteds)
  predicted.2 = cm.data.2$predicteds[[n2]]
  
  mcn = MakeMcNemarMatrix(predicted.1=predicted.1, 
                          predicted.2=predicted.2, 
                          actual=cm.data.1$actual)
  test = mcnemar.test(mcn)
  if (is.na(test$p.value)) {
    return( T )
  } else {
    return( test$p.value > 0.05 )
  }
}

Mode = function(v) {
  t.v = table(v)
  i.max = which(t.v == max(t.v))
  if (length(i.max) > 1) {
    i.max = sample(i.max, 1)
  }
  return( names(i.max) )
}

McNemar.avg.end = function(cms.1, cms.2) {
  actual = cms.1[[1]]$actual
  len.actual = length(actual)
  
  num.reps.1 = length(cms.1)
  len.preds.1 = length(cms.1[[1]]$predicteds)
  preds.1 = rep('', len.actual)
  for (i.slot in seq_len(len.actual)) {
    v = rep('', num.reps.1)
    for (i.rep in seq_len(num.reps.1)) {
      v[i.rep] = cms.1[[i.rep]]$predicteds[[len.preds.1]][i.slot]
    }
    preds.1[[i.slot]] = Mode(v)
  }
  
  num.reps.2 = length(cms.2)
  len.preds.2 = length(cms.2[[1]]$predicteds)
  preds.2 = rep('', len.actual)
  for (i.slot in seq_len(len.actual)) {
    v = rep('', num.reps.2)
    for (i.rep in seq_len(num.reps.2)) {
      v[i.rep] = cms.2[[i.rep]]$predicteds[[len.preds.2]][i.slot]
    }
    preds.2[[i.slot]] = Mode(v)
  }
  
  mcn = MakeMcNemarMatrix(predicted.1=preds.1, 
                          predicted.2=preds.2, 
                          actual=actual)
  test = mcnemar.test(mcn)
  if (is.na(test$p.value)) {
    return( T )
  } else {
    return( test$p.value > 0.05 )
  }
}

McNemar.vote.end = function(cms.1, baseline) {
  votes = sapply(cms.1, McNemar.end, cm.data.2=baseline[[1]])
  return( sum(votes)/length(votes) >= 0.5 )
}

BudgetSpentSoFar = function(cms, mcnemar.index) {
  num.reps = length(cms)
  budgets = rep(0, num.reps)
  for (i in seq_along(cms)) {
    budgets[i] = sum(cms[[i]]$q.i <= mcnemar.index)
  }
  return( mean(budgets) )
}

PrintBudgetTableData = 
  function(baseline.unshuffled.dir, baseline.shuffled.dir, 
           graphics.dir, subject.dirs, perc.dirs,
           i.subject, percs, text.file.name) {
    
    text.file.path = paste(graphics.dir, text.file.name, sep='/')
    file.remove(text.file.path)
    file.create(text.file.path)
    con = text.file.path
    print.budget = function(strategy.cms, strategy.y, 
                            baseline.cms, baseline.remaining, 
                            i.sub, i.perc, con) {
      index = McNemar.within.avg.index(strategy.cms[[i.sub]][[i.perc]])
      end = McNemar.avg.end(strategy.cms[[i.sub]][[i.perc]], 
                            baseline.cms[[i.sub]])
      budget = BudgetSpentSoFar(cms=strategy.cms[[i.sub]][[i.perc]], 
                                mcnemar.index=index)
      i.remaining = floor(index): 
        length(strategy.cms[[i.sub]][[i.perc]][[1]]$actual)
      remaining = strategy.y[[i.sub]][[i.perc]][i.remaining]
      difference = mean(baseline.remaining) - mean(remaining)
      last.y = strategy.y[[i.sub]][[i.perc]][length(strategy.y[[i.sub]][[i.perc]])]
      cat(' - timestamp:  ', index, '\n', file=con, append=T)
      cat(' - budget:     ', budget, '\n', file=con, append=T)
      cat(' - difference: ', difference, '\n', file=con, append=T)
      cat(' - significant:', !end, '\n', file=con, append=T)
      cat(' - end perf   :', last.y, '\n', file=con, append=T)
    }
    
    ### unshuffled    
    # baseline unshuffled
    cat('Baseline.unshuffled\n', file=con, append=T)
    baseline.unshuffled.cms = list()
    baseline.unshuffled.y = list()
    baseline.unshuffled.ma = list()
    x.max = rep(0, length(subjects))
    for (i in seq_along(subjects)) {
      baseline.path = paste(baseline.unshuffled.dir, subject.dirs[i], sep='/')
      baseline.unshuffled.cms[[i]] = LoadObject(file.name=file.name, dir=baseline.path)  
      x.max[i] = length(baseline.unshuffled.cms[[i]][[1]]$cms)
      baseline.unshuffled.y[[i]] = GetAverageLC(cm.data=baseline.unshuffled.cms[[i]], 
                                                x.max=x.max[i], 
                                                PerfFunction=PerfFunction)
      baseline.unshuffled.ma[[i]] = MovingAverage(y=baseline.unshuffled.y[[i]], 
                                                  kernel.width=kernel.width)
    }
    baseline.unshuffled.index = McNemar.within.avg.index(baseline.unshuffled.cms[[i.subject]])
    baseline.unshuffled.end = McNemar.avg.end(baseline.unshuffled.cms[[i.subject]], baseline.unshuffled.cms[[i.subject]])
    baseline.unshuffled.budget = BudgetSpentSoFar(cms=baseline.unshuffled.cms[[i.subject]], 
                                                  mcnemar.index=baseline.unshuffled.index)
    i.remaining = baseline.unshuffled.index: length(baseline.unshuffled.y[[i.subject]])
    baseline.unshuffled.remaining = baseline.unshuffled.y[[i.subject]][i.remaining]
    last.y = baseline.unshuffled.y[[i.subject]][length(baseline.unshuffled.y[[i.subject]])]
    cat(' - timestamp:  ', baseline.unshuffled.index, '\n', file=con, append=T)
    cat(' - budget:     ', baseline.unshuffled.budget, '\n', file=con, append=T)
    cat(' - difference: ', mean(baseline.unshuffled.remaining) - mean(baseline.unshuffled.remaining), '\n', file=con, append=T)
    cat(' - significant:', !baseline.unshuffled.end, '\n', file=con, append=T)
    cat(' - end perf   :', last.y, '\n', file=con, append=T)
    
    # uniform unshuffled
    cat('Uniform.unshuffled\n', file=con, append=T)
    unif.unshuffled.cms = list()
    unif.unshuffled.y = list()
    unif.unshuffled.ma = list()
    for (i.sub in seq_along(subjects)) {
      lc.cms = list()
      lc.y = list()
      lc.ma = list()
      for (i.perc in seq_along(percs)) {
        cms.path = paste(root.dir, 'unif.unshuffled', perc.dirs[i.perc], subject.dirs[i.sub], sep='/')
        lc.cms[[i.perc]] = LoadObject(file.name=file.name, dir=cms.path)
        lc.y[[i.perc]] = GetAverageLC(cm.data=lc.cms[[i.perc]], x.max=x.max[i.sub], 
                                      PerfFunction=PerfFunction)
        lc.ma[[i.perc]] = MovingAverage(y=lc.y[[i.perc]],
                                        kernel.width=kernel.width)
      }
      unif.unshuffled.cms[[i.sub]] = lc.cms
      unif.unshuffled.y[[i.sub]] = lc.y
      unif.unshuffled.ma[[i.sub]] = lc.ma
    }
    i.sub = -1
    for (i.perc in seq_along(percs)) {
      perc = percs[i.perc]
      cat('Uniform perc=', perc, '\n', file=con, append=T)
      print.budget(strategy.cms=unif.unshuffled.cms, 
                   strategy.y=unif.unshuffled.y, 
                   baseline.cms=baseline.unshuffled.cms, 
                   baseline.remaining=baseline.unshuffled.remaining, 
                   i.sub=i.subject, i.perc=i.perc, con=con)
    }
    
    # exponential unshuffled
    #cat('Exponential...\n')
    lambdas = c(1, 5)
    
    exp.unshuffled.cms = list()
    exp.unshuffled.y = list()
    exp.unshuffled.ma = list()    
    for (i.lambda in seq_along(lambdas)) {
      lambda = lambdas[i.lambda]
      
      exp.unshuffled.cms[[i.lambda]] = list()
      exp.unshuffled.y[[i.lambda]] = list()  
      exp.unshuffled.ma[[i.lambda]] = list()
      for (i.sub in seq_along(subjects)) {
        lc.cms = list()
        lc.y = list()
        lc.ma = list()
        for (i.perc in seq_along(percs)) {
          exp.dir = paste('exp_', lambda, '.unshuffled', sep='')
          cms.path = paste(root.dir, exp.dir, perc.dirs[i.perc], 
                           subject.dirs[i.sub], sep='/')
          lc.cms[[i.perc]] = LoadObject(file.name=file.name, dir=cms.path)
          lc.y[[i.perc]] = GetAverageLC(cm.data=lc.cms[[i.perc]], 
                                        x.max=x.max[i.sub], 
                                        PerfFunction=PerfFunction)
          lc.ma[[i.perc]] = MovingAverage(y=lc.y[[i.perc]], 
                                          kernel.width=kernel.width)
        }
        exp.unshuffled.cms[[i.lambda]][[i.sub]] = lc.cms
        exp.unshuffled.y[[i.lambda]][[i.sub]] = lc.y
        exp.unshuffled.ma[[i.lambda]][[i.sub]] = lc.ma
      }
      i.sub = -1
    }
    
    exp.1.cms = exp.unshuffled.cms[[which(lambdas == 1)]]
    exp.1.y = exp.unshuffled.y[[which(lambdas == 1)]]    
    for (i.perc in seq_along(percs)) {
      perc = percs[i.perc]
      cat('Exponential lambda=1 perc=', perc, '\n', file=con, append=T)
      print.budget(strategy.cms=exp.1.cms, 
                   strategy.y=exp.1.y, 
                   baseline.cms=baseline.unshuffled.cms, 
                   baseline.remaining=baseline.unshuffled.remaining, 
                   i.sub=i.subject, i.perc=i.perc, con=con)
    }
    
    exp.5.cms = exp.unshuffled.cms[[which(lambdas == 5)]]
    exp.5.y = exp.unshuffled.y[[which(lambdas == 5)]]    
    for (i.perc in seq_along(percs)) {
      perc = percs[i.perc]
      cat('Exponential lambda=5 perc=', perc, '\n', file=con, append=T)
      print.budget(strategy.cms=exp.5.cms, 
                   strategy.y=exp.5.y, 
                   baseline.cms=baseline.unshuffled.cms, 
                   baseline.remaining=baseline.unshuffled.remaining, 
                   i.sub=i.subject, i.perc=i.perc, con=con)
    }
    
    # upfront unshuffled
    upfront.unshuffled.cms = list()
    upfront.unshuffled.y = list()
    upfront.unshuffled.ma = list()
    for (i.sub in seq_along(subjects)) {
      lc.cms = list()
      lc.y = list()
      lc.ma = list()
      for (i.perc in seq_along(percs)) {
        cms.path = paste(root.dir, 'upfront.unshuffled', 
                         perc.dirs[i.perc], subject.dirs[i.sub], sep='/')
        lc.cms[[i.perc]] = LoadObject(file.name=file.name, dir=cms.path)
        lc.y[[i.perc]] = GetAverageLC(cm.data=lc.cms[[i.perc]], 
                                      x.max=x.max[i.sub], 
                                      PerfFunction=PerfFunction)
        lc.ma[[i.perc]] = MovingAverage(y=lc.y[[i.perc]], 
                                        kernel.width=kernel.width)
      }
      upfront.unshuffled.cms[[i.sub]] = lc.cms
      upfront.unshuffled.y[[i.sub]] = lc.y
      upfront.unshuffled.ma[[i.sub]] = lc.ma
    }
    i.sub = -1
    for (i.perc in seq_along(percs)) {
      perc = percs[i.perc]
      cat('Upfront perc=', perc, '\n', file=con, append=T)
      print.budget(strategy.cms=upfront.unshuffled.cms, 
                   strategy.y=upfront.unshuffled.y, 
                   baseline.cms=baseline.unshuffled.cms, 
                   baseline.remaining=baseline.unshuffled.remaining, 
                   i.sub=i.subject, i.perc=i.perc, con=con)
    }
    
    # equidistant unshuffled
    equi.unshuffled.cms = list()
    equi.unshuffled.y = list()
    equi.unshuffled.ma = list()
    for (i.sub in seq_along(subjects)) {
      lc.cms = list()
      lc.y = list()
      lc.ma = list()
      for (i.perc in seq_along(percs)) {
        cms.path = paste(root.dir, 'equi.unshuffled', 
                         perc.dirs[i.perc], subject.dirs[i.sub], sep='/')
        lc.cms[[i.perc]] = LoadObject(file.name=file.name, dir=cms.path)
        lc.y[[i.perc]] = GetAverageLC(cm.data=lc.cms[[i.perc]], 
                                      x.max=x.max[i.sub], 
                                      PerfFunction=PerfFunction)
        lc.ma[[i.perc]] = MovingAverage(y=lc.y[[i.perc]], 
                                        kernel.width=kernel.width)
      }
      equi.unshuffled.cms[[i.sub]] = lc.cms
      equi.unshuffled.y[[i.sub]] = lc.y
      equi.unshuffled.ma[[i.sub]] = lc.ma
    }
    i.sub = -1
    for (i.perc in seq_along(percs)) {
      perc = percs[i.perc]
      cat('Equidistant perc=', perc, '\n', file=con, append=T)
      print.budget(strategy.cms=equi.unshuffled.cms, 
                   strategy.y=equi.unshuffled.y, 
                   baseline.cms=baseline.unshuffled.cms, 
                   baseline.remaining=baseline.unshuffled.remaining, 
                   i.sub=i.subject, i.perc=i.perc, con=con)
    }
    
    cat('\n\n############################\n\n\n', file=con, append=T)
    
    ### shuffled
    # baseline shuffled
    cat('Baseline.shuffled\n', file=con, append=T)
    baseline.shuffled.cms = list()
    baseline.shuffled.y = list()
    baseline.shuffled.ma = list()
    x.max = rep(0, length(subjects))
    for (i in seq_along(subjects)) {
      baseline.path = paste(baseline.shuffled.dir, subject.dirs[i], sep='/')
      baseline.shuffled.cms[[i]] = LoadObject(file.name=file.name, dir=baseline.path)
      x.max[i] = length(baseline.shuffled.cms[[i]][[1]]$cms)
      baseline.shuffled.y[[i]] = GetAverageLC(cm.data=baseline.shuffled.cms[[i]], 
                                              x.max=x.max[i], 
                                              PerfFunction=PerfFunction)
      baseline.shuffled.ma[[i]] = MovingAverage(y=baseline.shuffled.y[[i]],
                                                kernel.width=kernel.width)
    }
    baseline.shuffled.index = McNemar.within.avg.index(baseline.shuffled.cms[[i.subject]])
    baseline.shuffled.end = McNemar.avg.end(baseline.shuffled.cms[[i.subject]], baseline.shuffled.cms[[i.subject]])
    baseline.shuffled.budget = BudgetSpentSoFar(cms=baseline.shuffled.cms[[i.subject]], 
                                                  mcnemar.index=baseline.shuffled.index)
    i.remaining = baseline.shuffled.index: length(baseline.shuffled.y[[i.subject]])
    baseline.shuffled.remaining = baseline.shuffled.y[[i.subject]][i.remaining]
    last.y = baseline.shuffled.y[[i.subject]][length(baseline.shuffled.y[[i.subject]])]
    cat(' - timestamp:  ', baseline.shuffled.index, '\n', file=con, append=T)
    cat(' - budget:     ', baseline.shuffled.budget, '\n', file=con, append=T)
    cat(' - difference: ', mean(baseline.shuffled.remaining) - mean(baseline.shuffled.remaining), '\n', file=con, append=T)
    cat(' - significant:', !baseline.shuffled.end, '\n', file=con, append=T)    
    cat(' - end perf   :', last.y, '\n', file=con, append=T)
    
    # unif    
    cat('Uniform.shuffled\n', file=con, append=T)
    unif.shuffled.cms = list()
    unif.shuffled.y = list()
    unif.shuffled.ma = list()
    for (i.sub in seq_along(subjects)) {
      lc.cms = list()
      lc.y = list()
      lc.ma = list()
      for (i.perc in seq_along(percs)) {
        cms.path = paste(root.dir, 'unif.shuffled', perc.dirs[i.perc], 
                         subject.dirs[i.sub], sep='/')
        lc.cms[[i.perc]] = LoadObject(file.name=file.name, dir=cms.path)
        lc.y[[i.perc]] = GetAverageLC(cm.data=lc.cms[[i.perc]], 
                                      x.max=x.max[i.sub], 
                                      PerfFunction=PerfFunction)
        lc.ma[[i.perc]] = MovingAverage(y=lc.y[[i.perc]], 
                                        kernel.width=kernel.width)
      }
      unif.shuffled.cms[[i.sub]] = lc.cms
      unif.shuffled.y[[i.sub]] = lc.y
      unif.shuffled.ma[[i.sub]] = lc.ma
    }
    i.sub = -1
    for (i.perc in seq_along(percs)) {
      perc = percs[i.perc]
      cat('Uniform perc=', perc, '\n', file=con, append=T)
      print.budget(strategy.cms=unif.shuffled.cms, 
                   strategy.y=unif.shuffled.y, 
                   baseline.cms=baseline.shuffled.cms, 
                   baseline.remaining=baseline.shuffled.remaining, 
                   i.sub=i.subject, i.perc=i.perc, con=con)
    }
        
    # exponential
    #cat('Exponential...\n')
    lambdas = c(1, 5)    
    exp.shuffled.cms = list()
    exp.shuffled.y = list()
    exp.shuffled.ma = list()
    for (i.lambda in seq_along(lambdas)) {
      lambda = lambdas[i.lambda]
      
      exp.shuffled.cms[[i.lambda]] = list()
      exp.shuffled.y[[i.lambda]] = list()  
      exp.shuffled.ma[[i.lambda]] = list()
      for (i.sub in seq_along(subjects)) {
        lc.cms = list()
        lc.y = list()
        for (i.perc in seq_along(percs)) {
          exp.dir = paste('exp_', lambda, '.shuffled', sep='')
          cms.path = paste(root.dir, exp.dir, perc.dirs[i.perc], 
                           subject.dirs[i.sub], sep='/')
          lc.cms[[i.perc]] = LoadObject(file.name=file.name, dir=cms.path)
          lc.y[[i.perc]] = GetAverageLC(cm.data=lc.cms[[i.perc]], 
                                        x.max=x.max[i.sub], 
                                        PerfFunction=PerfFunction)
          lc.ma[[i.perc]] = MovingAverage(y=lc.y[[i.perc]], 
                                          kernel.width=kernel.width)
        }
        exp.shuffled.cms[[i.lambda]][[i.sub]] = lc.cms
        exp.shuffled.y[[i.lambda]][[i.sub]] = lc.y
        exp.shuffled.ma[[i.lambda]][[i.sub]] = lc.ma
      }
      i.sub = -1
    }
    
    exp.1.cms = exp.shuffled.cms[[which(lambdas == 1)]]
    exp.1.y = exp.shuffled.y[[which(lambdas == 1)]]    
    for (i.perc in seq_along(percs)) {
      perc = percs[i.perc]
      cat('Exponential lambda=1 perc=', perc, '\n', file=con, append=T)
      print.budget(strategy.cms=exp.1.cms, 
                   strategy.y=exp.1.y, 
                   baseline.cms=baseline.shuffled.cms, 
                   baseline.remaining=baseline.shuffled.remaining, 
                   i.sub=i.subject, i.perc=i.perc, con=con)
    }
    
    exp.5.cms = exp.shuffled.cms[[which(lambdas == 5)]]
    exp.5.y = exp.shuffled.y[[which(lambdas == 5)]]    
    for (i.perc in seq_along(percs)) {
      perc = percs[i.perc]
      cat('Exponential lambda=5 perc=', perc, '\n', file=con, append=T)
      print.budget(strategy.cms=exp.5.cms, 
                   strategy.y=exp.5.y, 
                   baseline.cms=baseline.shuffled.cms, 
                   baseline.remaining=baseline.shuffled.remaining, 
                   i.sub=i.subject, i.perc=i.perc, con=con)
    }
    
    # upfront
    upfront.shuffled.cms = list()
    upfront.shuffled.y = list()
    upfront.shuffled.ma = list()
    for (i.sub in seq_along(subjects)) {
      lc.cms = list()
      lc.y = list()
      lc.ma = list()
      for (i.perc in seq_along(percs)) {
        cms.path = paste(root.dir, 'upfront.shuffled', perc.dirs[i.perc], 
                         subject.dirs[i.sub], sep='/')
        lc.cms[[i.perc]] = LoadObject(file.name=file.name, dir=cms.path)
        lc.y[[i.perc]] = GetAverageLC(cm.data=lc.cms[[i.perc]], 
                                      x.max=x.max[i.sub], 
                                      PerfFunction=PerfFunction)
        lc.ma[[i.perc]] = MovingAverage(y=lc.y[[i.perc]], 
                                        kernel.width=kernel.width)
      }
      upfront.shuffled.cms[[i.sub]] = lc.cms
      upfront.shuffled.y[[i.sub]] = lc.y
      upfront.shuffled.ma[[i.sub]] = lc.ma
    }
    i.sub = -1
    for (i.perc in seq_along(percs)) {
      perc = percs[i.perc]
      cat('Upfront perc=', perc, '\n', file=con, append=T)
      print.budget(strategy.cms=upfront.shuffled.cms, 
                   strategy.y=upfront.shuffled.y, 
                   baseline.cms=baseline.shuffled.cms, 
                   baseline.remaining=baseline.shuffled.remaining, 
                   i.sub=i.subject, i.perc=i.perc, con=con)
    }
    
    
    # equidistant
    equi.shuffled.cms = list()
    equi.shuffled.y = list()
    equi.shuffled.ma = list()
    for (i.sub in seq_along(subjects)) {
      lc.cms = list()
      lc.y = list()
      lc.ma = list()
      for (i.perc in seq_along(percs)) {
        cms.path = paste(root.dir, 'equi.shuffled', 
                         perc.dirs[i.perc], subject.dirs[i.sub], sep='/')
        lc.cms[[i.perc]] = LoadObject(file.name=file.name, dir=cms.path)
        lc.y[[i.perc]] = GetAverageLC(cm.data=lc.cms[[i.perc]], 
                                      x.max=x.max[i.sub], 
                                      PerfFunction=PerfFunction)
        lc.ma[[i.perc]] = MovingAverage(y=lc.y[[i.perc]], 
                                        kernel.width=kernel.width)
      }
      equi.shuffled.cms[[i.sub]] = lc.cms
      equi.shuffled.y[[i.sub]] = lc.y
      equi.shuffled.ma[[i.sub]] = lc.ma
    }
    i.sub = -1
    for (i.perc in seq_along(percs)) {
      perc = percs[i.perc]
      cat('Equidistant perc=', perc, '\n', file=con, append=T)
      print.budget(strategy.cms=equi.shuffled.cms, 
                   strategy.y=equi.shuffled.y, 
                   baseline.cms=baseline.shuffled.cms, 
                   baseline.remaining=baseline.shuffled.remaining, 
                   i.sub=i.subject, i.perc=i.perc, con=con)
    }
    
    #close(con)
  }

MakeCollage = function(baseline.y, baseline.index,
                       unif.y, unif.index, 
                       exp.1.y, exp.1.index, 
                       exp.5.y, exp.5.index, 
                       upfront.y, upfront.index, 
                       equi.y, equi.index, 
                       x.max, i.sub, ylim, percs,
                       graphics.dir, graphics.file.name, width, height) {
  line.dist = 2.2
  cols = c('black', 'red', 'green', 'blue', 'brown', 'purple')
  
  cutoff.lty = 'solid'
  
  SavePlotBegin(dir=graphics.dir, file.name=graphics.file.name, 
                width=width, height=height) 
  par(mfrow=c(2, 2), mar=c(4,4,2,1))
  for (i.perc in seq_along(percs)) {
    graphics.file.name = paste('p', percs[i.perc], sep='_')
    plot.dir = paste(graphics.dir, 'mozaic', sep='/') 
    main = paste(percs[i.perc], ' budget size', sep='')
       
    # baseline
    cat('  baseline...\n')
    plot(baseline.y, type='l', ylim=ylim, ann=F, col=cols[1])
    mtext(side=2, text='F-Score', line=line.dist)
    mtext(side=1, text='Number of segments seen', line=line.dist)
    mtext(side=3, font=2, text=main, line=line.dist-2)
    lines(x=c(baseline.index, baseline.index), y=ylim, lty=cutoff.lty)
    
    # unif
    cat('  uniform...\n')
    lines(unif.y[[i.perc]], col=cols[2])
    lines(x=c(unif.index[i.perc], unif.index[i.perc]), y=ylim,
          lty=cutoff.lty, col=cols[2])
    
    # equi
    cat('  equidistant...\n')
    lines(equi.y[[i.perc]], col=cols[3])
    lines(x=c(equi.index[i.perc], equi.index[i.perc]), y=ylim,
          lty=cutoff.lty, col=cols[3])
    
    # upfront
    cat('  upfront...\n')
    lines(upfront.y[[i.perc]], col=cols[4])
    lines(x=c(upfront.index[i.perc], upfront.index[i.perc]), y=ylim,
          lty=cutoff.lty, col=cols[4])
    
    # exponential lambda=1
    cat('  lambda=1...\n')
    lines(exp.1.y[[i.perc]], col=cols[5])
    lines(x=c(exp.1.index[i.perc], exp.1.index[i.perc]), y=ylim,
          lty=cutoff.lty, col=cols[5])
    
    # exponential lambda=5
    cat('  lambda=5...\n')
    lines(exp.5.y[[i.perc]], col=cols[6])
    lines(x=c(exp.5.index[i.perc], exp.5.index[i.perc]), y=ylim,
          lty=cutoff.lty, col=cols[6])
    
    
    # legend
    legend.text = c('Base', 'U.R.', 'U.C.', 'U.G.', 
                    expression(paste('Exp', lambda, '=1', sep='')), 
                    expression(paste('Exp', lambda, '=5', sep='')))
    legend(x='bottomright', col=cols, legend=legend.text, lwd=1, ncol=2)    
  }
  SavePlotEnd()
}

#file.name = '3-nn'
file.name = 'c4.5'

graphics.dir = 'graphics/opportunity/budget.5.mcnemar.segment-level'
#graphics.dir = 'graphics/opportunity/budget.5.mcnemar.segment-level.re'

root.dir = 'output/opportunity/budget.5.mcnemar.segment-level'
baseline.unshuffled.dir = paste(root.dir, 
                                'baseline.unshuffled/perc_1', sep='/')
baseline.shuffled.dir = paste(root.dir, 
                              'baseline.shuffled/perc_1', sep='/')

ylim = c(0, 0.8)

#height = 3.5
#width = 7

height = 7
width = 14

pchs = c(0, # empty square
         1, # empty circle
         2, # empty triangle
         5, # empty rhomboid
         16 # full circle
         )
pch.period = 150
pch.starts = c(25, 50, 75, 100, 125)

#percs = c(0.05, 0.10, 0.25, 0.5)
percs = c(0.1, 0.25, 0.5, 0.75)
perc.dirs = rep('', length(percs))
for (i in seq_along(percs)) {
  perc.dirs[i] = paste('perc', percs[i], sep='_')
}

cols = c('red', 'green', 'blue', 'brown')

#distr.dirs = c('equi', 'exp', 'unif', 'upfr')

subjects = c(1, 2, 3, 4)
#subjects = 2
subject.dirs = rep('', length(subjects))
for (i in seq_along(subjects)) {
  subject.dirs[i] = paste('subject', subjects[i], sep='_')
}


kernel.width = 20

PerfFunction = WeightedFMeasure

## load cm's for each subject

 # baselines
 cat('Baseline.unshuffled...\n')
 baseline.unshuffled.cms = list()
 baseline.unshuffled.y = list()
 baseline.unshuffled.ma = list()
 x.max = rep(0, length(subjects))
 for (i in seq_along(subjects)) {
   baseline.path = paste(baseline.unshuffled.dir, subject.dirs[i], sep='/')
   baseline.unshuffled.cms[[i]] = LoadObject(file.name=file.name, dir=baseline.path)  
   x.max[i] = length(baseline.unshuffled.cms[[i]][[1]]$cms)
   baseline.unshuffled.y[[i]] = GetAverageLC(cm.data=baseline.unshuffled.cms[[i]], 
                                            x.max=x.max[i], 
                                            PerfFunction=PerfFunction)
  baseline.unshuffled.ma[[i]] = MovingAverage(y=baseline.unshuffled.y[[i]], 
                                              kernel.width=kernel.width)
}

cat('Baseline.shuffled...\n')
baseline.shuffled.cms = list()
baseline.shuffled.y = list()
baseline.shuffled.ma = list()
x.max = rep(0, length(subjects))
for (i in seq_along(subjects)) {
  baseline.path = paste(baseline.shuffled.dir, subject.dirs[i], sep='/')
  baseline.shuffled.cms[[i]] = LoadObject(file.name=file.name, dir=baseline.path)
  x.max[i] = length(baseline.shuffled.cms[[i]][[1]]$cms)
  baseline.shuffled.y[[i]] = GetAverageLC(cm.data=baseline.shuffled.cms[[i]], 
                                          x.max=x.max[i], 
                                          PerfFunction=PerfFunction)
  baseline.shuffled.ma[[i]] = MovingAverage(y=baseline.shuffled.y[[i]],
                                            kernel.width=kernel.width)
}

# unif
cat('Uniform.unshuffled...\n')
unif.unshuffled.cms = list()
unif.unshuffled.y = list()
unif.unshuffled.ma = list()
for (i.sub in seq_along(subjects)) {
  lc.cms = list()
  lc.y = list()
  lc.ma = list()
  for (i.perc in seq_along(percs)) {
    cms.path = paste(root.dir, 'unif.unshuffled', perc.dirs[i.perc], subject.dirs[i.sub], sep='/')
    lc.cms[[i.perc]] = LoadObject(file.name=file.name, dir=cms.path)
    lc.y[[i.perc]] = GetAverageLC(cm.data=lc.cms[[i.perc]], x.max=x.max[i.sub], 
                                  PerfFunction=PerfFunction)
    lc.ma[[i.perc]] = MovingAverage(y=lc.y[[i.perc]],
                                    kernel.width=kernel.width)
  }
  unif.unshuffled.cms[[i.sub]] = lc.cms
  unif.unshuffled.y[[i.sub]] = lc.y
  unif.unshuffled.ma[[i.sub]] = lc.ma
}
distr = unif.unshuffled.y
for (i.sub in seq_along(subjects)) {
  main = paste('subject=', subjects[i.sub], '; uniform unshuffled', 
               sep='')
  graphics.file.name = paste('subject', subjects[i.sub], sep='_')
  plot.dir = paste(graphics.dir, 'unif.unshuffled', sep='/')
  SavePlotBegin(dir=plot.dir, file.name=graphics.file.name,
                height=height, width=width)
  plot(baseline.unshuffled.y[[i.sub]], type='l', col='black', 
       main=main, ylab='F-Score',
       xlab='Number of training segments seen', ylim=ylim)
  lc.len = length(baseline.unshuffled.y[[i.sub]])
  pch.points = seq(from=pch.starts[1], to=lc.len, by=pch.period)
  #points(x=pch.points, y=baseline.unshuffled.y[[i.sub]][pch.points], 
  #       col='black', pch=pchs[1])
  for (i.perc in seq_along(percs)) {
    lines(distr[[i.sub]][[i.perc]], col=cols[i.perc])
    pch.points = seq(from=pch.starts[i.perc+1], to=lc.len, by=pch.period,)
    #points(x=pch.points, y=distr[[i.sub]][[i.perc]][pch.points], 
    #       col=cols[i.perc], pch=pchs[i.perc+1])
  }
  legend.text = c(1, percs)
  legend(x='bottomright', legend=legend.text, col=c('black', cols), 
         lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
  SavePlotEnd()
}
distr.ma = unif.unshuffled.ma
for (i.sub in seq_along(subjects)) {
  main = paste('subject=', subjects[i.sub], '; uniform unshuffled', 
               sep='')
  graphics.file.name = paste('subject', subjects[i.sub], 'ma', sep='_')
  plot.dir = paste(graphics.dir, 'unif.unshuffled', sep='/')
  SavePlotBegin(dir=plot.dir, file.name=graphics.file.name,
                height=height, width=width)
  plot(baseline.unshuffled.ma[[i.sub]], type='l', col='black', 
       main=main, ylab='F-Score',
       xlab='Number of training segments seen', ylim=ylim)
  lc.len = length(baseline.unshuffled.ma[[i.sub]])
  pch.points = seq(from=pch.starts[1], to=lc.len, by=pch.period)
  #points(x=pch.points, y=baseline.unshuffled.y[[i.sub]][pch.points], 
  #       col='black', pch=pchs[1])
  for (i.perc in seq_along(percs)) {
    lines(distr.ma[[i.sub]][[i.perc]], col=cols[i.perc])
    pch.points = seq(from=pch.starts[i.perc+1], to=lc.len, 
                     by=pch.period,)
    #points(x=pch.points, y=distr[[i.sub]][[i.perc]][pch.points], 
    #       col=cols[i.perc], pch=pchs[i.perc+1])
  }
  legend.text = c(1, percs)
  legend(x='bottomright', legend=legend.text, col=c('black', cols), 
         lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
  SavePlotEnd()
}


cat('Uniform.shuffled...\n')
unif.shuffled.cms = list()
unif.shuffled.y = list()
unif.shuffled.ma = list()
for (i.sub in seq_along(subjects)) {
  lc.cms = list()
  lc.y = list()
  lc.ma = list()
  for (i.perc in seq_along(percs)) {
    cms.path = paste(root.dir, 'unif.shuffled', perc.dirs[i.perc], 
                     subject.dirs[i.sub], sep='/')
    lc.cms[[i.perc]] = LoadObject(file.name=file.name, dir=cms.path)
    lc.y[[i.perc]] = GetAverageLC(cm.data=lc.cms[[i.perc]], 
                                  x.max=x.max[i.sub], 
                                  PerfFunction=PerfFunction)
    lc.ma[[i.perc]] = MovingAverage(y=lc.y[[i.perc]], 
                                    kernel.width=kernel.width)
  }
  unif.shuffled.cms[[i.sub]] = lc.cms
  unif.shuffled.y[[i.sub]] = lc.y
  unif.shuffled.ma[[i.sub]] = lc.ma
}
distr = unif.shuffled.y
for (i.sub in seq_along(subjects)) {
  main = paste('subject=', subjects[i.sub], '; uniform shuffled', 
               sep='')
  graphics.file.name = paste('subject', subjects[i.sub], sep='_')
  plot.dir = paste(graphics.dir, 'unif.shuffled', sep='/')
  SavePlotBegin(dir=plot.dir, file.name=graphics.file.name,
                height=height, width=width)
  plot(baseline.shuffled.y[[i.sub]], type='l', col='black', 
       main=main, ylab='F-Score',
       xlab='Number of training segments seen', ylim=ylim)
  for (i.perc in seq_along(percs)) {
    lines(distr[[i.sub]][[i.perc]], col=cols[i.perc])
  }
  legend.text = c(1, percs)
  legend(x='bottomright', legend=legend.text, col=c('black', cols), 
         lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
  SavePlotEnd()
}
distr.ma = unif.shuffled.ma
for (i.sub in seq_along(subjects)) {
  main = paste('subject=', subjects[i.sub], '; uniform shuffled', 
               sep='')
  graphics.file.name = paste('subject', subjects[i.sub], 'ma', sep='_')
  plot.dir = paste(graphics.dir, 'unif.shuffled', sep='/')
  SavePlotBegin(dir=plot.dir, file.name=graphics.file.name,
                height=height, width=width)
  plot(baseline.shuffled.ma[[i.sub]], type='l', col='black', 
       main=main, ylab='F-Score',
       xlab='Number of training segments seen', ylim=ylim)
  lc.len = length(baseline.shuffled.ma[[i.sub]])
  pch.points = seq(from=pch.starts[1], to=lc.len, by=pch.period)
  #points(x=pch.points, y=baseline.unshuffled.y[[i.sub]][pch.points], 
  #       col='black', pch=pchs[1])
  for (i.perc in seq_along(percs)) {
    lines(distr.ma[[i.sub]][[i.perc]], col=cols[i.perc])
    pch.points = seq(from=pch.starts[i.perc+1], to=lc.len, 
                     by=pch.period,)
    #points(x=pch.points, y=distr[[i.sub]][[i.perc]][pch.points], 
    #       col=cols[i.perc], pch=pchs[i.perc+1])
  }
  legend.text = c(1, percs)
  legend(x='bottomright', legend=legend.text, col=c('black', cols), 
         lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
  SavePlotEnd()
}


# exponential
cat('Exponential...\n')
lambdas = c(1, 5)

exp.unshuffled.cms = list()
exp.unshuffled.y = list()
exp.unshuffled.ma = list()

exp.shuffled.cms = list()
exp.shuffled.y = list()
exp.shuffled.ma = list()

for (i.lambda in seq_along(lambdas)) {
  lambda = lambdas[i.lambda]
  
  exp.unshuffled.cms[[i.lambda]] = list()
  exp.unshuffled.y[[i.lambda]] = list()  
  exp.unshuffled.ma[[i.lambda]] = list()
  for (i.sub in seq_along(subjects)) {
    lc.cms = list()
    lc.y = list()
    lc.ma = list()
    for (i.perc in seq_along(percs)) {
      exp.dir = paste('exp_', lambda, '.unshuffled', sep='')
      cms.path = paste(root.dir, exp.dir, perc.dirs[i.perc], 
                       subject.dirs[i.sub], sep='/')
      lc.cms[[i.perc]] = LoadObject(file.name=file.name, dir=cms.path)
      lc.y[[i.perc]] = GetAverageLC(cm.data=lc.cms[[i.perc]], 
                                    x.max=x.max[i.sub], 
                                    PerfFunction=PerfFunction)
      lc.ma[[i.perc]] = MovingAverage(y=lc.y[[i.perc]], 
                                      kernel.width=kernel.width)
    }
    exp.unshuffled.cms[[i.lambda]][[i.sub]] = lc.cms
    exp.unshuffled.y[[i.lambda]][[i.sub]] = lc.y
    exp.unshuffled.ma[[i.lambda]][[i.sub]] = lc.ma
  }
  distr = exp.unshuffled.y[[i.lambda]]
  for (i.sub in seq_along(subjects)) {
    main = paste('subject=', subjects[i.sub], 
                 '; exponential.unshuffled lambda=',
                 lambda, sep='')
    graphics.file.name = paste('s.', subjects[i.sub], '.exp', sep='')
    exp.dir = paste('exp_', lambda, '.unshuffled', sep='')
    plot.dir = paste(graphics.dir, exp.dir, sep='/')
    SavePlotBegin(dir=plot.dir, file.name=graphics.file.name,
                  height=height, width=width)
    plot(baseline.unshuffled.y[[i.sub]], type='l', col='black', 
         main=main, ylab='F-Score',
         xlab='Number of training segments seen', ylim=ylim)
    for (i.perc in seq_along(percs)) {
      lines(distr[[i.sub]][[i.perc]], col=cols[i.perc])
    }
    legend.text = c(1, percs)
    legend(x='bottomright', legend=legend.text, col=c('black', cols), 
           lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
    SavePlotEnd()
  }
  distr.ma = exp.unshuffled.ma[[i.lambda]]
  for (i.sub in seq_along(subjects)) {
    main = paste('subject=', subjects[i.sub], 
                 '; exponential.unshuffled lambda=',
                 lambda, sep='')
    graphics.file.name = paste('s.', subjects[i.sub], '.exp_ma', 
                               sep='')
    exp.dir = paste('exp_', lambda, '.unshuffled', sep='')
    plot.dir = paste(graphics.dir, exp.dir, sep='/')
    SavePlotBegin(dir=plot.dir, file.name=graphics.file.name,
                  height=height, width=width)
    plot(baseline.unshuffled.ma[[i.sub]], type='l', col='black', 
         main=main, ylab='F-Score',
         xlab='Number of training segments seen', ylim=ylim)
    for (i.perc in seq_along(percs)) {
      lines(distr.ma[[i.sub]][[i.perc]], col=cols[i.perc])
    }
    legend.text = c(1, percs)
    legend(x='bottomright', legend=legend.text, col=c('black', cols), 
           lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
    SavePlotEnd()
  }
  
  
  exp.shuffled.cms[[i.lambda]] = list()
  exp.shuffled.y[[i.lambda]] = list()  
  exp.shuffled.ma[[i.lambda]] = list()
  for (i.sub in seq_along(subjects)) {
    lc.cms = list()
    lc.y = list()
    for (i.perc in seq_along(percs)) {
      exp.dir = paste('exp_', lambda, '.shuffled', sep='')
      cms.path = paste(root.dir, exp.dir, perc.dirs[i.perc], 
                       subject.dirs[i.sub], sep='/')
      lc.cms[[i.perc]] = LoadObject(file.name=file.name, dir=cms.path)
      lc.y[[i.perc]] = GetAverageLC(cm.data=lc.cms[[i.perc]], 
                                    x.max=x.max[i.sub], 
                                    PerfFunction=PerfFunction)
      lc.ma[[i.perc]] = MovingAverage(y=lc.y[[i.perc]], 
                                      kernel.width=kernel.width)
    }
    exp.shuffled.cms[[i.lambda]][[i.sub]] = lc.cms
    exp.shuffled.y[[i.lambda]][[i.sub]] = lc.y
    exp.shuffled.ma[[i.lambda]][[i.sub]] = lc.ma
  }
  distr = exp.shuffled.y[[i.lambda]]
  for (i.sub in seq_along(subjects)) {
    main = paste('subject=', subjects[i.sub], 
                 '; exponential.shuffled lambda=',
                 lambda, sep='')
    graphics.file.name = paste('s.', subjects[i.sub], '.exp', sep='')
    exp.dir = paste('exp_', lambda, '.shuffled', sep='')
    plot.dir = paste(graphics.dir, exp.dir, sep='/')
    SavePlotBegin(dir=plot.dir, file.name=graphics.file.name,
                  height=height, width=width)
    plot(baseline.shuffled.y[[i.sub]], type='l', col='black', 
         main=main, ylab='F-Score',
         xlab='Number of training segments seen', ylim=ylim)
    for (i.perc in seq_along(percs)) {
      lines(distr[[i.sub]][[i.perc]], col=cols[i.perc])
    }
    legend.text = c(1, percs)
    legend(x='bottomright', legend=legend.text, col=c('black', cols), 
           lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
    SavePlotEnd()
  }  
  distr.ma = exp.shuffled.ma[[i.lambda]]
  for (i.sub in seq_along(subjects)) {
    main = paste('subject=', subjects[i.sub], 
                 '; exponential.shuffled lambda=',
                 lambda, sep='')
    graphics.file.name = paste('s.', subjects[i.sub], '.exp_ma', 
                               sep='')
    exp.dir = paste('exp_', lambda, '.shuffled', sep='')
    plot.dir = paste(graphics.dir, exp.dir, sep='/')
    SavePlotBegin(dir=plot.dir, file.name=graphics.file.name,
                  height=height, width=width)
    plot(baseline.shuffled.ma[[i.sub]], type='l', col='black', 
         main=main, ylab='F-Score',
         xlab='Number of training segments seen', ylim=ylim)
    for (i.perc in seq_along(percs)) {
      lines(distr.ma[[i.sub]][[i.perc]], col=cols[i.perc])
    }
    legend.text = c(1, percs)
    legend(x='bottomright', legend=legend.text, col=c('black', cols), 
           lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
    SavePlotEnd()
  }
  
}

# upfront
upfront.unshuffled.cms = list()
upfront.unshuffled.y = list()
upfront.unshuffled.ma = list()
for (i.sub in seq_along(subjects)) {
  lc.cms = list()
  lc.y = list()
  lc.ma = list()
  for (i.perc in seq_along(percs)) {
    cms.path = paste(root.dir, 'upfront.unshuffled', 
                     perc.dirs[i.perc], subject.dirs[i.sub], sep='/')
    lc.cms[[i.perc]] = LoadObject(file.name=file.name, dir=cms.path)
    lc.y[[i.perc]] = GetAverageLC(cm.data=lc.cms[[i.perc]], 
                                  x.max=x.max[i.sub], 
                                  PerfFunction=PerfFunction)
    lc.ma[[i.perc]] = MovingAverage(y=lc.y[[i.perc]], 
                                    kernel.width=kernel.width)
  }
  upfront.unshuffled.cms[[i.sub]] = lc.cms
  upfront.unshuffled.y[[i.sub]] = lc.y
  upfront.unshuffled.ma[[i.sub]] = lc.ma
}
distr = upfront.unshuffled.y
for (i.sub in seq_along(subjects)) {
  main = paste('subject=', subjects[i.sub], '; upfront unshuffled', 
               sep='')
  graphics.file.name = paste('subject', subjects[i.sub], sep='_')
  plot.dir = paste(graphics.dir, 'upfr.unshuffled', sep='/')
  SavePlotBegin(dir=plot.dir, file.name=graphics.file.name,
                height=height, width=width)
  plot(baseline.unshuffled.y[[i.sub]], type='l', col='black', 
       main=main, ylab='F-Score',
       xlab='Number of training segments seen', ylim=ylim)
  for (i.perc in seq_along(percs)) {
    lines(distr[[i.sub]][[i.perc]], col=cols[i.perc])
  }
  legend.text = c(1, percs)
  legend(x='bottomright', legend=legend.text, col=c('black', cols), 
         lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
  SavePlotEnd()
}
distr.ma = upfront.unshuffled.ma
for (i.sub in seq_along(subjects)) {
  main = paste('subject=', subjects[i.sub], '; upfront unshuffled', 
               sep='')
  graphics.file.name = paste('subject', subjects[i.sub], 'ma', 
                             sep='_')
  plot.dir = paste(graphics.dir, 'upfr.unshuffled', sep='/')
  SavePlotBegin(dir=plot.dir, file.name=graphics.file.name,
                height=height, width=width)
  plot(baseline.unshuffled.ma[[i.sub]], type='l', col='black', 
       main=main, ylab='F-Score',
       xlab='Number of training segments seen', ylim=ylim)
  for (i.perc in seq_along(percs)) {
    lines(distr.ma[[i.sub]][[i.perc]], col=cols[i.perc])
  }
  legend.text = c(1, percs)
  legend(x='bottomright', legend=legend.text, col=c('black', cols), 
         lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
  SavePlotEnd()
}

upfront.shuffled.cms = list()
upfront.shuffled.y = list()
upfront.shuffled.ma = list()
for (i.sub in seq_along(subjects)) {
  lc.cms = list()
  lc.y = list()
  lc.ma = list()
  for (i.perc in seq_along(percs)) {
    cms.path = paste(root.dir, 'upfront.shuffled', perc.dirs[i.perc], 
                     subject.dirs[i.sub], sep='/')
    lc.cms[[i.perc]] = LoadObject(file.name=file.name, dir=cms.path)
    lc.y[[i.perc]] = GetAverageLC(cm.data=lc.cms[[i.perc]], 
                                  x.max=x.max[i.sub], 
                                  PerfFunction=PerfFunction)
    lc.ma[[i.perc]] = MovingAverage(y=lc.y[[i.perc]], 
                                    kernel.width=kernel.width)
  }
  upfront.shuffled.cms[[i.sub]] = lc.cms
  upfront.shuffled.y[[i.sub]] = lc.y
  upfront.shuffled.ma[[i.sub]] = lc.ma
}
distr = upfront.shuffled.y
for (i.sub in seq_along(subjects)) {
  main = paste('subject=', subjects[i.sub], '; upfront shuffled', 
               sep='')
  graphics.file.name = paste('subject', subjects[i.sub], sep='_')
  plot.dir = paste(graphics.dir, 'upfr.shuffled', sep='/')
  SavePlotBegin(dir=plot.dir, file.name=graphics.file.name,
                height=height, width=width)
  plot(baseline.shuffled.y[[i.sub]], type='l', col='black', 
       main=main, ylab='F-Score',
       xlab='Number of training segments seen', ylim=ylim)
  for (i.perc in seq_along(percs)) {
    lines(distr[[i.sub]][[i.perc]], col=cols[i.perc])
  }
  legend.text = c(1, percs)
  legend(x='bottomright', legend=legend.text, col=c('black', cols), 
         lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
  SavePlotEnd()
}
distr.ma = upfront.shuffled.ma
for (i.sub in seq_along(subjects)) {
  main = paste('subject=', subjects[i.sub], '; upfront shuffled', 
               sep='')
  graphics.file.name = paste('subject', subjects[i.sub], 'ma', sep='_')
  plot.dir = paste(graphics.dir, 'upfr.shuffled', sep='/')
  SavePlotBegin(dir=plot.dir, file.name=graphics.file.name,
                height=height, width=width)
  plot(baseline.shuffled.ma[[i.sub]], type='l', col='black', 
       main=main, ylab='F-Score',
       xlab='Number of training segments seen', ylim=ylim)
  for (i.perc in seq_along(percs)) {
    lines(distr.ma[[i.sub]][[i.perc]], col=cols[i.perc])
  }
  legend.text = c(1, percs)
  legend(x='bottomright', legend=legend.text, col=c('black', cols), 
         lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
  SavePlotEnd()
}

# equidistant
equi.unshuffled.cms = list()
equi.unshuffled.y = list()
equi.unshuffled.ma = list()
for (i.sub in seq_along(subjects)) {
  lc.cms = list()
  lc.y = list()
  lc.ma = list()
  for (i.perc in seq_along(percs)) {
    cms.path = paste(root.dir, 'equi.unshuffled', 
                     perc.dirs[i.perc], subject.dirs[i.sub], sep='/')
    lc.cms[[i.perc]] = LoadObject(file.name=file.name, dir=cms.path)
    lc.y[[i.perc]] = GetAverageLC(cm.data=lc.cms[[i.perc]], 
                                  x.max=x.max[i.sub], 
                                  PerfFunction=PerfFunction)
    lc.ma[[i.perc]] = MovingAverage(y=lc.y[[i.perc]], 
                                    kernel.width=kernel.width)
  }
  equi.unshuffled.cms[[i.sub]] = lc.cms
  equi.unshuffled.y[[i.sub]] = lc.y
  equi.unshuffled.ma[[i.sub]] = lc.ma
}
distr = equi.unshuffled.y
for (i.sub in seq_along(subjects)) {
  main = paste('subject=', subjects[i.sub], 
               '; equidistant.unshuffled', sep='')
  graphics.file.name = paste('s.', subjects[i.sub], '.equi', sep='')
  plot.dir = paste(graphics.dir, 'equi.unshuffled', sep='/')
  SavePlotBegin(dir=plot.dir, file.name=graphics.file.name,
                height=height, width=width)
  plot(baseline.unshuffled.y[[i.sub]], type='l', col='black', 
       main=main, ylab='F-Score',
       xlab='Number of training segments seen', ylim=ylim)
  for (i.perc in seq_along(percs)) {
    lines(distr[[i.sub]][[i.perc]], col=cols[i.perc])
  }
  legend.text = c(1, percs)
  legend(x='bottomright', legend=legend.text, col=c('black', cols), 
         lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
  SavePlotEnd()
}
distr.ma = equi.unshuffled.ma
for (i.sub in seq_along(subjects)) {
  main = paste('subject=', subjects[i.sub], 
               '; equidistant.unshuffled', sep='')
  graphics.file.name = paste('s.', subjects[i.sub], '.equi_ma', sep='')
  plot.dir = paste(graphics.dir, 'equi.unshuffled', sep='/')
  SavePlotBegin(dir=plot.dir, file.name=graphics.file.name,
                height=height, width=width)
  plot(baseline.unshuffled.ma[[i.sub]], type='l', col='black', 
       main=main, ylab='F-Score',
       xlab='Number of training segments seen', ylim=ylim)
  for (i.perc in seq_along(percs)) {
    lines(distr.ma[[i.sub]][[i.perc]], col=cols[i.perc])
  }
  legend.text = c(1, percs)
  legend(x='bottomright', legend=legend.text, col=c('black', cols), 
         lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
  SavePlotEnd()
}

equi.shuffled.cms = list()
equi.shuffled.y = list()
equi.shuffled.ma = list()
for (i.sub in seq_along(subjects)) {
  lc.cms = list()
  lc.y = list()
  lc.ma = list()
  for (i.perc in seq_along(percs)) {
    cms.path = paste(root.dir, 'equi.shuffled', 
                     perc.dirs[i.perc], subject.dirs[i.sub], sep='/')
    lc.cms[[i.perc]] = LoadObject(file.name=file.name, dir=cms.path)
    lc.y[[i.perc]] = GetAverageLC(cm.data=lc.cms[[i.perc]], 
                                  x.max=x.max[i.sub], 
                                  PerfFunction=PerfFunction)
    lc.ma[[i.perc]] = MovingAverage(y=lc.y[[i.perc]], 
                                    kernel.width=kernel.width)
  }
  equi.shuffled.cms[[i.sub]] = lc.cms
  equi.shuffled.y[[i.sub]] = lc.y
  equi.shuffled.ma[[i.sub]] = lc.ma
}
distr = equi.shuffled.y
for (i.sub in seq_along(subjects)) {
  main = paste('subject=', subjects[i.sub], 
               '; equidistant.shuffled', sep='')
  graphics.file.name = paste('s.', subjects[i.sub], '.equi', sep='')
  plot.dir = paste(graphics.dir, 'equi.shuffled', sep='/')
  SavePlotBegin(dir=plot.dir, file.name=graphics.file.name,
                height=height, width=width)
  plot(baseline.shuffled.y[[i.sub]], type='l', col='black', 
       main=main, ylab='F-Score',
       xlab='Number of training segments seen', ylim=ylim)
  for (i.perc in seq_along(percs)) {
    lines(distr[[i.sub]][[i.perc]], col=cols[i.perc])
  }
  legend.text = c(1, percs)
  legend(x='bottomright', legend=legend.text, col=c('black', cols), 
         lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
  SavePlotEnd()
}
distr.ma = equi.shuffled.ma
for (i.sub in seq_along(subjects)) {
  main = paste('subject=', subjects[i.sub], 
               '; equidistant.shuffled', sep='')
  graphics.file.name = paste('s.', subjects[i.sub], '.equi_ma', sep='')
  plot.dir = paste(graphics.dir, 'equi.shuffled', sep='/')
  SavePlotBegin(dir=plot.dir, file.name=graphics.file.name,
                height=height, width=width)
  plot(baseline.shuffled.ma[[i.sub]], type='l', col='black', 
       main=main, ylab='F-Score',
       xlab='Number of training segments seen', ylim=ylim)
  for (i.perc in seq_along(percs)) {
    lines(distr.ma[[i.sub]][[i.perc]], col=cols[i.perc])
  }
  legend.text = c(1, percs)
  legend(x='bottomright', legend=legend.text, col=c('black', cols), 
         lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
  SavePlotEnd()
}

# ### 'sexy' plot
i.lambda = which(lambdas == 5)
i.sub = which(subjects == 1)
i.perc = which(percs == 0.25)
i.rep = 1
cm.data.1.rep = exp.unshuffled.cms[[i.lambda]][[i.sub]][[i.perc]][[i.rep]]
#main = paste('subject=', subjects[i.sub], '; ', 
#             percs[i.perc], '%; lambda=', 
#             lambdas[i.lambda], sep='')
main=NULL
plot.file.name = 'annotation.points'
dir = graphics.dir
PlotAnnotationTimes(cm.data.1.rep=cm.data.1.rep, x.max=x.max[i.sub], 
                    PerfFunction=PerfFunction, col=cols[i.perc], 
                    main=main, file.name=plot.file.name, dir=dir,
                    ylim=ylim, height=height/1.5, width=width/1.5)

# #### collage - unshuffled
# i.lambda = which(lambdas == 1)
# i.sub = which(subjects == 3)
# 
# ## set par
# #dev.off()
# #SavePlotBegin(dir=graphics.dir, file.name='collage.unshuffled', 
# #              width=width, height=height)
# #old.par = par(mfrow=c(2, 2), mar = c(4,4,1,1), oma=c(2,2,2,2))
# 
# line.dist = 2.2
# 
# # unif
# distr = unif.unshuffled.y
# main = 'Uniform Random'
# graphics.file.name = paste('subject', subjects[i.sub], sep='_')
# plot.dir = paste(graphics.dir, 'unif.unshuffled', sep='/')
# plot(baseline.unshuffled.y[[i.sub]], type='l', col='black', 
#      main=main, ylim=ylim, ann=F)
# mtext(side=2, text='F-Score', line=line.dist)
# mtext(side=1, text='Number of segments seen', line=line.dist)
# lc.len = length(baseline.unshuffled.y[[i.sub]])
# for (i.perc in seq_along(percs)) {
#   lines(distr[[i.sub]][[i.perc]], col=cols[i.perc])
# }
# legend.text = c(1, percs)
# #legend(x='bottomright', legend=legend.text, col=c('black', cols), 
# #       lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
# 
# # equi
# distr = equi.unshuffled.y
# main = 'Uniform Constant'
# graphics.file.name = paste('subject', subjects[i.sub], sep='_')
# plot.dir = paste(graphics.dir, 'unif.unshuffled', sep='/')
# plot(baseline.unshuffled.y[[i.sub]], type='l', col='black', 
#      main=main, ylim=ylim, ann=F)
# mtext(side=2, text='F-Score', line=line.dist)
# mtext(side=1, text='Number of segments seen', line=line.dist)
# lc.len = length(baseline.unshuffled.y[[i.sub]])
# for (i.perc in seq_along(percs)) {
#   lines(distr[[i.sub]][[i.perc]], col=cols[i.perc])
# }
# legend.text = c(1, percs)
# #legend(x='bottomright', legend=legend.text, col=c('black', cols), 
# #       lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
# 
# # upfr
# distr = upfront.unshuffled.y
# main = 'Upfront Greedy'
# graphics.file.name = paste('subject', subjects[i.sub], sep='_')
# plot.dir = paste(graphics.dir, 'unif.unshuffled', sep='/')
# plot(baseline.unshuffled.y[[i.sub]], type='l', col='black', 
#      main=main, ylim=ylim, ann=F)
# mtext(side=2, text='F-Score', line=line.dist)
# mtext(side=1, text='Number of segments seen', line=line.dist)
# lc.len = length(baseline.unshuffled.y[[i.sub]])
# for (i.perc in seq_along(percs)) {
#   lines(distr[[i.sub]][[i.perc]], col=cols[i.perc])
# }
# legend.text = c(1, percs)
# #legend(x='bottomright', legend=legend.text, col=c('black', cols), 
# #       lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
# 
# # exp
# distr = exp.unshuffled.y[[i.lambda]]
# main = 'Exponential'
# graphics.file.name = paste('subject', subjects[i.sub], sep='_')
# plot.dir = paste(graphics.dir, 'unif.unshuffled', sep='/')
# plot(baseline.unshuffled.y[[i.sub]], type='l', col='black', 
#      main=main, ylim=ylim, ann=F)
# mtext(side=2, text='F-Score', line=line.dist)
# mtext(side=1, text='Number of segments seen', line=line.dist)
# lc.len = length(baseline.unshuffled.y[[i.sub]])
# for (i.perc in seq_along(percs)) {
#   lines(distr[[i.sub]][[i.perc]], col=cols[i.perc])
# }
# legend.text = c(1, percs)
# legend(x='bottomright', legend=legend.text, col=c('black', cols), 
#        lwd=1, bg='white', cex=1, ncol=3, y.intersp=0.8)
# SavePlotEnd()

# unshuffled collage
#i.sub = which(subjects == 3)
for (i.sub in seq_along(subjects)) {  
  baseline = baseline.unshuffled.cms[[i.sub]]
  unif = unif.unshuffled.cms[[i.sub]]
  exp.1 = exp.unshuffled.cms[[which(lambdas == 1)]][[i.sub]]
  exp.5 = exp.unshuffled.cms[[which(lambdas == 5)]][[i.sub]]
  upfront = upfront.unshuffled.cms[[i.sub]]
  equi = equi.unshuffled.cms[[i.sub]]
  
  baseline.y = baseline.unshuffled.y[[i.sub]]
  baseline.ma = baseline.unshuffled.ma[[i.sub]]
  
  unif.y = unif.unshuffled.y[[i.sub]]
  unif.ma = unif.unshuffled.ma[[i.sub]]
  
  exp.1.y = exp.unshuffled.y[[which(lambdas == 1)]][[i.sub]]
  exp.1.ma = exp.unshuffled.ma[[which(lambdas == 1)]][[i.sub]]
  
  exp.5.y = exp.unshuffled.y[[which(lambdas == 5)]][[i.sub]]
  exp.5.ma = exp.unshuffled.ma[[which(lambdas == 5)]][[i.sub]]
  
  upfront.y = upfront.unshuffled.y[[i.sub]]
  upfront.ma = upfront.unshuffled.ma[[i.sub]]
  
  equi.y = equi.unshuffled.y[[i.sub]]
  equi.ma = equi.unshuffled.ma[[i.sub]]
  
  # baseline
  baseline.index = McNemar.within.avg.index(baseline)
  baseline.end = McNemar.avg.end(baseline, baseline)
  baseline.budget = BudgetSpentSoFar(cms=baseline, 
                                     mcnemar.index=baseline.index)
  
  # unif
  unif.index = sapply(X=unif, FUN=McNemar.within.avg.index)
  unif.end = sapply(X=unif, FUN=McNemar.avg.end, cms.2=baseline)
  unif.budget = rep(0, length(percs))
  for (i in seq_along(percs)) {
    unif.budget[i] = BudgetSpentSoFar(cms=unif[[i]], 
                                      mcnemar.index=unif.index[i])
  }
  
  # exp.1
  exp.1.index = sapply(X=exp.1, FUN=McNemar.within.avg.index)
  exp.1.end = sapply(X=exp.1, FUN=McNemar.avg.end, cms.2=baseline)
  exp.1.budget = rep(0, length(percs))
  for (i in seq_along(percs)) {
    exp.1.budget[i] = BudgetSpentSoFar(cms=exp.1[[i]], 
                                       mcnemar.index=exp.1.index[i])
  }
  
  # exp.5
  exp.5.index = sapply(X=exp.5, FUN=McNemar.within.avg.index)
  exp.5.end = sapply(X=exp.5, FUN=McNemar.avg.end, cms.2=baseline)
  exp.5.budget = rep(0, length(percs))
  for (i in seq_along(percs)) {
    exp.5.budget[i] = BudgetSpentSoFar(cms=exp.5[[i]], 
                                       mcnemar.index=exp.1.index[i])
  }
  
  # upfront
  upfront.index = sapply(X=upfront, FUN=McNemar.within.avg.index)
  upfront.end = sapply(X=upfront, FUN=McNemar.avg.end, cms.2=baseline)
  upfront.budget = rep(0, length(percs))
  for (i in seq_along(percs)) {
    upfront.budget[i] = BudgetSpentSoFar(cms=upfront[[i]], 
                                         mcnemar.index=upfront.index[i])
  }
  
  # equi
  equi.index = sapply(X=equi, FUN=McNemar.within.avg.index)
  equi.end = sapply(X=equi, FUN=McNemar.avg.end, cms.2=baseline)
  equi.budget = rep(0, length(percs))
  for (i in seq_along(percs)) {
    equi.budget[i] = BudgetSpentSoFar(cms=equi[[i]], 
                                      mcnemar.index=equi.index[i])
  }
  
  graphics.file.name = paste('collage.unshuffled.raw.subject', i.sub,
                             sep='.')
  MakeCollage(baseline.y=baseline.y, baseline.index=baseline.index, 
              unif.y=unif.y, unif.index=unif.index,
              exp.1.y=exp.1.y, exp.1.index=exp.1.index,
              exp.5.y=exp.5.y, exp.5.index=exp.5.index,
              upfront.y=upfront.y, upfront.index=upfront.index,
              equi.y=equi.y, equi.index=equi.index,
              i.sub=i.sub, ylim=ylim, percs=percs, 
              graphics.dir=paste(graphics.dir, 'collage', 
                                 'unshuffled', sep='/'),
              graphics.file.name=graphics.file.name,
              width=width, height=height)
  
  graphics.file.name = paste('collage.unshuffled.smooth.subject', i.sub,
                             sep='.')
  MakeCollage(baseline.y=baseline.ma, baseline.index=baseline.index, 
              unif.y=unif.ma, unif.index=unif.index,
              exp.1.y=exp.1.ma, exp.1.index=exp.1.index,
              exp.5.y=exp.5.ma, exp.5.index=exp.5.index,
              upfront.y=upfront.ma, upfront.index=upfront.index,
              equi.y=equi.ma, equi.index=equi.index,
              i.sub=i.sub, ylim=ylim, percs=percs, 
              graphics.dir=paste(graphics.dir, 'collage', 
                                 'unshuffled', sep='/'),
              graphics.file.name=graphics.file.name,
              width=width, height=height)
  
  # shuffled collage
  baseline = baseline.shuffled.cms[[i.sub]]
  unif = unif.shuffled.cms[[i.sub]]
  exp.1 = exp.shuffled.cms[[which(lambdas == 1)]][[i.sub]]
  exp.5 = exp.shuffled.cms[[which(lambdas == 5)]][[i.sub]]
  upfront = upfront.shuffled.cms[[i.sub]]
  equi = equi.shuffled.cms[[i.sub]]
  
  baseline.y = baseline.shuffled.y[[i.sub]]
  baseline.ma = baseline.shuffled.ma[[i.sub]]
  
  unif.y = unif.shuffled.y[[i.sub]]
  unif.ma = unif.shuffled.ma[[i.sub]]
  
  exp.1.y = exp.shuffled.y[[which(lambdas == 1)]][[i.sub]]
  exp.1.ma = exp.shuffled.ma[[which(lambdas == 1)]][[i.sub]]
  
  exp.5.y = exp.shuffled.y[[which(lambdas == 5)]][[i.sub]]
  exp.5.ma = exp.shuffled.ma[[which(lambdas == 5)]][[i.sub]]
  
  upfront.y = upfront.shuffled.y[[i.sub]]
  upfront.ma = upfront.shuffled.ma[[i.sub]]
  
  equi.y = equi.shuffled.y[[i.sub]]
  equi.ma = equi.shuffled.ma[[i.sub]]
  
  # baseline
  baseline.index = McNemar.within.avg.index(baseline)
  baseline.end = McNemar.avg.end(baseline, baseline)
  baseline.budget = BudgetSpentSoFar(cms=baseline, 
                                     mcnemar.index=baseline.index)
  
  # unif
  unif.index = sapply(X=unif, FUN=McNemar.within.avg.index)
  unif.end = sapply(X=unif, FUN=McNemar.avg.end, cms.2=baseline)
  unif.budget = rep(0, length(percs))
  for (i in seq_along(percs)) {
    unif.budget[i] = BudgetSpentSoFar(cms=unif[[i]], 
                                      mcnemar.index=unif.index[i])
  }
  
  # exp.1
  exp.1.index = sapply(X=exp.1, FUN=McNemar.within.avg.index)
  exp.1.end = sapply(X=exp.1, FUN=McNemar.avg.end, cms.2=baseline)
  exp.1.budget = rep(0, length(percs))
  for (i in seq_along(percs)) {
    exp.1.budget[i] = BudgetSpentSoFar(cms=exp.1[[i]], 
                                       mcnemar.index=exp.1.index[i])
  }
  
  # exp.5
  exp.5.index = sapply(X=exp.5, FUN=McNemar.within.avg.index)
  exp.5.end = sapply(X=exp.5, FUN=McNemar.avg.end, cms.2=baseline)
  exp.5.budget = rep(0, length(percs))
  for (i in seq_along(percs)) {
    exp.5.budget[i] = BudgetSpentSoFar(cms=exp.5[[i]], 
                                       mcnemar.index=exp.1.index[i])
  }
  
  # upfront
  upfront.index = sapply(X=upfront, FUN=McNemar.within.avg.index)
  upfront.end = sapply(X=upfront, FUN=McNemar.avg.end, cms.2=baseline)
  upfront.budget = rep(0, length(percs))
  for (i in seq_along(percs)) {
    upfront.budget[i] = BudgetSpentSoFar(cms=upfront[[i]], 
                                         mcnemar.index=upfront.index[i])
  }
  
  # equi
  equi.index = sapply(X=equi, FUN=McNemar.within.avg.index)
  equi.end = sapply(X=equi, FUN=McNemar.avg.end, cms.2=baseline)
  equi.budget = rep(0, length(percs))
  for (i in seq_along(percs)) {
    equi.budget[i] = BudgetSpentSoFar(cms=equi[[i]], 
                                      mcnemar.index=equi.index[i])
  }
  
  graphics.file.name = paste('collage.shuffled.raw.subject', i.sub,
                             sep='.')
  MakeCollage(baseline.y=baseline.y, baseline.index=baseline.index, 
              unif.y=unif.y, unif.index=unif.index,
              exp.1.y=exp.1.y, exp.1.index=exp.1.index,
              exp.5.y=exp.5.y, exp.5.index=exp.5.index,
              upfront.y=upfront.y, upfront.index=upfront.index,
              equi.y=equi.y, equi.index=equi.index,
              i.sub=i.sub, ylim=ylim, percs=percs, 
              graphics.dir=paste(graphics.dir, 'collage', 
                                 'shuffled', sep='/'),
              graphics.file.name=graphics.file.name,
              width=width, height=height)
  
  graphics.file.name = paste('collage.shuffled.smooth.subject', i.sub,
                             sep='.')
  MakeCollage(baseline.y=baseline.ma, baseline.index=baseline.index, 
              unif.y=unif.ma, unif.index=unif.index,
              exp.1.y=exp.1.ma, exp.1.index=exp.1.index,
              exp.5.y=exp.5.ma, exp.5.index=exp.5.index,
              upfront.y=upfront.ma, upfront.index=upfront.index,
              equi.y=equi.ma, equi.index=equi.index,
              i.sub=i.sub, ylim=ylim, percs=percs, 
              graphics.dir=paste(graphics.dir, 'collage', 
                                 'shuffled', sep='/'),
              graphics.file.name=graphics.file.name,
              width=width, height=height)
  
  text.file.name = paste('output', i.sub, 'txt', sep='.')
  cat('Printing table data for subject', i.sub, '...\n')
  #PrintBudgetTableData(baseline.unshuffled.dir=baseline.unshuffled.dir, 
  #                     baseline.shuffled.dir=baseline.shuffled.dir, 
  #                     graphics.dir=graphics.dir, subject.dirs=subject.dirs, 
  #                     perc.dirs=perc.dirs, i.sub=i.sub, percs=percs,
  #                     text.file.name=text.file.name)
}
