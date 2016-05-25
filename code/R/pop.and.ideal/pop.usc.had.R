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

source('../ml.utils.R')

setwd('~/R/sf_ALTLAR/R/al.usc.had')



## B.NB
classifier.name = 'weka/classifiers/meta/Bagging'
weak.classifier.name = 'weka.classifiers.bayes.NaiveBayes'
num.iterations = 30
weka.control = Weka_control(W=weak.classifier.name, 
                            I=num.iterations)

load(file='~/R/sf_ALTLAR/output/organize.usc.had/subjects.features.Rdata')
#load(file='~/R/sf_ALTLAR/output/organize.pamap.2.protocol/subjects.features.Rdata')
subjects.features = obj

fms = c()
for (subject.number in seq_along(subjects.features)) {
  train.data = subjects.features[[subject.number]]$data
  test.data = train.data[0, ]
  
  for (s.f in subjects.features[-subject.number]) {
    test.data = rbind(test.data, s.f$data)
  }
  
  all.data = rbind(train.data, test.data)
  class.labels = sort(unique(as.character(all.data$activity)))
  
  cm = EvaluateModel(classifier.name=classifier.name, weka.control=weka.control, 
                     train.data=train.data, test.data=test.data, class.labels=class.labels)
  fm = WeightedFMeasure(cm=cm)
  cat(fm, '\n')
  fms = c(fm, fms)
}

cat('pop avg fm =', mean(fms), '\n\n')
