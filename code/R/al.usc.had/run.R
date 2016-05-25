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
setwd('~/R/sf_ALTLAR/R/al.usc.had')

source('al.R')

library(doRedis)


## TODO
# load preprocessed data from disk

queue.name='al.online.queue'
registerDoRedis(queue=queue.name)

## B.NB
classifier.name = 'weka/classifiers/meta/Bagging'
weak.classifier.name = 'weka.classifiers.bayes.NaiveBayes'
num.iterations = 30
weka.control = Weka_control(W=weak.classifier.name, 
                            I=num.iterations)

#load(file='~/R/sf_ALTLAR/output/organize.usc.had/subjects.features.Rdata')
#subjects.features = subjects.features[-2]

load(file='~/R/sf_ALTLAR/output/organize.pamap.2.protocol/subjects.features.Rdata')

subjects.features = obj

#class.labels = GetAllLabels(subjects.features=subjects.features)

num.folds = 10


max.instances = 200
#max.instances = 10
min.instances = 3

#repetitions = 20
#num.repetitions = 10
num.repetitions = 20

#gamma = 6
gamma = 2


online.heuristic = B.margin

#####subjects.features = subjects.features[-2]
#subjects.features = subjects.features[c(1, 3, 4, 5)]
#subjects.features = subjects.features[-2]
#subjects.features[[1]]$data = subjects.features[[1]]$data[1:100, ]
l.al = SimulateForAllSubjects(subjects.features=subjects.features, gamma=gamma,
                              classifier.name=classifier.name, 
                              weka.control=weka.control, num.folds=num.folds, 
                              min.instances=min.instances, 
                              max.instances=max.instances, 
                              num.repetitions=num.repetitions,
                              online.heuristic=online.heuristic)
#save(l.al, file='~/R/sf_ALTLAR/output/al.usc.had/pamap.2/l.al.frame.Rdata')
save(l.al, file='~/R/sf_ALTLAR/output/al.usc.had/pamap.2/l.al.frame.gamma.2.Rdata')
# 
# l.al.seg = SimulateForAllSubjects.seg(subjects.features=subjects.features, gamma=gamma,
#                                       classifier.name=classifier.name, 
#                                       weka.control=weka.control, num.folds=num.folds, 
#                                       min.instances=min.instances, 
#                                       max.instances=max.instances, 
#                                       num.repetitions=num.repetitions,
#                                       online.heuristic=online.heuristic)
# save(l.al.seg, file='~/R/sf_ALTLAR/output/al.usc.had/l.al.seg.Rdata')

#stop('done')

# online.heuristic = Random
# 
# l.rs = SimulateForAllSubjects(subjects.features=subjects.features, gamma=gamma,
#                               classifier.name=classifier.name, 
#                               weka.control=weka.control, num.folds=num.folds, 
#                               min.instances=min.instances, 
#                               max.instances=max.instances, 
#                               num.repetitions=num.repetitions,
#                               online.heuristic=online.heuristic)
# save(l.rs, file='~/R/sf_ALTLAR/output/al.usc.had/pamap.2/l.rs.frame.Rdata')
# 
# 
# l.rs.seg = SimulateForAllSubjects.seg(subjects.features=subjects.features, gamma=gamma,
#                                       classifier.name=classifier.name, 
#                                       weka.control=weka.control, num.folds=num.folds, 
#                                       min.instances=min.instances, 
#                                       max.instances=max.instances, 
#                                       num.repetitions=num.repetitions,
#                                       online.heuristic=online.heuristic)
# save(l.rs.seg, file='~/R/sf_ALTLAR/output/al.usc.had/l.rs.seg.Rdata')

#perf.al = GetPerformance(l=l.al.seg, subject.index=1)$perf.seg.full
#perf.rs = GetPerformance(l=l.rs.seg, subject.index=1)$perf.seg.full

#plot(perf.al, type='l', col='red', ylim=c(0, 1))
#lines(perf.rs, col='blue')


#online.heuristic = B.margin
# 
# online.heuristic = Random
# #subjects.features = subjects.features[c(1, 2, 3)]
# l.rs = SimulateForAllSubjects.generated.seg(subjects.features=subjects.features, gamma=gamma,
#                                             classifier.name=classifier.name, 
#                                             weka.control=weka.control, num.folds=num.folds, 
#                                             min.instances=min.instances, 
#                                             max.instances=max.instances, 
#                                             num.repetitions=num.repetitions,
#                                             online.heuristic=online.heuristic,
#                                             seg.size=2)
# save(l.rs, file='~/R/sf_ALTLAR/output/al.usc.had/pamap.2/seg.size.2/l.rs.frame.Rdata')
# 
# l.rs = SimulateForAllSubjects.generated.seg(subjects.features=subjects.features, gamma=gamma,
#                                             classifier.name=classifier.name, 
#                                             weka.control=weka.control, num.folds=num.folds, 
#                                             min.instances=min.instances, 
#                                             max.instances=max.instances, 
#                                             num.repetitions=num.repetitions,
#                                             online.heuristic=online.heuristic,
#                                             seg.size=3)
# save(l.rs, file='~/R/sf_ALTLAR/output/al.usc.had/pamap.2/seg.size.3/l.rs.frame.Rdata')
# 
# l.rs = SimulateForAllSubjects.generated.seg(subjects.features=subjects.features, gamma=gamma,
#                                             classifier.name=classifier.name, 
#                                             weka.control=weka.control, num.folds=num.folds, 
#                                             min.instances=min.instances, 
#                                             max.instances=max.instances, 
#                                             num.repetitions=num.repetitions,
#                                             online.heuristic=online.heuristic,
#                                             seg.size=4)
# save(l.rs, file='~/R/sf_ALTLAR/output/al.usc.had/pamap.2/seg.size.4/l.rs.frame.Rdata')
# 
# l.rs = SimulateForAllSubjects.generated.seg(subjects.features=subjects.features, gamma=gamma,
#                                             classifier.name=classifier.name, 
#                                             weka.control=weka.control, num.folds=num.folds, 
#                                             min.instances=min.instances, 
#                                             max.instances=max.instances, 
#                                             num.repetitions=num.repetitions,
#                                             online.heuristic=online.heuristic,
#                                             seg.size=5)
# save(l.rs, file='~/R/sf_ALTLAR/output/al.usc.had/pamap.2/seg.size.5/l.rs.frame.Rdata')
# 
# l.rs = SimulateForAllSubjects.generated.seg(subjects.features=subjects.features, gamma=gamma,
#                                             classifier.name=classifier.name, 
#                                             weka.control=weka.control, num.folds=num.folds, 
#                                             min.instances=min.instances, 
#                                             max.instances=max.instances, 
#                                             num.repetitions=num.repetitions,
#                                             online.heuristic=online.heuristic,
#                                             seg.size=6)
# save(l.rs, file='~/R/sf_ALTLAR/output/al.usc.had/pamap.2/seg.size.6/l.rs.frame.Rdata')

# l.rs = SimulateForAllSubjects.generated.seg(subjects.features=subjects.features, gamma=gamma,
#                                             classifier.name=classifier.name, 
#                                             weka.control=weka.control, num.folds=num.folds, 
#                                             min.instances=min.instances, 
#                                             max.instances=max.instances, 
#                                             num.repetitions=num.repetitions,
#                                             online.heuristic=online.heuristic,
#                                             seg.size=7)
# save(l.rs, file='~/R/sf_ALTLAR/output/al.usc.had/pamap.2/seg.size.7/l.rs.frame.Rdata')
# 
# l.rs = SimulateForAllSubjects.generated.seg(subjects.features=subjects.features, gamma=gamma,
#                                             classifier.name=classifier.name, 
#                                             weka.control=weka.control, num.folds=num.folds, 
#                                             min.instances=min.instances, 
#                                             max.instances=max.instances, 
#                                             num.repetitions=num.repetitions,
#                                             online.heuristic=online.heuristic,
#                                             seg.size=8)
# save(l.rs, file='~/R/sf_ALTLAR/output/al.usc.had/pamap.2/seg.size.8/l.rs.frame.Rdata')



# 
# online.heuristic = B.margin
# #subjects.features = subjects.features[c(1, 2, 3)]
# l.rs = SimulateForAllSubjects.generated.seg(subjects.features=subjects.features, gamma=gamma,
#                                             classifier.name=classifier.name, 
#                                             weka.control=weka.control, num.folds=num.folds, 
#                                             min.instances=min.instances, 
#                                             max.instances=max.instances, 
#                                             num.repetitions=num.repetitions,
#                                             online.heuristic=online.heuristic,
#                                             seg.size=2)
# save(l.rs, file='~/R/sf_ALTLAR/output/al.usc.had/pamap.2/seg.size.2/l.al.frame.Rdata')
# 
# l.rs = SimulateForAllSubjects.generated.seg(subjects.features=subjects.features, gamma=gamma,
#                                             classifier.name=classifier.name, 
#                                             weka.control=weka.control, num.folds=num.folds, 
#                                             min.instances=min.instances, 
#                                             max.instances=max.instances, 
#                                             num.repetitions=num.repetitions,
#                                             online.heuristic=online.heuristic,
#                                             seg.size=3)
# save(l.rs, file='~/R/sf_ALTLAR/output/al.usc.had/pamap.2/seg.size.3/l.al.frame.Rdata')
# 
# l.rs = SimulateForAllSubjects.generated.seg(subjects.features=subjects.features, gamma=gamma,
#                                             classifier.name=classifier.name, 
#                                             weka.control=weka.control, num.folds=num.folds, 
#                                             min.instances=min.instances, 
#                                             max.instances=max.instances, 
#                                             num.repetitions=num.repetitions,
#                                             online.heuristic=online.heuristic,
#                                             seg.size=4)
# save(l.rs, file='~/R/sf_ALTLAR/output/al.usc.had/pamap.2/seg.size.4/l.al.frame.Rdata')
# 
# l.rs = SimulateForAllSubjects.generated.seg(subjects.features=subjects.features, gamma=gamma,
#                                             classifier.name=classifier.name, 
#                                             weka.control=weka.control, num.folds=num.folds, 
#                                             min.instances=min.instances, 
#                                             max.instances=max.instances, 
#                                             num.repetitions=num.repetitions,
#                                             online.heuristic=online.heuristic,
#                                             seg.size=5)
# save(l.rs, file='~/R/sf_ALTLAR/output/al.usc.had/pamap.2/seg.size.5/l.al.frame.Rdata')
# 
# l.rs = SimulateForAllSubjects.generated.seg(subjects.features=subjects.features, gamma=gamma,
#                                             classifier.name=classifier.name, 
#                                             weka.control=weka.control, num.folds=num.folds, 
#                                             min.instances=min.instances, 
#                                             max.instances=max.instances, 
#                                             num.repetitions=num.repetitions,
#                                             online.heuristic=online.heuristic,
#                                             seg.size=6)
# save(l.rs, file='~/R/sf_ALTLAR/output/al.usc.had/pamap.2/seg.size.6/l.al.frame.Rdata')
# 
# # l.rs = SimulateForAllSubjects.generated.seg(subjects.features=subjects.features, gamma=gamma,
# #                                             classifier.name=classifier.name, 
# #                                             weka.control=weka.control, num.folds=num.folds, 
# #                                             min.instances=min.instances, 
# #                                             max.instances=max.instances, 
# #                                             num.repetitions=num.repetitions,
# #                                             online.heuristic=online.heuristic,
# #                                             seg.size=7)
# # save(l.rs, file='~/R/sf_ALTLAR/output/al.usc.had/pamap.2/seg.size.7/l.al.frame.Rdata')
# # 
# # l.rs = SimulateForAllSubjects.generated.seg(subjects.features=subjects.features, gamma=gamma,
# #                                             classifier.name=classifier.name, 
# #                                             weka.control=weka.control, num.folds=num.folds, 
# #                                             min.instances=min.instances, 
# #                                             max.instances=max.instances, 
# #                                             num.repetitions=num.repetitions,
# #                                             online.heuristic=online.heuristic,
# #                                             seg.size=8)
# # save(l.rs, file='~/R/sf_ALTLAR/output/al.usc.had/pamap.2/seg.size.8/l.al.frame.Rdata')
