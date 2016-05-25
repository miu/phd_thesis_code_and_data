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

setwd('~/R/sf_ALTLAR/R/user.study')

source('plot.R')

#load('~/R/sf_ALTLAR/output/user.study/l.perfs.with.sitting.Rdata')

#PlotAllUsers(l.perfs.with.sitting)
#PlotAvgUsers(l.perfs.with.sitting)

# load('~/R/sf_ALTLAR/output/user.study/noisy/l.perfs.without.sitting.noisy.ZeroR.Rdata')
# l.perfs.without.sitting.noisy.ZeroR = l.perfs.without.sitting.noisy
#  
# load('~/R/sf_ALTLAR/output/user.study/noisy/l.perfs.with.sitting.noisy.ZeroR.Rdata')
# l.perfs.with.sitting.noisy.ZeroR = l.perfs.with.sitting.noisy
#  
# load('~/R/sf_ALTLAR/output/user.study/noisy/l.perfs.without.sitting.noisy.Rdata')
#  
# load('~/R/sf_ALTLAR/output/user.study/noisy/l.perfs.with.sitting.noisy.Rdata')

#l.perfs.with.sitting.noisy.ZeroR.avg = AggregatePerformance(l.perfs=l.perfs.with.sitting.noisy.ZeroR)
#l.perfs.without.sitting.noisy.ZeroR.avg = AggregatePerformance(l.perfs=l.perfs.without.sitting.noisy.ZeroR)

#l.perfs.with.sitting.noisy = AggregatePerformance(l.perfs=l.perfs.with.sitting.noisy)
#l.perfs.without.sitting.noisy = AggregatePerformance(l.perfs=l.perfs.without.sitting.noisy)

# all.subject.dirs = c(
#   'participant_1',
#   'participant_2',
#   'participant_3',
#   'participant_4', 
#   'participant_5',
#   'participant_6',
#   'participant_7',
#   'participant_8',
#   'participant_9',
#   'participant_10'
# )
# 
# l.perfs = l.perfs.with.sitting.noisy
#l.perfs = FilterBySubjectDir(l.perfs=l.perfs, include.subject.dirs=all.subject.dirs[1:5])
#PlotAllUsers.noisy(l.perfs, noise.level=0)
#PlotAvgUsers.noisy(l.perfs, noise.level=0)
#PlotSingle(l.perf=l.perfs[[1]])

CombineSubjects = function(l.file.paths, obj.name) {
  l.perfs = list()
  
  obj.reg.exp = '^l\\.perfs\\.with.+sitting\\.noisy$'
  
  for (file.path in l.file.paths) {
    full.path = paste(output.dir, file.path, sep='/')
    load(file=full.path)
    
    l.perf = get(obj.name)
    
    for (indiv.perf in l.perf) {
      l.perfs[[1 + length(l.perfs)]] = indiv.perf
    }
  }
  
  return( l.perfs )
}

output.dir = '~/R/sf_ALTLAR/output/user.study/big.machine/noisy'

l.file.paths = list.files(path=output.dir, pattern='^l\\.perfs\\.with\\..+subject\\..+Rdata$')
obj.name='l.perfs.with.sitting.noisy'
l.perfs.with.sitting.noisy = CombineSubjects(l.file.paths=l.file.paths, obj.name=obj.name)

l.file.paths = list.files(path=output.dir, pattern='^l\\.perfs\\.without\\..+subject\\..+Rdata$')
obj.name='l.perfs.without.sitting.noisy'
l.perfs.without.sitting.noisy = CombineSubjects(l.file.paths=l.file.paths, obj.name=obj.name)

# ZeroR
output.dir = '~/R/sf_ALTLAR/output/user.study/zeror'

l.file.paths = list.files(path=output.dir, pattern='^l\\.perfs\\.with\\..+subject\\..+Rdata$')
obj.name='l.perfs.with.sitting.noisy'
l.perfs.with.sitting.noisy.zeror = CombineSubjects(l.file.paths=l.file.paths, obj.name=obj.name)

l.file.paths = list.files(path=output.dir, pattern='^l\\.perfs\\.without\\..+subject\\..+Rdata$')
obj.name='l.perfs.without.sitting.noisy'
l.perfs.without.sitting.noisy.zeror = CombineSubjects(l.file.paths=l.file.paths, obj.name=obj.name)


include.subject.dirs = c('participant_1',
                         'participant_2',
                         'participant_3',
                         'participant_4', 
                         'participant_5'
)

# include.subject.dirs = c('participant_6',
#                         'participant_7',
#                         'participant_8',
#                         'participant_9',
#                         'participant_10'
# )

if (!file.exists('~/R/sf_ALTLAR/output/user.study/datasets.segments.annotated.Rdata')) {
  cat('Loading datasets...\n')
  datasets.instances = LoadDatasets()
  #dataset.instances = datasets.instances[[1]]
  
  cat('Splitting into segments...\n')
  datasets.segments = lapply(X=datasets.instances, FUN=SplitIntoSegments)
  #dataset.segments = datasets.segments[[1]]
  
  exclude.labels = c(-1, -2)
  #dataset.segments.old = dataset.segments
  #dataset.segments.new = FilterSegments(dataset.segments=dataset.segments, exclude.labels=exclude.labels)
  
  datasets.segments.annotated = lapply(X=datasets.segments, FUN=FilterSegments, exclude.labels=exclude.labels)
  
  #datasets.segments.annotated = SortUsersBySize(datasets.segments.annotated)
  
  save(datasets.segments.annotated, file='~/R/sf_ALTLAR/output/user.study/datasets.segments.annotated.Rdata')
  save(datasets.segments, file='~/R/sf_ALTLAR/output/user.study/datasets.segments.Rdata')
} else {
  load(file='~/R/sf_ALTLAR/output/user.study/datasets.segments.annotated.Rdata')
  load(file='~/R/sf_ALTLAR/output/user.study/datasets.segments.Rdata')
}

graphics.dir = '~/R/sf_ALTLAR/graphics/user.study'

#############################################################
#l.perfs=FilterBySubjectDir(l.perfs=l.perfs.without.sitting.noisy, include.subject.dirs=include.subject.dirs)

l.perfs=FilterBySubjectDir(l.perfs=l.perfs.with.sitting.noisy, include.subject.dirs=include.subject.dirs)
l.perfs.zeror = FilterBySubjectDir(l.perfs=l.perfs.with.sitting.noisy.zeror, 
                                   include.subject.dirs=include.subject.dirs)

#PlotAllUsers.noisy.zeror(l.perfs=l.perfs, l.perfs.zeror=l.perfs.zeror, 
#                         noise.level=0, noise.level.zeror=0, graphics.dir=graphics.dir)

# PlotAllUsers.noisy(l.perfs=l.perfs, noise.level=0, graphics.dir=graphics.dir)
# PlotAllUsers.noisy(l.perfs=l.perfs, noise.level=0.1, graphics.dir=graphics.dir)
# PlotAvgUsers.noisy(l.perfs=l.perfs, noise.level=0, graphics.dir=graphics.dir)
# PlotAvgUsers.noisy(l.perfs=l.perfs, noise.level=0.1, graphics.dir=graphics.dir)

noise.levels = c(0, 0.1)
# PlotAllUsers.noisy.versus.noise.level(l.perfs=l.perfs, noise.levels=noise.levels, graphics.dir=graphics.dir)
# PlotAvgUsers.noisy.versus.noise.level(l.perfs=l.perfs, noise.levels=noise.levels, graphics.dir=graphics.dir)

# PlotAllUsers.noisy.versus.noise.level.zeror(l.perfs=l.perfs, noise.levels=noise.levels, 
#                                             l.perfs.zeror=l.perfs.zeror, noise.level.zeror=0, 
#                                             graphics.dir=graphics.dir)
PlotAvgUsers.noisy.versus.noise.level.zeror(l.perfs=l.perfs, noise.levels=noise.levels, 
                                            l.perfs.zeror=l.perfs.zeror, noise.level.zeror=0, 
                                            graphics.dir=graphics.dir)
#stop('done')

l.perfs=FilterBySubjectDir(l.perfs=l.perfs.without.sitting.noisy, include.subject.dirs=include.subject.dirs)
l.perfs.zeror = FilterBySubjectDir(l.perfs=l.perfs.without.sitting.noisy.zeror, 
                                   include.subject.dirs=include.subject.dirs)

# PlotAllUsers.noisy.zeror(l.perfs=l.perfs, l.perfs.zeror=l.perfs.zeror, 
#                          noise.level=0, noise.level.zeror=0, graphics.dir=graphics.dir)
# 
# PlotAllUsers.noisy(l.perfs=l.perfs, noise.level=0, graphics.dir=graphics.dir)
# PlotAllUsers.noisy(l.perfs=l.perfs, noise.level=0.1, graphics.dir=graphics.dir)
# PlotAvgUsers.noisy(l.perfs=l.perfs, noise.level=0, graphics.dir=graphics.dir)
# PlotAvgUsers.noisy(l.perfs=l.perfs, noise.level=0.1, graphics.dir=graphics.dir)

noise.levels = c(0, 0.1)
# PlotAllUsers.noisy.versus.noise.level(l.perfs=l.perfs, noise.levels=noise.levels, graphics.dir=graphics.dir)
# PlotAvgUsers.noisy.versus.noise.level(l.perfs=l.perfs, noise.levels=noise.levels, graphics.dir=graphics.dir)
# 
# PlotAllUsers.noisy.versus.noise.level.zeror(l.perfs=l.perfs, noise.levels=noise.levels, 
#                                             l.perfs.zeror=l.perfs.zeror, noise.level.zeror=0, 
#                                             graphics.dir=graphics.dir)
PlotAvgUsers.noisy.versus.noise.level.zeror(l.perfs=l.perfs, noise.levels=noise.levels, 
                                            l.perfs.zeror=l.perfs.zeror, noise.level.zeror=0, 
                                            graphics.dir=graphics.dir)
#############################################################


exclude.labels = 1 # sitting
datasets.segments.annotated = lapply(X=datasets.segments.annotated, FUN=FilterSegments, exclude.labels=exclude.labels)

#ylim = c(0, 60)
#ylim.norm = c(0, 0.6)

#ylim = c(0, 30)
#ylim.norm = c(0, 0.5)


#ylim = c(0, 20)
#ylim.norm = c(0, 0.5)

#ylim = c(0, 10)
#ylim.norm = c(0, 0.5)

ylim = c(0, 20)
ylim.norm = c(0, 0.5)

PlotHistAvgUsers(datasets.segments.annotated=datasets.segments.annotated[c(1:5)], graphics.dir=graphics.dir, 
                 ylim=ylim, ylim.norm=ylim.norm)



annotation.ratios = rep(NA, length(datasets.segments.annotated))
for (i in seq_along(datasets.segments.annotated)) {
  annotation.ratios[i] = GetRatioAnnotated(datasets.segments[[i]], datasets.segments.annotated[[i]])
}

print(annotation.ratios)

# B.NB.30 vs ZeroR
#load(file='~/R/sf_ALTLAR/output/user.study/l.perfs.without.sitting.ZeroR.Rdata')
#l.perfs.without.zeror = l.perfs.without.sitting

