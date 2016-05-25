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

library(foreach)

GetAllSubjectNumbers = function(subjects.features) {
  v = c()
  foreach(s.f = subjects.features) %do% {
    v = c(v, s.f$number)
  }
  return( v )
}

FindSubjectByNumber = function(subjects.features, subject.number) {
  for (s.f in subjects.features) {
    if (s.f$number == subject.number) {
      return( s.f )
    }
  }
  return( NULL )
}

ExcludeSubjectByNumber = function(subjects.features, subject.number) {
  s = list()
  foreach(s.f = subjects.features) %do% {
    if (s.f$number != subject.number) {
      next.i = 1 + length(s)
      s[[next.i]] = s.f
    }
  }
  return( s )
}

GetAllLabels = function(subjects.features) {
  labels = c()
  foreach (s.f = subjects.features) %do% {
    subj.labels = as.character(unique(s.f$data$activity))
    labels = unique(c(labels, subj.labels))
  }
  labels = as.character(sort(as.integer(labels), decreasing=F))
  return( labels )
}
