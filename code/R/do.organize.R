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

#------------------------------------------------------------------------------#
# Main
#output.dir='output/organize.usc.had'
#output.dir='output/organize.pamap'
output.dir='organize.pamap.2.protocol'

source('R/file.utils.R')
source('R/organize.R')
#WINDOW_LENGTH = 512
#WINDOW_LENGTH = 500
WINDOW_LENGTH = 500
#WINDOW_LENGTH = 1024
#registerDoMC(cores=8)

source('R/do.load.R')
subjects.features = GetFeaturesForSubjects(s)

#subjects.features = BalanceLabelsForSubjects(subjects.features)

# drop NULLS and NA's
for (i.subject in seq_along(subjects.features)) {
  i.nulls = c()
  for (i.row in seq_len(nrow(subjects.features[[i.subject]]$data))) {
    s = sum(subjects.features[[i.subject]]$data[i.row, 1:9])
    if (is.nan(s) | is.null(s) | is.na(s)) {
      i.nulls = c(i.nulls, i.row)
    }
  }
  
  if (length(i.nulls) > 0) {
    subjects.features[[i.subject]]$data = subjects.features[[i.subject]]$data[-i.nulls, ]
  }
}

# drop 9th subject because of too little data
subjects.features = subjects.features[-9]

cat('Extracted features for', length(subjects.features), 'subjects\n')
SaveObject(obj=subjects.features, 
           var.name=deparse(substitute(subjects.features)),
           dir=output.dir)
#------------------------------------------------------------------------------#
