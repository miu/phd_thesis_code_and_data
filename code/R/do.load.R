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
setwd("/home/tudor/R/sf_ALTLAR")
source('R/load.R')

#ReadSubjectData = ReadSubjectData.usc.had
#input.dir = '~/datasets/usc-had'

ReadSubjectData = ReadSubjectData.pamap
input.dir = '~/datasets/datasets/pamap 1 and 2 (Reiss2012)/PAMAP2_Dataset/Protocol/output'

#setwd("/home/tudor/R/sf_ALTLAR/input")
setwd(input.dir)
s = ReadSubjectsData()
cat('Loaded data for', length(s), 'subjects\n')
setwd("/home/tudor/R/sf_ALTLAR")
#------------------------------------------------------------------------------#
