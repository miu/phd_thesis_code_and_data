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

SaveObject = function(obj, var.name, dir) {
  file.name = paste(var.name, 'Rdata', sep='.')
  path = paste(dir, file.name, sep='/')
  #cat(' --- object path:', path, '\n')
  if (file.exists(path)) {
    msg = paste('File', path, 'already exists!')
    warning(msg)
  }
  dir.create(path=dir, showWarnings=F, recursive=T)
  save(obj, file=path)
  cat(' saved', var.name, 'to', path, '\n')
}

LoadObject = function(file.name, dir) {
  file.name = paste(file.name, 'Rdata', sep='.')
  path = paste(dir, file.name, sep='/')
  load(path)
  return( obj )
}

SavePlotBegin = function(dir, file.name, width=7, height=5) {
  dir.create(path=dir, showWarnings=F, recursive=T)
  #file.name = paste(file.name, 'png', sep='.')
  file.name = paste(file.name, 'pdf', sep='.')
  path = paste(dir, file.name, sep='/')
  #png(filename=path)
  pdf(file=path, width=width, height=height)
  cat('Saving plot to file', path, '...\n')
}

SavePlotEnd = function() {
  dev.off()
  cat('Closed plot file!\n')
}
