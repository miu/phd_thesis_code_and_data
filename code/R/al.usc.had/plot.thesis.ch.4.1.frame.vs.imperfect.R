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

## PAMAP
#load('~/R/sf_ALTLAR/output/ch4/pamap/1-frame.al.perf.Rdata')
#al.perf.1.frame = al.perf
#load('~/R/sf_ALTLAR/output/ch4/pamap/1-frame.rs.perf.Rdata')
#rs.perf.1.frame = rs.perf
#load('~/R/sf_ALTLAR/output/ch4/pamap/imperfect.al.perf.Rdata')
#al.perf.imperfect = al.perf
#load('~/R/sf_ALTLAR/output/ch4/pamap/imperfect.rs.perf.Rdata')
#rs.perf.imperfect = rs.perf

## USC-HAD
load('~/R/sf_ALTLAR/output/ch4/usc-had/1-frame.al.perf.Rdata')
al.perf.1.frame = al.perf
load('~/R/sf_ALTLAR/output/ch4/usc-had/1-frame.rs.perf.Rdata')
rs.perf.1.frame = rs.perf
load('~/R/sf_ALTLAR/output/ch4/usc-had/imperfect.al.perf.Rdata')
al.perf.imperfect = al.perf
load('~/R/sf_ALTLAR/output/ch4/usc-had/imperfect.rs.perf.Rdata')
rs.perf.imperfect = rs.perf


plot(al.perf.1.frame, xlab='Num. Annotations', ylab='F-Score', ylim=c(0, 1), 
     type='l', col='red', lwd=2)
lines(rs.perf.1.frame, col='blue', lwd=2)
lines(al.perf.imperfect, col='red', lwd=1, lty='dashed')
lines(rs.perf.imperfect, col='blue', lwd=1, lty='dashed')
lines(x=c(40, 40), y=c(0, 1), lty='dotted')
lines(x=c(40, 200), y=c(al.perf.imperfect[40], al.perf.imperfect[40]), col='red', lty='dotted')
lines(x=c(40, 200), y=c(rs.perf.imperfect[40], rs.perf.imperfect[40]), col='blue', lty='dotted')
legend(x='bottomright', 
       legend=c(
         'OAL 1-frame segments                  ',
         'RS 1-frame segments                   ', 
         'OAL noisy segments                    ',
         'RS noisy segments                     '),
       col=c('red', 'blue', 'red', 'blue'), 
       lwd=c(2, 2, 1, 1),
       lty=c('solid', 'solid', 'dashed', 'dashed'))
