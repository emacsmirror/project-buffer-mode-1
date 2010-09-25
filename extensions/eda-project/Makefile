###_  Makefile for EDA projects

###_. Headers
###_ , License
## Copyright (C) 2010  Tom Breton (Tehom)

## Author: Tom Breton (Tehom) <tehom@panix.com>
## Keywords: lisp, maint, internal

## This file is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.

## This file is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with GNU Emacs; see the file COPYING.  If not, write to
## the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
## Boston, MA 02111-1307, USA.

###_. Body

###_ , Definitions

DRC_PATTERN="DRC errors found"

###_ , Checking the design
#Here we indicate that a NAME.sch was successfully checked vs design
#errors by the presence of an up-to-date file NAME.drc2-succeeded.  

#Grep here succeeds (returns 0) just if DRC_PATTERN is found, ie if
#drc found errors. 
%.drc2-succeeded:%.sch
	@rm -f $*.drc2-succeeded
	@if gnetlist -g drc2 -o $*.drc2 $*.sch | grep --quiet $(DRC_PATTERN) ; \
	then echo "Design check failed, see $*.drc2" && false ; \
	else touch $*.drc2-succeeded ; \
	fi

###_ , Building netlists
#Make a netlist for gnucap.  "spice-sdb" is correct nonetheless.

#gnetlist creates the output file no matter what, so erase it if there
#are errors.
%.net:%.sch %.drc2-succeeded
	if gnetlist -s -g spice-sdb -o $*.net $*.sch ; \
	then true ; \
	else rm $*.net ; \
	fi

###_. Footers
###_ * Local emacs vars.
###_  + Local variables:
###_  + mode: allout
###_  + mode: makefile
###_  + End:

