
DRC_PATTERN="DRC errors found"

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

#For gnucap
%.net:%.sch %.drc2-succeeded
	gnetlist -s -g spice-sdb -o $*.net $*.sch


%.verbose-net:%.sch %.drc2-succeeded
	gnetlist -v -s -g spice-sdb -o $*.verbose-net $*.sch
