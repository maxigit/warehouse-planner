bench_%:
	stack bench --ba '-m prefix $* --output results/$*.html --csv results/$*.csv'

prof1_%:
	stack --work-dir .stack-profile bench --profile --ba '--iters 1 -m prefix $* '
	mv bench.prof results/bench-$$(date +"%F:%H:%M").prof
	
prof_%:
	stack --work-dir .stack-profile bench --profile --ba '--iters 5 -m prefix $* '
	mv bench.prof results/bench-$$(date +"%F:%H:%M").prof
%.prof.html:
	profiteur $*.prof
