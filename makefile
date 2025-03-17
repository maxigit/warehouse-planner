bench_%:
	stack bench --ba '-m prefix $* --output results/$*.html --csv results/$*.csv'

prof1_%:
	stack --work-dir .stack-profile bench --profile --ba '--iters 1 -m prefix $* '
	mv bench.prof results/bench-$$(date +"%F:%H:%M").prof
	
prof_%:
	stack --work-dir .stack-profile bench --profile --ba '--iters 5 -m prefix $* '
	mv bench.prof results/$*-$$(date +"%F:%H:%M").prof
	ln -sf $*-$$(date +"%F:%H:%M").prof results/$*.last.prof

%.prof.html:
	profiteur $*.prof
	
pview_%: results/$*.last.prof.html
				 rifle $<
  
# Update test
data/%.stocktake: data/%.org
	stack exec whp $< -- --no-check -k > $@
