

header = ; ----------------------------------------------------------------------\n; Begin file: $$file\n; ----------------------------------------------------------------------\n\n\n

footer = ; ----------------------------------------------------------------------\n; End file: $$file\n; ----------------------------------------------------------------------\n\n\n

cclgui: clean
	cat file-list.txt | while read file; do printf "${header}" >> cclgui.lisp; cat "../$$file" >> cclgui.lisp; printf "${footer}" >> cclgui.lisp; done

clean:
	rm -f cclgui.lisp
