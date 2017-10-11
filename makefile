Parser.grm.sml : Parser.grm
	mlyacc Parser.grm

clean :
	rm -rf Parser.grm.sml Parser.grm.sig Parser.grm.desc
