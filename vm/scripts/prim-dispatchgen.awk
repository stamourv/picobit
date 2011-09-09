function primgen(primclass, offset) {
	if(!pr[offset, "scheme_name"])
		return;

	print "\tcase " primclass " :"
	print "#ifdef CONFIG_DEBUG_STRINGS"
	print "\t\tIF_TRACE(printf(\"  (%s)\\n\", primitive_names[bytecode_lo4 + " offset "]));"
	print "#else"
	print "\t\tIF_TRACE(printf(\"  (<primitive>)\\n\"));"
	print "#endif"
	print ""
	print "\tswitch (bytecode_lo4) {"

	for(i=offset; i<offset+16; i++) {
		if(!pr[i, "scheme_name"])
			continue;

		print "\t\tcase " (i - offset) " :"

		if(i == 0) {
			print "\t\t\treturn;"
			print "";
			continue;
		}

		if(pr[i, "arguments"] > 3)
			print "\t\t\targ4 = pop ();"
		if(pr[i, "arguments"] > 2)
			print "\t\t\targ3 = pop ();"
		if(pr[i, "arguments"] > 1)
			print "\t\t\targ2 = pop ();"
		if(pr[i, "arguments"] > 0)
			print "\t\t\targ1 = pop ();"

		print "\t\t\tprim_" pr[i, "c_name"] " ();"

		if(!match(pr[i, "scheme_options"], "unspecified-result"))
			print "\t\t\tpush_arg1 ();"

		print "\t\t\tbreak;"
		print ""
	}

	print "\t}"
	print ""
	print "\tgoto dispatch;"
	print ""
}

END {
	primgen("PRIM1", 0);
	primgen("PRIM2", 16);
	primgen("PRIM3", 32);
	primgen("PRIM4", 48);
}
