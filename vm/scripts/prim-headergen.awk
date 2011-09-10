END {
	print "#include <picobit.h>"
	print "#include <primitives.h>"
	print ""
	print "#ifdef CONFIG_DEBUG_STRINGS"
	print "const char* const primitive_names[] = {"

	for(i=0; i<prc; i++) {
		print "\t\"" pr[i, "scheme_name"] "\","
	}

	print "};"
	print "#endif /* CONFIG_DEBUG_STRINGS */"
}
