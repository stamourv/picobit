END {
	print "#include <picobit.h>"
	print "#include <primitives.h>"
	print ""
	print "const char* const primitive_names[] = {"

	for(i=0; i<prc; i++) {
		print "\t\"" pr[i, "scheme_name"] "\","
	}

	print "};"
}
