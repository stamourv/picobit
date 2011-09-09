END {
	for(i=0; i<prc; i++) {
		print "(define-primitive " pr[i, "scheme_name"] " " pr[i, "arguments"] " " i " " pr[i, "scheme_options"] ")"
	}
}
