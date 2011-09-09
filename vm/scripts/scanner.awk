BEGIN {
	prc = 0
}

/^\W*PRIMITIVE(_UNSPEC|)/ {
	if(match($0, "PRIMITIVE_UNSPEC")) {
		pr[prc, "scheme_options"] = "#:unspecified-result";
	}

	match($0, /\([ \t]*(.+?)[ \t]*,[ \t]*(\w+)[ \t]*,[ \t]*(\w+)[ \t]*\)/, opts)
	pr[prc, "scheme_name"] = opts[1]
	pr[prc, "c_name"] = opts[2]
	pr[prc, "arguments"] = opts[3]
	prc++
}
