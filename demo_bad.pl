#! /usr/local/bin/perl -sw

# SHOWCASE VARIOUS ERROR MESSAGES WITH A VERY UNWELL GRAMMAR

use Parse::RecDescent;

$grammar =
q{
	typedef : a ... ...! ... b
	typedef : a ...!...!...!...! b
		| /type/ ident /has/ field(s) /end type/
			{ $result = $item[2]; }
		| /type/ ident /is/ ident
			{ $result = $item[2]; }
		| quasit(s)
		| quasit(-1..3)
		| quasit(..0)
		| field(?) field end
		| <error> quasit "here" ...!/why/

	Extend: extend

	Replace: replace
		$%^@#

	field   : /field/ ident /is/ ident
		| field quasit
		| !quasit(s?)
		!| #NOTHING


	typedef : whatever

	package somewhereelse

	!ident   : /[A-Za-z]\w*???/
			{ $result = $item[1]; }

	quasit  : field
		| typedef
 };

new Parse::RecDescent ($grammar) || die "Bad grammar! No biscuit!\n";
