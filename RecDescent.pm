# GENERATE RECURSIVE DESCENT PARSER OBJECTS FROM A GRAMMAR
# SEE RecDescent.pod FOR FULL DETAILS

use strict;

package Parse::RecDescent;

use Text::Balanced qw ( extract_codeblock extract_bracketed extract_quotelike );

use vars qw ( $tokensep );

   *deftokensep  = \'\s*';	# DEFAULT SEPARATOR IS OPTIONAL WHITESPACE
   $tokensep  = '\s*';		# UNIVERSAL SEPARATOR IS OPTIONAL WHITESPACE
my $MAXREP  = 100_000_000;	# REPETITIONS MATCH AT MOST 100,000,000 TIMES

package Parse::RecDescent::LineCounter;

sub TIESCALAR
{
	bless { offset => $_[1], text => $_[2] }, $_[0];
}

sub FETCH    
{
	${$_[0]->{offset}}-Parse::RecDescent::_linecount(${$_[0]->{text}})+1;
}

sub STORE
{}


package Parse::RecDescent::Rule;

sub new ($$$$$)
{
	my $class = ref($_[0]) || $_[0];
	my $name  = $_[1];
	my $owner = $_[2];
	my $line  = $_[3];
	my $replace = $_[4];

	if (defined $owner->{"rules"}{$name})
	{
		my $self = $owner->{"rules"}{$name};
		if ($replace && !$self->{"changed"})
		{
			$self->reset;
		}
		return $self;
	}
	else
	{
		return $owner->{"rules"}{$name} =
			bless
			{
				"name"     => $name,
				"prods"    => [],
				"calls"    => [],
				"changed"  => 0,
				"tokensep" => undef,
				"line"     => $line,
				"impcount" => 0,
			}, $class;
	}
}

sub reset($)
{
	@{$_[0]->{"prods"}} = ();
	@{$_[0]->{"calls"}} = ();
	$_[0]->{"changed"}  = 0;
	$_[0]->{"impcount"}  = 0;
}

sub DESTROY {}

sub hasleftmost($$)
{
	my ($self, $ref) = @_;

	my $prod;
	foreach $prod ( @{$self->{"prods"}} )
	{
		return 1 if $prod->hasleftmost($ref);
	}

	return 0;
}

sub leftmostsubrules($)
{
	my $self = shift;
	my @subrules = ();

	my $prod;
	foreach $prod ( @{$self->{"prods"}} )
	{
		push @subrules, $prod->leftmostsubrule();
	}

	return @subrules;
}

sub expected($)
{
	my $self = shift;
	my @expected = ();

	my $prod;
	foreach $prod ( @{$self->{"prods"}} )
	{
		my $next = $prod->expected();
		unless (! $next or _contains($next,@expected) )
		{
			push @expected, $next;
		}
	}

	return join ' or ', @expected;
}

sub _contains($@)
{
	my $target = shift;
	my $item;
	foreach $item ( @_ ) { return 1 if $target eq $item; }
	return 0;
}

sub addcall($$)
{
	my ( $self, $subrule ) = @_;
	unless ( _contains($subrule, @{$self->{"calls"}}) )
	{
		push @{$self->{"calls"}}, $subrule;
	}
}

sub addprod($$)
{
	my ( $self, $prod ) = @_;
	push @{$self->{"prods"}}, $prod;
	$self->{"changed"} = 1;
	$self->{"impcount"} = 0;
	$prod->{"number"} = $#{$self->{"prods"}};
	return $prod;
}

sub nextimplicit($)
{
	my $self = shift;
	my $prodcount = scalar @{$self->{"prods"}};
	my $impcount = ++$self->{"impcount"};
	return "_alternation_${impcount}_of_production_${prodcount}_of_rule_$self->{name}";
}

sub code($$)
{
	my ($self, $namespace) = @_;

eval 'undef &' . $namespace . '::' . $self->{"name"};

	my $code =
'
# ARGS ARE: ($parser, $text; $_linenum, $repeating, $_noactions)
sub ' . $namespace . '::' . $self->{"name"} .  '
{
	my $_toksep = undef;
	my $thisparser = $_[0];
	$ERRORS = 0;
	my $_linenum = $_[2] ? $_[2] : Parse::RecDescent::_linecount($_[1]);
	my $thisrule = $thisparser->{"rules"}{"' . $self->{"name"} . '"};
	Parse::RecDescent::_trace(q{Trying rule: ' . $self->{"name"} . ' at "}
				  .  Parse::RecDescent::_tracefirst($_[1])
				  . q{"});
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my $repeating =  defined($_[3]) && $_[3];
	my $_noactions = defined($_[4]) && $_[4];
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);

	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$_linenum, \$text;

';

	my $prod;
	foreach $prod ( @{$self->{"prods"}} )
	{
		$code .= $prod->code($namespace,$self);
	}

	$code .=
'
        unless ( $_matched || $return )
	{
		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{Didn\'t match rule: '. $self->{"name"} .'});
		return undef;
	}
	Parse::RecDescent::_trace(q{Matched rule '. $self->{"name"} .' with "}
				  . Parse::RecDescent::_tracemax(substr($_[1],0,-length($text)))
				  . q{"});
	$_[1] = $text;
	return defined($return)?$return:$item[$#item];
}
';

	return $code;
}

my @left;
sub isleftrec($$)
{
	my ($self, $rules) = @_;
	my $root = $self->{"name"};
	@left = $self->leftmostsubrules();
	my $next;
	foreach $next ( @left )
	{
		next unless defined $rules->{$next}; # SKIP NON-EXISTENT RULES
		return 1 if $next eq $root;
		my $child;
		foreach $child ( $rules->{$next}->leftmostsubrules() )
		{
		    push(@left, $child)
			if ! _contains($child, @left) ;
		}
	}
	return 0;
}

package Parse::RecDescent::Production;

sub describe ($)
{
	return join ' ', map { $_->describe or () } @{$_[0]->{items}};
}

sub new ($$;$$)
{
	my ($self, $line, $uncommit, $error) = @_;
	my $class = ref($self) || $self;

	bless
	{
		"items"    => [],
		"uncommit" => $uncommit,
		"error"    => $error,
		"tokensep" => undef,
		"line"     => $line,
	}, $class;
}

sub expected ($)
{
	my $itemcount = scalar @{$_[0]->{"items"}};
	return ($itemcount) ? $_[0]->{"items"}[0]->describe() : '';
}

sub hasleftmost ($$)
{
	my ($self, $ref) = @_;
	return ${$self->{"items"}}[0] eq $ref  if scalar @{$self->{"items"}};
	return 0;
}

sub leftmostsubrule($)
{
	my $self = shift;

	if ( $#{$self->{"items"}} >= 0 )
	{
		my $subrule = $self->{"items"}[0]->issubrule();
		return $subrule if defined $subrule;
	}

	return ();
}

sub additem($$)
{
	my ( $self, $item ) = @_;
	push @{$self->{"items"}}, $item;
	return $item;
}

sub code($$$)
{
	my ($self,$namespace,$rule) = @_;
	my $code =
'
	while (!$_matched'
	. (defined $self->{"uncommit"} ? '' : ' && !$commit')
	. ')
	{
		Parse::RecDescent::_trace(q{Trying production: ['
					  . $self->describe . '] at "}
					  . Parse::RecDescent::_tracefirst($_[1])
					  . q{"});
		my $thisprod = $thisrule->{"prods"}[' . $self->{"number"} . '];
		' . (defined $self->{"error"} ? '' : '$text = $_[1];' ) . '
		my $_savetext;
		my $tokensep;
		@item = ("' . $rule->{"name"} . '");

';

	my $item;
	foreach $item ( @{$self->{"items"}} )
	{
		$code .= $item->code($namespace,$rule);
	}

	$code .= 
'

		$_matched = 1;
		last;
	}

';
	return $code;
}

1;

package Parse::RecDescent::Action;

sub describe { undef }

sub new ($$$$)
{
	my $class = ref($_[0]) || $_[0];
	bless 
	{
		"code"      => $_[1],
		"lookahead" => $_[2],
		"line"      => $_[3],
	}, $class;
}

sub issubrule { undef }

sub code($$$)
{
	my ($self, $namespace) = @_;
	
'
		Parse::RecDescent::_trace(q{Trying action at "} .
					  Parse::RecDescent::_tracefirst($text) .
					  q{"});
		' . ($self->{"lookahead"} ? '$_savetext = $text;' : '' ) .'

		$_tok = ($_noactions) ? 0 : do ' . $self->{"code"} . ';
		' . ($self->{"lookahead"}<0?'if':'unless') . ' (defined $_tok)
		{
			Parse::RecDescent::_trace(q{Didn\'t match action (it returned an undefined value)});
			last;
		}
		Parse::RecDescent::_trace(q{Matched action. Return value was: }
					  . $_tok);
		push @item, $_tok;
		' . ($self->{"lookahead"} ? '$text = $_savetext;' : '' ) .'
'
}

1;

package Parse::RecDescent::Directive;

sub issubrule { undef }
sub describe { undef }

sub new ($$$$)
{
	my $class = ref($_[0]) || $_[0];
	bless 
	{
		"code"      => $_[1],
		"lookahead" => $_[2],
		"line"      => $_[3],
	}, $class;
}

sub code($$$)
{
	my ($self, $namespace) = @_;
	
'
		' . ($self->{"lookahead"} ? '$_savetext = $text;' : '' ) .'

		$_tok = do { ' . $self->{"code"} . ' };
		' . ($self->{"lookahead"} ? '$text = $_savetext and ' : '' ) .'
		last '
		. ($self->{"lookahead"}<0?'if':'unless') . ' defined $_tok;
		push @item, $_tok;
		' . ($self->{"lookahead"} ? '$text = $_savetext;' : '' ) .'
'
}

1;

package Parse::RecDescent::Error;

sub issubrule { undef }
sub describe { undef }

sub new ($$$$$)
{
	my $class = ref($_[0]) || $_[0];
	bless 
	{
		"msg"        => $_[1],
		"lookahead"  => $_[2],
		"commitonly" => $_[3],
		"line"       => $_[4],
	}, $class;
}

sub code($$$)
{
	my ($self, $namespace, $rule) = @_;
	
	my $action = '';
	
	if ($self->{"msg"})  # ERROR MESSAGE SUPPLIED
	{
		$action .= "Parse::RecDescent::_error(qq{$self->{msg}}" .
			    ',$thisline);'; 
	}
	else	  # GENERATE ERROR MESSAGE DURING PARSE
	{
		$action .= '
		my $rule = $item[0];
		   $rule =~ s/\Aimplicit_subrule_.\d*\Z/implicit subrule/;
		   $rule =~ s/_/ /g;
		Parse::RecDescent::_error("Invalid $rule: "
					  . $expectation->message()
			   		  ,$thisline);
		'; 
	}

	my $dir =
	      new Parse::RecDescent::Directive('if (' .
		($self->{"commitonly"} ? '$commit' : '1') . 
		") { do {$action} unless ".' $_noactions; undef } else {0}',
	        			$self->{"lookahead"}); 
	return $dir->code($namespace, $rule);
}

1;

package Parse::RecDescent::Token;

sub issubrule { undef }
sub describe ($)
{
	my $pat = $_[0]->{pattern};
	my $ldel = $_[0]->{ldelim};
	my $rdel = $_[0]->{rdelim};
	my $mod  = $_[0]->{mod};
	if ($pat=~s/\$$//) { return "m$ldel".quotemeta($pat).'\$'."$rdel$mod" }
	else               { return "m$ldel".quotemeta($pat)."$rdel$mod" }
}

# ARGS ARE: $self, $pattern, $left_delim, $modifiers, $lookahead, $linenum
sub new ($$$$$$)
{
	my $class = ref($_[0]) || $_[0];
	my $ldel = $_[2];
	my $rdel = $ldel;
	$rdel =~ tr/{[(</}])>/;
	my $mod = $_[3];

	if (!eval "'' =~ m$ldel$_[1]$rdel" and $@)
	{
		Parse::RecDescent::_error("Token pattern \"m$ldel$_[1]$rdel\"
					   is not a valid regular expression",
					   $_[5]);
		$@ =~ s/ at \(eval.*/./;
		Parse::RecDescent::_hint($@);
	}

	bless 
	{
		"pattern"   => $_[1],
		"ldelim"    => $ldel,
		"rdelim"    => $rdel,
		"mod"       => $mod,
		"lookahead" => $_[4],
		"line"      => $_[5],
	}, $class;
}

sub code($$$)
{
	my ($self, $namespace, $rule) = @_;
	my $ldel = $self->{"ldelim"};
	my $rdel = $self->{"rdelim"};
	my $sdel = $ldel;
	my $mod  = $self->{"mod"};

	$sdel =~ s/[[{(<]/{}/;
	
my $code = '
		Parse::RecDescent::_trace(q{Trying token: ' . $self->describe
					  . ' at "} 
					  . Parse::RecDescent::_tracefirst($text)
					  . q{"});
		$lastsep = "";
		$_toksep =
		    defined $tokensep		     ? $tokensep
		  : defined $thisprod->{"tokensep"}  ? $thisprod->{"tokensep"}
		  : defined $thisrule->{"tokensep"}  ? $thisrule->{"tokensep"}
		  : defined $thisparser->{"tokensep"}? $thisparser->{"tokensep"}
		  : defined $Parse::RecDescent::tokensep    ? $Parse::RecDescent::tokensep 
						     : $Parse::RecDescent::deftokensep;
		Parse::RecDescent::_trace(qq{tokensep: [$_toksep]});
		$expectation->is(q{' . ($rule->hasleftmost($self) ? ''
				: $self->describe ) . '})->at($text);
		' . ($self->{"lookahead"} ? '$_savetext = $text;' : '' ) . '

		' . ($self->{"lookahead"}<0?'if':'unless')
		. ' ($text =~ s/\A($_toksep)/$lastsep=$1 and ""/e and '
		. '  $text =~ s' . $ldel . '\A(?:' . $self->{"pattern"} . ')'
				 . $rdel . $sdel . $mod . ')
		{
			'.($self->{"lookahead"} ? '$text = $_savetext;' : '').'
			$expectation->failed();
			Parse::RecDescent::_trace(q{Failed on: [}
						  . Parse::RecDescent::_tracefirst($text)
						  . q{]});
			Parse::RecDescent::_trace(qq{Lastsep:   [$lastsep]});

			last;
		}
		push @item, $&;
		' . ($self->{"lookahead"} ? '$text = $_savetext;' : '' ) .'
';

	return $code;
}

1;

package Parse::RecDescent::Literal;

sub issubrule { undef }
sub describe ($) { "'$_[0]->{pattern}'" }

sub new ($$$$)
{
	my $class = ref($_[0]) || $_[0];

	my $pattern = $_[1];

	bless 
	{
		"pattern"   => $pattern,
		"lookahead" => $_[2],
		"line"      => $_[3],
	}, $class;
}

sub code($$$)
{
	my ($self, $namespace, $rule) = @_;
	
my $code = '
		Parse::RecDescent::_trace(q{Trying token: ' . $self->describe
					  . ' at "}
					  . Parse::RecDescent::_tracefirst($text)
					  . q{"});
		$lastsep = "";
		$_toksep =
		    defined $tokensep		     ? $tokensep
		  : defined $thisprod->{"tokensep"}  ? $thisprod->{"tokensep"}
		  : defined $thisrule->{"tokensep"}  ? $thisrule->{"tokensep"}
		  : defined $thisparser->{"tokensep"}? $thisparser->{"tokensep"}
		  : defined $Parse::RecDescent::tokensep    ? $Parse::RecDescent::tokensep 
						     : $Parse::RecDescent::deftokensep;
		Parse::RecDescent::_trace(qq{tokensep: [$_toksep]});
		$expectation->is(q{' . ($rule->hasleftmost($self) ? ''
				: $self->describe ) . '})->at($text);
		' . ($self->{"lookahead"} ? '$_savetext = $text;' : '' ) . '

		' . ($self->{"lookahead"}<0?'if':'unless')
		. ' ($text =~ s/\A($_toksep)/$lastsep=$1 and ""/e and '
		. '  $text =~ s/\A' . quotemeta($self->{"pattern"}) . '//)
		{
			'.($self->{"lookahead"} ? '$text = $_savetext;' : '').'
			$expectation->failed();
			Parse::RecDescent::_trace(qq{Failed on: [}
						  . Parse::RecDescent::_tracefirst($text) . q{]});
			Parse::RecDescent::_trace(qq{Lastsep:   [$lastsep]});
			last;
		}
		push @item, $&;
		' . ($self->{"lookahead"} ? '$text = $_savetext;' : '' ) .'
';

	return $code;
}

1;

package Parse::RecDescent::InterpLit;

sub issubrule { undef }
sub describe ($)
{
	my $pat = $_[0]->{pattern};
	$pat=~s/\\/\\\\/g;
	return '"' . eval("qq{$pat}") . '"';
}

sub new ($$$$)
{
	my $class = ref($_[0]) || $_[0];

	my $pattern = $_[1];
	$pattern =~ s#/#\\/#g;

	bless 
	{
		"pattern"   => $pattern,
		"lookahead" => $_[2],
		"line"      => $_[3],
	}, $class;
}

sub code($$$)
{
	my ($self, $namespace, $rule) = @_;
	
my $code = '
		Parse::RecDescent::_trace(q{Trying token: ' . $self->describe
					  . ' at "}
					  . Parse::RecDescent::_tracefirst($text)
					  . q{"});
		$lastsep = "";
		$_toksep =
		    defined $tokensep		     ? $tokensep
		  : defined $thisprod->{"tokensep"}  ? $thisprod->{"tokensep"}
		  : defined $thisrule->{"tokensep"}  ? $thisrule->{"tokensep"}
		  : defined $thisparser->{"tokensep"}? $thisparser->{"tokensep"}
		  : defined $Parse::RecDescent::tokensep    ? $Parse::RecDescent::tokensep 
						     : $Parse::RecDescent::deftokensep;
		Parse::RecDescent::_trace(qq{tokensep: [$_toksep]});
		$expectation->is(q{' . ($rule->hasleftmost($self) ? ''
				: $self->describe ) . '})->at($text);
		' . ($self->{"lookahead"} ? '$_savetext = $text;' : '' ) . '

		' . ($self->{"lookahead"}<0?'if':'unless')
		. ' ($text =~ s/\A($_toksep)/$lastsep=$1 and ""/e and '
		. '  $text =~ s/\A(?:' . $self->{"pattern"} . ')//)
		{
			'.($self->{"lookahead"} ? '$text = $_savetext;' : '').'
			$expectation->failed();
			Parse::RecDescent::_trace(qq{Failed on: [}
						  . Parse::RecDescent::_tracefirst($text)
						  . q{]});
			Parse::RecDescent::_trace(qq{Lastsep:   [$lastsep]});
			last;
		}
		push @item, $&;
		' . ($self->{"lookahead"} ? '$text = $_savetext;' : '' ) .'
';

	return $code;
}

1;

package Parse::RecDescent::Subrule;

sub issubrule ($) { return $_[0]->{"subrule"} }

sub describe ($) { $_[0]->{"implicit"} or $_[0]->{"subrule"} }

sub new ($$$$;$)
{
	my $class = ref($_[0]) || $_[0];
	bless 
	{
		"subrule"   => $_[1],
		"lookahead" => $_[2],
		"line"      => $_[3],
		"implicit"  => $_[4] || undef,
	}, $class;
}

sub code($$$)
{
	my ($self, $namespace, $rule) = @_;
	
'
		$expectation->is(q{' . ($rule->hasleftmost($self) ? ''
				: $self->describe ) . '})->at($text);
		' . ($self->{"lookahead"} ? '$_savetext = $text;' : '' ) .'

		' . ($self->{"lookahead"}<0?'if':'unless')
		. ' (defined ($_tok = $thisparser->'
		. $self->{"subrule"}
		. '($text,$_linenum,$repeating,'
		. ($self->{"lookahead"}?'1':'$_noactions')
		. ')))
		{
			'.($self->{"lookahead"} ? '$text = $_savetext;' : '').'
			$expectation->failed();
			last;
		}
		push @item, $_tok;
		' . ($self->{"lookahead"} ? '$text = $_savetext;' : '' ) .'
'
}

1;

package Parse::RecDescent::Repetition;

sub issubrule ($) { return $_[0]->{"subrule"} }
sub describe ($) { $_[0]->{"expected"} || $_[0]->{"subrule"} }

sub new ($$$$$$$$)
{
	my ($self, $subrule, $repspec, $min, $max, $lookahead, $line, $parser) = @_;
	my $class = ref($self) || $self;
	($max, $min) = ( $min, $max) if ($max<$min);

	my $desc;
	if ($subrule=~/\A_alternation_\d+_of_production_\d+_of_rule/)
		{ $desc = $parser->{"rules"}{$subrule}->expected }

	if ($lookahead)
	{
		if ($min>0)
		{
		   return new Parse::RecDescent::Subrule($subrule,$lookahead,$line,$desc);
		}
		else
		{
			Parse::RecDescent::_error("Not symbol (\"!\") before
				            \"$subrule($repspec)\" doesn't make
					    sense.",$line);
			Parse::RecDescent::_hint("Lookahead for negated optional
					   repetitions (such as
					   \"!$subrule($repspec)\" can never
					   succeed, since optional items always
					   match (zero times at worst). 
					   Did you mean a single \"!$subrule\", 
					   instead?");
		}
	}
	bless 
	{
		"subrule"   => $subrule,
		"repspec"   => $repspec,
		"min"       => $min,
		"max"       => $max,
		"lookahead" => $lookahead,
		"line"      => $line,
		"expected"  => $desc,
	}, $class;
}

sub code($$$)
{
	my ($self, $namespace, $rule) = @_;
	
	my ($subrule, $repspec, $min, $max, $lookahead) =
		@{$self}{ qw{subrule repspec min max lookahead} };

'
		$expectation->is(q{' . ($rule->hasleftmost($self) ? ''
				: $self->describe ) . '})->at($text);
		' . ($self->{"lookahead"} ? '$_savetext = $text;' : '' ) .'

		unless (defined ($_tok = $thisparser->_parserepeat($text, \&'
		. $namespace . '::' . $subrule
		. ', ' . $min . ', ' . $max . ', '
		. ($self->{"lookahead"}?'1':'$_noactions')
		. ',$_linenum,$expectation))) 
		{
			last;
		}
		push @item, $_tok;
		' . ($self->{"lookahead"} ? '$text = $_savetext;' : '' ) .'

'
}

1;

package Parse::RecDescent::Expectation;

sub new ($)
{
	bless {
		"failed"	  => 0,
		"expected"	  => "",
		"unexpected"	  => "",
		"lastexpected"	  => "",
		"lastunexpected"  => "",
		"defexpected"	  => $_[1],
	      };
}

sub is ($$)
{
	$_[0]->{lastexpected} = $_[1]; return $_[0];
}

sub at ($$)
{
	$_[0]->{lastunexpected} = $_[1]; return $_[0];
}

sub failed ($)
{
	return unless $_[0]->{lastexpected};
	$_[0]->{expected}   = $_[0]->{lastexpected}   unless $_[0]->{failed};
	$_[0]->{unexpected} = $_[0]->{lastunexpected} unless $_[0]->{failed};
	$_[0]->{failed} = 1;
}

sub message ($)
{
	my ($self) = @_;
	$self->{expected} = $self->{defexpected} unless $self->{expected};
	$self->{expected} =~ s/_/ /g;
	if (!$self->{unexpected} || $self->{unexpected} =~ /\A\s*\Z/s)
	{
		return "Was expecting $self->{expected}";
	}
	else
	{
		$self->{unexpected} =~ /\s*(.*)/;
		return "Was expecting $self->{expected} but found \"$1\" instead";
	}
}

1;

package Parse::RecDescent;

use Carp;
use vars qw ( $AUTOLOAD $VERSION );

$VERSION = 1.24;

# BUILDING A PARSER

my $nextnamespace = "namespace000001";

sub _nextnamespace()
{
	return "Parse::RecDescent::" . $nextnamespace++;
}

sub new ($$)
{
	my $class = ref($_[0]) || $_[0];
	my $self =
	{
		"rules"     => {},
		"namespace" => _nextnamespace(),
		"startcode" => '',
	};
	bless $self, $class;
	shift;
	return $self->Replace(@_)
}

sub DESTROY {}  # SO AUTOLOADER IGNORES IT

# BUILDING A GRAMMAR....

sub Replace ($$)
{
	splice(@_, 2, 0, 1);
	return _generate(@_);
}

sub Extend ($$)
{
	splice(@_, 2, 0, 0);
	return _generate(@_);
}

sub _no_rule ($$;$)
{
	_error("Ruleless $_[0] at start of grammar.",$_[1]);
	my $desc = $_[2] ? "\"$_[2]\"" : "";
	_hint("You need to define a rule for the $_[0] $desc
	       to be part of.");
}

my $NEGLOOKAHEAD	= '\A(\s*\.\.\.!)';
my $POSLOOKAHEAD	= '\A(\s*\.\.\.)';
my $RULE		= '\A\s*(\w+)\s*:';
my $PROD		= '\A\s*([|])';
my $TOKEN		= q{\A\s*/((\\\\/|[^/])+)/([gimsox]*)};
my $MTOKEN		= q{\A\s*m[^\w\s]};
my $LITERAL		= q{\A\s*'((\\\\'|[^'])+)'};
my $INTERPLIT		= q{\A\s*"((\\\\"|[^"])+)"};
my $SUBRULE		= '\A\s*(\w+)';
my $OPTIONAL		= $SUBRULE.'\((\?)\)';
my $ANY			= $SUBRULE.'\((s\?)\)';
my $MANY 		= $SUBRULE.'\((s|\.\.)\)';
my $EXACTLY		= $SUBRULE.'\(([1-9]\d*)\)';
my $BETWEEN		= $SUBRULE.'\((\d+)\.\.([1-9]\d*)\)';
my $ATLEAST		= $SUBRULE.'\((\d+)\.\.\)';
my $ATMOST		= $SUBRULE.'\(\.\.([1-9]\d*)\)';
my $BADREP		= "($SUBRULE".'\((-?\d+)?\.\.(-?\d+)?\))';
my $ACTION		= '\A\s*\{';
my $IMPLICITSUBRULE	= '\A\s*\(';
my $COMMENT		= '\A\s*(#.*)';
my $COMMITMK		= '\A\s*<commit>';
my $UNCOMMITMK		= '\A\s*<uncommit>';
my $REJECTMK		= '\A\s*<reject>';
my $CONDREJECTMK	= '\A\s*<reject:';
my $RESYNCMK		= '\A\s*<resync>';
my $RESYNCPATMK		= '\A\s*<resync:';
my $AUTOERRORMK		= '\A\s*<error(\??)>';
my $MSGERRORMK		= '\A\s*<error(\??):';
my $UNCOMMITPROD	= $PROD.'\s*(?=<uncommit)';
my $ERRORPROD		= $PROD.'\s*(?=<error)';
my $OTHER		= '\A\s*([^\s]+)';

my $lines = 0;

my $ERRORS = 0;

sub _generate($$$;$)
{
	my ($self, $grammar, $replace, $isimplicit) = (@_, 0);

	my $aftererror = 0;
	my $lookahead = 0;
	my $lookaheadspec = "";
	$lines = _linecount($grammar) unless $lines;
	my $line;

	my $rule = undef;
	my $prod = undef;
	my $item = undef;
	my $lastgreedy = '';

	while ($grammar !~ /^\s*$/)
	{
		$line = $lines - _linecount($grammar) + 1;
		my $commitonly;
		my $code = "";
		my @components = ();
		if ($grammar =~ s/$COMMENT//)
		{
			_parse("a comment",0,$line);
			next;
		}
		elsif ($grammar =~ s/$NEGLOOKAHEAD//)
		{
			_parse("a negative lookahead",$aftererror,$line);
			$lookahead = $lookahead ? -$lookahead : -1;
			$lookaheadspec .= $1;
			next;	# SKIP LOOKAHEAD RESET AT END OF while LOOP
		}
		elsif ($grammar =~ s/$POSLOOKAHEAD//)
		{
			_parse("a positive lookahead",$aftererror,$line);
			$lookahead = $lookahead ? $lookahead : 1;
			$lookaheadspec .= $1;
			next;	# SKIP LOOKAHEAD RESET AT END OF while LOOP
		}
		elsif ($grammar =~ m/$ACTION/
			and do {($code,$grammar) = extract_codeblock($grammar);
				$code })
		{
			_parse("an action", $aftererror, $line, $code);
			$item = new Parse::RecDescent::Action($code,$lookahead,$line);
			$prod and $prod->additem($item)
			      or  $self->_addstartcode($code);
		}
		elsif ($grammar =~ m/$IMPLICITSUBRULE/
			and do {($code,$grammar) = extract_codeblock($grammar,
				'{('); $code })
		{
			$code =~ s/\A\s*\(|\)\Z//g;
			_parse("an implicit subrule", $aftererror, $line,
				"( $code )");
			my $implicit = $rule->nextimplicit;
			$self->_generate("$implicit : $code",0,1);
			$grammar = $implicit . $grammar;
		}
		elsif ($grammar =~ s/$UNCOMMITPROD//)
		{
			_parseunneg("a new (uncommitted) production",
				    0, $lookahead, $line) or next;

			$prod = new Parse::RecDescent::Production($line,1,0);
			$rule and $rule->addprod($prod)
			      or  _no_rule("<uncommit>",$line);
			$aftererror = 0;
		}
		elsif ($grammar =~ s/$ERRORPROD//)
		{
			_parseunneg("a new (error) production", $aftererror,
				    $lookahead,$line) or next;
			$prod = new Parse::RecDescent::Production($line,0,1);
			$rule and $rule->addprod($prod)
			      or  _no_rule("<error>",$line);
			$aftererror = 0;
		}
		elsif ($grammar =~ s/$UNCOMMITMK//)
		{
			_parse("an uncommit marker", $aftererror,$line);
			$item = new Parse::RecDescent::Directive('$commit=0;1',
							  $lookahead,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("<uncommit>",$line);
		}
		elsif ($grammar =~ s/$REJECTMK//)
		{
			_parse("an reject marker", $aftererror,$line);
			$item = new Parse::RecDescent::Directive('undef', $lookahead,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("<reject>",$line);
		}
		elsif ($grammar =~ m/$CONDREJECTMK/
			and do { ($code,$grammar)
					= extract_codeblock($grammar,'<{');
				  $code })
		{
			_parse("a (conditional) reject marker", $aftererror,$line);
			$code =~ /\A\s*<reject:(.*)>\Z/s;
			$item = new Parse::RecDescent::Directive(
				      "($1) ? undef : 1", $lookahead,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("<reject:$code>",$line);
		}
		elsif ($grammar =~ s/$RESYNCMK//)
		{
			_parse("a resync to newline marker", $aftererror,$line);
			$item = new Parse::RecDescent::Directive(
				      'if ($text =~ s/[^\n]*\n//) { $return = 0; $& } else { undef }',
				      $lookahead,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("<resync>",$line);
		}
		elsif ($grammar =~ m/$RESYNCPATMK/
			and do { ($code,$grammar)
					= extract_bracketed($grammar,'<');
				  $code })
		{
			_parse("a resync with pattern marker", $aftererror,$line);
			$code =~ /\A\s*<resync:(.*)>\Z/s;
			$item = new Parse::RecDescent::Directive(
				      "($1) ? undef : ".'$&', $lookahead,$line);
				      'if ($text =~ s/'.$1.'//) { $return = 0; $& } else { undef }',
			$item = new Parse::RecDescent::Directive(
				      '$text =~ s/'.$code.'// or undef',
				      $lookahead,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("<reject:$code>",$line);
		}
		elsif ($grammar =~ s/$COMMITMK//)
		{
			_parse("an commit marker", $aftererror,$line);
			$item = new Parse::RecDescent::Directive('$commit = 1',
							  $lookahead,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("<commit>",$line);
		}
		elsif ($grammar =~ s/$AUTOERRORMK//)
		{
			_parse("an error marker", $aftererror,$line);
			$item = new Parse::RecDescent::Error('',$lookahead,$1,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("<error>",$line);
			$aftererror = !$commitonly;
		}
		elsif ($grammar =~ m/$MSGERRORMK/
			and do { $commitonly = $1;
				($code,$grammar)
					= extract_bracketed($grammar,'<');
				$code })
		{
			_parse("an error marker", $aftererror,$line,$code);
			$code =~ /\A\s*<error\??:(.*)>\Z/s;
			$item = new Parse::RecDescent::Error($1,$lookahead,$commitonly,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("$code",$line);
			$aftererror = !$commitonly;
		}
		elsif ($grammar =~ s/$RULE//)
		{
			_parseunneg("a rule declaration", 0,
				    $lookahead,$line) or next;
			if ($1 eq "Replace" || $1 eq "Extend")
			{	
				_warn("Rule \"$1\" hidden by method
				       Parse::RecDescent::$1",$line);
				_hint("The rule named \"$1\" cannot be directly
                                       called through the Parse::RecDescent object
                                       for this grammar (although it may still
                                       be used as a subrule of other rules).
                                       It can't be directly called because
				       Parse::RecDescent::$1 is already defined (it
				       is the standard method used to
				       dynamically \L$1\E rules in a parser).");
			}
			$rule = new Parse::RecDescent::Rule($1,$self,$line,$replace);
			$prod = $rule->addprod( new Parse::RecDescent::Production );
			$aftererror = 0;
		}
		elsif ($grammar =~ s/$PROD//)
		{
			_parseunneg("a new production", 0,
				    $lookahead,$line) or next;
			$rule
			  and $prod = $rule->addprod(new Parse::RecDescent::Production($line))
			or  _no_rule("production",$line);
			$aftererror = 0;
		}
		elsif ($grammar =~ s#$LITERAL##)
		{
			_parse("a literal token", $aftererror,$line);
			$item = new Parse::RecDescent::Literal($1,$lookahead,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("literal token",$line,"'$1'");
		}
		elsif ($grammar =~ s#$INTERPLIT##)
		{
			_parse("an interpolated literal token", $aftererror,$line);
			$item = new Parse::RecDescent::InterpLit($1,$lookahead,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("interpolated literal token",$line,"'$1'");
		}
		elsif ($grammar =~ s#$TOKEN##)
		{
			_parse("a pattern token", $aftererror,$line);
			$item = new Parse::RecDescent::Token($1,'/',$3?$3:'',$lookahead,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("regex token",$line,"/$1/");
		}
		elsif ($grammar =~ m#$MTOKEN#
			and do { ($code,$grammar,@components)
					= extract_quotelike($grammar);
				 $code }
		      )

		{
			_parse("a pattern token", $aftererror,$line,$code);
			$item = new Parse::RecDescent::Token(@components[3,2,8],
							     $lookahead,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("regex token",$line,$code);
		}
		elsif ($grammar =~ s/$OPTIONAL//)
		{
			_parse("an zero-or-one subrule match", $aftererror,$line);
			$item = new Parse::RecDescent::Repetition($1,$2,0,1,
							   $lookahead,$line,
							   $self);
			$prod and $prod->additem($item)
			      or  _no_rule("repetition",$line,"$1($2)");

			$rule and $rule->addcall($1);
		}
		elsif ($grammar =~ s/$ANY//)
		{
			_parse("a zero-or-more subrule match", $aftererror,$line);
			$item = new Parse::RecDescent::Repetition($1,$2,0,$MAXREP,
							   $lookahead,$line,
							   $self);
			$prod and $prod->additem($item)
			      or  _no_rule("repetition",$line,"$1($2)");

			$rule and $rule->addcall($1);

			_check_insatiable($1,$2,$grammar,$line);
		}
		elsif ($grammar =~ s/$MANY//)
		{
			_parse("a one-or-more subrule match", $aftererror,$line);
			$item = new Parse::RecDescent::Repetition($1,$2,1,$MAXREP,
							   $lookahead,$line,
							   $self);
							   
			$prod and $prod->additem($item)
			      or  _no_rule("repetition",$line,"$1($2)");

			$rule and $rule->addcall($1);

			_check_insatiable($1,$2,$grammar,$line);
		}
		elsif ($grammar =~ s/$EXACTLY//)
		{
			_parse("an exactly-$2-times subrule match", $aftererror,$line);
			$item = new Parse::RecDescent::Repetition($1,$2,$2,$2,
							   $lookahead,$line,
							   $self);
			$prod and $prod->additem($item)
			      or  _no_rule("repetition",$line,"$1($2)");

			$rule and $rule->addcall($1);
		}
		elsif ($grammar =~ s/$BETWEEN//)
		{
			_parse("a $2-to-$3 subrule match", $aftererror,$line);
			$item = new Parse::RecDescent::Repetition($1,"$2..$3",$2,$3,
							   $lookahead,$line,
							   $self);
			$prod and $prod->additem($item)
			      or  _no_rule("repetition",$line,"$1($2..$3)");

			$rule and $rule->addcall($1);
		}
		elsif ($grammar =~ s/$ATLEAST//)
		{
			_parse("a $2-or-more subrule match", $aftererror,$line);
			$item = new Parse::RecDescent::Repetition($1,"$2..",$2,$MAXREP,
							   $lookahead,$line,
							   $self);
			$prod and $prod->additem($item)
			      or  _no_rule("repetition",$line,"$1($2..)");

			$rule and $rule->addcall($1);

			_check_insatiable($1,"$2..",$grammar,$line);
		}
		elsif ($grammar =~ s/$ATMOST// 			)
		{
			_parse("a one-to-$2 subrule match", $aftererror,$line);
			$item = new Parse::RecDescent::Repetition($1,"..$2",1,$2,
							   $lookahead,$line,
							   $self);
			$prod and $prod->additem($item)
			      or  _no_rule("repetition",$line,"$1(..$2)");

			$rule and $rule->addcall($1);
		}
		elsif ($grammar =~ s/$BADREP// 			)
		{
			_parse("an subrule match with invalid repetition specifier", 0,$line);
			_error("Incorrect specification of a repeated subrule",
			       $line);
			_hint("Repeated subrules like \"$1\" cannot have
			       a maximum repetition of zero, nor can they have
			       negative components in their ranges.");
		}
		elsif ($grammar =~ s/$SUBRULE// )
		{
			_parse("a subrule match", $aftererror,$line);
			# EXPERIMENTAL
			my $name = $1;
			my $desc;
			if ($name=~/\A_alternation_\d+_of_production_\d+_of_rule/)
				{ $desc = $self->{"rules"}{$name}->expected }
			$item = new Parse::RecDescent::Subrule($name,$lookahead,$line,$desc);
			$prod and $prod->additem($item)
			      or  _no_rule("(sub)rule",$line,$name);

			$rule and $rule->addcall($name);
		}
		elsif ($grammar =~ s/$OTHER//   )
		{
			_error("Untranslatable item encountered: \"$1\"",
			       $line);
			_hint("Did you misspell \"$1\"
			           or forget to comment it out?");
		}

		if ($lookaheadspec =~ tr /././ > 3)
		{
			$lookaheadspec =~ s/\A\s+//;
			$lookahead = $lookahead<0
					? 'a negative lookahead ("...!")'
					: 'a positive lookahead ("...")' ;
			_warn("Found two or more lookahead specifiers in a
			       row.",$line);
			_hint("Multiple positive and/or negative lookaheads
			       are simply multiplied together to produce a
			       single positive or negative lookahead
			       specification. In this case the sequence
			       \"$lookaheadspec\" was reduced to $lookahead.
			       Was this your intention?");
		}
		$lookahead = 0;
		$lookaheadspec = "";
	}

	unless ($ERRORS or $isimplicit)
	{
		$self->_check_grammar();
	}

	unless ($ERRORS or $isimplicit)
	{
		my $code = $self->_code();
		if (defined $::RD_TRACE)
		{
			print STDERR "printing code (", length($code),") to RD_TRACE\n";
			open TRACE_FILE, ">RD_TRACE"
			and print TRACE_FILE $code
			and close TRACE_FILE;
		}
		unless ( eval "$code 1" )
		{
			_error("Internal error in generated parser code!");
			$@ =~ s/at grammar/in grammar at/;
			_hint($@);
		}
	}

	if ($ERRORS and !_verbosity("HINT"))
	{
		$::RD_HINT = 1;
		_hint('Set $::RD_HINT (or -RD_HINT if you\'re using "perl -s")
		       for hints on fixing these problems.');
		undef $::RD_HINT;
	}
	return $ERRORS ? undef : $self;
}

sub _addstartcode($$)
{
	my ($self, $code) = @_;
	$code =~ s/\A\s*\{(.*)\}\Z/$1/s;

	$self->{"startcode"} .= "$code;\n";
}

# CHECK FOR GRAMMAR PROBLEMS....

sub _check_insatiable($$$$)
{
	my ($subrule,$repspec,$grammar,$line) = @_;
	#return unless $repspec =~ /s|\.\.\Z/;
	return if $grammar =~ /$OPTIONAL/ || $grammar =~ /$ANY/;
	my $min = 1;
	if ( $grammar =~ /$MANY/
	  || $grammar =~ /$EXACTLY/
	  || $grammar =~ /$ATMOST/ 
	  || $grammar =~ /$BETWEEN/ && do { $min=$2; 1 }
	  || $grammar =~ /$ATLEAST/ && do { $min=$2; 1 }
	  || $grammar =~ /$SUBRULE(?!\s*:)/
	   )
	{
		return unless $1 eq $subrule && $min > 0;
		_warn("Subrule sequence \"$subrule($repspec) $&\" will
		       (almost certainly) fail.",$line);
		_hint("Unless subrule \"$subrule\" performs some cunning
		       lookahead, the repetition \"$subrule($repspec)\" will
		       insatiably consume as many matches of \"$subrule\" as it
		       can, leaving none to match the \"$&\" that follows.");
	}
}

sub _check_grammar ($)
{
	my $self = shift;
	my $rules = $self->{"rules"};
	my $rule;
	foreach $rule ( values %$rules )
	{
		next if ! $rule->{"changed"};

	# CHECK FOR UNDEFINED RULES

		my $call;
		foreach $call ( @{$rule->{"calls"}} )
		{
			if (!defined ${$rules}{$call})
			{
				if (!defined $::RD_AUTOSTUB)
				{
					_warn("Undefined (sub)rule \"$call\"
					       used in a production.");
					_hint("Will you be providing this rule
					       later, or did you perhaps
					       misspell \"$call\"? Otherwise
					       it will be treated as an 
					       immediate <reject>.");
					eval "sub $self->{namespace}::$call {undef}";
				}
				else	# EXPERIMENTAL
				{
					_warn("Autogenerating rule: $call");
					_hint("A call was made to a subrule
					       named \"$call\", but no such
					       rule was specified. However,
					       since \$::RD_AUTOSTUB
					       was defined, a rule stub
					       ($call : '$call') was
					       automatically created.");

					$self->_generate("$call : '$call'",0,1);
				}
			}
		}

	# CHECK FOR LEFT RECURSION

		if ($rule->isleftrec($rules))
		{
			_error("Rule \"$rule->{name}\" is left-recursive.");
			_hint("Redesign the grammar so it's not left-recursive.
			       That will probably mean you need to re-implement
			       repetitions using the '(s)' notation.
			       For example: \"$rule->{name}(s)\".");
			next;
		}
	}
}
	
# GENERATE ACTUAL PARSER CODE

sub _code($)
{
	my $self = shift;
	my $code = "package $self->{namespace};\n$self->{startcode}";
	$self->{"startcode"} = '';

	my $rule;
	foreach $rule ( values %{$self->{"rules"}} )
	{
		if ($rule->{"changed"})
		{
			$code .= $rule->code($self->{"namespace"});
			$rule->{"changed"} = 0;
		}
	}

	return $code;
}

# EXECUTING A PARSE....

sub AUTOLOAD
{
	die "Could not find method: $AUTOLOAD\n" unless ref $_[0];
	my $class = ref($_[0]) || $_[0];
	$AUTOLOAD =~ s/$class/$_[0]->{namespace}/;
	goto &$AUTOLOAD;
}

sub _parserepeat($$$$$$$$$)	# RETURNS A REF TO AN ARRAY OF MATCHES
{
	my ($parser, $text, $prod, $min, $max, $_noactions, $_linenum) = @_;
	my @tokens = ();
	
	my $reps;
	for ($reps=0; $reps<$max;)
	{
		$_[7]->at($text);	 # $_[7] IS $expectation FROM CALLER
		my $_savetext = $text;
		my $prevtextlen = length $text;
		my $_tok;
		if (! defined ($_tok = &$prod($parser,$text,$_linenum,1,$_noactions)))
		{
			$text = $_savetext;
			last;
		}
		last if ++$reps >= $min and $prevtextlen == length $text;
		push @tokens, $_tok;
	}

	do { $_[7]->failed(); return undef} if $reps<$min;

	$_[1] = $text;
	return [@tokens];
}


# ERROR REPORTING....

my $errortext;
my $errorprefix;

open (ERROR, ">&STDERR");
format ERROR =
@>>>>>>>>>>>>>>>>>>>>: ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$errorprefix,          $errortext
~~                     ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                       $errortext
.

select ERROR;
$| = 1;
select STDOUT;

sub _verbosity($)
{
	   defined $::RD_TRACE
	or defined $::RD_HINT    and  $_[0] =~ /ERRORS|WARN|HINT/
	or defined $::RD_WARN    and  $_[0] =~ /ERRORS|WARN/
	or defined $::RD_ERRORS  and  $_[0] =~ /ERRORS/
}

sub _error($;$)
{
	$ERRORS++;
	return if ! _verbosity("ERRORS");
	$errortext   = $_[0];
	$errorprefix = "ERROR" .  ($_[1] ? " (line $_[1])" : "");
	$errortext =~ s/\s+/ /g;
	print ERROR "\n" if _verbosity("WARN");
	write ERROR;
}

sub _warn($;$)
{
	return if !_verbosity("WARN");
	$errortext   = $_[0];
	$errorprefix = "Warning" .  ($_[1] ? " (line $_[1])" : "");
	print ERROR "\n";
	$errortext =~ s/\s+/ /g;
	write ERROR;
}

sub _hint($)
{
	return unless defined $::RD_HINT;
	$errortext = "$_[0])";
	$errorprefix = "(Hint";
	$errortext =~ s/\s+/ /g;
	write ERROR;
}

sub _tracemax($)
{
	if (defined $::RD_TRACE
	    && $::RD_TRACE =~ /\d+/
	    && $::RD_TRACE>1
	    && $::RD_TRACE+10<length($_[0]))
	{
		my $count = length($_[0]) - $::RD_TRACE;
		return substr($_[0],0,$::RD_TRACE/2)
			. "...<$count>..."
			. substr($_[0],-$::RD_TRACE/2);
	}
	else
	{
		return $_[0];
	}
}

sub _tracefirst($)
{
	if (defined $::RD_TRACE
	    && $::RD_TRACE =~ /\d+/
	    && $::RD_TRACE>1
	    && $::RD_TRACE+10<length($_[0]))
	{
		my $count = length($_[0]) - $::RD_TRACE;
		return substr($_[0],0,$::RD_TRACE) . "...<+$count>";
	}
	else
	{
		return $_[0];
	}
}

sub _trace($;$)
{
	return unless _verbosity("TRACE");
	$errortext   = $_[0];
	$errorprefix = "TRACE" . ($_[1] ? "(line $_[1]) " : "");
	$errortext =~ s/\s+/ /g;
	write ERROR;
}

sub _parseunneg($$$$)
{
	_parse($_[0],$_[1],$_[3]);
	if ($_[2]<0)
	{
		_error("Can't negate \"$&\".",$_[3]);
		_hint("You can't negate $_[0]. Remove the \"...!\" before
		       \"$&\".");
		return 0;
	}
	return 1;
}

sub _parse($$$;$)
{
	my $what = $_[3] || $&;
	   $what =~ s/^\s+//;
	if ($_[1])
	{
		_warn("Found $_[0] ($what) after an unconditional <error>",$_[2]);
		_hint("An unconditional <error> always causes the
		       production containing it to immediately fail.
		       \u$_[0] which follows an <error>
		       will never be reached.  Did you mean to use
		       <error?> instead?");
	}

	return if ! _verbosity("TRACE");
	$errortext = "Treating \"$what\" as $_[0]";
	$errorprefix = "Parse::RecDescent";
	$errortext =~ s/\s+/ /g;
	write ERROR;
}

sub _linecount($)
{
	my $string = $_[0];
	return scalar ( $string =~ tr/\n// );
}

package main;

use vars qw ( $RD_ERRORS $RD_WARN $RD_HINT $RD_TRACE );
$RD_ERRORS = 1;

1;
