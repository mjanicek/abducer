#!/usr/bin/perl
#
# Use this followingly:
#   ./rcc-gen.pl < rcc-gen.data

use warnings;
use strict;

my @sequence = ("dc", "ec", "po", "tpp", "ntpp", "tppi", "ntppi", "eq");

my $l = 0;

while (<>) {
	chomp;
	my @cols = split /\t/;

	for (my $r = 0; $r < 8; $r++) {
#		print STDERR $sequence[$l] . " o " . $sequence[$r] . " = " . $cols[$r] . "\n";
		my @possible = split /,/, $cols[$r];

		my $suffix = "";
		foreach my $value (@possible) {
			if ($value eq "*") {
				$value = "Any";
				$suffix = " :- any_relation(Any)";
			}
			else {
				$suffix = "";
			}

			print "compose($sequence[$l], $sequence[$r], $value)$suffix.\n";
		}
	}
	print "\n";
	$l++;
}
