#!/usr/bin/env perl

use strict;
use warnings;

my $num = 70;
my $d_num = 3;

print "%world_exist = [\n";
for (my $i = 0; $i < $num; $i++) {
	my $add = 0.75;
	for (my $j = 0; $j < $d_num; $j++) {
		print "\t(bel : w(b_gen$i, d$j)) = p(" . $add ."),\n";
		$add = $add / 4.0;
	}
}

print "\n";

print "%belief_exist = [\n";
for (my $i = 0; $i < $num; $i++) {
	my $prob = rand();
	print "\t(bel : b(b_gen$i)) = p(" . $prob ."),\n";
}

print "\n";

for (my $i = 0; $i < $num; $i++) {

	my $bel_id = "b_gen$i";

	my @ds = ();
	for (my $j = 0; $j < $d_num; $j++) {
		push @ds, "bel: w($bel_id, d$j)";
	}

	print "disjoint([\n\t" . join(",\n\t", @ds) . "\n]).\n";
}

print "\n";

my @colors = ("blue", "red", "green", "yellow");
my @shapes = ("compact", "elongated", "round");

for (my $i = 0; $i < $num; $i++) {
	for (my $j = 0; $j < $d_num; $j++) {
		print "bel : color(b_gen$i, known(" . $colors[($i + $j) % 4] . ")) <- bel : w(b_gen$i, d$j) / world_exist, bel : b(b_gen$i) / belief_exist.\n";
		print "bel : shape(b_gen$i, known(" .$shapes[($i + $j) % 3] . ")) <- bel : w(b_gen$i, d$j) / world_exist, bel : b(b_gen$i) / belief_exist.\n";
	}
}
