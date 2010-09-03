#!/usr/bin/env perl

use strict;
use warnings;

if ($#ARGV != 1) {
	die "usage: generate NUM-BELIEFS NUM-DISTRIBS-IN-EACH-BELIEF";
}

my $num = $ARGV[0];
my $d_num = $ARGV[1];

my @world_exist_func = ();
for (my $i = 0; $i < $num; $i++) {
	my $add = 0.75;
	for (my $j = 0; $j < $d_num; $j++) {
		push @world_exist_func, "(bel : w(b_gen$i, d$j)) = p(" . $add .")";
		$add = $add / 4.0;
	}
}
print "world_exist = [\n\t" . join(",\n\t", @world_exist_func) . "\n].\n";

print "\n";

my @belief_exist_func = ();
for (my $i = 0; $i < $num; $i++) {
	my $prob = rand();
	push @belief_exist_func, "(bel : b(b_gen$i)) = p(" . sprintf("%.3f", $prob) .")";
}
print "belief_exist = [\n\t" . join(",\n\t", @belief_exist_func) . "\n].\n";

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

		my @belex = (
			"bel : w(b_gen$i, d$j) / world_exist",
			"bel : b(b_gen$i) / belief_exist"
		);

		print "bel : color(b_gen$i, known(" . $colors[($i + $j) % 4] . ")) <- " . join(", ", @belex) . ".\n";
		print "bel : shape(b_gen$i, known(" .$shapes[($i + $j) % 3] . ")) <- " . join(", ", @belex) . ".\n";
	}
}
