#!/usr/local/bin/perl 

use 5.014;
use utf8;
use strict;
use warnings;

use version; our $VERSION = qv(0.0_1);

use Data::Dumper; $Data::Dumper::Indent = 1;

use Test::More;
my $builder = Test::More->builder;
binmode $builder->output,         ":utf8";
binmode $builder->failure_output, ":utf8";
binmode $builder->todo_output,    ":utf8";

# BEGIN { use_ok('Some::Module'); }

ok(1);

done_testing();
