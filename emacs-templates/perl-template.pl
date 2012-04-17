#!/usr/local/bin/perl -CS

use 5.014;
use utf8;
use strict;
use warnings;

use version; our $VERSION = qv(0.0_1);

use Data::Dumper; $Data::Dumper::Indent = 1;

binmode(STDIN, ':encoding(utf-8)');
binmode(STDOUT, ':encoding(utf-8)');

use lib './lib';

