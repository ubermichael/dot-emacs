package Perl::Template;

use 5.014;
use utf8;
use strict;
use warnings;

use Data::Dumper; $Data::Dumper::Indent = 1;

binmode(STDIN, ':encoding(utf-8)');
binmode(STDOUT, ':encoding(utf-8)');

use version; our $VERSION = qv(0.0_1);
our $AUTOLOAD;

sub new {
    my $class = shift;
    my $self = {};
    bless $self, $class;

    $self->init(@_); 
    
    return $self;
}

sub init {
    my $self = shift;
    # $self->SUPER::init();
}

sub AUTOLOAD {
    my $self = shift;
    my $class = ref($self);
    (my $name = $AUTOLOAD) =~ s/.*:://;
    unless (exists $self->{$name}) {
        die "cannot access ${class}::${name}() called from " . join(":", caller());
    }
    if(@_) {
        return $self->{$name} = shift @_;
    } else {
        return $self->{$name};
    }    
}

sub DESTROY { }

sub method {
    my $self = shift;
}

1;    
