package Perl::Template;

use 5.014;
use utf8;
use strict;
use warnings;

use Data::Dumper; $Data::Dumper::Indent = 1;

binmode(STDIN, ':encoding(utf-8)');
binmode(STDOUT, ':encoding(utf-8)');
binmode(STDERR, ':encoding(utf-8)');

=head1 NAME

=head1 VERSION

=head1 SYNOPSIS

=head1 DESCRIPTION

=cut

BEGIN {
  use version; our $VERSION = qv(0.0_1);
}

=head1 METHODS

=over

=item B<<  __PACKAGE->new() >>

=cut

sub new {
    my $class = shift;
    my $self = {};
    bless $self, $class;

    $self->init(@_);

    return $self;
}

=item B<< $obj->init() >>

=cut

sub init {
    my $self = shift;
    # $self->SUPER::init();
}

=item B<< $obj->type >>

Return the type of the object. This method is only really useful for
Token and Node subtypes.

=cut

sub type {
    my $self  = shift;
    my $class = ref $self;
    $class =~ m/::(\w+)$/;
    return $1;
}

=item B<< AUTOLOAD >>

Accessors (getters and setters) are autoloaded.

=cut

sub AUTOLOAD {
    my ($name) = our $AUTOLOAD =~ /::(\w+)$/;
    my $method = sub {
        my $self = shift;
        if (!exists $self->{$name}) {
            die "Cannot access $AUTOLOAD called from "
              . join(":", caller()) . "\n";
        }
        if (@_) {
            return $self->{$name} = shift @_;
        } else {
            return $self->{$name};
        }
    };
    {
        ## no critic (ProhibitNoStrict)
        no strict 'refs';
        *{$AUTOLOAD} = $method;
    }
    goto &$method;
}

=item B<< $obj->DESTORY() >>

Mostly just a placeholder, because Perl gets confused
with the Autoload stuff above.

=cut

sub DESTROY { }

=item B<< $obj->method() >>

=cut

sub method {
    my $self = shift;
}

1;

=back

=head1 DIAGNOSTICS

None. All diagnostics are provided by sub-modules.

=head1 LICENCE AND COPYRIGHT

Copyright (c) 2012, Michael S. Joyce ubermichael@gmail.com. All rights
reserved.

This package is free software; you can use, modify, and redistribute
copies of this software under the terms of the GNU General Public
License version 2, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License
Version 2 along with ISE Tidy.  If not, see
L<GPL v2|http://www.gnu.org/licenses/gpl-2.0.txt>

=head1 DISCLAIMER

ISE Tidy is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.
