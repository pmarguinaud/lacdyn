#!/usr/bin/perl -w
#
#
use strict;
use FileHandle;
use Data::Dumper;
use FindBin qw ($Bin);
use lib "$Bin/../scripts";
use Fxtran;
use Blocks;

use Carp;
$SIG{__DIE__} = \&Carp::confess;

my ($f) = @ARGV;

my $d = &Fxtran::fxtran (location => $f);

&Blocks::addDataDirectivesNonJlonVariables ($d);

'FileHandle'->new (">$f.new")->print ($d->textContent);
