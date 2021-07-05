#!/usr/bin/perl -w
#
#
use strict;
use FileHandle;
use Data::Dumper;
use FindBin qw ($Bin);
use lib "$Bin/../scripts";
use Fxtran;

use Carp;
$SIG{__DIE__} = \&Carp::confess;


sub getIndent
{
  # get statement indentation

  my $stmt = shift;

  my $indent = 0;

  my $n = $stmt->previousSibling;

  unless ($n)
    {
      if ($stmt->parentNode)
        {
          return &getIndent ($stmt->parentNode);
        }
      $indent = 0;
      goto RETURN;
    }


  if (($n->nodeName eq '#text') && ($n->data =~ m/\n/o))
    {
      (my $t = $n->data) =~ s/^.*\n//smo;
      $indent = length ($t);
      goto RETURN;
    }

  $indent = 0;

RETURN:

  return $indent;
}

sub reIndent
{
  my ($node, $ns) = @_;

  my $sp = ' ' x $ns;

  my @cr = &f ('.//text ()[contains (.,"' . "\n" . '")]', $node);

  for my $cr (@cr)
    {
      (my $t = $cr->data) =~ s/\n/\n$sp/g;
      $cr->setData ($t);
    }

}

my ($f, $f2, @n2) = @ARGV;

my $d2 = &Fxtran::fxtran (location => $f2);

my ($n2) = &f ('.//f:subroutine-stmt/f:subroutine-N/f:N/f:n/text ()', $d2);

my @da2 = &f ('.//f:subroutine-stmt//f:dummy-arg-LT/f:arg-N/f:N/f:n/text ()', $d2, 1);

my @i = map { my $n2 = $_; grep { my $i = $_; ($n2 eq $da2[$i]) } (0 .. $#da2) } @n2;

$d2 = undef;




my $d = &Fxtran::fxtran (location => $f);

my @call = &f ('.//f:call-stmt[./f:procedure-designator/f:named-E[string (.)="?"]]', $n2, $d);

for my $call (@call)
  {
    my @e = &f ('./f:arg-spec/f:arg/*', $call);
    @e = @e[@i];

    for my $e (@e)
      {
        my ($n) = &f ('./f:N/f:n/text ()', $e, 1);
        my ($rlt) = &f ('./f:R-LT', $e);
        my ($sslt) = &f ('./f:R-LT/f:array-R/f:section-subscript-LT', $e);
        my @ss = &f ('./f:section-subscript', $sslt);
        my $r = scalar (@ss);
        my ($idx) = &f ('./f:lower-bound/*', $ss[-1]);
        my $len = &n ("<named-E><N><n>SIZE</n></N> <R-LT><parens-R>(<element-LT><element><named-E><N><n>$n</n></N></named-E></element>, "
                    . "<element><literal-E><l>$r</l></literal-E></element></element-LT>)</parens-R></R-LT></named-E>");

        my $arg = $e->parentNode;

        $arg->parentNode->insertAfter ($idx, $arg);
        $arg->parentNode->insertAfter (&t (','), $arg);
        $arg->parentNode->insertAfter ($len, $arg);
        $arg->parentNode->insertAfter (&t (','), $arg);

        $rlt->unbindNode ();

      }

  }

'FileHandle'->new (">$f.new")->print ($d->textContent);
