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

sub getShapeSpecList
{
  my ($var, $doc, %opts) = @_; 

  my $cr = $opts{create};

  my @en_decl = &f ('.//f:EN-decl[./f:EN-N/f:N/f:n/text ()="' . $var . '"]', $doc);

  my $sslt;

  # Update dimensions : look for existing array spec

  for my $en_decl (@en_decl)
    {   
      ($sslt) = &f ('./f:array-spec/f:shape-spec-LT', $en_decl);
      if ($sslt)
        {
          last;
        }
    }   

  # No dimensions: add array spec

  if ((! $sslt) && ($cr))
    {   
      for my $en_decl (@en_decl)
        {
          my $as = $en_decl->appendChild (&n ("<array-spec/>"));
          $as->appendChild (&t ("("));
          $sslt = $as->appendChild (&n ("<shape-spec-LT/>"));
          $as->appendChild (&t (")"));
          last;
        }
    }   

  return $sslt;
}

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

my ($f1, $f2) = @ARGV;

my $d1 = &Fxtran::fxtran (location => $f1);
my $d2 = &Fxtran::fxtran (location => $f2);

my ($n2) = &f ('.//f:subroutine-stmt/f:subroutine-N/f:N/f:n/text ()', $d2);

my @a2 = &f ('.//f:subroutine-stmt//f:dummy-arg-LT/f:arg-N/f:N/f:n/text ()', $d2, 1);

my @call = &f ('.//f:call-stmt[./f:procedure-designator/f:named-E[string (.)="?"]]', $n2, $d1);

for my $call (@call)
  {
    my @e1 = &f ('./f:arg-spec/f:arg/*', $call);

    for my $i (0 .. $#e1)
      {   
        my $e1 = $e1[$i];

        my ($rlt1) = &f ('./f:R-LT', $e1);
        my ($sslt1) = &f ('./f:R-LT/f:array-R/f:section-subscript-LT', $e1);

        next unless ($sslt1);

print $e1->textContent, "\n";

        my $a2 = $a2[$i];

        my $sslt2 = &getShapeSpecList ($n2, $d2);

        print $sslt1->textContent, "\n", $sslt2->textContent, "\n\n";

#       my ($n) = &f ('./f:N/f:n/text ()', $e, 1); 
#       my ($rlt) = &f ('./f:R-LT', $e);
#       my ($sslt) = &f ('./f:R-LT/f:array-R/f:section-subscript-LT', $e);
#       my @ss = &f ('./f:section-subscript', $sslt);
#       my $r = scalar (@ss);
#       my ($idx) = &f ('./f:lower-bound/*', $ss[-1]);
#       my $len = &n ("<named-E><N><n>SIZE</n></N> <R-LT><parens-R>(<element-LT><element><named-E><N><n>$n</n></N></named-E></element>, "
#                   . "<element><literal-E><l>$r</l></literal-E></element></element-LT>)</parens-R></R-LT></named-E>");

#       my $arg = $e->parentNode;

#       $arg->parentNode->insertAfter ($idx, $arg);
#       $arg->parentNode->insertAfter (&t (','), $arg);
#       $arg->parentNode->insertAfter ($len, $arg);
#       $arg->parentNode->insertAfter (&t (','), $arg);

#       $rlt->unbindNode (); 

      }   

  }



'FileHandle'->new (">$f1.new")->print ($d1->textContent);
'FileHandle'->new (">$f2.new")->print ($d2->textContent);
