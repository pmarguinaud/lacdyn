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

my ($f, @n) = @ARGV;

my $d = &Fxtran::fxtran (location => $f);

for my $n (@n)
  {
    # Add extra dimension
    
    my ($sslt) = &f ('.//f:EN-decl[./f:EN-N[string (.)="?"]]/f:array-spec/f:shape-spec-LT', $n, $d);
    my @ss = &f ('./f:shape-spec', $sslt);
    my $r = scalar (@ss);
    $r++;
    $sslt->appendChild (&t (','));
    $sslt->appendChild (&n ("<named-E><N><n>KLEN$r\_$n</n></N></named-E>"));


    my ($decl) = &f ('.//f:T-decl-stmt[.//f:EN-decl/f:EN-N[string (.)="KLON"]]', $d);

    $decl = $decl->cloneNode (1);
    my ($edlt) = &f ('.//f:EN-decl-LT', $decl);
    for ($edlt->childNodes ())
      {
        $_->unbindNode ();
      }

    $edlt->appendChild (&n ("<EN-decl><N><n>KLEN$r\_$n</n></N></EN-decl>"));
    $edlt->appendChild (&t (','));
    $edlt->appendChild (&n ("<EN-decl><N><n>KIDX$r\_$n</n></N></EN-decl>"));

    my $stmt = &Fxtran::stmt ($sslt);
    my $sp = &getIndent ($stmt);

    $stmt->parentNode->insertAfter ($decl, $stmt);
    $stmt->parentNode->insertAfter (&t ("\n" . (' ' x $sp)), $stmt);

    my @elt = &f ('.//f:named-E[./f:N/f:n/text ()="?"]/f:R-LT/f:parens-R/f:element-LT', $n, $d);

    for my $elt (@elt)
      {
        $elt->appendChild (&t (','));
        $elt->appendChild (&n ("<named-E><N><n>KIDX$r\_$n</n></N></named-E>"));
      }

    my ($da) = &f ('.//f:subroutine-stmt//f:dummy-arg-LT/f:arg-N[string (.)="?"]', $n, $d);
    my $dan = $da->nextSibling;
    $da->parentNode->insertBefore (&t (','), $dan);
    $da->parentNode->insertBefore (&n ("<arg-N><N><n>KLEN$r\_$n</n></N></arg-N>"), $dan);
    $da->parentNode->insertBefore (&t (','), $dan);
    $da->parentNode->insertBefore (&n ("<arg-N><N><n>KIDX$r\_$n</n></N></arg-N>"), $dan);
    

  }


'FileHandle'->new (">$f.new")->print ($d->textContent);
