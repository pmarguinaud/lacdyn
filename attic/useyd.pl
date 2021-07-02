#!/home/ms/fr/sor/install/perl-5.32.1/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

my $F90 = shift;

my $doc = &Fxtran::fxtran (location => $F90);

my @X = qw (
YOMDIM        TDIM               YRDIM 
YOMDIMV       TDIMV              YRDIMV 
YOMDYN        TDYN               YRDYN 
YOMGEM        TGEM               YRGEM 
YOMLDDH       TLDDH              YRLDDH 
YOMMDDH       TMDDH              YRMDDH 
YOMPARAR      TPARAR             YRPARAR 
YOMPHY        TPHY               YRPHY 
YOMSTA        TSTA               YRSTA 
YOMVERT       TVAB               YRVAB  
YOMVERT       TVETA              YRVETA 
YOMVERT       TVFE               YRVFE  
YOM_YGFL      TYPE_GFLD          YGFL 
);

my ($pu) = &f ('.//f:program-unit', $doc);

my ($stmt) = &f ('.//f:' . &Fxtran::xpath_by_type ('stmt'), $pu);

my ($darglt) = &f ('./f:dummy-arg-LT', $stmt);
my ($arg0) = &f ('./f:arg-N', $darglt);

my %V;

my @x;

@x = @X; while (my ($m, $t, $v) = splice (@x, 0, 3))
  {
    my @use = &f ('.//f:use-stmt/f:rename-LT/f:rename/f:use-N/f:N/f:n[text ()="?"]', $v, $doc);
    for (@use)
      {
        $_->replaceNode (&t ($t));
        $V{$v} = 1;
      }
  }

($stmt) = &f ('.//f:T-decl-stmt[.//f:EN-N/f:N/f:n[text ()="?"]]', $arg0->textContent (), $doc);

@x = @X; while (my ($m, $t, $v) = splice (@x, 0, 3))
  {
    next unless ($V{$v});
    $darglt->insertBefore (&t ("$v, "), $darglt->firstChild);
    my ($cr) = &f ('(preceding::text ()[contains (., "' . "\n" . '")])[last ()]', $stmt);
    $cr->parentNode->insertAfter (&t ("TYPE ($t), " . (' ' x (10 - length ($t))) . "INTENT(IN) :: $v\n"), $cr);
  }



'FileHandle'->new (">$F90.new")->print ($doc->textContent);

