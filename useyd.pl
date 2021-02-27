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

my @x = qw (
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

while (my ($m, $t, $v) = splice (@x, 0, 3))
  {
    my @use = &f ('.//f:use-stmt/f:rename-LT/f:rename/f:use-N/f:N/f:n[text ()="?"]', $v, $doc);
    for (@use)
      {
        print $_->toString (), "\n";
      }
  }


