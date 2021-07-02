#!/bin/bash

set -x

dir_in=.
dir_out=.

function loadsave ()
{
  f="${!#}"  
  set -- "${@:1:$#-1}"
  ../loadsave.pl $* $f
}


loadsave --dir=$dir_out --types --skip TYPE_GFL_COMP%PREVIOUS --skip TYPE_GFLD%YAERO_WVL_DIAG_NL $dir_in/yom_ygfl.F90

for f in yomgsgeom.F90 yomgmv.F90 type_gmvs.F90 yomorog.F90 yomvert.F90 yomlddh.F90 yomsta.F90 yomparar.F90 \
  yomgem.F90 yomphy.F90 yomdim.F90 yomdimv.F90 yomdyn.F90  yommddh.F90 

do
loadsave --dir=$dir_out --types $dir_in/$f
done



