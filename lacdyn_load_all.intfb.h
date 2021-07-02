INTERFACE
SUBROUTINE LACDYN_LOAD_ALL (CDNAME)

USE LOAD_MOD, ONLY : OPEN_LOAD, CLOSE_LOAD, ILUN_IN, LOAD

CHARACTER (LEN=*) :: CDNAME

USE INTDYN_MOD
  
  
  
!$acc update device (YYTCTY0)
  
!$acc update device (YYTHW0)
  
  
  
  
  
  
  
  
  
  
  
!$acc update device (YYTTND)
  
  
  
  
!$acc update device (YYTVC0)

USE PAR_RDLR

USE PTRSLB1
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
!$acc update device (YRPTRSLB1)

USE PTRSLB2
  
  
  
  
  
  
  
  
  
  
  
!$acc update device (YRPTRSLB2)

USE YEMDYN
  
!$acc update device (YREDYN)

USE YOEPHY
  
  
!$acc update device (YREPHY)

USE YOMCST
  
!$acc update device (RA)
  
!$acc update device (RCPD)
  
!$acc update device (RCPV)
  
!$acc update device (RCS)
  
!$acc update device (RCW)
  
!$acc update device (RD)
  
!$acc update device (RKAPPA)
  
!$acc update device (RV)

USE YOMCT0
  
!$acc update device (LAROME)
  
!$acc update device (LNHDYN)
  
!$acc update device (LSLPHY)
  
!$acc update device (LSPRT)
  
!$acc update device (LTWOTL)

USE YOMCT3
  
!$acc update device (NSTEP)

USE YOMCVER
  
!$acc update device (LRNHC1)
  
!$acc update device (LVERTFE)
  
!$acc update device (LVFE_INTB)

USE YOMDIM

USE YOMDIMV

USE YOMDYN

USE YOMDYNA
  
!$acc update device (LCOMAD)
  
!$acc update device (LCOMADH)
  
!$acc update device (LCOMADV)
  
!$acc update device (LELTRA)
  
!$acc update device (LGWADV)
  
!$acc update device (LNESC)
  
!$acc update device (LNESCT)
  
!$acc update device (LNESCV)
  
!$acc update device (LNH_GEOGW)
  
!$acc update device (LNH_PDVD)
  
!$acc update device (LPC_CHEAP)
  
!$acc update device (LPC_FULL)
  
!$acc update device (LRDBBC)
  
!$acc update device (LRWSDLR)
  
!$acc update device (LRWSDLW)
  
!$acc update device (LSETTLS)
  
!$acc update device (LSETTLST)
  
!$acc update device (LSETTLSV)
  
!$acc update device (LSLHD)
  
!$acc update device (LSLINL)
  
!$acc update device (LSLINLC1)
  
!$acc update device (LSLINLC2)
  
!$acc update device (LVERCOR)
  
!$acc update device (ND4SYS)
  
!$acc update device (NPDVAR)
  
!$acc update device (NVDVAR)

USE YOMGEM

USE YOMLDDH

USE YOMMDDH

USE YOMPARAR

USE YOMPHY

USE YOMSTA

USE YOMVERT

USE YOM_YGFL

END SUBROUTINE LACDYN_LOAD_ALL

END INTERFACE
