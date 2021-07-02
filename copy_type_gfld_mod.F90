MODULE COPY_TYPE_GFLD_MOD

USE YOM_YGFL, ONLY : TYPE_GFLD

INTERFACE COPY
MODULE PROCEDURE COPY_TYPE_GFLD
END INTERFACE

CONTAINS

SUBROUTINE COPY_TYPE_GFLD (YD)
USE COPY_TYPE_GFL_COMP_MOD
IMPLICIT NONE
TYPE (TYPE_GFLD), INTENT (IN) :: YD
INTEGER :: J1

!$acc update device (YD%NUMFLDS)

!$acc update device (YD%NDERS)

!$acc update device (YD%NUMSPFLDS)

!$acc update device (YD%NUMGPFLDS)

!$acc update device (YD%NUMFLDS9)

!$acc update device (YD%NUMFLDS1)

!$acc update device (YD%NUMSPFLDS1)

!$acc update device (YD%NUMFLDS5)

!$acc update device (YD%NUMFLDSPHY)

!$acc update device (YD%NUMFLDS_SPL)

!$acc update device (YD%NUMFLDS_SL1)

!$acc update device (YD%NUMFLDSPC)

!$acc update device (YD%NDIM)

!$acc update device (YD%NUMFLDSPT)

!$acc update device (YD%NDIM0)

!$acc update device (YD%NDIM9)

!$acc update device (YD%NDIM1)

!$acc update device (YD%NDIM5)

!$acc update device (YD%NDIMSLP)

!$acc update device (YD%NDIM_SPL)

!$acc update device (YD%NDIMPT)

!$acc update device (YD%NDIMPC)

!$acc update device (YD%NGFL_EXT)

!$acc update device (YD%NGFL_FORC)

!$acc update device (YD%NGFL_EZDIAG)

!$acc update device (YD%NGHG)

!$acc update device (YD%NTRAC)

!$acc update device (YD%NAERO)

!$acc update device (YD%NACTAERO)

!$acc update device (YD%NDDHAERO)

!$acc update device (YD%NERA40)

!$acc update device (YD%NNOGW)

!$acc update device (YD%NAEROUT)

!$acc update device (YD%NUVP)

!$acc update device (YD%NSLDIA)

!$acc update device (YD%NSLDIAGP)

!$acc update device (YD%NGFL_PHYS)

!$acc update device (YD%NCRM)

!$acc update device (YD%LGHGRTTOV)

!$acc update device (YD%LCO2SFC)

!$acc update device (YD%LCH4SFC)

!$acc update device (YD%LAEROSFC)

!$acc update device (YD%LFIRE)

!$acc update device (YD%LINJ)

!$acc update device (YD%LINJ_CHEM)

!$acc update device (YD%LAERODIU)

!$acc update device (YD%LTRCMFIX)

!$acc update device (YD%LTRCMFIX_PS)

!$acc update device (YD%LAEROUT)

!$acc update device (YD%LUVPOUT)

!$acc update device (YD%LCHEM)

!$acc update device (YD%NGEMS)

!$acc update device (YD%NCHEM)

!$acc update device (YD%NCHEM_ASSIM)

!$acc update device (YD%NCHEM_FLX)

!$acc update device (YD%NCHEM_DV)

!$acc update device (YD%NCHEM_TC)

!$acc update device (YD%NCHEM_SCV)

!$acc update device (YD%NNEGAFIX)

!$acc update device (YD%NOPTNEGFIX)

!$acc update device (YD%LQM3DCONS)

!$acc update device (YD%LADVNEGFIX)

!$acc update device (YD%LTRCMFBC)

!$acc update device (YD%LTRCMFPR)

!$acc update device (YD%LTRCMFMG)

!$acc update device (YD%LTRCMFP)

!$acc update device (YD%LTRCMFA_DIF)

!$acc update device (YD%LTRCMFA_LAP)

!$acc update device (YD%LTRCMFA_VER)

!$acc update device (YD%LEXTRADF)

!$acc update device (YD%NFLDSFIX)

!$acc update device (YD%NOPTMFBC)

!$acc update device (YD%NOPTMFPR)

!$acc update device (YD%NOPTVFE)

!$acc update device (YD%NMFDIAGLEV)

!$acc update device (YD%NMFIXFLDS)

!$acc update device (YD%NNEGFLDS)

!$acc update device (YD%ZMFIXEPS)

!$acc enter data create (YD%YCOMP)
DO J1 = LBOUND (YD%YCOMP, 1), UBOUND (YD%YCOMP, 1)
  CALL COPY (YD%YCOMP (J1))
ENDDO
!$acc enter data attach (YD%YCOMP)

CALL COPY (YD%YQ)

CALL COPY (YD%YI)

CALL COPY (YD%YL)

CALL COPY (YD%YS)

CALL COPY (YD%YR)

CALL COPY (YD%YG)

CALL COPY (YD%YCVGQ)

END SUBROUTINE

END MODULE
