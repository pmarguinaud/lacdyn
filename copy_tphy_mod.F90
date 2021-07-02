MODULE COPY_TPHY_MOD

USE YOMPHY, ONLY : TPHY

INTERFACE COPY
MODULE PROCEDURE COPY_TPHY
END INTERFACE

CONTAINS

SUBROUTINE COPY_TPHY (YD)

IMPLICIT NONE
TYPE (TPHY), INTENT (IN) :: YD

!$acc update device (YD%NBITER)

!$acc update device (YD%NITERFL)

!$acc update device (YD%NDPSFI)

!$acc update device (YD%LMPHYS)

!$acc update device (YD%LREASUR)

!$acc update device (YD%LCAPE)

!$acc update device (YD%LCVGQD)

!$acc update device (YD%LCVGQM)

!$acc update device (YD%LCVGQP)

!$acc update device (YD%LCONDWT)

!$acc update device (YD%LCVDD)

!$acc update device (YD%LCVPGY)

!$acc update device (YD%LCVPP)

!$acc update device (YD%LDIFCEXP)

!$acc update device (YD%LDIFCONS)

!$acc update device (YD%LNEIGE)

!$acc update device (YD%LRNUMX)

!$acc update device (YD%LACPANMX)

!$acc update device (YD%LCLSATUR)

!$acc update device (YD%LVOIGT)

!$acc update device (YD%LGLACIERS)

!$acc update device (YD%LVFULL)

!$acc update device (YD%LSLC)

!$acc update device (YD%LRAYLU)

!$acc update device (YD%LREWS)

!$acc update device (YD%LRPROX)

!$acc update device (YD%LRMIX)

!$acc update device (YD%LRSTAB)

!$acc update device (YD%LRSTAER)

!$acc update device (YD%LRTPP)

!$acc update device (YD%LRTDL)

!$acc update device (YD%LRAYPL)

!$acc update device (YD%LRRGUST)

!$acc update device (YD%LO3ABC)

!$acc update device (YD%LO3FL)

!$acc update device (YD%LECSHAL)

!$acc update device (YD%LECDEEP)

!$acc update device (YD%LNEBNXR)

!$acc update device (YD%LNEB_FP)

!$acc update device (YD%LQXRTGH)

!$acc update device (YD%LHUCN)

!$acc update device (YD%LFPCOR)

!$acc update device (YD%LNOIAS)

!$acc update device (YD%LGLT)

!$acc update device (YD%LNEWD)

!$acc update device (YD%LPROCLD)

!$acc update device (YD%LEVAPP)

!$acc update device (YD%LCOLLEC)

!$acc update device (YD%LADJCLD)

!$acc update device (YD%LSEDSTA)

!$acc update device (YD%LSEDLAG)

!$acc update device (YD%LSEDCL)

!$acc update device (YD%LFSVAR)

!$acc update device (YD%LFSFIX)

!$acc update device (YD%LA0MPS)

!$acc update device (YD%LARPMPS)

!$acc update device (YD%L3MT)

!$acc update device (YD%LGPCMT)

!$acc update device (YD%LENTCH)

!$acc update device (YD%LCDDEVPRO)

!$acc update device (YD%LAEROSEA)

!$acc update device (YD%LAEROLAN)

!$acc update device (YD%LAEROSOO)

!$acc update device (YD%LAERODES)

!$acc update device (YD%LAEROVOL)

!$acc update device (YD%LAEROSUL)

!$acc update device (YD%LRELAXT)

!$acc update device (YD%LRELAXW)

!$acc update device (YD%LSSD)

!$acc update device (YD%LPBLE)

!$acc update device (YD%LBLVAR)

!$acc update device (YD%LZ0HSREL)

!$acc update device (YD%LMLH)

!$acc update device (YD%LNODIFQC)

!$acc update device (YD%LNCVPGY)

!$acc update device (YD%LSQRML)

!$acc update device (YD%LAB12)

!$acc update device (YD%CGMIXLEN)

!$acc update device (YD%CGTURS)

!$acc update device (YD%LPRGML)

!$acc update device (YD%LCVRA)

!$acc update device (YD%LDAYD)

!$acc update device (YD%LCVPRO)

!$acc update device (YD%LCDDPRO)

!$acc update device (YD%LECT)

!$acc update device (YD%LEFB)

!$acc update device (YD%LFLUSO)

!$acc update device (YD%LECTQ1)

!$acc update device (YD%LECTREP)

!$acc update device (YD%LPTKE)

!$acc update device (YD%LCOEFKTKE)

!$acc update device (YD%LCOEFKSURF)

!$acc update device (YD%LCOEFK_MSC)

!$acc update device (YD%LCOEFK_F1)

!$acc update device (YD%LCOEFK_TOMS)

!$acc update device (YD%LCOEFK_THS1)

!$acc update device (YD%LCOEFK_RIS)

!$acc update device (YD%LCOEFK_RIH)

!$acc update device (YD%LCOEFK_PL)

!$acc update device (YD%LCOEFK_ML)

!$acc update device (YD%LCOEFK_PTTE)

!$acc update device (YD%LCOEFK_FLX)

!$acc update device (YD%LCOEFK_SCQ)

!$acc update device (YD%LAFGD_A)

!$acc update device (YD%LGWD)

!$acc update device (YD%LGWDC)

!$acc update device (YD%LHMTO)

!$acc update device (YD%LNEBCO)

!$acc update device (YD%LNEBN)

!$acc update device (YD%LNEBR)

!$acc update device (YD%LOZONE)

!$acc update device (YD%LRAY)

!$acc update device (YD%LRAYFM)

!$acc update device (YD%LRMU0M)

!$acc update device (YD%LRAYFM15)

!$acc update device (YD%LRRMES)

!$acc update device (YD%LSFHYD)

!$acc update device (YD%LSNV)

!$acc update device (YD%LSOLV)

!$acc update device (YD%LFGEL)

!$acc update device (YD%LSTRA)

!$acc update device (YD%LSTRAS)

!$acc update device (YD%LSTRAPRO)

!$acc update device (YD%LNEWSTAT)

!$acc update device (YD%LTHERMO)

!$acc update device (YD%LVDIF)

!$acc update device (YD%LCVLIS)

!$acc update device (YD%LCVCAS)

!$acc update device (YD%LSCMF)

!$acc update device (YD%LVGSN)

!$acc update device (YD%LPHCDPI)

!$acc update device (YD%LNEBGR)

!$acc update device (YD%LNEBGY)

!$acc update device (YD%LCVRAV3)

!$acc update device (YD%LCVPPKF)

!$acc update device (YD%LEDKF)

!$acc update device (YD%LEDMFI)

!$acc update device (YD%LEDMFS)

!$acc update device (YD%LECTFL)

!$acc update device (YD%LECTFL0)

!$acc update device (YD%NPHYREP)

!$acc update device (YD%NOIR)

!$acc update device (YD%LPHSPSH)

!$acc update device (YD%LSMNIMBT)

!$acc update device (YD%LSMTPS)

!$acc update device (YD%LSMGCDEV)

!$acc update device (YD%LXRCDEV)

!$acc update device (YD%LRKCDEV)

!$acc update device (YD%LSMITH_CDEV)

!$acc update device (YD%LRCVOTT)

!$acc update device (YD%LNEBCV)

!$acc update device (YD%LNEBINS)

!$acc update device (YD%NDIFFNEB)

!$acc update device (YD%NIMELIT)

!$acc update device (YD%NRAY)

!$acc update device (YD%NSORAYFR)

!$acc update device (YD%NTHRAYFR)

!$acc update device (YD%NRAUTOEV)

!$acc update device (YD%NSMTBOT)

!$acc update device (YD%NSMDNEB)

!$acc update device (YD%NPRAC)

!$acc update device (YD%NPRRI)

!$acc update device (YD%NPHY)

!$acc update device (YD%NCALLRAD)

!$acc update device (YD%NLEND)

!$acc update device (YD%LNEBECT)

!$acc update device (YD%LCVGQKF)

!$acc update device (YD%LACDIFUS)

!$acc update device (YD%L2MICRO)

!$acc update device (YD%LAJUCV)

!$acc update device (YD%LSMOOTHMELT)

!$acc update device (YD%LGCHECKMV)

!$acc update device (YD%LGCHECKTE)

!$acc update device (YD%LEDR)

END SUBROUTINE

END MODULE
