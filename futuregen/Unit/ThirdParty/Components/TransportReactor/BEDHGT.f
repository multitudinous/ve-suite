
       SUBROUTINE BEDHGT

************************************************************************
* LAST CHANGE AT : 08.21.01                 BY:   Z. Chen              *
************************************************************************
* CALLED BY : PREPOP                                                   *
* CALLS     : Z_BUBB, Z_ESMF                                           *
************************************************************************
* COMMON in : AEXPO(M), AREA(K), BEDMAS, G, HEIGHT(K), MEAND(M), NCELLS*
*             PRESS(2), RHOS(M), WRTSCR                                *
* out       : NBED                                                     *
* in & out  :                                                          *
************************************************************************
* passed    : -                                                        *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : iterates the height of the dense bottom bed              *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8             Z_BUBB, ! bubble fraction 
     &                   Z_ESMF, ! solids volume fraction at min. fluidiz.
     &				   Z_VELO,
     &                   Z_EXP
      REAL*8             DENSE,  ! solids volume fraction in dense bed
     &                   HBED,   ! height of dense bed
     &				   CONT	   ! solids circulation rate

      INTEGER            K,I,  ! riser cell indicator
     &                   IJ,M    ! iteration loop
	REAL*8			   AA,EE,ESE,UU,EPSELUA,CONTP,ERROR

      REAL*8      Z_RHOG, Z_VIS
      REAL*8      AR, DUMMY, EPSFIX, FR, FRMX, FRUMF, FRWF
      REAL*8      PHIHM, PHIO, RHOG, VISG, VSV, Gs 


      real*8 pp,ppd,xx,a,b,c

	COMMON /Solids/Gs
	COMMON /INDEX/IJ,ERROR

      CHARACTER          CH

      IF (WRTSCR) write(*,*) ' BEDHGT   '

      IF (OPTION(1) .EQ. 1.) THEN		! Wen and Chen
       UU = U(NCELLS) - Z_VELO(MEAND(BED),BED)
       IF (UU .GT. 0.) THEN
       AA = 1. + 6.15 / G / RHOS(BED) / SQRT(MEAND(BED)) / Z_EXP(UU,0.5)
       EE = -1./4.7
       EPSELUA = 1. - Z_EXP(AA,EE)
       ELSE
       EPSELUA = 1.E-6
       ENDIF
	ELSE							! Wirth
       RHOG = Z_RHOG(TBED,PRESS(1))
       VISG = Z_VIS(TBED)
         EPSFIX  = 0.4
         PHIHM = 1. - EPSSHM(BED) / (1.-EPSFIX)
C*       superficial Fr-number and Ar-number [-]
         FR = U(NCELLS)/((RHOS(BED)-RHOG)*MEAND(BED)*G/RHOG)**.5
         AR = (RHOS(BED)-RHOG)*RHOG*MEAND(BED)**3.*G/(VISG**2.)

C*       terminal Fr-number
         FRWF = Z_VELO(MEAND(BED),BED)/((RHOS(BED)-RHOG)
     &		  *MEAND(BED)*G/RHOG)**.5

         IF (FR .LE. FRWF) THEN
            PHIO = 1.
         ELSE
C*           iteration maximum Fr-Number (-)
             FRMX  = 1.
             CALL SUB02 (PHIHM, FRWF, FRUMF, FRMX)
C*           iteration strand-volume ratio PHIO (-)
             PHIO = 0.9
             CALL SUB03 (PHIHM,FR,FRWF,FRUMF,FRMX,PHIO)
             IF (PHIO .LT. 1.-1.E-7) THEN
                CALL SUB01 (FR,FRWF,FRUMF,PHIO,VSV,DUMMY)
             ENDIF
         ENDIF
C*         saturation solid part of fraction [-]
           EPSELUA = (1.-PHIO)*(1.-EPSFIX)
	ENDIF
       DENSE = (1-Z_BUBB(MEAND(BED),BED))*Z_ESMF(MEAND(BED),BED)
	 IF(IJ.EQ.1.OR.U(NCELLS).LE.Z_VELO(MEAND(BED),BED)) THEN
	 HBED = HEIGHT(NCELLS)*0.01
	 ELSE
c	 ESE = Gs/(RHOS(BED)*(U(NCELLS)-Z_VELO(MEAND(BED),BED)))
       a = RHOS(BED)*Z_VELO(MEAND(BED),BED)
       b = RHOS(BED)*(U(NCELLS)-Z_VELO(MEAND(BED),BED))+Gs
       c = -Gs
       ESE = (SQRT(b**2-4*a*c)-b)/(2*a)
		IF(ESE.LE.EPSELUA.OR.DENSE.LE.EPSELUA) THEN
		HBED = HEIGHT(NCELLS)*0.01
		WRITE(LOG,*)'Dense bed height is 0.010*total riser height'
		WRITE(*,*)'Dense bed height is 0.010*total riser height'
		ELSE
		HBED = HEIGHT(NCELLS)-HEIGHT(0)-
     &		DLOG((DENSE-EPSELUA)/(ESE-EPSELUA))/AEXPO(BED)
			IF(HBED.LT.0.0) HBED = 0.0
c		WRITE(LOG,*)'Dense bed height was calculated using eq.(19)'
		WRITE(LOG,*)'Dense bed height is',HBED
		ENDIF
	 ENDIF
          CONT  = 0.0
         DO I = 1,NCLASS
          CONT = CONT 
     &        +(MCYC(I,CMP(4),2)+MEHE(I,CMP(5),2))
     &        +(MCYC(I,CMP(4),3)+MEHE(I,CMP(5),3))
     &        +(MCYC(I,CMP(4),4)+MEHE(I,CMP(5),4))  
         ENDDO
		CONT = CONT/AREA(NCELLS)
		ERROR = DABS(CONT-CONTP)/CONT
		CONTP= CONT

	 PRESS(2) = RHOS(BED) * G * 
     &	(DENSE*HBED + EPSELUA* (HEIGHT(NCELLS)-HEIGHT(0)-HBED) 
     &	+ (DENSE-EPSELUA)/AEXPO(BED)
     &	* (1.0-EXP(-AEXPO(BED)*(HEIGHT(NCELLS)-HEIGHT(0)-HBED))))
	 PRESS(4) = PRESS(2)
!zc
      DO M = 1,4
        EPSSHM(M) = PRESS(2)/(HEIGHT(NCELLS)-HEIGHT(0))
     &              /G/(RHOS(M)-Z_RHOG(TBED,PRESS(1)))
        IF(EPSSHM(M).GT.0.20) EPSSHM(M) = 0.20
      ENDDO
         EPSSHM(BED)=PRESS(2)/HEIGHT(NCELLS)/G
     &               / (RHOS(BED)-Z_RHOG(TBED,PRESS(1))) 
        IF(EPSSHM(BED).GT.0.20) EPSSHM(BED) = 0.20
!zc
             DO K=0,NCELLS
              IF (HEIGHT(K) .GT. HBED) THEN
                 NBED = K
                 GOTO 30
              ENDIF
             ENDDO
 30         CONTINUE
          IF (HEIGHT(NBED) .GT. 0.) THEN
            DENSE = DENSE * HBED / HEIGHT(NBED)
          ENDIF

C*          warning if the cell grid does not match the bed surface
!zc      IF (ABS(HBED-HEIGHT(NBED))/HEIGHT(NCELLS) .GT. 0.01) THEN
!zc       WRITE(*,*) ' The calculated bedheight is ',HBED,' (m)'
!zc       WRITE(*,*) ' Pointer set to cell         ',HEIGHT(NBED),' (m)'
!zc       WRITE(*,*) ' Change no of cells in input file or accept above !'
!zc       WRITE(*,*) ' Any key to accept, q to abort '
!zc       READ(*,1001) CH
!zc 1001  FORMAT(A1)
!zc       IF(CH .EQ. 'q' .OR. CH .EQ. 'Q') STOP
!zc      ENDIF

         BEDMAS = 0.
        DO K=1,NBED
          BEDMAS = BEDMAS+RHOS(BED)*AREA(K)*(HEIGHT(K)-HEIGHT(K-1))
     &                          * DENSE
        ENDDO
        DO K = NBED+1, NCELLS
          BEDMAS = BEDMAS + RHOS(BED)*AREA(K) * DENSE/AEXPO(BED)
     &     *(EXP(-AEXPO(BED)*(HEIGHT(K-1)-HEIGHT(NBED)))
     &      -EXP(-AEXPO(BED)*(HEIGHT(K)  -HEIGHT(NBED))) )
        ENDDO

       IF (WRTSCR) write(*,*) ' END BEDHGT '

       RETURN
       END       ! (BEDHGT)