
      PROGRAM CFBC

************************************************************************
* CALLED BY :                                                          *
* CALLS     : CHARBL,CHARPR,CHARRT,CHECK,COMBUS,CORFAC,DATAIN,DENSBD,  *
*             ELUTRI0,ENERGY,FEEDFL,FLUIDI,FRAGMT,GASFLO,INTEGR,MASS,  *
*             NOXPRE,PREPAR,PRESOL,POPUL,PYROLY, RISER, SOLCON,SOLFLO, *
*             SOLVEL,SURE,WRITE0-5                                     *
************************************************************************
* COMMON in : BEDMAS, LENERG, LMASS, MWS(1), NCELLS, NCLASS, PART(I,M),*
*             POSTPR, RRTOT(I), WRTOUT, WRTSCR                         *
*       out :                                                          *
*  in & out :                                                          *
************************************************************************
* passed    : -                                                        *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : MAIN , ADMINISTRATION OF SUBROUTINES                     *
************************************************************************


      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      INTEGER    I, ! particle size indicator
     &           K, ! cell indicator
     &           M, ! material indicator
     &		   IJ !	index for solids circulation
      REAL*8     ROLD,  ! char combustion residue in previous loop
     &           RES_E, ! residue of energy balance
     &           RES_M, ! residue of mass balance
     &           RESID1,! char combustion residue
     &           RESID2,! sulfur retention residue
     &           RTEST, ! overall combustion rate
     &           SOLD,  ! old suflur retention rate
     &           SUM,   ! auxiliary
     &           SUMN   ! auxiliary
      REAL*8     TOLD1, ! temperature in first cell in previous loop
     &           TOLDN  ! temperature in last cell in previous loop
      REAL*8     Z_EXP  ! root function (zero if NaN)
	REAL*8     ERROR
	INTEGER  IUg
	COMMON /INDEX/IJ,ERROR

      OPEN(UNIT=LOG,FILE='LOG.DAT',STATUS='UNKNOWN')

C*    ************ data input *****************************  
      WRITE(*,*) 'Read and check input data '
      CALL DATAIN
      CALL CHECK
      CALL WRITE0

      WRITE(LOG,*) ' The input file of the last run was: ',FLNAME 

       write(*,*) 'Prepare fuidization pattern '

C*    ************ geometry and gas flow ******************
       CALL RISER

C*    ************ population *****************************

C*              redistribution of particle sizes to sqrt 2 distribution
       CALL REDIST

C*              immediate fragmentation of feed coal
       CALL FRAGMT(COAL)

C*              feed flow split coal to inert and char
       CALL FEEDFL
       
C*              secondary fragmentation of char 
C*              and fragmentation of lime and inert
       CALL FRAGMT(LIME)
       CALL FRAGMT(INERT)
       CALL FRAGMT(COKE)
C*       pre balance of bed material
       IUg = -1            !zc
1     CONTINUE             !zc        
       IUg = IUg + 1       !zc
	 IF(IUg.eq.1) CALL RISER_add	!zc

	DO 10 IJ=1,1000            
       CALL PREPOP

       CALL DENSBD(BED)
       CALL CORFAC(BED)
       CALL SOLVEL(BED)

C*    ************ fluidization properties of solids ******
      DO M=1,4
       CALL ELUTRI(M)
       CALL DENSBD(M)
       CALL CORFAC(M)
       CALL SOLVEL(M)
      ENDDO

C*    ************ preparation solids *********************
      CALL PRESOL

C*    ************ population balance  ********************
      CALL POPUL
                  
C*    ************ stoichiometric test calculation **********
      CALL COMBUS

C*    ************ solid flows and solids holdup***********
      DO M=1,4
        CALL NETFLO(M)
        CALL SOLCON(M)
        CALL SOLFLO(M)
      ENDDO 
        BEDMAS = BEDM(2) + BEDM(3) + BEDM(4)

	 IF(IJ.NE.1.AND.ERROR.LT.0.0001) GOTO 15
10	CONTINUE
15	CONTINUE

      CALL WRITE2

C*    ************ pyrolysis ******************************
      CALL PYROLY

C*    ************ gas flows ******************************
      CALL GASFLO

C*********************** ENERGY BALANCE LOOP 40**************************

           TOLDN = 1.
           TOLD1 = 1.
           WRTOUT(3) = .FALSE.  ! chartr
           POSTPR    = .TRUE.

      DO 40 LENERG = 1,30

                WRITE(*,*)   ' ENERGY BALANCE LOOP ',LENERG
                WRITE(LOG,*) ' ENERGY BALANCE LOOP ',LENERG

C*    ************ preparation of char combustion ***
      CALL CHARPR

C********** MASS BALANCE LOOP 20 ****************************************
               
C*          initialize char reaction and sulfur retention residues
        ROLD = 0.
        SOLD = 0.

      DO 20 LMASS = 1,100


C           CALL WRITE2

C*    ********* local char combustion balance ******************
           CALL CHARBL

C*    ********* gas char combustion source terms ************
           CALL CHARRT

C*    ********* sulfur retention rates **********************
           CALL SURE

C*    ********* NOx catalyst surfaces **********************
           CALL NOXPRE

C*    ********* GAS PHASE REACTIONS **********************
           CALL INTEGR
      
C*             write preliminary gas concentrations
           CALL WRITE4
 
C########################################################################

C*             determine overall combustion rate 
                 RTEST = 0.
             DO I = 1,NCLASS            
                 RTEST = RTEST + RRTOT(I)*PART(I,COKE)*BEDMAS*MWS(1)
             ENDDO            

C*             check if there is char combustion at all
          IF (RTEST .LE. 0.) THEN
              write(*,*) 'Char combustion does not occur'
              write(LOG,*) 'Char combustion does not occur'
C             GOTO 30
          ENDIF   

           SUM = 0.
          DO K=1,NCELLS
           SUM = SUM 
     &      +RSURE(K,CORE)*XGAS(H2S,K,CORE)  !*Z_EXP(XGAS(O2,K,CORE),0.5)
     &      +RSURE(K,ANNU)*XGAS(H2S,K,ANNU)  !*Z_EXP(XGAS(O2,K,ANNU),0.5)
C        write(*,*)'SSS',K,RSURE(K,CORE),XGAS(SO2,K,CORE),XGAS(O2,K,CORE)
          ENDDO
           IF (SUM .GT. 0.) THEN
             SUMN = SUM
           ELSE
             SUMN = 1.
             SUM  = 0.
             write(*,*) ' SURE RETENTION SUM IS ZERO OR NEGATIVE ', SUM
           ENDIF

C*             combustion residue 1=char combbustion, 2= sulfur retention
          IF (RTEST .NE. 0) THEN
           RESID1 = ABS((RTEST-ROLD)/RTEST) 
           RESID2 = ABS((SUM-SOLD)/SUMN)
           RES_M = RESID1 + RESID2
          ELSE
           RES_M = 0.
          ENDIF

      WRITE(*,*)'MASS BALANCE LOOP ',LMASS,' R1= ',RESID1,' R2= ',RESID2
      WRITE(LOG,*)'   MASS BAL ',LMASS,' RCOKE=',RESID1,' RSURE=',RESID2

          IF (RES_M .LT. 0.005) THEN
               GOTO 30  
          ENDIF
       
          ROLD = RTEST
          SOLD = SUM

C#########################################################################

  20   CONTINUE                  

  30   CONTINUE

C*            perform energy balance                                   
           CALL ENERGY

C*            residue of energy balance
        RES_E = ABS((TEMP(NCELLS,CORE)-TOLDN)/TOLDN) 
     &        + ABS((TEMP(1,CORE)     -TOLD1)/TOLD1)

      write(*,*) ' ENERGY BAL ',LENERG,' RESIDUE=',RES_E
      write(LOG,*) ' ENERGY BAL ',LENERG,' RESIDUE=',RES_E

      IF (RES_E .LT. 1.E-3) GOTO 50

              TOLDN = TEMP(NCELLS,CORE) 
              TOLD1 = TEMP(1,CORE)

  40  CONTINUE

  50  CONTINUE
         IF(IUg.eq.0) GOTO 1       !zc

         POSTPR = .TRUE.                            
C         CALL INTEGR
                                     
C*           write gas concentrations                  
          CALL WRITE4

C*           write total C balance                  
          CALL WRITE5

            WRITE(LOG,*)
            WRITE(LOG,*) ' no of energy loops'
            WRITE(LOG,*) LENERG
            WRITE(LOG,*)
            WRITE(LOG,*) ' no of size classes used:'
            WRITE(LOG,*) NI
            WRITE(LOG,*) 
            WRITE(LOG,*) ' no of cells in riser: '
            WRITE(LOG,*) NCELLS
            WRITE(LOG,*)
            WRITE(LOG,*) ' no of species used: '
            WRITE(LOG,*) NJ
            WRITE(LOG,*)
            WRITE(LOG,*) ' no of solid materials used: '
            WRITE(LOG,*) NMAT
            WRITE(LOG,*)

          write(*,*) ' RUN FINISHED SUCCESSFULLY ! '
          write(LOG,*) ' RUN FINISHED SUCCESSFULLY ! '      

          CLOSE(LOG)

      END   ! (CFBC)

