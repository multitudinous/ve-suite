      SUBROUTINE FEEDFL

************************************************************************
* LAST CHANGE AT : 24.04.95                 BY:   J. Hannes  RWTH      *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : Z_RHOG                                                   *
************************************************************************
* COMMON in : NCLASS, COALM()                                          *
* out       :                                                          *
* in & out  : ATTCON(M), AEXPO(M), FEED(M), HEATCP(M), RHOS(M), SPHR(M)*
*             SIVFED(I,M), EPSSHM(M)                                   *
************************************************************************
* passed    : -                                                        *
* rturned   : -                                                        *
************************************************************************
* PURPOSE   : calculation of feed flow and the pre-treatment of the    *
*             fed material by fragmentation                            *
*             A virtual material is created by averaging (index BED)   *
*             to calculate a general flow pattern                      *
************************************************************************

       INCLUDE 'PARAMTR.FTN'
       INCLUDE 'COMMONBL.FTN'

       INTEGER  I        ! counter for size class
       REAL*8   Z_RHOG   ! gas density              (kg/m3)

         FEED(COKE)   = FEED(COAL)*(1.-COALM(MOIS))*COALM(ASH)
 
         FEED(BED) = FEED(LIME)+FEED(INERT)+FEED(COKE)  

C*           determine global bed state with indicator "BED" 

         DO I=1, NCLASS

           SIVFED(I,COKE) =   SIVFED(I,COAL)

           SIVFED(I,BED)  = ( SIVFED(I,LIME) *FEED(LIME)
     &                      +SIVFED(I,INERT)*FEED(INERT)
     &                      +SIVFED(I,COAL) *FEED(COKE)  )
     &                  / ( FEED(BED) )
         ENDDO

         
         RHOS(BED) = (RHOS(LIME)*FEED(LIME) + RHOS(INERT)*FEED(INERT)
     &                + RHOS(COKE)*FEED(COKE) )     /  FEED(BED)

         SPHR(BED) = (SPHR(LIME)*FEED(LIME)+SPHR(INERT)*FEED(INERT)
     &                + SPHR(COKE)*FEED(COKE) )     /  FEED(BED)

         HEATCP(BED)=(HEATCP(LIME)*FEED(LIME)+HEATCP(INERT)*FEED(INERT)
     &                + HEATCP(COKE)*FEED(COKE) )   /  FEED(BED)

         AEXPO(BED) = (AEXPO(LIME)*FEED(LIME)+AEXPO(INERT)*FEED(INERT)
     &                 + AEXPO(COKE)*FEED(COKE) )   /  FEED(BED)

         ATTCON(BED)=(ATTCON(LIME)*FEED(LIME)+ATTCON(INERT)*FEED(INERT)
     &                 + ATTCON(COKE)*FEED(COKE) )  /  FEED(BED) 

         EPSSHM(BED)=PRESS(2)/HEIGHT(NCELLS)/G
     &               / (RHOS(BED)-Z_RHOG(TBED,PRESS(1))) 

      RETURN
      END         ! (FEEDFL)



