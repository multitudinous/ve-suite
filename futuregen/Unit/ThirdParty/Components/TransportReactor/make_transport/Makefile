
SRCDIR = ..

FFLAGS = -I$(SRCDIR)/Includes -I../ -Bstatic -g

#FC 	      = g77 
#LINKER	      = g77 

FC 	      = pgf77
LINKER	      = pgf77 

MAKEFILE      = Makefile

LIBS          = /usr/pgi/linux86/lib/libpgftnrtl.a /usr/pgi/linux86/lib/libpgc.a

HDR1          = COMMONBL.FTN
HDR2	      = PARAMTR.FTN
HDR3	      = KINETICS.FTN
HDR4          = PARAMT.FTN

OBJS	      =	$(SRCDIR)/ATTRIT.o \
		$(SRCDIR)/BEDHGT.o \
		$(SRCDIR)/BLOCK.o \
		$(SRCDIR)/BOTTOM.o \
		$(SRCDIR)/CFBC.o \
		$(SRCDIR)/CHARBL.o \
		$(SRCDIR)/CHARPR.o \
		$(SRCDIR)/CHARRT.o \
		$(SRCDIR)/CHARTR.o \
		$(SRCDIR)/CHECK.o \
		$(SRCDIR)/COMBUS.o \
		$(SRCDIR)/CORFAC.o \
		$(SRCDIR)/CPD_NLG.o \
		$(SRCDIR)/CRRECT.o \
		$(SRCDIR)/CYCLON.o \
		$(SRCDIR)/DATAIN.o \
		$(SRCDIR)/DENSBD.o \
		$(SRCDIR)/DRY.o \
		$(SRCDIR)/ELUTRI.o \
		$(SRCDIR)/ENERGY.o \
		$(SRCDIR)/FEEDFL.o \
		$(SRCDIR)/FRAGMT.o \
		$(SRCDIR)/FUNCV.o \
		$(SRCDIR)/GASFLO.o \
		$(SRCDIR)/GASMIX.o \
		$(SRCDIR)/HEATTR.o \
		$(SRCDIR)/HETGR.o \
		$(SRCDIR)/HOMOGR.o \
		$(SRCDIR)/INTEGR.o \
		$(SRCDIR)/LINEQS.o \
		$(SRCDIR)/NETFLO.o \
		$(SRCDIR)/NOXPRE.o \
		$(SRCDIR)/NWTSVD.o \
		$(SRCDIR)/POPUL.o \
		$(SRCDIR)/PREPOP.o \
		$(SRCDIR)/PRESOL.o \
		$(SRCDIR)/PROPER.o \
		$(SRCDIR)/PYROLY.o \
		$(SRCDIR)/REDIST.o \
		$(SRCDIR)/REXIT.o \
		$(SRCDIR)/RISER.o \
		$(SRCDIR)/RISER_add.o \
		$(SRCDIR)/SOLCON.o \
		$(SRCDIR)/SOLFLO.o \
		$(SRCDIR)/SOLVEL.o \
		$(SRCDIR)/SURE_H2S.o \
		$(SRCDIR)/TDER.o \
		$(SRCDIR)/VOLCOM.o \
		$(SRCDIR)/WRITE0.o \
		$(SRCDIR)/WRITE2.o \
		$(SRCDIR)/WRITE4.o \
		$(SRCDIR)/WRITE5.o \
		$(SRCDIR)/Z_BUBB.o \
		$(SRCDIR)/Z_CORE.o \
		$(SRCDIR)/Z_EPSA.o \
		$(SRCDIR)/Z_EPSS.o \
		$(SRCDIR)/Z_ESMF.o \
		$(SRCDIR)/Z_EXP.o \
		$(SRCDIR)/Z_NAME.o \
		$(SRCDIR)/Z_NUSL.o \
		$(SRCDIR)/Z_QGEN.o \
		$(SRCDIR)/Z_QTRN.o \
		$(SRCDIR)/Z_RHOG.o \
		$(SRCDIR)/Z_SHWO.o \
		$(SRCDIR)/Z_SPLT.o \
		$(SRCDIR)/Z_THC.o \
		$(SRCDIR)/Z_UMF.o \
		$(SRCDIR)/Z_VELO.o \
		$(SRCDIR)/Z_VIS.o \

DEST          = ./

PROGRAM	      = transport_reactor.so

all:		$(PROGRAM)

#$(PROGRAM):     $(OBJS) 
#		$(LINKER) $(OBJS) $(LIBS) -fPIC -shared -o $(PROGRAM)
$(PROGRAM):     $(OBJS) $(LIBS) 
		@echo -n "Loading $(PROGRAM) ... "
		@$(LINKER) $(FFLAGS) $(OBJS) $(LIBS) -fPIC -shared -o $(PROGRAM)
		@echo "done"

clean:;		@rm -f $(OBJS) $(PROGRAM)

