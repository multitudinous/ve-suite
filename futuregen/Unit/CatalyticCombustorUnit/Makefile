IDL_HOME = ../../IDL
PLATFORM = LINUX
COMPILER = g++
GCC = yes
GXX = yes
F77 = g77
CXXFLAGS =   -g -w -O  -DAPP_NO_THREADS -DXML_USE_NO_THREADS  
CFLAGS =   -g -w -O  -DAPP_NO_THREADS -DXML_USE_NO_THREADS  

LDFLAGS =    
THREADS = none
EXTRA_LIBS =  
PREFIX = /usr/local

include ${XERCESCROOT}/version.incl
include ../Makefile.incl

ifndef TAO_ROOT
  TAO_ROOT = $(ACE_ROOT)/TAO
endif # ! TAO_ROOT

APP = CatalyticCombustorUnitApp

LDLIBS = -lTAO_CosNaming -lTAO_Svc_Utils -lTAO_IORTable -lTAO_Messaging -lTAO_PortableServer -lTAO

include $(ACE_ROOT)/include/makeinclude/wrapper_macros.GNU
include $(TAO_ROOT)/rules.tao.GNU

FILE_UNIT_CLT_OBJS = moduleC.o moduleS.o CatalyticCombustorUnit_i.o CatalyticCombustorUnit_client.o

include $(ACE_ROOT)/include/makeinclude/macros.GNU
include $(ACE_ROOT)/include/makeinclude/rules.common.GNU
include $(ACE_ROOT)/include/makeinclude/rules.nonested.GNU
include $(ACE_ROOT)/include/makeinclude/rules.local.GNU
include $(TAO_ROOT)/taoconfig.mk

vpath %.cpp ${IDL_HOME}


LDFLAGS += -L$(TAO_ROOT)/tao -L$(TAO_ROOT)/orbsvcs/orbsvcs

CPPFLAGS += -g  ${INCLUDES} ${CMP} -I$(TAO_ROOT) \
		-I$(TAO_ROOT)/orbsvcs -I${IDL_HOME} \
		-I./. -I../ -I../V21Helper/

.PRECIOUS: moduleC.i moduleC.cpp moduleS.i moduleS.cpp

OBJECTS = ../string_ops.o				\
	../interface.o					\
	../package.o 					\
	../V21Helper/V21Helper.o 			\
	../V21Helper/SummaryValues/summary_values.o 	\
	../V21Helper/SummaryValues/unit_conversion.o 	\
	../V21Helper/Datatypes/Gas.o 			\
	../V21Helper/Datatypes/GasCell.o 		\
	../V21Helper/Datatypes/Water.o 			\
	../V21Helper/Therm/matrix.o 			\
	../V21Helper/Therm/stream.o 			\
	../V21Helper/Therm/thermo.o  			\
	../V21Helper/Steam67/Steam67.o 			\
	../ThirdParty/Components/cat_comb.o 		\
	../ThirdParty/Reks/ElementReader.o		\
	../ThirdParty/Reks/InpReader.o			\
	../ThirdParty/Reks/MixedThermoReader.o		\
	../ThirdParty/Reks/Reaction.o			\
	../ThirdParty/Reks/ReactionReader.o		\
	../ThirdParty/Reks/reks.o			\
	../ThirdParty/Reks/reks_container.o		\
	../ThirdParty/Reks/REKS_RED_specie_reader.o	\
	../ThirdParty/Reks/reks_solve.o			\
	../ThirdParty/Reks/REKS_Thrm_Info.o		\
	../ThirdParty/Reks/SpecieReader.o		\
	../ThirdParty/Reks/SpecieThermo.o		\
	../ThirdParty/Reks/StringParseUtil.o		\
	../ThirdParty/Reks/usr_gas.o			\
	../ThirdParty/Reks/expl_dll.o			\
	../ThirdParty/Reks/thermo_db.o			\
	../ThirdParty/Reks/reactionObject.o		\
	../ThirdParty/Reks/surface_chem.o		\
	../ThirdParty/Reks/surf_parser.o		\
	../ThirdParty/Cvode/band.o			\
	../ThirdParty/Cvode/cvband.o			\
	../ThirdParty/Cvode/cvbandpre.o			\
	../ThirdParty/Cvode/cvdense.o			\
	../ThirdParty/Cvode/cvdiag.o			\
	../ThirdParty/Cvode/cvode.o			\
	../ThirdParty/Cvode/cvspgmr.o			\
	../ThirdParty/Cvode/dense.o			\
	../ThirdParty/Cvode/iterativ.o			\
	../ThirdParty/Cvode/llnlmath.o			\
	../ThirdParty/Cvode/nvector.o			\
	../ThirdParty/Cvode/spgmr.o		

.SUFFIXES : .o .cpp .cc	

.cpp.o :
	$(CXX) -g -c $(CPPFLAGS) -w -o $@ $<

.cc.o :
	$(CXX) -g -c $(CPPFLAGS) -w -o $@ $<

.f.o : 
	$(F77) -g -c -w -o $@ $<
all: $(APP)

$(APP): $(OBJECTS) $(addprefix $(VDIR),$(FILE_UNIT_CLT_OBJS))
	g++ ${PLATFORM_LIB_LINK_OPTIONS} $(LDFLAGS) -o $@ $^ $(TAO_SRVR_LIBS) $(VLDLIBS) ${LIBRARY_SEARCH_PATHS} ${LIBRARY_NAMES} ${EXTRA_LINK_OPTIONS} $(POSTLINK)

clean:
	rm -rf *.o $(APP) $(FILE_UNIT_CLT_OBJS) $(OBJECTS)
