// sp_dbase.h
// Class CSpecieDatabase

#ifndef SP_DBASEDEFS
#define SP_DBASEDEFS

#include <string>
#include "ProgDefs.h"

// why ?? #include "Electrolyte.h"

namespace Vision21 {

enum SpDbTransportVariables{
	SpDbTransportVarSTART=0,
	Cp0=0,
	Cp1,
	Cp2,
	Cp3,
	Cp4,
	CP_TMIN,
	CP_TMAX,
	Mu0,
	Mu1,
	Mu2,
	Mu3,
	MU_TMIN,
	MU_TMAX,
	K0,
	K1,
	K2,
	K3,
	K_TMIN,
	K_TMAX,
	REF_H,
	REF_S,
	MW,
	SpDbTransportVarEND=MW
	};

#define	NO_CPVAL	5	/* this is the total number of Cp coefs!! */
#define	NO_MUVAL	4	/* this is the total number of viscosity coefs!! */
#define	NO_KVAL		4	/* this is the total number of conductivity coefs!! */

// GasSpecie has 22x8 + ~40 bytes = 216 bytes per specie!
struct GasSpecie {
	std::string	gasname;
	std::string	ref_info;
	double	lfTDData[SpDbTransportVarEND+1];
	};

//species index
#define		H2	0
#define		CO	1
#define		H2O	2
#define		CO2	3
#define		O2	4
#define		N2	5
#define		CH4 6
#define		SP_LAST CH4

#define		CONST_SPHEAT	1200.0*28.0 // joule/kg-mole K -- this is a rough estimate for typical Cp value

extern char *SpecieNames[MAX_NOSP];

class CSpecieDatabase// : public CObject
{
//	DECLARE_SERIAL(CSpecieDatabase)

public:
	bool	m_bDoConstCp;
	int	wNoSpecies;	// actual no. of species loaded
	struct	GasSpecie	Specie[MAX_NOSP];
	double	REFERENCE_T;	/* janaf reference state temperature */
	double	REFERENCE_P;	/* janaf reference state pressure is .1MPa*/
	
	// Functions
	CSpecieDatabase();
	~CSpecieDatabase();
	double	getSpecieCp(double Temp,int GPidx);
	double	getSpecieMu(double Temp,int GPidx);
	double	getSpecieK(double Temp,int GPidx);
private:

};

} // end namespace Vision21

#endif
