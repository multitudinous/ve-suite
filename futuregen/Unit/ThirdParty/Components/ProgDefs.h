#if !defined(_PROGDEFS_INCLUDED_)
#define _PROGDEFS_INCLUDED_

namespace Vision21 {

typedef unsigned int  UINT;


extern UINT	CUR;
extern UINT	NXT;

extern	double hSp(UINT idx, double lfT);
extern  double MolarEntropySp( UINT idx, double T, double P);
extern 	double CPTterm(int GPidx, double t);

#define MAX_AXIALNODES			15
#define MAX_SURFACESPECIEFLUX	10
#define MAX_NOSP				10
#define MAX_STORE				2		// this is number of time steps to handle

#define	FARADAY					96439 //coulombs/g-mole electrons
#define Runiv					8.3144//joule/g-mole
#define g_lfStephanBoltz		5.67051E-8//watt/m2-K4
#define CURRENT_PRECISION		0.0001// in fraction units.

#define PRINT_STEP				1

#define MAX(x,y)	((x)>(y) ? (x):(y))
#define MIN(x,y)	((x)<(y) ? (x):(y))
#define ABS(x)		((x)>(0.0) ? (x):(-(x)))

struct Resistance {   // formula is Resistance[ohm-cm]=Const + Linear*T + PreExpon*exp(Expon/T)
	double lfConst;//ohm-cm
	double lfLinear;//ohm-cm/K
	double lfPreExpon;//ohm-cm
	double lfExpon;//Kelvin
	};

struct CellResistance {
	struct Resistance AirElectrodeRes;
	struct Resistance FuelElectrodeRes;
	struct Resistance ElectrolyteRes;
	struct Resistance InterconectRes;
	double lfAnodeInterfaceResistance;//ohm-cm2
	double lfCathodeInterfaceResistance;//ohm-cm2
	};

} // end namespace Vision21


#endif //!_PROGDEFS_INCLUDED_
