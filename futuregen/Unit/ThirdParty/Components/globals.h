
#include "ProgDefs.h"
#include "Sp_dbase.h"

namespace Vision21 {

// globals

extern UINT	CUR;
extern UINT	NXT;

extern	double hSp(UINT idx, double lfT);
extern  double MolarEntropySp(UINT idx, double T, double P);
extern 	double CPTterm(int GPidx, double t);
extern	CSpecieDatabase	G_SpDb;


} // end namespace Vision21
