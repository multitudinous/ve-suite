// SolidOxideElectrolyte.h: interface for the CSolidOxideElectrolyte class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(_SOLIDOXIDEELECTROLYTE_)
#define _SOLIDOXIDEELECTROLYTE_


#include "Electrolyte.h"

namespace Vision21 {

class CSolidOxideElectrolyte : public CElectrolyte  
{
public:
	CSolidOxideElectrolyte();
	virtual ~CSolidOxideElectrolyte();

	double GetMolarDelHStd(double Ti);
	double GetMolarDelSStd(double Ti);
	double GetReactionDeltaEntropy(double TCathode, double TAnode, double AnodeP, double CathodeP);
	double GetReactionDeltaGibbs(double TCathode, double TAnode, double AnodeP, double CathodeP);
	double GetNodeENernstVoltage(int i);
	double GetReactantActivity(int i,double AnodeP,double CathodeP);
	double GetProductActivity(int i,double AnodeP,double CathodeP);

};

} // end namespace Vision21

#endif // !defined(_SOLIDOXIDEELECTROLYTE_)
