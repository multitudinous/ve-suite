// CFCMcontroller.h : interface of the CFCMcontroller class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(_CFCMCONTROLLER_)
#define _CFCMCONTROLLER_

#include "PlannerSolidOxideFuelCellModel.h"

struct REIInput;

namespace Vision21 {

class CFCMcontroller
{
public:
	CFCMcontroller();
	virtual ~CFCMcontroller();
	void OnAnalyzedynamics(double lfStopTime, REIInput* rei_inp, const char *outfile);
	void OnAnalyzecontinue();
	void OnAnalyzeVicurve();
	void OnAnalyzeSteadystate();
	void OnAnalyzeOverallperformance();
	void OnHeatxgr();

private:
	double m_lfPrintTimeStep;
	CPlanarSolidOxideFuelCellModel	SolidOxide;
	bool m_bDoMCFC;
	CPlanarFuelCellModel *m_pFCModel;

};

} // end namespace Vision21

/////////////////////////////////////////////////////////////////////////////

#endif // !defined(_CFCMCONTROLLER_)
