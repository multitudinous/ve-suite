// CathodeGasModel.h: interface for the CCathodeGasModel class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(_CATHODEGASMODEL_)
#define _CATHODEGASMODEL_

#include "GasPhaseModel.h"

namespace Vision21 {

class CCathodeGasModel : public GasPhaseModel  
{
public:
	double m_lfCathodeConcLimitingCurrDen;
	bool m_bManualConcLimitSpec;
	CCathodeGasModel();
	virtual ~CCathodeGasModel();

	int AnalyzeNextTimeStep(double lfTimeStep);
	int SetBoundaryFlowsFromCurrent( const double * plfCurrentDensity);

	virtual void ContinuationSetup(bool bDoCathodeFixedFlow, double lfVelInput,bool bDoQS, bool bDoRef, bool bDoEquilRef, const double *lfXS_AreaAnode, const double *lfDelX)=0;
	virtual void Setup(int nNoNodes, bool bDoCathodeFixedFlow, bool bDoQS, bool bDoRef, bool bDoEquilRef, ChanGeom *CaGeom, double lfCellWidth, const double *lfXS_AreaAnode, const double *lfDelX, double lfAnodeVelInput, double lfAnodePInput, double lfAnodePExit, double lfAnodeTInput, int nNoAnodeSp, const double *lfAnodeSpInput, bool bMan, double lfManVal)=0;
	virtual double GetOxygenFlowRate()=0;
	virtual double GetUtilization(void)=0;
	virtual double GetLimitingSupplyCurrentDensity(int i)=0;
	virtual double GetCathodeLimitingCurrentDensity(int i, double CathodeP, double Ti, double *lfWhich_n=NULL)=0;
	virtual	double GetCathodeConcLoss(double Ti, double CathodeP, int i, double lfCurrentDensity)=0;
	virtual	double GetNetEnthalpyInputToCell(int i, const double CellT, double CathodeT)=0;

private:
	virtual void SetFlowsFromCurrent(int i, double lfCurrDen)=0;
};

} // end namespace Vision21

#endif // !defined(_CATHODEGASMODEL_)
