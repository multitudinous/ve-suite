// AnodeGasModel.h: interface for the CAnodeGasModel class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(_ANODEGASMODEL_)
#define _ANODEGASMODEL_


#include "GasPhaseModel.h"

namespace Vision21 {

class CAnodeGasModel : public GasPhaseModel  
{
public:
	CAnodeGasModel();
	virtual ~CAnodeGasModel();
	int AnalyzeNextTimeStep(double lfTimeStep);
	int SetBoundaryFlowsFromCurrent(const double *plfCurrentDensity);


	virtual double NodeInputChemicalPower(int nNode)=0;
	virtual void ContinuationSetup(bool bDoAnodeFixedFlow, double lfVelInput,bool bDoQS, bool bDoRef, bool bDoEquilRef,const double * lfXS_AreaAnode, const double * lfDelX)=0;
	virtual void Setup(int nNoNodes, bool bDoAnodeFixedFlow, bool bDoQS, bool bDoRef, bool bDoEquilRef, ChanGeom *AnGeom, double lfCellWidth, const double * lfXS_AreaAnode, const double * lfDelX, double lfAnodeVelInput, double lfAnodePInput, double lfAnodePExit, double lfAnodeTInput, int nNoAnodeSp, const double * lfAnodeSpInput, bool bMan, double lfManVal)=0;
	virtual double GetFuelFlowRate()=0;
	virtual double GetUtilization(void)=0;
	virtual double GetLimitingSupplyCurrentDensity(int i)=0;
	virtual double GetAnodeLimitingCurrentDensity(int i, double AnodeP, double Ti)=0;
	virtual double GetAnodeConcLoss(double Ti, double AnodeP, int i, double lfCurrentDensity)=0;
	virtual double GetNetEnthalpyInputToCell(int i, const double CellT, double AnodeT)=0;

	double m_lfAnodeConcLimitingCurrDen;
	bool m_bManualConcLimitSpec;
private:
	void SetFlowsFromCurrent(int i, double lfCurrentDen)=0;

};

} // end namespace Vision21

#endif // !defined(_ANODEGASMODEL_)
