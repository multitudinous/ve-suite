// SolidOxideAnodeGasModel.h: interface for the CSolidOxideAnodeGasModel class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(_SOLIDOXIDEANODEGASMODEL_)
#define _SOLIDOXIDEANODEGASMODEL_


#include "AnodeGasModel.h"

#include "Electrolyte.h"

namespace Vision21 {

class CSolidOxideAnodeGasModel : public CAnodeGasModel  
{
public:
	CSolidOxideAnodeGasModel();
	virtual ~CSolidOxideAnodeGasModel();

	double NodeInputChemicalPower(int nNode);
	void ContinuationSetup(bool bDoAnodeFixedFlow, double lfVelInput,bool bDoQS, bool bDoRef, bool bDoEquilRef,const double * lfXS_AreaAnode, const double * lfDelX);
	void Setup(int nNoNodes, bool bDoAnodeFixedFlow, bool bDoQS, bool bDoRef, bool bDoEquilRef, ChanGeom *AnGeom, double lfCellWidth, const double * lfXS_AreaAnode, const double * lfDelX, double lfAnodeVelInput, double lfAnodePInput, double lfAnodePExit, double lfAnodeTInput, int nNoAnodeSp, const double * lfAnodeSpInput, bool bMan, double lfManVal);
	double GetFuelFlowRate();
	double GetUtilization(void);

	double m_lfSurfaceH2MoleFluxIntoControlVol[MAX_AXIALNODES];
	double m_lfSurfaceH2OMoleFluxIntoControlVol[MAX_AXIALNODES];
	double GetLimitingSupplyCurrentDensity(int i);
	double GetAnodeLimitingCurrentDensity(int i, double AnodeP, double Ti);
	double GetAnodeConcLoss(double Ti, double AnodeP, int i, double lfCurrentDensity);
	double GetNetEnthalpyInputToCell(int i, const double CellT, double AnodeT);

private:
	void SetFlowsFromCurrent(int i, double lfCurrentDen);
	double m_lfGmMoleElectronsPerGmMoleH2;
};

} // end namespace Vision21

#endif // !defined(_SOLIDOXIDEANODEGASMODEL_)
