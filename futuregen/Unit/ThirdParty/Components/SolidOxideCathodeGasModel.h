// SolidOxideCathodeGasModel.h: interface for the CSolidOxideCathodeGasModel class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(_SOLIDOXIDECATHODEGASMODEL_)
#define _SOLIDOXIDECATHODEGASMODEL_


#include "Electrolyte.h"

#include "CathodeGasModel.h"

namespace Vision21 {

class CSolidOxideCathodeGasModel : public CCathodeGasModel  
{
public:
	CSolidOxideCathodeGasModel();
	virtual ~CSolidOxideCathodeGasModel();

	void ContinuationSetup(bool bDoCathodeFixedFlow, double lfVelInput,bool bDoQS, bool bDoRef, bool bDoEquilRef, const double *lfXS_AreaAnode, const double *lfDelX);
	void Setup(int nNoNodes, bool bDoCathodeFixedFlow, bool bDoQS, bool bDoRef, bool bDoEquilRef, ChanGeom *CaGeom, double lfCellWidth, const double *lfXS_AreaAnode, const double *lfDelX, double lfAnodeVelInput, double lfAnodePInput, double lfAnodePExit, double lfAnodeTInput, int nNoAnodeSp, const double *lfAnodeSpInput, bool bMan, double lfManVal);
	double GetOxygenFlowRate();
	double GetUtilization(void);

	double	m_lfSurfaceO2MoleFluxIntoControlVol[MAX_AXIALNODES];
	double GetLimitingSupplyCurrentDensity(int i);
	double GetCathodeLimitingCurrentDensity(int i, double CathodeP, double Ti, double *lfWhich_n=NULL);
	double GetCathodeConcLoss(double Ti, double CathodeP, int i, double lfCurrentDensity);
	double GetNetEnthalpyInputToCell(int i, const double CellT, double CathodeT);

private:
	void SetFlowsFromCurrent(int i, double lfCurrDen);
	double m_lfGmMoleElectronsPerGmMoleO2;
};

} // end namespace Vision21

#endif // !defined(_SOLIDOXIDECATHODEGASMODEL_)
