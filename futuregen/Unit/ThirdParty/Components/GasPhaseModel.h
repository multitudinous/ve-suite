// GasPhaseModel.h: interface for the GasPhaseModel class.
//
//////////////////////////////////////////////////////////////////////
#include "ProgDefs.h"
#include "Sp_dbase.h"

#if !defined(_GASPHASEMODEL_)
#define _GASPHASEMODEL_

namespace Vision21 {


#define QS_LOWER_CURRENT	0
#define QS_ERROR			-1
#define QS_GOOD				1

struct ChanGeom{
	double	lfHeight;
	double	lfChannelWidth;//m
	double	lfChannelSpacing;//m  This is the spacing between the channels (i.e., the wall between channels)
	double	lfNumberOfChannels;// yes, keep double, not integer
	double	lfChannelLength;//m
	};


class GasPhaseModel
{
private:
	struct ChanGeom m_ChanGeom;
	void AnalyzeNodeSurfaceHeat(int i, double T);
	bool m_bDoFixedFlow;
	virtual void SetFlowsFromCurrent(int i, double lfCurrDen);
	double GetMaxTemp();
	double GetMinTemp();
	double GetNetSurfaceMomentumIn(int i, bool bDoDynAnal);
	double GetNetSurfaceEnergyFlowIn(int i, double Tgas);
	double GetGibbsFreeEnergy_Reform(double h);
	double GetGibbsFreeEnergy_WaterGasShift(double Temp);
	double GetMixIntEnergyFromT(double *Specie,double Temp);
	double GetTurbulentFriction(double Re, double rough, double guess);
	double GetMolarCp(double *Sp,double T);
	void AnalyzePressureEqu(double lfTimeStep);
	double GetMomentumNetMassFlowIn(int i);
	double GetTFromE(double *Sp, double lfIntEnergy, double Ttry);
	double GetNetMassInAtSurface(int i);
	double KE(int i);
	int AnalyzeSpeciesEqu(double lfTimeStep);
	double GetPressureForce(int i);
	double GetMixCp(double *sp,double T);
	double get_friction(double Re, double rough, double guess);
	double GetFrictionForce(int i);
	double GetNetMomentumFlowIn(int i);
	double GetNetIntEnergyFlowIn(int i);
	int AnalyzeMomentumEqu(double lfTimeStep);
	int AnalyzeMassEqu(double lfTimeStep);
	int AnalyzeEnergyEqu(double lfTimeStep);

	double m_lfQdot[MAX_AXIALNODES];
	double Pnew[MAX_AXIALNODES];
	UINT	m_uSpIdx[MAX_SURFACESPECIEFLUX];
	double *m_pplfMoleFluxIntoGas[MAX_SURFACESPECIEFLUX];
	int		m_nNoSurfaceSpecieFluxes;
	double Psave[MAX_AXIALNODES];

	const double *m_plfLowerWallTemp[MAX_AXIALNODES*MAX_STORE];
	const double *m_plfLowerWallTempSource;
	const double *m_plfUpperWallTemp[MAX_AXIALNODES*MAX_STORE];
	const double *m_plfUpperWallTempSource;
	const double *m_plfElecTemp[MAX_AXIALNODES*MAX_STORE];
	const double *m_plfElecTempSource;
	double m_lfHeatRateAddedUpperWall[MAX_AXIALNODES];
	double m_lfHeatRateAddedLowerWall[MAX_AXIALNODES];

public:
	double m_lfVelInput;
	double m_bDoWeightingOfCatalyst;
	double GetNetMassFlowIn(int i);
	double m_lfCellWidth;
	double m_lfFlowResistanceGain;
	double get_nusselt(double Re, double Pr);
	double GetMixMu(double *spXi,double Temp);
	double GetMixK(double *spXi,double Temp);
	void VelMultiplyFactor(double factor);
	double GetMixMW(double *sp);
	double GetMixHFromT(double *sp,double Tguess);
	double GetTFromH(double *sp, double H);
	void NormalizeSpeciesMoleFractions(double *Sp, int nNoSp);
	void AnalyzeSurfaceHeats();
	double m_lfPublic_HeatRateAddedUpperWall[MAX_AXIALNODES];
	double m_lfPublic_HeatRateAddedLowerWall[MAX_AXIALNODES];
	void SetLowerSurfaceTemp(const double *lfTempArray);
	void SetUpperSurfaceTemp(const double *lfTempArray);
	double GetMassFlowOut(int nNode);
	bool bFixedFlow(void);
	int m_nNoSp;
	bool HitPDropLimit(int i, double PLimit);
	int AnalyzeNodeQSGasPhysics(int i, double lfCurrDen);
	bool QSUpdateOfXi2(int i, double CH4ProdRate);
	int AnalyzeQSSpeciesAndDensity();
	int AnalyzeQSEnthalpy();
	int AnalyzeQSGasPhysics(double *pCurrDen);
	void DoublePressDrop_Vel();
	double GetSpecieExitFlowAtNode(int SpIdx,int Node);
	double GetNetKgMoleFlowIn(int SpIdx, int nodei);
	void UpdateBoundaryValues(double lfAnodePInput, double lfAnodePExit, double lfAnodeTInput, int nNoSp, double *lfAnodeSpInput);
	double GetMinTimeStep();
	void PrintData(FILE *f, bool bDoHeader, char *szType, UINT uNoSp, UINT *uSpID);
	void ContinuationSetup(bool bDoFixedFlow, double lfVelInput,bool bDoQS, bool bDoRef, bool bDoEquilRef,const double * lfXS_AreaAnode, const double * lfDelX);
	void Setup(int nNoNodes, bool bDoFixedFlow, bool bDoQS, bool bDoReforming, bool bDoEqilRef, ChanGeom *ChanGeom, double lfCellWidth, const double *lfXS_AreaAnode, const double *lfDelX,double lfAnodeVelInput, double lfAnodePInput, double lfAnodePExit, double lfAnodeTInput,int nNoAnodeSp, const double *lfAnodeSpInput);
	double GetTemp(int i);
	int AnalyzePressures(void);
	int AnalyzeTemperatures(void);
	void SetElectrolyteTempArray(const double *plfElecTemp);
	int AddSpecieFluxArray(UINT uSpIdx, double *plfSurfaceMoleFlux);
	int AnalyzeNextTimeStep(double lfTimeStep);
	GasPhaseModel();
	virtual ~GasPhaseModel();
	double GetHeatTransferCoef(int i);

	int m_nNoNodes;
	const double *DelX;
	double Sp[MAX_STORE][MAX_AXIALNODES][MAX_NOSP];//All these data are at the same point in space!!
	double P[MAX_AXIALNODES];//Pa
	double T[MAX_AXIALNODES];// K
	double h[MAX_STORE][MAX_AXIALNODES];// this is really Internal Energy, execpt for last node which we assume Enthalpy at different times during the calc., but we always return to Int. energy when done!  Tricky!
	double Rho[MAX_STORE][MAX_AXIALNODES];// kg/m3

	bool m_bCounterFlowTo_SurfaceTempArrayIndicieDirection;
	double Vel[MAX_STORE][MAX_AXIALNODES];// Vel and XS_Area are at the same point in space!!!
	const double *XS_Area;// Vel and XS_Area (Total Flow Area for Entire Cell, not just the channel) are at the same point in space!!!
protected:
	bool m_bDoQS;
};

} // end namespace Vision21

#endif // !defined(_GASPHASEMODEL_)
