// PlannerFuelCellModel.h: interface for the CPlannerFuelCellModel class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(_PLANNERFUELCELLMODEL_)
#define _PLANNERFUELCELLMODEL_

#include "AnodeGasModel.h"
#include "CathodeGasModel.h"
#include "Electrolyte.h"
#include "SeparatorPlate.h"
#include <string>

namespace Vision21 {

class CPlanarFuelCellModel  
{
public:
	CPlanarFuelCellModel();
	virtual ~CPlanarFuelCellModel();
public:
	double AnalyzeOverallPerformance();
	int UpdateBoundaryValues(double lfCellVoltage=-1.0, bool bDoGradualSolutionSearching=false);

	double GetMinTimeStep();
	void PrintData(FILE *f, bool bDoHeader);
	void JumpstartVelProfiles(double PAnE, double PAnL, double PCaE, double PCaL);
	void PrintSolidTemps(FILE *f);
	int SetupProblem(void);
	int AnalyzeFuelCell(double lfTimeStep);
	void PrintConfig(FILE *f);
	double AnalyzeOverallPerformanceFollowingDetailedAnalysis(FILE *f, double lfMaxUtil);

	void TestArray(double *Array);

private:
	double GetDelG(const double lfH2Xi, const double lfO2Xi, const double lfH2OXi, const double Ti, const double AnodeP, const double CathodeP);
	double GetStdGibbs(double Temp);
	int AnalyzeQSTemps(double lfCellVoltage, double lfCurrentFactor, bool bDoAve);

	
	int AnalyzeNodalInterfacialHeatFluxes();
//	int AnalyzeNodalCurrentDensity(double lfCellVoltage,double lfCurrentFactor, bool bDoAve);


	double m_lfElecQSTemp[MAX_AXIALNODES];
	double m_lfSepQSTemp[MAX_AXIALNODES];

public:
	int UpdateBoundaryValuesViaCurrentSpec(double lfTotalCurrent);
	bool m_bDoManualLogDiffusionOverPotential;
	double m_lfAnodeLogLimitingDiffusionCurrDen;
	double m_lfCathodeLogLimitingDiffusionCurrDen;

	double m_lfXS_AreaSeparator[MAX_AXIALNODES];
	double m_lfXS_AreaElectrolyte[MAX_AXIALNODES];
	double m_lfDelX[MAX_AXIALNODES];
	double m_lfXS_AreaAnode[MAX_AXIALNODES];
	double m_lfXS_AreaCathode[MAX_AXIALNODES];
	double m_lfSeparatorThickness;
	double m_lfSeparatorRho;
	double m_lfSeparatorC;
	double m_lfSeparatorInitialTemp;
	double m_lfElectrolyteThickness;
	double m_lfAnodeThickness;
	double m_lfCathodeThickness;
	double m_lfCellRho;
	double m_lfCellC;
	double m_lfCellInitialTemp;
	struct CellResistance	CellRes;
	double m_lfCathodeExgCurDensityBCoef;//offset
	double m_lfCathodeExgCurDensityMCoef;//slope
	double m_lfAnodeExgCurDensity;
	double m_lfAlphaAnode;
	double m_lfAlphaCathodeBCoef;
	double m_lfAlphaCathodeMCoef;
	int		m_nNoNodes;
	double	m_lfCellLength;
	double	m_lfCellWidth;
	struct	ChanGeom AnGeom;
	double	m_lfAnodeVelInput;
	double	m_lfAnodeSpInput[MAX_NOSP];
	int		m_nNoAnodeSp;
	struct	ChanGeom CaGeom;
//	double	m_lfCathodeHeight;
//	double	m_lfCathodeWidth;//m
//	double	m_lfCathodeChannelWidth;//m
//	double	m_lfCathodeChannelSpacing;//m
	double	m_lfCathodeVelInput;
	double	m_lfCathodeSpInput[MAX_NOSP];
	int		m_nNoCathodeSp;
	double	m_lfCathodePExit;
	double	m_lfAnodePExit;
	double	m_lfCathodePInput;
	double	m_lfAnodePInput;
	double	m_lfAnodeTInput;
	bool m_bDoCathodeFixedFlow;
	bool m_bDoAnodeFixedFlow;
	bool m_bIfQuasiSteadySolidPhase_ThenDoTempUpdate;
	double	m_lfCathodeTInput;
	double QS_Converged();
	void DoubleAnodePressDrop_Vel();
	void DoVICurve(FILE *, FILE *);
	int SetupContinuationProblem(void);
	bool	bProbSetup;
	std::string m_szDescription;
	bool	m_bDoQuasiSteadySolidPhase;
	bool	m_bDoQuasiSteadyGasFlow;
	bool	m_bDoRadiation;
	double m_lfLoadResistance;

	class CAnodeGasModel	*AnodeGas;
	class CCathodeGasModel	*CathodeGas;
	class CElectrolyte		*Electrolyte;
	class CSeparatorPlate	SepPlt_AirSide;
	class CSeparatorPlate	SepPlt_FuelSide;
};

} // end namespace Vision21

#endif // !defined(_PLANNERFUELCELLMODEL_)
