// Electrolyte.h: interface for the CElectrolyte class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(_CELECTROLYTE_)
#define _CELECTROLYTE_

#include "ProgDefs.h"
#include "AnodeGasModel.h"
#include "CathodeGasModel.h"
#include "SeparatorPlate.h"

namespace Vision21 {

#define GOOD_RESULT		1
#define BAD_SETUP		0
#define	TRY_REDUCED_VOLTAGE		-1
#define MUST_HAVE_ELECTROLYSIS_CONDITIONS	-2

#define MAX_CURRENTARRAY	500

class CElectrolyte  
{
public:
	double m_lfTemp[MAX_STORE][MAX_AXIALNODES];
	double m_lfLoadVoltage;
	CAnodeGasModel *An;
	CCathodeGasModel *Ca;
	CSeparatorPlate *AirSepPlt;
	CSeparatorPlate *FuelSepPlt;

public:
	double GetAveCellNernstVoltage();
	int AnalyzeCurrDenGivenCurrent_GasQS(double lfTotalCurrent, bool bDoGradualSolutionSearching);
	int AnalyzeCurrDenGivenCurrent_FullyQS(double lfTotalCurrent, CSeparatorPlate &SP, bool m_bIfQuasiSteadySolidPhase_ThenDoTempUpdate, bool bDoGradualSolutionSearching);
	double GetCathodeAlphaCoef(double Ti);
	virtual 	double GetMolarDelHStd(double Ti)=0;
	virtual		double GetMolarDelSStd(double Ti)=0;
	virtual		double GetReactionDeltaEntropy(double TCathode, double TAnode, double AnodeP, double CathodeP)=0;
	virtual		double GetReactionDeltaGibbs(double TCathode, double TAnode, double AnodeP, double CathodeP)=0;
	virtual		double GetNodeENernstVoltage(int i)=0;
	virtual		double GetReactantActivity(int i,double AnodeP,double CathodeP)=0;
	virtual		double GetProductActivity(int i,double AnodeP,double CathodeP)=0;

	double CurrentAveCellTemp();
	bool bUpdateSolidTemps(int i, double TotalEntropyChange, double ENurnst,double CathodeEtaConc,double AnodeEtaConc,double lfCurrentDensityGuess, CSeparatorPlate &SP,bool bDoQSSolidTempUpdate);
	int AnalyzeCurrDenGivenVoltage(double lfCellVoltage);
	int AnalyzeCurrDenGivenVoltage_FullyQS(double lfCellVoltage, CSeparatorPlate &SP, bool bDoQSSolidTempUpdate, bool bDoHoneIn);
	int AnalyzeCurrDenGivenVoltage_GasQS(double lfCellVoltage, bool bHoneIn);

	int AnalyzeCurrDen();
	int AnalyzeCurrDen_FullyQS(CSeparatorPlate &SP, bool bDoQSSolidTempUpdate);
	int AnalyzeCurrDen_GasQS();

	void SetUniformCurrentDensities(double val);
	void InitializeElectrolyteData(double Ti);
	int AnalyzeOveallLosses(const double *CathodeP, const double *AnodeP);
	double GetMinTimeStep( const double * AnodeT, const double * CathodeT);
	double GetTotalCurrent(void);
	double GetLoadVoltage(void);
	void PrintData(FILE *f,bool b);
	void PrintSolidTemps(FILE *f);
	int ContinueSetup(double lfLoad, double lfCathodeExgCurDensityB, double lfCathodeExgCurDensityM, double lfAnodeExgCurDensity, const double *lfDelX, const double *lfXS_Area, bool bDoRadiation);
	int Setup(CAnodeGasModel *pAnode, CCathodeGasModel *pCathode, CSeparatorPlate *pAirSepPlt, CSeparatorPlate *pFuelSepPlt, int nNoNodes,double lfInitialTemp, double lfElectrolyteThickness, double lfAnodeThickness,double lfCathodeThickness,double lfLoad, const struct CellResistance* CellRes, double lfCathodeExgCurDensityB, double lfCathodeExgCurDensityM, double lfAnodeExgCurDensity, double lfAlphaCathodeBCoef, double lfAlphaCathodeMCoef, double lfAlphaAnode, const double *lfDelX, ChanGeom *pChanGeom, double lfCellWidth, const double *lfXS_Area, double lfC, double lfRho, bool bDoRadiation);
	void SetGeomArrays(const double *lfXS_Area, const double *lfDelX);
	void SetLowerSurfaceHeatRemoval(const double *lfQFromElectrToCathodeGas, const double * lfQFromSepToElec);
	void SetUpperSurfaceHeatRemoval(const double *lfQFromElectrToAnodeGas, const double * lfQFromSepToElec);
	int AnalyzeNextTimeStep(double lfTimeStep, const double * AnodeT, const double * CathodeT);
	CElectrolyte();
	virtual ~CElectrolyte();

	const double * DelX;
	double m_lfGmMoleElectronsPerGmMoleH2;//g-moles of electrons per g-mole of H2
	double m_lfGmMoleElectronsPerGmMoleO2;
	double lfCurrentDensity[MAX_AXIALNODES];
	double GetQGenOfCellNode(int i);
	double GetTotalSepPltResistance(int i);

private:
	struct ChanGeom m_ChanGeom;
	double m_lfCellWidth;
	double GetCathodeExchangeCurrentDensity(double Temp);
	const double * m_plfLowerRadTemp;
	const double * m_plfUpperRadTemp;
	double GetAnodeActivationLoss(double Ti,double lfCurrentDensity);
	double GetCathodeActivationLoss(double Ti,double lfCurrentDensity);
	double GetOverallNodeVoltageDrop(int i);
	double GetCellNodeResistance(int nNode, double T);
	int m_nNoNodes;
	double lfEntropyHeatGen[MAX_AXIALNODES];
	double m_lfNodalIdealVoltage[MAX_AXIALNODES];
	double m_lfNodalIdealVoltage_AtElectrolyteSurface[MAX_AXIALNODES];
	double m_lfLoadResistance;//ohm
	double m_lfAnodeExchangeCurrentDensity;
	double m_lfAlphaAnode;
	double m_lfCathodeExchangeCurrentDensityB;//offset for temperature depend. linear curve fit--data must be for Geometric Facial Surface Area (not for actual area throughout all pores).
	double m_lfCathodeExchangeCurrentDensityM;//slope for temperature depend. linear curve fit
	double m_lfAlphaCathodeBCoef;
	double m_lfAlphaCathodeMCoef;
	double m_lfRho;
	double m_lfC;
	bool m_bDoRadiation;
	const double * XS_Area;
	const double * m_plfQFromElectrToCathodeGas;
	const double * m_plfQFromElectrToAnodeGas;

	const struct CellResistance *pCellRes;
	double m_lfElectrolyteThickness;
	double m_lfAnodeThickness;
	double m_lfCathodeThickness;
//	double m_lfInterconnectThickness;
	
	double m_lfSumCathActivLoss;
	double m_lfSumAnodActivLoss;
	double m_lfSumCathConcLoss;
	double m_lfSumAnodConcLoss;
	double m_lfSumOhmicLoss;
	double m_lfSumTotalLoss;
	double m_lfStoredExponentParameter[MAX_CURRENTARRAY];
	double m_lfStoredCurrentDensity[MAX_CURRENTARRAY];
};

} // end namespace Vision21

#endif // !defined(_CELECTROLYTE_)
