// SeparatorPlate.h: interface for the CSeparatorPlate class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(_SEPARATORPLATE_)
#define _SEPARATORPLATE_

#include "ProgDefs.h"
#include "GasPhaseModel.h"

namespace Vision21 {

class CSeparatorPlate  
{
public:
	void PrintSolidTemps(FILE *f);
	double GetMinTimeStep();
	void PrintData(FILE *f,bool bDoHeader);
	int ContinueSetup(const double * lfDelX, const double * lfXS_Area, bool bDoRadiation);
	int Setup(int nNoNodes,double lfInitialTemp,const double *lfDelX, ChanGeom *pChanGeom, double lfCellWidth, const double *lfXS_Area, double lfC, double lfRho, bool bDoRadiation, double lfSeparatorPlateThickness=0.0, const struct CellResistance *CellResistance=NULL, const double *plfCurrentDensity=NULL);
	void SetLowerSurfaceHeatRemoval(const double * lfQFromSeparatorToAnodeGas, const double * lfQFromSepToElec);
	void SetUpperSurfaceHeatRemoval(const double * lfQFromSeparatorToCathodeGas, const double * lfQFromSepToElec);
	int AnalyzeNextTimeStep(double lfTimeStep);
	CSeparatorPlate();
	virtual ~CSeparatorPlate();
	double GetNodeResistance(int i);
	double m_lfTemp[MAX_STORE][MAX_AXIALNODES];
private:
	double m_lfCellWidth;
	struct ChanGeom m_ChanGeom;
	double AvePlateTemp();
	const double *lfCurrentDensity;
	double GetQGenOfNode(int i);
	bool	m_bDoRadiation;
	const double * m_plfLowerRadTemp;
	const struct CellResistance *pCellRes;
	double m_lfSeparatorPlateThickness;
	const double * m_plfUpperRadTemp;
	const double * DelX;
	const double * XS_Area;
	double m_lfC;
	double m_lfRho;
	int m_nNoNodes;
	const double * m_plfQFromSeparatorToAnodeGas;
	const double * m_plfQFromSeparatorToCathodeGas;
};

} // end namespace Vision21

#endif // !defined(_SEPARATORPLATE_)
