// CathodeGasModel.cpp: implementation of the CCathodeGasModel class.
//
//////////////////////////////////////////////////////////////////////

#include "globals.h"
#include "CathodeGasModel.h"

namespace Vision21 {


//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CCathodeGasModel::CCathodeGasModel()
{
	m_bManualConcLimitSpec=false;
	m_lfCathodeConcLimitingCurrDen=4000.0;//amp/cm2
}

CCathodeGasModel::~CCathodeGasModel()
{

}

int CCathodeGasModel::AnalyzeNextTimeStep(double lfTimeStep)
{
	return(GasPhaseModel::AnalyzeNextTimeStep(lfTimeStep));
}

int CCathodeGasModel::SetBoundaryFlowsFromCurrent( const double * plfCurrentDensity)
{
  int i;
	for(i=1;i<m_nNoNodes-1;i++){
		SetFlowsFromCurrent(i,plfCurrentDensity[i]);
		}
	SetFlowsFromCurrent(i,0.0);
	SetFlowsFromCurrent(0,0.0);
	return(1);
}

} // end namespace Vision21




