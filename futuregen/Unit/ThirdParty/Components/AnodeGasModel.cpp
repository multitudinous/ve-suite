// AnodeGasModel.cpp: implementation of the CAnodeGasModel class.
//
//////////////////////////////////////////////////////////////////////

#include "globals.h"
#include "AnodeGasModel.h"


namespace Vision21 {

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CAnodeGasModel::CAnodeGasModel()
{
	m_bManualConcLimitSpec=false;
	m_lfAnodeConcLimitingCurrDen=4000.0;//amp/cm2
}

CAnodeGasModel::~CAnodeGasModel()
{

}

int CAnodeGasModel::AnalyzeNextTimeStep(double lfTimeStep)
{
	int zz=GasPhaseModel::AnalyzeNextTimeStep(lfTimeStep);
	return(zz);
}

int CAnodeGasModel::SetBoundaryFlowsFromCurrent( const double * plfCurrentDensity)
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



