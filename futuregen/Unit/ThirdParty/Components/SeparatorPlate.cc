// SeparatorPlate.cpp: implementation of the CSeparatorPlate class.
//
//////////////////////////////////////////////////////////////////////

#include "globals.h"
#include "GasPhaseModel.h"
#include "SeparatorPlate.h"
#include "math.h"


namespace Vision21 {

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CSeparatorPlate::CSeparatorPlate()
{
	m_nNoNodes=0;
	m_lfC=1500.0;//joule/kg/K
	m_lfRho=1000.0;//kg/m3
	m_lfSeparatorPlateThickness=0.0;
	pCellRes=NULL;
	lfCurrentDensity=NULL;
}

CSeparatorPlate::~CSeparatorPlate()
{

}

int CSeparatorPlate::AnalyzeNextTimeStep(double lfTimeStep)
{
	//currently neglects axial conduction
	for(int i=1;i<m_nNoNodes-1;i++){
		double Qgen=GetQGenOfNode(i);
		double lfQFromLowerSepPltToUpperElectr=0.0;
		double lfQFromUpperSepPltToLowerElectr=0.0;
		if(m_bDoRadiation){
			lfQFromLowerSepPltToUpperElectr=g_lfStephanBoltz*(pow(m_plfLowerRadTemp[MAX_AXIALNODES*CUR+i],4.0)-pow(m_lfTemp[CUR][i],4.0))*DelX[i]*m_ChanGeom.lfChannelWidth*m_ChanGeom.lfNumberOfChannels;
			lfQFromUpperSepPltToLowerElectr=g_lfStephanBoltz*(pow(m_plfUpperRadTemp[MAX_AXIALNODES*CUR+i],4.0)-pow(m_lfTemp[CUR][i],4.0))*DelX[i]*m_ChanGeom.lfChannelWidth*m_ChanGeom.lfNumberOfChannels;
			}
		m_lfTemp[NXT][i]=m_lfTemp[CUR][i]+lfTimeStep/m_lfRho/m_lfC/XS_Area[i]/DelX[i]*(Qgen-m_plfQFromSeparatorToAnodeGas[i]-m_plfQFromSeparatorToCathodeGas[i]+lfQFromUpperSepPltToLowerElectr+lfQFromLowerSepPltToUpperElectr);
		}
	return(1);
}

double CSeparatorPlate::GetQGenOfNode(int i)
{
	if(lfCurrentDensity==NULL){
		return(0.0);
		}
	double SeparatorPlateRes=GetNodeResistance(i);

	double Q=0.0;
	double NodeVoltDrop=lfCurrentDensity[i]*SeparatorPlateRes;
	Q+=lfCurrentDensity[i]*NodeVoltDrop*DelX[i]*m_lfCellWidth;
	return(Q);//watts
}

double CSeparatorPlate::GetNodeResistance(int i)
{
	if(pCellRes==NULL){
		return(0.0);
		}
	//The 'cell' node heat is everything but due to the SeparatorPlate Ohmic Heating
	double SeparatorPlateElecRes=pCellRes->InterconectRes.lfConst+pCellRes->InterconectRes.lfLinear*m_lfTemp[CUR][i]+pCellRes->InterconectRes.lfPreExpon*exp(pCellRes->InterconectRes.lfExpon/m_lfTemp[CUR][i]);
	double SeparatorPlateThicknessCM=m_lfSeparatorPlateThickness*100.0;
	double SeparatorPlateRes=SeparatorPlateElecRes*SeparatorPlateThicknessCM;
	SeparatorPlateRes/=10000.0;// convert to ohm-meter2
	return(SeparatorPlateRes);
}

int CSeparatorPlate::Setup(int nNoNodes, double lfInitialTemp, const double * lfDelX, ChanGeom *pChanGeom, double lfCellWidth, const double * lfXS_Area, double lfC, double lfRho, bool bDoRadiation, double lfSeparatorPlateThickness, const struct CellResistance *CellResistance, const double *plfCurrentDensity)
{
	m_lfCellWidth=lfCellWidth;
	m_ChanGeom=*pChanGeom;
	m_nNoNodes=nNoNodes;
	m_lfRho=lfRho;
	m_lfC=lfC;
	DelX=lfDelX;
	XS_Area=lfXS_Area;
	m_bDoRadiation=bDoRadiation;
	m_lfSeparatorPlateThickness=lfSeparatorPlateThickness;
	pCellRes=CellResistance;
	lfCurrentDensity=plfCurrentDensity;
	if(lfInitialTemp>0.0){
		for(int i=0;i<m_nNoNodes;i++){
			m_lfTemp[CUR][i]=lfInitialTemp;
			m_lfTemp[NXT][i]=lfInitialTemp;
			}
		}
	return(1);
}

int CSeparatorPlate::ContinueSetup(const double * lfDelX, const double * lfXS_Area, bool bDoRadiation)
{
	DelX=lfDelX;
	XS_Area=lfXS_Area;
	m_bDoRadiation=bDoRadiation;
	for(int i=0;i<m_nNoNodes;i++){
		m_lfTemp[NXT][i]=m_lfTemp[CUR][i];
		}
	return(1);
}


void CSeparatorPlate::PrintSolidTemps(FILE *f)
{
	for(int i=1;i<m_nNoNodes-1;i++){
		fprintf(f,"%le,",m_lfTemp[CUR][i]);
		}
}

void CSeparatorPlate::PrintData(FILE * f, bool bDoHeader)
{
	int i;
	if(bDoHeader){
		for(i=1;i<m_nNoNodes-1;i+=PRINT_STEP){
			fprintf(f,"SepPlt T[K]%d,",i);
			}
		}
	else {
		for(i=1;i<m_nNoNodes-1;i+=PRINT_STEP){
			fprintf(f,"%le,",m_lfTemp[CUR][i]);
			}
		}
}

/*
void CSeparatorPlate::Serialize(CArchive & ar)
{
	int i;
	if (ar.IsStoring())	{
		ar<<(WORD)m_nNoNodes;
		for(i=0;i<m_nNoNodes;i++){
			ar<<m_lfTemp[CUR][i];
			}
		ar<<m_lfC;
		ar<<m_lfRho;
		}
	else{
		WORD w;
		ar>>w;	m_nNoNodes=(int)w;
		for(i=0;i<m_nNoNodes;i++){
			ar>>m_lfTemp[CUR][i];
			}
		ar>>m_lfC;
		ar>>m_lfRho;
		}
}

*/

double CSeparatorPlate::GetMinTimeStep()
{
	double minT=1.0E+10;
	int i;
	for(i=1;i<m_nNoNodes-1;i++){
		double lfAcceptableDelTempPerTimeStep=10.1;//K
		double lfQFromLowerSepPltToUpperElectr=0.0;
		double lfQFromUpperSepPltToLowerElectr=0.0;
		if(m_bDoRadiation){
			lfQFromLowerSepPltToUpperElectr=g_lfStephanBoltz*(pow(m_plfLowerRadTemp[MAX_AXIALNODES*CUR+i],4.0)-pow(m_lfTemp[CUR][i],4.0))*DelX[i]*m_ChanGeom.lfChannelWidth*m_ChanGeom.lfNumberOfChannels;
			lfQFromUpperSepPltToLowerElectr=g_lfStephanBoltz*(pow(m_plfUpperRadTemp[MAX_AXIALNODES*CUR+i],4.0)-pow(m_lfTemp[CUR][i],4.0))*DelX[i]*m_ChanGeom.lfChannelWidth*m_ChanGeom.lfNumberOfChannels;
			}
		double lfHtime=lfAcceptableDelTempPerTimeStep*m_lfRho*m_lfC*XS_Area[i]*DelX[i]/(-m_plfQFromSeparatorToAnodeGas[i]-m_plfQFromSeparatorToCathodeGas[i]-lfQFromUpperSepPltToLowerElectr-lfQFromLowerSepPltToUpperElectr);
		lfHtime=fabs(lfHtime);
		if(lfHtime<minT){
			minT=lfHtime;
			}
		}
	return(minT);
}

void CSeparatorPlate::SetUpperSurfaceHeatRemoval(const double * lfQFromSeparatorToCathodeGas, const double * lfRadiationTemp)
{
	m_plfQFromSeparatorToCathodeGas=lfQFromSeparatorToCathodeGas;
	m_plfUpperRadTemp=lfRadiationTemp;
}

void CSeparatorPlate::SetLowerSurfaceHeatRemoval(const double * lfQFromSeparatorToAnodeGas, const double * lfRadiationTemp)
{
	m_plfQFromSeparatorToAnodeGas=lfQFromSeparatorToAnodeGas;
	m_plfLowerRadTemp=lfRadiationTemp;
}

double CSeparatorPlate::AvePlateTemp()
{
	double ave=0.0;
	for(int i=1;i<m_nNoNodes-1;i++){
		ave+=m_lfTemp[CUR][i];
		}
	ave/=((double)(m_nNoNodes-2));
	return(ave);
}

} // end namespace Vision21
