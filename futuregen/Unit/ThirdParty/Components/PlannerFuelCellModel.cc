// PlannerFuelCellModel.cpp: implementation of the CPlanarFuelCellModel class.
//
//////////////////////////////////////////////////////////////////////

#include "globals.h"
#include "PlannerFuelCellModel.h"
#include "cmath"
#include <string>

namespace Vision21 {


//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

#define INITTEMP	700

CPlanarFuelCellModel::CPlanarFuelCellModel()
{
	AnodeGas=NULL;
	CathodeGas=NULL;
	Electrolyte=NULL;
	m_bDoManualLogDiffusionOverPotential=false;
	m_lfCathodeLogLimitingDiffusionCurrDen=4000.0;//amp/m2
	m_lfAnodeLogLimitingDiffusionCurrDen=400000.0;//amp/m2

	bProbSetup=false;
}

CPlanarFuelCellModel::~CPlanarFuelCellModel()
{

}

int CPlanarFuelCellModel::AnalyzeFuelCell(double lfTimeStep)
{
	// Main driver for calculating next time step.
	if(!bProbSetup || AnodeGas==NULL) {
		printf("Problem needs to be setup first.");
		return(0);
		}

	int ret=1;
	if(!m_bDoQuasiSteadyGasFlow){
		int zz=0;
			zz=AnodeGas->AnalyzeNextTimeStep(lfTimeStep);
			if(zz==-1) ret=-1;
			if(zz==-2) return(-2);
		zz=CathodeGas->AnalyzeNextTimeStep(lfTimeStep);
		if(zz==-1) ret=-1;
		if(zz==-2) return(-2);
		}

	if(!m_bDoQuasiSteadySolidPhase){
		Electrolyte->AnalyzeNextTimeStep(lfTimeStep, (const double *)AnodeGas->T, (const double *)CathodeGas->T);
		SepPlt_AirSide.AnalyzeNextTimeStep(lfTimeStep);
		}

	UINT sv=NXT;
	NXT=CUR;
	CUR=sv;
	AnodeGas->AnalyzePressures();
	AnodeGas->AnalyzeTemperatures();
	CathodeGas->AnalyzePressures();
	CathodeGas->AnalyzeTemperatures();

	return(ret);
}

int CPlanarFuelCellModel::AnalyzeNodalInterfacialHeatFluxes()
{
	// Heat flux between various control volumes will now be calculated...
	AnodeGas->AnalyzeSurfaceHeats();
	CathodeGas->AnalyzeSurfaceHeats();

	return(1);
}

int CPlanarFuelCellModel::SetupProblem()
{
	bProbSetup=false;
	double lfCellThickness=(m_lfElectrolyteThickness+m_lfAnodeThickness+m_lfCathodeThickness);
	AnGeom.lfNumberOfChannels=m_lfCellWidth/(AnGeom.lfChannelWidth+AnGeom.lfChannelSpacing);// I won't care right now if there are fractional channels...
	CaGeom.lfNumberOfChannels=m_lfCellWidth/(CaGeom.lfChannelWidth+CaGeom.lfChannelSpacing);

	for(int i=0;i<m_nNoNodes;i++){
		m_lfXS_AreaAnode[i]=AnGeom.lfHeight*AnGeom.lfChannelWidth*AnGeom.lfNumberOfChannels;// total flow area
		m_lfXS_AreaCathode[i]=CaGeom.lfHeight*CaGeom.lfChannelWidth*CaGeom.lfNumberOfChannels;// total flow area
		m_lfXS_AreaElectrolyte[i]=m_lfCellWidth*lfCellThickness;// solid body area
		m_lfXS_AreaSeparator[i]=m_lfCellWidth*m_lfSeparatorThickness+\
			CaGeom.lfHeight*CaGeom.lfChannelSpacing*CaGeom.lfNumberOfChannels+\
			AnGeom.lfHeight*AnGeom.lfChannelSpacing*AnGeom.lfNumberOfChannels;// solid body area
		m_lfDelX[i]=m_lfCellLength/((double)m_nNoNodes);
		}

	AnodeGas->Setup(m_nNoNodes,m_bDoAnodeFixedFlow,m_bDoQuasiSteadyGasFlow,false, false,\
		&AnGeom,m_lfCellWidth,m_lfXS_AreaAnode,m_lfDelX,m_lfAnodeVelInput,m_lfAnodePInput,m_lfAnodePExit,m_lfAnodeTInput,\
		m_nNoAnodeSp,m_lfAnodeSpInput,m_bDoManualLogDiffusionOverPotential,m_lfAnodeLogLimitingDiffusionCurrDen);
	CathodeGas->Setup(m_nNoNodes,m_bDoCathodeFixedFlow,m_bDoQuasiSteadyGasFlow,false,false,\
		&CaGeom,m_lfCellWidth,m_lfXS_AreaCathode,m_lfDelX,m_lfCathodeVelInput,m_lfCathodePInput,m_lfCathodePExit,\
		m_lfCathodeTInput,m_nNoCathodeSp,m_lfCathodeSpInput,m_bDoManualLogDiffusionOverPotential,\
		m_lfCathodeLogLimitingDiffusionCurrDen);

	Electrolyte->Setup((CAnodeGasModel *)AnodeGas, (CCathodeGasModel *)CathodeGas,\
		(CSeparatorPlate *)&SepPlt_AirSide, (CSeparatorPlate *)&SepPlt_FuelSide, m_nNoNodes,\
		m_lfCellInitialTemp,m_lfElectrolyteThickness,m_lfAnodeThickness,m_lfCathodeThickness,\
		m_lfLoadResistance,(const struct CellResistance *)&CellRes,\
		m_lfCathodeExgCurDensityBCoef,m_lfCathodeExgCurDensityMCoef,m_lfAnodeExgCurDensity,\
		m_lfAlphaCathodeBCoef,m_lfAlphaCathodeMCoef,m_lfAlphaAnode,
		(const double *)m_lfDelX, &CaGeom, m_lfCellWidth, (const double *)m_lfXS_AreaElectrolyte, m_lfCellC, \
		m_lfCellRho, m_bDoRadiation);

	SepPlt_AirSide.Setup(m_nNoNodes, m_lfSeparatorInitialTemp, (const double *)m_lfDelX, \
		&CaGeom, m_lfCellWidth, (const double *)m_lfXS_AreaSeparator, m_lfSeparatorC, m_lfSeparatorRho,\
		m_bDoRadiation, m_lfSeparatorThickness,(const struct CellResistance *)&CellRes, \
		(const double *)Electrolyte->lfCurrentDensity);


	bProbSetup=true;
	return(1);
}


int CPlanarFuelCellModel::SetupContinuationProblem()
{
	// Routine allows you to continue a problem that has been previously saved.
	// If run under the debugger, you can use the following code to change different
	// parameters to initiate step changes in operating conditions.
	bProbSetup=false;

	AnodeGas->SetBoundaryFlowsFromCurrent((const double *)Electrolyte->lfCurrentDensity);// this will make sure that all nodes (from 0 to the end) will have the surface flows specified at the get go...
	CathodeGas->SetBoundaryFlowsFromCurrent((const double *)Electrolyte->lfCurrentDensity);

	AnodeGas->ContinuationSetup(m_bDoAnodeFixedFlow, m_lfAnodeVelInput,m_bDoQuasiSteadyGasFlow,false, false,m_lfXS_AreaAnode,m_lfDelX);
	CathodeGas->ContinuationSetup(m_bDoCathodeFixedFlow,m_lfCathodeVelInput,m_bDoQuasiSteadyGasFlow,false,false,m_lfXS_AreaCathode,m_lfDelX);

	Electrolyte->ContinueSetup(m_lfLoadResistance,m_lfCathodeExgCurDensityBCoef,m_lfCathodeExgCurDensityMCoef,m_lfAnodeExgCurDensity,\
		(const double *)m_lfDelX, (const double *)m_lfXS_AreaElectrolyte,m_bDoRadiation);

// not needed here anymore since it is calc inside the main loop...	Electrolyte->InitializeElectrolyteData();

	SepPlt_AirSide.ContinueSetup((const double *)m_lfDelX,(const double *)m_lfXS_AreaSeparator,m_bDoRadiation);
	SepPlt_FuelSide.ContinueSetup((const double *)m_lfDelX,(const double *)m_lfXS_AreaSeparator,m_bDoRadiation);

	bProbSetup=true;
	return(1);
}


int CPlanarFuelCellModel::UpdateBoundaryValuesViaCurrentSpec(double lfTotalCurrent)
{

	double *pSpArray=m_lfAnodeSpInput;
	double *pP=&m_lfAnodePInput;
	double *pT=&m_lfAnodeTInput;
	AnodeGas->UpdateBoundaryValues(*pP,m_lfAnodePExit,*pT,m_nNoAnodeSp,pSpArray);
	CathodeGas->UpdateBoundaryValues(m_lfCathodePInput,m_lfCathodePExit,m_lfCathodeTInput,m_nNoCathodeSp,m_lfCathodeSpInput);

	if(m_bDoQuasiSteadySolidPhase && m_bDoQuasiSteadyGasFlow){
		if(lfTotalCurrent>0.0){
			if(GOOD_RESULT!=Electrolyte->AnalyzeCurrDenGivenCurrent_FullyQS(lfTotalCurrent,SepPlt_AirSide,m_bIfQuasiSteadySolidPhase_ThenDoTempUpdate,true)){
				return(-1);
				}
			}
		else {
			if(GOOD_RESULT!=Electrolyte->AnalyzeCurrDen_FullyQS(SepPlt_AirSide,m_bIfQuasiSteadySolidPhase_ThenDoTempUpdate)){
				return(-1);
				}
			}
		}
	else {
		if(m_bDoQuasiSteadyGasFlow){
			if(lfTotalCurrent>=0.0){
				if(GOOD_RESULT!=Electrolyte->AnalyzeCurrDenGivenCurrent_GasQS(lfTotalCurrent,false)){
					return(-1);
					}
				}
			else {
				if(GOOD_RESULT!=Electrolyte->AnalyzeCurrDen_GasQS()){
					return(-1);
					}
				}
			}
		else{
			if(lfTotalCurrent>0.0){
//				if(GOOD_RESULT!=Electrolyte->AnalyzeCurrDenGivenCurrent(lfTotalCurrent)){
//					return(-1);
//					}
				}
			else {
				if(GOOD_RESULT!=Electrolyte->AnalyzeCurrDen()){
					return(-1);
					}
				}
			AnodeGas->SetBoundaryFlowsFromCurrent((const double *)Electrolyte->lfCurrentDensity);
			CathodeGas->SetBoundaryFlowsFromCurrent((const double *)Electrolyte->lfCurrentDensity);
			}
		AnalyzeNodalInterfacialHeatFluxes();
		}
	return(1);
}

int CPlanarFuelCellModel::UpdateBoundaryValues(double lfCellVoltage, bool bDoGradualSolutionSearching)
{
	double *pSpArray=m_lfAnodeSpInput;
	double *pP=&m_lfAnodePInput;
	double *pT=&m_lfAnodeTInput;
	AnodeGas->UpdateBoundaryValues(*pP,m_lfAnodePExit,*pT,m_nNoAnodeSp,pSpArray);
	CathodeGas->UpdateBoundaryValues(m_lfCathodePInput,m_lfCathodePExit,m_lfCathodeTInput,m_nNoCathodeSp,m_lfCathodeSpInput);

	if(m_bDoQuasiSteadySolidPhase && m_bDoQuasiSteadyGasFlow){
		if(lfCellVoltage>0.0){
			if(GOOD_RESULT!=Electrolyte->AnalyzeCurrDenGivenVoltage_FullyQS(lfCellVoltage,SepPlt_AirSide,m_bIfQuasiSteadySolidPhase_ThenDoTempUpdate,bDoGradualSolutionSearching)){
				return(-1);
				}
			}
		else {
			if(GOOD_RESULT!=Electrolyte->AnalyzeCurrDen_FullyQS(SepPlt_AirSide,m_bIfQuasiSteadySolidPhase_ThenDoTempUpdate)){
				return(-1);
				}
			}
		}
	else {
		if(m_bDoQuasiSteadyGasFlow){
			if(lfCellVoltage>0.0){
				if(GOOD_RESULT!=Electrolyte->AnalyzeCurrDenGivenVoltage_GasQS(lfCellVoltage,bDoGradualSolutionSearching)){
					return(-1);
					}
				}
			else {
				if(GOOD_RESULT!=Electrolyte->AnalyzeCurrDen_GasQS()){
					return(-1);
					}
				}
			}
		else{
			if(lfCellVoltage>0.0){
				if(GOOD_RESULT!=Electrolyte->AnalyzeCurrDenGivenVoltage(lfCellVoltage)){
					return(-1);
					}
				}
			else {
				if(GOOD_RESULT!=Electrolyte->AnalyzeCurrDen()){
					return(-1);
					}
				}
			AnodeGas->SetBoundaryFlowsFromCurrent((const double *)Electrolyte->lfCurrentDensity);
			CathodeGas->SetBoundaryFlowsFromCurrent((const double *)Electrolyte->lfCurrentDensity);
			}
		AnalyzeNodalInterfacialHeatFluxes();
		}
	return(1);
}

void CPlanarFuelCellModel::PrintData(FILE *f, bool bDoHeader)
{
  //UINT uAnSpIDs[]={H2,CO,H2O,CH4,CO2};
  //UINT uCathSpIDs[]={O2,H2O,CO2};
	UINT uAnSpIDs[]={H2,CO,H2O,CO2,O2,N2,CH4};
	UINT uCathSpIDs[]={H2,CO,H2O,CO2,O2,N2,CH4};
	AnodeGas->PrintData(f,bDoHeader,"Anode", (UINT)(sizeof(uAnSpIDs)/sizeof(UINT)), uAnSpIDs);
	CathodeGas->PrintData(f,bDoHeader,"Cathode", (UINT)(sizeof(uCathSpIDs)/sizeof(UINT)),uCathSpIDs);
	Electrolyte->PrintData(f,bDoHeader);
	SepPlt_AirSide.PrintData(f,bDoHeader);
	SepPlt_FuelSide.PrintData(f,bDoHeader);
	if(bDoHeader){
		fprintf(f,"Anode Utilization,Cathode Utilization,");
		}
	else {
		fprintf(f,"%le,%le,",AnodeGas->GetUtilization(),CathodeGas->GetUtilization());
		}
	fprintf(f,"\n");
}

double CPlanarFuelCellModel::GetMinTimeStep()
{
	double minT=AnodeGas->GetMinTimeStep();
	minT=MIN(minT,CathodeGas->GetMinTimeStep());
	int nTst=0;
	if(!m_bDoQuasiSteadySolidPhase){
		double tm=Electrolyte->GetMinTimeStep(((const double *)AnodeGas->T), ((const double *)CathodeGas->T));
		if(tm<minT){
			nTst=1;
			minT=tm;
			}
		tm=SepPlt_AirSide.GetMinTimeStep();
		if(tm<minT){
			nTst=2;
			minT=tm;
			}
		}
//	if(nTst==1){
//		TRACE1("CellControl=%le\n",minT);
//		}
//	else if(nTst==2){
//		TRACE1("SepPltControl=%le\n",minT);
//		}
//	else if(nTst==3){
//		TRACE1("SepPltControl=%le\n",minT);
//		}
//	else{
//		TRACE1("GasCtrl=%le\n",minT);
//		}
	if(minT>10000000.0) minT=-1.0;// must be doing a QS problem
	if(minT>15) minT=15;//25.0;// don't go too fast....
	return(minT);
}

void CPlanarFuelCellModel::PrintConfig(FILE * f)
{
	fprintf(f,"\n%s\n\n",m_szDescription.c_str());
	fprintf(f,"Doing Const. Cp (1/0; 1=yes):,%d\n",G_SpDb.m_bDoConstCp);
	fprintf(f,"Doing Radiation (1/0; 1=yes):,%d\n",m_bDoRadiation);
	fprintf(f,"Doing QS Gas (1/0; 1=yes):,%d\n",m_bDoQuasiSteadyGasFlow);
	fprintf(f,"Doing QS Solid (1/0; 1=yes):,%d\n",m_bDoQuasiSteadySolidPhase);
	fprintf(f,"No. Axial Nodes=, %d\n",m_nNoNodes);
	fprintf(f,"Cell Axial Length[m]=, %le\n",m_lfCellLength);

	fprintf(f,"Anode Chan H[m]=, %le\n",AnGeom.lfHeight);
	fprintf(f,"Anode Chan W[m]=, %le\n",AnGeom.lfChannelWidth);
	fprintf(f,"Anode Chan Spacing[m]=, %le\n",AnGeom.lfChannelSpacing);
	fprintf(f,"Anode Inlet T[K]=, %le\n",m_lfAnodeTInput);
	fprintf(f,"Anode Inlet P[Pa]=, %le\n",m_lfAnodePInput);
	fprintf(f,"Anode Exit P[m]=, %le\n",m_lfAnodePExit);

	int i;
	for(i=0;i<m_nNoAnodeSp;i++){
		fprintf(f,"%s Anode Inlet Mole Fraction=, %le\n",SpecieNames[i],m_lfAnodeSpInput[i]);
		}
	fprintf(f,"Anode Exg.Cur.Den.[Amp/m2]=, %le\n",m_lfAnodeExgCurDensity);
	fprintf(f,"Anode Trans.Coef.[1]=, %le\n",m_lfAlphaAnode);

//	fprintf(f,"Cathode Gap[m]=, %le\n",m_lfCathodeHeight);
	fprintf(f,"Cathode Chan H[m]=, %le\n",CaGeom.lfHeight);
	fprintf(f,"Cathode Chan W[m]=, %le\n",CaGeom.lfChannelWidth);
	fprintf(f,"Cathode Chan Spacing[m]=, %le\n",CaGeom.lfChannelSpacing);
	fprintf(f,"Cathode Inlet T[K]=, %le\n",m_lfCathodeTInput);
	fprintf(f,"Cathode Inlet P[Pa]=, %le\n",m_lfCathodePInput);
	fprintf(f,"Cathode Exit P[m]=, %le\n",m_lfCathodePExit);
	for(i=0;i<m_nNoCathodeSp;i++){
		fprintf(f,"%s Cathode Inlet Mole Fraction=, %le\n",SpecieNames[i],m_lfCathodeSpInput[i]);
		}
	fprintf(f,"Cathode Exg.Cur.Den. 'b' coef.=, %le\n",m_lfCathodeExgCurDensityBCoef);
	fprintf(f,"Cathode Exg.Cur.Den. 'm' coef.=, %le\n",m_lfCathodeExgCurDensityMCoef);

	fprintf(f,"Cathode Trans.BCoef.[1]=, %le\n",m_lfAlphaCathodeBCoef);
	fprintf(f,"Cathode Trans.MCoef.[1]=, %le\n",m_lfAlphaCathodeMCoef);

	fprintf(f,"Electrolyte Thickness[m]=, %le\n",m_lfElectrolyteThickness);
	fprintf(f,"Anode Electrode Thickness[m]=, %le\n",m_lfAnodeThickness);
	fprintf(f,"Cathode Electrode Thickness[m]=, %le\n",m_lfCathodeThickness);
	fprintf(f,"Cell Heat Cap.[J/kg-K]=, %le\n",m_lfCellC);
	fprintf(f,"Cell Density[kg/m3]=, %le\n",m_lfCellRho);
	fprintf(f,"Load Resistance[Ohm]=, %le\n",m_lfLoadResistance);

	fprintf(f,"AirElectrode Res. PreExpon[ohm-cm]=, %le\n",CellRes.AirElectrodeRes.lfPreExpon);
	fprintf(f,"AirElectrode Res. Expon[K]=, %le\n",CellRes.AirElectrodeRes.lfExpon);

	fprintf(f,"Electrolyte Res. PreExpon[ohm-cm]=, %le\n",CellRes.ElectrolyteRes.lfPreExpon);
	fprintf(f,"Electrolyte Res. Expon[K]=, %le\n",CellRes.ElectrolyteRes.lfExpon);

	fprintf(f,"FuelElectrode Res. PreExpon[ohm-cm]=, %le\n",CellRes.FuelElectrodeRes.lfPreExpon);
	fprintf(f,"FuelElectrode Res. Expon[K]=, %le\n",CellRes.FuelElectrodeRes.lfExpon);

	fprintf(f,"Interconect Res. PreExpon[ohm-cm]=, %le\n",CellRes.InterconectRes.lfPreExpon);
	fprintf(f,"Interconect Res. Expon[K]=, %le\n",CellRes.InterconectRes.lfExpon);

	fprintf(f,"Separator Thickness[m]=, %le\n",m_lfSeparatorThickness);
	fprintf(f,"Separator Heat Cap.[J/kg-K]=, %le\n",m_lfSeparatorC);
	fprintf(f,"Separator Density[kg/m3]=, %le\n",m_lfSeparatorRho);

	fprintf(f,"\n\n");
}

/*
void CPlanarFuelCellModel::Serialize(CArchive & ar)
{
	int i;
	if (ar.IsStoring())
	{
		ar<<m_szDescription;
		ar<<bProbSetup;
		ar<<(WORD)m_nNoNodes;
		for(i=0;i<m_nNoNodes;i++){
			ar<<m_lfXS_AreaCathode[i];
			ar<<m_lfXS_AreaAnode[i];
			ar<<m_lfXS_AreaElectrolyte[i];
			ar<<m_lfXS_AreaSeparator[i];
			ar<<m_lfDelX[i];
			}
		ar<<m_lfCellLength;//1.0;
		ar<<AnGeom.lfHeight;//m
		ar<<AnGeom.lfChannelWidth;//m
		ar<<AnGeom.lfChannelSpacing;//m
		ar<<m_lfAnodeVelInput;//m/s
		ar<<m_lfAnodeTInput;//K
		ar<<m_lfAnodePInput;//Pa
		ar<<m_lfAnodePExit;
		ar<<(WORD)m_nNoAnodeSp;
		ar<<m_lfAnodeSpInput[H2];// mole fractions
		ar<<m_lfAnodeSpInput[CO];
		ar<<m_lfAnodeSpInput[H2O];
		ar<<m_lfAnodeSpInput[CO2];
		ar<<m_lfAnodeSpInput[N2];
		ar<<m_lfAnodeSpInput[O2];

		ar<<CaGeom.lfHeight;//m
		ar<<CaGeom.lfChannelWidth;//m
		ar<<CaGeom.lfChannelSpacing;//m
		ar<<m_lfCathodeVelInput;
		ar<<m_lfCathodeTInput;//K
		ar<<m_lfCathodePInput;//Pa
		ar<<m_lfCathodePExit;
		ar<<(WORD)m_nNoCathodeSp;
		ar<<m_lfCathodeSpInput[H2];
		ar<<m_lfCathodeSpInput[CO];
		ar<<m_lfCathodeSpInput[H2O];
		ar<<m_lfCathodeSpInput[CO2];
		ar<<m_lfCathodeSpInput[N2];
		ar<<m_lfCathodeSpInput[O2];

		ar<<m_lfCellInitialTemp;
		ar<<m_lfElectrolyteThickness;//m
		ar<<m_lfAnodeThickness;//m
		ar<<m_lfCathodeThickness;//m
		ar<<m_lfCellC;//2000.; //joule/kg/K
		ar<<m_lfCellRho;//kg/m3
		ar<<0.0;
		ar<<0.0;
		ar<<m_lfLoadResistance;

		ar<<CellRes.AirElectrodeRes.lfPreExpon;//ohm-cm
		ar<<CellRes.AirElectrodeRes.lfExpon;//Kelvin
		ar<<CellRes.ElectrolyteRes.lfPreExpon;
		ar<<CellRes.ElectrolyteRes.lfExpon;
		ar<<CellRes.FuelElectrodeRes.lfPreExpon;
		ar<<CellRes.FuelElectrodeRes.lfExpon;
		ar<<CellRes.InterconectRes.lfPreExpon;
		ar<<CellRes.InterconectRes.lfExpon;

		ar<<m_lfCathodeExgCurDensityBCoef;
		ar<<m_lfCathodeExgCurDensityMCoef;
		ar<<m_lfAnodeExgCurDensity;
		ar<<m_lfAlphaCathodeBCoef;
		ar<<m_lfAlphaCathodeMCoef;
		ar<<m_lfAlphaAnode;

		ar<<m_lfSeparatorInitialTemp;
		ar<<m_lfSeparatorThickness;//m
		ar<<m_lfSeparatorC;//1500.;//joule/kg/K
		ar<<m_lfSeparatorRho;//1200.;//kg/m3

		AnodeGas->Serialize(ar);
		CathodeGas->Serialize(ar);
		Electrolyte->Serialize(ar);
		SepPlt_AirSide.Serialize(ar);
		SepPlt_FuelSide.Serialize(ar);
		G_SpDb.Serialize(ar);
		}
	else {
		WORD w;
		double dd;
		ar>>m_szDescription;
		ar>>bProbSetup;
		ar>>w;	m_nNoNodes=(int)w;
		for(i=0;i<m_nNoNodes;i++){
			ar>>m_lfXS_AreaCathode[i];
			ar>>m_lfXS_AreaAnode[i];
			ar>>m_lfXS_AreaElectrolyte[i];
			ar>>m_lfXS_AreaSeparator[i];
			ar>>m_lfDelX[i];
			}
		ar>>m_lfCellLength;//1.0;
		ar>>AnGeom.lfHeight;//m
		ar>>AnGeom.lfChannelWidth;//m
		ar>>AnGeom.lfChannelSpacing;//m
		ar>>m_lfAnodeVelInput;//m/s
		ar>>m_lfAnodeTInput;//K
		ar>>m_lfAnodePInput;//Pa
		ar>>m_lfAnodePExit;
		ar>>w;	m_nNoAnodeSp=(int)w;
		ar>>m_lfAnodeSpInput[H2];// mole fractions
		ar>>m_lfAnodeSpInput[CO];
		ar>>m_lfAnodeSpInput[H2O];
		ar>>m_lfAnodeSpInput[CO2];
		ar>>m_lfAnodeSpInput[N2];
		ar>>m_lfAnodeSpInput[O2];

		ar>>CaGeom.lfHeight;//m
		ar>>CaGeom.lfChannelWidth;//m
		ar>>CaGeom.lfChannelSpacing;//m
		ar>>m_lfCathodeVelInput;
		ar>>m_lfCathodeTInput;//K
		ar>>m_lfCathodePInput;//Pa
		ar>>m_lfCathodePExit;
		ar>>w;	m_nNoCathodeSp=(int)w;
		ar>>m_lfCathodeSpInput[H2];
		ar>>m_lfCathodeSpInput[CO];
		ar>>m_lfCathodeSpInput[H2O];
		ar>>m_lfCathodeSpInput[CO2];
		ar>>m_lfCathodeSpInput[N2];
		ar>>m_lfCathodeSpInput[O2];

		ar>>m_lfCellInitialTemp;
		ar>>m_lfElectrolyteThickness;//m
		ar>>m_lfAnodeThickness;//m
		ar>>m_lfCathodeThickness;//m
		ar>>m_lfCellC;//2000.; //joule/kg/K
		ar>>m_lfCellRho;//kg/m3
		ar>>dd;
		ar>>dd;
		ar>>m_lfLoadResistance;

		ar>>CellRes.AirElectrodeRes.lfPreExpon;//ohm-cm
		ar>>CellRes.AirElectrodeRes.lfExpon;//Kelvin
		ar>>CellRes.ElectrolyteRes.lfPreExpon;
		ar>>CellRes.ElectrolyteRes.lfExpon;
		ar>>CellRes.FuelElectrodeRes.lfPreExpon;
		ar>>CellRes.FuelElectrodeRes.lfExpon;
		ar>>CellRes.InterconectRes.lfPreExpon;
		ar>>CellRes.InterconectRes.lfExpon;

		ar>>m_lfCathodeExgCurDensityBCoef;
		ar>>m_lfCathodeExgCurDensityMCoef;
		ar>>m_lfAnodeExgCurDensity;
		ar>>m_lfAlphaCathodeBCoef;
		ar>>m_lfAlphaCathodeMCoef;
		ar>>m_lfAlphaAnode;

		ar>>m_lfSeparatorInitialTemp;
		ar>>m_lfSeparatorThickness;//m
		ar>>m_lfSeparatorC;//1500.;//joule/kg/K
		ar>>m_lfSeparatorRho;//1200.;//kg/m3

		AnodeGas->Serialize(ar);
		CathodeGas->Serialize(ar);
		Electrolyte->Serialize(ar);
		SepPlt_AirSide.Serialize(ar);
		SepPlt_FuelSide.Serialize(ar);
		G_SpDb.Serialize(ar);
		}
}

*/

bool g_bDoDump=false;

void CPlanarFuelCellModel::DoVICurve(FILE *f, FILE *fData)
{
	std::string SaveDescription=m_szDescription;

	SetupProblem();
	m_bDoQuasiSteadySolidPhase=true;
	m_bDoQuasiSteadyGasFlow=true;
	m_bDoRadiation=false;
	m_bDoAnodeFixedFlow=true;
	m_bDoCathodeFixedFlow=true;

	double Savem_lfAnodeVelInput=m_lfAnodeVelInput;

	m_szDescription="Doing Base Case VI curves--floating Tcell.";

	for(int jj=0;jj<6;jj++){
//		double SaveAnodPInlet=101000.0+200.0/(2.+(double)jj);//Pa
//		double SaveAnodPExit=101000.0;
//		m_lfAnodePExit=SaveAnodPExit+(SaveAnodPExit-SaveAnodPInlet)/m_lfCellLength*(m_lfCellLength/((double)(m_nNoNodes-2)))*0.5;
//		m_lfAnodePInput=SaveAnodPExit+(SaveAnodPExit-SaveAnodPInlet)/m_lfCellLength*(-m_lfCellLength/((double)(m_nNoNodes-2)))*((double)m_nNoNodes-2.0+0.5);

		m_lfAnodeVelInput=Savem_lfAnodeVelInput*(2.0-(double)jj/4.0);

		double lfDelVolt=0.05;
		double lfStartVolt=0.90;
		double lfCellVoltage=lfStartVolt;
		lfCellVoltage+=lfDelVolt;
		fprintf(f,"\n\n\nStarting New VI Curve----\n");
		PrintConfig(f);
		fprintf(f,"Case #,GetTotalCurrent[A],Cell Voltage [V],AnodeGas Utilization[1],CathodeGas Utilization[1],Hydrogen Flow Rate [kg-mole/sec],Oxygen Flow Rate [kg-mole/sec]\n");
		PrintConfig(fData);
		SetupContinuationProblem();
		fprintf(fData,"Case #,Time[sec],");	PrintData(fData,true);
		fprintf(fData,"%d,%le,",jj,0.0);	PrintData(fData,false);
		double oldDelTime=10000000.0;

		while(lfCellVoltage>0.3){
			lfCellVoltage-=lfDelVolt;// may need to go with small steps (0.01) to make sure you get SS results

			double lfDelTime=0.00000001;
			double lfTotTime=0.0;
			int nCnt=0;
			double lfTime=0.0;
			int nGetTime=4;

			double OldVArrary[50];
			double OldDelTimes[50];
			for(int n=0;n<50;n++){
				OldVArrary[n]=0.0;
				OldDelTimes[n]=0.0;
				}
			int nStatus=0;
			double lfAveTestVarb=0.0;
			bool bDoingFullyQS=m_bDoQuasiSteadySolidPhase && m_bDoQuasiSteadyGasFlow;
			double lfConvValue=10.0;
			double lfCurrentFactor=0.0;// slowly load up the current
			m_bIfQuasiSteadySolidPhase_ThenDoTempUpdate=false;//= (jj==0) && ((lfCellVoltage+0.00000001)>=lfStartVolt);
			while(1){
				if((nStatus=UpdateBoundaryValues(lfCellVoltage,true))==-1){
					// must not be able to do this voltage (May be bigger than ENurnst!!)
					printf("Terminating the calculation as we can't treat requested voltage.\n");
					break;
					}
				nGetTime++;
				if(nGetTime>3){
					lfDelTime=GetMinTimeStep();///3.0;
					if(lfDelTime<0.0) {
						bDoingFullyQS=true;
						}
					else {
						bDoingFullyQS=false;
						}
					if(lfDelTime/oldDelTime>1.05){
						lfDelTime=1.05*oldDelTime;
						}
					oldDelTime=lfDelTime;
					nGetTime=0;
					}
				if(!bDoingFullyQS){
					lfCurrentFactor=1.0;
					lfTime+=lfDelTime;
					lfTotTime+=lfDelTime;
					OldDelTimes[nCnt]=lfDelTime;
					}
				if(AnalyzeFuelCell(lfDelTime)==-1) {
					printf("Reanalyze the current flow cuz apparently there was too much...\n");//Skipping to next voltage case\n");
					}
				nCnt++;
				if(nCnt>40){
					nCnt=0;
					printf("%le, ",lfTime);
					printf("%le; \n",lfDelTime);
					fprintf(fData,"%d,%le,",jj,lfTime);	PrintData(fData,false);
					}
				//then we are doing a fully QS calculation, so test for convergence
				if(lfConvValue<CURRENT_PRECISION*1.1) break;// put this before evaluation so that we know we'll go at least two times through so CUR and NXT varbs will all get updated
				lfConvValue=QS_Converged();
				if(lfCurrentFactor<1.0){
					lfCurrentFactor+=0.1;
					}
				else {
					printf("CF=1.0,");
					}
				if(lfCurrentFactor>1.0) lfCurrentFactor=1.0;
				printf("%d:V=%le, QS Conv. factor=%le\n",jj,lfCellVoltage,lfConvValue);
				}
			if(nStatus==-1) {
				continue;
				}
			if(nCnt!=-1){
				double Vnew=Electrolyte->GetLoadVoltage();
				double FuelFlow=AnodeGas->GetFuelFlowRate();
				double OxygenFlow=CathodeGas->GetOxygenFlowRate();
				fprintf(f,"%d,%le,%le,%le,%le,%le,%le\n",jj,Electrolyte->GetTotalCurrent(),Vnew,AnodeGas->GetUtilization(),CathodeGas->GetUtilization(),FuelFlow,OxygenFlow);
				fprintf(fData,"%d,%le,",jj,lfTime);	PrintData(fData,false);
				}
			}
		}
	m_szDescription=SaveDescription;
}

double CPlanarFuelCellModel::AnalyzeOverallPerformance()
{
//  From SOFCo Presentation...  Only H2 and O2 chemistry is considered!
	double lfMaxFuelUtilization=0.95;//1.0;// this is how far out you want to integrate [values 0 to 1.0]
	FILE *f=fopen("OverallPerformance.dat","w");
	if(f==NULL){
		return(-1.0);
		}

	m_lfAnodeSpInput[H2]=1.14;//set it high first cuz we subtract as we loop...
	double lfOverallEffMax=0.0;
	for(int i=0;i<4;i++){
		m_lfAnodeSpInput[H2]-=0.15;
		m_lfAnodeSpInput[H2O]=1.0-m_lfAnodeSpInput[H2];
		m_lfCathodeSpInput[O2]=0.21;
		Electrolyte->m_lfTemp[CUR][0]=273.0+1000.0;//850.0;//(m_lfAnodeTInput+m_lfCathodeTInput)/2.0;
		m_lfCathodePInput=101000.0;
		m_lfAnodePInput=m_lfCathodePInput;
		// the next 4 variables define the proportion of H2 to O2 for the problem....
		AnGeom.lfHeight=0.001;
		CaGeom.lfHeight=0.001;
//		m_lfCathodeHeight=1.0;// by supplying alot more O2, we won't be limited by O2 concentration!
		AnodeGas->Vel[CUR][0]=1.0;
		CathodeGas->Vel[CUR][0]=1.0;

		lfOverallEffMax=AnalyzeOverallPerformanceFollowingDetailedAnalysis(f,lfMaxFuelUtilization);
		}
	fclose(f);
	return(lfOverallEffMax);
}

double CPlanarFuelCellModel::GetDelG(const double lfH2Xi, const double lfO2Xi, const double lfH2OXi, const double Ti, const double AnodeP, const double CathodeP)
{
	double MolarDelHStd=hSp(H2O,Ti)*G_SpDb.Specie[H2O].lfTDData[MW]-0.5*hSp(O2,Ti)*G_SpDb.Specie[O2].lfTDData[MW]-hSp(H2,Ti)*G_SpDb.Specie[H2].lfTDData[MW];//joule/kg-mole
	double MolarDelSStd=Electrolyte->GetReactionDeltaEntropy(Ti,Ti,G_SpDb.REFERENCE_P,G_SpDb.REFERENCE_P);// per kg-mole of H2! without effects of mixing and at std. conditions!!
	double MolarDelGStd=MolarDelHStd-Ti*MolarDelSStd;//delG per kg-mole of H2
	MolarDelGStd/=1000.0;//delG per g-mole of H2
	double Eo=MolarDelGStd/2.0/FARADAY;// this voltage was confirmed to be correct as compared to TD data from VanWylan and Sontag--see MathCAD document "Cell Voltage Calculations.mcd"!!  It also compares to Fuel Cell Handbook Table 2-3 for vapor H2O!

	double AnSpH2=lfH2Xi;
	double CaSpO2=lfO2Xi;
	double AnSpH2O=lfH2OXi;
	double ReactantActivity=AnSpH2*AnodeP/G_SpDb.REFERENCE_P*sqrt(CaSpO2*CathodeP/G_SpDb.REFERENCE_P);//note: need to put pressure in terms of atmospheres--see Hossein Ghezel-Ayagh paper from ERC
	double ProductActivity=AnSpH2O*AnodeP/G_SpDb.REFERENCE_P+0.0000001;//add a small extra cuz we don't want to divide by zero!

	double MolarDelG=MolarDelGStd-Runiv*Ti*log(ReactantActivity/ProductActivity);
	return(MolarDelG);//delG joule per g-mole of H2
}

double CPlanarFuelCellModel::GetStdGibbs(double Temp)
{
	double StdGibb=GetDelG(1., 1., 1., Temp, G_SpDb.REFERENCE_P, G_SpDb.REFERENCE_P);
	return(StdGibb);
}

void CPlanarFuelCellModel::DoubleAnodePressDrop_Vel()
{
	AnodeGas->DoublePressDrop_Vel();	
	m_lfAnodePInput=m_lfAnodePExit+2.0*(m_lfAnodePInput-m_lfAnodePExit);
}

double CPlanarFuelCellModel::AnalyzeOverallPerformanceFollowingDetailedAnalysis(FILE *f,double lfMaxFuelUtilization)
{
	// this may need to be reviewed since I changed the defintion of channels and height variables for the gas paths....
		fprintf(f,"\nLet's calculate the ideal performace given a Fuel Utilization...\n");
		double AnSpH2Xi=m_lfAnodeSpInput[H2];//use inlet specie conditions
		double CaSpO2Xi=m_lfCathodeSpInput[O2];
		double AnSpH2OXi=m_lfAnodeSpInput[H2O];
		double Temp=Electrolyte->m_lfTemp[CUR][1];//we assume fixed temp for this analysis

		double dU=0.001;
		double AnP=m_lfAnodePInput;
		double CaP=m_lfCathodePInput;
		double TotAnConc=AnP/Runiv/Temp;
		double TotCaConc=CaP/Runiv/Temp;
		double AnVelInput=AnodeGas->Vel[CUR][0];
		double CaVelInput=CathodeGas->Vel[CUR][0];
		double MolarDelHStdLHV=hSp(H2O,Temp)*G_SpDb.Specie[H2O].lfTDData[MW]-0.5*hSp(O2,Temp)*G_SpDb.Specie[O2].lfTDData[MW]-hSp(H2,Temp)*G_SpDb.Specie[H2].lfTDData[MW];//joule/kg-mole
		MolarDelHStdLHV/=1000.0;//delH per g-mole of H2
		double lfEffInletConds=GetDelG(AnSpH2Xi, CaSpO2Xi, AnSpH2OXi, Temp, AnP, CaP);
		lfEffInletConds/=MolarDelHStdLHV;

		double lfOverallEffMax=0.0;
		double lfUtilizationAveraged_Voltage=0.0;
		double lfMaxVolt=0.0;
		double lfMinVolt=10.0;
		double lfU=0.0;

		double lfO2StoichWeight=CaSpO2Xi/AnSpH2Xi*AnGeom.lfHeight*AnVelInput/CaGeom.lfHeight/CaVelInput;
		//Need to weight by geometry cuz one side may have more volume flow of material than the other!
		//This use of lfO2StoichWeight assumes that the geometry AND VELOCITY remain constant
		//along the length of the fuel cell, such that at each dU the associated changes in H2 and O2
		//concentrations remain in-sync with eachother!!!!  Otherwise, for example, you could have
		//a change in H2 (for a given dU) occur only over a small dx, whereas the O2 change
		//may occur over a BIG dx.  May want to change this as velocity in general
		//may not be constant, especially for the cathode as O2 is removed from the flow, while
		//for the anode, H2 is simply replaced by H2O.  FOR NOW, WE'LL LET THIS BE PART OF THE
		//OVERALL ASSUMPTIONS FOR THIS ANALYSIS, AND IT WILL BE A POINT OF DISCUSSION WHEN WE COMPARE
		//THIS RESULT TO ANY OTHER ANALYSIS TECHNIQUE, ALONG WITH THE FACT THAT WE ASSUME CONST. TEMP AND PRESS.
		fprintf(f,"Input Data:\nOverall H2 Utilization=,%le\n",lfMaxFuelUtilization);
		fprintf(f,"Supply H2 Molefraction=,%le\n",AnSpH2Xi);
		fprintf(f,"Supply O2 Molefraction=,%le\n",CaSpO2Xi);
		fprintf(f,"Uniform Temp. Value=,%le\n",Temp);
		fprintf(f,"Uniform Press. Value=,%le\n",CaP);
		fprintf(f,"Excess O2[1]=,%le\n",(m_lfCathodeSpInput[O2]*TotCaConc-m_lfAnodeSpInput[H2]*TotAnConc/2.0*lfO2StoichWeight)/(m_lfAnodeSpInput[H2]*TotAnConc*lfO2StoichWeight));
		fprintf(f,"delH_Reaction [J/g-mole H2]=,%le\n",MolarDelHStdLHV);
		fprintf(f,"\nOutput:\nInlet H2 Mole Fraction[1],H2Xi[1],H2OXi[1],O2Xi[1],Utilization, Local delG[J/gm-mole H2], Local Voltage[volt], Ideal Overall Efficiency, Ideal Ave. Voltage [volt]\n");
		while(1){
			double lfDelGLeft=GetDelG(AnSpH2Xi, CaSpO2Xi, AnSpH2OXi, Temp, AnP, CaP);
			lfU+=dU;
			if(lfU>lfMaxFuelUtilization){
				dU=(lfU-lfMaxFuelUtilization)/2.0;
				}
			double H2AnConc=m_lfAnodeSpInput[H2]*TotAnConc*(1.0-lfU);
			double H2OAnConc=m_lfAnodeSpInput[H2O]*TotAnConc+m_lfAnodeSpInput[H2]*TotAnConc*(lfU);
			double O2CaConc=m_lfCathodeSpInput[O2]*TotCaConc-m_lfAnodeSpInput[H2]*TotAnConc*(lfU)/2.0*lfO2StoichWeight;
			if(H2AnConc<0.0 || O2CaConc<0.0) break;
			AnSpH2Xi=H2AnConc/TotAnConc;
			AnSpH2OXi=H2OAnConc/TotAnConc;
			CaSpO2Xi=O2CaConc/TotCaConc;
			double lfDelGRight=GetDelG(AnSpH2Xi, CaSpO2Xi, AnSpH2OXi, Temp, AnP, CaP);
			double lfDelEff=(lfDelGLeft+lfDelGRight)/2.0*dU/MolarDelHStdLHV;//trapezoidal rule
			double lfDelVolt=-(lfDelGLeft+lfDelGRight)/2.0*dU/2.0/FARADAY;//trapezoidal rule
			lfMinVolt=MIN(lfMinVolt,lfDelVolt/dU);
			lfMaxVolt=MAX(lfMaxVolt,lfDelVolt/dU);
			lfUtilizationAveraged_Voltage+=lfDelVolt;
			lfOverallEffMax+=lfDelEff;
			if(lfU>=lfMaxFuelUtilization){
				break;
				}
			fprintf(f,"%le, %le, %le, %le, %le, %le,",m_lfAnodeSpInput[H2],AnSpH2Xi,AnSpH2OXi,CaSpO2Xi,lfU,lfDelGLeft);
			fprintf(f,"%le, %le, %le\n",lfDelVolt/dU,lfOverallEffMax/lfU,lfUtilizationAveraged_Voltage/lfU);
			}

		lfUtilizationAveraged_Voltage/=lfMaxFuelUtilization;
		lfOverallEffMax/=lfMaxFuelUtilization;

		double lfStdVoltage=-GetStdGibbs(Temp)/2./FARADAY;//get standard voltage (ambient press. & at given temp.)

		fprintf(f,"\nOverall Ideal Performance Data:\nMax. Std. Voltage=,%le\n",lfStdVoltage);
		fprintf(f,"Max. Nernst Voltage=,%le\n",lfMaxVolt);
		fprintf(f,"Min. Nernst Voltage=,%le\n",lfMinVolt);
		fprintf(f,"Ideal (Util. Averaged) Voltage=,%le\n",lfUtilizationAveraged_Voltage);
		fprintf(f,"Ideal (Util. Averaged) Thermal Eff. (LHV)=,%le\n",lfOverallEffMax);// I've compared this reslt to that by SOFCo Presentation and they look about the same.  I'm about 1% lower than their value, which could be due to integration
		fprintf(f,"Ideal (Util. Averaged) Thermal Eff. (LHV) (with accounting for unconverted fuel)=,%le\n",lfOverallEffMax*lfMaxFuelUtilization);// I've compared this reslt to that by SOFCo Presentation and they look about the same.  I'm about 1% lower than their value, which could be due to integration
		fprintf(f,"Ideal Thermal Eff. (based on inlet voltage)=,%le\n",lfEffInletConds);
		fprintf(f,"Ideal Operational Eff. (based on exit voltage with accounting for unconverted fuel)=,%le\n\n\n",-lfMinVolt*lfMaxFuelUtilization/MolarDelHStdLHV*2.*FARADAY);//I compared this variable with SOFCo too and they are very close!
		return(lfOverallEffMax);
}

double CPlanarFuelCellModel::QS_Converged()
{
	static double OldAnT=10000000.0;
	static double OldAnP=10000000.0;
	static double OldCaT=10000000.0;
	static double OldCaP=10000000.0;
	static double OldCellT=10000000.0;
	static double OldSepPltT=10000000.0;
	static double OldCurDen=10000000.0;
	static double OldCurDen2=10000000.0;
	int nNode=3;
	if(m_nNoNodes<4) nNode=1;

	double AnTtest=AnodeGas->T[nNode];
	double AnPtest=AnodeGas->P[nNode];

	double CaTtest=CathodeGas->T[nNode];
	double CaPtest=CathodeGas->P[nNode];

	double CellTtest=Electrolyte->m_lfTemp[CUR][nNode];
	double SepPltTtest=SepPlt_AirSide.m_lfTemp[CUR][nNode];
	double CurrDentest=Electrolyte->lfCurrentDensity[1];
	double CurrDentest2=Electrolyte->lfCurrentDensity[m_nNoNodes-2];

	double diff=fabs((AnTtest-OldAnT)/AnTtest);
	diff+=fabs((AnPtest-OldAnP)/AnPtest);
	diff+=fabs((CaTtest-OldCaT)/CaTtest);
	diff+=fabs((CaPtest-OldCaP)/CaPtest);
	diff+=fabs((CellTtest-OldCellT)/CellTtest);
	diff+=fabs((SepPltTtest-OldSepPltT)/SepPltTtest);
	diff+=fabs((CurrDentest-OldCurDen)/(CurrDentest+1.0E-30));
	diff+=fabs((CurrDentest2-OldCurDen2)/(CurrDentest2+1.0E-30));

	OldAnT=AnTtest;
	OldAnP=AnPtest;
	OldCaT=CaTtest;
	OldCaP=CaPtest;
	OldCellT=CellTtest;
	OldSepPltT=SepPltTtest;
	OldCurDen=CurrDentest;
	OldCurDen2=CurrDentest2;
	return(diff);
}

void CPlanarFuelCellModel::TestArray(double *Array)
{
	double *pArr;
	pArr=Array;
	pArr[MAX_AXIALNODES*0+10]=10.0;
	pArr[MAX_AXIALNODES*1+10]=10.1;
	pArr[MAX_AXIALNODES*0+0]=-1.0;
	pArr[MAX_AXIALNODES*1+1]=1.1;
}

void CPlanarFuelCellModel::JumpstartVelProfiles(double PAnE, double PAnL, double PCaE, double PCaL)
{
	//keep for ProTrax Library
	double lfNewAnPGradFactor=(PAnE-PAnL)/(AnodeGas->P[0]-AnodeGas->P[m_nNoNodes-1]);
	double lfNewCaPGradFactor=(PCaE-PCaL)/(CathodeGas->P[0]-CathodeGas->P[m_nNoNodes-1]);

	AnodeGas->VelMultiplyFactor(lfNewAnPGradFactor);
	CathodeGas->VelMultiplyFactor(lfNewCaPGradFactor);
}

void CPlanarFuelCellModel::PrintSolidTemps(FILE *f)
{
	//keep for ProTrax Library
	fprintf(f,"AirSidePlate:,");
	SepPlt_AirSide.PrintSolidTemps(f);

	fprintf(f,"Cell:,");
	Electrolyte->PrintSolidTemps(f);

	fprintf(f,"\n");
}

} // end namespace Vision21
