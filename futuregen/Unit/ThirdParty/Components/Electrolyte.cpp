// Electrolyte.cpp: implementation of the CElectrolyte class.
//
//////////////////////////////////////////////////////////////////////

#include "globals.h"
#include "Electrolyte.h"
#include "AnodeGasModel.h"
#include "CathodeGasModel.h"
#include "cmath"
#include "cfloat"


namespace Vision21 {

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CElectrolyte::CElectrolyte()
{
	m_lfSumCathActivLoss=0.0;
	m_lfSumAnodActivLoss=0.0;
	m_lfSumCathConcLoss=0.0;
	m_lfSumAnodConcLoss=0.0;
	m_lfSumOhmicLoss=0.0;
	m_lfCellWidth=-1.0;
}

CElectrolyte::~CElectrolyte()
{
	
}

int CElectrolyte::AnalyzeNextTimeStep(double lfTimeStep, const double * AnodeT, const double * CathodeT)
{
  int i;
	//currently neglects axial conduction
	for(i=1;i<m_nNoNodes-1;i++){
		double Qgen=GetQGenOfCellNode(i);
		double Hin=An->GetNetEnthalpyInputToCell(i, (const double) m_lfTemp[CUR][i],AnodeT[i])+Ca->GetNetEnthalpyInputToCell(i, (const double) m_lfTemp[CUR][i],CathodeT[i]);
		double lfQFromLowerSepPltToUpperElectr=0.0;
		double lfQFromUpperSepPltToLowerElectr=0.0;
		if(m_bDoRadiation){
			lfQFromLowerSepPltToUpperElectr=g_lfStephanBoltz*(pow(m_plfLowerRadTemp[MAX_AXIALNODES*CUR+i],4.0)-pow(m_lfTemp[CUR][i],4.0))*DelX[i]*m_ChanGeom.lfChannelWidth*m_ChanGeom.lfNumberOfChannels;
			lfQFromUpperSepPltToLowerElectr=g_lfStephanBoltz*(pow(m_plfUpperRadTemp[MAX_AXIALNODES*CUR+i],4.0)-pow(m_lfTemp[CUR][i],4.0))*DelX[i]*m_ChanGeom.lfChannelWidth*m_ChanGeom.lfNumberOfChannels;
		}
		m_lfTemp[NXT][i]=m_lfTemp[CUR][i]+lfTimeStep/m_lfRho/m_lfC/XS_Area[i]/DelX[i]*(Hin+Qgen-m_plfQFromElectrToAnodeGas[i]-m_plfQFromElectrToCathodeGas[i]+lfQFromLowerSepPltToUpperElectr+lfQFromUpperSepPltToLowerElectr);
	}
	return(1);
}

double CElectrolyte::GetTotalSepPltResistance(int i)
{
	double SeparatorPlateRes=0.0;
	SeparatorPlateRes+=AirSepPlt->GetNodeResistance(i);//ohm-m2	-- this function is a 'fail-safe' call
	SeparatorPlateRes+=FuelSepPlt->GetNodeResistance(i);//ohm-m2-- this function is a 'fail-safe' call
	return(SeparatorPlateRes);
}

double CElectrolyte::GetQGenOfCellNode(int i)
{
	// this can only be called after the cell current and voltage are analyzed
	double SeparatorPlateRes=GetTotalSepPltResistance(i);//ohm-m2
	double Q=0.0;
	double CellVoltDrop=GetOverallNodeVoltageDrop(i)-lfCurrentDensity[i]*SeparatorPlateRes;
	Q+=lfCurrentDensity[i]*CellVoltDrop*DelX[i]*m_lfCellWidth;
	Q+=lfEntropyHeatGen[i];
	return(Q);//joule/sec
}
double CElectrolyte::GetCellNodeResistance(int i, double T)
{
	double AirElecRes=pCellRes->AirElectrodeRes.lfConst+pCellRes->AirElectrodeRes.lfLinear*T+pCellRes->AirElectrodeRes.lfPreExpon*exp(pCellRes->AirElectrodeRes.lfExpon/T);
	double FuelElecRes=pCellRes->FuelElectrodeRes.lfConst+pCellRes->FuelElectrodeRes.lfLinear*T+pCellRes->FuelElectrodeRes.lfPreExpon*exp(pCellRes->FuelElectrodeRes.lfExpon/T);
	double ElectrolyteElecRes=pCellRes->ElectrolyteRes.lfConst+pCellRes->ElectrolyteRes.lfLinear*T+pCellRes->ElectrolyteRes.lfPreExpon*exp(pCellRes->ElectrolyteRes.lfExpon/T);
	
	//Assuming a planar SOFC...
	double AirElectrodeThicknessCM=m_lfCathodeThickness*100.0;
	double FuelElectrodeThicknessCM=m_lfAnodeThickness*100.0;
	double ElectrolyteThicknessCM=m_lfElectrolyteThickness*100.0;
	
	double res=AirElecRes*AirElectrodeThicknessCM;//ohm-cm2 ~ 0.001 -- these numbers come from sample calc.
	res+=FuelElecRes*FuelElectrodeThicknessCM;//            ~ 0.0001
	res+=ElectrolyteElecRes*ElectrolyteThicknessCM;//       ~ 0.2
	res+=pCellRes->lfAnodeInterfaceResistance;//ohm-cm2
	res+=pCellRes->lfCathodeInterfaceResistance;//ohm-cm2
	res/=10000.0;// convert to ohm-meter2
	
	//The 'cell' node heat is everything but due to the SeparatorPlate Ohmic Heating
	double SeparatorPlateRes=GetTotalSepPltResistance(i);//ohm-m2
	
	res+=SeparatorPlateRes;
	
	return(res);//ohms-m2
}

double CElectrolyte::GetOverallNodeVoltageDrop(int i)
{
	// Diffusion not only lowers the reaction rate by impeading the movement of reactants
	// to the surface, but also adds to the voltage loss and heat generation!  Every
	// reactnat molecule that goes from the free stream chemical potential (concentration)
	// to the NEAR zero chemical potential at the electrolyte (under diffusion limit) will
	// convert a portion of its chemical potential to heat energy.  The simplest way
	// to consider this effect is that IF indeed diffusion begins to limit the voltage,
	// then the "lost voltage" due to "diffusion-losses" needs to end up as heat.
	double VDrop=m_lfNodalIdealVoltage[i]-m_lfLoadVoltage;
	if(VDrop<0.0){
		if(lfCurrentDensity[i]<1.0E-10){
			// then let's ignore the losses since they are tolerance errors only
			VDrop=0.0;
		}
		else {
			printf("THIS CAN'T HAPPEN--NEGATIVE VOLTAGE(%le @node=%d)?\n",VDrop,i);
		}
	}
	return(VDrop);//volts=function(T,current...)
}

void CElectrolyte::SetGeomArrays(const double * lfXS_Area, const double * lfDelX)
{
	XS_Area=lfXS_Area;
	DelX=lfDelX;
}

int CElectrolyte::Setup(CAnodeGasModel *pAnode, CCathodeGasModel *pCathode, CSeparatorPlate *pAirSepPlt,CSeparatorPlate *pFuelSepPlt, int nNoNodes,double lfInitialTemp,double lfElectrolyteThickness, double lfAnodeThickness,double lfCathodeThickness, double lfLoad, const struct CellResistance *CellResistance, double lfCathodeExgCurDensityB, double lfCathodeExgCurDensityM, double lfAnodeExgCurDensity, double lfAlphaCathodeBCoef, double lfAlphaCathodeMCoef, double lfAlphaAnode, const double *lfDelX, ChanGeom *pChanGeom, double lfCellWidth, const double *lfXS_Area, double lfC, double lfRho, bool bDoRadiation)
{
	An=pAnode;
	Ca=pCathode;
	AirSepPlt=pAirSepPlt;
	FuelSepPlt=pFuelSepPlt;
	
	m_lfLoadVoltage=-1.0;
	m_nNoNodes=nNoNodes;
	m_lfAlphaAnode=lfAlphaAnode;
	m_lfAlphaCathodeBCoef=lfAlphaCathodeBCoef;
	m_lfAlphaCathodeMCoef=lfAlphaCathodeMCoef;
	m_lfAnodeExchangeCurrentDensity=lfAnodeExgCurDensity;
	m_lfCathodeExchangeCurrentDensityB=lfCathodeExgCurDensityB;
	m_lfCathodeExchangeCurrentDensityM=lfCathodeExgCurDensityM;
	
	m_lfElectrolyteThickness=lfElectrolyteThickness;
	m_lfAnodeThickness=lfAnodeThickness;
	m_lfCathodeThickness=lfCathodeThickness;
	
	m_bDoRadiation=bDoRadiation;
	
	pCellRes=CellResistance;
	
	m_lfLoadResistance=lfLoad;
	m_lfRho=lfRho;
	m_lfC=lfC;
	DelX=lfDelX;
	XS_Area=lfXS_Area;
	m_lfCellWidth=lfCellWidth;
	m_ChanGeom=*pChanGeom;
	int i;
	for(i=0;i<m_nNoNodes;i++){
		m_lfTemp[CUR][i]=lfInitialTemp;
		m_lfTemp[NXT][i]=lfInitialTemp;
		lfCurrentDensity[i]=300.0;// this is very small current density (300.0 mA/cm2)
	}
	return(1);
}

int CElectrolyte::ContinueSetup(double lfLoad, double lfCathodeExgCurDensityB, double lfCathodeExgCurDensityM, double lfAnodeExgCurDensity, const double *lfDelX, const double *lfXS_Area, bool bDoRadiation)
{
	m_lfAnodeExchangeCurrentDensity=lfAnodeExgCurDensity;
	m_lfCathodeExchangeCurrentDensityB=lfCathodeExgCurDensityB;
	m_lfCathodeExchangeCurrentDensityM=lfCathodeExgCurDensityM;
	m_lfLoadResistance=lfLoad;
	DelX=lfDelX;
	XS_Area=lfXS_Area;
	m_bDoRadiation=bDoRadiation;
	int i;
	for(i=0;i<m_nNoNodes;i++){
		m_lfTemp[NXT][i]=m_lfTemp[CUR][i];
	}
	for(i=0;i<m_nNoNodes;i++){
		m_lfNodalIdealVoltage_AtElectrolyteSurface[i]=0.0;
	}
	return(1);
}

void CElectrolyte::PrintSolidTemps(FILE *f)
{
  int i;
	for(i=1;i<m_nNoNodes-1;i++){
		fprintf(f,"%le,",m_lfTemp[CUR][i]);
	}
}

void CElectrolyte::PrintData(FILE * f, bool bDoHeader)
{
	
	int i;
	if(bDoHeader){
		fprintf(f,"LoadVoltage [volt],LoadResistance[ohm],Cell Efficiency (just those assoc. with departure from ideal electrochemistry; i.e. not accounting for any unused fuel and not accounting for entropy loss due to reaction) [1],Cell Std. Def. Eff. (LHV but not accounting for any unused fuel) [1],Produced Power [watt], Overall Q Gen. [watt], Total Loss due to Conc.+Electrochem.+Ohmic [watt], CathActivLoss [1], AnodActivLoss[1], CathConcLoss [1], AnodConcLoss [1], OhmicLoss [1],");
		for(i=1;i<m_nNoNodes-1;i+=PRINT_STEP){
			fprintf(f,"ElecPlt T%d,",i);
		}
		for(i=1;i<m_nNoNodes-1;i+=PRINT_STEP){
			fprintf(f,"IdealVoltage%d,",i);
		}
		for(i=1;i<m_nNoNodes-1;i+=PRINT_STEP){
			fprintf(f,"i[A/m2]%d,",i);
		}
		for(i=1;i<m_nNoNodes-1;i+=PRINT_STEP){
			fprintf(f,"Q_delS[watt/m2]%d,",i);
		}
		for(i=1;i<m_nNoNodes-1;i+=PRINT_STEP){
			fprintf(f,"Q_NonMassDiff[watt/m2]%d,",i);
		}
	}
	else {
		double SumIdealPower=0.0;
		double SumCurrentFlowAmps=0.0;
		for(i=1;i<m_nNoNodes-1;i++){
			SumIdealPower+=m_lfNodalIdealVoltage[i]*lfCurrentDensity[i]*DelX[i]*m_lfCellWidth;
			SumCurrentFlowAmps+=lfCurrentDensity[i]*DelX[i]*m_lfCellWidth;
		}
		double SuppliedChemicalPowerLHV=An->NodeInputChemicalPower(0);
		double ActualPower=m_lfLoadVoltage*SumCurrentFlowAmps;
		double ElectrochemEfficiency=ActualPower/SumIdealPower;
		double StandardLHV_Eff=ActualPower/SuppliedChemicalPowerLHV;
		// calc. overall heat loss by determining overall fuel conversion minus the elec. power generated...
		double OverallHeatGen=(An->NodeInputChemicalPower(0)-An->NodeInputChemicalPower(m_nNoNodes-1))-ActualPower;
		
		fprintf(f,"%le,%le,%le,%le,%le,%le,",m_lfLoadVoltage,m_lfLoadResistance,ElectrochemEfficiency,StandardLHV_Eff,ActualPower,OverallHeatGen);
		fprintf(f,"%le,%le,%le,%le,%le,%le,",m_lfSumTotalLoss,m_lfSumCathActivLoss,m_lfSumAnodActivLoss,m_lfSumCathConcLoss,m_lfSumAnodConcLoss,m_lfSumOhmicLoss);
		for(i=1;i<m_nNoNodes-1;i+=PRINT_STEP){
			fprintf(f,"%le,",m_lfTemp[CUR][i]);
		}
		for(i=1;i<m_nNoNodes-1;i+=PRINT_STEP){
			fprintf(f,"%le,",m_lfNodalIdealVoltage[i]);
		}
		for(i=1;i<m_nNoNodes-1;i+=PRINT_STEP){
			fprintf(f,"%le,",lfCurrentDensity[i]);
		}
		for(i=1;i<m_nNoNodes-1;i+=PRINT_STEP){
			fprintf(f,"%le,",lfEntropyHeatGen[i]/(DelX[i]*m_lfCellWidth));
		}
		for(i=1;i<m_nNoNodes-1;i+=PRINT_STEP){
			fprintf(f,"%le,",(m_lfNodalIdealVoltage_AtElectrolyteSurface[i]-m_lfLoadVoltage)*lfCurrentDensity[i]);
		}
	}
}
/*
void CElectrolyte::Serialize(CArchive & ar)
{
int i;
if (ar.IsStoring())	{
ar<<(WORD)m_nNoNodes;
for(i=0;i<m_nNoNodes;i++){
ar<<lfEntropyHeatGen[i];
ar<<m_lfNodalIdealVoltage[i];
ar<<lfCurrentDensity[i];
ar<<m_lfTemp[CUR][i];
}
ar<<m_lfLoadVoltage;
ar<<m_lfLoadResistance;//ohm
ar<<m_lfAnodeExchangeCurrentDensity;
ar<<m_lfAlphaAnode;
ar<<m_lfCathodeExchangeCurrentDensityB;
ar<<m_lfCathodeExchangeCurrentDensityM;
ar<<m_lfAlphaCathodeBCoef;
ar<<m_lfAlphaCathodeMCoef;
ar<<m_lfGmMoleElectronsPerGmMoleH2;//g-moles of electrons per g-mole of H2
ar<<m_lfGmMoleElectronsPerGmMoleO2;//g-moles of electrons per g-mole of O2
ar<<m_lfRho;
ar<<m_lfC;

ar<<m_lfSumCathActivLoss;//added 10-22-98
ar<<m_lfSumAnodActivLoss;
ar<<m_lfSumCathConcLoss;
ar<<m_lfSumAnodConcLoss;
ar<<m_lfSumOhmicLoss;
}
else {
WORD w;
ar>>w;	m_nNoNodes=(int)w;
for(i=0;i<m_nNoNodes;i++){
ar>>lfEntropyHeatGen[i];
ar>>m_lfNodalIdealVoltage[i];
ar>>lfCurrentDensity[i];
ar>>m_lfTemp[CUR][i];
}
ar>>m_lfLoadVoltage;
ar>>m_lfLoadResistance;//ohm
ar>>m_lfAnodeExchangeCurrentDensity;
ar>>m_lfAlphaAnode;
ar>>m_lfCathodeExchangeCurrentDensityB;
ar>>m_lfCathodeExchangeCurrentDensityM;
ar>>m_lfAlphaCathodeBCoef;
ar>>m_lfAlphaCathodeMCoef;
ar>>m_lfGmMoleElectronsPerGmMoleH2;//g-moles of electrons per g-mole of H2
ar>>m_lfGmMoleElectronsPerGmMoleO2;//g-moles of electrons per g-mole of O2
ar>>m_lfRho;
ar>>m_lfC;

  ar>>m_lfSumCathActivLoss;//added 10-22-98
  ar>>m_lfSumAnodActivLoss;
  ar>>m_lfSumCathConcLoss;
  ar>>m_lfSumAnodConcLoss;
  ar>>m_lfSumOhmicLoss;
  }
  }
		  */  // DAS
		  
		  
void CElectrolyte::InitializeElectrolyteData(double Ti)
{
	  // We do it this way, in order to avoid an hard iteration step during transient calc.
	  double lfAlphaCathode=GetCathodeAlphaCoef(Ti);
	  double lfMaxVal=3.0/(lfAlphaCathode*m_lfGmMoleElectronsPerGmMoleO2*FARADAY/Runiv);
	  double lfVal=0.0;
	  double lfStepVal=lfMaxVal/MAX_CURRENTARRAY;
	  int i;
	  for(i=0;i<MAX_CURRENTARRAY;i++){
		  m_lfStoredExponentParameter[i]=lfVal;
		  m_lfStoredCurrentDensity[i]=exp(lfAlphaCathode*m_lfGmMoleElectronsPerGmMoleO2*FARADAY/Runiv*lfVal);
		  m_lfStoredCurrentDensity[i]-=exp((lfAlphaCathode-1.0)*m_lfGmMoleElectronsPerGmMoleO2*FARADAY/Runiv*lfVal);
		  lfVal+=lfStepVal;
	  }
}

double CElectrolyte::GetCathodeActivationLoss(double Ti, double lfCurrentDensity)
{
	  double Tafel;
	  
	  double lfCathodeExchangeCurrentDensity=GetCathodeExchangeCurrentDensity(Ti);
	  double lfAlphaCathode=GetCathodeAlphaCoef(Ti);
	  
	  double logArg=lfCurrentDensity/(lfCathodeExchangeCurrentDensity+0.000000001);
	  if(logArg>exp(3.0)){
		  // then we will be within about 0.3 percent of the real current, and about 0.005 percent on the voltage loss due to activation
		  // this would be the high activation voltage case where one of the exponential terms can be ignored!!
		  // see my notes on "Activation Analysis"
		  double Test=log(logArg);
		  Tafel=Runiv*Ti/lfAlphaCathode/m_lfGmMoleElectronsPerGmMoleO2/FARADAY*Test;
		  return(Tafel);
	  }
	  
	  InitializeElectrolyteData(Ti);
	  
	  int nUpperIdx=MAX_CURRENTARRAY;
	  int nLowerIdx=0;
	  int i=(int)(MAX_CURRENTARRAY/2.);
	  while(nUpperIdx>nLowerIdx+2){
		  if(lfCurrentDensity>m_lfStoredCurrentDensity[i]*lfCathodeExchangeCurrentDensity){
			  nLowerIdx=i;
		  }
		  else {
			  nUpperIdx=i;
		  }
		  i=(int)((nLowerIdx+nUpperIdx)/2.0);
	  }
	  double diffL=-(m_lfStoredCurrentDensity[nLowerIdx]*lfCathodeExchangeCurrentDensity)+lfCurrentDensity;
	  double diffH=(m_lfStoredCurrentDensity[nUpperIdx]*lfCathodeExchangeCurrentDensity)-lfCurrentDensity;
	  if(!(nLowerIdx+1==nUpperIdx)){
		  if(diffL>diffH){
			  nLowerIdx=i;
		  }
		  else {
			  nUpperIdx=i;
		  }
	  }
	  if(nLowerIdx==nUpperIdx){
		  printf("BADTIME\n");
	  }
	  double wt=-(m_lfStoredCurrentDensity[nLowerIdx]*lfCathodeExchangeCurrentDensity)+lfCurrentDensity;
	  wt/=(m_lfStoredCurrentDensity[nUpperIdx]-m_lfStoredCurrentDensity[nLowerIdx])*lfCathodeExchangeCurrentDensity;
	  double ButlerVolmer=(m_lfStoredExponentParameter[nLowerIdx]*(1.0-wt)+m_lfStoredExponentParameter[nUpperIdx]*(wt))*Ti;
	  return(ButlerVolmer);// a loss in voltage due to activation should be returned as a postive valued number
}

double CElectrolyte::GetAnodeActivationLoss(double Ti, double lfCurrentDensity)
{
	  // this formula assumes that the second term to the electrode equation is negligible, which
	  // is true for relatively high overpotentials -- see p. 68 of Bockris and Srinivasan (1969).
	  // Hence, this form is similar to the Tafel Equation!
	  if(lfCurrentDensity/m_lfAnodeExchangeCurrentDensity>exp(3.0)){
		  return(Runiv*Ti/m_lfAlphaAnode/m_lfGmMoleElectronsPerGmMoleH2/FARADAY*log(lfCurrentDensity/m_lfAnodeExchangeCurrentDensity));
	  }
	  else{
		  return(0.0);
	  }
}

double CElectrolyte::GetLoadVoltage()
{
	  return(m_lfLoadVoltage);
}

double CElectrolyte::GetTotalCurrent()
{
	  double TotalCurrent=0;
	  int i;
	  for(i=0;i<m_nNoNodes;i++){
		  TotalCurrent+=lfCurrentDensity[i]*DelX[i]*m_lfCellWidth;
	  }
	  return(TotalCurrent);//amps
}

double CElectrolyte::GetMinTimeStep( const double * AnodeT, const double * CathodeT)
{
	  double minT=10000000000.0;
	  int i;
	  int nIdx=0;
	  for(i=1;i<m_nNoNodes-1;i++){
		  double Qgen=GetQGenOfCellNode(i);
		  double Hin=An->GetNetEnthalpyInputToCell(i, (const double) m_lfTemp[CUR][i],AnodeT[i])+Ca->GetNetEnthalpyInputToCell(i, (const double) m_lfTemp[CUR][i],CathodeT[i]);
		  double delTempAn=m_lfTemp[CUR][i]-An->GetTemp(i);
		  double delTempCa=m_lfTemp[CUR][i]-Ca->GetTemp(i);
		  double lfAcceptableDelTempPerTimeStep=MIN(delTempAn,delTempCa)/10.0;// only go 10% of the change
		  lfAcceptableDelTempPerTimeStep=MAX(lfAcceptableDelTempPerTimeStep,2.0);//should be able to treat some level w/o going too small just because deltaTemp is small..., right?
		  double lfQFromLowerSepPltToUpperElectr=0.0;
		  double lfQFromUpperSepPltToLowerElectr=0.0;
		  if(m_bDoRadiation){
			  lfQFromLowerSepPltToUpperElectr=g_lfStephanBoltz*(pow(m_plfLowerRadTemp[MAX_AXIALNODES*CUR+i],4.0)-pow(m_lfTemp[CUR][i],4.0))*DelX[i]*m_ChanGeom.lfChannelWidth*m_ChanGeom.lfNumberOfChannels;
			  lfQFromUpperSepPltToLowerElectr=g_lfStephanBoltz*(pow(m_plfUpperRadTemp[MAX_AXIALNODES*CUR+i],4.0)-pow(m_lfTemp[CUR][i],4.0))*DelX[i]*m_ChanGeom.lfChannelWidth*m_ChanGeom.lfNumberOfChannels;
		  }
		  double lfHtime=lfAcceptableDelTempPerTimeStep*m_lfRho*m_lfC*XS_Area[i]*DelX[i]/(Hin+Qgen-m_plfQFromElectrToAnodeGas[i]-m_plfQFromElectrToCathodeGas[i]+lfQFromLowerSepPltToUpperElectr+lfQFromUpperSepPltToLowerElectr);
		  lfHtime=fabs(lfHtime);
		  if(lfHtime<minT){
			  minT=lfHtime;
			  nIdx=i;
		  }
	  }
	  //	TRACE2("CellLimit: i=%d, T=%le,",nIdx,m_lfTemp[CUR][nIdx]);
	  return(minT);
}

int CElectrolyte::AnalyzeOveallLosses(const double *CathodeP, const double *AnodeP)
{
	  // calculate via power loss, since current may be different for each node making direct voltage comparison meaningless!
	  m_lfSumCathActivLoss=0.0;
	  m_lfSumAnodActivLoss=0.0;
	  m_lfSumCathConcLoss=0.0;
	  m_lfSumAnodConcLoss=0.0;
	  m_lfSumOhmicLoss=0.0;
	  int i;
	  for(i=1;i<m_nNoNodes-1;i++){
		  double lfAmps=lfCurrentDensity[i]*DelX[i]*m_lfCellWidth;
		  m_lfSumCathActivLoss+=lfAmps*GetCathodeActivationLoss(m_lfTemp[CUR][i],lfCurrentDensity[i]);
		  m_lfSumAnodActivLoss+=lfAmps*GetAnodeActivationLoss(m_lfTemp[CUR][i],lfCurrentDensity[i]);
		  // Note, the concentration losses produce a loss in operating voltage compared to where NO
		  // diffusion losses occur.
		  m_lfSumCathConcLoss+=-lfAmps*Ca->GetCathodeConcLoss(m_lfTemp[CUR][i],CathodeP[i], i, lfCurrentDensity[i]);
		  m_lfSumAnodConcLoss+=-lfAmps*An->GetAnodeConcLoss(m_lfTemp[CUR][i],AnodeP[i], i, lfCurrentDensity[i]);
		  m_lfSumOhmicLoss+=lfAmps*GetCellNodeResistance(i,m_lfTemp[CUR][i])*lfCurrentDensity[i];
	  }
	  m_lfSumTotalLoss=m_lfSumCathActivLoss+m_lfSumAnodActivLoss+m_lfSumCathConcLoss+m_lfSumAnodConcLoss+m_lfSumOhmicLoss;
	  m_lfSumCathActivLoss/=m_lfSumTotalLoss;
	  m_lfSumAnodActivLoss/=m_lfSumTotalLoss;
	  m_lfSumCathConcLoss/=m_lfSumTotalLoss;
	  m_lfSumAnodConcLoss/=m_lfSumTotalLoss;
	  m_lfSumOhmicLoss/=m_lfSumTotalLoss;
	  return(1);
}

double CElectrolyte::GetCathodeExchangeCurrentDensity(double Temp)
{
	  // assumes a linear effect on temperature per Bessette thesis.
	  // Keith Williams, Intro to FC, shows that it could also be a function of O2 concentration--see Massardo paper Equ. 8. or 1994 Achenbach.
	  double CathodeExchangeCurrentDensity=m_lfCathodeExchangeCurrentDensityB+m_lfCathodeExchangeCurrentDensityM*Temp;
	  if(CathodeExchangeCurrentDensity<1.0) CathodeExchangeCurrentDensity=-10000.0/(CathodeExchangeCurrentDensity-1010.0);// decay to zero, but never go to zero....
	  return(CathodeExchangeCurrentDensity);
}

void CElectrolyte::SetUniformCurrentDensities(double val)
{
  int i;
	  for(i=1;i<m_nNoNodes-1;i++){
		  lfCurrentDensity[i]*=val;
	  }
}

double CElectrolyte::CurrentAveCellTemp()
{
	  // weight the temperature by the amount of molecules converted at that temperature which is proportional to the electrical current 
	  double AveT=0.0;
	  double TotCurrent=0.0;
	  int i;
	  for(i=1;i<m_nNoNodes-1;i++){
		  AveT+=m_lfTemp[CUR][i]*lfCurrentDensity[i]*DelX[i]*m_lfCellWidth;
		  TotCurrent+=lfCurrentDensity[i]*DelX[i]*m_lfCellWidth;
	  }
	  AveT/=TotCurrent;
	  return(AveT);
}

void CElectrolyte::SetUpperSurfaceHeatRemoval(const double * lfQFromElectrToAnodeGas, const double * lfRadiationTemp)
{
	  m_plfQFromElectrToAnodeGas=lfQFromElectrToAnodeGas;
	  m_plfUpperRadTemp=lfRadiationTemp;
}

void CElectrolyte::SetLowerSurfaceHeatRemoval(const double * lfQFromElectrToCathodeGas, const double * lfRadiationTemp)
{
	  m_plfQFromElectrToCathodeGas=lfQFromElectrToCathodeGas;
	  m_plfLowerRadTemp=lfRadiationTemp;
}


double CElectrolyte::GetCathodeAlphaCoef(double Ti)
{
	  double alpha=m_lfAlphaCathodeBCoef+m_lfAlphaCathodeMCoef*Ti;
	  if(alpha<0.1) alpha=0.1;//arbitrary limits set for now...
	  if(alpha>0.9) alpha=0.9;
	  return(alpha);
}

} // end namespace Vision21

