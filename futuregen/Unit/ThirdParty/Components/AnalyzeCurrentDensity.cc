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
double CElectrolyte::GetAveCellNernstVoltage()
{
	double AveCurr=0.0;
	for(int i=1;i<m_nNoNodes-1;i++){
		AveCurr+=GetNodeENernstVoltage(i);
		}
	AveCurr/=(double)(m_nNoNodes-2);
	return(AveCurr);
}


int CElectrolyte::AnalyzeCurrDen_GasQS()
{
double LoadVoltageGuess=0.7;
if(m_lfLoadVoltage>0.0){
	LoadVoltageGuess=m_lfLoadVoltage;// use the last employed voltage as initial guess
	}
double lfStepVolt=LoadVoltageGuess/1000.;

bool   bLowerVolt=false;
double lfLowerVolt=0.0;
double lfLowerSideDiffVolt;
bool   bUpperVolt=false;
double lfUpperVolt=0.0;
double lfMaxPossibleVolt=GetNodeENernstVoltage(0);
double lfUpperSideDiffVolt;

int nUpper=1;
int nLower=1;
bool bDoHoneIn=true;// If true, this forces the use of the more robust stepping technique...
while(1){
	int zz=AnalyzeCurrDenGivenVoltage_GasQS(LoadVoltageGuess, bDoHoneIn);
	if(TRY_REDUCED_VOLTAGE==zz){
		//probably guessed too high a voltage...
		lfUpperVolt=LoadVoltageGuess;
		bUpperVolt=false;// need to clear this, because at this point we can't evaluate lfUpperSideDiffVolt
		lfStepVolt=-fabs(lfStepVolt*0.95);
		LoadVoltageGuess+=lfStepVolt;
		lfStepVolt=-LoadVoltageGuess*0.001;
		bLowerVolt=false;
		continue;
		}
	else if(zz==MUST_HAVE_ELECTROLYSIS_CONDITIONS) {
		// possibly attemptng electrolysis--not considered here
		return(MUST_HAVE_ELECTROLYSIS_CONDITIONS);
		}
	double lfTotalCurrent=GetTotalCurrent();
	double LoadCurrentExpected=LoadVoltageGuess/m_lfLoadResistance;
	double Diff=LoadCurrentExpected-lfTotalCurrent;
	double AbsDiff=fabs(Diff/LoadCurrentExpected);
	if(AbsDiff<CURRENT_PRECISION) {
		break;
		}
	if(nUpper>10){
		// may be experiencies various convergence tolerance clashes--possibly need to decrease limits
		bLowerVolt=false;
		lfStepVolt=-LoadVoltageGuess/1000.0;
		}
	if(nLower>10){
		// may be experiencies various convergence tolerance clashes--possibly need to increase limits
		bUpperVolt=false;
		lfStepVolt=LoadVoltageGuess/1000.0;
		}
	// because of non-linearities, can't do pure 'end point' convergence technique.
	if(bLowerVolt&&bUpperVolt){
		if(Diff>0.0){
			nUpper++;
			nLower=1;
			lfUpperVolt=LoadVoltageGuess;
			lfUpperSideDiffVolt=Diff;
			}
		else{
			nLower++;
			nUpper=1;
			lfLowerVolt=LoadVoltageGuess;
			lfLowerSideDiffVolt=Diff;
			}
		lfStepVolt=-LoadVoltageGuess+lfLowerVolt-(lfUpperVolt-lfLowerVolt)/(lfUpperSideDiffVolt-lfLowerSideDiffVolt)*(lfLowerSideDiffVolt);// update StepVolt just to make sure it stays current
		LoadVoltageGuess+=lfStepVolt;
		}
	else {
		// start by searching for min/max limits...
		nLower=0;
		nUpper=0;
		if(Diff>0.0){
			bUpperVolt=true;
			lfUpperVolt=LoadVoltageGuess;
			lfUpperSideDiffVolt=Diff;
			}
		else {
			bLowerVolt=true;
			lfLowerVolt=LoadVoltageGuess;
			lfLowerSideDiffVolt=Diff;
			}
		if(Diff>0.0 && lfStepVolt>0.0){// need to go to smaller voltage so that expected current is less and calc. current through cell is more
			lfStepVolt*=-0.3333;
			}
		else if(Diff<0.0 && lfStepVolt<0.0){
			lfStepVolt*=-0.33333;
			}
		else {
			lfStepVolt*=2.0;
			}
		if(LoadVoltageGuess+lfStepVolt<0.0){
			lfStepVolt=-LoadVoltageGuess/2.0;
			}
		if(LoadVoltageGuess+lfStepVolt>lfMaxPossibleVolt){
			lfStepVolt=(lfMaxPossibleVolt-LoadVoltageGuess)/2.0;
			}
		LoadVoltageGuess+=lfStepVolt;
		}
	}
return(GOOD_RESULT);
}

#define MIN_CURLIMIT	1.0E-25  // this is the minimum current to analyze for a node

int CElectrolyte::AnalyzeCurrDenGivenVoltage_GasQS(double lfCellVoltage,bool bHoneIn)
{
// In this analysis, you are given voltage on some load resistance.  Then crunch through
// to calculate the resultant current through all nodes...
// This routine cannot handle negative current flow--that is electrolysis!!!

if(lfCellVoltage<0.0){
	printf("ERR AnalCurrDenGivenVolts\n");
	}

m_lfLoadVoltage=lfCellVoltage;

double AnPExitSave=An->P[m_nNoNodes-1];
double lfAnStep=An->Vel[CUR][0]/1000.0;
bool bAnUpperBound=false;
bool bAnLowerBound=false;
double lfAnLower=-1.0,lfAnUpper=-1.0,lfAnLowerSideDelForce=-1.0,lfAnUpperSideDelForce=-1.0;

double CaPExitSave=Ca->P[m_nNoNodes-1];
double lfCaStep=Ca->Vel[CUR][0]/1000.0;
bool bCaUpperBound=false;
bool bCaLowerBound=false;
double lfCaLower=-1.0,lfCaUpper=-1.0,lfCaLowerSideDelForce=-1.0,lfCaUpperSideDelForce=-1.0;

double lfUpperCurrDiffVolt=1.0;
double lfLowerCurrDiffVolt=1.0;
double lfOldAnVel=-1.0;
double lfOldAnDelForce=1.0;

double lfOldCaVel=-1.0;
double lfOldCaDelForce=1.0;

int i;
bool bFirstPass=true;
bool bDoFlowBoostForENernst=true;

STARTOFLOOP:


while(1){// iterate to get Vel at inlet by balancing overall friction with pressure drop...
	bool bAnHitAbsoluteLowerVelLimit=false;
	bool bCaHitAbsoluteLowerVelLimit=false;
	bool bAnOverShootingDelP=false;
	bool bCaOverShootingDelP=false;
	for(i=1;i<m_nNoNodes-1;i++){
		double Ti=m_lfTemp[CUR][i];
		double MolarDelHStd=GetMolarDelHStd(Ti);//joule/kg-mole
		double MolarDelSStd=GetMolarDelSStd(Ti);// per kg-mole of H2! without effects of mixing and at std. conditions!!
		double MolarDelGStd=MolarDelHStd-Ti*MolarDelSStd;//delG per kg-mole of H2
		MolarDelGStd/=1000.0;//delG per g-mole of H2

		double lfCurrentDensityGuess=0.1*100.0*100.0;//ampere/m2
		if(lfCurrentDensity[i]>0.0){
			lfCurrentDensityGuess=lfCurrentDensity[i];
			}
		if(i>1 && bFirstPass){
			// assume that if there are big variations in current, that there must have been a big change in conditions, so use upstream data to est. new downstream data
			double lfTst=lfCurrentDensity[i-1]/lfCurrentDensityGuess;
			if(lfTst>30.0 || lfTst<0.03333){
				lfCurrentDensityGuess=lfCurrentDensity[i-1];
				}
			}
		double lfStep=CURRENT_PRECISION*lfCurrentDensityGuess*4.5;// try to step around close to previous solution first

		double lfTotalCellResistanceOhmM2=GetCellNodeResistance(i,Ti);

		bool bUpperBound=false;
		bool bLowerBound=false;
		double lfUpper=-10.0;
		double lfLower=-10.0;

		double TotalEntropyChange=0.0;
		double CathodeEtaConc=0.0;
		double AnodeEtaConc=0.0;
		double ENernst;

		double lfMinCurr=MIN_CURLIMIT;
		double lfMaxCurr=-1000000.0;
		double lfAnodeLimitingSupplyCurrentDensity=An->GetLimitingSupplyCurrentDensity(i);
		lfMaxCurr=lfAnodeLimitingSupplyCurrentDensity;
		double lfCathodeLimitingSupplyCurrentDensity=Ca->GetLimitingSupplyCurrentDensity(i);
		if(lfCathodeLimitingSupplyCurrentDensity<lfMaxCurr){
			lfMaxCurr=lfCathodeLimitingSupplyCurrentDensity;
			}
		if(lfCurrentDensityGuess>lfAnodeLimitingSupplyCurrentDensity){
			lfCurrentDensityGuess=lfAnodeLimitingSupplyCurrentDensity*0.99999999;
			}
		if(lfCurrentDensityGuess>lfCathodeLimitingSupplyCurrentDensity){
			lfCurrentDensityGuess=lfCathodeLimitingSupplyCurrentDensity*0.99999999;
			}
		int nMin=1;
		int nMax=1;
		while(1){
			int cnt=0;
			while(An->AnalyzeNodeQSGasPhysics(i, lfCurrentDensityGuess)==QS_LOWER_CURRENT ||	lfCurrentDensityGuess>=An->GetAnodeLimitingCurrentDensity(i, An->P[i],Ti)){
				lfMaxCurr=lfCurrentDensityGuess;
				cnt++;
				if(cnt>100){
					lfMinCurr*=0.99;
					}
				lfCurrentDensityGuess=(lfMinCurr+lfMaxCurr)/2.0;
				lfStep=(lfMaxCurr-lfMinCurr)/100.0;
				}
			bool bUpdatedCurr=false;
			cnt=0;
			while(Ca->AnalyzeNodeQSGasPhysics(i, lfCurrentDensityGuess)==QS_LOWER_CURRENT || lfCurrentDensityGuess>=Ca->GetCathodeLimitingCurrentDensity(i, Ca->P[i], Ti)){
				lfMaxCurr=lfCurrentDensityGuess;
				cnt++;
				if(cnt>100){
					lfMinCurr*=0.99;
					}
				lfCurrentDensityGuess=(lfMinCurr+lfMaxCurr)/2.0;
				lfStep=(lfMaxCurr-lfMinCurr)/100.0;
				}
			if(bUpdatedCurr){
				An->AnalyzeNodeQSGasPhysics(i, lfCurrentDensityGuess);
				}

			double AnodeP=An->P[i];
			double CathodeP=Ca->P[i];

			double ReactantActivity=GetReactantActivity(i,AnodeP,CathodeP);
			double ProductActivity=GetProductActivity(i,AnodeP,CathodeP);

			ENernst=-MolarDelGStd/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;
			ENernst+=Runiv*Ti*log(ReactantActivity/ProductActivity)/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;

			TotalEntropyChange=MolarDelSStd/1000.0+Runiv*log(ReactantActivity/ProductActivity);//joule/gm-moleH2/K
			if(ENernst<0.0){
				printf("Can't do electolysis\n");
				An->P[m_nNoNodes-1]=AnPExitSave;
				Ca->P[m_nNoNodes-1]=CaPExitSave;
				return(MUST_HAVE_ELECTROLYSIS_CONDITIONS);
				}

			double CaSpO2=Ca->Sp[CUR][i][O2];
			double AnSpH2=An->Sp[CUR][i][H2]+0.000000001;// assume that it will never be zero....
			if(m_lfLoadVoltage>ENernst){
				//perhaps we lost too much reactant upstream due to electrochemistry--first try lowering the current density, then try
				//upping the inlet velocity and set the lowerbound parameters if current density has become smaller than practical...
				if(lfCurrentDensityGuess>MIN_CURLIMIT*1.01){
					// so long as current density is 'high', keep trying a lower value...until it is smaller than a practical value
					lfMaxCurr=lfCurrentDensityGuess;
					lfCurrentDensityGuess=(lfMinCurr+lfMaxCurr)/2.0;
					continue;
					}
				else if(bDoFlowBoostForENernst && (((An->Sp[CUR][0][H2]-AnSpH2)/An->Sp[CUR][0][H2]>0.90 && !An->bFixedFlow()) || ((Ca->Sp[CUR][0][O2]-CaSpO2)/Ca->Sp[CUR][0][O2]>0.90 && !Ca->bFixedFlow()))){
					// we apparently have consumed a lot of reactant.  try increaseing inlet flow if that is possible.
					// that is, there might be room to increase flow some more so as to boost voltage to the desired value...
					//!!!!!!!!!!!!  if doing molten-carbonate, may have to update this part to include CO2
					if(!An->bFixedFlow() && (An->Sp[CUR][0][H2]-AnSpH2)/An->Sp[CUR][0][H2]>(Ca->Sp[CUR][0][O2]-CaSpO2)/Ca->Sp[CUR][0][O2]){
						lfAnLower=An->Vel[CUR][0];
						bAnLowerBound=true;
						An->Vel[CUR][0]*=1.1;
						bAnHitAbsoluteLowerVelLimit=true;
						}
					else if(!Ca->bFixedFlow()){
						lfCaLower=Ca->Vel[CUR][0];
						bCaLowerBound=true;
						Ca->Vel[CUR][0]*=1.1;
						bCaHitAbsoluteLowerVelLimit=true;
						}
					}
				else {
					An->P[m_nNoNodes-1]=AnPExitSave;
					Ca->P[m_nNoNodes-1]=CaPExitSave;
					if(i==1){
						// We can't even get the inlet flow boost to work, so must be an impossible
						// situation, such as requesting a larger voltage than what can
						// theoretically be provided at the very inlet.
						// Try reducing requested voltage.
						printf("We apparently have an unrealistic situation\n");
						return(TRY_REDUCED_VOLTAGE);
						}
					else {
						// Perhaps we consumed all reactants upstream, such as occurs when
						// we don't supply enough reactant flows for the proposed voltage
						// condition which drives the current consumption to the limit.
						// Under this condition, we would get reverse current (electrolysis)
						// in the downstream nodes when frictional losses cause further
						// reduce the Reactant/Product Activities Ratio causing ENernst
						// to be reduced relative to upstream nodes.
						// This has been shown in the model, and again,
						// can occur when the reactants are so reduced that even the frictional
						// pressure drop causes reduced ENernst conditions as you go downstream
						// and the weak reactants can't support the loss in chemical potential
						// that results from pressure losses.

						// let's assume for now that this condition will only produce minor
						// levels of electrolysis, and that the current at the present node is
						// zero...  This is a potential research area for modeling.
						if(lfCurrentDensityGuess>MIN_CURLIMIT*4.0){
							printf("ERR MIN CURRENT\n");
							}
						break;
						// if we find that there can be significant reverse current, then
						// well need to add a ... return(MUST_HAVE_ELECTROLYSIS_CONDITIONS);
						}
					}
				goto STARTOFLOOP;
				}

			double lfAnodeLimitingCurrentDensity=An->GetAnodeLimitingCurrentDensity(i, AnodeP,Ti);
			if(lfAnodeLimitingSupplyCurrentDensity<lfAnodeLimitingCurrentDensity){
				lfAnodeLimitingCurrentDensity=lfAnodeLimitingSupplyCurrentDensity;
				}
			if(lfCurrentDensityGuess>lfAnodeLimitingCurrentDensity){
				lfCurrentDensityGuess=lfAnodeLimitingCurrentDensity*.99932;
				}

			double lfCathodeLimitingCurrentDensity=Ca->GetCathodeLimitingCurrentDensity(i, CathodeP, Ti);
			if(lfCathodeLimitingSupplyCurrentDensity<lfCathodeLimitingCurrentDensity){
				lfCathodeLimitingCurrentDensity=lfCathodeLimitingSupplyCurrentDensity;
				}
			if(lfCurrentDensityGuess>lfCathodeLimitingCurrentDensity){
				lfCurrentDensityGuess=lfCathodeLimitingCurrentDensity*.99942;
				}
		
			double CathodeEtaActivation=GetCathodeActivationLoss(Ti,lfCurrentDensityGuess);
			CathodeEtaConc=Ca->GetCathodeConcLoss(Ti,CathodeP,i,lfCurrentDensityGuess);

			double AnodeEtaActivation=GetAnodeActivationLoss(Ti,lfCurrentDensityGuess);
			AnodeEtaConc=An->GetAnodeConcLoss(Ti,AnodeP,i,lfCurrentDensityGuess);

			double TotalOutputVoltage=0.0;
			TotalOutputVoltage+=ENernst;
			TotalOutputVoltage-=(CathodeEtaActivation-CathodeEtaConc);
			TotalOutputVoltage-=(AnodeEtaActivation-AnodeEtaConc);
			TotalOutputVoltage-=lfCurrentDensityGuess*lfTotalCellResistanceOhmM2;
			if(TotalOutputVoltage<0.0) {
				lfMaxCurr=lfCurrentDensityGuess;
				lfCurrentDensityGuess=(lfMinCurr+lfMaxCurr)/2.0;
				TotalOutputVoltage=0.000000001;
				continue;
				}

			double DiffVolt=TotalOutputVoltage-m_lfLoadVoltage;
			double AbsDiffVolt=fabs(DiffVolt);
			double diffLimits=(lfMaxCurr-lfMinCurr)/lfMaxCurr;
			if(fabs(DiffVolt)<CURRENT_PRECISION/25.0 || diffLimits<0.000000001){// I think the slope of the V-I curve is such that dividing by 20 should give a similar assessment for "convergence" between V and I
				if(diffLimits<0.000000001){
					printf("CHECK THIS OUT\n");
					}
				break;
				}

			// the system appears to be too non-linear (for the general case) to do pure end-point convergence...
			if(bHoneIn){
				// here, for robustness, we keep stepping back and forth until we converge
				if(DiffVolt>0.0){
					lfMinCurr=lfCurrentDensityGuess;
					}
				else if(DiffVolt<0.0){
					lfMaxCurr=lfCurrentDensityGuess;
					}
				if(DiffVolt>0.0 && lfStep<0.0 || DiffVolt<0.0 && lfStep>0.0){
					lfStep*=-0.22232;
					}
				else {
					lfStep*=1.8;
					}
				if(lfCurrentDensityGuess+lfStep>lfMaxCurr){
					lfStep=(lfCurrentDensityGuess+lfMaxCurr)/2.0-lfCurrentDensityGuess;
					}
				if(lfCurrentDensityGuess+lfStep<lfMinCurr){
					lfStep=(lfCurrentDensityGuess+lfMinCurr)/2.0-lfCurrentDensityGuess;
					}
				lfCurrentDensityGuess+=lfStep;
				}
			else{
				if(DiffVolt>0.0){
					nMax=1;
					nMin++;
					lfMinCurr=lfCurrentDensityGuess;
					}
				else if(DiffVolt<0.0){
					nMin=1;
					nMax++;
					lfMaxCurr=lfCurrentDensityGuess;
					}
				if(nMin>10){
					lfMaxCurr*=1.1;
					}
				if(nMax>10){
					lfMinCurr/=1.1;
					}
				lfCurrentDensityGuess=(lfMinCurr*(double)nMax+lfMaxCurr*(double)nMin)/((double)(nMax+nMin));
				}
			}
		lfCurrentDensity[i]=lfCurrentDensityGuess;
		lfEntropyHeatGen[i]=-m_lfTemp[CUR][i]*TotalEntropyChange*lfCurrentDensity[i]*DelX[i]*m_lfCellWidth/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;//watts

		m_lfNodalIdealVoltage[i]=ENernst;
		m_lfNodalIdealVoltage_AtElectrolyteSurface[i]=ENernst+CathodeEtaConc+AnodeEtaConc;//these etaConc losses are negative valued...

		if(An->HitPDropLimit(i,AnPExitSave) && !An->bFixedFlow()){
			lfAnUpper=An->Vel[CUR][0];
			bAnUpperBound=true;
			bAnOverShootingDelP=true;
			}
		if(Ca->HitPDropLimit(i,CaPExitSave) && !Ca->bFixedFlow()){
			lfCaUpper=Ca->Vel[CUR][0];
			bCaUpperBound=true;
			bCaOverShootingDelP=true;
			}
		if(bCaOverShootingDelP||bAnOverShootingDelP) break;
		}

	if(i==m_nNoNodes-1){
		bHoneIn=true;
		}

	if(bAnOverShootingDelP){
		if(bAnHitAbsoluteLowerVelLimit){
			bDoFlowBoostForENernst=false;
//			printf("We apparently have requested to high a voltage than what can be provided given the Specified DelP and Load Resistance\n");
//			An->P[m_nNoNodes-1]=AnPExitSave;
//			Ca->P[m_nNoNodes-1]=CaPExitSave;
//			return(TRY_REDUCED_VOLTAGE);
			}
		}
	else {
		An->AnalyzeNodeQSGasPhysics(i, 0.0);
		}
	if(bCaOverShootingDelP){
		if(bCaHitAbsoluteLowerVelLimit){
			bDoFlowBoostForENernst=false;
//			printf("We apparently have requested to high a voltage than what can be provided given the Specified DelP and Load Resistance\n");
//			An->P[m_nNoNodes-1]=AnPExitSave;
//			Ca->P[m_nNoNodes-1]=CaPExitSave;
//			return(TRY_REDUCED_VOLTAGE);
			}
		}
	else {
		Ca->AnalyzeNodeQSGasPhysics(i, 0.0);
		}

	double AnDelForce=(An->P[m_nNoNodes-1] - AnPExitSave)/(AnPExitSave+0.0000001);
	double CaDelForce=(Ca->P[m_nNoNodes-1] - CaPExitSave)/(CaPExitSave+0.0000001);
	double AnAbsdelForce=fabs(AnDelForce);
	double CaAbsdelForce=fabs(CaDelForce);
	if(AnAbsdelForce<0.000000005 && CaAbsdelForce<0.000000005 && !bAnOverShootingDelP && !bCaOverShootingDelP) {
		break;
		}
	if(An->bFixedFlow()){
		AnPExitSave=An->P[m_nNoNodes-1];
		}
	if(Ca->bFixedFlow()){
		CaPExitSave=Ca->P[m_nNoNodes-1];
		}

	if(!An->bFixedFlow()){//Do Anode Update....
		if(bAnUpperBound&&bAnLowerBound && i==(m_nNoNodes-1)){
			if(AnDelForce>0.0){
				if(An->Vel[CUR][0]>lfAnLower){
					lfAnLower=An->Vel[CUR][0];
					}
				}
			else {
				if(An->Vel[CUR][0]<lfAnUpper){
					lfAnUpper=An->Vel[CUR][0];
					}
				}
			lfAnStep=-(An->Vel[CUR][0]-lfOldAnVel)/(AnDelForce-lfOldAnDelForce)*AnDelForce/1.1;
			if(fabs(lfAnStep)<1.0E-20){
				lfAnStep=lfOldAnVel/1.0E5;
				lfAnUpper+=fabs(lfAnStep);
				lfAnLower-=fabs(lfAnStep);
				}
			lfOldAnVel=An->Vel[CUR][0];
			lfOldAnDelForce=AnDelForce;
			An->Vel[CUR][0]+=lfAnStep;
			if(An->Vel[CUR][0]>lfAnUpper){
				An->Vel[CUR][0]=(An->Vel[CUR][0]*0.3+0.7*lfAnUpper);
				lfAnUpper=An->Vel[CUR][0];
				}
			if(An->Vel[CUR][0]<lfAnLower){
				An->Vel[CUR][0]=(An->Vel[CUR][0]*0.3+0.7*lfAnLower);
				lfAnLower=An->Vel[CUR][0];
				}
			if(An->Vel[CUR][0]<0.0){
				An->Vel[CUR][0]=lfAnLower/2.0;
				}
		
			lfAnStep=(lfAnUpper-lfAnLower)/5.0;
			}
		else {
			if(bAnOverShootingDelP){
				lfAnUpperSideDelForce=AnDelForce;
				bAnUpperBound=true;
				lfAnUpper=An->Vel[CUR][0];
				if(bAnLowerBound){
					lfAnStep=-An->Vel[CUR][0]+(An->Vel[CUR][0]+lfAnLower)/2.0;
					}
				else {
					lfAnStep=-0.25*An->Vel[CUR][0];
					}
				An->Vel[CUR][0]+=lfAnStep;
				}
			else if(i==(m_nNoNodes-1)){
				if(AnDelForce<0.0){
					lfAnUpperSideDelForce=AnDelForce;
					bAnUpperBound=true;
					lfAnUpper=An->Vel[CUR][0];
					}
				else {
					lfAnLowerSideDelForce=AnDelForce;
					bAnLowerBound=true;
					lfAnLower=An->Vel[CUR][0];
					}
				if(lfOldAnVel>0.0){
					lfAnStep=-(An->Vel[CUR][0]-lfOldAnVel)/(AnDelForce-lfOldAnDelForce)*AnDelForce/1.2;
					}
				else {
					if(AnDelForce<0.0 && lfAnStep>0.0){
						lfAnStep*=-0.33333;
						}
					else if(AnDelForce>0.0 && lfAnStep<0.0){
						lfAnStep*=-0.3333;
						}
					else {
						if(fabs(lfAnStep*5.0)<An->Vel[CUR][0]){
							lfAnStep*=2.0;
							}
						else{
							lfAnStep=fabs(lfAnStep)/lfAnStep*An->Vel[CUR][0]/5.0;
							}
						}
					}
				lfOldAnVel=An->Vel[CUR][0];
				lfOldAnDelForce=AnDelForce;
				An->Vel[CUR][0]+=lfAnStep;
				}
			}
		}//End of Anode Check

	if(!Ca->bFixedFlow()){//Do Cathode Update....
		if(bCaUpperBound&&bCaLowerBound && i==(m_nNoNodes-1)){
			if(CaDelForce>0.0){
				if(Ca->Vel[CUR][0]>lfCaLower){
					lfCaLower=Ca->Vel[CUR][0];
					}
				}
			else {
				if(Ca->Vel[CUR][0]<lfCaUpper){
					lfCaUpper=Ca->Vel[CUR][0];
					}
				}
			lfCaStep=-(Ca->Vel[CUR][0]-lfOldCaVel)/(CaDelForce-lfOldCaDelForce)*CaDelForce/1.1;
			if(fabs(lfCaStep)<1.0E-20){
				lfCaStep=lfOldCaVel/1.0E5;
				lfCaUpper+=fabs(lfCaStep);
				lfCaLower-=fabs(lfCaStep);
				}
			lfOldCaVel=Ca->Vel[CUR][0];
			lfOldCaDelForce=CaDelForce;
			Ca->Vel[CUR][0]+=lfCaStep;
			if(Ca->Vel[CUR][0]>lfCaUpper){
				Ca->Vel[CUR][0]=(Ca->Vel[CUR][0]*0.3+0.7*lfCaUpper);
				lfCaUpper=Ca->Vel[CUR][0];
				}
			if(Ca->Vel[CUR][0]<lfCaLower){
				Ca->Vel[CUR][0]=(Ca->Vel[CUR][0]*0.3+0.7*lfCaLower);
				lfCaLower=Ca->Vel[CUR][0];
				}
			if(Ca->Vel[CUR][0]<0.0){
				Ca->Vel[CUR][0]=lfCaLower/2.0;
				}
		
			lfCaStep=(lfCaUpper-lfCaLower)/5.0;
			}
		else {
			if(bCaOverShootingDelP){
				lfCaUpperSideDelForce=CaDelForce;
				bCaUpperBound=true;
				lfCaUpper=Ca->Vel[CUR][0];
				if(bCaLowerBound){
					lfCaStep=-Ca->Vel[CUR][0]+(Ca->Vel[CUR][0]+lfCaLower)/2.0;
					}
				else {
					lfCaStep=-0.25*Ca->Vel[CUR][0];
					}
				Ca->Vel[CUR][0]+=lfCaStep;
				}
			else if(i==(m_nNoNodes-1)){
				if(CaDelForce<0.0){
					lfCaUpperSideDelForce=CaDelForce;
					bCaUpperBound=true;
					lfCaUpper=Ca->Vel[CUR][0];
					}
				else{
					lfCaLowerSideDelForce=CaDelForce;
					bCaLowerBound=true;
					lfCaLower=Ca->Vel[CUR][0];
					}
				if(lfOldCaVel>0.0){
					lfCaStep=-(Ca->Vel[CUR][0]-lfOldCaVel)/(CaDelForce-lfOldCaDelForce)*CaDelForce/1.2;
					}
				else {
					if(CaDelForce<0.0 && lfCaStep>0.0){
						lfCaStep*=-0.33333;
						}
					else if(CaDelForce>0.0 && lfCaStep<0.0){
						lfCaStep*=-0.3333;
						}
					else {
						if(fabs(lfCaStep*5.0)<Ca->Vel[CUR][0]){
							lfCaStep*=2.0;
							}
						else{
							lfCaStep=fabs(lfCaStep)/lfCaStep*Ca->Vel[CUR][0]/5.0;
							}
						}
					}
				lfOldCaVel=Ca->Vel[CUR][0];
				lfOldCaDelForce=CaDelForce;
				Ca->Vel[CUR][0]+=lfCaStep;
				}
			}
		}//End of Cathode Check
	bFirstPass=false;
	}

lfCurrentDensity[m_nNoNodes-1]=0.0;
lfCurrentDensity[0]=0.0;
An->Vel[NXT][0]=An->Vel[CUR][0];
Ca->Vel[NXT][0]=Ca->Vel[CUR][0];
for(i=0;i<An->m_nNoSp;i++){
	An->Sp[NXT][0][i]=An->Sp[CUR][0][i];
	}
for(i=0;i<Ca->m_nNoSp;i++){
	Ca->Sp[NXT][0][i]=Ca->Sp[CUR][0][i];
	}

An->P[m_nNoNodes-1]=AnPExitSave;
Ca->P[m_nNoNodes-1]=CaPExitSave;

AnalyzeOveallLosses(Ca->P, An->P);
	
return(GOOD_RESULT);
}

int CElectrolyte::AnalyzeCurrDen_FullyQS(CSeparatorPlate &SP, bool bDoQSSolidTempUpdate)
{
double LoadVoltageGuess=0.7;
if(m_lfLoadVoltage>0.0){
	LoadVoltageGuess=m_lfLoadVoltage;// use the last employed voltage as initial guess
	}
double lfStepVolt=LoadVoltageGuess/100.;

bool   bLowerVolt=false;
double lfLowerVolt=0.0;
double lfLowerSideDiffVolt;
bool   bUpperVolt=true;
double lfUpperVolt=GetNodeENernstVoltage(0);
double lfUpperSideDiffVolt;

bool   bDoThisTime=false;
double lfPtAVolt=0.0;
double lfPtADiff=0.0;
double lfPtBVolt=-1.0;
double lfPtBDiff=-1.0;
int nUpper=1;
int nLower=1;
bool bDoHoneIn=false;// start by saying false until we know we are close to the solution...
while(1){
	printf("Volt=%le, ",LoadVoltageGuess);
	int zz=AnalyzeCurrDenGivenVoltage_FullyQS(LoadVoltageGuess, SP, bDoQSSolidTempUpdate,bDoHoneIn);
	if(zz==TRY_REDUCED_VOLTAGE){
		//probably guessed too high a voltage...
		lfUpperVolt=LoadVoltageGuess;
		bUpperVolt=false;// need to clear this, because at this point we can't evaluate lfUpperSideDiffVolt
		lfStepVolt=-fabs(lfStepVolt*0.95);
		LoadVoltageGuess+=lfStepVolt;
		lfStepVolt=-LoadVoltageGuess*0.001;
		bLowerVolt=false;
		continue;
		}
	else if(zz==MUST_HAVE_ELECTROLYSIS_CONDITIONS) {
		// possibly attemptng electrolysis--not considered here
		return(MUST_HAVE_ELECTROLYSIS_CONDITIONS);
		}

	double lfTotalCurrent=GetTotalCurrent();
	double LoadCurrentExpected=LoadVoltageGuess/m_lfLoadResistance;
	double Diff=LoadCurrentExpected-lfTotalCurrent;
	double AbsDiff=fabs(Diff/LoadCurrentExpected);
	if(AbsDiff<CURRENT_PRECISION) {
		break;
		}
	if(lfPtBVolt>0.0){
		if(Diff>0.0){
			nUpper++;
			nLower=1;
			lfUpperVolt=(1.0*LoadVoltageGuess+0.0*lfUpperVolt);
			lfUpperSideDiffVolt=Diff;
			}
		else{
			nLower++;
			nUpper=1;
			lfLowerVolt=(1.0*LoadVoltageGuess+0.0*lfLowerVolt);
			lfLowerSideDiffVolt=Diff;
			}
		lfPtAVolt=LoadVoltageGuess;
		lfPtADiff=Diff;
		lfStepVolt=(lfPtBVolt-lfPtAVolt)/(lfPtBDiff-lfPtADiff)*(-lfPtADiff);
		lfPtBVolt=LoadVoltageGuess;
		lfPtBDiff=Diff;
		if(fabs(lfStepVolt)/LoadVoltageGuess<0.0000001){
			lfStepVolt=LoadVoltageGuess/1000.0;
			}
		if(lfLowerVolt/lfUpperVolt>0.99999999){
			lfLowerVolt*=0.99;
			lfUpperVolt*=1.01;
			bDoHoneIn=true;
			}
		if(nLower>7){
			lfUpperVolt*=1.1;
			bDoHoneIn=true;
			}
		if(nUpper>7){
			lfLowerVolt*=0.9;
			bDoHoneIn=true;
			}
		double lfAve=(lfLowerVolt*(double)nUpper+lfUpperVolt*(double)nLower)/((double)(nUpper+nLower));
		if(lfStepVolt<0.0 && LoadVoltageGuess+lfStepVolt<(lfAve)){
			LoadVoltageGuess=lfAve;
			}
		else if(lfStepVolt>0.0 && LoadVoltageGuess+lfStepVolt>(lfAve)){
			LoadVoltageGuess=lfAve;
			}
		else{
			LoadVoltageGuess+=lfStepVolt;
			}
		if(zz==TRY_REDUCED_VOLTAGE || LoadVoltageGuess>lfUpperVolt || LoadVoltageGuess<lfLowerVolt){
			LoadVoltageGuess=(lfUpperVolt+lfLowerVolt)/2.0;
			}
		}
	else {
		lfPtBVolt=LoadVoltageGuess;
		lfPtBDiff=Diff;
		if(Diff>0.0){
			bUpperVolt=true;
			lfUpperVolt=LoadVoltageGuess;
			lfUpperSideDiffVolt=Diff;
			}
		else {
			bLowerVolt=true;
			lfLowerVolt=LoadVoltageGuess;
			lfLowerSideDiffVolt=Diff;
			}
		if(Diff>0.0 && lfStepVolt>0.0){// need to go to smaller voltage so that expected current is less and calc. current through cell is more
			lfStepVolt*=-0.3333;
			}
		else if(Diff<0.0 && lfStepVolt<0.0){
			lfStepVolt*=-0.33333;
			}
		else {
			lfStepVolt*=2.0;
			}
		if(LoadVoltageGuess+lfStepVolt<0.0){
			lfStepVolt=-LoadVoltageGuess/2.0;
			}
		LoadVoltageGuess+=lfStepVolt;
		}
	}
return(GOOD_RESULT);
}

int CElectrolyte::AnalyzeCurrDenGivenCurrent_GasQS(double lfCellTotalCurrent_Amp, bool bHoneIn)
{
// In this analysis, you are given CURRENT on some load resistance.  Then crunch through
// to calculate the resultant CONSTANT VOLTAGE ASSUMPTION ON CELL through all nodes...
// This routine cannot handle negative current flow--that is electrolysis!!!
if(lfCellTotalCurrent_Amp<0.0){
	printf("ERR AnalCurrDenGivenCurr\n");
	}

double AnPExitSave=An->P[m_nNoNodes-1];
double lfAnStep=An->Vel[CUR][0]/10000.0;
bool bAnUpperBound=false;
bool bAnLowerBound=false;
double lfAnLower=-1.0,lfAnUpper=-1.0,lfAnLowerSideDelForce=-1.0,lfAnUpperSideDelForce=-1.0;

double CaPExitSave=Ca->P[m_nNoNodes-1];
double lfCaStep=Ca->Vel[CUR][0]/10000.0;
bool bCaUpperBound=false;
bool bCaLowerBound=false;
double lfCaLower=-1.0,lfCaUpper=-1.0,lfCaLowerSideDelForce=-1.0,lfCaUpperSideDelForce=-1.0;

double lfOldAnVel=-1.0;
double lfOldAnDelForce=1.0;

double lfOldCaVel=-1.0;
double lfOldCaDelForce=1.0;

int i;
bool bFirstPass=true;
bool bDoFlowBoostForENernst=true;

double lfCellTotalCurrent_AmpSaved=lfCellTotalCurrent_Amp;
double ActiveArea=m_ChanGeom.lfChannelLength*m_lfCellWidth;
bool bKeepCurrentProfileWhereItIs=false;


while(1){// iterate to get Vel at inlet by balancing overall friction with pressure drop...
	lfCellTotalCurrent_Amp=lfCellTotalCurrent_AmpSaved;
	double lfMaxTotalCurr=1.0;
	lfMaxTotalCurr=An->GetLimitingSupplyCurrentDensity(1)*DelX[1]*m_lfCellWidth;
	if(Ca->GetLimitingSupplyCurrentDensity(1)*DelX[1]*m_lfCellWidth<lfMaxTotalCurr){
		lfMaxTotalCurr=Ca->GetLimitingSupplyCurrentDensity(1)*DelX[1]*m_lfCellWidth;
		}
	if(lfCellTotalCurrent_Amp>lfMaxTotalCurr){
		lfCellTotalCurrent_Amp=0.8*lfMaxTotalCurr;//force a max total current loading of 80% utilization
		}
	double lfSumCurr=0.0;
	for(i=1;i<m_nNoNodes-1;i++){
		if(lfCurrentDensity[i]<=0.0){
			lfCurrentDensity[i]=lfCellTotalCurrent_Amp/(m_nNoNodes-2)/ActiveArea;
			}
		lfSumCurr+=lfCurrentDensity[i]*DelX[i]*m_lfCellWidth;
		}
	for(i=1;i<m_nNoNodes-1;i++){
		lfCurrentDensity[i]*=lfCellTotalCurrent_Amp/(lfSumCurr+0.00000000000001);
		}
	bool bAnHitAbsoluteLowerVelLimit=false;
	bool bCaHitAbsoluteLowerVelLimit=false;
	bool bAnOverShootingDelP=false;
	bool bCaOverShootingDelP=false;
	int nCnt=0;
	double Ti;
	while(1){
		for(i=1;i<m_nNoNodes-1;i++){
			Ti=m_lfTemp[CUR][i];//*0.2+0.8*Ti;
			while(An->AnalyzeNodeQSGasPhysics(i, lfCurrentDensity[i])==QS_LOWER_CURRENT ||	lfCurrentDensity[i]>=An->GetAnodeLimitingCurrentDensity(i, An->P[i],Ti)){
				return(0);
				}
			bool bUpdatedCurr=false;
			while(Ca->AnalyzeNodeQSGasPhysics(i, lfCurrentDensity[i])==QS_LOWER_CURRENT || lfCurrentDensity[i]>=Ca->GetCathodeLimitingCurrentDensity(i, Ca->P[i], Ti)){
				return(0);
				}
			if(bUpdatedCurr){
				An->AnalyzeNodeQSGasPhysics(i, lfCurrentDensity[i]);
				}
			if(An->HitPDropLimit(i,AnPExitSave) && !An->bFixedFlow()){
				if(lfAnUpper>An->Vel[CUR][0]){
					lfAnUpper=An->Vel[CUR][0];
					}
				bAnUpperBound=true;
				bAnOverShootingDelP=true;
				}
			if(Ca->HitPDropLimit(i,CaPExitSave) && !Ca->bFixedFlow()){
				if(lfCaUpper>Ca->Vel[CUR][0]){
					lfCaUpper=Ca->Vel[CUR][0];
					}
				bCaUpperBound=true;
				bCaOverShootingDelP=true;
				}
			if(bCaOverShootingDelP||bAnOverShootingDelP) {
				goto FLOWADJUST;
				}
			}
	
		double lfAveCellVoltage=0.0;
		double logArg[MAX_AXIALNODES];
		double lfCellVoltage[MAX_AXIALNODES];
		double lfEffectiveInternalRes[MAX_AXIALNODES];
		double ENernst;
		for(i=1;i<m_nNoNodes-1;i++){
			double lfMinCurr=MIN_CURLIMIT;
			double lfMaxCurr=-1000000.0;

			Ti=m_lfTemp[CUR][i];//*0.2+0.8*Ti;
			double MolarDelHStd=GetMolarDelHStd(Ti);//joule/kg-mole
			double MolarDelSStd=GetMolarDelSStd(Ti);// per kg-mole of H2! without effects of mixing and at std. conditions!!
			double MolarDelGStd=MolarDelHStd-Ti*MolarDelSStd;//delG per kg-mole of H2
			MolarDelGStd/=1000.0;//delG per g-mole of H2

			double AnodeP;
			double CathodeP;
			AnodeP=An->P[i];
			CathodeP=Ca->P[i];

			double ReactantActivity=GetReactantActivity(i,AnodeP,CathodeP);
			double ProductActivity=GetProductActivity(i,AnodeP,CathodeP);
			logArg[i]=ReactantActivity/ProductActivity;
			ENernst=-MolarDelGStd/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;
			ENernst+=Runiv*Ti*log(logArg[i])/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;
			double TotalEntropyChange=MolarDelSStd/1000.0+Runiv*log(logArg[i]);//joule/gm-moleH2/K
			if(ENernst<0.0){
				printf("Can't do electolysis\n");
				An->P[m_nNoNodes-1]=AnPExitSave;
				Ca->P[m_nNoNodes-1]=CaPExitSave;
				return(MUST_HAVE_ELECTROLYSIS_CONDITIONS);
				}
			double CathodeEtaConc=Ca->GetCathodeConcLoss(Ti,CathodeP,i,lfCurrentDensity[i]);
			double AnodeEtaConc=An->GetAnodeConcLoss(Ti,AnodeP,i,lfCurrentDensity[i]);
			double CathodeEtaActivation=GetCathodeActivationLoss(Ti,lfCurrentDensity[i]);
			double AnodeEtaActivation=GetAnodeActivationLoss(Ti,lfCurrentDensity[i]);
			double lfTotalCellResistanceOhmM2=GetCellNodeResistance(i,Ti);

			double TotalOutputVoltage;
			TotalOutputVoltage=0.0;
			TotalOutputVoltage+=ENernst;
			TotalOutputVoltage-=(CathodeEtaActivation-CathodeEtaConc);
			TotalOutputVoltage-=(AnodeEtaActivation-AnodeEtaConc);
			TotalOutputVoltage-=lfCurrentDensity[i]*lfTotalCellResistanceOhmM2;
			m_lfLoadVoltage=TotalOutputVoltage;
			if(TotalOutputVoltage<0.0) {
				TotalOutputVoltage=0.000000001;
				lfMaxCurr=lfCurrentDensity[i];
				lfCurrentDensity[i]=(lfMinCurr+lfMaxCurr)/2.0;
				continue;
				}
			lfCellVoltage[i]=TotalOutputVoltage;// update this so the Qgen function will be using what the current system provides...
			lfEffectiveInternalRes[i]=(CathodeEtaActivation-CathodeEtaConc+AnodeEtaActivation-AnodeEtaConc)/(lfCurrentDensity[i]+0.00000001)+lfTotalCellResistanceOhmM2;
			lfAveCellVoltage+=lfCellVoltage[i];
			lfEntropyHeatGen[i]=-m_lfTemp[CUR][i]*TotalEntropyChange*lfCurrentDensity[i]*DelX[i]*m_lfCellWidth/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;//watts
			m_lfNodalIdealVoltage[i]=ENernst;
			m_lfNodalIdealVoltage_AtElectrolyteSurface[i]=ENernst+CathodeEtaConc+AnodeEtaConc;//these etaConc losses are negative valued...
			}
		lfAveCellVoltage/=(double)(m_nNoNodes-2);
		if(bKeepCurrentProfileWhereItIs || lfCellTotalCurrent_Amp<0.01) {
			break;
			}

		double lfSumCurrDeltas=0.0;
		double lfSumTotalCurrDen=0.0;
		double lfSumTotalCurr=0.0;
		double lfAbsSumTotalCurrCorr=0.0;
		double lfCurrentCorrection[MAX_AXIALNODES];
		for(i=1;i<m_nNoNodes-1;i++){
//			double O2Conc=Ca->Sp[CUR][i][O2];//will mole fractions work here?
//			double H2Conc=An->Sp[CUR][i][H2]+0.000000001;
//			double H2OConc=An->Sp[CUR][i][H2O]+0.000000001;
//			double dRdI=-sqrt(O2Conc)/m_lfGmMoleElectronsPerGmMoleH2-0.5*H2Conc/sqrt(O2Conc)/m_lfGmMoleElectronsPerGmMoleO2;//deriv of React term wrt current
//			double dPdI=1.0/m_lfGmMoleElectronsPerGmMoleH2;//derivative of Product term wrt current
//			double lfDelta_logArg=lfSumCurrDeltas/lfSumTotalCurr*(dRdI/H2OConc-logArg[i]/H2OConc*dPdI);//the delta of the Arg term due to sum of upstream current changes
//			double dNernstdLogArg=0.0;//Runiv*Ti/m_lfGmMoleElectronsPerGmMoleH2/FARADAY/logArg[i];//derivative of nernst with resp. to Arg of log term
			//calc the change in local Nernst due to upstream changes in current (this will cause change in local current)
//			double delI_sp=0.0;
//			if(i>1) delI_sp=(dNernstdLogArg*lfDelta_logArg)/lfEffectiveInternalRes[i];
			//calc the local change in voltage due to local change in current (node's V-I)
			double delI_V =(lfCellVoltage[i]-lfAveCellVoltage)/lfEffectiveInternalRes[i];//could modify to include resistance effects of other overpotentials too
//			TRACE2("   %le,%le, ",delI_V,delI_sp);
//			TRACE2("%le,%le, ",dRdI,dPdI);
//			TRACE2("%le,%le\n",dNernstdLogArg,lfDelta_logArg);
			lfCurrentCorrection[i]=delI_V;//+delI_sp;
			if(lfCurrentDensity[i]+lfCurrentCorrection[i]<0.0) lfCurrentCorrection[i]=-0.9*lfCurrentDensity[i];
			lfCurrentDensity[i]=lfCurrentDensity[i]*0.95+0.05*lfCurrentCorrection[i];
			lfSumTotalCurrDen+=lfCurrentDensity[i];
			lfSumTotalCurr+=lfCurrentDensity[i]*DelX[i]*m_lfCellWidth;
			lfAbsSumTotalCurrCorr+=fabs(lfCurrentCorrection[i]);
			lfSumCurrDeltas+=lfCurrentCorrection[i]*DelX[i]*m_lfCellWidth;
			}
//		TRACE2(" %le,%le  ",lfAveCellVoltage,lfSumTotalCurrDen);
//		for(i=1;i<m_nNoNodes-1;i++){
//			TRACE1(",%le",(lfCellVoltage[i]-lfAveCellVoltage));
//			}

//		printf("\n");
//		printf("\n");
		for(i=1;i<m_nNoNodes-1;i++){
			lfCurrentDensity[i]*=lfCellTotalCurrent_Amp/lfSumTotalCurr;//renormalize current to total current
			}
		if(lfAbsSumTotalCurrCorr/lfCellTotalCurrent_Amp/ActiveArea<0.005) {
			//convergence is good!
			break;
			}
		nCnt++;
		if(lfCellTotalCurrent_Amp/ActiveArea/10000.0<0.01 && nCnt>100){
			//if not pulling much current, soften the accuracy requirement
			bKeepCurrentProfileWhereItIs=true;
			break;
			}
		}

FLOWADJUST:

	if(i==m_nNoNodes-1){
		bHoneIn=true;
		}

	if(bAnOverShootingDelP){
		if(bAnHitAbsoluteLowerVelLimit){
			bDoFlowBoostForENernst=false;
			printf("We apparently have requested to high a voltage than what can be provided given the Specified DelP and Load Resistance\n");
//			An->P[m_nNoNodes-1]=AnPExitSave;
//			Ca->P[m_nNoNodes-1]=CaPExitSave;
//			return(TRY_REDUCED_VOLTAGE);
			}
		}
	else {
		An->AnalyzeNodeQSGasPhysics(i, 0.0);
		}
	if(bCaOverShootingDelP){
		if(bCaHitAbsoluteLowerVelLimit){
			bDoFlowBoostForENernst=false;
			printf("We apparently have requested to high a voltage than what can be provided given the Specified DelP and Load Resistance\n");
//			An->P[m_nNoNodes-1]=AnPExitSave;
//			Ca->P[m_nNoNodes-1]=CaPExitSave;
//			return(TRY_REDUCED_VOLTAGE);
			}
		}
	else {
		Ca->AnalyzeNodeQSGasPhysics(i, 0.0);
		}

	double AnDelForce=(An->P[m_nNoNodes-1] - AnPExitSave)/(AnPExitSave+0.0000001);
	double CaDelForce=(Ca->P[m_nNoNodes-1] - CaPExitSave)/(CaPExitSave+0.0000001);
	double AnAbsdelForce=fabs(AnDelForce);
	double CaAbsdelForce=fabs(CaDelForce);
	if(AnAbsdelForce<0.00000001 && CaAbsdelForce<0.00000001 && !bAnOverShootingDelP && !bCaOverShootingDelP) {
		break;
		}
	if(An->bFixedFlow()){
		AnPExitSave=An->P[m_nNoNodes-1];
		}
	if(Ca->bFixedFlow()){
		CaPExitSave=Ca->P[m_nNoNodes-1];
		}

	if(!An->bFixedFlow()){//Do Anode Update....
		if(bAnUpperBound&&bAnLowerBound && i==(m_nNoNodes-1)){
			if(AnDelForce>0.0){
				if(An->Vel[CUR][0]>lfAnLower){
					lfAnLower=An->Vel[CUR][0];
					}
				}
			else {
				if(An->Vel[CUR][0]<lfAnUpper){
					lfAnUpper=An->Vel[CUR][0];
					}
				}
			lfAnStep=-(An->Vel[CUR][0]-lfOldAnVel)/(AnDelForce-lfOldAnDelForce)*AnDelForce/1.1;
			if(fabs(lfAnStep)<1.0E-20){
				lfAnStep=lfOldAnVel/1.0E5;
				lfAnUpper+=fabs(lfAnStep);
				lfAnLower-=fabs(lfAnStep);
				}
			lfOldAnVel=An->Vel[CUR][0];
			lfOldAnDelForce=AnDelForce;
			An->Vel[CUR][0]+=lfAnStep;
			if(An->Vel[CUR][0]>lfAnUpper){
				An->Vel[CUR][0]=(An->Vel[CUR][0]*0.3+0.7*lfAnUpper);
				lfAnUpper=An->Vel[CUR][0];
				}
			if(An->Vel[CUR][0]<lfAnLower){
				An->Vel[CUR][0]=(An->Vel[CUR][0]*0.3+0.7*lfAnLower);
				lfAnLower=An->Vel[CUR][0];
				}
			if(An->Vel[CUR][0]<0.0){
				An->Vel[CUR][0]=lfAnLower/2.0;
				}
		
			lfAnStep=(lfAnUpper-lfAnLower)/5.0;
			}
		else {
			if(bAnOverShootingDelP){
				lfAnUpperSideDelForce=AnDelForce;
				bAnUpperBound=true;
				lfAnUpper=An->Vel[CUR][0];
				if(bAnLowerBound){
					lfAnStep=-An->Vel[CUR][0]+(An->Vel[CUR][0]+lfAnLower)/2.0;
					}
				else {
					lfAnStep=-0.25*An->Vel[CUR][0];
					}
				An->Vel[CUR][0]+=lfAnStep;
				}
			else if(i==(m_nNoNodes-1)){
				if(AnDelForce<0.0){
					lfAnUpperSideDelForce=AnDelForce;
					bAnUpperBound=true;
					lfAnUpper=An->Vel[CUR][0];
					}
				else {
					lfAnLowerSideDelForce=AnDelForce;
					bAnLowerBound=true;
					lfAnLower=An->Vel[CUR][0];
					}
				if(lfOldAnVel>0.0){
					lfAnStep=-(An->Vel[CUR][0]-lfOldAnVel)/(AnDelForce-lfOldAnDelForce)*AnDelForce/1.2;
					}
				else {
					if(AnDelForce<0.0 && lfAnStep>0.0){
						lfAnStep*=-0.33333;
						}
					else if(AnDelForce>0.0 && lfAnStep<0.0){
						lfAnStep*=-0.3333;
						}
					else {
						if(fabs(lfAnStep*5.0)<An->Vel[CUR][0]){
							lfAnStep*=2.0;
							}
						else{
							lfAnStep=fabs(lfAnStep)/lfAnStep*An->Vel[CUR][0]/5.0;
							}
						}
					}
				lfOldAnVel=An->Vel[CUR][0];
				lfOldAnDelForce=AnDelForce;
				An->Vel[CUR][0]+=lfAnStep;
				}
			}
		}//End of Anode Check

	if(!Ca->bFixedFlow()){//Do Cathode Update....
		if(bCaUpperBound&&bCaLowerBound && i==(m_nNoNodes-1)){
			if(CaDelForce>0.0){
				if(Ca->Vel[CUR][0]>lfCaLower){
					lfCaLower=Ca->Vel[CUR][0];
					}
				}
			else {
				if(Ca->Vel[CUR][0]<lfCaUpper){
					lfCaUpper=Ca->Vel[CUR][0];
					}
				}
			lfCaStep=-(Ca->Vel[CUR][0]-lfOldCaVel)/(CaDelForce-lfOldCaDelForce)*CaDelForce/1.1;
			if(fabs(lfCaStep)<1.0E-20){
				lfCaStep=lfOldCaVel/1.0E5;
				lfCaUpper+=fabs(lfCaStep);
				lfCaLower-=fabs(lfCaStep);
				}
			lfOldCaVel=Ca->Vel[CUR][0];
			lfOldCaDelForce=CaDelForce;
			Ca->Vel[CUR][0]+=lfCaStep;
			if(Ca->Vel[CUR][0]>lfCaUpper){
				Ca->Vel[CUR][0]=(Ca->Vel[CUR][0]*0.3+0.7*lfCaUpper);
				lfCaUpper=Ca->Vel[CUR][0];
				}
			if(Ca->Vel[CUR][0]<lfCaLower){
				Ca->Vel[CUR][0]=(Ca->Vel[CUR][0]*0.3+0.7*lfCaLower);
				lfCaLower=Ca->Vel[CUR][0];
				}
			if(Ca->Vel[CUR][0]<0.0){
				Ca->Vel[CUR][0]=lfCaLower/2.0;
				}
		
			lfCaStep=(lfCaUpper-lfCaLower)/5.0;
			}
		else {
			if(bCaOverShootingDelP){
				lfCaUpperSideDelForce=CaDelForce;
				bCaUpperBound=true;
				lfCaUpper=Ca->Vel[CUR][0];
				if(bAnLowerBound){
					lfCaStep=-Ca->Vel[CUR][0]+(Ca->Vel[CUR][0]+lfCaLower)/2.0;
					}
				else {
					lfCaStep=-0.25*Ca->Vel[CUR][0];
					}
				Ca->Vel[CUR][0]+=lfCaStep;
				}
			else if(i==(m_nNoNodes-1)){
				if(CaDelForce<0.0){
					lfCaUpperSideDelForce=CaDelForce;
					bCaUpperBound=true;
					lfCaUpper=Ca->Vel[CUR][0];
					}
				else{
					lfCaLowerSideDelForce=CaDelForce;
					bCaLowerBound=true;
					lfCaLower=Ca->Vel[CUR][0];
					}
				if(lfOldCaVel>0.0){
					lfCaStep=-(Ca->Vel[CUR][0]-lfOldCaVel)/(CaDelForce-lfOldCaDelForce)*CaDelForce/1.2;
					}
				else {
					if(CaDelForce<0.0 && lfCaStep>0.0){
						lfCaStep*=-0.33333;
						}
					else if(CaDelForce>0.0 && lfCaStep<0.0){
						lfCaStep*=-0.3333;
						}
					else {
						if(fabs(lfCaStep*5.0)<Ca->Vel[CUR][0]){
							lfCaStep*=2.0;
							}
						else{
							lfCaStep=fabs(lfCaStep)/lfCaStep*Ca->Vel[CUR][0]/5.0;
							}
						}
					}
				lfOldCaVel=Ca->Vel[CUR][0];
				lfOldCaDelForce=CaDelForce;
				Ca->Vel[CUR][0]+=lfCaStep;
				}
			}
		}//End of Cathode Check
	bFirstPass=false;
	}

lfCurrentDensity[m_nNoNodes-1]=0.0;
lfCurrentDensity[0]=0.0;
An->Vel[NXT][0]=An->Vel[CUR][0];
Ca->Vel[NXT][0]=Ca->Vel[CUR][0];
for(i=0;i<An->m_nNoSp;i++){
	An->Sp[NXT][0][i]=An->Sp[CUR][0][i];
	}
for(i=0;i<Ca->m_nNoSp;i++){
	Ca->Sp[NXT][0][i]=Ca->Sp[CUR][0][i];
	}

An->P[m_nNoNodes-1]=AnPExitSave;
Ca->P[m_nNoNodes-1]=CaPExitSave;

AnalyzeOveallLosses(Ca->P, An->P);
	
return(GOOD_RESULT);
}

int CElectrolyte::AnalyzeCurrDenGivenCurrent_FullyQS(double lfCellTotalCurrent_Amp, CSeparatorPlate &SP, bool bDoQSSolidTempUpdate, bool bHoneIn)
{
	
return(GOOD_RESULT);
}

int CElectrolyte::AnalyzeCurrDenGivenVoltage_FullyQS(double lfCellVoltage, CSeparatorPlate &SP, bool bDoQSSolidTempUpdate, bool bHoneIn)
{
// In this analysis, you are given voltage on some load resistance.  Then crunch through
// to calculate the resultant current through all nodes...
// This routine cannot handle negative current flow--that is electrolysis!!!
if(lfCellVoltage<0.0){
	printf("ERR AnalCurrDenGivenVolts\n");
	}

double lfTargetVoltage=lfCellVoltage;
m_lfLoadVoltage=lfCellVoltage;// initialize for Qgen routine

double AnPExitSave=An->P[m_nNoNodes-1];
double lfAnStep=An->Vel[CUR][0]/10000.0;
bool bAnUpperBound=false;
bool bAnLowerBound=false;
double lfAnLower=-1.0,lfAnUpper=-1.0,lfAnLowerSideDelForce=-1.0,lfAnUpperSideDelForce=-1.0;

double CaPExitSave=Ca->P[m_nNoNodes-1];
double lfCaStep=Ca->Vel[CUR][0]/10000.0;
bool bCaUpperBound=false;
bool bCaLowerBound=false;
double lfCaLower=-1.0,lfCaUpper=-1.0,lfCaLowerSideDelForce=-1.0,lfCaUpperSideDelForce=-1.0;

double lfUpperCurrDiffVolt=1.0;
double lfLowerCurrDiffVolt=1.0;
double lfOldAnVel=-1.0;
double lfOldAnDelForce=1.0;

double lfOldCaVel=-1.0;
double lfOldCaDelForce=1.0;

int i;
bool bFirstPass=true;
bool bDoFlowBoostForENernst=true;

STARTOFLOOP:

while(1){// iterate to get Vel at inlet by balancing overall friction with pressure drop...
	bool bAnHitAbsoluteLowerVelLimit=false;
	bool bCaHitAbsoluteLowerVelLimit=false;
	bool bAnOverShootingDelP=false;
	bool bCaOverShootingDelP=false;
	for(i=1;i<m_nNoNodes-1;i++){

		double lfCurrentDensityGuess=0.1*100.0*100.0;//ampere/m2
		if(lfCurrentDensity[i]>0.0){
			lfCurrentDensityGuess=lfCurrentDensity[i];
			}
		if(i>1 && bFirstPass){
			double lfTst=lfCurrentDensity[i-1]/lfCurrentDensityGuess;
			if(lfTst>30.0 || lfTst<0.03333){
				lfCurrentDensityGuess=lfCurrentDensity[i-1];
				}
			}
		double lfStep=CURRENT_PRECISION*lfCurrentDensityGuess*10.0;

		bool bUpperBound=false;
		bool bLowerBound=false;
		double lfUpper=-10.0;
		double lfLower=-10.0;

		double TotalEntropyChange=0.0;
		double ENernst=0.0;
		double CathodeEtaConc=0.0;
		double AnodeEtaConc=0.0;
		int nConvergeCnt=0;

		bool bReady=false;
		double lfMinCurr=MIN_CURLIMIT;
		double lfMaxCurr=-1000000.0;
		double lfAnodeLimitingSupplyCurrentDensity=An->GetLimitingSupplyCurrentDensity(i);
		lfMaxCurr=lfAnodeLimitingSupplyCurrentDensity;
		double lfCathodeLimitingSupplyCurrentDensity=Ca->GetLimitingSupplyCurrentDensity(i);
		if(lfCathodeLimitingSupplyCurrentDensity<lfMaxCurr){
			lfMaxCurr=lfCathodeLimitingSupplyCurrentDensity;
			}
		if(lfCurrentDensityGuess>lfAnodeLimitingSupplyCurrentDensity){
			lfCurrentDensityGuess=lfAnodeLimitingSupplyCurrentDensity*0.99999999;
			}
		if(lfCurrentDensityGuess>lfCathodeLimitingSupplyCurrentDensity){
			lfCurrentDensityGuess=lfCathodeLimitingSupplyCurrentDensity*0.99999999;
			}
		int nMin=1;
		int nMax=1;
		while(1){// search for the current, vels, Ps, gasTs, solidTs for given voltage
			double lfDiffCurrDiff=10.0;
			double DiffVolt;
			double ENurnstOld=ENernst;
			int nCntPass=0;
			double lfLimit=0.000001;
			double lfMinDiff=100000.0;
			double Ti=m_lfTemp[CUR][i];
			while(lfDiffCurrDiff>lfLimit){
				double AnSpH2;
				double CaSpO2;
				double AnodeP;
				double CathodeP;
				double TotalOutputVoltage;
				// don't do the following loop unless we are ready---that is, ENernst has been calc., etc.
				if(!bReady){
					while(An->AnalyzeNodeQSGasPhysics(i, lfCurrentDensityGuess)==QS_LOWER_CURRENT ||	lfCurrentDensityGuess>=An->GetAnodeLimitingCurrentDensity(i, An->P[i],Ti)){
						lfMaxCurr=lfCurrentDensityGuess;
						lfCurrentDensityGuess=(lfMinCurr+lfMaxCurr)/2.0;
						}
					bool bUpdatedCurr=false;
					while(Ca->AnalyzeNodeQSGasPhysics(i, lfCurrentDensityGuess)==QS_LOWER_CURRENT || lfCurrentDensityGuess>=Ca->GetCathodeLimitingCurrentDensity(i, Ca->P[i], Ti)){
						lfMaxCurr=lfCurrentDensityGuess;
						lfCurrentDensityGuess=(lfMinCurr+lfMaxCurr)/2.0;
						}
					if(bUpdatedCurr){
						An->AnalyzeNodeQSGasPhysics(i, lfCurrentDensityGuess);
						}
					Ti=m_lfTemp[CUR][i]*0.2+0.8*Ti;
					double MolarDelHStd=GetMolarDelHStd(Ti);//joule/kg-mole
					double MolarDelSStd=GetMolarDelSStd(Ti);// per kg-mole of H2! without effects of mixing and at std. conditions!!
					double MolarDelGStd=MolarDelHStd-Ti*MolarDelSStd;//delG per kg-mole of H2
					MolarDelGStd/=1000.0;//delG per g-mole of H2

					AnodeP=An->P[i];
					CathodeP=Ca->P[i];

					double ReactantActivity=GetReactantActivity(i,AnodeP,CathodeP);
					double ProductActivity=GetProductActivity(i,AnodeP,CathodeP);
					ENernst=-MolarDelGStd/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;
					ENernst+=Runiv*Ti*log(ReactantActivity/ProductActivity)/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;
					TotalEntropyChange=MolarDelSStd/1000.0+Runiv*log(ReactantActivity/ProductActivity);//joule/gm-moleH2/K
					if(ENernst<0.0){
						printf("Can't do electolysis\n");
						An->P[m_nNoNodes-1]=AnPExitSave;
						Ca->P[m_nNoNodes-1]=CaPExitSave;
						return(MUST_HAVE_ELECTROLYSIS_CONDITIONS);
						}
					double CathodeEtaActivation=GetCathodeActivationLoss(Ti,lfCurrentDensityGuess);
					double AnodeEtaActivation=GetAnodeActivationLoss(Ti,lfCurrentDensityGuess);
					double lfTotalCellResistanceOhmM2=GetCellNodeResistance(i,Ti);

					TotalOutputVoltage=0.0;
					TotalOutputVoltage+=ENernst;
					TotalOutputVoltage-=(CathodeEtaActivation-CathodeEtaConc);
					TotalOutputVoltage-=(AnodeEtaActivation-AnodeEtaConc);
					TotalOutputVoltage-=lfCurrentDensityGuess*lfTotalCellResistanceOhmM2;
					if(TotalOutputVoltage<0.0) {
						TotalOutputVoltage=0.000000001;
						lfMaxCurr=lfCurrentDensityGuess;
						lfCurrentDensityGuess=(lfMinCurr+lfMaxCurr)/2.0;
						continue;
						}
					m_lfLoadVoltage=TotalOutputVoltage;// update this so the Qgen function will be using what the current system provides...
					}
				else {
					do{
						//first update the gas phase
						int cnt=0;
						while(An->AnalyzeNodeQSGasPhysics(i, lfCurrentDensityGuess)==QS_LOWER_CURRENT ||	lfCurrentDensityGuess>=An->GetAnodeLimitingCurrentDensity(i, An->P[i],Ti)){
							lfMaxCurr=lfCurrentDensityGuess;
							cnt++;
							if(cnt>100){
								lfMinCurr*=0.99;
								}
							lfCurrentDensityGuess=(lfMinCurr+lfMaxCurr)/2.0;
							lfStep=(lfMaxCurr-lfMinCurr)/100.0;
							}
						bool bUpdatedCurr=false;
						cnt=0;
						while(Ca->AnalyzeNodeQSGasPhysics(i, lfCurrentDensityGuess)==QS_LOWER_CURRENT || lfCurrentDensityGuess>=Ca->GetCathodeLimitingCurrentDensity(i, Ca->P[i], Ti)){
							lfMaxCurr=lfCurrentDensityGuess;
							cnt++;
							if(cnt>100){
								lfMinCurr*=0.99;
								}
							lfCurrentDensityGuess=(lfMinCurr+lfMaxCurr)/2.0;
							lfStep=(lfMaxCurr-lfMinCurr)/100.0;
							}
						if(bUpdatedCurr){
							An->AnalyzeNodeQSGasPhysics(i, lfCurrentDensityGuess);
							}
						Ti=m_lfTemp[CUR][i]*0.2+0.8*Ti;
						double MolarDelHStd=GetMolarDelHStd(Ti);//joule/kg-mole
						double MolarDelSStd=GetMolarDelSStd(Ti);// per kg-mole of H2! without effects of mixing and at std. conditions!!
						double MolarDelGStd=MolarDelHStd-Ti*MolarDelSStd;//delG per kg-mole of H2
						MolarDelGStd/=1000.0;//delG per g-mole of H2

						AnodeP=An->P[i];
						CathodeP=Ca->P[i];

						AnSpH2=An->Sp[CUR][i][H2]+0.000000001;// assume that it will never be zero....
						CaSpO2=Ca->Sp[CUR][i][O2];
						double ReactantActivity=GetReactantActivity(i,AnodeP,CathodeP);
						double ProductActivity=GetProductActivity(i,AnodeP,CathodeP);
						ENernst=-MolarDelGStd/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;
						ENernst+=Runiv*Ti*log(ReactantActivity/ProductActivity)/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;
						if(ENernst<0.0){
							printf("Can't do electolysis\n");
							An->P[m_nNoNodes-1]=AnPExitSave;
							Ca->P[m_nNoNodes-1]=CaPExitSave;
							return(MUST_HAVE_ELECTROLYSIS_CONDITIONS);
							}
						TotalEntropyChange=MolarDelSStd/1000.0+Runiv*log(ReactantActivity/ProductActivity);//joule/gm-moleH2/K
						CathodeEtaConc=Ca->GetCathodeConcLoss(Ti,CathodeP,i,lfCurrentDensityGuess);
						AnodeEtaConc=An->GetAnodeConcLoss(Ti,AnodeP,i,lfCurrentDensityGuess);
						double CathodeEtaActivation=GetCathodeActivationLoss(Ti,lfCurrentDensityGuess);
						double AnodeEtaActivation=GetAnodeActivationLoss(Ti,lfCurrentDensityGuess);
						double lfTotalCellResistanceOhmM2=GetCellNodeResistance(i,Ti);

						TotalOutputVoltage=0.0;
						TotalOutputVoltage+=ENernst;
						TotalOutputVoltage-=(CathodeEtaActivation-CathodeEtaConc);
						TotalOutputVoltage-=(AnodeEtaActivation-AnodeEtaConc);
						TotalOutputVoltage-=lfCurrentDensityGuess*lfTotalCellResistanceOhmM2;
						if(TotalOutputVoltage<0.0) {
							lfMaxCurr=lfCurrentDensityGuess;
							lfCurrentDensityGuess=(lfMinCurr+lfMaxCurr)/2.0;
							TotalOutputVoltage=0.000000001;
							continue;
							}
						m_lfLoadVoltage=TotalOutputVoltage;// update this so the Qgen function will be using what the current system provides...
						}while(!bUpdateSolidTemps(i, TotalEntropyChange, ENernst,CathodeEtaConc,AnodeEtaConc,lfCurrentDensityGuess,SP,bDoQSSolidTempUpdate));

					}
				bReady=false;

				if(lfTargetVoltage>ENernst){
					//perhaps we lost too much reactant upstream due to electrochemistry--try
					//upping the inlet velocity and set the lowerbound parameters...
					if(lfCurrentDensityGuess>MIN_CURLIMIT*1.01){
						lfMaxCurr=lfCurrentDensityGuess;
						lfCurrentDensityGuess=(lfMinCurr+lfMaxCurr)/2.0;
						continue;
						}
					else if(bDoQSSolidTempUpdate && m_lfTemp[CUR][i]>900.0){// slow up reactions on all upstream nodes by droping Tcell values...
						m_lfTemp[CUR][i]*=0.9;
						bLowerBound=false;
						bUpperBound=false;
						continue;
						}
					else if(bDoFlowBoostForENernst && (((An->Sp[CUR][0][H2]-AnSpH2)/An->Sp[CUR][0][H2]>0.90 && !An->bFixedFlow()) || ((Ca->Sp[CUR][0][O2]-CaSpO2)/Ca->Sp[CUR][0][O2]>0.90 && !Ca->bFixedFlow()))){
						// we apparently have consumed a lot of reactant.  try increaseing inlet flow if that is possible.
						// that is, there might be room to increase flow some more so as to boost voltage to the desired value...
						//!!!!!!!!!!!!  if doing molten-carbonate, may have to update this part to include CO2
						if(!An->bFixedFlow() && (An->Sp[CUR][0][H2]-AnSpH2)/An->Sp[CUR][0][H2]>(Ca->Sp[CUR][0][O2]-CaSpO2)/Ca->Sp[CUR][0][O2]){
							lfAnLower=An->Vel[CUR][0];
							bAnLowerBound=true;
							An->Vel[CUR][0]*=1.1;
							bAnHitAbsoluteLowerVelLimit=true;
							}
						else if(!Ca->bFixedFlow()){
							lfCaLower=Ca->Vel[CUR][0];
							bCaLowerBound=true;
							Ca->Vel[CUR][0]*=1.1;
							bCaHitAbsoluteLowerVelLimit=true;
							}
						}
					else {
						An->P[m_nNoNodes-1]=AnPExitSave;
						Ca->P[m_nNoNodes-1]=CaPExitSave;
						if(i==1){
							// We can't even get the inlet to work, so must be an impossible
							// situation, such as requesting a larger voltage than what can
							// theoretically be provided at the very inlet.
							// Try reducing requested voltage.
							printf("We apparently have an unrealistic situation\n");
							return(TRY_REDUCED_VOLTAGE);
							}
						else {
							// Perhaps we consumed all reactants upstream, such as occurs when
							// we don't supply enough reactant flows for the proposed voltage
							// condition which drives the current consumption to the limit.
							// Under this condition, we would get reverse current (electrolysis)
							// in the downstream nodes when frictional losses cause further
							// reduce the Reactant/Product Activities Ratio causing ENernst
							// to be reduced relative to upstream nodes.
							// This has been shown in the model, and again,
							// can occur when the reactants are so reduced that even the frictional
							// pressure drop causes reduced ENernst conditions as you go downstream
							// and the weak reactants can't support the loss in chemical potential
							// that results from pressure losses.

							// let's assume for now that this condition will only produce minor
							// levels of electrolysis, and that the current at the present node is
							// zero...  This is a potential research area for modeling.
							if(lfCurrentDensityGuess>MIN_CURLIMIT*4.0){
								printf("ERR MIN CURRENT\n");
								}
							TotalOutputVoltage=lfTargetVoltage;// force the DiffVolt 'break' below...
							break;
							// if we find that there can be significant reverse current, then
							// well need to add a ... return(MUST_HAVE_ELECTROLYSIS_CONDITIONS);
							}
						}
					goto STARTOFLOOP;
					}

				bReady=true;

				DiffVolt=TotalOutputVoltage-lfTargetVoltage;
				double AbsDiffVolt=fabs(DiffVolt);
				double AbsDiffTemp=fabs((ENernst-ENurnstOld)/ENernst);
				lfDiffCurrDiff=AbsDiffTemp;
				ENurnstOld=ENernst;
				nCntPass++;
				lfMinDiff=MIN(lfMinDiff,lfDiffCurrDiff);
				if(nCntPass>100){
					lfLimit=lfMinDiff*2.0;// back off a little on the limit
					}
				}
		
			double diffLimits=(lfMaxCurr-lfMinCurr)/lfMaxCurr;
			if(fabs(DiffVolt)<CURRENT_PRECISION/25.0 || diffLimits<0.000000001){// I think the slope of the V-I curve is such that dividing by 20 should give a similar assessment for "convergence" between V and I
				if(diffLimits<0.000000001){
					printf("CHECK THIS OUT\n");
					}
				break;
				}

			if(bHoneIn){
				if(DiffVolt>0.0){
					lfMinCurr=lfCurrentDensityGuess;
					}
				else if(DiffVolt<0.0){
					lfMaxCurr=lfCurrentDensityGuess;
					}
				if(DiffVolt>0.0 && lfStep<0.0 || DiffVolt<0.0 && lfStep>0.0){
					lfStep*=-0.22232;
					}
				else {
					lfStep*=1.8;
					}
				if(lfCurrentDensityGuess+lfStep>lfMaxCurr){
					lfStep=(lfCurrentDensityGuess+lfMaxCurr)/2.0-lfCurrentDensityGuess;
					}
				if(lfCurrentDensityGuess+lfStep<lfMinCurr){
					lfStep=(lfCurrentDensityGuess+lfMinCurr)/2.0-lfCurrentDensityGuess;
					}
				lfCurrentDensityGuess+=lfStep;
				}
			else{
				if(DiffVolt>0.0){
					nMax=1;
					nMin++;
					lfMinCurr=lfCurrentDensityGuess;
					}
				else if(DiffVolt<0.0){
					nMin=1;
					nMax++;
					lfMaxCurr=lfCurrentDensityGuess;
					}
				if(nMin>10){
					lfMaxCurr*=1.1;
					}
				if(nMax>10){
					lfMinCurr/=1.1;
					}
				lfCurrentDensityGuess=(lfMinCurr*(double)nMax+lfMaxCurr*(double)nMin)/((double)(nMax+nMin));
				}
			}
		lfCurrentDensity[i]=lfCurrentDensityGuess;
		lfEntropyHeatGen[i]=-m_lfTemp[CUR][i]*TotalEntropyChange*lfCurrentDensity[i]*DelX[i]*m_lfCellWidth/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;//watts

		m_lfNodalIdealVoltage[i]=ENernst;
		m_lfNodalIdealVoltage_AtElectrolyteSurface[i]=ENernst+CathodeEtaConc+AnodeEtaConc;//these etaConc losses are negative valued...
		m_lfTemp[NXT][i]=m_lfTemp[CUR][i];
		SP.m_lfTemp[NXT][i]=SP.m_lfTemp[CUR][i];

		if(An->HitPDropLimit(i,AnPExitSave) && !An->bFixedFlow()){
			if(lfAnUpper>An->Vel[CUR][0]){
				lfAnUpper=An->Vel[CUR][0];
				}
			bAnUpperBound=true;
			bAnOverShootingDelP=true;
			}
		if(Ca->HitPDropLimit(i,CaPExitSave) && !Ca->bFixedFlow()){
			if(lfCaUpper>Ca->Vel[CUR][0]){
				lfCaUpper=Ca->Vel[CUR][0];
				}
			bCaUpperBound=true;
			bCaOverShootingDelP=true;
			}
		if(bCaOverShootingDelP||bAnOverShootingDelP) break;
		}

	if(i==m_nNoNodes-1){
		bHoneIn=true;
		}

	if(bAnOverShootingDelP){
		if(bAnHitAbsoluteLowerVelLimit){
			bDoFlowBoostForENernst=false;
//			printf("We apparently have requested to high a voltage than what can be provided given the Specified DelP and Load Resistance\n");
//			An->P[m_nNoNodes-1]=AnPExitSave;
//			Ca->P[m_nNoNodes-1]=CaPExitSave;
//			return(TRY_REDUCED_VOLTAGE);
			}
		}
	else {
		An->AnalyzeNodeQSGasPhysics(i, 0.0);
		}
	if(bCaOverShootingDelP){
		if(bCaHitAbsoluteLowerVelLimit){
			bDoFlowBoostForENernst=false;
//			printf("We apparently have requested to high a voltage than what can be provided given the Specified DelP and Load Resistance\n");
//			An->P[m_nNoNodes-1]=AnPExitSave;
//			Ca->P[m_nNoNodes-1]=CaPExitSave;
//			return(TRY_REDUCED_VOLTAGE);
			}
		}
	else {
		Ca->AnalyzeNodeQSGasPhysics(i, 0.0);
		}

	double AnDelForce=(An->P[m_nNoNodes-1] - AnPExitSave)/(AnPExitSave+0.0000001);
	double CaDelForce=(Ca->P[m_nNoNodes-1] - CaPExitSave)/(CaPExitSave+0.0000001);
	double AnAbsdelForce=fabs(AnDelForce);
	double CaAbsdelForce=fabs(CaDelForce);
	if(AnAbsdelForce<0.000000001 && CaAbsdelForce<0.000000001 && !bAnOverShootingDelP && !bCaOverShootingDelP) {
		break;
		}
	if(An->bFixedFlow()){
		AnPExitSave=An->P[m_nNoNodes-1];
		}
	if(Ca->bFixedFlow()){
		CaPExitSave=Ca->P[m_nNoNodes-1];
		}

	if(!An->bFixedFlow()){//Do Anode Update....
		if(bAnUpperBound&&bAnLowerBound && i==(m_nNoNodes-1)){
			if(AnDelForce>0.0){
				if(An->Vel[CUR][0]>lfAnLower){
					lfAnLower=An->Vel[CUR][0];
					}
				}
			else {
				if(An->Vel[CUR][0]<lfAnUpper){
					lfAnUpper=An->Vel[CUR][0];
					}
				}
			lfAnStep=-(An->Vel[CUR][0]-lfOldAnVel)/(AnDelForce-lfOldAnDelForce)*AnDelForce/1.1;
			if(fabs(lfAnStep)<1.0E-20){
				lfAnStep=lfOldAnVel/1.0E5;
				lfAnUpper+=fabs(lfAnStep);
				lfAnLower-=fabs(lfAnStep);
				}
			lfOldAnVel=An->Vel[CUR][0];
			lfOldAnDelForce=AnDelForce;
			An->Vel[CUR][0]+=lfAnStep;
			if(An->Vel[CUR][0]>lfAnUpper){
				An->Vel[CUR][0]=(An->Vel[CUR][0]*0.3+0.7*lfAnUpper);
				lfAnUpper=An->Vel[CUR][0];
				}
			if(An->Vel[CUR][0]<lfAnLower){
				An->Vel[CUR][0]=(An->Vel[CUR][0]*0.3+0.7*lfAnLower);
				lfAnLower=An->Vel[CUR][0];
				}
			if(An->Vel[CUR][0]<0.0){
				An->Vel[CUR][0]=lfAnLower/2.0;
				}
		
			lfAnStep=(lfAnUpper-lfAnLower)/5.0;
			}
		else {
			if(bAnOverShootingDelP){
				lfAnUpperSideDelForce=AnDelForce;
				bAnUpperBound=true;
				lfAnUpper=An->Vel[CUR][0];
				if(bAnLowerBound){
					lfAnStep=-An->Vel[CUR][0]+(An->Vel[CUR][0]+lfAnLower)/2.0;
					}
				else {
					lfAnStep=-0.25*An->Vel[CUR][0];
					}
				An->Vel[CUR][0]+=lfAnStep;
				}
			else if(i==(m_nNoNodes-1)){
				if(AnDelForce<0.0){
					lfAnUpperSideDelForce=AnDelForce;
					bAnUpperBound=true;
					lfAnUpper=An->Vel[CUR][0];
					}
				else {
					lfAnLowerSideDelForce=AnDelForce;
					bAnLowerBound=true;
					lfAnLower=An->Vel[CUR][0];
					}
				if(lfOldAnVel>0.0){
					lfAnStep=-(An->Vel[CUR][0]-lfOldAnVel)/(AnDelForce-lfOldAnDelForce)*AnDelForce/1.2;
					}
				else {
					if(AnDelForce<0.0 && lfAnStep>0.0){
						lfAnStep*=-0.33333;
						}
					else if(AnDelForce>0.0 && lfAnStep<0.0){
						lfAnStep*=-0.3333;
						}
					else {
						if(fabs(lfAnStep*5.0)<An->Vel[CUR][0]){
							lfAnStep*=2.0;
							}
						else{
							lfAnStep=fabs(lfAnStep)/lfAnStep*An->Vel[CUR][0]/5.0;
							}
						}
					}
				lfOldAnVel=An->Vel[CUR][0];
				lfOldAnDelForce=AnDelForce;
				An->Vel[CUR][0]+=lfAnStep;
				}
			}
		}//End of Anode Check

	if(!Ca->bFixedFlow()){//Do Cathode Update....
		if(bCaUpperBound&&bCaLowerBound && i==(m_nNoNodes-1)){
			if(CaDelForce>0.0){
				if(Ca->Vel[CUR][0]>lfCaLower){
					lfCaLower=Ca->Vel[CUR][0];
					}
				}
			else {
				if(Ca->Vel[CUR][0]<lfCaUpper){
					lfCaUpper=Ca->Vel[CUR][0];
					}
				}
			lfCaStep=-(Ca->Vel[CUR][0]-lfOldCaVel)/(CaDelForce-lfOldCaDelForce)*CaDelForce/1.1;
			if(fabs(lfCaStep)<1.0E-20){
				lfCaStep=lfOldCaVel/1.0E5;
				lfCaUpper+=fabs(lfCaStep);
				lfCaLower-=fabs(lfCaStep);
				}
			lfOldCaVel=Ca->Vel[CUR][0];
			lfOldCaDelForce=CaDelForce;
			Ca->Vel[CUR][0]+=lfCaStep;
			if(Ca->Vel[CUR][0]>lfCaUpper){
				Ca->Vel[CUR][0]=(Ca->Vel[CUR][0]*0.3+0.7*lfCaUpper);
				lfCaUpper=Ca->Vel[CUR][0];
				}
			if(Ca->Vel[CUR][0]<lfCaLower){
				Ca->Vel[CUR][0]=(Ca->Vel[CUR][0]*0.3+0.7*lfCaLower);
				lfCaLower=Ca->Vel[CUR][0];
				}
			if(Ca->Vel[CUR][0]<0.0){
				Ca->Vel[CUR][0]=lfCaLower/2.0;
				}
		
			lfCaStep=(lfCaUpper-lfCaLower)/5.0;
			}
		else {
			if(bCaOverShootingDelP){
				lfCaUpperSideDelForce=CaDelForce;
				bCaUpperBound=true;
				lfCaUpper=Ca->Vel[CUR][0];
				if(bAnLowerBound){
					lfCaStep=-Ca->Vel[CUR][0]+(Ca->Vel[CUR][0]+lfCaLower)/2.0;
					}
				else {
					lfCaStep=-0.25*Ca->Vel[CUR][0];
					}
				Ca->Vel[CUR][0]+=lfCaStep;
				}
			else if(i==(m_nNoNodes-1)){
				if(CaDelForce<0.0){
					lfCaUpperSideDelForce=CaDelForce;
					bCaUpperBound=true;
					lfCaUpper=Ca->Vel[CUR][0];
					}
				else{
					lfCaLowerSideDelForce=CaDelForce;
					bCaLowerBound=true;
					lfCaLower=Ca->Vel[CUR][0];
					}
				if(lfOldCaVel>0.0){
					lfCaStep=-(Ca->Vel[CUR][0]-lfOldCaVel)/(CaDelForce-lfOldCaDelForce)*CaDelForce/1.2;
					}
				else {
					if(CaDelForce<0.0 && lfCaStep>0.0){
						lfCaStep*=-0.33333;
						}
					else if(CaDelForce>0.0 && lfCaStep<0.0){
						lfCaStep*=-0.3333;
						}
					else {
						if(fabs(lfCaStep*5.0)<Ca->Vel[CUR][0]){
							lfCaStep*=2.0;
							}
						else{
							lfCaStep=fabs(lfCaStep)/lfCaStep*Ca->Vel[CUR][0]/5.0;
							}
						}
					}
				lfOldCaVel=Ca->Vel[CUR][0];
				lfOldCaDelForce=CaDelForce;
				Ca->Vel[CUR][0]+=lfCaStep;
				}
			}
		}//End of Cathode Check
	bFirstPass=false;
	}

m_lfLoadVoltage=lfTargetVoltage;// let's make sure things are exactly produced

lfCurrentDensity[m_nNoNodes-1]=0.0;
lfCurrentDensity[0]=0.0;
An->Vel[NXT][0]=An->Vel[CUR][0];
Ca->Vel[NXT][0]=Ca->Vel[CUR][0];
for(i=0;i<An->m_nNoSp;i++){
	An->Sp[NXT][0][i]=An->Sp[CUR][0][i];
	}
for(i=0;i<Ca->m_nNoSp;i++){
	Ca->Sp[NXT][0][i]=Ca->Sp[CUR][0][i];
	}

An->P[m_nNoNodes-1]=AnPExitSave;
Ca->P[m_nNoNodes-1]=CaPExitSave;

AnalyzeOveallLosses(Ca->P, An->P);
	
return(GOOD_RESULT);
}

bool CElectrolyte::bUpdateSolidTemps(int i, double TotalEntropyChange, double ENernst,double CathodeEtaConc,double AnodeEtaConc,double lfCurrentDensityGuess, CSeparatorPlate &SP, bool bDoQSSolidTempUpdate)
{
	if(!bDoQSSolidTempUpdate){
		return(true);
		}

	double lfSaveT=m_lfTemp[CUR][i];

	lfCurrentDensity[i]=lfCurrentDensityGuess;
	lfEntropyHeatGen[i]=-m_lfTemp[CUR][i]*TotalEntropyChange*lfCurrentDensity[i]*DelX[i]*m_lfCellWidth/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;//watts
	m_lfNodalIdealVoltage[i]=ENernst;
	m_lfNodalIdealVoltage_AtElectrolyteSurface[i]=ENernst+CathodeEtaConc+AnodeEtaConc;//these etaConc losses are negative valued...
	double ET=m_lfTemp[CUR][i];
	double StepCell=ET/1000.0;
	bool bLowerBoundCell=false;
	bool bUpperBoundCell=false;
	double lfLowerBoundCell=0.0;
	double lfUpperBoundCell=10000.0;
	double AbsDiffOld=0.0;
	// MAY NEED TO LOOOP THROUGH IF WE DO RADIATION...
	// THIS CODE WILL HAVE TO BE UPDATED FOR THAT CAPABILITY
	//while(doRAdiation){
		double AT_i=An->GetTemp(i);
		double Ahcoef=An->GetHeatTransferCoef(i-1);
		double CT_i=Ca->GetTemp(i);
		double Chcoef=Ca->GetHeatTransferCoef(i-1);
		double ST=(Chcoef*CT_i+Ahcoef*AT_i)/(Chcoef+Ahcoef);

		double QRad=0.0;
		double STguess=SP.m_lfTemp[CUR][i];
#if 0
		if(bDoRadiation){
			// do iteration to get SepPlt temp for current ET...
			double delST=STguess/500.0;
			double OldDelT=1000000.0;
			while(1){
				QRad=2.0*g_lfStephanBoltz*(pow(STguess,4.0)-pow(ET,4.0));
				ST=(Chcoef*CT_i+Ahcoef*AT_i-QRad)/(Chcoef+Ahcoef);
				double DelT=ST-STguess;//DelQ/Chcoef;//make it into a hypothetical Temp.
				DelT=fabs(DelT);
				if(DelT<0.001 || fabs(delST)<0.000001) break;
				if(DelT>OldDelT){
					delST*=-.0333213;
					}
				else{
					delST*=1.42;
					}
				OldDelT=DelT;
				STguess+=delST;
				}
			}
#endif
		double Qgen=GetQGenOfCellNode(i);//this requires that m_lfNodalIdealVoltage[i]-m_lfLoadVoltage be known
		double QQ=Qgen+Ca->GetNetEnthalpyInputToCell(i, (const double) ET, CT_i)+An->GetNetEnthalpyInputToCell(i, (const double) ET, AT_i);
		double newET=(Chcoef*CT_i+Ahcoef*AT_i-QRad+QQ/DelX[i]/m_lfCellWidth)/(Chcoef+Ahcoef);
		double DelET=newET-ET;
		double AbsDelET=fabs(DelET/ET);

		double wt=1.0/(1.0+fabs(ET-newET)/10.0);// the farther away we are from convergence, the slower we want to go so we don't pass up the correct steady state point
		ET=(1.0-wt)*ET+wt*newET;// do some averaging...
#if 0
		
Only need to iterate if doing radiation, so include this later
		if(AbsDelET<0.000001 || lfUpperBoundCell/(lfLowerBoundCell+0.000000001)<1.000000001) {
			if(lfUpperBoundCell<lfLowerBoundCell) {
				printf("BAD BOUNDS");
				}
			else if(AbsDelET>0.00002){
				TRACE2("AbsDelET=%le, %d\n",AbsDelET,i);
				}
			break;
			}
		if(bUpperBoundCell&&bLowerBoundCell){
			if(DelET<0.0){
				lfUpperBoundCell=(0.5*ET+0.5*lfUpperBoundCell);
				}
			else {
				lfLowerBoundCell=(0.5*ET+0.5*lfLowerBoundCell);
				}
			ET=(lfUpperBoundCell+lfLowerBoundCell)/2.;
			}
		else{
			if(AbsDelET>AbsDiffOld){
				if(AbsDiffOld>0.0){
					if(DelET<0.0 && StepCell>0.0){
						bUpperBoundCell=true;
						lfUpperBoundCell=ET;
						StepCell*=-0.5*(1.0-0.000002/AbsDelET);// the stuff in () slows down the step if we happen to be real close to the desired value...
						}
					else if (DelET>0.0 && StepCell<0.0){
						bLowerBoundCell=true;
						lfLowerBoundCell=ET;
						StepCell*=-0.5*(1.0-0.000002/AbsDelET);// the stuff in () slows down the step if we happen to be real close to the desired value...
						}
					if(bUpperBoundCell&&bLowerBoundCell) continue;
					}
				}
			else{
				StepCell*=1.121;
				}
			ET+=StepCell;
			if(ET<0.0){
				ET=0.00000001;//we don't want to drive in reverse...
				StepCell=ET/100.0;
				}
			AbsDiffOld=AbsDelET;
			}
		}
#endif
	m_lfTemp[CUR][i]=ET;
	double Diff=fabs((m_lfTemp[CUR][i]-lfSaveT)/lfSaveT) + fabs((SP.m_lfTemp[CUR][i]-ST)/ST);
	SP.m_lfTemp[CUR][i]=ST;
	if(Diff<0.0000005) {
		return(true);
		}
	return(false);
}

double lfLastCurrentChange[MAX_AXIALNODES];
int CElectrolyte::AnalyzeCurrDen()
{
// Use this code only for Dynamic analysis--not when gas phase is quasi-steady--there, use fully coupled solution techniques.
// In this analysis, you first guess the voltage on the load resistance.  Then crunch through
// to calculate the resultant current through all nodes to see if they compare....
// This routine cannot handle negative current flow--that is electrolysis!!!
static double lfLastVoltageChange=1000000.0;
double lfSaveCurrentDensity[MAX_AXIALNODES];
for(int i=1;i<m_nNoNodes-1;i++){
	lfSaveCurrentDensity[i]=lfCurrentDensity[i];
	}
double LoadVoltageGuess=0.7;
if(m_lfLoadVoltage>0.0){
	LoadVoltageGuess=m_lfLoadVoltage;// use the last employed voltage as initial guess
	}
double lfStepVolt=LoadVoltageGuess/100.;
if(lfLastVoltageChange<3.0){
	lfStepVolt=lfLastVoltageChange/2.0;
	}
bool bLowerVolt=false;
bool bUpperVolt=false;
double lfLowerVolt=0.0;
double lfUpperVolt=0.0;

while(1){
	double lfTotalCurrent=0.0;
	for(int i=1;i<m_nNoNodes-1;i++){
		double Ti=m_lfTemp[CUR][i];
		double AnodeP=An->P[i];
		double CathodeP=Ca->P[i];
		double MolarDelHStd=GetMolarDelHStd(Ti);//joule/kg-mole
		double MolarDelSStd=GetMolarDelSStd(Ti);// per kg-mole of H2! without effects of mixing and at std. conditions!!
		double MolarDelGStd=MolarDelHStd-Ti*MolarDelSStd;//delG per kg-mole of H2
		MolarDelGStd/=1000.0;//delG per g-mole of H2

		double ReactantActivity=GetReactantActivity(i,AnodeP,CathodeP);
		double ProductActivity=GetProductActivity(i,AnodeP,CathodeP);//AnSpH2O*AnodeP/G_SpDb.REFERENCE_P;
		double ENernst=-MolarDelGStd/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;
		ENernst+=Runiv*Ti*log(ReactantActivity/ProductActivity)/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;
		double TotalEntropyChange=MolarDelSStd/1000.0+Runiv*log(ReactantActivity/ProductActivity);//joule/gm-mole/K
		if(ENernst<0.0){
			printf("Can't do electolysis\n");
			return(MUST_HAVE_ELECTROLYSIS_CONDITIONS);
			}
		if(LoadVoltageGuess>ENernst) {
			LoadVoltageGuess=ENernst*0.99;//assume that we will always pull a little current
			}

		double lfCurrentDensityGuess=0.1*100.0*100.0;//ampere/m2
		if(lfCurrentDensity[i]>0.0){
			lfCurrentDensityGuess=lfCurrentDensity[i];
			}


		double lfStep=lfCurrentDensityGuess/500.0;
		if(lfLastCurrentChange[i]!=0.0){
			// then we must have passed through at least once already
			lfStep=lfLastCurrentChange[i]/2.0;
			}


		double lfTotalCellResistanceOhmM2=GetCellNodeResistance(i,Ti);

		double lfCathodeLimitingSupplyCurrentDensity=Ca->GetLimitingSupplyCurrentDensity(i);
		double lfCathodeLimitingCurrentDensity=Ca->GetCathodeLimitingCurrentDensity(i, CathodeP, Ti);
		if(lfCathodeLimitingSupplyCurrentDensity<lfCathodeLimitingCurrentDensity){
			lfCathodeLimitingCurrentDensity=lfCathodeLimitingSupplyCurrentDensity;
			}
		if(lfCurrentDensityGuess>lfCathodeLimitingCurrentDensity){
			lfCurrentDensityGuess=lfCathodeLimitingCurrentDensity*.9;
			}

//		double lfAnodeLimitingSupplyCurrentDensity=An->GetLimitingSupplyCurrentDensity(i);
		double lfAnodeLimitingCurrentDensity=An->GetAnodeLimitingCurrentDensity(i,AnodeP,Ti);
//		if(lfAnodeLimitingSupplyCurrentDensity<lfAnodeLimitingCurrentDensity){
//			lfAnodeLimitingCurrentDensity=lfAnodeLimitingSupplyCurrentDensity;
//			}
		if(lfCurrentDensityGuess>lfAnodeLimitingCurrentDensity){
			lfCurrentDensityGuess=lfAnodeLimitingCurrentDensity*.9;
			}

		bool bUpperBound=false;
		bool bLowerBound=false;
		double lfUpper,lfLower;

		double CathodeEtaConc=0.0;
		double AnodeEtaConc=0.0;
		while(1){
			double CathodeEtaActivation=GetCathodeActivationLoss(Ti,lfCurrentDensityGuess);
			CathodeEtaConc=Ca->GetCathodeConcLoss(Ti,CathodeP,i,lfCurrentDensityGuess);

			double AnodeEtaActivation=GetAnodeActivationLoss(Ti,lfCurrentDensityGuess);
			AnodeEtaConc=An->GetAnodeConcLoss(Ti,AnodeP,i,lfCurrentDensityGuess);

			double TotalOutputVoltage=0.0;
			TotalOutputVoltage+=ENernst;
			TotalOutputVoltage-=(CathodeEtaActivation-CathodeEtaConc);
			TotalOutputVoltage-=(AnodeEtaActivation-AnodeEtaConc);
			TotalOutputVoltage-=lfCurrentDensityGuess*lfTotalCellResistanceOhmM2;

			double DiffVolt=TotalOutputVoltage-LoadVoltageGuess;
			double AbsDiffVolt=fabs(DiffVolt);
			double DiffCurr=lfStep/lfCurrentDensityGuess;
			DiffCurr=fabs(DiffCurr);
			if(DiffCurr<CURRENT_PRECISION) {
				if(DiffVolt<0.01){
					break;
					}
				printf("Current converged, not volts? (%le)\n",DiffVolt);
				}

			if(bUpperBound&&bLowerBound){
				lfStep=lfCurrentDensityGuess;
				if(DiffVolt>0.0){
					lfLower=lfCurrentDensityGuess;
					lfCurrentDensityGuess=(lfUpper+lfLower)/2.;
					}
				else {
					lfUpper=lfCurrentDensityGuess;
					lfCurrentDensityGuess=(lfUpper+lfLower)/2.;
					}
				lfStep-=lfCurrentDensityGuess;
				}
			else {
				if(DiffVolt<0.0 && lfStep>0.0){
					lfStep*=-0.33333;
					bUpperBound=true;
					lfUpper=lfCurrentDensityGuess;
					}
				else if(DiffVolt>0.0 && lfStep<0.0){
					lfStep*=-0.3333;
					bLowerBound=true;
					lfLower=lfCurrentDensityGuess;
					}
				else {
					if(fabs(lfStep*5.0)<lfCurrentDensityGuess){
						lfStep*=2.0;
						}
					else{
						lfStep=fabs(lfStep)/lfStep*lfCurrentDensityGuess/20.0;
						}
					}
				if(lfCurrentDensityGuess+lfStep>lfCathodeLimitingCurrentDensity){
					lfStep=(lfCurrentDensityGuess+lfCathodeLimitingCurrentDensity)/2.-lfCurrentDensityGuess;
					}
				if(lfCurrentDensityGuess+lfStep>lfAnodeLimitingCurrentDensity){
					lfStep=(lfCurrentDensityGuess+lfAnodeLimitingCurrentDensity)/2.-lfCurrentDensityGuess;
					}
				if(lfCurrentDensityGuess+lfStep<0.0){
					lfStep=-lfCurrentDensityGuess/2.0;// don't allow negative currents at this time (the activation overpotentials can't handle this yet)
					}
				lfCurrentDensityGuess+=lfStep;
				}
			}

		lfCurrentDensity[i]=lfCurrentDensityGuess;
		lfEntropyHeatGen[i]=-m_lfTemp[CUR][i]*TotalEntropyChange*lfCurrentDensity[i]*DelX[i]*m_lfCellWidth/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;//watts

		lfTotalCurrent+=lfCurrentDensityGuess*DelX[i]*m_lfCellWidth;//Amperes
		m_lfNodalIdealVoltage[i]=ENernst;
		m_lfNodalIdealVoltage_AtElectrolyteSurface[i]=ENernst+CathodeEtaConc+AnodeEtaConc;//these etaConc losses are negative valued...
		}

	double LoadCurrentExpected=LoadVoltageGuess/m_lfLoadResistance;
	double Diff=LoadCurrentExpected-lfTotalCurrent;
	double AbsDiff=fabs(Diff/LoadCurrentExpected);
	if(AbsDiff<CURRENT_PRECISION) {
		break;
		}

	if(bLowerVolt&&bUpperVolt){
		if(Diff>0.0){
			lfUpperVolt=LoadVoltageGuess;
			}
		else{
			lfLowerVolt=LoadVoltageGuess;
			}
		LoadVoltageGuess=(lfLowerVolt+lfUpperVolt)/2.0;
		}
	else {
		if(Diff>0.0 && lfStepVolt>0.0){// need to go to smaller voltage so that expected current is less and calc. current through cell is more
			bUpperVolt=true;
			lfUpperVolt=LoadVoltageGuess;
			lfStepVolt*=-0.3333;
			}
		else if(Diff<0.0 && lfStepVolt<0.0){
			bLowerVolt=true;
			lfLowerVolt=LoadVoltageGuess;
			lfStepVolt*=-0.33333;
			}
		else {
			lfStepVolt*=2.0;
			}
		LoadVoltageGuess+=lfStepVolt;
		}
	}
lfCurrentDensity[m_nNoNodes-1]=0.0;
lfCurrentDensity[0]=0.0;
if(m_lfLoadVoltage>0.0){
	double r=LoadVoltageGuess/m_lfLoadVoltage;
	if((r>1.001 || r<0.999)){
		lfLastVoltageChange=LoadVoltageGuess-m_lfLoadVoltage;
		}
	for(int i=1;i<m_nNoNodes-1;i++){
		double rc=lfSaveCurrentDensity[i]/lfCurrentDensity[i];
		if(rc>1.001 || rc<0.999){
			lfLastCurrentChange[i]=lfSaveCurrentDensity[i]-lfCurrentDensity[i];
			}
		}
	}
m_lfLoadVoltage=LoadVoltageGuess;

AnalyzeOveallLosses(Ca->P, An->P);

return(GOOD_RESULT);
}

extern bool g_bDoDump;
int CElectrolyte::AnalyzeCurrDenGivenVoltage(double lfCellVoltage)
{
	// In this analysis, you are given voltage on some load resistance.  Then crunch through
	// to calculate the resultant current through all nodes...
	if(lfCellVoltage<0.0){
		printf("ERR AnalCurrDenGivenVolts\n");
		return(BAD_SETUP);
		}

	double LoadVoltageGuess=lfCellVoltage;

	for(int i=1;i<m_nNoNodes-1;i++){
		double Ti=m_lfTemp[CUR][i];
		double AnodeP=An->P[i];//AnodePres[i];
		double CathodeP=Ca->P[i];//CathodePres[i];
		double MolarDelHStd=GetMolarDelHStd(Ti);//joule/kg-mole
		double MolarDelSStd=GetMolarDelSStd(Ti);// per kg-mole of H2! without effects of mixing and at std. conditions!!
		double MolarDelGStd=MolarDelHStd-Ti*MolarDelSStd;//delG per kg-mole of H2
		MolarDelGStd/=1000.0;//delG per g-mole of H2

		double ReactantActivity=GetReactantActivity(i,AnodeP,CathodeP);
		double ProductActivity=GetReactantActivity(i,AnodeP,CathodeP);
		double ENernst=-MolarDelGStd/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;
		ENernst+=Runiv*Ti*log(ReactantActivity/ProductActivity)/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;
		if(ENernst<0.0){
			printf("Can't do electolysis\n");
			return(MUST_HAVE_ELECTROLYSIS_CONDITIONS);
			}
		if(LoadVoltageGuess>ENernst) {
			return(MUST_HAVE_ELECTROLYSIS_CONDITIONS);
			}
		double TotalEntropyChange=MolarDelSStd/1000.0+Runiv*log(ReactantActivity/ProductActivity);//joule/gm-mol/K

		double lfCurrentDensityGuess=0.1*100.0*100.0;//ampere/m2
		if(lfCurrentDensity[i]>0.0){
			lfCurrentDensityGuess=lfCurrentDensity[i];
			}

		double lfTotalCellResistanceOhmM2=GetCellNodeResistance(i,Ti);

		double lfCathodeLimitingSupplyCurrentDensity=Ca->GetLimitingSupplyCurrentDensity(i);
		double lfCathodeLimitingCurrentDensity=Ca->GetCathodeLimitingCurrentDensity(i, CathodeP, Ti);
		if(lfCathodeLimitingSupplyCurrentDensity<lfCathodeLimitingCurrentDensity){
			lfCathodeLimitingCurrentDensity=lfCathodeLimitingSupplyCurrentDensity;
			}
		if(lfCurrentDensityGuess>lfCathodeLimitingCurrentDensity){
			lfCurrentDensityGuess=lfCathodeLimitingCurrentDensity*.9;
			}

		double lfAnodeLimitingSupplyCurrentDensity=An->GetLimitingSupplyCurrentDensity(i);
		double lfAnodeLimitingCurrentDensity=An->GetAnodeLimitingCurrentDensity(i,AnodeP,Ti);
		if(lfAnodeLimitingSupplyCurrentDensity<lfAnodeLimitingCurrentDensity){
			lfAnodeLimitingCurrentDensity=lfAnodeLimitingSupplyCurrentDensity;
			}
		if(lfCurrentDensityGuess>lfAnodeLimitingCurrentDensity){
			lfCurrentDensityGuess=lfAnodeLimitingCurrentDensity*.9;
			}
		
		double lfStep=lfCurrentDensityGuess/50.0;

		bool bUpperBound=false;
		bool bLowerBound=false;
		double lfUpper=0.0,lfLower=0.0;

		double CathodeEtaConc=0.0;
		double AnodeEtaConc=0.0;
		while(1){
			double CathodeEtaActivation=GetCathodeActivationLoss(Ti,lfCurrentDensityGuess);
			CathodeEtaConc=Ca->GetCathodeConcLoss(Ti,CathodeP,i,lfCurrentDensityGuess);

			double AnodeEtaActivation=GetAnodeActivationLoss(Ti,lfCurrentDensityGuess);
			AnodeEtaConc=An->GetAnodeConcLoss(Ti,AnodeP,i,lfCurrentDensityGuess);

			double TotalOutputVoltage=0.0;
			TotalOutputVoltage+=ENernst;
			TotalOutputVoltage-=(CathodeEtaActivation-CathodeEtaConc);
			TotalOutputVoltage-=(AnodeEtaActivation-AnodeEtaConc);
			TotalOutputVoltage-=lfCurrentDensityGuess*lfTotalCellResistanceOhmM2;

			double DiffVolt=TotalOutputVoltage-LoadVoltageGuess;
			double DiffCurr=lfStep/lfCurrentDensityGuess;
			DiffCurr=fabs(DiffCurr);
			if(DiffCurr<CURRENT_PRECISION) {
				break;
				}

			if(bUpperBound&&bLowerBound){
				lfStep=lfCurrentDensityGuess;
				if(DiffVolt>0.0){
					lfLower=lfCurrentDensityGuess;
					lfCurrentDensityGuess=(lfUpper+lfLower)/2.;
					}
				else {
					lfUpper=lfCurrentDensityGuess;
					lfCurrentDensityGuess=(lfUpper+lfLower)/2.;
					}
				lfStep-=lfCurrentDensityGuess;
				}
			else {
				if(DiffVolt<0.0 && lfStep>0.0){
					lfStep*=-0.33333;
					bUpperBound=true;
					lfUpper=lfCurrentDensityGuess;
					}
				else if(DiffVolt>0.0 && lfStep<0.0){
					lfStep*=-0.3333;
					bLowerBound=true;
					lfLower=lfCurrentDensityGuess;
					}
				else {
					if(fabs(lfStep*5.0)<lfCurrentDensityGuess){
						lfStep*=2.0;
						}
					else{
						lfStep=fabs(lfStep)/lfStep*lfCurrentDensityGuess/20.0;
						}
					}
				if(lfCurrentDensityGuess+lfStep>lfCathodeLimitingCurrentDensity){
					lfStep=(lfCurrentDensityGuess+lfCathodeLimitingCurrentDensity)/2.-lfCurrentDensityGuess;
					}
				if(lfCurrentDensityGuess+lfStep>lfAnodeLimitingCurrentDensity){
					lfStep=(lfCurrentDensityGuess+lfAnodeLimitingCurrentDensity)/2.-lfCurrentDensityGuess;
					}
				lfCurrentDensityGuess+=lfStep;
				}
			}
//		if(i==1 && g_bDoDump){
//			TRACE2("Videal=%le, CurrDen=%le\n",ENernst,lfCurrentDensityGuess);
//			}
		lfCurrentDensity[i]=lfCurrentDensityGuess;
		lfEntropyHeatGen[i]=-m_lfTemp[CUR][i]*TotalEntropyChange*lfCurrentDensity[i]*DelX[i]*m_lfCellWidth/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;//watts
		m_lfNodalIdealVoltage[i]=ENernst;
		m_lfNodalIdealVoltage_AtElectrolyteSurface[i]=ENernst+CathodeEtaConc+AnodeEtaConc;//these etaConc losses are negative valued...
		}

	lfCurrentDensity[m_nNoNodes-1]=0.0;
	lfCurrentDensity[0]=0.0;
	m_lfLoadVoltage=LoadVoltageGuess;
	AnalyzeOveallLosses(Ca->P, An->P);
	return(GOOD_RESULT);
}

} // end namespace Vision21
