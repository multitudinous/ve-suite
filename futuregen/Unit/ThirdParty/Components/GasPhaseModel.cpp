
#include "globals.h"
#include "GasPhaseModel.h"
#include "Sp_dbase.h"
#include "cmath"
#include "cfloat"
#include <stdio.h>

namespace Vision21 {

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

GasPhaseModel::GasPhaseModel()
{
	m_bDoWeightingOfCatalyst=false;
	m_plfElecTempSource=NULL;
	m_plfUpperWallTempSource=NULL;
	m_plfLowerWallTempSource=NULL;
	m_bCounterFlowTo_SurfaceTempArrayIndicieDirection=false;
	m_nNoNodes=0;
	m_nNoSurfaceSpecieFluxes=0;
	m_pplfMoleFluxIntoGas[0]=NULL;
	m_bDoQS=false;
	m_lfFlowResistanceGain=1.0;// this is a 'calibration' parameter based on user input as desired
}

void GasPhaseModel::Setup(int nNoNodes, bool bDoFixedFlow, bool bDoQS, bool bDoRef, bool bDoEquilRef, ChanGeom *ChanGeom, double lfCellWidth, const double * lfXS_AreaAnode, const double * lfDelX, double lfVelInput, double lfAnodePInput, double lfAnodePExit, double lfAnodeTInput, int nNoAnodeSp, const double * lfAnodeSpInput)
{
	m_lfCellWidth=lfCellWidth;
	m_ChanGeom=*ChanGeom;
	m_nNoNodes=nNoNodes;
	m_bDoFixedFlow=bDoFixedFlow;
	XS_Area=lfXS_AreaAnode;
	DelX=lfDelX;
	m_bDoQS=bDoQS;
	m_nNoSp=nNoAnodeSp;
	m_lfVelInput=lfVelInput;
	int i;
	for(i=0;i<m_nNoNodes;i++){
		double lfSpSum=0.0;
		for(int j=0;j<m_nNoSp;j++){
			Sp[CUR][i][j]=lfAnodeSpInput[j];
			lfSpSum+=lfAnodeSpInput[j];
			}
		if(lfSpSum<0.99 || lfSpSum>1.01){
			printf("Bad Specie Setup--Input does not sum to 1.0\n");
//			exit(0);
			}
		Vel[CUR][i]=m_lfVelInput;// this will actually specify one more node beyond that needed!
		P[i]=lfAnodePInput+(lfAnodePExit-lfAnodePInput)*(double)i/(m_nNoNodes-1);
		Pnew[i]=P[i];
		Rho[CUR][i]=P[i]/Runiv/1000.0/lfAnodeTInput;
		double tst=GetMixMW(Sp[CUR][i]);
		Rho[CUR][i]*=tst;
		h[CUR][i]=GetMixHFromT(Sp[CUR][i],lfAnodeTInput);
		if(!m_bDoQS){
			// we will use Total Internal Energy (u+KE) for Dyn. Analysis (QS analysis uses enthalpy)
			h[CUR][i]+=-P[i]/Rho[CUR][i]+Vel[CUR][i]*Vel[CUR][i]/2.;
			}
		}
	AnalyzeTemperatures();
	if(m_plfUpperWallTempSource==NULL || m_plfLowerWallTempSource==NULL){
		printf("BAD SETUP\n");
//		exit(0);
		}
	for(int j=0;j<MAX_STORE;j++){
		for(i=0;i<m_nNoNodes;i++){
			if(m_bCounterFlowTo_SurfaceTempArrayIndicieDirection){
				if(m_plfElecTempSource!=NULL){
					m_plfElecTemp[MAX_AXIALNODES*j+i]=&m_plfElecTempSource[MAX_AXIALNODES*j+m_nNoNodes-1-i];
					}
				else {
					m_plfElecTemp[MAX_AXIALNODES*j+i]=NULL;
					}
				m_plfUpperWallTemp[MAX_AXIALNODES*j+i]=&m_plfUpperWallTempSource[MAX_AXIALNODES*j+m_nNoNodes-1-i];
				m_plfLowerWallTemp[MAX_AXIALNODES*j+i]=&m_plfLowerWallTempSource[MAX_AXIALNODES*j+m_nNoNodes-1-i];
				}
			else{
				if(m_plfElecTempSource!=NULL){
					m_plfElecTemp[MAX_AXIALNODES*j+i]=&m_plfElecTempSource[MAX_AXIALNODES*j+i];
					}
				else{
					m_plfElecTemp[MAX_AXIALNODES*j+i]=NULL;
					}
				m_plfUpperWallTemp[MAX_AXIALNODES*j+i]=&m_plfUpperWallTempSource[MAX_AXIALNODES*j+i];
				m_plfLowerWallTemp[MAX_AXIALNODES*j+i]=&m_plfLowerWallTempSource[MAX_AXIALNODES*j+i];
				}
			}
		}
}

void GasPhaseModel::ContinuationSetup(bool bDoFixedFlow, double lfVelInput,bool bDoQS, bool bDoRef, bool bDoEquilRef, const double * lfXS_AreaAnode, const double * lfDelX)
{
	XS_Area=lfXS_AreaAnode;
	DelX=lfDelX;
	m_bDoFixedFlow=bDoFixedFlow;
	m_lfVelInput=lfVelInput;

	if(m_bDoQS && !bDoQS){
		// was QS, but now requesting to start non-QS...
		for(int i=0;i<m_nNoNodes;i++){
			h[CUR][i]+=-P[i]/Rho[CUR][i]+Vel[CUR][i]*Vel[CUR][i]/2.;// going to start using (e+KE) for h[]
			}
		}
	else if(!m_bDoQS && bDoQS){
		for(int i=0;i<m_nNoNodes;i++){
			h[CUR][i]-=-P[i]/Rho[CUR][i]+Vel[CUR][i]*Vel[CUR][i]/2.;// going to start using (h) for h[]
			}
		}
	m_bDoQS=bDoQS;
}


GasPhaseModel::~GasPhaseModel()
{

}

int GasPhaseModel::AnalyzeNextTimeStep(double lfTimeStep)
{
	if(lfTimeStep<0.0 && !m_bDoQS){
		printf("ERR TIME STEP\n");
		}

	if(!m_bDoQS){
		AnalyzeEnergyEqu(lfTimeStep);//get h[NXT]
		AnalyzeMassEqu(lfTimeStep);//get Rho[NXT]
		AnalyzeSpeciesEqu(lfTimeStep);//get Sp[NXT]
		AnalyzeMomentumEqu(lfTimeStep);//get Vel[NXT]
		}

	return(1);
}

int GasPhaseModel::AnalyzeEnergyEqu(double lfTimeStep)
{
	// going into this routine, h[] is (Internal Energy + KE) for ALL nodes
	// going out of this routine, h[] is (Internal Energy + KE) at all nodes except last one which is (Enthalpy + KE)
	for(int i=1;i<m_nNoNodes;i++){
		double Hin=GetNetIntEnergyFlowIn(i);
		double Min=GetNetMassFlowIn(i);
		double Qdot=m_lfHeatRateAddedUpperWall[i] + m_lfHeatRateAddedLowerWall[i];
		m_lfQdot[i]=Qdot;
		double lfHH=h[CUR][i];
		if(i==m_nNoNodes-1){
			lfHH=h[CUR][i]+P[i]/Rho[CUR][i];
			}
		double XS_A=XS_Area[i-1];
		if(i<m_nNoNodes-1){
			XS_A+=XS_Area[i];
			XS_A/=2.;
			}
		// this equation is all d(Internal Energy+KE)/dt for all nodes but the last one, where we use d(Enthalpy)/dt since we assume dP/dt=0.0 there!!
		h[NXT][i]=lfHH+lfTimeStep/Rho[CUR][i]/(XS_A*DelX[i])*(Hin-lfHH*Min+Qdot);
		}
	return(1);
}

int GasPhaseModel::AnalyzeMassEqu(double lfTimeStep)
{
	for(int i=1;i<m_nNoNodes;i++){
		double Min=GetNetMassFlowIn(i);
		double XS_A=XS_Area[i-1];
		if(i<m_nNoNodes-1){
			XS_A+=XS_Area[i];
			XS_A/=2.;
			}
		Rho[NXT][i]=Rho[CUR][i]+lfTimeStep/(XS_A*DelX[i])*Min;
		}
	return(1);
}

int GasPhaseModel::AnalyzeSpeciesEqu(double lfTimeStep)
{
	double CjNew[MAX_NOSP];
	for(int i=1;i<m_nNoNodes;i++){
		int j;
		double lfTotConc=0.0;
		for(j=0;j<m_nNoSp;j++){
			double Min=GetNetKgMoleFlowIn(j,i);
			double MWT=GetMixMW(Sp[CUR][i]);
			double CjCurrent=Sp[CUR][i][j]*Rho[CUR][i]/MWT;
			double XS_A=XS_Area[i-1];
			if(i<m_nNoNodes-1){
				XS_A+=XS_Area[i];
				XS_A/=2.;
				}
			CjNew[j]=Min/(XS_A*DelX[i])*lfTimeStep+CjCurrent;
			lfTotConc+=CjNew[j];
			}
		//now make sure that species fractions stay summed to 1.0
		for(j=0;j<m_nNoSp;j++){
			Sp[NXT][i][j]=CjNew[j]/lfTotConc;
			}
		}
	return(1);

}

int GasPhaseModel::AnalyzeMomentumEqu(double lfTimeStep)
{
	// now the INDEX 'i' refers to the Vel[] array!!!!
	for(int i=0;i<m_nNoNodes-1;i++){
		double MOMin=GetNetMomentumFlowIn(i);
		double Min=GetMomentumNetMassFlowIn(i);
		double SurfaceForceOnGas=GetFrictionForce(i);
		SurfaceForceOnGas+=GetPressureForce(i);
		double RhoAve=(Rho[CUR][i]+Rho[CUR][i+1])/2.;
		Vel[NXT][i]=Vel[CUR][i]+lfTimeStep/RhoAve/(XS_Area[i]*DelX[i])*\
			(MOMin+SurfaceForceOnGas-Vel[CUR][i]*Min);
		}
	return(1);
}

int GasPhaseModel::AddSpecieFluxArray(UINT uSpIdx, double * plfSurfaceMoleFlux)
{
	int i=0;
	for(i=0;i<m_nNoSurfaceSpecieFluxes;i++){
		if(m_uSpIdx[i]==uSpIdx){
			m_pplfMoleFluxIntoGas[i]=plfSurfaceMoleFlux;
			break;
			}
		}
	if(i==m_nNoSurfaceSpecieFluxes){
		if(i==MAX_SURFACESPECIEFLUX){
			printf("Too many surface species requested.");
			return(0);
			}
		m_uSpIdx[i]=uSpIdx;
		m_pplfMoleFluxIntoGas[i]=plfSurfaceMoleFlux;
		m_nNoSurfaceSpecieFluxes++;
		}
	return(1);
}

double GasPhaseModel::GetNetIntEnergyFlowIn(int i)
{
	// this is total energy (e+VelSq/2)
	double Hin=0.0;
	if(i==0){
		printf("err ENERGY FLOW\n");
		return(Hin);
		}
	int nLNode=i-1;
	int nRNode=i;
	double VelLeft=Vel[CUR][nLNode];
	double VelRight=Vel[CUR][nRNode];
	double ALeft=XS_Area[nLNode];
	double ARight=XS_Area[nRNode];

	double PLeft=(P[i-1]+P[i])/2.;
	double RhoHLeft=h[CUR][i-1]*Rho[CUR][i-1];
	double RhoH=h[CUR][i]*Rho[CUR][i];
	if(i<m_nNoNodes-1){
		double PRight=(P[i]+P[i+1])/2.;
		double RhoHRight=h[CUR][i+1]*Rho[CUR][i+1];
		if(VelLeft>0.0){
			Hin+=VelLeft*(RhoHLeft)*ALeft+\
					PLeft*VelLeft*ALeft;
			}
		else {
			Hin+=VelLeft*(RhoH)*ALeft+\
					PLeft*VelLeft*ALeft;
			}
		if(VelRight>0.0){
			Hin-=VelRight*RhoH*ARight+\
					PRight*VelRight*ARight;
			}
		else {
			Hin-=VelRight*RhoHRight*ARight+\
					PRight*VelRight*ARight;
			}
		}
	else {
		// solve for enthalpy at last node cuz there we assume dP/dt=0.0
		if(VelLeft>0.0){
			Hin+=VelLeft*(RhoHLeft+PLeft)*ALeft;
			}
		else {
			Hin+=VelLeft*(RhoH+P[i])*ALeft;
			}
		Hin-=VelLeft*(RhoH+P[i])*ALeft;
		}
	Hin+=GetNetSurfaceEnergyFlowIn(i,GetTemp(i));
	return(Hin); // joule/sec
}

void GasPhaseModel::SetElectrolyteTempArray(const double * plfElecTemp)
{
	m_plfElecTempSource=plfElecTemp;
}

double GasPhaseModel::GetMixMW(double * sp)
{
	double MWt=0.0;
	for(int i=0;i<m_nNoSp;i++){
		MWt+=sp[i]*G_SpDb.Specie[i].lfTDData[MW];
		}
	return(MWt);
}

double GasPhaseModel::GetMixCp(double * sp, double T)
{
	double Cp=0.0;
	double MixMW=GetMixMW(sp);
	for(int i=0;i<m_nNoSp;i++){
		Cp+=sp[i]*G_SpDb.Specie[i].lfTDData[MW]/MixMW*G_SpDb.getSpecieCp(T,(int)i);
		}
	return(Cp);//joule/kg-K
}

double GasPhaseModel::GetHeatTransferCoef(int i)
{
	// Now the INDEX 'i' refers to the node upstream of the node for which you are analyzing heat transfer
	// This works for both QS and Dynamic analysis.
	if(i>=m_nNoNodes-1){
		printf("OVERSHOOT ARRAY?");
		}
	if(i==m_nNoNodes-2||i==m_nNoNodes-1){
		// the perspective here is that we use the closest upstream velocity node conditions
		// to determine the convective conditions that impact the heat flux just down stream
		// of the velocity node.  hence, the inlet T node remains fixed in temp (cuz there
		// is no upstream vel node to provide info.), and the exit T node has zero wall heat flux cuz
		// we force hcoef to be zero by the following return(0.0) statement for i==m_nNoNodes-2
		// case!!  This is fairly close, since streamwise gradients are very small for
		// fuel cell technology by necessity.  The benefit is improved
		// convergence for the QuasiSteady gas physics!
		return(0.0);
		}
	double Ti=GetTemp(i);
	Ti+=GetTemp(i+1);
	Ti/=2.0;
	double Mu=GetMixMu(Sp[CUR][i],Ti);
	Mu+=GetMixMu(Sp[CUR][i+1],Ti);
	Mu/=2.0;
	double k=GetMixK(Sp[CUR][i],Ti);
	k+=GetMixK(Sp[CUR][i+1],Ti);
	k/=2.0;
	double Cpmix=GetMixCp(Sp[CUR][i],Ti);
	Cpmix+=GetMixCp(Sp[CUR][i+1],Ti);
	Cpmix/=2.0;
	double VelAve=Vel[CUR][i];

	double lfChanFlowArea=m_ChanGeom.lfHeight*m_ChanGeom.lfChannelWidth;
	double lfPerimeter=(m_ChanGeom.lfHeight+m_ChanGeom.lfChannelWidth)*2.0;//note: we assume square type geometry
	double hyd_diam=4.0*lfChanFlowArea/lfPerimeter;
	double RhoAve=(Rho[CUR][i]+Rho[CUR][i+1])/2.;
	double Re=RhoAve*VelAve*hyd_diam/Mu;
	double Pr=Mu*Cpmix/k;
	double Nu=get_nusselt(Re,Pr);
	double hcoef=Nu/hyd_diam*k;
	return(hcoef);//watt/m2/K
}



#define LOWERTRANSITION_RE	1500.0	// Below this Re, the channel IS solidly laminar
#define UPPERTRANSITION_RE	2000.0
double GasPhaseModel::get_nusselt(double Re, double Pr)
{
// Nu based on hydraulic diamter!  Nu_Dh = heat coeficient x Hydraulic Diamter / conductivity
// The problem with this analysis, is that the Nusselt numbers shown in text books (Mills "Basic Heat and Mass Transfer", pg. 241) are very different
// depending on if you pick constant heat flux, or constant wall temperature, especially at near unity aspect ratio, which is the flow geometry often seen.
// So there can be a big change depending on your perspective.  I'll do a comparitive analysis to see if there is
// any major impact to the results...

double Nu=(.022*(pow((Re),.8)*pow(Pr,.6)));/* from Reynolds and Perkins, table C.8 */

// Laminar--Fully developed laminar flow, uniform wall temperature
// From A.F. Mills Basic Heat and Mass Transfer, Table 4.5, page 241.
// (square duct Nu=3.66--was used) (infinite flat plate channel which is closer to fuel cell Nu=7.541) (1x8 rectangle duct Nu=5.6)
double AspectRatio=m_ChanGeom.lfHeight/m_ChanGeom.lfChannelWidth;
if(AspectRatio<1.0) AspectRatio=1.0/AspectRatio;//want to use the 'bigger value' in the definition of the ratio
if(AspectRatio>10.0) AspectRatio=10.0;//let's use a simple straight fit to the Nu vs. AspectRatio relationship--see Mill's text referenced above.
double LaminarLimitNu=3.66+(8.235-3.66)/9.0*(AspectRatio-1.0);

if(Re<UPPERTRANSITION_RE && Re>LOWERTRANSITION_RE){
	//weight the contribution of turbulent and laminar during "transition region"
	Nu=(UPPERTRANSITION_RE-Re)/(UPPERTRANSITION_RE-LOWERTRANSITION_RE)*LaminarLimitNu+(Re-LOWERTRANSITION_RE)/(UPPERTRANSITION_RE-LOWERTRANSITION_RE)*Nu;
	}
else if(Re<LOWERTRANSITION_RE){
	Nu=LaminarLimitNu;
	}
return(Nu);

}

double hSp(UINT GPidx, double t)
{
	double Hval=0.0;

	/* remember, you must integrate FROM REFERENCE_T TO t */
	double TotHval=G_SpDb.Specie[GPidx].lfTDData[REF_H];

	if(G_SpDb.m_bDoConstCp){
		TotHval+=(CONST_SPHEAT/G_SpDb.Specie[GPidx].lfTDData[MW])*(t-G_SpDb.REFERENCE_T);
		return(TotHval);//TRY CONST.SP HEAT...  joule/kg
		}

	double tstart=G_SpDb.REFERENCE_T;
	if(t<G_SpDb.Specie[GPidx].lfTDData[CP_TMIN]){
		if(G_SpDb.Specie[GPidx].lfTDData[CP_TMIN]<tstart){
			Hval+=G_SpDb.getSpecieCp(G_SpDb.Specie[GPidx].lfTDData[CP_TMIN],(int)GPidx)*(t-G_SpDb.Specie[GPidx].lfTDData[CP_TMIN]);
			t=G_SpDb.Specie[GPidx].lfTDData[CP_TMIN];
			}
		else {
			Hval+=G_SpDb.getSpecieCp(G_SpDb.Specie[GPidx].lfTDData[CP_TMIN],(int)GPidx)*(t-tstart);
			TotHval+=Hval;
			return(TotHval);
			}
		}
	else if(t>G_SpDb.Specie[GPidx].lfTDData[CP_TMAX]){
		if(G_SpDb.Specie[GPidx].lfTDData[CP_TMAX]<tstart){
			Hval+=G_SpDb.getSpecieCp(G_SpDb.Specie[GPidx].lfTDData[CP_TMAX],(int)GPidx)*(t-tstart);
			TotHval+=Hval;
			return(TotHval);
			}
		else {
			Hval+=G_SpDb.getSpecieCp(G_SpDb.Specie[GPidx].lfTDData[CP_TMAX],(int)GPidx)*(t-G_SpDb.Specie[GPidx].lfTDData[CP_TMAX]);
			t=G_SpDb.Specie[GPidx].lfTDData[CP_TMAX];
			}
		}
	/* Now, if we are still here, we need to integrate from REFERENE_T to t */
	if(tstart<G_SpDb.Specie[GPidx].lfTDData[CP_TMIN]){
		Hval+=G_SpDb.getSpecieCp(G_SpDb.Specie[GPidx].lfTDData[CP_TMIN],(int)GPidx)*(G_SpDb.Specie[GPidx].lfTDData[CP_TMIN]-tstart);
		tstart=G_SpDb.Specie[GPidx].lfTDData[CP_TMIN];
		}
	else if(tstart>G_SpDb.Specie[GPidx].lfTDData[CP_TMAX]){
		Hval+=G_SpDb.getSpecieCp(G_SpDb.Specie[GPidx].lfTDData[CP_TMAX],(int)GPidx)*(G_SpDb.Specie[GPidx].lfTDData[CP_TMAX]-tstart);
		tstart=G_SpDb.Specie[GPidx].lfTDData[CP_TMAX];
		}

	for(int i=Cp0;i<NO_CPVAL+Cp0;i++){
		Hval+=G_SpDb.Specie[GPidx].lfTDData[i]*pow((t     ),(double)(i-Cp0+1))/(double)(i-Cp0+1);
		Hval-=G_SpDb.Specie[GPidx].lfTDData[i]*pow((tstart),(double)(i-Cp0+1))/(double)(i-Cp0+1);
		}
	TotHval+=Hval;
	return(TotHval);//joule/kg
}

double MolarEntropySp(UINT GPidx, double t, double p)
{
	double Sval;

	Sval=G_SpDb.Specie[GPidx].lfTDData[REF_S]*G_SpDb.Specie[GPidx].lfTDData[MW]-Runiv*1000.0*log((p/G_SpDb.REFERENCE_P));
	Sval+=CPTterm(GPidx,t);
	return(Sval);//joule/kg-mole-K
}

double CPTterm(int GPidx, double t)	// this is the integral of (Cp/T) dT
{
	double Sval=0.0;
	double TotSval=0.0;

	if(G_SpDb.m_bDoConstCp){
		Sval=CONST_SPHEAT/G_SpDb.Specie[GPidx].lfTDData[MW]*log((t/G_SpDb.REFERENCE_T));
		TotSval+=Sval;
		TotSval*=G_SpDb.Specie[GPidx].lfTDData[MW];
		return(TotSval);
		}

	if(t<G_SpDb.Specie[GPidx].lfTDData[CP_TMIN]){
		if(G_SpDb.Specie[GPidx].lfTDData[CP_TMIN]<G_SpDb.REFERENCE_T){
			Sval=G_SpDb.getSpecieCp(G_SpDb.Specie[GPidx].lfTDData[CP_TMIN],(int)GPidx)*log((t/G_SpDb.Specie[GPidx].lfTDData[CP_TMIN]));
			t=G_SpDb.Specie[GPidx].lfTDData[CP_TMIN];
			}
		else {
			Sval=G_SpDb.getSpecieCp(G_SpDb.Specie[GPidx].lfTDData[CP_TMIN],(int)GPidx)*log((t/G_SpDb.REFERENCE_T));
			TotSval+=Sval;
			TotSval*=G_SpDb.Specie[GPidx].lfTDData[MW];
			return(TotSval);//joule/kg-mole-K
			}
		}
	else if(t>G_SpDb.Specie[GPidx].lfTDData[CP_TMAX]){
		if(G_SpDb.Specie[GPidx].lfTDData[CP_TMAX]<G_SpDb.REFERENCE_T){
			Sval=G_SpDb.getSpecieCp(G_SpDb.Specie[GPidx].lfTDData[CP_TMAX],(int)GPidx)*log((t/G_SpDb.REFERENCE_T));
			TotSval+=Sval;
			TotSval*=G_SpDb.Specie[GPidx].lfTDData[MW];
			return(TotSval);//joule/kg-mole-K
			}
		else {
			Sval=G_SpDb.getSpecieCp(G_SpDb.Specie[GPidx].lfTDData[CP_TMAX],(int)GPidx)*log((t/G_SpDb.Specie[GPidx].lfTDData[CP_TMAX]));
			t=G_SpDb.Specie[GPidx].lfTDData[CP_TMAX];
			}
		}
	/* Now, if we are still here, we need to integrate from REFERENE_T to t */
	double tstart=G_SpDb.REFERENCE_T;
	if(tstart<G_SpDb.Specie[GPidx].lfTDData[CP_TMIN]){
		Sval+=G_SpDb.getSpecieCp(G_SpDb.Specie[GPidx].lfTDData[CP_TMIN],(int)GPidx)*log((G_SpDb.Specie[GPidx].lfTDData[CP_TMIN]/tstart));
		tstart=G_SpDb.Specie[GPidx].lfTDData[CP_TMIN];
		}
	else if(tstart>G_SpDb.Specie[GPidx].lfTDData[CP_TMAX]){
		Sval+=G_SpDb.getSpecieCp(G_SpDb.Specie[GPidx].lfTDData[CP_TMAX],(int)GPidx)*log((G_SpDb.Specie[GPidx].lfTDData[CP_TMAX]/tstart));
		tstart=G_SpDb.Specie[GPidx].lfTDData[CP_TMAX];
		}
	Sval+=G_SpDb.Specie[GPidx].lfTDData[Cp0]*log((t/tstart));
	for(int i=Cp1;i<NO_CPVAL+Cp0;i++){
		Sval+=G_SpDb.Specie[GPidx].lfTDData[i]*pow(t,(double)(i-Cp0))/(double)(i-Cp0);
		Sval-=G_SpDb.Specie[GPidx].lfTDData[i]*pow(tstart,(double)(i-Cp0))/(double)(i-Cp0);
		}
	TotSval+=Sval;
	TotSval*=G_SpDb.Specie[GPidx].lfTDData[MW];
	return(TotSval);//joule/kg-mole-K
}

double GasPhaseModel::GetNetMassFlowIn(int i)
{
	double Min=0.0;
	if(i==0){
		printf("err MASS FLOW\n");
		return(Min);
		}
	int nLNode=i-1;
	int nRNode=i;
	double VelLeft=Vel[CUR][nLNode];
	double VelRight=Vel[CUR][nRNode];
	double ALeft=XS_Area[nLNode];
	double ARight=XS_Area[nRNode];
	double RhoLeft=Rho[CUR][i-1];
	double RhoRight=Rho[CUR][i+1];
	if(i<m_nNoNodes-1){
		if(VelLeft>0.0){
			Min+=RhoLeft*VelLeft*ALeft;
			}
		else {
			Min+=Rho[CUR][i]*VelLeft*ALeft;
			}
		if(VelRight>0.0){
			Min-=Rho[CUR][i]*VelRight*ARight;
			}
		else {
			Min-=RhoRight*VelRight*ARight;
			}
		}
	else {
		if(VelLeft>0.0){
			Min+=RhoLeft*VelLeft*ALeft;
			}
		else {
			Min+=Rho[CUR][i]*VelLeft*ALeft;
			}
		Min-=Rho[CUR][i]*VelLeft*ALeft;
		}
	Min+=GetNetMassInAtSurface(i);
	return(Min);// kg/sec
}

double GasPhaseModel::GetMomentumNetMassFlowIn(int i)
{
	// This mass flow rate calculated is for the Velocity(momentum) Control Volume which is different
	// than the other control volumes used for calc. the energy and specie equs.

	// now the INDEX 'i' refers to the Vel[] array!!!!
	// now the INDEX 'i' refers to the Vel[] array!!!!
	// now the INDEX 'i' refers to the Vel[] array!!!!

	double Min=0.0;
	if(i==m_nNoNodes-1){
		printf("err MOMENTUM MASS FLOW\n");
		return(Min);
		}

	int nLNode=i-1;
	int nRNode=i+1;
	if(i==0){
		nLNode=i;
		}
	if(i==m_nNoNodes-2){
		nRNode=i;
		}
	double VelLeft=(Vel[CUR][nLNode]+Vel[CUR][i])/2.;
	double VelRight=(Vel[CUR][i]+Vel[CUR][nRNode])/2.;
	double ALeft=(XS_Area[nLNode]+XS_Area[i])/2.0;
	double ARight=(XS_Area[i]+XS_Area[nRNode])/2.0;
	double RhoLeft=Rho[CUR][i];
	double RhoRight=Rho[CUR][i+1];

	Min+=VelLeft*RhoLeft*ALeft;
	Min-=VelRight*RhoRight*ARight;
	// Now Average the surface flows at the State Nodes...
	double SurfMassIn=GetNetMassInAtSurface(i);
	SurfMassIn+=GetNetMassInAtSurface(i+1);
	SurfMassIn/=2.;
	Min+=SurfMassIn;
	return(Min);//kg/sec
}

double GasPhaseModel::GetNetMassInAtSurface(int i)
{
	//This is the surface corresponding to the P,T,Rho control volumes
	double SurfMassFlowRateIntoControlVolume=0.0;
	if(i==m_nNoNodes-1 || i==0){
		return(SurfMassFlowRateIntoControlVolume);
		}
	if(m_pplfMoleFluxIntoGas[0]!=NULL){
		for(int j=0;j<m_nNoSurfaceSpecieFluxes;j++){
			SurfMassFlowRateIntoControlVolume+=m_pplfMoleFluxIntoGas[j][i]/1000.0*G_SpDb.Specie[m_uSpIdx[j]].lfTDData[MW]*DelX[i]*m_lfCellWidth;
			}
		}
	return(SurfMassFlowRateIntoControlVolume);//kg/sec
}

double GasPhaseModel::GetNetKgMoleFlowIn(int SpIdx, int i)
{
	double Min=0.0;
	if(i==0){
		printf("err SPECIE FLOW\n");
		return(Min);
		}

	int nLNode=i-1;
	int nRNode=i;
	double VelLeft=Vel[CUR][nLNode];
	double VelRight=Vel[CUR][nRNode];
	double ALeft=XS_Area[nLNode];
	double ARight=XS_Area[nRNode];

	double NDenLeft=Rho[CUR][i-1]/GetMixMW(Sp[CUR][i-1])*Sp[CUR][i-1][SpIdx];
	double NDen=Rho[CUR][i]/GetMixMW(Sp[CUR][i])*Sp[CUR][i][SpIdx];
	double NDenRight=Rho[CUR][i+1]/GetMixMW(Sp[CUR][i+1])*Sp[CUR][i+1][SpIdx];
	if(i<m_nNoNodes-1){
		if(VelLeft>0.0){
			Min+=(NDenLeft)*VelLeft*ALeft;
			}
		else {
			Min+=(NDen)*VelLeft*ALeft;
			}
		if(VelRight>0.0){
			Min-=(NDen)*VelRight*ARight;
			}
		else {
			Min-=(NDenRight)*VelRight*ARight;
			}
		}
	else {
		if(VelLeft>0.0){
			Min+=NDenLeft*VelLeft*ALeft;
			}
		else {
			Min+=(NDen)*VelLeft*ALeft;
			}
		Min-=NDen*VelLeft*ALeft;
		}
	if(i<m_nNoNodes-1){// assume no electrochem at last node
		for(int j=0;j<m_nNoSurfaceSpecieFluxes;j++){
			if(m_pplfMoleFluxIntoGas[j]!=NULL){
				if(SpIdx==(int)m_uSpIdx[j]){
					Min+=m_pplfMoleFluxIntoGas[j][i]/1000.0*DelX[i]*m_lfCellWidth;
					}
				}
			}
		}
	return(Min);// kg-mole/sec
}

double GasPhaseModel::GetNetMomentumFlowIn(int i)
{
	// now the INDEX 'i' refers to the Vel[] array!!!!
	// now the INDEX 'i' refers to the Vel[] array!!!!
	// now the INDEX 'i' refers to the Vel[] array!!!!

	double MOMin=0.0;
	if(i==m_nNoNodes-1){
		printf("err MOMENTUM FLOW\n");
		return(MOMin);
		}

	int nLNode=i-1;
	int nRNode=i+1;
	if(i==0){
		nLNode=i;
		}
	if(i==m_nNoNodes-2){
		nRNode=i;
		}
	double VelLeft=(Vel[CUR][nLNode]+Vel[CUR][i])/2.;
	double VelRight=(Vel[CUR][i]+Vel[CUR][nRNode])/2.;
	double ALeft=(XS_Area[nLNode]+XS_Area[i])/2.0;
	double ARight=(XS_Area[i]+XS_Area[nRNode])/2.0;

	double RhoLeft=Rho[CUR][i];
	double RhoRight=Rho[CUR][i+1];

	MOMin+=RhoLeft*VelLeft*VelLeft*ALeft;
	MOMin-=RhoRight*VelRight*VelRight*ARight;
	MOMin-=GetNetSurfaceMomentumIn(i,true);
	return(MOMin);// kg-m/sec2
}

double GasPhaseModel::GetFrictionForce(int i)
{
	// now the INDEX 'i' refers to the Vel[] array!!!!
	// now the INDEX 'i' refers to the Vel[] array!!!!
	// now the INDEX 'i' refers to the Vel[] array!!!!
	if(i>=m_nNoNodes-1){
		printf("OVERSHOOT ARRAY?");
		}
	double SurfaceForceOnGas=0.0;
	double Ti=GetTemp(i);
	Ti+=GetTemp(i+1);
	Ti/=2.;
	double Mu=GetMixMu(Sp[CUR][i],Ti);
	Mu+=GetMixMu(Sp[CUR][i+1],Ti);
	Mu/=2.;
	double VelAve=Vel[CUR][i];
	double AbsVel=fabs(VelAve);
//	double XS_A=XS_Area[i];
	double lfChanFlowArea=m_ChanGeom.lfHeight*m_ChanGeom.lfChannelWidth;
	double lfPerimeter=(m_ChanGeom.lfHeight+m_ChanGeom.lfChannelWidth)*2.0;//note: we assume square type geometry
	double hyd_diam=4.0*lfChanFlowArea/lfPerimeter;
	double RhoAve=(Rho[CUR][i]+Rho[CUR][i+1])/2.;
	double Re=RhoAve*AbsVel*hyd_diam/Mu;
	double rough=0.01;
	double frictionfactor=get_friction(Re, rough, .06);
	SurfaceForceOnGas-=0.5*frictionfactor*RhoAve*VelAve*AbsVel*DelX[i]*m_ChanGeom.lfNumberOfChannels*lfPerimeter;//area is of the perimeter of the channel x DelX x number of channels
	return(SurfaceForceOnGas);
}


double GasPhaseModel::get_friction(double Re, double rough, double guess)
{
double f=GetTurbulentFriction(Re,rough,guess);
if(Re<UPPERTRANSITION_RE && Re>LOWERTRANSITION_RE){
	//weight the contribution of turbulent and laminar during "transition region"
	f=(UPPERTRANSITION_RE-Re)/(UPPERTRANSITION_RE-LOWERTRANSITION_RE)*16./Re+(Re-LOWERTRANSITION_RE)/(UPPERTRANSITION_RE-LOWERTRANSITION_RE)*f;
	}
else if(Re<LOWERTRANSITION_RE){
	// Laminar
	f=16./Re;
	}
f*=m_lfFlowResistanceGain;// apply a calibration gain factor if user provides it...
return(f);// return Fanning Friction Factor
}

double GasPhaseModel::GetMixMu(double *spXi, double Temp)
{
int j,GPidx;
double mu[MAX_NOSP],mwt[MAX_NOSP],mumix;
double phi,denom;
double GPTemp;

for(j=0;j<m_nNoSp;j++){
	GPidx=j;
	GPTemp=Temp;
	if(Temp<G_SpDb.Specie[GPidx].lfTDData[MU_TMIN]) GPTemp=G_SpDb.Specie[GPidx].lfTDData[MU_TMIN];
	if(Temp>G_SpDb.Specie[GPidx].lfTDData[MU_TMAX]) GPTemp=G_SpDb.Specie[GPidx].lfTDData[MU_TMAX];

	mwt[j]=G_SpDb.Specie[GPidx].lfTDData[MW];
	mu[j]=G_SpDb.getSpecieMu(GPTemp,(int)GPidx);
	if(mu[j]<1.e-20) {
		printf("Bad1");
		return(-1.0);
		}
	}
mumix=0.0;
for(int i=0;i<m_nNoSp;i++){
	denom=0.0;
	if(spXi[i]<DBL_MIN) continue;
	for(j=0;j<m_nNoSp;j++){
		phi = (1.+sqrt((mu[i]/mu[j]))*pow((mwt[j]/mwt[i]),.25));
		phi*=phi;
		phi/=sqrt(8.*(1.+mwt[i]/mwt[j]));
		denom+=spXi[i] * phi;
		}
	if(denom>FLT_MIN) {
		mumix+=spXi[i] * mu[i]/denom;
		}
	}
return(mumix);
}

double GasPhaseModel::GetMixK(double *spXi, double Temp)
{
int j,GPidx;
double mu[MAX_NOSP],k[MAX_NOSP],mwt[MAX_NOSP],kmix;
double phi,denom;
double GPTemp;

/* be consistent in definition of Cp */
for(j=0;j<m_nNoSp;j++){
	GPidx=j;
	GPTemp=Temp;
	if(Temp<G_SpDb.Specie[GPidx].lfTDData[K_TMIN]) GPTemp=G_SpDb.Specie[GPidx].lfTDData[K_TMIN];
	if(Temp>G_SpDb.Specie[GPidx].lfTDData[K_TMAX]) GPTemp=G_SpDb.Specie[GPidx].lfTDData[K_TMAX];

	mwt[j]=G_SpDb.Specie[GPidx].lfTDData[MW];
	mu[j]=G_SpDb.getSpecieMu(GPTemp,(int)GPidx);
	if(mu[j]<1.e-20) {
		printf("Bad2");
		return(-1.0);
		}
	k[j]=G_SpDb.getSpecieK(GPTemp,(int)GPidx);
	if(k[j]<1.e-20){
		printf("Bad3");
		return(-1.0);
		}
	}
kmix=0.0;
for(int i=0;i<m_nNoSp;i++){
	denom=0.0;
	if(spXi[i]<DBL_MIN) continue;
	for(j=0;j<m_nNoSp;j++){
		phi = (1.+sqrt((mu[i]/mu[j]))*pow((mwt[j]/mwt[i]),.25));
		phi*=phi;
		phi/=sqrt(8.*(1.+mwt[i]/mwt[j]));
		denom+=spXi[i] * phi;
		}
	if(denom>FLT_MIN) {
		kmix+=spXi[i] * k[i]/denom;
		}
	}
return(kmix);//watt/m/K
}

int GasPhaseModel::AnalyzePressures()
{
  int i;
	double lfT=0.0;
	for(i=1;i<m_nNoNodes-1;i++){
		double MW_i=GetMixMW(Sp[CUR][i]);
		if(m_bDoQS){
			lfT=GetTFromH(Sp[CUR][i],h[CUR][i]);
			}
		else {
			lfT=GetTFromE(Sp[CUR][i],h[CUR][i]-Vel[CUR][i]*Vel[CUR][i]/2.,T[i]);
			}
		P[i]=Rho[CUR][i]*Runiv/MW_i*1000.0*lfT;
		}
	// now get the last node....this node uses enthalpy for 'h[]'
	double MW_i=GetMixMW(Sp[CUR][i]);
	if(m_bDoQS){
		lfT=GetTFromH(Sp[CUR][i],h[CUR][i]);
		}
	else {
		lfT=GetTFromH(Sp[CUR][i],h[CUR][i]-Vel[CUR][i-1]*Vel[CUR][i-1]/2.);//use upstream node, since we don't have a final node due to offset grid b/t P,T nodes and Vel Nodes.
		}
	P[i]=Rho[CUR][i]*Runiv/MW_i*1000.0*lfT;

	if(!m_bDoQS){
		h[CUR][i]-=P[i]/Rho[CUR][i];// correct back to internal energy (see code on energy equation for dynamic analysis)
		}
	return(1);
}


int GasPhaseModel::AnalyzeTemperatures()
{
  int i;
	for(i=1;i<m_nNoNodes;i++){
		T[i]=GetTemp(i);
		}
	return(1);
}

double GasPhaseModel::GetTFromH(double *sp, double H)
{
	int cnt;
	double t,Hmin,Hmax,Tmin,Tmax,hguess,getabs,Hnorm;

	Tmin=300.0;
	Tmax=1000.0;
	Hmin=GetMixHFromT(sp,Tmin);
	Hmax=GetMixHFromT(sp,Tmax);
	while(Tmin>10.0 && Tmax<100000.0){
		t=(H-Hmin)/(Hmax-Hmin)*(Tmax-Tmin)+Tmin;
		if(t>=Tmin && t<=Tmax) break;
		if(t<Tmin){
			Tmin/=2.0;
			Hmin=GetMixHFromT(sp,Tmin);
			}
		else {
			Tmax*=2.0;
			Hmax=GetMixHFromT(sp,Tmax);
			}
		}

	Hnorm=fabs(Hmax-Hmin); // take difference since H could be negative...
	cnt=0;
	while(cnt<10000){
		cnt++;
		t=(H-Hmin)/(Hmax-Hmin)*(Tmax-Tmin)+Tmin;
		hguess=GetMixHFromT(sp,t);
		if(hguess>Hmax){
			Hmin=Hmax;
			Tmin=Tmax;
			Hmax=hguess;
			Tmax=t;
			}
		else if(hguess<Hmin){
			Hmax=Hmin;
			Tmax=Tmin;
			Hmin=hguess;
			Tmin=t;
			}
		else {
			if((hguess-H)>0.0){
				Hmax=hguess;
				Tmax=t;
				}
			else{
				Hmin=hguess;
				Tmin=t;
				}
			}
		getabs=fabs(H-hguess);
		getabs/=Hnorm;
		if(getabs<0.00000001) break;
		}
	if(cnt>=10000){
		printf("BAD CONV.\n");
		}
	return(t);
	}

double GasPhaseModel::GetMixHFromT(double *sp, double Tguess)
{
	double mixH=0.0;
	for(int j=0;j<m_nNoSp;j++){
		if(sp[j]>0.0){
			mixH+=sp[j]*G_SpDb.Specie[j].lfTDData[MW]*hSp(j,Tguess);
			}
		}
	mixH/=GetMixMW(sp);
	return(mixH);//joule/kg
}

double GasPhaseModel::GetTemp(int i)
{
	double MW_i=GetMixMW(Sp[CUR][i]);
	return(P[i]/Rho[CUR][i]/Runiv/1000.0*MW_i);
}

double GasPhaseModel::GetPressureForce(int i)
{
	// now the INDEX 'i' refers to the Vel[] array!!!!
	// now the INDEX 'i' refers to the Vel[] array!!!!
	// now the INDEX 'i' refers to the Vel[] array!!!!
	double PForce=0.0;
	double PLeft,PRight,XSLeft,XSRight;
	if(i==m_nNoNodes-1){
		printf("Err PForce");
		return(PForce);
		}
	else if(i==0){
		XSRight=(XS_Area[i]+XS_Area[i+1])/2.;
		XSLeft=XS_Area[i];
		}
	else if(i==m_nNoNodes-2){
		XSLeft=(XS_Area[i-1]+XS_Area[i])/2.;
		XSRight=XS_Area[i];
		}
	else{
		XSLeft=(XS_Area[i-1]+XS_Area[i])/2.;
		XSRight=(XS_Area[i]+XS_Area[i+1])/2.;
		}
	PRight=P[i+1];
	PLeft=P[i];

	double P_A_in=PLeft*XSLeft;
	double P_A_out=PRight*XSRight;
	double P_A_cv=(PLeft+PRight)/2.*(XSRight-XSLeft);//this is surface area force due to boundary pressure
	PForce=P_A_in-P_A_out+P_A_cv;
	return(PForce);
}


void GasPhaseModel::PrintData(FILE * f, bool bDoHeader, char *szType, UINT uNoSp, UINT *uSpID)
{
	int i;
	UINT j;
	int nNoVelNodes=m_nNoNodes-1;
	if(m_bDoQS){
		// only for QS are there same Vel nodes as P,T nodes
		nNoVelNodes=m_nNoNodes;
		}
	if(bDoHeader){
		for(i=0;i<m_nNoNodes;i+=PRINT_STEP){
			fprintf(f,"%s P%d,",szType,i);
			}
		for(i=0;i<nNoVelNodes;i+=PRINT_STEP){
			fprintf(f,"%s Vel%d,",szType,i);
			}
		for(j=0;j<uNoSp;j++){
			for(i=0;i<m_nNoNodes;i+=PRINT_STEP){
				fprintf(f,"%s Sp%s%d,",szType,SpecieNames[uSpID[j]],i);
				}
			}
		for(i=0;i<m_nNoNodes;i+=PRINT_STEP){
			fprintf(f,"%s T%d,",szType,i);
			}
		for(i=0;i<m_nNoNodes;i+=PRINT_STEP){
			fprintf(f,"%s QDotCv%d,",szType,i);
			}
		}
	else {
		for(i=0;i<m_nNoNodes;i+=PRINT_STEP){
			fprintf(f,"%le,",P[i]);
			}
		for(i=0;i<nNoVelNodes;i+=PRINT_STEP){
			fprintf(f,"%le,",Vel[CUR][i]);
			}
		for(j=0;j<uNoSp;j++){
			for(i=0;i<m_nNoNodes;i+=PRINT_STEP){
				fprintf(f,"%le,",Sp[CUR][i][uSpID[j]]);
				}
			}
		for(i=0;i<m_nNoNodes;i+=PRINT_STEP){
			fprintf(f,"%le,",GetTemp(i));
			}
		for(i=0;i<m_nNoNodes;i+=PRINT_STEP){
			fprintf(f,"%le,",m_lfQdot[i]);
			}
		}
}



double GasPhaseModel::GetMinTimeStep()
{
	// Here we apply a few equations to estimate a 'good' time step
	static	double lfLastTimeStep=0.0000000001;
	double minT=1.0E+10;
	if(!m_bDoQS){
		int i;
		for(i=1;i<m_nNoNodes;i++){
			// Energy Equation Limit...
			double Hin=GetNetIntEnergyFlowIn(i);
			double Min=GetNetMassFlowIn(i);
			double lfHH=h[CUR][i];
			if(i==m_nNoNodes-1){
				lfHH=h[CUR][i]+P[i]/Rho[CUR][i];
				}
			double XS_A=XS_Area[i-1];
			if(i<m_nNoNodes-1){
				XS_A+=XS_Area[i];
				XS_A/=2.;
				}
			double Qdot=0.0;
			if(m_lfHeatRateAddedUpperWall!=NULL){
				Qdot=m_lfHeatRateAddedUpperWall[i] + m_lfHeatRateAddedLowerWall[i];
				}
			double Ti=GetTemp(i);
			double MWi=GetMixMW(Sp[CUR][i]);// just take properties at closest node
			double delH=GetMixCp(Sp[CUR][i],Ti)-Runiv/MWi*1000.0;//joule/kg change for 1 deg. K, i.e., this allows each time step to change T by 1 deg;
			delH*=10.0;//Allow 10K temp change at any time step!
			delH*=Rho[CUR][i]*(XS_A*DelX[i]);
			double lfHtime=delH/(Hin-lfHH*Min+Qdot+0.000001);// don't div. by zero
			lfHtime=fabs(lfHtime);

			if(lfHtime<minT){
				minT=lfHtime;
				}
			}
		// now the INDEX 'i' refers to the Vel[] array!!!!
		for(i=0;i<m_nNoNodes-1;i++){
			// Speed of Sound Limit...
			double t=10000000000.0;
			double Ti=GetTemp(i);
			double MW=GetMixMW(Sp[CUR][i]);// just take properties at closest node
			double MolarCp=GetMixCp(Sp[CUR][i],Ti)*MW/1000.0;//joule/gm-mole-K
			double MolarCv=MolarCp-Runiv;
			double gamma=MolarCp/MolarCv;
			double Speed=sqrt(gamma*P[i]/Rho[CUR][i]);
			Speed+=fabs(Vel[CUR][i]);
			t=DelX[i]/Speed;// we'll divide by extra amount below...

			if(t<minT){
				minT=t;
				}

			//Mom. equ. limit
			double MOMin=GetNetMomentumFlowIn(i);
			double Min=GetMomentumNetMassFlowIn(i);
			double SurfaceForceOnGas=GetFrictionForce(i);
			SurfaceForceOnGas+=GetPressureForce(i);
			double RhoAve=(Rho[CUR][i]+Rho[CUR][i+1])/2.;
			double delVel=0.1;// max change in Vel permitted in one step m/s
			delVel*=RhoAve*(XS_Area[i]*DelX[i]);
			double Momt=delVel/(MOMin+SurfaceForceOnGas-Vel[CUR][i]*Min+0.0000001);//don't div. by zero
			Momt=fabs(Momt);

			if(Momt<minT){
				minT=Momt;
				}
			}
		lfLastTimeStep=minT;
		}
	return(minT);
}

void GasPhaseModel::UpdateBoundaryValues(double lfAnodePInput, \
	double lfAnodePExit, double lfAnodeTInput,\
	int nNoSp, double * lfAnodeSpInput)
{
  int i;
	if(nNoSp!=m_nNoSp){
		printf("Bad boundary update.");
		return;
		}
	for(i=0;i<m_nNoSp;i++){
		Sp[CUR][0][i]=lfAnodeSpInput[i];
		}
	if(m_bDoFixedFlow){
		Vel[CUR][0]=m_lfVelInput;
		}
	P[0]=lfAnodePInput;
	double tst=GetMixMW(Sp[CUR][0]);
	Rho[CUR][0]=P[0]/Runiv/1000.0/lfAnodeTInput*tst;
	Rho[NXT][0]=Rho[CUR][0];// need to do this too cuz we may only go one calculation during QS
	if(m_bDoQS){
		// (h[]=enthalpy)
		h[CUR][0]=GetMixHFromT(Sp[CUR][0],lfAnodeTInput);
		}
	else {
		// (h[]=e+KE) except for last node....
		h[CUR][0]=GetMixHFromT(Sp[CUR][0],lfAnodeTInput)-P[0]/Rho[CUR][0]+Vel[CUR][0]*Vel[CUR][0]/2.;
		}
	h[NXT][0]=h[CUR][0];
	if(!m_bDoFixedFlow){
		// fix up the exit node....
		P[m_nNoNodes-1]=lfAnodePExit;
		}
	Rho[CUR][m_nNoNodes-1]=P[m_nNoNodes-1]/Runiv/1000.0/T[m_nNoNodes-1]*GetMixMW(Sp[CUR][m_nNoNodes-1]);
}


double GasPhaseModel::GetTFromE(double * Sp, double lfIntEnergy, double Ttry)
{
		double MW_i=GetMixMW(Sp);
		double Tguess=Ttry;//start with previous T
		if(Tguess<=0.0) Tguess=500.0;
		double lfStep=Tguess/50.0;
		bool bUpperBound=false;
		bool bLowerBound=false;
		double lfLower,lfUpper;
		while(1){
			double lfh=GetMixHFromT(Sp,Tguess);
			lfh*=MW_i/1000.0;//joule/gm-mole
			double lfe=lfh-Runiv*Tguess;//internal energy
			lfe/=(MW_i/1000.0);//joule/kg

			double DiffE=lfe-lfIntEnergy;
			double DiffT=lfStep/Tguess;
			DiffT=fabs(DiffT);
			double AbsStep=fabs(DiffE/lfIntEnergy);
			if(AbsStep<0.000000001 || DiffT<0.00000000001) {
				break;
				}

			if(bUpperBound&&bLowerBound){
				lfStep=Tguess;
				if(DiffE<0.0){
					lfLower=Tguess;
					Tguess=(lfUpper+lfLower)/2.;
					}
				else {
					lfUpper=Tguess;
					Tguess=(lfUpper+lfLower)/2.;
					}
				lfStep-=Tguess;
				}
			else {
				if(DiffE<0.0 && lfStep<0.0){
					lfStep*=-0.33333;
					bLowerBound=true;
					lfLower=Tguess;
					}
				else if(DiffE>0.0 && lfStep>0.0){
					lfStep*=-0.3333;
					bUpperBound=true;
					lfUpper=Tguess;
					}
				else {
					if(fabs(lfStep*5.0)<Tguess){
						lfStep*=2.0;
						}
					else{
						lfStep=fabs(lfStep)/lfStep*Tguess/20.0;
						}
					}
				Tguess+=lfStep;
				}
			}
		return(Tguess);
}


void GasPhaseModel::AnalyzePressureEqu(double lfTimeStep)
{
  int i;
	for(i=1;i<m_nNoNodes;i++){
		double Hin=GetNetIntEnergyFlowIn(i);
		double Min=GetNetMassFlowIn(i);

		double XS_A=XS_Area[i-1];
		if(i<m_nNoNodes-1){
			XS_A+=XS_Area[i];
			XS_A/=2.;
			}
		double DKE=Rho[CUR][i]*(Vel[NXT][i]*Vel[NXT][i]-Vel[CUR][i]*Vel[CUR][i])/2.;
		DKE+=Vel[CUR][i]*Vel[CUR][i]/2.*Min/(XS_A*DelX[i])*lfTimeStep;
		double Cp=GetMolarCp(Sp[CUR][i],T[i]);
		Pnew[i]=P[i]+(lfTimeStep/(XS_A*DelX[i])*Hin - DKE)*Runiv/(Cp-Runiv);
		}
}

double GasPhaseModel::GetMolarCp(double * Sp, double T)
{
	double MW=GetMixMW(Sp);
	double Cpkg=GetMixCp(Sp,T);
	return(Cpkg*MW/1000.0);//joule/gm-moleK
}

/*
void GasPhaseModel::Serialize(CArchive & ar)
{
	int i,j;
	if (ar.IsStoring())	{
		ar<<(int)m_nNoNodes;
		ar<<(int)m_nNoSp;
		for(i=0;i<m_nNoNodes;i++){
			ar<<P[i];
			ar<<T[i];
			ar<<h[CUR][i];
			ar<<Rho[CUR][i];
			ar<<Vel[CUR][i];
			ar<<Pnew[i];
			ar<<Psave[i];
			for(j=0;j<m_nNoSp;j++){
				ar<<Sp[CUR][i][j];
				}
			}
		}
	else {
		int w;
		ar>>w;	m_nNoNodes=(int)w;
		ar>>w;	m_nNoSp=(int)w;
		for(i=0;i<m_nNoNodes;i++){
			ar>>P[i];
			ar>>T[i];
			ar>>h[CUR][i];
			ar>>Rho[CUR][i];
			ar>>Vel[CUR][i];
			ar>>Pnew[i];
			ar>>Psave[i];
			for(j=0;j<m_nNoSp;j++){
				ar>>Sp[CUR][i][j];
				}
			}
		}
}
*/

double GasPhaseModel::GetSpecieExitFlowAtNode(int SpIdx, int Node)
{
	// Here, for dynamic gas analysis, the Node revers to the P and T type Nodes, not Vel nodes which are between the PT nodes...
	// In other ints, you can use this if you are looking for the flow rate of
	// componenets that cross the VEL node
	// If you do quasi-steady gas flow, then Vel node and P,T nodes are all aligned.
	int VelNode=Node;
	if(Node>=m_nNoNodes-1 && !m_bDoQS){
		if(Node>=m_nNoNodes){
			printf("err");
			}
		VelNode=m_nNoNodes-2;
		Node=m_nNoNodes-1;
		}
	double NDenLeft=Rho[CUR][Node]/GetMixMW(Sp[CUR][Node])*Sp[CUR][Node][SpIdx];
	double NDot=NDenLeft*Vel[CUR][VelNode]*XS_Area[VelNode];
	return(NDot);//kg-mole/sec for entire CELL, not just the channel
}

double GasPhaseModel::GetMassFlowOut(int nNode)
{
  int i;
	double sum=0.0;
	if(nNode>=m_nNoNodes){
		nNode=m_nNoNodes-1;
		}
	for(i=0;i<m_nNoSp;i++){
		sum+=GetSpecieExitFlowAtNode(i, nNode)*G_SpDb.Specie[i].lfTDData[MW];//kg/sec
		}
	return(sum);//kg/sec
}

double GasPhaseModel::GetTurbulentFriction(double Re, double rough, double guess)
{
double tmp1,tmp2,fsqroot,Refact,rfact;
/* This routine calcs. the Fanning Friction Factor NOT THE DARCY!!! */
/* Re is the Reynolds number */
/* rough is the roughness defined as roughness of surface/diameter */
/* see page 255 of Zucrow and hoffman */
	if(guess<0.0) fsqroot=.0797;	/* this is the sqareroot of .025/4. which seems to be a good place to start */
	else fsqroot=sqrt(guess);

	Refact=1.255/Re;
	rfact=rough/3.7;

	while(1){
		tmp1= - 4.*log10(rfact + Refact/fsqroot); /* equation 5.27 of zucrow */
		tmp2=1./tmp1;
		tmp1=fabs(tmp2-fsqroot);
		if(tmp1<0.00001) break;
		fsqroot=tmp2;
		}
	return((tmp2*tmp2)); // return Fanning Friction Factor

}

double GasPhaseModel::GetMixIntEnergyFromT(double *sp, double Temp)
{
	double mixE=0.0;
	for(int j=0;j<m_nNoSp;j++){
		if(sp[j]>0.0){
			mixE+=sp[j]*G_SpDb.Specie[j].lfTDData[MW]*hSp(j,Temp)/1000.0;
			}
		}
	mixE-=Runiv*Temp;
	return(mixE);//joule/gm-mole
}

double GasPhaseModel::GetGibbsFreeEnergy_WaterGasShift(double Ti)
{
	// we assume direction of from CO to CO2 for the 'change'....
	double MolarDelHStd=hSp(CO2,Ti)*G_SpDb.Specie[CO2].lfTDData[MW]+hSp(H2,Ti)*G_SpDb.Specie[H2].lfTDData[MW];
	MolarDelHStd-=(hSp(H2O,Ti)*G_SpDb.Specie[H2O].lfTDData[MW]+hSp(CO,Ti)*G_SpDb.Specie[CO].lfTDData[MW]);//joule/kg-mole
	double MolarDelSStd=MolarEntropySp(CO2,Ti,101000.0)+MolarEntropySp(H2,Ti,101000.0);
	MolarDelSStd-=(MolarEntropySp(CO,Ti,101000.0)+MolarEntropySp(H2O,Ti,101000.0));//joule/kg-mole-CO-K at T -- Note, this does not include the entropy of mixing!!!
	double MolarDelGStd=MolarDelHStd-Ti*MolarDelSStd;//delG per kg-mole of CO
	MolarDelGStd/=1000.0;//delG per g-mole of CO
	return(MolarDelGStd);//delG per g-mole of CO
}


double GasPhaseModel::GetGibbsFreeEnergy_Reform(double Ti)
{
	// we assume direction of from CO to CO2 for the 'change'....
	double MolarDelHStd=hSp(CO,Ti)*G_SpDb.Specie[CO].lfTDData[MW] + 3.0*hSp(H2,Ti)*G_SpDb.Specie[H2].lfTDData[MW];
	MolarDelHStd-=(hSp(H2O,Ti)*G_SpDb.Specie[H2O].lfTDData[MW] + hSp(CH4,Ti)*G_SpDb.Specie[CH4].lfTDData[MW]);//joule/kg-mole
	double MolarDelSStd=MolarEntropySp(CO,Ti,101000.0) + MolarEntropySp(H2,Ti,101000.0);
	MolarDelSStd-=(MolarEntropySp(CH4,Ti,101000.0) + MolarEntropySp(H2O,Ti,101000.0));//joule/kg-mole-CO-K at T -- Note, this does not include the entropy of mixing!!!
	double MolarDelGStd=MolarDelHStd-Ti*MolarDelSStd;//delG per kg-mole of CO
	MolarDelGStd/=1000.0;//delG per g-mole of CO
	return(MolarDelGStd);//delG per g-mole of CO
}

void GasPhaseModel::DoublePressDrop_Vel()
{
  int i;
	// the goal here is to enable a change in the pressure and vel that get you close to 
	// a final SS value...
	for(i=0;i<m_nNoNodes;i++){
		double delP=P[i]-P[m_nNoNodes-1];
		P[i]=P[m_nNoNodes-1]+delP*2.0;
		Vel[CUR][i]*=2.0;
		}
}

int GasPhaseModel::AnalyzeNodeQSGasPhysics(int i, double lfCurrDen)
{
	if(i<1) return(QS_ERROR);
	i--;// let 'i' be the reference to the upsteam node...

	if(m_bDoFixedFlow){
		Vel[CUR][0]=m_lfVelInput;// update regardless of 'i'
		}
	double &P1=P[i];
	double &P2=P[i+1];
	double &Rho1=Rho[CUR][i];
	double &Rho2=Rho[CUR][i+1];
	double &Vel1=Vel[CUR][i];
	const double &A1=XS_Area[i];
	const double &A2=XS_Area[i+1];
	double MW1=GetMixMW(Sp[CUR][i]);
	double Rgas1=Runiv/MW1*1000.0;// joule/kg-K
	double T1=(P1)/Rho1/Rgas1;

	double mdot1=Rho1*Vel1*A1;
	
	double CH4ProdRate=0.0;
	double Hin=0.0;
	double &CH4mol=Sp[CUR][i+1][CH4];
	double &COmol=Sp[CUR][i+1][CO];
	double &H2Omol=Sp[CUR][i+1][H2O];
	double &CO2mol=Sp[CUR][i+1][CO2];
	double &H2mol=Sp[CUR][i+1][H2];

	SetFlowsFromCurrent(i+1,lfCurrDen);
	if(!QSUpdateOfXi2(i,CH4ProdRate)){
		// must have had too much current draw--force less current draw...
		return(QS_LOWER_CURRENT);
		}
	double mdot_surface=GetNetMassInAtSurface(i+1);
	double mdot2=mdot1+mdot_surface;

	double Told=T1;
	double MW2=GetMixMW(Sp[CUR][i+1]);
	double Rgas2=Runiv/MW2*1000.0; // joule/kg-K

	bool bUpperBoundTemp=false;
	bool bLowerBoundTemp=false;
	double lfLowerTemp=GetMinTemp();
	double lfUpperTemp=GetMaxTemp();

	double DelTemp=0.0;
	double T2=((P2)/Rho2/Rgas2);
	double lfStepTemp=T2/500.0;
	if(T2>lfUpperTemp){
		T2=lfUpperTemp*0.9999999;
		}

	double VelOld=Vel[CUR][i+1];
	double AbsDelT=100.0;
	double T2PreviousPass=lfLowerTemp;
	double lfSurfEnthalpy=0.0;
	while(1){
		double AbsDelT2=100.0;
		double T2Old=T[i+1];//100.0;
		double Vel2;
		double V2old=Vel[CUR][i+1];
		Vel[NXT][i+1]=Vel[CUR][i+1];
		double Rho2Old=Rho2;
		int cnt=0;
		while(AbsDelT2>0.000001){
			// first need to let the system of equs. converged on the solution for the guessed temperature
			cnt++;
			AnalyzeNodeSurfaceHeat(i+1,T2);
			double Qdot=m_lfHeatRateAddedUpperWall[i+1]+m_lfHeatRateAddedLowerWall[i+1];
			m_lfQdot[i+1]=Qdot;
			MW2=GetMixMW(Sp[CUR][i+1]);
			Rgas2=Runiv/MW2*1000.0; // joule/kg-K
			T[i+1]=T2;// reset with T2 (the guessed value)
			Rho2=P2/Rgas2/T2;//reset with updated T2 guessed value
			Vel[CUR][i+1]=(Vel[CUR][i+1]+V2old)/2.0;
			V2old=Vel[NXT][i+1];
			double Cp2=GetMixCp(Sp[CUR][i+1],T2);
			double gam=Cp2/Rgas2;

			// h[] is now enthalpy at ALL nodes!!!!!
			lfSurfEnthalpy=GetNetSurfaceEnergyFlowIn(i+1,T2);
			double RHS_E=mdot1*(h[CUR][i] + Vel1*Vel1/2.0) + Qdot + lfSurfEnthalpy;
			double SurfaceForceOnGas=GetFrictionForce(i);
			// the following equ. assumes fixed area duct....
			double RHS_Mom=P1*A1 + mdot1*Vel1 + SurfaceForceOnGas + GetNetSurfaceMomentumIn(i,true);

			double A=-gam*RHS_Mom;
			double ho=GetMixHFromT(Sp[CUR][i+1],T1);// we've expanded h2 to be = (enthalpy at T1) + Cp2(T2-T1)
			double B=gam*RHS_Mom*gam*RHS_Mom - 4.0*mdot2*mdot2*gam*(Cp2*T1-ho) - 4.0*mdot2*gam*RHS_E + 2.0*mdot2*mdot2*(Cp2*T1-ho) + 2.0*mdot2*RHS_E;
			Vel2=(A+sqrt(B))*1.0/(2.0*(mdot2/2.0-mdot2*gam));
			if(Vel2>0.0){
				Vel[CUR][i+1]=Vel2;
				Vel[NXT][i+1]=Vel2;
				Rho2=mdot2/Vel2/A2;
				Rho[NXT][i+1]=Rho2;
				P2=(RHS_Mom-mdot2*Vel2)/A2;
				T[i+1]=P2/Rgas2/Rho2;
				AbsDelT2=(T2Old-T[i+1])/T2Old;
				AbsDelT2=fabs(AbsDelT2);
				T2Old=T[i+1];
				}
			else {
				bUpperBoundTemp=true;
				lfUpperTemp=T2;
				if(bUpperBoundTemp&&bLowerBoundTemp){
					T2=(lfUpperTemp+lfLowerTemp)/2.0;
					if((lfUpperTemp-lfLowerTemp)/lfLowerTemp<0.00000000000001){
						lfUpperTemp+=5.12135266341;
						lfLowerTemp-=5.66352341435;
						}
					}
				else{
					if(fabs(T2PreviousPass-T2)<0.00000001){
						T2PreviousPass*=0.97;
						}
					T2=(T2+T2PreviousPass)/2.0;
					if(T2<lfLowerTemp){// only gradually drop
						T2=lfLowerTemp;
						lfLowerTemp-=10.0;
						}
					lfStepTemp=-T2/1000.0;
					}
				}
			}
		h[CUR][i+1]=GetMixHFromT(Sp[CUR][i+1],T[i+1]);
		h[NXT][i+1]=h[CUR][i+1];
		T2PreviousPass=T2;
		DelTemp=T[i+1]-T2;
		double DelVel=(Vel2-VelOld)/VelOld;
		double AbsDelVel=fabs(DelVel);
		if(AbsDelVel<0.00000003){
			break;
			}
		VelOld=Vel2;
		AbsDelT=fabs(DelTemp/T2);
		if(AbsDelT<0.0000001){
			break;
			}

		if(bUpperBoundTemp&&bLowerBoundTemp){
			if(DelTemp>0.0){
				lfLowerTemp=(T2+lfLowerTemp)/2.0;//don't drive too hard to the middle--do a double average
				T2=(lfUpperTemp+lfLowerTemp)/2.0;
				}
			else {
				lfUpperTemp=(T2+lfUpperTemp)/2.0;
				T2=(lfUpperTemp+lfLowerTemp)/2.0;
				}
			if((lfUpperTemp-lfLowerTemp)/lfLowerTemp<0.00000000000001){
				lfUpperTemp+=5.12135266341;
				lfLowerTemp-=5.66352341435;
				}
			continue;
			}
		else {
			if(DelTemp<0.0){
				bUpperBoundTemp=true;
				lfUpperTemp=T2;
				}
			else{
				bLowerBoundTemp=true;
				lfLowerTemp=T2;
				}
			if(DelTemp<0.0 && lfStepTemp>0.0){
				lfStepTemp*=-0.33333;
				}
			else if(DelTemp>0.0 && lfStepTemp<0.0){
				lfStepTemp*=-0.3333;
				}
			else {
				if(fabs(lfStepTemp*5.0)<T2){
					lfStepTemp*=2.0;
					}
				else{
					lfStepTemp=fabs(lfStepTemp)/lfStepTemp*T2/5.0;
					}
				}
			if(T2+lfStepTemp>lfUpperTemp){
				lfStepTemp=lfUpperTemp-(T2+lfUpperTemp)/2.0;
				}
			T2+=lfStepTemp;
			}
		}
	return(QS_GOOD);
}

int GasPhaseModel::AnalyzeQSGasPhysics(double *pCurrDen)
{
double AnPExitSave=P[m_nNoNodes-1];
double lfAnStep=Vel[CUR][0]/1000.0;
bool bAnUpperBound=false;
bool bAnLowerBound=false;
bool bAnHitAbsoluteLowerVelLimit=false;
double lfAnLower=-1.0,lfAnUpper=-1.0,lfAnLowerSideDelForce=-1.0,lfAnUpperSideDelForce=-1.0;

double lfOldAnVel=-1.0;
double lfOldAnDelForce=1.0;

int i;

while(1){
	bool bAnOverShootingDelP=false;
	for(i=1;i<m_nNoNodes-1;i++){
		if(pCurrDen!=NULL){
			AnalyzeNodeQSGasPhysics(i, pCurrDen[i]);
			}
		else{
			AnalyzeNodeQSGasPhysics(i, 0.0);
			}

		if(HitPDropLimit(i,AnPExitSave) && !bFixedFlow()){
			lfAnUpper=Vel[CUR][0];
			bAnUpperBound=true;
			bAnOverShootingDelP=true;
			}
		if(bAnOverShootingDelP) break;
		}

	if(bAnOverShootingDelP){
		if(bAnHitAbsoluteLowerVelLimit){
			printf("We apparently have requested to high a voltage than what can be provided given the Specified DelP and Load Resistance\n");
			break;
			}
		}
	else {
		// now calc. last node, but assume no internal reforming going on, cuz there is no heat transfer out there anyway as is assumed at exit node
		AnalyzeNodeQSGasPhysics(i, 0.0);
		}

	double AnDelForce=(P[m_nNoNodes-1] - AnPExitSave)/(AnPExitSave+0.0000001);
	double AnAbsdelForce=fabs(AnDelForce);
	if(AnAbsdelForce<0.000000001 && !bAnOverShootingDelP) {
		break;
		}
	if(bFixedFlow()){
		AnPExitSave=P[m_nNoNodes-1];
		}
	else{
		if(bAnUpperBound&&bAnLowerBound && i==(m_nNoNodes-1)){
			if(AnDelForce>0.0){
				if(Vel[CUR][0]>lfAnLower){
					lfAnLower=Vel[CUR][0];
					}
				}
			else {
				if(Vel[CUR][0]<lfAnUpper){
					lfAnUpper=Vel[CUR][0];
					}
				}
			lfAnStep=-(Vel[CUR][0]-lfOldAnVel)/(AnDelForce-lfOldAnDelForce)*AnDelForce/1.1;
			if(fabs(lfAnStep)<1.0E-20){
				lfAnStep=lfOldAnVel/1.0E5;
				lfAnUpper+=fabs(lfAnStep);
				lfAnLower-=fabs(lfAnStep);
				}
			lfOldAnVel=Vel[CUR][0];
			lfOldAnDelForce=AnDelForce;
			Vel[CUR][0]+=lfAnStep;
			if(Vel[CUR][0]>lfAnUpper){
				Vel[CUR][0]=(Vel[CUR][0]*0.3+0.7*lfAnUpper);
				lfAnUpper=Vel[CUR][0];
				}
			if(Vel[CUR][0]<lfAnLower){
				Vel[CUR][0]=(Vel[CUR][0]*0.3+0.7*lfAnLower);
				lfAnLower=Vel[CUR][0];
				}
			if(Vel[CUR][0]<0.0){
				Vel[CUR][0]=lfAnLower/2.0;
				}
		
			lfAnStep=(lfAnUpper-lfAnLower)/5.0;
			if((lfAnUpper-lfAnLower)/lfAnUpper<0.0000000001){
				bAnUpperBound=false;
				bAnLowerBound=false;
				lfAnStep=lfAnUpper/10000.0;
				lfOldAnVel=-1.0;//trip to avoid gradient step routines
				}	
			}
		else {
			if(bAnOverShootingDelP){
				lfAnUpperSideDelForce=AnDelForce;
				bAnUpperBound=true;
				lfAnUpper=Vel[CUR][0];
				if(bAnLowerBound){
					lfAnStep=-Vel[CUR][0]+(Vel[CUR][0]+lfAnLower)/2.0;
					}
				else {
					lfAnStep=-0.25*Vel[CUR][0];
					}
				Vel[CUR][0]+=lfAnStep;
				}
			else if(i==(m_nNoNodes-1)){
				if(AnDelForce<0.0){
					lfAnUpperSideDelForce=AnDelForce;
					bAnUpperBound=true;
					lfAnUpper=Vel[CUR][0];
					}
				else {
					lfAnLowerSideDelForce=AnDelForce;
					bAnLowerBound=true;
					lfAnLower=Vel[CUR][0];
					}
				if(lfOldAnVel>0.0){
					//the deltaP vs Vel curve is too non-linear to take too big a jump
					// so take a 90% wt on the last step....
					lfAnStep=lfAnStep*0.9-0.1*(Vel[CUR][0]-lfOldAnVel)/(AnDelForce-lfOldAnDelForce)*AnDelForce/5.0;
					}
				else {
					if(AnDelForce<0.0 && lfAnStep>0.0){
						lfAnStep*=-0.33333;
						}
					else if(AnDelForce>0.0 && lfAnStep<0.0){
						lfAnStep*=-0.3333;
						}
					else {
						if(fabs(lfAnStep*5.0)<Vel[CUR][0]){
							lfAnStep*=2.0;
							}
						else{
							lfAnStep=fabs(lfAnStep)/lfAnStep*Vel[CUR][0]/5.0;
							}
						}
					}
				lfOldAnVel=Vel[CUR][0];
				lfOldAnDelForce=AnDelForce;
				Vel[CUR][0]+=lfAnStep;
				}
			}
		}//End of Anode Check
	}
Vel[NXT][0]=Vel[CUR][0];
for(i=0;i<m_nNoSp;i++){
	Sp[NXT][0][i]=Sp[CUR][0][i];
	}
P[m_nNoNodes-1]=AnPExitSave;
return(QS_GOOD);
}

bool GasPhaseModel::QSUpdateOfXi2(int i, double CH4ProdRate)
{
	// we are using known info. at 'i' to solve for info. at 'i+1'....
	// 'i' corresponds to p, rho, T node and the transfer surface
	if(!(i<m_nNoNodes-1)){
		return(true);
		}
	int j;
	int nLNode=i;
	int nRNode=i+1;
	double VelLeft=Vel[CUR][nLNode];
	double ALeft=XS_Area[nLNode];
	double lfTotConc=0.0;
	double CjNew[MAX_NOSP];
	double MWT=GetMixMW(Sp[CUR][nLNode]);
	for(j=0;j<m_nNoSp;j++){
		double SpMolein=0.0;//Kg-MOLE/SEC UNITS
		double SpMolLeft=Sp[CUR][nLNode][j]*Rho[CUR][nLNode]/MWT;//kg-mole/m3
		SpMolein=VelLeft*SpMolLeft*ALeft;
		for(int k=0;k<m_nNoSurfaceSpecieFluxes;k++){
			if(m_pplfMoleFluxIntoGas[k]!=NULL){
				if(j==(int)m_uSpIdx[k]){
					SpMolein+=m_pplfMoleFluxIntoGas[k][i+1]/1000.0*DelX[i]*m_lfCellWidth;
					}
				}
			}

		CjNew[j]=SpMolein;
		if(CjNew[j]<0.0){
			printf("BAD SPECIE CONC.\n");
			return(false);
			}
		lfTotConc+=CjNew[j];
		}

	//now make sure that species fractions stay summed to 1.0
	for(j=0;j<m_nNoSp;j++){
		Sp[CUR][i+1][j]=CjNew[j]/lfTotConc;
		Sp[NXT][i+1][j]=Sp[CUR][i+1][j];
		}
	return(true);
}

double GasPhaseModel::GetNetSurfaceEnergyFlowIn(int i,double Tgas)
{
	double Hin=0.0;
	if(i<m_nNoNodes-1){// assume no electrochem at last node for Dynamic Calculations, and there is NO such node for Full Quasi-Steady!!
		for(int j=0;j<m_nNoSurfaceSpecieFluxes;j++){
			// NOW NEED TO USE ENTHALPY CUZ ADDITION OF MASS ALSO HAS
			// A Pv TERM!!
			if(m_pplfMoleFluxIntoGas[j]!=NULL){
				if(m_pplfMoleFluxIntoGas[j][i]>0.0 && m_plfElecTemp[MAX_AXIALNODES*CUR+i]!=NULL){ // if m_plfElecTemp==NULL then must not be doing electrochem model, but a HXGR, etc.
					Hin+=m_pplfMoleFluxIntoGas[j][i]/1000.0*G_SpDb.Specie[m_uSpIdx[j]].lfTDData[MW]*hSp(m_uSpIdx[j],*m_plfElecTemp[MAX_AXIALNODES*CUR+i])*DelX[i]*m_lfCellWidth;
					}
				else {
					Hin+=m_pplfMoleFluxIntoGas[j][i]/1000.0*G_SpDb.Specie[m_uSpIdx[j]].lfTDData[MW]*hSp(m_uSpIdx[j],Tgas)*DelX[i]*m_lfCellWidth;
					}
				}
			}
		}
	return(Hin); // joule/sec
}

double GasPhaseModel::GetNetSurfaceMomentumIn(int i, bool bDoDynamicAnal)
{
	// 'i' is the Vel array
	// for species going from gas phase to surface, assume that they
	// cause a loss in momentum flow.  for species going from surface
	// to the gas phase, no X momentum is provided, the molecules
	// simply go straight up.
	double SurfMassFlowRateOutofControlVolume=0.0;
	double MOMin=0.0;
	if(i<m_nNoNodes-1){// Remember, there is NO existance of a VEL node at (m_nNoNodes-1).  Vel nodes have one less node than PVT nodes...
		int j;
		if(bDoDynamicAnal){
			// now average the surface flows around the Vel Node...
			for(j=0;j<m_nNoSurfaceSpecieFluxes;j++){
				if(m_pplfMoleFluxIntoGas[j]!=NULL && m_pplfMoleFluxIntoGas[j][i]<0.0){
					SurfMassFlowRateOutofControlVolume-=m_pplfMoleFluxIntoGas[j][i]/1000.0*G_SpDb.Specie[m_uSpIdx[j]].lfTDData[MW]*DelX[i]*m_lfCellWidth;
					}
				}
			for(j=0;j<m_nNoSurfaceSpecieFluxes;j++){
				if(m_pplfMoleFluxIntoGas[j]!=NULL && m_pplfMoleFluxIntoGas[j][i+1]<0.0){
					SurfMassFlowRateOutofControlVolume-=m_pplfMoleFluxIntoGas[j][i+1]/1000.0*G_SpDb.Specie[m_uSpIdx[j]].lfTDData[MW]*DelX[i+1]*m_lfCellWidth;
					}
				}
			SurfMassFlowRateOutofControlVolume/=2.0;
			}
		else {
			for(j=0;j<m_nNoSurfaceSpecieFluxes;j++){
				if(m_pplfMoleFluxIntoGas[j]!=NULL && m_pplfMoleFluxIntoGas[j][i]<0.0){
					SurfMassFlowRateOutofControlVolume-=m_pplfMoleFluxIntoGas[j][i]/1000.0*G_SpDb.Specie[m_uSpIdx[j]].lfTDData[MW]*DelX[i]*m_lfCellWidth;
					}
				}
			}
		}
	if(bDoDynamicAnal){
		MOMin-=SurfMassFlowRateOutofControlVolume*Vel[CUR][i];
		}
	else {
		// average the velocity...
		MOMin-=SurfMassFlowRateOutofControlVolume*(Vel[CUR][i]+Vel[CUR][i+1]);
		}
	return(MOMin);// kg-m/sec2
}


double GasPhaseModel::GetMaxTemp()
{
  int i;
	double lfMaxT=0.0;
	for(i=1;i<m_nNoNodes-1;i++){
		lfMaxT=MAX(lfMaxT,*m_plfLowerWallTemp[MAX_AXIALNODES*CUR+i]);
		lfMaxT=MAX(lfMaxT,*m_plfUpperWallTemp[MAX_AXIALNODES*CUR+i]);
		lfMaxT=MAX(lfMaxT,T[i]);
		}
	return(lfMaxT);
}

double GasPhaseModel::GetMinTemp()
{
  int i;
	double lfMaxT=1000000000000.0;
	for(i=1;i<m_nNoNodes-1;i++){
		lfMaxT=MIN(lfMaxT,*m_plfLowerWallTemp[MAX_AXIALNODES*CUR+i]);
		lfMaxT=MIN(lfMaxT,*m_plfUpperWallTemp[MAX_AXIALNODES*CUR+i]);
		lfMaxT=MIN(lfMaxT,T[i]);
		}
	return(lfMaxT);
}


void GasPhaseModel::SetFlowsFromCurrent(int i, double lfCurrDen)
{

}

bool GasPhaseModel::HitPDropLimit(int i, double PLimit)
{
	return(P[i]<PLimit);
}

bool GasPhaseModel::bFixedFlow()
{
	return(m_bDoFixedFlow);
}

void GasPhaseModel::SetUpperSurfaceTemp(const double *lfTempArray)
{
	m_plfUpperWallTempSource=lfTempArray;
}

void GasPhaseModel::SetLowerSurfaceTemp(const double *lfTempArray)
{
	m_plfLowerWallTempSource=lfTempArray;
}


void GasPhaseModel::AnalyzeSurfaceHeats()
{
  int i;
	for(i=1;i<m_nNoNodes-1;i++){
		double T_i=GetTemp(i);
		AnalyzeNodeSurfaceHeat(i,T_i);
		}
	m_lfHeatRateAddedUpperWall[i]=0.0;
	m_lfHeatRateAddedUpperWall[0]=0.0;
	m_lfHeatRateAddedLowerWall[i]=0.0;
	m_lfHeatRateAddedLowerWall[0]=0.0;
	m_lfPublic_HeatRateAddedUpperWall[i]=0.0;
	m_lfPublic_HeatRateAddedLowerWall[i]=0.0;
	m_lfPublic_HeatRateAddedUpperWall[0]=0.0;
	m_lfPublic_HeatRateAddedLowerWall[0]=0.0;
}

void GasPhaseModel::AnalyzeNodeSurfaceHeat(int i, double T_i)
{
	// this is used for both QS and Dynamic analysis...
	double hcoef=GetHeatTransferCoef(i-1);
	m_lfHeatRateAddedUpperWall[i]=hcoef*(*m_plfUpperWallTemp[MAX_AXIALNODES*CUR+i]-T_i)*DelX[i]*m_lfCellWidth;//there is no good way to handle this heat exchange now that we use 'channels'.  let's use m_lfCellWidth for now and make sure we are consistent so as to conserve energy!
	m_lfHeatRateAddedLowerWall[i]=hcoef*(*m_plfLowerWallTemp[MAX_AXIALNODES*CUR+i]-T_i)*DelX[i]*m_lfCellWidth;
	if(m_bCounterFlowTo_SurfaceTempArrayIndicieDirection){
		m_lfPublic_HeatRateAddedUpperWall[m_nNoNodes-1-i]=m_lfHeatRateAddedUpperWall[i];
		m_lfPublic_HeatRateAddedLowerWall[m_nNoNodes-1-i]=m_lfHeatRateAddedLowerWall[i];
		}
	else{
		m_lfPublic_HeatRateAddedUpperWall[i]=m_lfHeatRateAddedUpperWall[i];
		m_lfPublic_HeatRateAddedLowerWall[i]=m_lfHeatRateAddedLowerWall[i];
		}
}

void GasPhaseModel::NormalizeSpeciesMoleFractions(double *Sp, int nNoSp)
{
	double sum=0.0;
	int i;
	for(i=0;i<nNoSp;i++){
		sum+=Sp[i];
		}
	for(i=0;i<nNoSp;i++){
		Sp[i]/=sum;
		}
	return;
}


void GasPhaseModel::VelMultiplyFactor(double factor)
{
  int i;
	for(i=0;i<m_nNoNodes-1;i++){
		Vel[CUR][i]*=factor;
		}	
}

} // end namespace Vision21

