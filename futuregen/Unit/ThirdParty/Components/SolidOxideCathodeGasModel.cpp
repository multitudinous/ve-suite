// SolidOxideCathodeGasModel.cpp: implementation of the CSolidOxideCathodeGasModel class.
//
//////////////////////////////////////////////////////////////////////

#include "globals.h"
#include "SolidOxideCathodeGasModel.h"
#include "math.h"

namespace Vision21 {


//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CSolidOxideCathodeGasModel::CSolidOxideCathodeGasModel()
{
	m_lfGmMoleElectronsPerGmMoleO2=4.0;
}

CSolidOxideCathodeGasModel::~CSolidOxideCathodeGasModel()
{

}

//virtual void functions...
void CSolidOxideCathodeGasModel::SetFlowsFromCurrent(int i, double lfCurrentDen)
{
	m_lfSurfaceO2MoleFluxIntoControlVol[i]=-(lfCurrentDen)/FARADAY/2./2.; //mole/m2/sec
}

double CSolidOxideCathodeGasModel::GetUtilization()
{
	double O2in=GetSpecieExitFlowAtNode(O2,0);
	double O2out=GetSpecieExitFlowAtNode(O2,m_nNoNodes-1);
	double Utilization=O2in-O2out;
	Utilization/=O2in;
	return(Utilization);
}

double CSolidOxideCathodeGasModel::GetOxygenFlowRate()
{
	double O2Flow=GetSpecieExitFlowAtNode(O2,0);
	return(O2Flow);//kg-mole/sec
}

void CSolidOxideCathodeGasModel::Setup(int nNoNodes, bool bDoCathodeFixedFlow, bool bDoQS, bool bDoRef, bool bDoEquilRef, ChanGeom *pCaGeom, double lfCellWidth, const double *lfXS_AreaAnode, const double *lfDelX, double lfAnodeVelInput, double lfAnodePInput, double lfAnodePExit, double lfAnodeTInput, int nNoAnodeSp, const double *lfAnodeSpInput, bool bManualConcSpec, double lfManConcVal)
{
	m_bManualConcLimitSpec=bManualConcSpec;
	m_lfCathodeConcLimitingCurrDen=lfManConcVal;//amp/cm2

	m_lfSurfaceO2MoleFluxIntoControlVol[0]=0.0;
	m_lfSurfaceO2MoleFluxIntoControlVol[nNoNodes-1]=0.0;
	AddSpecieFluxArray(O2,m_lfSurfaceO2MoleFluxIntoControlVol);
	GasPhaseModel::Setup(nNoNodes, bDoCathodeFixedFlow, bDoQS, bDoRef, bDoEquilRef, pCaGeom, lfCellWidth, lfXS_AreaAnode, lfDelX, lfAnodeVelInput, lfAnodePInput, lfAnodePExit, lfAnodeTInput, nNoAnodeSp, lfAnodeSpInput);
}

void CSolidOxideCathodeGasModel::ContinuationSetup(bool bDoCathodeFixedFlow, double lfVelInput,bool bDoQS, bool bDoRef, bool bDoEquilRef, const double *lfXS_AreaAnode, const double *lfDelX)
{
	AddSpecieFluxArray(O2,m_lfSurfaceO2MoleFluxIntoControlVol);
	GasPhaseModel::ContinuationSetup(bDoCathodeFixedFlow, lfVelInput,bDoQS, bDoRef, bDoEquilRef, lfXS_AreaAnode, lfDelX);
}

double CSolidOxideCathodeGasModel::GetLimitingSupplyCurrentDensity(int i)
{
	double ret=GetSpecieExitFlowAtNode(O2,i-1)*1000.0*m_lfGmMoleElectronsPerGmMoleO2*FARADAY/DelX[i]/m_lfCellWidth;// we want the incoming flow, so do 'i-1'
	return(ret);
}


double CSolidOxideCathodeGasModel::GetCathodeLimitingCurrentDensity(int i, double CathodeP, double Ti, double *lfWhich_n)
{
	if(m_bManualConcLimitSpec){
		if(lfWhich_n!=NULL){
			*lfWhich_n=2.0;//assume spec is given based on n=2!
			}
		return(m_lfCathodeConcLimitingCurrDen);
		}
	double lfPorosity=0.45;
	double lfTortuosity=4.0;
	double lfCorrectionFactor=pow((Ti/(900.0+273.0)), 1.5)*101000.0/CathodeP;// correction from PNNL evaluated data
	double O2Conc=Sp[CUR][i][O2];
	double O2Diffisivity=2.0/100.0/100.0*lfCorrectionFactor*lfPorosity/lfTortuosity; // from PNNL white paper I got on 1st visit.  was 9.0e-9;//m2/sec  

	double O2DiffusionThickness=1.2e-6;//m
	double lfCathodeLimitingCurrentDensity=m_lfGmMoleElectronsPerGmMoleO2*FARADAY*O2Diffisivity*O2Conc*CathodeP/Runiv/Ti/O2DiffusionThickness;
	return(lfCathodeLimitingCurrentDensity);
}

double CSolidOxideCathodeGasModel::GetCathodeConcLoss(double Ti, double CathodeP, int i, double lfCurrentDensity)
{
	double lfCathodeLimitingCurrentDensity=GetCathodeLimitingCurrentDensity(i,CathodeP,Ti);
	double lfLogArg=1.0-lfCurrentDensity/lfCathodeLimitingCurrentDensity;
	double A=Runiv*Ti/m_lfGmMoleElectronsPerGmMoleO2/FARADAY;
	double lfLimit=0.0001;
	double ret;
	if(lfLogArg<0.0){
		ret=-1000000.0;
		}
	else{
		ret=A*log(lfLogArg);
		}
	return(ret);
}

double CSolidOxideCathodeGasModel::GetNetEnthalpyInputToCell(int i, const double CellT, double CathodeT)
{

// The important thing is that we make sure we don't create/destroy energy....!
// Here it is....
// We assume that H2 lands on the surface carying the thermal energy (enthalpy) it has from
// the gas phase at the gas temperature.  Once it lands, it comes to the surface temp.
// but to do so, needs to evolve that much heat TO THE SURFACE (as a change in enthalpy)...
// The enthalpy change is only that equivalent to the gas phase enthalpy change, meaning
// that we don't account for surface binding energy, since we don't have that data anyway.
// This (the equiv. gas phase enthalpy change) is the ONLY heat THAT IMPACTS THE CELL TEMP!!!
// All the heat for this H2 surface process impacts the Cell alone--ditto for O2.  That is,
// the gas phase does not contribute to the heating/cooling process.  
// ALL OF THIS IS CONSISTENT WITH OUR INTERPRETATION FOR THE ELECTROCHEMICAL PROCESS
// WHICH WE ASSUME OCCURS AT THE TEMPERAURE OF THE SURFACE!!!
// Hence, overall we see enthalpy flow of H2 and O2 to the surface at the respective gas temp.,
// where the first step is the conversion of thermal energy (enthalpy) of the specie to reach the solid
// surface temperature of the electrolyte (the heat coming from the solid), then we have
// the transformation of chemical energy due to electrochemical processes that converts the
// gibbs free energy (that is, we stick to an enthalpy based free energy, not Helmholtz
// internal energy, since we follow standard thermodynamic gas analysis for electrochemical
// conversion at constant pressure which continues the use of enthalpy, not internal energy
// which may be more correct, but we don't have any TD data for H2 bound to a surface).

// We assume that the heat to evolve H2O comes from the gas.  That is, H2O comes off the surface
// at the surface temperature (without any surface-binding-energy being considered, again since 
// we don't have that data).  We see this to be consistent with the gas phase
// analysis, where the enthalpy of H2O is supplied at the solid temperature and then ends
// up mixing at the enthalpy of the gas mixture temperature.  For this reason, H2O does
// not impact surface thermal energy balance...

	double O2FlowFromSurface=m_lfSurfaceO2MoleFluxIntoControlVol[i]*DelX[i]*m_lfCellWidth/1000.0;// kg-mole/sec;  
	O2FlowFromSurface*=G_SpDb.Specie[O2].lfTDData[MW];

	double DelH_OfSurface=-O2FlowFromSurface*(hSp(O2,CathodeT)-hSp(O2,CellT));
	return(DelH_OfSurface);// watts -- should be negative for cooling
}

} // end namespace Vision21
