// SolidOxideAnodeGasModel.cpp: implementation of the CSolidOxideAnodeGasModel class.
//
//////////////////////////////////////////////////////////////////////

#include "globals.h"
#include "SolidOxideAnodeGasModel.h"
#include "math.h"


namespace Vision21 {

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CSolidOxideAnodeGasModel::CSolidOxideAnodeGasModel()
{
	m_lfGmMoleElectronsPerGmMoleH2=2.0;
}

CSolidOxideAnodeGasModel::~CSolidOxideAnodeGasModel()
{

}

//virtual void functions....

void CSolidOxideAnodeGasModel::SetFlowsFromCurrent(int i, double lfCurrentDen)
{
	m_lfSurfaceH2MoleFluxIntoControlVol[i]=-lfCurrentDen/FARADAY/2.; // g-mole/m2/sec
	m_lfSurfaceH2OMoleFluxIntoControlVol[i]=lfCurrentDen/FARADAY/2.; // g-mole/m2/sec
}

double CSolidOxideAnodeGasModel::GetUtilization()
{
	double H2in=GetSpecieExitFlowAtNode(H2,0);
	double H2out=GetSpecieExitFlowAtNode(H2,m_nNoNodes-1);
	double Utilization=H2in-H2out;
	Utilization/=H2in;
	return(Utilization);
}

double CSolidOxideAnodeGasModel::GetFuelFlowRate()
{
	double H2Flow=GetSpecieExitFlowAtNode(H2,0);
	return(H2Flow);//kg-mole/sec
}

void CSolidOxideAnodeGasModel::Setup(int nNoNodes, bool bDoCathodeFixedFlow, bool bDoQS, bool bDoRef, bool bDoEquilRef, ChanGeom *pAnGeom, double lfCellWidth, const double *lfXS_AreaAnode, const double *lfDelX, double lfAnodeVelInput, double lfAnodePInput, double lfAnodePExit, double lfAnodeTInput, int nNoAnodeSp, const double *lfAnodeSpInput, bool bManualConcSpec, double lfManConcVal)
{
	m_bManualConcLimitSpec=bManualConcSpec;
	m_lfAnodeConcLimitingCurrDen=lfManConcVal;//amp/cm2

	m_lfSurfaceH2MoleFluxIntoControlVol[0]=0.0;
	m_lfSurfaceH2MoleFluxIntoControlVol[nNoNodes-1]=0.0;
	m_lfSurfaceH2OMoleFluxIntoControlVol[0]=0.0;
	m_lfSurfaceH2OMoleFluxIntoControlVol[nNoNodes-1]=0.0;
	AddSpecieFluxArray(H2,m_lfSurfaceH2MoleFluxIntoControlVol);
	AddSpecieFluxArray(H2O,m_lfSurfaceH2OMoleFluxIntoControlVol);
	GasPhaseModel::Setup(nNoNodes, bDoCathodeFixedFlow, bDoQS, bDoRef, bDoEquilRef, pAnGeom, lfCellWidth, lfXS_AreaAnode, lfDelX, lfAnodeVelInput, lfAnodePInput, lfAnodePExit, lfAnodeTInput, nNoAnodeSp, lfAnodeSpInput);
}

void CSolidOxideAnodeGasModel::ContinuationSetup(bool bDoAnodeFixedFlow,double lfVelInput,bool bDoQS, bool bDoRef, bool bDoEquilRef, const double *lfXS_AreaAnode, const double *lfDelX)
{
	AddSpecieFluxArray(H2,m_lfSurfaceH2MoleFluxIntoControlVol);
	AddSpecieFluxArray(H2O,m_lfSurfaceH2OMoleFluxIntoControlVol);
	GasPhaseModel::ContinuationSetup(bDoAnodeFixedFlow,lfVelInput,bDoQS, bDoRef, bDoEquilRef, lfXS_AreaAnode, lfDelX);
}

double CSolidOxideAnodeGasModel::NodeInputChemicalPower(int nNode)
{
	double lfHeatingValueH2=2.471E5;//joule/gm-moleH2
	double lfHeatingValueCO=2.885E5;//joule/gm-moleCO
	double lfChemEnergyFlow=GetSpecieExitFlowAtNode(H2, nNode)*lfHeatingValueH2*1000.0;
	lfChemEnergyFlow+=GetSpecieExitFlowAtNode(CO, nNode)*lfHeatingValueCO*1000.0;
	return(lfChemEnergyFlow);//watt
}

double CSolidOxideAnodeGasModel::GetLimitingSupplyCurrentDensity(int i)
{
	double ret=GetSpecieExitFlowAtNode(H2,i-1)*1000.0*m_lfGmMoleElectronsPerGmMoleH2*FARADAY/DelX[i]/m_lfCellWidth;// we want the incoming flow, so do 'i-1'
	return(ret);
}

double CSolidOxideAnodeGasModel::GetAnodeLimitingCurrentDensity(int i, double AnodeP, double Ti)
{
	if(m_bManualConcLimitSpec){
		return(m_lfAnodeConcLimitingCurrDen);
		}
	double lfPorosity=0.45;
	double lfTortuosity=4.0;
	double lfCorrectionFactor=pow((Ti/(900.0+273.0)), 1.5)*101000.0/AnodeP;// correction from PNNL evaluated data
	double H2Conc=Sp[CUR][i][H2]+0.000000001;
	double H2Diffisivity=7.3/100.0/100.0*lfCorrectionFactor*lfPorosity/lfTortuosity; // from PNNL white paper I got on 1st visit.  was 4.0e-8;//m2/sec 

	double H2DiffusionThickness=1.2E-6;//m
	double lfAnodeLimitingCurrentDensity=m_lfGmMoleElectronsPerGmMoleH2*FARADAY*H2Diffisivity*H2Conc*AnodeP/Runiv/Ti/H2DiffusionThickness;
	return(lfAnodeLimitingCurrentDensity);
}

double CSolidOxideAnodeGasModel::GetAnodeConcLoss(double Ti, double AnodeP, int i, double lfCurrentDensity)
{
	double lfAnodeLimitingCurrentDensity=GetAnodeLimitingCurrentDensity(i,AnodeP,Ti);
	double lfLogArg=1.0-lfCurrentDensity/lfAnodeLimitingCurrentDensity;
	double A=Runiv*Ti/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;
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

double CSolidOxideAnodeGasModel::GetNetEnthalpyInputToCell(int i, const double CellT, double AnodeT)
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

	double H2FlowFromSurface=m_lfSurfaceH2MoleFluxIntoControlVol[i]*DelX[i]*m_lfCellWidth/1000.0;// kg-mole/sec;  
	H2FlowFromSurface*=G_SpDb.Specie[H2].lfTDData[MW];// go to kg/sec

	double DelH_OfSurface=H2FlowFromSurface*(hSp(H2,CellT)-hSp(H2,AnodeT));//assume H2 and O2 come on electrode at their gas temps
	return(DelH_OfSurface);// watts -- should be negative number so that H2 landing on surface will cool the surface.
}

} // end namespace Vision21
