// PlannerSolidOxideFuelCellModel.cpp: implementation of the CPlanarSolidOxideFuelCellModel class.
//
//////////////////////////////////////////////////////////////////////

// why? #include "globals.h"
#include "PlannerSolidOxideFuelCellModel.h"
// why? #include "math.h"
#include <iostream.h>

namespace Vision21 {

  using namespace std;

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CPlanarSolidOxideFuelCellModel::CPlanarSolidOxideFuelCellModel()
{
    
}

////////////////////////////////////////////////////////////////
CPlanarSolidOxideFuelCellModel::~CPlanarSolidOxideFuelCellModel()
{
}

////////////////////////////////////////////////////////////////
void CPlanarSolidOxideFuelCellModel::SetREIInputs(REIInput *rei_inp)
{
  int i;

  CPlanarFuelCellModel::AnodeGas=(CAnodeGasModel*)&AnodeGas;
  CPlanarFuelCellModel::CathodeGas=(CCathodeGasModel*)&CathodeGas;
  CPlanarFuelCellModel::Electrolyte=(CElectrolyte*)&Electrolyte;
  
  m_bIfQuasiSteadySolidPhase_ThenDoTempUpdate = true;
  m_bDoQuasiSteadySolidPhase                  = rei_inp->qs_solid;
  m_bDoQuasiSteadyGasFlow                     = rei_inp->qs_gas;
  m_bDoRadiation                              = rei_inp->radiation;
  
  //  Make the surface connections and thereby define the fuel cell configuration...
  //  Standard Configuration--without any reforming board...

  m_bDoAnodeFixedFlow = false;// set this as you want
  AnodeGas.SetElectrolyteTempArray          ((const double *)Electrolyte.m_lfTemp);
  CathodeGas.SetElectrolyteTempArray        ((const double *)Electrolyte.m_lfTemp);
  AnodeGas.SetUpperSurfaceTemp              ((const double *)&SepPlt_AirSide.m_lfTemp[0][0]);
  AnodeGas.SetLowerSurfaceTemp              ((const double *)&Electrolyte.m_lfTemp[0][0]);
  SepPlt_AirSide.SetLowerSurfaceHeatRemoval (AnodeGas.m_lfPublic_HeatRateAddedUpperWall,&Electrolyte.m_lfTemp[0][0]);
  SepPlt_AirSide.SetUpperSurfaceHeatRemoval (CathodeGas.m_lfPublic_HeatRateAddedLowerWall,&Electrolyte.m_lfTemp[0][0]);
  CathodeGas.SetUpperSurfaceTemp            ((const double *)&Electrolyte.m_lfTemp[0][0]);
  CathodeGas.SetLowerSurfaceTemp            ((const double *)&SepPlt_AirSide.m_lfTemp[0][0]);
  Electrolyte.SetLowerSurfaceHeatRemoval    (CathodeGas.m_lfPublic_HeatRateAddedUpperWall,&SepPlt_AirSide.m_lfTemp[0][0]);
  Electrolyte.SetUpperSurfaceHeatRemoval    (AnodeGas.m_lfPublic_HeatRateAddedLowerWall,&SepPlt_FuelSide.m_lfTemp[0][0]);
  
  // m_nNoNodes: Number of nodes within the domain of the active area,
  //   plus two more for the boudaries!!  If this gets too large, then 
  //   may need to tighted the time step even more!  Watch for oscillations
  //   in the results, then tighten the time step.

  m_nNoNodes = rei_inp->ax_nodes;

  // m_lfCellLength: This equation forces the active area to be 1m x ZZ, where ZZ [m] is the
  //                 number you specify in the right hand side

  double DesiredCellLength = rei_inp->l_length;
  m_lfCellLength           = DesiredCellLength/(m_nNoNodes-2)*m_nNoNodes;
  m_lfCellWidth            = rei_inp->l_width;  // m cell active width
  AnGeom.lfHeight          = rei_inp->a_height; // m
  AnGeom.lfChannelLength   = DesiredCellLength;
  AnGeom.lfChannelWidth    = rei_inp->a_width;  // m
  AnGeom.lfChannelSpacing  = rei_inp->a_space;  // m
  
  // m_lfAnodeTInput: WH is 366 K at inlet to stack, but the temp. at inlet to active area will 
  //                  be closer to 900 deg. C.  The Argonne paper used 900 deg. C as inlet temp.!!!!
  // m_lfAnodePInput: This is the pressure right at the boundary of the cell (not at the first 
  //                  node which is 1/2 cell distance further upstream)
  // m_lfAnodePExit: This is the pressure right at the boundary of the cell (not at the last node
  //                 which is 1/2 cell distance further downstream)

  m_lfAnodeVelInput = rei_inp->a_vel;   // m/s
  m_lfAnodeTInput   = rei_inp->a_temp;  // K
  m_lfAnodePInput   = rei_inp->a_ipres; // Pa
  m_lfAnodePExit    = rei_inp->a_opres; // Pa
  m_nNoAnodeSp      = 7;

  // Achenbach (1995) used a ratio of H2O/CH4=2.5:
  // At 30 percent pre-reformed, he had: 17.1% CH4, 26.26% H2, 2.94% CO, 4.36% CO2, 49.34% H2O.
  // At fully equilibrated at 1000 deg. C, it would be: 0.00% CH4, 58.33% H2, 14.39% CO, 3.78% CO2, 23.5% H2O.
  // If this mixture is forced to be fully reformed and shifted (but not at equil.), 
  // it would be: 0.0% CH4, 72.72% H2, 0.0% CO, 18.18% CO2, 9.10% H2O.
  
  // if doing internal reforming, then this data is simply used to initialize the anode data

  m_lfAnodeSpInput[H2]  = rei_inp->a_specie[H2]; // mole fractions
  m_lfAnodeSpInput[CO]  = rei_inp->a_specie[CO];
  m_lfAnodeSpInput[H2O] = rei_inp->a_specie[H2O];
  m_lfAnodeSpInput[CO2] = rei_inp->a_specie[CO2];
  m_lfAnodeSpInput[N2]  = rei_inp->a_specie[N2];
  m_lfAnodeSpInput[O2]  = rei_inp->a_specie[O2];
  m_lfAnodeSpInput[CH4] = rei_inp->a_specie[CH4];
  
  CaGeom.lfHeight         = rei_inp->c_height; // m
  CaGeom.lfChannelLength  = DesiredCellLength;
  CaGeom.lfChannelWidth   = rei_inp->c_width;  // m
  CaGeom.lfChannelSpacing = rei_inp->c_space;  // m
  

  // m_lfCathodeTInput: If you don't select internal reforming you need lower inlet temps.
  //                    WH is about 870 K at inlet to 1st stage stack and 973 K at inlet to second stage,
  //                    but there is some heating of the gas as it passes down the center tube!!
  //                    The Argonne paper used 900 deg. C!! for inlet gas temp!!!
  // m_lfCathodePInput: This is the pressure right at the boundary of the cell 
  //                    (not at the first node which is 1/2 cell distance further upstream)
  // m_lfCathodePExit: This is the pressure right at the boundary of the cell 
  //                   (not at the last node which is 1/2 cell distance further downstream)

  m_lfCathodeVelInput   = rei_inp->c_vel;
  m_lfCathodeTInput     = rei_inp->c_temp;
  m_bDoCathodeFixedFlow = false;
  m_lfCathodePInput     = rei_inp->c_ipres; // Pa
  m_lfCathodePExit      = rei_inp->c_opres; // Pa
  
  double SaveCathPExit  = m_lfCathodePExit;
  double SaveAnodPExit  = m_lfAnodePExit;
  double SaveCathPInlet = m_lfCathodePInput;
  double SaveAnodPInlet = m_lfAnodePInput;

  // Make the corrections due to the fact that we assume linear delP from boundary to the inlet
  // and exit nodes locations--this corrects the bias you get when changing the number of nodes
  // used to calculate a fixed length geometry--we needed this when doing the node sensitivity
  // analysis, since lower count nodes gave us lower pressure gradient otherwise...
  
  if(!m_bDoCathodeFixedFlow){
    m_lfCathodePExit=SaveCathPExit+(SaveCathPExit-SaveCathPInlet)
      / m_lfCellLength*(m_lfCellLength/((double)(m_nNoNodes-2)))*0.5;
    m_lfCathodePInput=SaveCathPExit+(SaveCathPExit-SaveCathPInlet)
      / m_lfCellLength*(-m_lfCellLength/((double)(m_nNoNodes-2)))*((double)m_nNoNodes-2.0+0.5);
  }
  if(!m_bDoAnodeFixedFlow){
    m_lfAnodePExit=SaveAnodPExit+(SaveAnodPExit-SaveAnodPInlet)
      / m_lfCellLength*(m_lfCellLength/((double)(m_nNoNodes-2)))*0.5;
    m_lfAnodePInput=SaveAnodPExit+(SaveAnodPExit-SaveAnodPInlet)
      / m_lfCellLength*(-m_lfCellLength/((double)(m_nNoNodes-2)))*((double)m_nNoNodes-2.0+0.5);
  }
  
  m_nNoCathodeSp=6;
  m_lfCathodeSpInput[H2]  = rei_inp->c_specie[H2]; // mole fractions
  m_lfCathodeSpInput[CO]  = rei_inp->c_specie[CO];
  m_lfCathodeSpInput[H2O] = rei_inp->c_specie[H2O];
  m_lfCathodeSpInput[CO2] = rei_inp->c_specie[CO2];
  m_lfCathodeSpInput[N2]  = rei_inp->c_specie[N2];
  m_lfCathodeSpInput[O2]  = rei_inp->c_specie[O2];
  
  m_lfCellInitialTemp=1200.0; // K
  
  m_lfElectrolyteThickness = rei_inp->e_thick; // m  SOFCo presentation shows 180 microns for the electrolyte
  m_lfAnodeThickness       = rei_inp->a_thick;
  m_lfCathodeThickness     = rei_inp->c_thick;
  m_lfSeparatorThickness   = rei_inp->s_thick; // m
  
  // Achenbach (1995) used ceramic data of: Cp=400 joule/kg/K, 
  //   rho=6600 kg/m3, and metalic data of: Cp=500 joule/kg/K, rho=7800 kg/m3
  
  m_lfCellC          = rei_inp->l_heatcap; // joule/kg/K
  m_lfCellRho        = rei_inp->l_density; // kg/m3
  m_lfLoadResistance = rei_inp->loadres;   // ohm (don't make it too big, else have problems with Activation Overpotential Calc.
  
  // SOFCo presentation shows 0.9 to 1.0 ohm-cm2 as curent technology at 800 deg. C
  // Our FCPT AR&TD Topics included 10 ohm-cm at 500 deg.C. for electrolyte ionic resistivity.
  // If we use 180 microns for the electrolyte resistance, then the value to use would be 
  // 0.000018 ohm-m2
  // The following Resistivity Data comes from Bessette thesis (1994).

  CellRes.AirElectrodeRes.lfConst     = 0.0;               // ohm-cm  Resistivity
  CellRes.AirElectrodeRes.lfLinear    = 0.0;               // ohm-cm
  CellRes.AirElectrodeRes.lfPreExpon  = rei_inp->a_preexp; // ohm-cm  Resistivity
  CellRes.AirElectrodeRes.lfExpon     = rei_inp->a_exp;    // K
  CellRes.ElectrolyteRes.lfConst      = 0.3685;            // ohm-cm
  CellRes.ElectrolyteRes.lfLinear     = 0.0;               // ohm-cm
  CellRes.ElectrolyteRes.lfPreExpon   = rei_inp->e_preexp; // ohm-cm
  CellRes.ElectrolyteRes.lfExpon      = rei_inp->e_exp;    // K
  CellRes.FuelElectrodeRes.lfConst    = 0.0;               // ohm-cm
  CellRes.FuelElectrodeRes.lfLinear   = 0.0;               // ohm-cm
  CellRes.FuelElectrodeRes.lfPreExpon = rei_inp->f_preexp; // ohm-cm
  CellRes.FuelElectrodeRes.lfExpon    = rei_inp->f_exp;    // K
  CellRes.InterconectRes.lfConst      = 0.0;               // ohm-cm
  CellRes.InterconectRes.lfLinear     = 0.0;               // ohm-cm
  CellRes.InterconectRes.lfPreExpon   = rei_inp->i_preexp; // ohm-cm
  CellRes.InterconectRes.lfExpon      = rei_inp->i_exp;    // K
  
  CellRes.lfAnodeInterfaceResistance   = 0.10; // ohm-cm2
  CellRes.lfCathodeInterfaceResistance = 0.10; // ohm-cm2
  
  // For the following data, need to make sure the area being referenced is the flat surface area,
  // not the true pore area.  Bessette gave the following temperature dependant data on page 60
  // and I fit a linear curve to the data for LANGTHANUM MANGANITE.

  // m_lfCathodeExgCurDensityBCoef: 5000 @ 1000 deg C.--500mA/cm2 comes from Bessette thesis p. 60

  m_lfCathodeExgCurDensityBCoef = rei_inp->c_ecdb; // AMPERE/M2
  m_lfCathodeExgCurDensityMCoef = rei_inp->c_ecdm;
  m_lfAlphaCathodeBCoef         = rei_inp->c_tcoeffb;
  m_lfAlphaCathodeMCoef         = rei_inp->c_tcoeffm;
  
  m_lfAnodeExgCurDensity = rei_inp->a_ecd;
  m_lfAlphaAnode         = rei_inp->a_tcoeff; // set really high to effectively remove it since anode does not do much to the problem anyway according to Fig. 1-4 of Fuel Cell Handbook!
  

  // m_lfCathodeLogLimitingDiffusionCurrDen:  Make one of these really big if you only want to
  //                                          effectively have the other one provide an overpotenital
  // m_lfAnodeLogLimitingDiffusionCurrDen: Make one of these really big if you only want to
  //                                       effectively have the other one provide an overpotenital

  m_bDoManualLogDiffusionOverPotential   = false;
  m_lfCathodeLogLimitingDiffusionCurrDen = 4000.0;   // amp/m2
  m_lfAnodeLogLimitingDiffusionCurrDen   = 400000.0; // amp/m2
  
  // Achenbach (1995) used ceramic data of: Cp=400 joule/kg/K, rho=6600 kg/m3,
  //   and metalic data of: Cp=500 joule/kg/K, rho=7800 kg/m3

  m_lfSeparatorInitialTemp = m_lfCellInitialTemp;
  m_lfSeparatorC           = rei_inp->s_heatcap; // joule/kg/K
  m_lfSeparatorRho         = rei_inp->s_density; // kg/m3
  
  // m_lfXS_AreaCathode, m_lfXS_AreaAnode: Total flow area.
  // m_lfXS_AreaElectrolyte, m_lfXS_AreaSeparator: Solid body area.
  
  for(i=0;i<m_nNoNodes;i++){
    m_lfDelX[i]               = m_lfCellLength/((double)m_nNoNodes);
    m_lfXS_AreaCathode[i]     = AnGeom.lfHeight * AnGeom.lfChannelWidth * AnGeom.lfNumberOfChannels;
    m_lfXS_AreaAnode[i]       = CaGeom.lfHeight * CaGeom.lfChannelWidth * CaGeom.lfNumberOfChannels;
    m_lfXS_AreaElectrolyte[i] = m_lfCellWidth * (m_lfElectrolyteThickness + m_lfAnodeThickness + m_lfCathodeThickness);
    m_lfXS_AreaSeparator[i]   = m_lfCellWidth * m_lfSeparatorThickness + \
      CaGeom.lfHeight*CaGeom.lfChannelSpacing * CaGeom.lfNumberOfChannels + \
      AnGeom.lfHeight*AnGeom.lfChannelSpacing * AnGeom.lfNumberOfChannels;
  }
  m_szDescription="SOFC Base Case--.";  
}

} // end namespace Vision21
