#include "V21Helper.h"
#include "CondenserUnit_i.h"

// Implementation skeleton constructor
Body_Unit_i::Body_Unit_i (Body::Executive_ptr exec, std::string name)
  : executive_(Body::Executive::_duplicate(exec))
{
  UnitName_=name;
  return_state = 0;
}
  
// Implementation skeleton destructor
Body_Unit_i::~Body_Unit_i (void)
  {
  }
  
void Body_Unit_i::StartCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    const char* iwater1;
    const char* iwater2;
    const char* owater1;
    const char* owater2;
    bool rv;
    Package pack;
    string therm_path="thermo";
    int i;
    const char* result;
    summary_values summaries;

    iwater1 = executive_->GetImportData(id_, 0); //port 0 will be the gas input port;
    iwater2 = executive_->GetImportData(id_, 1); //port 1 will be the sweep input port;
    
    if (string(iwater1)=="" || string(iwater2)=="")
      {
	error("Condenser: Missing input input.");
	return;
      }

    pack.SetSysId("water_in.xml");
      
    Water *water_in_data1 = new Water;
    Water *water_in_data2 = new Water;

    V21Helper waterhelper(therm_path.c_str());
    
    pack.Load(iwater1, strlen(iwater1)); 
    waterhelper.IntToWat(&(pack.intfs[0]), *water_in_data1);
    
    pack.Load(iwater2, strlen(iwater2)); 
    waterhelper.IntToWat(&(pack.intfs[0]), *water_in_data2);
    
    Steam67 steam_table;
    
    // Check incoming
    if(water_in_data1->T <= 200 || water_in_data1->T >= 3000) {
      warning("Condenser: Incoming water temperature out of range.");
    }
    
  // get sat_temp and hfg
    double enthalpy1 = 0.0;
    double quality1 = 1.0;
    double pressure1 = water_in_data1->P;
    double Temperature1 = 0.0;
    double entropy, sat_temp, weight, sat_press, sup_heat;
    double sub_cool, visc, crit_vel;
    entropy=sat_temp=weight=sat_press=sup_heat=sub_cool=visc=crit_vel=0.0;
    steam_table.calculate(&Temperature1, &pressure1, &quality1,
			  &weight, &enthalpy1, &entropy, &sat_temp,
			  &sat_press, &sup_heat, &sub_cool, &visc,
			  &crit_vel, 0);
    double hfg = enthalpy1;
    
    enthalpy1 = 0.0;
    quality1 = 0.0;
    pressure1 = water_in_data1->P;
    Temperature1 = 0.0;
    entropy=sat_temp=weight=sat_press=sup_heat=sub_cool=visc=crit_vel=0.0;
    steam_table.calculate(&Temperature1, &pressure1, &quality1,
			  &weight, &enthalpy1, &entropy, &sat_temp,
			  &sat_press, &sup_heat, &sub_cool, &visc,
			  &crit_vel, 0);
    hfg -= enthalpy1;
    double enthalpy1min = enthalpy1;
    double qmax = water_in_data1->H - enthalpy1min;
    double Qmax = water_in_data1->M * qmax;
    
    cout << " Qmax " << Qmax << endl;
    
    // CONDITIONS OF INCOMING WATER
    enthalpy1 = water_in_data1->H;
    quality1 = water_in_data1->Q;
    pressure1 = water_in_data1->P;
    Temperature1 = water_in_data1->T;
    
    if(enthalpy1 < 1e-03) {
      // Get H,Q from T, P
      quality1=weight=enthalpy1=entropy=sat_temp=sat_press=sup_heat=sub_cool=visc=crit_vel=0;
      steam_table.calculate(&Temperature1, &pressure1, &quality1,
			    &weight, &enthalpy1, &entropy, &sat_temp,
			    &sat_press, &sup_heat, &sub_cool, &visc,
			    &crit_vel, 0);
    }
    
    // incoming water_in_data1 must have some vapor
    
    if(quality1==0.0){
      error("Condenser: Incoming steam has no steam.");
      return;
    }
    
    double enthalpy2 = water_in_data2->H;
    double quality2 = water_in_data2->Q;
    double pressure2 = water_in_data2->P;
    double temperature2 = water_in_data2->T;
    
    if(enthalpy2 < 1e-03) {
      // Get H,Q from T, P
      quality2=weight=enthalpy2=entropy=sat_temp=sat_press=sup_heat=sub_cool=visc=crit_vel=0;
      steam_table.calculate(&temperature2, &pressure2, &quality2,
			    &weight, &enthalpy2, &entropy, &sat_temp,
			    &sat_press, &sup_heat, &sub_cool, &visc,
			    &crit_vel, 0);
    }
    
    // CALCULATE C's
    
    
    double p = water_in_data2->P * 1.0e-5;
    
    double cwat2 = 1.83191e-3 * p*p*p - 0.2339152 * p*p + 24.28962 * p + 4186.514;
    double Cwat2 = water_in_data2->M * cwat2;
    
    double Cmin = Cwat2;
    
    // AREA
    
    const double pi = 3.14159265359;
    double hxA = pi * tube_od * tube_length * num_tubeH * num_tubeV;
    
    // Tube Diameter Ratio.
    double tratio = tube_od / tube_id;
    // shell side properties for vapor and condensing liquid
    double rhol = 1000.0;
    double rhov = pressure1/(8314.*sat_temp)*18.01534;
    p = pressure1;
    
    double kl;
    if (p < 1.4e5)
      kl = 2.06170536e-02*log(p)+4.44196461e-01;
    else {
      kl = -1.37192481e-43*p*p*p*p*p*p + 8.26688556e-36*p*p*p*p*p - 1.8713164e-28*p*p*p*p + 
	1.94520086e-21*p*p*p - 8.75679422e-15*p*p -5.0630019e-09*p + 6.89586452e-01;
    }
    double cpl = 1.83191e-3 * p*p*p - 0.02339152 * p*p + 24.28962 * p + 4186.514;
    p = log(p);
    double viscl = -1.390838E-02*p*p*p + 1.023875E-01*p*p - 4.506144E-01*p - 8.140231E+00;
    viscl = exp(viscl);
    double hfgp;
    
    // Heat Transfer Coefficient.
    
    double WoutT1 = water_in_data1->T;
    double WoutT2 = water_in_data2->T;
    double wout1 = WoutT1;
    double wout2 = WoutT2;
    
    double enthalpy1_out;
    
    // LOOP HERE
    
    do {
      WoutT1 = (0.9 * wout1) + (0.1 * WoutT1);
      WoutT2 = (0.9 * wout2) + (0.1 * WoutT2);    
      
      double Tsurf = (water_in_data2->T + WoutT2)*0.5;
      
      hfgp = hfg + 0.68*cpl*(sat_temp - Tsurf);
      
      // External Heat Transfer Coefficient, Icropera and Dewitt Third Ed. Eq. (10.41)
      
      double ho = 0.729*pow(9.81*rhol*(rhol-rhov)*kl*kl*kl*hfgp/
			    (num_tubeV*viscl*(sat_temp - Tsurf)*tube_od),0.25); 
      
      double FFO = 0; // fouling factor in
      double FFI = 0; // fouling factor out
      
      double U = 1 / ( 1 / ho + FFO + tratio * FFI );
      
      // FINISH CALCULATION
      
      double NTU = U * hxA / Cmin;
      
      double eff;
      
      // all HX's if water is at saturation
      eff = 1 - exp(-NTU);
      
      double Q = Cmin * eff * fabs(water_in_data1->T - water_in_data2->T);
      
      cout << " Q " << Q << endl;
      
      if( Q > Qmax ) {
	Q = Qmax;
	cout << " Reset Q to Qmax " << endl;
      }
      
      int s = (water_in_data1->T > water_in_data2->T)?1:-1;
      
      wout2 = water_in_data2->T + s * Q / Cwat2;
      enthalpy1_out = enthalpy1 - s * Q / water_in_data1->M;
      
      // Get T,Q from H, P
      wout1 = 0;
      pressure1 = water_in_data1->P - ext_press_drop;
      quality1=weight=entropy=sat_temp=sat_press=sup_heat=sub_cool=visc=crit_vel=0;
      steam_table.calculate(&wout1, &pressure1, &quality1,
			    &weight, &enthalpy1_out, &entropy, &sat_temp,
			    &sat_press, &sup_heat, &sub_cool, &visc,
			    &crit_vel, 0);
      
    } while (fabs(WoutT1-wout1) > 0.001 || fabs(WoutT2-wout2) > 0.001);
    
    Water *water_out_data1 = new Water;
    Water *water_out_data2 = new Water;
    
    // FILL IN OUTGOING DATA
    
    water_out_data1->H = enthalpy1_out;
    water_out_data1->P = water_in_data1->P - ext_press_drop;
    water_out_data1->M = water_in_data1->M;
    
    //  Temperature1 = water_out_data1->T;
    enthalpy1 = water_out_data1->H;
    pressure1 = water_out_data1->P;
    //  quality1=weight=enthalpy=entropy=sat_temp=sat_press=sup_heat=sub_cool=visc=crit_vel=0;
    Temperature1=quality1=weight=entropy=sat_temp=sat_press=sup_heat=sub_cool=visc=crit_vel=0;
    steam_table.calculate(&Temperature1, &pressure1, &quality1,
			  &weight, &enthalpy1, &entropy, &sat_temp,
			  &sat_press, &sup_heat, &sub_cool, &visc,
			  &crit_vel, 0);
    
    water_out_data1->T = Temperature1;
    water_out_data1->P = pressure1;
    water_out_data1->H = enthalpy1;
    water_out_data1->Q = quality1;
    
    water_out_data2->T = WoutT2;
    water_out_data2->P = water_in_data2->P - int_press_drop;
    water_out_data2->M = water_in_data2->M;
    
    temperature2 = water_out_data2->T;
    pressure2 = water_out_data2->P;
    quality2=weight=enthalpy2=entropy=sat_temp=sat_press=sup_heat=sub_cool=visc=crit_vel=0;
    steam_table.calculate(&temperature2, &pressure2, &quality2,
			  &weight, &enthalpy2, &entropy, &sat_temp,
			  &sat_press, &sup_heat, &sub_cool, &visc,
			  &crit_vel, 0);
    
    water_out_data2->T = temperature2;
    water_out_data2->P = pressure2;
    water_out_data2->H = enthalpy2;
    water_out_data2->Q = quality2;
    if(quality2>0.0){
      warning("Condenser: Internal cooling water has reached saturation. Try to increase flow.");
    }
    
    // SEND OUTGOING DATA   
    
    pack.intfs.resize(1); //each port has its own package
    pack.SetPackName("ExportData");
    pack.SetSysId("test.xml");
    
    waterhelper.WatToInt(water_out_data1, pack.intfs[0]);
    owater1 = pack.Save(rv);
    executive_->SetExportData(id_, 0, owater1);
    waterhelper.WatToInt(water_out_data2, pack.intfs[0]);
    owater2 = pack.Save(rv);
    executive_->SetExportData(id_, 1, owater2);
    
    pack.intfs.clear();
    result = pack.Save(rv); 

    executive_->SetModuleResult(id_, result); //marks the end the execution
    
    delete water_in_data1;
    delete water_in_data2;
    delete water_out_data1;
    delete water_out_data2;
  }
  
void Body_Unit_i::StopCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    string msg;
    msg = UnitName_+" : Instant calculation, already finished\n";
    executive_->SetModuleMessage(id_,msg.c_str());
  }
  
void Body_Unit_i::PauseCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    string msg;
    msg = UnitName_+" : Instant calculation, already finished\n";
    executive_->SetModuleMessage(id_,msg.c_str());
  }
  
void Body_Unit_i::Resume (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    string msg;
    msg = UnitName_+" : Instant calculation, already finished\n";
    executive_->SetModuleMessage(id_,msg.c_str());
  }
  
char * Body_Unit_i::GetStatusMessage (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    const char *status;
    bool rv;
    Package pack;
    pack.SetPackName("Status");
    pack.SetSysId("status.xml");
    pack.intfs.resize(1);
    pack.intfs[0].setInt("return_state", return_state);
    status = pack.Save(rv);
    return CORBA::string_dup(status);
  }
  
char * Body_Unit_i::GetUserData (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    std::cout<<UnitName_<<" :GetUserData called"<<endl;
    return CORBA::string_dup(data_.c_str());
  }
  
void Body_Unit_i::SetParams (
    const char * param
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    if (string(param)=="")
      return;
    std::cout<<UnitName_<<" :SetParams called"<<endl;
    Package pack;
        
    pack.SetSysId("gui.xml");
    pack.Load(param, strlen(param));
    //Now make use of pack.intfs to get your GUI vars out
    num_tubeH = pack.intfs[0].getInt("num_tubeH");
    num_tubeV = pack.intfs[0].getInt("num_tubeV");
    tube_id = pack.intfs[0].getDouble("tube_id");
    tube_od = pack.intfs[0].getDouble("tube_od");
    tube_length = pack.intfs[0].getDouble("tube_length");
    int_press_drop = pack.intfs[0].getDouble("int_press_drop");
    ext_press_drop = pack.intfs[0].getDouble("ext_press_drop");
  }
  
void Body_Unit_i::SetID (
    CORBA::Long id
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    id_=id;
    std::cout<<UnitName_<<" :SetID called"<<endl;
  }
  
CORBA::Long Body_Unit_i::GetID (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    std::cout<<UnitName_<<" :GetID called"<<endl;
    return id_;
  }
  
void Body_Unit_i::SetName (
    const char * name
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    UnitName_ = std::string(name);
    std::cout<<UnitName_<<" :SetName called"<<endl;
  }
  
char * Body_Unit_i::GetName (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    std::cout<<UnitName_<<" :GetName called"<<endl;
    return CORBA::string_dup(UnitName_.c_str());
  }

void Body_Unit_i::error (std::string msg)
{
  Package pack;
  const char* result;
  bool rv;
  pack.SetPackName("result");
  pack.SetSysId("result.xml");
  msg+="\n";
  executive_->SetModuleMessage(id_, msg.c_str());
  pack.intfs.clear();
  result = pack.Save(rv);
  return_state = 1;
  executive_->SetModuleResult(id_, result); //this marks the end the execution
}

void Body_Unit_i::warning (std::string msg)
{
  msg+="\n";
  executive_->SetModuleMessage(id_, msg.c_str());
}
// Interpolates to find f(xt) along the line defined by the other four variables.
