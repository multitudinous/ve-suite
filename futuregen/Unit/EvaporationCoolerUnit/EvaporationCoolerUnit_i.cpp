#include "V21Helper.h"
#include "EvaporationCoolerUnit_i.h"

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
    const char* iwater;
    const char* owater;
    bool rv;
    Package pack;
    string therm_path="thermo";
    int i;
    const char* result;
    summary_values summaries;

    iwater = executive_->GetImportData(id_, 0); //port 0 will be the gas input port;
    
    if (string(iwater)=="")
      {
	error("Missing input input.");
	return;
      }

    pack.SetSysId("water_in.xml");
      
    Water *water_in = new Water;
    Water *water_out = new Water;

    V21Helper waterhelper(therm_path.c_str());
    
    pack.Load(iwater, strlen(iwater)); 
    waterhelper.IntToWat(&(pack.intfs[0]), *water_in);
    
    double temperature1 = air_temp;
    double enthalpy1 = water_in->H;
    
    double weight, entropy, sat_temp, sat_press, sup_heat, sub_cool, visc, crit_vel;
    
    Steam67 steam_table;
    
    double hma1 = 0.0, pg1 = 0.0, quality = 1.0;
    
    //first determine humidity ratio of incomming air (omega1)
    
    weight=entropy=sat_temp=sat_press=sup_heat=sub_cool=visc=crit_vel=0.0;
    steam_table.calculate(&temperature1, &pg1, &quality,
			  &weight, &hma1, &entropy, &sat_temp,
			  &sat_press, &sup_heat, &sub_cool, &visc,
			  &crit_vel, 0);
    
    double pv1 = pg1*air_rel_humidity/100.0;
    double omega1 = 0.622*pv1/(amb_pressure - pv1);
    
    //determine humidity ratio (omega2) of outgoing air assuming 100% relative humidity
    
    double hv2 = 0.0, pg2 = 0.0, temperature2 = desired_temp;
    quality = 1.0;
    
    weight=entropy=sat_temp=sat_press=sup_heat=sub_cool=visc=crit_vel=0.0;
    steam_table.calculate(&temperature2, &pg2, &quality,
			  &weight, &hv2, &entropy, &sat_temp,
			  &sat_press, &sup_heat, &sub_cool, &visc,
			  &crit_vel, 0);
    
    double pv2 = pg2;
    double omega2 = 0.622*pv2/(amb_pressure - pv2);
    
    //evaluate enthalpies needed for 1st law evaluation of the evaporation rate
    
    double pressure2 = amb_pressure;
    double hw2 = 0.0;
    quality = 0.0;
    
    weight=entropy=sat_temp=sat_press=sup_heat=sub_cool=visc=crit_vel=0.0;
    steam_table.calculate(&temperature2, &pressure2, &quality,
			  &weight, &hw2, &entropy, &sat_temp,
			  &sat_press, &sup_heat, &sub_cool, &visc,
			  &crit_vel, 0);

    thermo thm(therm_path);
    int nspc = thm.get_spec_nam().size();
    vector<REAL> comp(nspc,0.0);
    map<string,int>::const_iterator it;
    it = thm.get_nam_spec().find("O2");
    if(it==thm.get_nam_spec().end()){
      error("therm file must have O2.");
      return;
    }
    comp[it->second] = 0.21;
    it = thm.get_nam_spec().find("N2");
    if(it==thm.get_nam_spec().end()){
      error("therm file must have N2.");
      return;
    }
    comp[it->second] = 0.79;
    double air_mwt = thm.mweight(comp);
    double hda1 = thm.enthalpy_mix(comp,(REAL)temperature1)/air_mwt;
    double hda2 = thm.enthalpy_mix(comp,(REAL)temperature2)/air_mwt;
    
    //evaporation rate
    
    double mdot_evap = water_in->M*(hw2 - enthalpy1)/
      ((hda1 + omega1*hma1 - hda2 - omega1*hv2)/(omega2 - omega1) + hw2 - hv2);
    double mdot_da = mdot_evap/(omega2 - omega1); // dry air flow required
    double mdot_ma = omega1*mdot_da; // moisture flow in air
    
    // outgoing data
    water_out->M = water_in->M - mdot_evap;
    water_out->T = temperature2;
    water_out->P = pressure2;
    water_out->Q = quality;
    water_out->H = hw2;
    
    //Fill in summary table
    
    summaries.clear();
    summaries.insert_summary_val("Air flow required UNITS:kg/s FORMAT:10.2f",
				 mdot_da+mdot_ma);
    summaries.insert_summary_val("Evaporation rate UNITS:kg/s FORMAT:10.2f",
				 mdot_evap);

    
    pack.intfs.resize(1); //each port has its own package
    pack.SetPackName("ExportData");
    pack.SetSysId("test.xml");
    
    waterhelper.WatToInt(water_out, pack.intfs[0]);
    owater = pack.Save(rv);
    executive_->SetExportData(id_, 0, owater);
    
    pack.intfs.resize(1);
    waterhelper.SumToInt(&summaries, pack.intfs[0]);
    result = pack.Save(rv); 
 
    executive_->SetModuleResult(id_, result); //marks the end the execution
    
    delete water_in;
    delete water_out;
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
    desired_temp = pack.intfs[0].getDouble("desired_temp");
    air_temp = pack.intfs[0].getDouble("air_temp");
    air_rel_humidity = pack.intfs[0].getDouble("air_humidity");
    amb_pressure = pack.intfs[0].getDouble("ambient_pres");
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
  msg = "Evaporation Unit: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
  pack.intfs.clear();
  result = pack.Save(rv);
  return_state = 1;
  executive_->SetModuleResult(id_, result); //this marks the end the execution
}

void Body_Unit_i::warning (std::string msg)
{
  msg = "Evaporation Unit: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
}
// Interpolates to find f(xt) along the line defined by the other four variables.
