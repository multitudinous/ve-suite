#include "V21Helper.h"
#include "WaterPumpUnit_i.h"

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
    Steam67 steam_table;

    iwater = executive_->GetImportData(id_, 0); //port 0 will be the gas input port;
    
    if (string(iwater)=="")
      {
	error("Missing input input.");
	return;
      }

    pack.SetSysId("water_in.xml");
      
    Water *water_in_data = new Water;
    Water *water_out_data = new Water;

    V21Helper waterhelper(therm_path.c_str());
    
    pack.Load(iwater, strlen(iwater)); 
    waterhelper.IntToWat(&(pack.intfs[0]), *water_in_data);
    
    //Tables used by component
    // look up at T,P
    double h1 = 0.0;
    double quality = 0.0;
    double pinlet = water_in_data->P;
    double temperature = water_in_data->T;
    double entropy, sat_temp, weight, sat_press, sup_heat;
    double sub_cool, visc, crit_vel;
    entropy=sat_temp=weight=sat_press=sup_heat=sub_cool=visc=crit_vel=0.0;
    steam_table.calculate(&temperature, &pinlet, &quality,
			  &weight, &h1, &entropy, &sat_temp,
			  &sat_press, &sup_heat, &sub_cool, &visc,
			  &crit_vel, 0);
    
    // find h2s (isentropic enthalpy at outlet) at P and entropy
    double h2s = 0.0;
    double poutlet;
    if(case_type==0) {
      poutlet = pinlet + pressure_change; // pascals
    } else {
      poutlet = pressure_out; // pascals
      // pressure_change.set(poutlet-pinlet);
      if(pressure_change<0.0){
	error("Water pump cannot decrease pressure.");
	return;
      }
    }
    
    if(poutlet<0) {
      error("Pressure drop too large");
      return_state = 1;
      return;
    }
    temperature = 0.0;
    quality=weight=h2s=sat_temp=sat_press=sup_heat=sub_cool=visc=crit_vel=0.0;
    steam_table.calculate(&temperature, &poutlet, &quality,
			  &weight, &h2s, &entropy, &sat_temp,
			  &sat_press, &sup_heat, &sub_cool, &visc,
			  &crit_vel, 0);
    
    // apply isentropic efficiency
    double h2a = -1*((h1-h2s)/eff-h1);
    
    // find temperature at exit and power at enthalpy and pressure (h2a, poutlet)
    temperature = 0.0;
    quality=weight=entropy=sat_temp=sat_press=sup_heat=sub_cool=visc=crit_vel=0.0;
    steam_table.calculate(&temperature, &poutlet, &quality,
			  &weight, &h2a, &entropy, &sat_temp,
			  &sat_press, &sup_heat, &sub_cool, &visc,
			  &crit_vel, 0);
    
    // FILL IN OUTGOING DATA
    
    water_out_data->T = temperature;
    water_out_data->P = poutlet;
    water_out_data->H = h2a;
    water_out_data->Q = quality;
    water_out_data->M = water_in_data->M;
    
    
    // fill in summary table
    summaries.clear();
    
    summaries.insert_summary_val("Power UNITS:MW FORMAT:10.2f",
				 -water_in_data->M*(h2a - h1)/1e6);
    
    pack.intfs.resize(1); //each port has its own package
    pack.SetPackName("ExportData");
    pack.SetSysId("test.xml");
    
    waterhelper.WatToInt(water_out_data, pack.intfs[0]);
    owater = pack.Save(rv);
    executive_->SetExportData(id_, 0, owater);
    
    pack.intfs.resize(1);
    waterhelper.SumToInt(&summaries, pack.intfs[0]);
    result = pack.Save(rv); 

    executive_->SetModuleResult(id_, result); //marks the end the execution
    
    delete water_in_data;
    delete water_out_data;
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
    eff = pack.intfs[0].getDouble("eff");
    pressure_out = pack.intfs[0].getDouble("pressure_out");
    pressure_change = pack.intfs[0].getDouble("pressure_change");
    case_type = pack.intfs[0].getInt("case_type");
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
  msg = "Water Pump: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
  pack.intfs.clear();
  result = pack.Save(rv);
  return_state = 1;
  executive_->SetModuleResult(id_, result); //this marks the end the execution
}

void Body_Unit_i::warning (std::string msg)
{
  msg = "Water Pump: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
}
// Interpolates to find f(xt) along the line defined by the other four variables.
