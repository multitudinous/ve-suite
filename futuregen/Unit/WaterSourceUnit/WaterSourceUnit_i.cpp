#include "V21Helper.h"
#include "WaterSourceUnit_i.h"

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
    const char* owater;
    const char* result;
    bool rv;
    Package p;
    string therm_path="thermo";

    Water *water_data = new Water;

    if(case_type==0) {
      water_data->T = temp;
      water_data->H = 0;
    } else {
      water_data->T = 0;
      water_data->H = enth;
    }
    
    water_data->P = pres;
    water_data->Q = 0;
    water_data->M = flow;
    
    Steam67 steam_table;
    
    double temperature = water_data->T;
    double pressure = water_data->P;
    double enthalpy = water_data->H;
    double quality = water_data->Q;
    
    double weight, entropy, sat_temp, sat_press, sup_heat, sub_cool, visc, crit_vel;
    
    weight=entropy=sat_temp=sat_press=sup_heat=sub_cool=visc=crit_vel=0;
    if(!steam_table.calculate(&temperature, &pressure, &quality,
			      &weight, &enthalpy, &entropy, &sat_temp,
			      &sat_press, &sup_heat, &sub_cool, &visc,
			      &crit_vel, 0))
      {
	error("Fatal error in steam table calculate");
	return;
      }
    
    if(case_type==0) {
      enth=enthalpy;
      water_data->H = enthalpy;
    } else {
      temp=temperature;
      water_data->T = temperature;
    }

    water_data->Q = quality;
    
    V21Helper gashelper(therm_path.c_str());

    //set p.intf to be something containing the export data
    p.SetPackName("ExportData");
    p.SetSysId("test.xml");
    p.intfs.resize(1);

    gashelper.WatToInt(water_data, p.intfs[0]);
    owater = p.Save(rv);
    //set p.intf to be someting containing the results
    //    result = p.Save(rv);
    executive_->SetExportData(id_, 0, owater);

    p.intfs.clear();
    result = p.Save(rv);
    executive_->SetModuleResult(id_, result); //this marks the end the execution
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
    Package p;
    p.SetPackName("Status");
    p.SetSysId("status.xml");
    p.intfs.resize(1);
    p.intfs[0].setInt("return_state", return_state);
    status = p.Save(rv);
    return CORBA::string_dup(status);    
    return NULL;
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
    Package p;
        
    p.SetSysId("temp.xml");
    p.Load(param, strlen(param));
    //Now make use of p.intfs to get your GUI vars out
    temp = p.intfs[0].getDouble("temp");
    pres = p.intfs[0].getDouble("pres");
    enth = p.intfs[0].getDouble("enth");
    flow = p.intfs[0].getDouble("flow");
    case_type = p.intfs[0].getInt("case_type");
   
    std::cout<<"Set Params done"<<std::endl;
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
  Package p;
  const char* result;
  bool rv;
  p.SetPackName("result");
  p.SetSysId("result.xml");
  msg = "WaterSource: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
  p.intfs.clear();
  result = p.Save(rv);
  return_state = 1;
  executive_->SetModuleResult(id_, result); //this marks the end the execution
}

void Body_Unit_i::warning (std::string msg)
{
  msg = "WaterSource: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
}
