#include "V21Helper.h"
#include "SteamTurbineUnit_i.h"

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
    
    if (!iwater)
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
    
    double temperature            = water_in_data->T;
    double pressure               = water_in_data->P;
    double quality                = water_in_data->Q;
    double weight                 = 0;
    double enthalpy               = water_in_data->H;
    double entropy                = 0;
    double saturation_temperature = 0;
    double saturation_pressure    = 0;
    double degrees_superheat      = 0;
    double degrees_subcooling     = 0;
    double viscosity              = 0;
    double critical_velocity      = 0;
    int    action                 = 0;
    
    double pin = pressure;
    double mdot = water_in_data->M;
    
    // GUI variables
    double ad_effi = ad_eff;
    double pres_drop = pressure_drop;
    
    steam_table.calculate (&temperature,
			   &pressure,
			   &quality,
			   &weight,
			   &enthalpy,
			   &entropy,
			   &saturation_temperature,
			   &saturation_pressure,
			   &degrees_superheat,
			   &degrees_subcooling,
			   &viscosity,
			   &critical_velocity,
			   action);
    
    /*
      cout << "steam in\n"
      << "\n  temperature "            << temperature
      << "\n  pressure "               << pressure
      << "\n  quality "                << quality
      << "\n  weight "                 << weight
      << "\n  enthalpy "               << enthalpy
      << "\n  entropy "                << entropy
      << "\n  saturation_temperature " << saturation_temperature
      << "\n  saturation_pressure "    << saturation_pressure
      << "\n  degrees_superheat "      << degrees_superheat
      << "\n  degrees_subcooling "     << degrees_subcooling
      << "\n  viscosity "              << viscosity
      << "\n  critical_velocity "      << critical_velocity
      << "\n  action "                 << action                 << endl << endl;
    */
    
    double h1 = enthalpy;
    
    temperature            = 0;
    pressure               = pin - pres_drop;
    quality                = 0;
    weight                 = 0;
    enthalpy               = 0;
    // entropy
    saturation_temperature = 0;
    saturation_pressure    = 0;
    degrees_superheat      = 0;
    degrees_subcooling     = 0;
    viscosity              = 0;
    critical_velocity      = 0;
    action                 = 0;
    
    steam_table.calculate (&temperature,
			   &pressure,
			   &quality,
			   &weight,
			   &enthalpy,
			   &entropy,
			   &saturation_temperature,
			   &saturation_pressure,
			   &degrees_superheat,
			   &degrees_subcooling,
			   &viscosity,
			   &critical_velocity,
			   action);
    
    /*
      cout << "steam insentropic out\n"
      << "\n  temperature "            << temperature
      << "\n  pressure "               << pressure
      << "\n  quality "                << quality
      << "\n  weight "                 << weight
      << "\n  enthalpy "               << enthalpy
      << "\n  entropy "                << entropy
      << "\n  saturation_temperature " << saturation_temperature
      << "\n  saturation_pressure "    << saturation_pressure
      << "\n  degrees_superheat "      << degrees_superheat
      << "\n  degrees_subcooling "     << degrees_subcooling
      << "\n  viscosity "              << viscosity
      << "\n  critical_velocity "      << critical_velocity
      << "\n  action "                 << action << endl << endl;
    */
    
    double h2s = enthalpy;
    
    double h2 = h1 - (h1 - h2s)*ad_effi;
    
    temperature            = 0;
    quality                = 0;
    weight                 = 0;
    enthalpy               = h2;
    entropy                = 0;
    saturation_temperature = 0;
    saturation_pressure    = 0;
    degrees_superheat      = 0;
    degrees_subcooling     = 0;
    viscosity              = 0;
    critical_velocity      = 0;
    action                 = 0;
    
    steam_table.calculate (&temperature,
			   &pressure,
			   &quality,
			   &weight,
			   &enthalpy,
			   &entropy,
			   &saturation_temperature,
			   &saturation_pressure,
			   &degrees_superheat,
			   &degrees_subcooling,
			   &viscosity,
			   &critical_velocity,
			   action);
    
    /*
      cout << "steam out \n"
      << "\n  temperature "            << temperature
      << "\n  pressure "               << pressure
      << "\n  quality "                << quality
      << "\n  weight "                 << weight
      << "\n  enthalpy "               << enthalpy
      << "\n  entropy "                << entropy
      << "\n  saturation_temperature " << saturation_temperature
      << "\n  saturation_pressure "    << saturation_pressure
      << "\n  degrees_superheat "      << degrees_superheat
      << "\n  degrees_subcooling "     << degrees_subcooling
      << "\n  viscosity "              << viscosity
      << "\n  critical_velocity "      << critical_velocity
      << "\n  action "                 << action
      << "\n  shaft output power " << mdot*(h1 - h2) << endl << endl;
    */
    
    summaries.insert_summary_val("Power UNITS:MW FORMAT:10.2f", mdot*(h1 - h2)/1e6);
    
    water_out_data->T = temperature;
    water_out_data->P = pressure; 
    water_out_data->M = mdot; 
    water_out_data->H = enthalpy;
    water_out_data->Q = quality;
    
    pack.intfs.resize(1); //each port has its own package
    pack.SetPackName("ExportData");
    pack.SetSysId("test.xml");
    
    waterhelper.WatToInt(water_out_data, pack.intfs[0]);
    owater = pack.Save(rv);
    executive_->SetExportData(id_, 0, owater);
    
    pack.intfs.resize(1);
    waterhelper.SumToInt(&summaries, pack.intfs[0]);
    result = pack.Save(rv); 
    std::cout<<"cp5\n";
    executive_->SetModuleResult(id_, result); //marks the end the execution
    
    delete water_in_data;
    delete water_out_data;

    std::cout<<"cp6\n";
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
    if (param!=NULL)
      std::cout<<param<<std::endl;
    std::cout<<UnitName_<<" :SetParams called"<<endl;
    Package pack;
        
    pack.SetSysId("gui.xml");
    pack.Load(param, strlen(param));
    //Now make use of pack.intfs to get your GUI vars out
    ad_eff = pack.intfs[0].getDouble("ad_eff");
    pressure_drop = pack.intfs[0].getDouble("pressure_drop");
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
