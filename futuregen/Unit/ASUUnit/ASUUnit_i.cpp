#include "V21Helper.h"
#include <ThirdParty/Components/Air_separation_unit.h>
#include "ASUUnit_i.h"

using namespace Vision21;
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
    const char* igas;
    const char* ogas_o2;
    const char* ogas_n2;
    bool rv;
    Package p;
    string therm_path="thermo";
    const char* result;
    summary_values summaries;

    fflush(NULL);

    igas = executive_->GetImportData(id_, 0); //port 0 will be the gas input port;

    if (string(igas)=="")
      {
	error("Missing input input.");
	return;
      }

    p.SetSysId("gas_in.xml");
    p.Load(igas, strlen(igas)); 

    Gas *AirIn = new Gas;
    Gas *O2stream = new Gas;
    Gas *N2stream = new Gas;

    V21Helper gashelper(therm_path.c_str());
    gashelper.IntToGas(&(p.intfs[0]), *AirIn);
    
    // Check incoming
    if(AirIn->gas_composite.T <= 225 || AirIn->gas_composite.T >= 1000) {
      warning("Incoming Gas temperature out of range.");
    }
    
    Air_separation_unit asu_obj;
    
    asu_obj.T_O2stream = T_O2stream;
    asu_obj.P_O2stream = P_O2stream;
    asu_obj.T_N2stream = T_N2stream;
    asu_obj.P_N2stream = P_N2stream ;
    asu_obj.O2_purity  = O2_purity;
    
    asu_obj.Calculate(*AirIn, *O2stream, *N2stream);
  
    // Fill in summary tables
    summaries.insert_summary_val("Energy Requirement UNITS:MW FORMAT:10.4f",
				 asu_obj.energy_requirement/1000);
    summaries.insert_summary_val("Energy Requirement UNITS:kW FORMAT:10.4f",
				 asu_obj.energy_requirement);
    
    // gui->eval(id + " module_power " + to_string(asu_obj.energy_requirement/1000), result);
    
    //cout << "\n T O2 stream = " << asu_obj.T_O2stream
    //     << "\n P O2 stream = " << asu_obj.P_O2stream
    //     << "\n T N2 stream = " << asu_obj.T_N2stream
    //     << "\n P N2 stream = " << asu_obj.P_N2stream
    //     << "\nO2_Purity    = " << asu_obj.O2_purity << "%"
    //     << "\nEnergy Requirement = " << asu_obj.energy_requirement;
    
    // Send data out
    
    // Check outgoing
    if(O2stream->gas_composite.T <= 25 || O2stream->gas_composite.T >= 500) {
      warning("O2 Gas temperature out of range.");
    }
    if(N2stream->gas_composite.T <= 25 || N2stream->gas_composite.T >= 500) {
      warning("N2 Gas temperature out of range.");
    }    
    
    
    p.intfs.resize(1);
    p.SetPackName("ExportData");
    p.SetSysId("test.xml");
    
    gashelper.GasToInt(O2stream, p.intfs[0]);
    ogas_o2 = p.Save(rv);
    executive_->SetExportData(id_, 0, ogas_o2);

    gashelper.GasToInt(N2stream, p.intfs[0]);
    ogas_n2 = p.Save(rv);
    executive_->SetExportData(id_, 1, ogas_n2);

    gashelper.SumToInt(&summaries, p.intfs[0]);
    result = p.Save(rv);

    executive_->SetModuleResult(id_, result); //marks the end the execution
    
    delete AirIn;
    delete O2stream;
    delete N2stream;
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
    std::cout << UnitName_ << " :GetUserData called\n";
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
    std::cout << UnitName_ << " :SetParams called\n";
    Package p;
        
    p.SetSysId("gui.xml");
    p.Load(param, strlen(param));
    //Now make use of p.intfs to get your GUI vars out
    T_O2stream = p.intfs[0].getDouble("o2_temp");
    P_O2stream = p.intfs[0].getDouble("o2_pres");
    O2_purity = p.intfs[0].getDouble("o2_purity");
    T_N2stream = p.intfs[0].getDouble("n2_temp");
    P_N2stream = p.intfs[0].getDouble("n2_pres");
    
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
    std::cout << UnitName_ << " :SetID called " << id << endl;
    
    id_ = id;
  }
  
CORBA::Long Body_Unit_i::GetID (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  std::cout << UnitName_ << " :GetID called\n";
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
    UnitName_ = std::string(name);
    std::cout << UnitName_ << " :SetName called\n";
  }
  
char * Body_Unit_i::GetName (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    std::cout << UnitName_ << " :GetName called\n";

    return CORBA::string_dup(UnitName_.c_str());
  }

void Body_Unit_i::error (std::string msg)
{
  Package p;
  const char* result;
  bool rv;
  p.SetPackName("result");
  p.SetSysId("result.xml");
  msg+="\n";
  executive_->SetModuleMessage(id_, msg.c_str());
  p.intfs.clear();
  result = p.Save(rv);
  return_state = 1;
  executive_->SetModuleResult(id_, result); //this marks the end the execution
}

void Body_Unit_i::warning (std::string msg)
{
  msg+="\n";
  executive_->SetModuleMessage(id_, msg.c_str());
}
