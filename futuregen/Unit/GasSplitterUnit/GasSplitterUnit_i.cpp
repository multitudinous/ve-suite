#include "V21Helper.h"
#include "GasSplitterUnit_i.h"

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
    const char* ogas[4];
    bool rv;
    Package p;
    string therm_path="thermo";
    int i;
    const char* result;

    fflush(NULL);

    igas = executive_->GetImportData(id_, 0); //port 0 will be the gas input port;
    if (string(igas)=="")
      {
	error("Missing input input.");
	return;
      }

    p.SetSysId("gas_in.xml");
    p.Load(igas, strlen(igas)); 

    Gas *gas_in = new Gas;

    V21Helper gashelper(therm_path.c_str());
    gashelper.IntToGas(&(p.intfs[0]), *gas_in);

    p.intfs.resize(1); //each port has its own package
    Gas *gas_out[4];

    for(i=0; i<4; i++)
      gas_out[i] = new Gas(*gas_in);

    for(i=0; i<4; i++) 
      {
	if(pct[i] > 1e-6) 
	  {
	    gas_out[i]->gas_composite.M = gas_in->gas_composite.M * pct[i] / 100;
	    gas_out[i]->gas_composite.M_particle = gas_in->gas_composite.M_particle * pct[i] / 100;
	    gashelper.GasToInt(gas_out[i], p.intfs[0]);
	    
	    p.SetPackName("ExportData");
	    p.SetSysId("test.xml");
	    ogas[i] = p.Save(rv);
    	    executive_->SetExportData(id_, i, ogas[i]);
	  } 
	else 
	  ;
      }
    if(gas_in) delete gas_in;
    p.intfs.clear();
    result = p.Save(rv);
    executive_->SetModuleResult(id_, result); //marks the end the execution

    for(i=0; i<4; i++)
      if(gas_out[i]) delete gas_out[i];
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
        
    p.SetSysId("gui.xml");
    p.Load(param, strlen(param));
    //Now make use of p.intfs to get your GUI vars out
    pct[0] = p.intfs[0].getDouble("percent_port1");
    pct[1] = p.intfs[0].getDouble("percent_port2");
    pct[2] = p.intfs[0].getDouble("percent_port3");
    pct[3] = p.intfs[0].getDouble("percent_port4");
    
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
  msg = "GasSplitter: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
  p.intfs.clear();
  result = p.Save(rv);
  return_state = 1;
  executive_->SetModuleResult(id_, result); //this marks the end the execution
}

void Body_Unit_i::warning (std::string msg)
{
  msg = "GasSplitter: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
}
