#include "GasSplitterUnit_i.h"
#include "V21Helper.h"

// Implementation skeleton constructor
Body_Unit_i::Body_Unit_i (Body::Executive_ptr exec, std::string name)
  : executive_(Body::Executive::_duplicate(exec))
{
  UnitName_=name;
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

    igas = executive_->GetImportData(id_, 0); //port 0 will be the gas input port;
    
    if (!igas)
      {
	cerr<<"Missing initial input."<<endl;
	return;
      }

    p.SetSysId("gas_in.xml");
    p.Load(igas, strlen(igas)); 

    Gas *gas_in = new Gas();

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
	    delete gas_out[i];
	    p.SetPackName("ExportData");
	    p.SetSysId("test.xml");
	    ogas[i] = p.Save(rv);
    	    executive_->SetExportData(id_, i, ogas[i]);
	  } 
	else 
	  ;
      }
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
    std::cout<<UnitName_<<" : Instant calculation, already finished"<<endl;
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
    std::cout<<UnitName_<<" : Instant calculation, already finished"<<endl;
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
    std::cout<<UnitName_<<" : Instant calculation, already finished"<<endl;
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
    std::cout<<UnitName_<<" :GetStatusMessage called"<<endl;
    return CORBA::string_dup(status_.c_str());
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
