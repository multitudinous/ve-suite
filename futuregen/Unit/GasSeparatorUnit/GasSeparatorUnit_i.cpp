#include "V21Helper.h"
#include "GasSeparatorUnit_i.h"

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
    const char* ogas;
    const char* ogas_pure;
    bool rv;
    Package p;
    string therm_path="thermo";
    int i;
    const char* result;
    summary_values summaries;

    igas = executive_->GetImportData(id_, 0); //port 0 will be the gas input port;

    if (string(igas)=="")
      {
	error("Missing input input.");
	return;
      }

    p.SetSysId("gas_in.xml");
        
    Gas *gas_in = new Gas;
    V21Helper gashelper(therm_path.c_str());
    
    p.Load(igas, strlen(igas)); 
    gashelper.IntToGas(&(p.intfs[0]), *gas_in);
    
    map<string,int>::const_iterator it;

    it = gas_in->specie.find(specie);
    if(it==gas_in->specie.end()){
      error("Specie not in inlet stream.");
      return;
    }
    
    double n_sp_in = gas_in->gas_composite.M*
    gas_in->gas_composite.comp_specie[it->second]/gas_in->gas_composite.mw();
    double n_others_in = gas_in->gas_composite.M*
      (1.0 - gas_in->gas_composite.comp_specie[it->second])/gas_in->gas_composite.mw();
    double n_sp_pure = (1.0 - remain/100.0)*n_sp_in;
    double n_sp_out = remain/100.0*n_sp_in;
    double n_others_pure_max = n_sp_pure*(100.0 - purity)/purity;
    double n_others_pure = n_others_pure_max;
    if(n_others_in < n_others_pure_max) n_others_pure = n_others_in;
    double n_others_out = n_others_in - n_others_pure;
    if(n_others_out<0.0) n_others_out = 0.0;
    
    Gas *gas_pure = new Gas(*gas_in);
    int nspc = gas_pure->specie.size(), is;
    
    gas_pure->gas_composite.comp_specie[it->second] = n_sp_pure/(n_sp_pure + n_others_pure);
    double sum = 0.0;
    for(is=0; is<nspc; is++) if(is!=it->second) sum += gas_pure->gas_composite.comp_specie[is];
    for(is=0; is<nspc; is++) if(is!=it->second)
      gas_pure->gas_composite.comp_specie[is] *= 
	(1.0 - gas_pure->gas_composite.comp_specie[it->second])/sum;
    gas_pure->gas_composite.M = (n_sp_pure + n_others_pure)*gas_pure->gas_composite.mw();
    
    Gas *gas_out = new Gas(*gas_in);

    gas_out->gas_composite.comp_specie[it->second] = n_sp_out/(n_sp_out + n_others_out);
    sum = 0.0;
    for(is=0; is<nspc; is++) if(is!=it->second) sum += gas_out->gas_composite.comp_specie[is];
    for(is=0; is<nspc; is++) if(is!=it->second)
      gas_out->gas_composite.comp_specie[is] *= 
	(1.0 - gas_out->gas_composite.comp_specie[it->second])/sum;
    gas_out->gas_composite.M = (n_sp_out + n_others_out)*gas_out->gas_composite.mw();
    
    p.intfs.resize(1); //each port has its own package
    p.SetPackName("ExportData");
    p.SetSysId("test.xml");
    
    gashelper.GasToInt(gas_pure, p.intfs[0]);
    ogas_pure = p.Save(rv);
    executive_->SetExportData(id_, 0, ogas_pure);

    gashelper.GasToInt(gas_out, p.intfs[0]);
    ogas = p.Save(rv);
    executive_->SetExportData(id_, 1, ogas);

    p.intfs.clear();
    result = p.Save(rv); 
    std::cout<<"cp5\n";
    executive_->SetModuleResult(id_, result); //marks the end the execution
    delete gas_pure;
    delete gas_out;
    delete gas_in;

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
    
    specie = p.intfs[0].getString("specie");
    purity = p.intfs[0].getDouble("purity");
    remain = p.intfs[0].getDouble("remain");
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
// Interpolates to find f(xt) along the line defined by the other four variables.
