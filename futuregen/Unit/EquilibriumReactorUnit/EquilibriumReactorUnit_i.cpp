#include "V21Helper.h"
#include "EquilibriumReactorUnit_i.h"

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
  
  Gas *gas_in_data = new Gas;
  
  V21Helper gashelper(therm_path.c_str());
  gashelper.IntToGas(&(p.intfs[0]), *gas_in_data);
  
  // Check incoming
  if(gas_in_data->gas_composite.T <= 200 || gas_in_data->gas_composite.T >= 3000) {
    warning("Incoming gas temperature out of range.");
  }
  
  Gas *gas_out_data = new Gas;
  gas_out_data->copy(*gas_in_data);
  gas_out_data->gas_composite.equilb();
  
    // get exact mercury
  double mole_sec = gas_in_data->gas_composite.M/gas_in_data->gas_composite.mw();
  double mfr = gas_in_data->gas_composite.getFrac("HG");
  double mole_HG_sec = 0.0;
  if(mfr>0.0) mole_HG_sec += mole_sec*mfr;
  mfr = gas_in_data->gas_composite.getFrac("HGCL2");
  if(mfr>0.0) mole_HG_sec += mole_sec*mfr;

  // split the HG between el HG and HGCL2 per equilibrium result
  double fracHG = 0.0, totHG = 0.0;
  mfr = gas_out_data->gas_composite.getFrac("HG");
  if(mfr>0.0){
     fracHG += mfr;
     totHG += mfr;
  }
  mfr = gas_out_data->gas_composite.getFrac("HGCL2");
  if(mfr>0.0) totHG += mfr;
  if(totHG) fracHG /= totHG;
  gas_out_data->thermo_database = gashelper.thermo_database;
  mole_sec = gas_out_data->gas_composite.M/gas_out_data->gas_composite.mw();
  if(totHG>0.0){
     if(fracHG>0.0) gas_out_data->gas_composite.setFrac("HG",mole_HG_sec*fracHG/mole_sec);
     if(fracHG<1.0) gas_out_data->gas_composite.setFrac("HGCL2",mole_HG_sec*(1.0-fracHG)/mole_sec);
  }

  // Check incoming
  if(gas_out_data->gas_composite.T <= 200 || gas_out_data->gas_composite.T >= 3000) {
    warning("Outgoing gas temperature out of range.");
  }
  
  
  //fill out the output stream  
  p.intfs.resize(1); //each port has its own package
  gashelper.GasToInt(gas_out_data, p.intfs[0]);
 
  p.SetPackName("ExportData");
  p.SetSysId("gasout.xml");
  ogas = p.Save(rv);
  executive_->SetExportData(id_, 0, ogas);

  p.intfs.clear();
  result = p.Save(rv);

  executive_->SetModuleResult(id_, result); //marks the end the execution
  
  if(gas_out_data) delete gas_out_data;
  if(gas_in_data)  delete gas_in_data;
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
    //No UI variables
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
  msg = "Equilibrium Reactor: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
  p.intfs.clear();
  result = p.Save(rv);
  return_state = 1;
  executive_->SetModuleResult(id_, result); //this marks the end the execution
}

void Body_Unit_i::warning (std::string msg)
{
  msg = "Equilibrium Reactor: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
}
