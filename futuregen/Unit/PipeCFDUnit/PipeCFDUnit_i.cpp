#include "V21Helper.h"
#include "PipeCFD.h"

#include "PipeCFDUnit_i.h"

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
  int i, j;

  const char* igas;
  const char* ogas;

  Gas *gas_in  = NULL;
  Gas *gas_out = NULL;

  bool rv;
  Package p;
  std::string therm_path="thermo";
  const char* result;
  V21Helper gashelper(therm_path.c_str());

  /////////
  // Gas in
  /////////

  igas = executive_->GetImportData(id_, 0); 

  if (std::string(igas)=="") {
    error("Missing gas in");
    return;
  }
  
  p.SetSysId("gas_in.xml");
  p.Load(igas, strlen(igas)); 
    
  gas_in = new Gas;
  
  gashelper.IntToGas(&(p.intfs[0]), *gas_in);

  //////////
  // Profile
  //////////

  Types::Profile_var prof_in;

  executive_->GetProfileData(id_, 0, prof_in);

  if(prof_in->profile_vars.length() != prof_in->profile_vals.length())
    cout << "Error profile_vars != profile_vals\n";

  // Test
  for(i=0; i<prof_in->profile_vals.length(); i++) {
    cout << prof_in->profile_vars[i] << endl;
    for(j=0; j<prof_in->profile_vals[i].length(); j++) {
      cout << prof_in->profile_vals[i][j] << endl;
    }
  }

  ////////
  // Next
  ////////

  PipeCFD gas_model;

  ///////////
  // Set vars
  ///////////


  //////////
  // Execute
  //////////

  gas_out = new Gas;
  gas_out->thermo_database = gashelper.thermo_database;

  summary_values summaries;

  gas_model.execute(gas_in, gas_out, &summaries);


  /////////
  // Finish
  /////////

  p.intfs.resize(1);
  
  gashelper.GasToInt(gas_out, p.intfs[0]);
 
  p.SetPackName("ExportData");
  p.SetSysId("gasout.xml");
  ogas = p.Save(rv);
  executive_->SetExportData(id_, 0, ogas);

  p.intfs.clear();
  p.intfs.resize(1);
  gashelper.SumToInt(&summaries, p.intfs[0]);
  result = p.Save(rv);

  // Profile
  Types::Profile prof_out;

  prof_out.profile_vars.length(10);
  prof_out.profile_vals.length(10);

  for(i=0; i<prof_out.profile_vals.length(); i++)
    prof_out.profile_vals[i].length(gas_out->gas_cell.size());

  prof_out.profile_vars[0] = "X_LOC";
  prof_out.profile_vars[1] = "Y_LOC";
  prof_out.profile_vars[2] = "Z_LOC";
  prof_out.profile_vars[3] = "U_VEL";
  prof_out.profile_vars[4] = "V_VEL";
  prof_out.profile_vars[5] = "W_VEL";
  prof_out.profile_vars[6] = "EFF";
  prof_out.profile_vars[7] = "ETA";
  prof_out.profile_vars[8] = "CHI";
  prof_out.profile_vars[9] = "TEMPERATURE";

  std::vector<GasCell>::iterator iter;
  for(iter=gas_out->gas_cell.begin(), i=0; iter!=gas_out->gas_cell.end(); iter++, i++) {
    prof_out.profile_vals[0][i] = iter->node_location[0];
    prof_out.profile_vals[1][i] = iter->node_location[1];
    prof_out.profile_vals[2][i] = iter->node_location[2];
    prof_out.profile_vals[3][i] = iter->velocity[0];
    prof_out.profile_vals[4][i] = iter->velocity[1];
    prof_out.profile_vals[5][i] = iter->velocity[2];
    prof_out.profile_vals[6][i] = iter->eff;
    prof_out.profile_vals[7][i] = iter->eta;
    prof_out.profile_vals[8][i] = iter->chi;
    prof_out.profile_vals[9][i] = iter->T;
  }

  executive_->SetProfileData(id_, 0, prof_out);

  // Done
  executive_->SetModuleResult(id_, result);

  if(gas_out)  delete gas_out;
  if(gas_in)   delete gas_in;

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
    std::cout << UnitName_ <<" :GetStatusMessages called" << endl;

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
    if (string(param)=="") return;

    std::cout<<UnitName_<<" :SetParams called"<<endl;

    Package p;
    p.SetSysId("temp.xml");
    p.Load(param, strlen(param));

    // Now make use of p.intfs to get your GUI vars out
 

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
    std::cout<<UnitName_<<" :SetID called"<<endl;

    id_=id;
  }
  
CORBA::Long Body_Unit_i::GetID (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
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
  msg = "PipeCFD: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
  p.intfs.clear();
  result = p.Save(rv);
  return_state = 1;
  executive_->SetModuleResult(id_, result); //this marks the end the execution
}

void Body_Unit_i::warning (std::string msg)
{
  msg = "PipeCFD: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
}
