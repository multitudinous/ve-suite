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
  int i;

  const char* o2gas;
  const char* ogas;

  Gas *ox_in   = NULL;
  Gas *gas_out = NULL;

  bool rv;
  Package p;
  std::string therm_path="thermo";
  const char* result;
  V21Helper gashelper(therm_path.c_str());

  /////////////////
  // OXIDANT stream
  /////////////////
  
  o2gas = executive_->GetImportData(id_, 0); //port 0 will be the o2 input port;

  if (std::string(o2gas)=="") {
    error("Missing o2");
    return;
  }
  
  p.SetSysId("o2_in.xml");
  p.Load(o2gas, strlen(o2gas)); 
    
  ox_in = new Gas;
  
  gashelper.IntToGas(&(p.intfs[0]), *ox_in);

  ////////
  // NEXT
  ////////

  PipeCFD gas_model;

  //////////
  // Oxidant
  //////////

  if (_stage != 1) {
    gas_model._ox_temp[0] = ox_in->gas_composite.T;
    gas_model._ox_temp[1] = ox_in->gas_composite.T;
    gas_model._ox_temp[2] = 300.0;
    gas_model._ox_flow[0] = ox_in->gas_composite.M / 2;
    gas_model._ox_flow[1] = ox_in->gas_composite.M / 2;
    gas_model._ox_flow[2] = 0.0;
  }
  else {
    gas_model._ox_temp[0] = ox_in->gas_composite.T;
    gas_model._ox_temp[1] = 0.0;
    gas_model._ox_temp[2] = 0.0;
    gas_model._ox_flow[0] = ox_in->gas_composite.M;
    gas_model._ox_flow[1] = 0.0;
    gas_model._ox_flow[2] = 0.0;
  }

  /////////////
  // GUI Arrays
  /////////////
  
  for(i=0; i<3; i++) {
    gas_model._stm_temp  [i] = _stm_temp  [i];
    gas_model._stm_flow  [i] = _stm_flow  [i];
    gas_model._slur_temp [i] = _slur_temp [i];
    gas_model._slur_flow [i] = _slur_flow [i];
    gas_model._coal_pct  [i] = _coal_pct  [i];
    gas_model._char_pct  [i] = _char_pct  [i];
  }

  ////////////////
  // GUI variables
  ////////////////
 
  gas_model._size_50       = _size_50;
  gas_model._size_200      = _size_200; 
  gas_model._press_drop    = _press; 
  gas_model._stage         = _stage;
  
  /////////////
  // GUI - coal
  /////////////

  gas_model.setCoalType(_coal_type);
 
  //////////
  // Execute
  //////////

  gas_out = new Gas;
  gas_out->thermo_database = gashelper.thermo_database;

  summary_values summaries;

  gas_model.execute(ox_in, gas_out, &summaries);

  // TEMPORARY COAL
  gas_out->_wic_C = gas_model._wic_C;
  gas_out->_wic_H = gas_model._wic_H;
  gas_out->_wic_O = gas_model._wic_O;
  gas_out->_wic_N = gas_model._wic_N;
  gas_out->_wic_S = gas_model._wic_S;
  gas_out->_wic_CL = gas_model._wic_CL;
  gas_out->_ash_ult = gas_model._ash_ult;
  gas_out->_ash_prox = gas_model._ash_prox;
  gas_out->_proxH2O = gas_model._proxH2O;
  gas_out->_proxVM = gas_model._proxVM;
  gas_out->_proxFC = gas_model._proxFC;
  gas_out->_hhv = gas_model._hhv;
  gas_out->_comp1 = gas_model._comp1;
  gas_out->_comp2 = gas_model._comp2;
  gas_out->_comp3 = gas_model._comp3;
  gas_out->_comp4 = gas_model._comp4;
  gas_out->_comp5 = gas_model._comp5;
  gas_out->_comp6 = gas_model._comp6;
  gas_out->_comp7 = gas_model._comp7;
  gas_out->_comp8 = gas_model._comp8;
  gas_out->_comp9 = gas_model._comp9;
  gas_out->_comp10 = gas_model._comp10;
  gas_out->_comp11 = gas_model._comp11;
  gas_out->_comp12 = gas_model._comp12;
  if(_stage==1) gas_out->_coal_feedRate = _slur_flow[0] * _coal_pct[0];
  else          gas_out->_coal_feedRate = _slur_flow[0] * _coal_pct[0] +
		  _slur_flow[1] * _coal_pct[1] + _slur_flow[2] * _coal_pct[2];

  /////////
  // Finish
  /////////

  p.intfs.resize(1); //each port has its own package
  
  gashelper.GasToInt(gas_out, p.intfs[0]);
 
  p.SetPackName("ExportData");
  p.SetSysId("gasout.xml");
  ogas = p.Save(rv);
  executive_->SetExportData(id_, 0, ogas);

  p.intfs.clear();
  p.intfs.resize(1);
  gashelper.SumToInt(&summaries, p.intfs[0]);
  result = p.Save(rv);

  executive_->SetModuleResult(id_, result); //marks the end the execution

  if(gas_out)  delete gas_out;
  if(ox_in)    delete ox_in;

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
    double temp;

    if (string(param)=="")
		return;
    std::cout<<UnitName_<<" :SetParams called"<<endl;

    Package p;
    p.SetSysId("temp.xml");
    p.Load(param, strlen(param));

    //Now make use of p.intfs to get your GUI vars out
    temp = p.intfs[0].getDouble("temp");

    _coal_type     = p.intfs[0].getString ("coal_type");
    _size_50       = p.intfs[0].getDouble ("size_50");
    _size_200      = p.intfs[0].getDouble ("size_200");

    _stm_temp[0]   = p.intfs[0].getDouble ("steam_temp1");
    _stm_temp[1]   = p.intfs[0].getDouble ("steam_temp2");
    _stm_temp[2]   = p.intfs[0].getDouble ("steam_temp3");

    _stm_flow[0]   = p.intfs[0].getDouble ("steam_flrt1");
    _stm_flow[1]   = p.intfs[0].getDouble ("steam_flrt2");
    _stm_flow[2]   = p.intfs[0].getDouble ("steam_flrt3");

    _slur_temp[0]  = p.intfs[0].getDouble ("slurry_temp1");
    _slur_temp[1]  = p.intfs[0].getDouble ("slurry_temp2");
    _slur_temp[2]  = p.intfs[0].getDouble ("slurry_temp3");

    _slur_flow[0]  = p.intfs[0].getDouble ("slurry_flrt1");
    _slur_flow[1]  = p.intfs[0].getDouble ("slurry_flrt2");
    _slur_flow[2]  = p.intfs[0].getDouble ("slurry_flrt3");

    _coal_pct[0]   = p.intfs[0].getDouble ("coal_percent1");
    _coal_pct[1]   = p.intfs[0].getDouble ("coal_percent2");
    _coal_pct[2]   = p.intfs[0].getDouble ("coal_percent3");
    
    _char_pct[0]   = p.intfs[0].getDouble ("char_percent1");
    _char_pct[1]   = p.intfs[0].getDouble ("char_percent2");
    _char_pct[2]   = p.intfs[0].getDouble ("char_percent3");

    _press         = p.intfs[0].getDouble ("pres_drop");
    _stage         = p.intfs[0].getInt    ("stage");

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
