#include "V21Helper.h"
#include "Gasifier0D.h"

#include "Gasifier0DUnit_i.h"

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

  // Add your implementation here
  const char* o2gas;
  const char* ipart;
  const char* stage2gas;
  const char* ogas;

  bool rv;
  Package p;
  string therm_path="thermo";
  const char* result;

  summary_values summaries;

  Gas *ox_in    = NULL;
  Gas *part_in  = NULL;
  Gas *stage2in = NULL;

  V21Helper gashelper(therm_path.c_str());
 
  /////////////////
  // OXIDANT stream
  /////////////////

  o2gas = executive_->GetImportData(id_, 0); //port 0 will be the o2 input port;

  if (std::string(o2gas)=="")
    {
      error("Missing o2");
      return;
    }
  
  p.SetSysId("o2_in.xml");
  p.Load(o2gas, strlen(o2gas)); 
    
  ox_in = new Gas();
  
  gashelper.IntToGas(&(p.intfs[0]), *ox_in);

  //////////////////
  // PARTICLE stream
  //////////////////

  ipart = executive_->GetImportData(id_, 1); //port 1 will be the paricle input port;

  double ash_in_char = 0.0, char_size=100.0;
  double char_sd=char_size*0.5;

  if (std::string(ipart)=="")
    {
      warning("No particle recycle stream");
    }
  else
    {
      p.SetSysId("part_in.xml");
      p.Load(ipart, strlen(ipart)); 
       
      part_in = new Gas();
  
      gashelper.IntToGas(&(p.intfs[0]), *part_in);
    }

  //////////////////////////
  // SECOND STAGE GAS stream
  //////////////////////////
  
  stage2gas = executive_->GetImportData(id_, 2); //port 2 will be the stage 2 input port;

  if (std::string(stage2gas)=="")
    {
      if (_stage!=1)
	warning("No 2nd stage gas stream.");
    }
  else
    {
      p.SetSysId("stage2gas_in.xml");
      p.Load(stage2gas, strlen(stage2gas)); 
      
      stage2in = new Gas();
      
      gashelper.IntToGas(&(p.intfs[0]), *stage2in);
    }
  
  ////////
  // NEXT
  ////////
  
  Gasifier0D gas_model(gashelper.thermo_database);

  //////////
  // Oxidant
  //////////

  if (_stage!=1) {
    gas_model._ox_temp[0] = ox_in->gas_composite.T;
    gas_model._ox_temp[1] = ox_in->gas_composite.T;
    gas_model._ox_temp[2] = 300.0;
    gas_model._ox_flow[0] = ox_in->gas_composite.M;
    gas_model._ox_flow[1] = ox_in->gas_composite.M;
    gas_model._ox_flow[2] = 0.0;

    if(stage2in) {
      gas_model._ox_temp[2] = stage2in->gas_composite.T;
      gas_model._ox_flow[2] = stage2in->gas_composite.M;
    }
  }
  else {
    gas_model._ox_temp[0] = ox_in->gas_composite.T;
    gas_model._ox_temp[1] = 0.0;
    gas_model._ox_temp[2] = 0.0;
    gas_model._ox_flow[0] = ox_in->gas_composite.M;
    gas_model._ox_flow[1] = 0.0;
    gas_model._ox_flow[2] = 0.0;
  }
  
  ////////////
  // Particles
  ////////////
 
  gas_model._char_flow[0] = 0.0;
  gas_model._char_flow[1] = 0.0;
  gas_model._char_flow[2] = 0.0;

  gas_model._char_pct[0] = 0.0;
  gas_model._char_pct[1] = 0.0;
  gas_model._char_pct[2] = 0.0;
  
  gas_model._ash_in_char = 0.0;
  gas_model._char_size   = 100.0;
  gas_model._char_sd     = gas_model._char_size * 0.5;

  if(part_in) {
    std::map<string,int>::const_iterator itp;

    itp = part_in->particle.find("CHAR");
    if(itp == part_in->particle.end()) {
      error("CHAR missing as particle component");
      return_state = 1;
      return;
    }

    gas_model._char_flow[0] = part_in->gas_composite.M_particle;
    gas_model._char_pct[0]  = part_in->gas_composite.comp_particle[itp->second]*100.0;

    itp = part_in->particle.find("ASH");
    if(itp == part_in->particle.end()) {
      error("ASH missing as particle component");
      return_state = 1;
      return;
    }
    gas_model._ash_in_char = part_in->gas_composite.comp_particle[itp->second];
    
    itp = part_in->particle.find("COAL");
    if(itp == part_in->particle.end()) {
      error("COAL missing as particle component");
      return_state = 1;
      return;
    }
    if(part_in->gas_composite.comp_particle[itp->second]>0.0) {
      error("NO raw coal can exist in recycled char");
      return_state = 1;
      return;
    }

    itp = part_in->particle.find("WATER");
    if(itp == part_in->particle.end()) {
      error("WATER missing as particle component");
      return_state = 1;
      return;
    }
    if(part_in->gas_composite.comp_particle[itp->second]>0.0) {
      error("NO water can exist in recycled char");
      return_state = 1;
      return;
    }
    
    gas_model._char_size = part_in->gas_composite.mean_size * 1.0e6;
    gas_model._char_sd = part_in->gas_composite.size_variance * 1.0e12;
    gas_model._char_sd = sqrt(gas_model._char_sd);
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
  }

  ////////////////
  // GUI variables
  ////////////////
 
  gas_model._size_50       = _size_50;
  gas_model._size_200      = _size_200; 
  gas_model._press_drop    = _press; 
  gas_model._stage         = _stage;
  gas_model._diameter      = _diameter;
  gas_model._length1       = _length1;
  gas_model._length2       = _length2;
  gas_model._burnout_gui   = _burnout_gui;
  gas_model._heatloss_gui1 = _heatloss_gui1;
  gas_model._heatloss_gui2 = _heatloss_gui2;
  gas_model._LD            = _LD;
  gas_model._specify_geom  = _specify_geom;
  gas_model._design_mode   = _design_mode;
  gas_model._rwall1        = _rwall1;
  gas_model._rwall2        = _rwall2;
  gas_model._emis1         = _emis1;
  gas_model._emis2         = _emis2;
  gas_model._back_temp     = _back_temp;
  gas_model._slag_eff      = _slag_eff;
  
  /////////////
  // GUI - coal
  /////////////

  gas_model.setCoalType(_coal_type);
 
  //////////
  // Execute
  //////////

  //gas_model.print_inputs();

  Gas *gas_out = new Gas;

  gas_model.execute(ox_in, stage2in, gas_out, &summaries);

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
  if(part_in)  delete part_in;
  if(stage2in) delete stage2in;

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

    //if (param!=NULL) std::cout<<param<<std::endl;
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
    
    _LD            = p.intfs[0].getDouble ("LD_ratio");
    _diameter      = p.intfs[0].getDouble ("geo_diam");
    _length1       = p.intfs[0].getDouble ("geo_stage1_len");
    _length2       = p.intfs[0].getDouble ("geo_stage2_len");
    
    _rwall1        = p.intfs[0].getDouble ("geo_stage1_wall");
    _rwall2        = p.intfs[0].getDouble ("geo_stage2_wall");
    _emis1         = p.intfs[0].getDouble ("stage1_emis");
    _emis2         = p.intfs[0].getDouble ("stage2_emis");

    _burnout_gui   = p.intfs[0].getDouble ("burn_out");
    _heatloss_gui1 = p.intfs[0].getDouble ("stage1_heatloss");
    _heatloss_gui2 = p.intfs[0].getDouble ("stage2_heatloss");

    _back_temp     = p.intfs[0].getDouble ("backside_temp");
    _slag_eff      = p.intfs[0].getDouble ("slag_eff");

    _press         = p.intfs[0].getDouble ("pres_drop");
    _stage         = p.intfs[0].getInt    ("stage");

    _specify_geom  = p.intfs[0].getInt    ("spec_geometry");
    _design_mode   = p.intfs[0].getInt    ("des_mode");

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
