#include "V21Helper.h"
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
  Gas *ox_in;
  Gas *part_in;
  Gas *stage2in;

  double ox_temp   [3];
  double ox_flow   [3];
  double stm_temp  [3];
  double stm_flow  [3];
  double slur_temp [3];
  double slur_flow [3];
  double coal_pct  [3];
  double char_pct  [3];
  double angle_fi  [3];
  double char_flow [3];

  char_pct[0] = char_pct[1] = char_pct[2] = 0.0;
  fflush(NULL);
  std::cout<<"cp1\n";

  V21Helper gashelper(therm_path.c_str());
  o2gas = executive_->GetImportData(id_, 0); //port 0 will be the o2 input port;

  if (!o2gas)
    {
      error("Missing o2");
      return;
    }
  
  p.SetSysId("o2_in.xml");
  p.Load(o2gas, strlen(o2gas)); 
    
  ox_in = new Gas();
  
  gashelper.IntToGas(&(p.intfs[0]), *ox_in);

  ipart = executive_->GetImportData(id_, 1); //port 1 will be the paricle input port;

  double ash_in_char = 0.0, char_size=100.0;
  double char_sd=char_size*0.5;

  if (!ipart)
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

  stage2gas = executive_->GetImportData(id_, 2); //port 2 will be the stage 2 input port;

  if (!stage2gas)
    {
      if (stage)
	warning("No 2nd stage gas stream.");
    }
  else
    {
      p.SetSysId("part_in.xml");
      p.Load(stage2gas, strlen(stage2gas)); 
      
      stage2in = new Gas();
      
      gashelper.IntToGas(&(p.intfs[0]), *stage2in);
    }
  
  if (ipart) 
    {
      std::map<string,int>::const_iterator itp;
      itp = part_in->particle.find("CHAR");
      if(itp == part_in->particle.end()) 
	{
	  error("CHAR missing as particle component");
	  return;
	}
      char_pct[0] = part_in->gas_composite.comp_particle[itp->second]*100.0;
    
      itp = part_in->particle.find("ASH");
      if(itp == part_in->particle.end()) 
	{
	  error("ASH missing as particle component");
	  return;
	}

      ash_in_char = part_in->gas_composite.comp_particle[itp->second];
    
      itp = part_in->particle.find("COAL");
      if(itp == part_in->particle.end()) 
	{
	  error("COAL missing as particle component");
	  return;
	}
      if(part_in->gas_composite.comp_particle[itp->second]>0.0) 
	{
	  error("NO raw coal can exist in recycled char");
	  return;
	}

      itp = part_in->particle.find("WATER");
      if(itp == part_in->particle.end()) 
	{
	  error("WATER missing as particle component");
	  return;
	}
      if(part_in->gas_composite.comp_particle[itp->second]>0.0) 
	{
	  error("NO water can exist in recycled char");
	  return;
	}
    
      char_size = part_in->gas_composite.mean_size*1.0e6;
      char_sd = part_in->gas_composite.size_variance*1.0e12;
      char_sd = sqrt(char_sd);
    }
  else 
    {
      char_pct[0] = 0.0;
      ash_in_char = 0.0;
    } // if(i_part)

  
  Gas *gas_out_data = new Gas;
  gas_out_data->copy(*gas_in_data);
  gas_out_data->gas_composite.equilb();

  ox_temp[i] = atof(argv2[0]);
  ox_flow[i] = atof(argv2[1]);
  // Steam
  stm_temp[0] = steam_temp1;
  stm_flow[0] = steam_flrt1;
  stm_temp[1] = steam_temp2;
  stm_flow[1] = steam_flrt2;
  stm_temp[2] = steam_temp3;
  stm_flow[2] = steam_flrt3;

  // Slurry
  slur_temp[0] = slurry_temp1;
  slur_flow[0] = slurry_flrt1;
  slur_temp[1] = slurry_temp2;
  slur_flow[1] = slurry_flrt2;
  slur_temp[2] = slurry_temp3;
  slur_flow[2] = slurry_flrt3;
  // Coal percent
  coal_pct[0] = coal_percent1;  
  coal_pct[1] = coal_percent2;
  coal_pct[2] = coal_percent3;
  
  //Supposablly, there will be a Coal Class takes a string name as constructor
  Coal coal("Utah Coal");
  double wic_c  = coal.wic_C;
  double wic_h  = coal.wic_H;
  double wic_o  = coal.wic_O;
  double wic_n  = coal.wic_N;
  double wic_s  = coal.wic_S;
  double wic_cl = coal.wic_CL;

  REAL pres0   = ox_in->gas_composite.P - press_drop;

  REAL ashcomp[12];
  ashcomp[0] =coal.comp1;  ashcomp[1]  =coal.comp2;  ashcomp[2]  =coal.comp3;
  ashcomp[3] =coal.comp4;  ashcomp[4]  =coal.comp5;  ashcomp[5]  =coal.comp6;
  ashcomp[6] =coal.comp7;  ashcomp[7]  =coal.comp8;  ashcomp[8]  =coal.comp9;
  ashcomp[9] =coal.comp10; ashcomp[10] =coal.comp11; ashcomp[11] =coal.comp12;
  
  //# Particle sizes
  int ndpo = 5;
  double dpo[20], modpo[20], dpo_c[20], modpo_c[20];
  double passed_areas[19] = {0.05, 0.2, 0.5, 0.2, 0.05, 0.0, 0.0, 0.00, 0.00,
			     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
  double pct_thru_50  = coal.size_50;
  double pct_thru_200 = coal.size_200;

  histfit_(dpo, modpo, passed_areas, &ndpo, &pct_thru_50, &pct_thru_200);

  createbins_(dpo_c, modpo_c, passed_areas, &ndpo, &char_size, &char_sd);

  REAL dp_mean = 0.0;
  int i;
  for(i=0; i<ndpo; i++) 
    {
      dp_mean += dpo[i]*modpo[i];
      //cout << "MODPO " << modpo[i] << " " << dpo[i] << endl;
    }
  REAL dp_var = 0.0;
  for(i=0; i<ndpo; i++) 
    dp_var += (dpo[i] - dp_mean)*(dpo[i] - dp_mean)*modpo[i];

  //# Calculate heat of formation for coal
  //# Basis (kg)
  double basis = 1;
  //# Heats of formation.
  double co2 = -94.05*4.184e6;
  double h2o = -68.32*4.184e6;
  double so2 = -71*4.184e6;
  double hcl = -92.31/4.184*4.184e6;
  //# Convert wics from wt%-dry to gmols.
  double wicAsh = coal.ash_ult;
  double wicC   = (wic_c * basis) / ((100 - wicAsh) * 12.010);
  double wicH   = (wic_h * basis) / ((100 - wicAsh) * 1.0079);
  //double wicO   = (wic_o * basis) / ((100 - wicAsh) * 15.9994);
  //double wicN   = (wic_n * basis) / ((100 - wicAsh) * 14.010);
  double wicS   = (wic_s * basis) / ((100 - wicAsh) * 32.060);
  double wicCL  = (wic_cl * basis) / ((100 - wicAsh) * 35.453);
  //# Convert hhv from BTU/lb (as received) to (daf).
  double hhv_1 = (-1) * coal.hhv / (1 - (coal.ash_prox + coal.proxH2O) / 100);
  double lhv_1 = (-1) * (coal.hhv - 970.3 * (8.9364 * wic_h/100 * (1-coal.proxH2O/100) + coal.proxH2O/100))
      /(1 - (coal.ash_prox + coal.proxH2O) / 100);
  hhv_1 = hhv_1/4.2995e-4; // Btu/lb -> J/kg
  lhv_1 = lhv_1/4.2995e-4;
  //# HC0 in J/kg @ 298K
  double hhf_coal = (wicC*co2 + (wicH-wicCL)/2*h2o + wicS*so2 + wicCL*hcl - basis * hhv_1);
  //cout << "hhf_coal " << hhf_coal << endl;
  //cout << "hhv_1 " << hhv_1 << endl;
  //cout << "lhv_1 " << lhv_1 << endl;
  //cout << "lhv correction " <<  - 970.3 * (8.9364 * wic_h/100 * (1-proxH2O.get()/100) + proxH2O.get()/100) << endl;
  
  //# Calculate dry, ashfree flowrate of coal
  double coal_m_daf[3], omegac[3], omegah[3], omegaash[3];
  for(i=0; i<3; i++)
    {
      omegac[i] = (coal_pct[i] / 100.0) * (1.0 - coal.proxH2O/100.0 - coal.ash_prox/100.0); // for slurry only
      omegaash[i] = (coal_pct[i]/100.0) * (coal.ash_prox/100.0); // for slurry only
      omegah[i] = char_pct[i]/100.0; // for recycled char only
      coal_m_daf[i] = slur_flow[i] * omegac[i] + char_flow[i]*omegah[i]; 
    }

  //# Calculate flowrate of liquid water
  double omegal[3], h2o_m_liq[3];
  for(i=0; i<3; i++) {
    omegal[i] = (100 - coal_pct[i]) / 100 + coal_pct[i] / 100 * coal.proxH2O/100;
    h2o_m_liq[i] = slur_flow[i] * omegal[i];
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
  std::cout<<"cp5\n";
  executive_->SetModuleResult(id_, result); //marks the end the execution
  delete gas_out_data;
  delete gas_in_data;
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

    if (param!=NULL)
      std::cout<<param<<std::endl;
    std::cout<<UnitName_<<" :SetParams called"<<endl;
    Package p;
    
    p.SetSysId("temp.xml");
    p.Load(param, strlen(param));
    //Now make use of p.intfs to get your GUI vars out
    temp = p.intfs[0].getDouble("temp");

    steam_temp1 = p.intfs[0].getDouble("steam_temp1");
    steam_flrt1 = p.intfs[0].getDouble("steam_flrt1");
    slurry_temp1 = p.intfs[0].getDouble("slurry_temp1");
    slurry_flrt1 = p.intfs[0].getDouble("slurry_flrt1");
    coal_percent1 = p.intfs[0].getDouble("coal_percent1");
    steam_temp2 = p.intfs[0].getDouble("steam_temp2");
    steam_flrt2 = p.intfs[0].getDouble("steam_flrt2");
    slurry_temp2 = p.intfs[0].getDouble("slurry_temp2");
    slurry_flrt2 = p.intfs[0].getDouble("slurry_flrt2");
    coal_percent2 = p.intfs[0].getDouble("coal_percent2");
    steam_temp3 = p.intfs[0].getDouble("steam_temp3");
    steam_flrt3 = p.intfs[0].getDouble("steam_flrt3");
    slurry_temp3 = p.intfs[0].getDouble("slurry_temp3");
    slurry_flrt3 = p.intfs[0].getDouble("slurry_flrt3");
    coal_percent3 = p.intfs[0].getDouble("coal_percent3");
    geo_diam = p.intfs[0].getDouble("geo_diam");
    geo_stage1_len = p.intfs[0].getDouble("geo_stage1_len");
    geo_stage2_len = p.intfs[0].getDouble("geo_stage2_len");
    geo_stage1_wall = p.intfs[0].getDouble("geo_stage1_wall");
    geo_stage2_wall = p.intfs[0].getDouble("geo_stage2_wall");
    burn_out = p.intfs[0].getDouble("burn_out");
    stage1_heatloss = p.intfs[0].getDouble("stage1_heatloss");
    stage2_heatloss = p.intfs[0].getDouble("stage2_heatloss");
    LD_ratio = p.intfs[0].getDouble("LD_ratio");
    stage1_emis = p.intfs[0].getDouble("stage1_emis");
    stage2_emis = p.intfs[0].getDouble("stage2_emis");
    backside_temp = p.intfs[0].getDouble("backside_temp");
    slag_eff = p.intfs[0].getDouble("slag_eff");
    pres_drop = p.intfs[0].getDouble("pres_drop");
    stage = p.intfs[0].getInt("stage");
    spec_geometry = p.intfs[0].getInt("spec_geometry");
    des_mode = p.intfs[0].getInt("des_mode");
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
