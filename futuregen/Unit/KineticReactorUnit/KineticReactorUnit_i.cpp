#include "KineticReactorUnit_i.h"

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

    thermo thm(therm_path);

    // Perform Calculations

    int caseType = case_type; // 0: PFR, 1: PSR    
    //std::string workDir = work_dir.get();
    
    std::string griMechFname = "grimech30.dat";
    std::string griMechThermoFname  = "thermo30.dat";

    std::vector<string> inp_files;
    inp_files.push_back(griMechFname);
    inp_files.push_back(griMechThermoFname);
    
    reks_container reks(inp_files);
    if(caseType == 0)     reks.set_case_flag("CONP");
    else if(caseType ==1) reks.set_case_flag("TTIM");
    else                  reks.set_case_flag("ENRG");
    reks.set_atol(1.0e-14);
    reks.set_rtol(1.0e-8);
    reks.set_time(res_time);
    if(caseType == 1){
      reks.set_ttime_type(0);
      reks.set_start_temp(gas_in->gas_composite.T);
      reks.set_to_ttim(0.0);
      reks.set_slope_ttim(-quench_rate);
    }else if(caseType == 2){
      reks.set_start_temp(gas_in->gas_composite.T);
      reks.set_tinl(gas_in->gas_composite.T);
      reks.set_volume(1.0);
      reks.set_qloss(qloss);
      reks.set_tau(res_time);
    }
    reks.set_state(gas_in->gas_composite.T,gas_in->gas_composite.P*10);    // dynes/cm2
    reks.zero_mol_frac();
    setREKSgasComp(reks, *gas_in);
    reks.norm_molf();
    
    FILE* out_file;
    if( (out_file = fopen("REKS.out", "w")) == NULL ) {
      warning("REKS.out output file not opened");
    }
    
    reks.read_kinetics(inp_files);
    reks_solve REKSsolve;
    
    if( !REKSsolve.solve(reks, out_file) ) { //, conversion, length) ) {
      error("Kinetic solver failed");
      return;
    }

    // Output Gas Data
    
    Gas *gas_out = new Gas(*gas_in);
    setGasComp(reks, *gas_out);

    p.intfs.resize(1); //each port has its own package
    p.SetPackName("ExportData");
    p.SetSysId("test.xml");
    
    gashelper.GasToInt(gas_out, p.intfs[0]);
    ogas = p.Save(rv);
    executive_->SetExportData(id_, 0, ogas);
    
    p.intfs.clear();
    result = p.Save(rv); 

    executive_->SetModuleResult(id_, result); //marks the end the execution
    
    if(gas_in)  delete gas_in;
    if(gas_out) delete gas_out;
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
    case_type = p.intfs[0].getInt("case_type");
    res_time = p.intfs[0].getDouble("res_time");
    qloss = p.intfs[0].getDouble("qloss");
    quench_rate = p.intfs[0].getDouble("quench_rate");
    work_dir = p.intfs[0].getString("work_dir");
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
  msg = "Kinetic Reactor: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
  p.intfs.clear();
  result = p.Save(rv);
  return_state = 1;
  executive_->SetModuleResult(id_, result); //this marks the end the execution
}

void Body_Unit_i::warning (std::string msg)
{
  msg = "Kinetic Reactor: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
}
// Interpolates to find f(xt) along the line defined by the other four variables.
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
		
void Body_Unit_i::setREKSgasComp(reks_container& reks, Gas& gasw) {
	// note, should have all the species in kinetic mech and kinetic thermo
	// as in the gas thermo, or at least as in the current gas list
	reks.zero_mol_frac();
	// loop through all species in REKS gas, if species is in gasw, set the species in reks gas
	int i;
	string name;
	map<string, int>::iterator it;
	for(i=0; i < reks.get_num_specs(); i++) {
		name = reks.get_specie_name(i);
		it = gasw.specie.find(name);
		if(it != gasw.specie.end())
			reks.set_mol_frac(name, gasw.gas_composite.comp_specie[it->second]);
		//cout << "\nSpecies: " << name << "\t" << REKSgas.specs[i].mole_fraction;
	}
	//reks.norm_molf();
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
		
void Body_Unit_i::setGasComp(reks_container& reks, Gas& gasw) {
	// note, should have all the species in kinetic mech and kinetic thermo
	// as in the gas thermo, or at least as in the current gas list
		
	gasw.gas_composite.T = reks.get_temperature();
	gasw.gas_composite.P = reks.get_pressure()/10.0;          // dyne/cm2 to Pa

	// loop through all the species in gas, if species is in REKS gas set the gas species
	int i;
	map<string, int>::const_iterator it, it1;
	string name; double sum = 0.0;
        for(i=0; i<reks.get_num_specs(); i++) {
	  name = reks.get_specie_name(i);
	  it = gasw.specie.find(name);
	  if(it!=gasw.specie.end()){
	    gasw.gas_composite.comp_specie[it->second] = reks.get_specie_mole_fraction(i);
            sum += reks.get_specie_mole_fraction(i);
	  } else {
            it1 = gasw.thermo_database->get_nam_spec().find(name);
	    if(it1!=gasw.thermo_database->get_nam_spec().end()){
	      gasw.addSpecie(name);
	      gasw.gas_composite.comp_specie[gasw.gas_composite.comp_specie.size()-1] = 
		reks.get_specie_mole_fraction(i);
	      sum += reks.get_specie_mole_fraction(i);
	    }
	  }
	}
	cout << " SUM " << sum << endl;

	//gasw.gas_composite.normalize_specie();
}
