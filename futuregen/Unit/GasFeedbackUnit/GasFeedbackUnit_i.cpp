#include "V21Helper.h"
#include "GasFeedbackUnit_i.h"


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
    
    if(iter_counter.find((int)id_)==iter_counter.end())
      iter_counter[(int)id_] = 0;

    if(last_values.find((int)id_)==last_values.end()) {
      std::map<std::string, double> junk;
      last_values[(int)id_] = junk;
    }

    // Add your implementation here
    const char* initial_igas;
    const char* feedbck_igas;
    const char* outport;
    const char* result;
    bool rv;
    Package p;
    
    string therm_path="thermo";
    V21Helper gashelper(therm_path.c_str());

    const std::map<std::string, int>& name_map = gashelper.thermo_database->get_nam_spec();
    vector<string> species; // available species, from thermo
    species.clear();

    species.push_back("Temperature");
    species.push_back("Pressure");
    species.push_back("Flowrate");
    species.push_back("Particle Flowrate");

    map<std::string, int>::const_iterator iter;
    for(iter=name_map.begin(); iter!=name_map.end(); iter++)
      species.push_back(iter->first);

    //Input Ports
    
    initial_igas = executive_->GetImportData(id_, 0); //port 0 will be the initial input port;
    
    if (string(initial_igas)=="")
      {
	error("Missing initial input.");
	return;
      }

    p.SetSysId("gas_in.xml");

    if(iter_counter[(int)id_] == 0) {
      p.Load(initial_igas, strlen(initial_igas));
    } else {
      feedbck_igas = executive_->GetImportData(id_, 1); //port 1 will be the feedback input port;
      if (std::string(feedbck_igas) == "") {
	error("Missing feedback input.");
	iter_counter[(int)id_] = 0;
	return;
      }
      p.Load(feedbck_igas, strlen(feedbck_igas));
    }

    summary_values summaries;

    Gas *gas_in = new Gas;

    gashelper.IntToGas(&(p.intfs[0]), *gas_in);
         
    std::map<std::string, double>::iterator mapiter;

    bool done = false;
    
    std::string notconv;
    double sp;
    double pchg;
    std::string msg;

    char s[150];
    
    for(int i=0; i<(int)sel_species.size(); i++) 
      {
	if(sel_species[i] == "Temperature")            
	  sp = gas_in->gas_composite.T;
	else if(sel_species[i] == "Pressure")
	  sp = gas_in->gas_composite.P;
	else if(sel_species[i] == "Flowrate")
	  sp = gas_in->gas_composite.M;
	else if(sel_species[i] == "Particle Flowrate")
	  sp = gas_in->gas_composite.M_particle;
	else 
	  {
	    sp = gas_in->gas_composite.getFrac(sel_species[i]);
	    if(sp < 0)
	      { 
		warning(sel_species[i]+ " not in stream.");
	      }
	  }
	
	mapiter = last_values[(int)id_].find(sel_species[i]);
	
	if(mapiter != last_values[(int)id_].end()) 
	  {
	    if(sp == 0) 
	      sp = 1e-08;
	    
	    pchg = fabs(mapiter->second - sp) / sp;

	    sprintf(s, "%s  UNITS:% FORMAT:10.2f", sel_species[i].c_str());
	    summaries.insert_summary_val(s, pchg);
	    
	    if(pchg <= atof(max_error[i].c_str())) 
	      done = true;
	    else 
	      {
		notconv += sel_species[i] + " ";
		done = false;
	      }
      
	  } 
	else {
	    sprintf(s, "%s  UNITS:% FORMAT:10.2f", sel_species[i].c_str());
	    summaries.insert_summary_val(s, 100);
	}
	
	last_values[(int)id_][sel_species[i]] = sp;
      }

    if(++iter_counter[(int)id_] >= iterations) 
      {
	warning("Max iterations reached, items not converged: " + notconv);
	done = true;
      }

    if(done) {
      last_values[(int)id_].clear();
      iter_counter[(int)id_] = 0;
      return_state = 3;
    } else
      return_state = 0;

    p.intfs.resize(1); //each port has its own package
    p.SetPackName("ExportData");
    p.SetSysId("test.xml");
    gashelper.GasToInt(gas_in, p.intfs[0]);
    p.intfs[0].setInt("RETURN_STATE", return_state);

    result = p.Save(rv); 

    executive_->SetExportData(id_, 0, result);
   
    p.intfs.clear();
    p.intfs.resize(1);
    gashelper.SumToInt(&summaries, p.intfs[0]);
    result = p.Save(rv);
    
    executive_->SetModuleResult(id_, result);  

    if(gas_in) delete gas_in;
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
    std::cout<<UnitName_<<" :GetStatusMessage called"<<endl;
    
    const char *status;
    bool rv;
    Package p;
    p.SetPackName("Status");
    p.SetSysId("status.xml");
    p.intfs.resize(1);
    p.intfs[0].setInt("RETURN_STATE", return_state);
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
    if (string(param)=="") 
		return;
    
    std::cout<<UnitName_<<" :SetParams called"<<endl;
    
    Package p;
    p.SetSysId("UI.xml");
    p.Load(param, strlen(param));
    
    //Now make use of p.intfs to get your GUI vars out
    iterations = p.intfs[0].getInt("iterations");

    species = p.intfs[0].getString1D("species");
    sel_species = p.intfs[0].getString1D("sel_species");
    max_error = p.intfs[0].getString1D("max_error");
   
    cout << param << endl;
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
    std::cout<<UnitName_<<" :SetName called"<<endl;

    UnitName_ = std::string(name);
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
  msg = "GasFeedback: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
  p.intfs.clear();
  result = p.Save(rv);
  return_state = 1;
  executive_->SetModuleResult(id_, result); //this marks the end the execution
}

void Body_Unit_i::warning (std::string msg)
{
  msg = "GasFeedback: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
}
