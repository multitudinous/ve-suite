#include "V21Helper.h"
#include "GasMixerUnit_i.h"

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
    const char* igas[4];
    const char* ogas;
    bool rv;
    Package p;
    string therm_path="thermo";
    int i, count;
    const char* result;

    Gas *gas_in[4];

    count = 0;
    p.SetSysId("gas_in.xml");
    V21Helper gashelper(therm_path.c_str());
    for (i=0; i<4; i++)
      {
	igas[i] = executive_->GetImportData(id_, i); //port i will be the gas input port i;
	if (string(igas[i])!="")
	  {
	    count++;
	    p.Load(igas[i], strlen(igas[i])); 
	    gas_in[i] = new Gas;
	    gashelper.IntToGas(&(p.intfs[0]), *(gas_in[i]));
	  }
	else {
	  gas_in[i] = NULL;
	}
      }

    if (count==0)
      {
	error("Missing input input.");
	return;
      }

    Gas *gas_out = new Gas; 
    gas_out->gas_composite.M = 0;
    gas_out->gas_composite.P = gas_in[0]->gas_composite.P;
    // particle
    gas_out->gas_composite.T_particle = 0.0;
    gas_out->gas_composite.M_particle = 0.0;
    gas_out->gas_composite.mean_size = 0.0;
    gas_out->gas_composite.size_variance = 0.0;
    //# Particle composition
    gas_out->particle.clear();
    gas_out->gas_composite.comp_particle.clear();
    
    double mf, h_out = 0, temp = 0, cp_out = 0;
    std::map<std::string, int >::const_iterator iter, iter2;

    double sumn, sum_M_particle = 0.0;
    std::vector<double> n(count);
    
    for(i=0, sumn=0; i<count; i++) {
      n[i] = gas_in[i]->gas_composite.M / gas_in[i]->gas_composite.mw();
      sumn += n[i];
      
      for(iter=gas_in[i]->specie.begin(); iter!=gas_in[i]->specie.end(); iter++) {
	mf = gas_out->gas_composite.getFrac(iter->first);
	if(mf<0) {
	  gas_out->addSpecie(iter->first);
	  gas_out->gas_composite.setFrac(iter->first,
					 gas_in[i]->gas_composite.getFrac(iter->first)*n[i]);
	}
	else
	  gas_out->gas_composite.setFrac(iter->first,
					 gas_in[i]->gas_composite.getFrac(iter->first)*n[i]+mf);
      }
      
      h_out += gas_in[i]->gas_composite.enthalpy() *
	(gas_in[i]->gas_composite.M / gas_in[i]->gas_composite.mw());
      
      temp += gas_in[i]->gas_composite.T;
      gas_out->gas_composite.M += gas_in[i]->gas_composite.M;
      
      if(gas_in[i]->gas_composite.P < gas_out->gas_composite.P)
	gas_out->gas_composite.P = gas_in[i]->gas_composite.P;
      
      // particle
      gas_out->gas_composite.M_particle += gas_in[i]->gas_composite.M_particle;
      
      sum_M_particle += gas_in[i]->gas_composite.M_particle;
      
      gas_out->gas_composite.T_particle += gas_in[i]->gas_composite.T_particle*gas_in[i]->gas_composite.M_particle;
      
      gas_out->gas_composite.mean_size += gas_in[i]->gas_composite.mean_size*gas_in[i]->gas_composite.M_particle;
      gas_out->gas_composite.size_variance += gas_in[i]->gas_composite.size_variance*gas_in[i]->gas_composite.M_particle;
      for(iter=gas_in[i]->particle.begin(); iter!=gas_in[i]->particle.end(); iter++) {
	mf = gas_out->gas_composite.getPFrac(iter->first);
	
	if(mf<0) {
	  gas_out->addParticle(iter->first, 0.0);
	  mf = 0.0;
	}
	gas_out->gas_composite.setPFrac(iter->first,
					mf + gas_in[i]->gas_composite.getPFrac(iter->first)*gas_in[i]->gas_composite.M_particle);
      }
    }
    
    for(iter=gas_out->specie.begin(); iter!=gas_out->specie.end(); iter++) {
      gas_out->gas_composite.comp_specie[iter->second] /= sumn;
    }
    
    h_out /= sumn;
    
    // Need a THERMO
    thermo *thm;
    thm = new thermo(therm_path);
      
    if(sum_M_particle){
      gas_out->gas_composite.T_particle /= sum_M_particle;
      gas_out->gas_composite.mean_size /= sum_M_particle;
      gas_out->gas_composite.size_variance /= sum_M_particle;
      int num = gas_out->gas_composite.comp_particle.size();
      for(int j=0; j<num; j++) gas_out->gas_composite.comp_particle[j] /= sum_M_particle;
    }
    
    // grab map for species names
    const std::map<std::string, int>& name_map = thm->get_nam_spec();
    
    // initialize composition
    std::vector<double> composition(name_map.size(), 0.0);
    
    for(iter=gas_out->specie.begin(); iter!=gas_out->specie.end(); iter++) {
      iter2 = name_map.find(iter->first);
      if(iter2!=name_map.end())
	composition[iter2->second] = gas_out->gas_composite.comp_specie[iter->second];
      else {
	string msg = "GasMixer: Specie " + iter->first + " not found.";
	error(msg);
	return;
      }
    }
    
    temp /= count;
    
    thm->find_temperature(temp, cp_out, h_out, composition);
    
    gas_out->gas_composite.T = temp;
    
    p.intfs.resize(1); //each port has its own package
    p.SetPackName("ExportData");
    p.SetSysId("test.xml");
    
    gashelper.GasToInt(gas_out, p.intfs[0]);
    ogas = p.Save(rv);
    executive_->SetExportData(id_, 0, ogas);

    for (i=0; i<4; i++)
      if(gas_in[i]) delete gas_in[i];
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
    // not GUI variable
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
