#include "Unit_i.h"
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
    const char* outport;
    const char* result;
    bool rv;
    Package p;
    
   
    //Your code of using the import data in the p.intfs
    Gas *gas_data = new Gas;
    
    string tmp_str;
    
    // Zero things in gas, not in GUI
    gas_data->gas_composite.area = 0;
    gas_data->gas_composite.eff = 0;
    gas_data->gas_composite.eta = 0;
    gas_data->gas_composite.chi = 0;
    gas_data->gas_composite.mean_size = 0;
    gas_data->gas_composite.size_variance = 0;
    gas_data->gas_composite.T_particle = 0;
    gas_data->gas_composite.M_particle = 0;
    gas_data->pressure_drop = 0;
    
    // Set things in GUI
    gas_data->gas_composite.T = temp;
    gas_data->gas_composite.P = pres;
    gas_data->gas_composite.M = flow;
    // Particle data
    gas_data->gas_composite.M_particle = p_m;
    gas_data->CoalCal = coalcal;
    gas_data->AshCal = ashcal;
    gas_data->AshpH = ashph;
    gas_data->gas_composite.mean_size = mps;
    gas_data->gas_composite.size_variance = sv;
    gas_data->gas_composite.T_particle = p_temp;
    
    string therm_path="thermo";

    thermo *thrmo = new thermo(therm_path);
    const std::map<std::string, int>& name_map = thrmo->get_nam_spec();
    vector<string> species; // available species, from thermo
    species.clear();
    map<std::string, int>::const_iterator iter;

    for(iter=name_map.begin(); iter!=name_map.end(); iter++)
      species.push_back(iter->first);
  
    if(spec_frac.size()==0)
      for (unsigned int i=0; i<species.size(); i++) 
	{
	  gas_data->gas_composite.comp_specie.push_back(0.0);
	  gas_data->specie[species[i]] = i;
	}
    else
      for (unsigned int i=0; i<spec_frac.size(); i++) 
	{
	  gas_data->gas_composite.comp_specie.push_back(atof(spec_frac[i].c_str()));
	  gas_data->specie[comp[i].c_str()] = i;
	}
    
    vector<string> particles; // available particles
    // available particles
    particles.clear();
    particles.push_back("ASH");
    particles.push_back("CHAR");
    particles.push_back("COAL");
    particles.push_back("WATER");
    
    if(p_frac.size()==0)
      for (unsigned int i=0; i<particles.size(); i++) 
	{
	  gas_data->gas_composite.comp_particle.push_back(0.0);
	  gas_data->particle[particles[i]] = i;
	}
    else
      for (unsigned int i=0; i<p_frac.size(); i++) 
	{
	  gas_data->gas_composite.comp_particle.push_back(atof(p_frac[i].c_str()));
	  gas_data->particle[p_comp[i].c_str()] = i;
	}
    V21Helper gashelper(therm_path.c_str());
    p.intfs.resize(1);
    gashelper.GasToInt(gas_data, p.intfs[0]);
    delete gas_data;

    //set p.intf to be something containing the export data
    p.SetPackName("ExportData");
    p.SetSysId("test.xml");
    outport = p.Save(rv);
    //set p.intf to be someting containing the results
    
    //    result = p.Save(rv);
    executive_->SetExportData(id_, 0, outport);
    //executive_->SetExportData(id_, 0, result);
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
        
    p.SetSysId("temp.xml");
    p.Load(param, strlen(param));
    //Now make use of p.intfs to get your GUI vars out
    temp = p.intfs[0].getDouble("temp");
    pres = p.intfs[0].getDouble("pres");
    flow = p.intfs[0].getDouble("flow");
    p_temp = p.intfs[0].getDouble("p_temp");
    p_m = p.intfs[0].getDouble("p_m");
    mps = p.intfs[0].getDouble("mps");
    sv = p.intfs[0].getDouble("sv");
    coalcal = p.intfs[0].getDouble("coalcal");
    ashcal = p.intfs[0].getDouble("ashcal");
    ashph = p.intfs[0].getDouble("ashph");
    comp = p.intfs[0].getString1D("comp");
    spec_frac = p.intfs[0].getString1D("spec_frac");
    p_comp = p.intfs[0].getString1D("p_comp");
    p_frac = p.intfs[0].getString1D("p_frac");
   
    
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
