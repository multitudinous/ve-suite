#include "V21Helper.h"
#include <ThirdParty/Components/cat_comb.h>
#include "CatalyticCombustorUnit_i.h"


using namespace Vision21;
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
	error("Catalytic Combustor: Missing input input.");
	return;
      }

    p.SetSysId("gas_in.xml");
        
    Gas *gas_in = new Gas();
    V21Helper gashelper(therm_path.c_str());
    
    p.Load(igas, strlen(igas)); 
    gashelper.IntToGas(&(p.intfs[0]), *gas_in);
    
    Gas *gas_out = new Gas;
    gas_out->copy(*gas_in);
    
    pair<vector<int>, vector<string> > exit_condition;
    
    // WHAT ?
    string surf_mech_name = "Chou_Surf_Chem_ch4.dat"; 
    
    cout << "MKD3" << endl;
    cat_comb cat(surf_mech_name);
  
    if(cat.fError) {
      error("Catalytic Combustor: Error Initializing Surface Mechanism");
      return;
    }
    
    // Initialize surface chem gas compositions
    
    cat.setSurfChemGasComp(*gas_in);             // bulk gas  
    
    cat.surf.molf     = cat.surf.bulkMolf;       // surface gas  bulkMolf is size nGasSp, molf is nGasSp + nSurfSp
    cat.surf.bulkTemp = gas_in->gas_composite.T; // K
    cat.surf.surfTemp = cat.surf.bulkTemp;       // K
    cat.surf.pres     = gas_in->gas_composite.P; // Pa
    
    // Initialize surface species
    
    for(i=cat.surf.nGasSp; i<(int)cat.surf.spNames.size(); i++)
      cat.surf.molf.push_back(1.0);         
    cat.surf.molf[cat.surf.speciesMap.find("PT(S)")->second] = 1000.;
    cat.surf.normalize(cat.surf.molf);
    
    // Initialize channel properties (monolith rxr)
    
    if(!case_type) { cat.mode = "eval"; }
    else                 { cat.mode = "design"; }
    
    cat.length            = length;      // m
    cat.desiredConversion = conversion;
    cat.surf.eff_factor   = effect;      // effectiveness factor
    cat.surf.Dh           = hydDiameter; // m, hydraulic diameter						
    cat.surf.siteDensity  = site_den;    // moles/m2
    cat.inlet_velocity    = velocity;    // m/s
    
    // overrides gas flow for now; later make dep on geom
    
    //-----------------------------------------------------------------------------
    // SOLVER
    //-----------------------------------------------------------------------------
    
    
    exit_condition = cat.calculate(*gas_in, *gas_out);
    
    if(exit_condition.first.size() != exit_condition.second.size()) {
      error("Catalytic Combustor: Error following calculate");
      return;
    }
    if(!exit_condition.first.size() == 0) {
      string s1;
      int err_count = 0;
      for(i=0; i <(int)exit_condition.first.size(); i++)
	if (exit_condition.first[i] != 1) {
	  err_count++;
	  error("Catalytic Combustor:" + exit_condition.second[i]);
	} else {
	  warning("Catalytic Combustor:" + exit_condition.second[i]); 
	}
      if(err_count) 
	return;
    }

    summaries.clear();
    
    summaries.insert_summary_val("Pressure Drop UNITS:Pa FORMAT:10.2f", cat.pressure_drop);
    //summaries.insert_summary_val("Ignition Temperature UNITS:K FORMAT:10.2f", ig_temp);
    summaries.insert_summary_val("Res. Time UNITS:sec FORMAT:11.5e", cat.tauAvg);
    //summaries.insert_summary_val("Monolith Footprint UNITS:m^2 FORMAT:10.2f", footprint);
    
    if(!case_type) {
      summaries.insert_summary_val("Calculated Conversion UNITS:% FORMAT:10.2f", cat.conversion);
    } else {
      summaries.insert_summary_val("Calculated Length UNITS:m FORMAT:11.5e", cat.length);
    }

    p.intfs.resize(1); //each port has its own package
    p.SetPackName("ExportData");
    p.SetSysId("test.xml");
    
    gashelper.GasToInt(gas_out, p.intfs[0]);
    ogas = p.Save(rv);
    executive_->SetExportData(id_, 0, ogas);
    
    p.intfs.resize(1);
    gashelper.SumToInt(&summaries, p.intfs[0]);
    result = p.Save(rv); 

    executive_->SetModuleResult(id_, result); //marks the end the execution
    delete gas_out;
    delete gas_in;
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
    effect = p.intfs[0].getDouble("effect");
    site_den = p.intfs[0].getDouble("site_den");
    hydDiameter = p.intfs[0].getDouble("hydDiameter");
    length = p.intfs[0].getDouble("length");
    conversion = p.intfs[0].getDouble("conversion");
    velocity = p.intfs[0].getDouble("velocity");
    case_type = p.intfs[0].getInt("case_type");
    
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
