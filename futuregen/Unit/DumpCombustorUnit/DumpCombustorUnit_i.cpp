#include "V21Helper.h"
#include <ThirdParty/Components/dump_combustor.h>
#include "DumpCombustorUnit_i.h"


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

    if (!igas)
      {
	error("Missing input input.");
	return;
      }

    p.SetSysId("gas_in.xml");
        
    Gas *gasin = new Gas();
    V21Helper gashelper(therm_path.c_str());
    
    p.Load(igas, strlen(igas)); 
    gashelper.IntToGas(&(p.intfs[0]), *gasin);
    // Check incoming
    if(gasin->gas_composite.T <= 750 || gasin->gas_composite.T >= 1600) {
      warning("Incoming Gas temperature out of range.");
    }

    summaries.clear();

    dump_combustor dumpComb;
  
    Gas *gasw = new Gas;
    pair<vector<int>, vector<string> > exit_condition;

    int ii = 0;

    gasw->copy(*gasin);

    dumpComb.caseType          = case_type;  // 0 evaluation; 1 design
    dumpComb.volume            = volume;     // m3/s
    dumpComb.desiredConversion = conversion; // caseType = 2       
    dumpComb.fHeatLoss         = fracQloss;  // fraction of inlet sensible energy relative to 298 K
    dumpComb.deltaP            = press_drop; // psi  (should be positive)
  
    exit_condition = dumpComb.calculate(*gasin, *gasw);
    
    if(exit_condition.first.size() != 0) {
      unsigned int i;
      string s1;
      for(i=0; i < exit_condition.first.size(); i++) 
	if(exit_condition.first[i] == 1)
	  warning(exit_condition.second[i]);
	else {
	  error(exit_condition.second[i]);
	}
    }

    summaries.insert_summary_val("HeatLoss UNITS:MW FORMAT:10.2f", dumpComb.heatLoss/1000);
    summaries.insert_summary_val("Pressure Drop UNITS:psi FORMAT:10.2f", dumpComb.deltaP);
    summaries.insert_summary_val("Tau Average UNITS:sec FORMAT:10.2f", dumpComb.tauAvg);
    
    if(!case_type) {
      summaries.insert_summary_val("Calculated Conversion UNITS:frac FORMAT:10.2f",
				   dumpComb.conversion); 
    } else {
      summaries.insert_summary_val("Calculated Volume UNITS:m^3 FORMAT:10.2f",
				   dumpComb.volume); 
    }
        
    p.intfs.resize(1); //each port has its own package
    p.SetPackName("ExportData");
    p.SetSysId("test.xml");
    
    gashelper.GasToInt(gasw, p.intfs[0]);
    ogas = p.Save(rv);
    executive_->SetExportData(id_, 0, ogas);
    
    p.intfs.resize(1);
    gashelper.SumToInt(&summaries, p.intfs[0]);
    result = p.Save(rv); 
    std::cout<<"cp5\n";
    executive_->SetModuleResult(id_, result); //marks the end the execution
    delete gasw;
    delete gasin;

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
        
    p.SetSysId("gui.xml");
    p.Load(param, strlen(param));
    //Now make use of p.intfs to get your GUI vars out
    case_type = p.intfs[0].getDouble("case_type");
    conversion = p.intfs[0].getDouble("conversion");
    volume = p.intfs[0].getDouble("volume");
    fracQloss = p.intfs[0].getDouble("fracQloss");
    press_drop = p.intfs[0].getDouble("press_drop");
    
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
