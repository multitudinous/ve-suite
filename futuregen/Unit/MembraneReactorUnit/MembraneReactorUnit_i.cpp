#include "V21Helper.h"
#include <ThirdParty/Components/memb_rxr.h>
#include "MembraneReactorUnit_i.h"

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
    const char* isweep;
    const char* ogas;
    const char* osweep;
    bool rv;
    Package p;
    string therm_path="thermo";
    int i;
    const char* result;
    summary_values summaries;

    igas = executive_->GetImportData(id_, 0); //port 0 will be the gas input port;
    isweep = executive_->GetImportData(id_, 1); //port 1 will be the sweep input port;
    
    if (string(igas)=="" || string(isweep)=="")
      {
	error("Missing input input.");
	return;
      }

    p.SetSysId("gas_in.xml");
        
    Gas *gasin = new Gas();
    Gas *sweepin = new Gas();
    V21Helper gashelper(therm_path.c_str());
    
    p.Load(igas, strlen(igas)); 
    gashelper.IntToGas(&(p.intfs[0]), *gasin);
    gasin->gas_composite.normalize_specie();
    p.Load(isweep, strlen(isweep)); 
    gashelper.IntToGas(&(p.intfs[0]), *sweepin);
    sweepin->gas_composite.normalize_specie();

    // Check incoming
    if(gasin->gas_composite.T <= 500 || gasin->gas_composite.T >= 900) {
      warning("Incoming gas temperature out of range.");
    }
    
    summaries.clear();
    
    std::pair<std::vector<int>, std::vector<string> > exit_condition;
    
    Gas *gasw = new Gas;
    Gas *sweepout = new Gas;
    gasw->copy(*gasin);
    sweepout->copy(*sweepin);
    sweepout->addSpecie("H2");
 
    //double d1 = 0.0; // dummy variable
    //int ii = 0;
    
    memb_rxr memb_shft_rxr;
    memb_shft_rxr.memb_diameter  = memb_diameter;
    memb_shft_rxr.Pd_thickness   = Pd_thickness;
    memb_shft_rxr.L_rxr          = L_rxr;
    memb_shft_rxr. fheat_loss    = 0.0;
    memb_shft_rxr.case_type      = case_type;
    memb_shft_rxr.CO_conv_want   = CO_conv_want;
    memb_shft_rxr.shell_diameter = shell_diameter;
    memb_shft_rxr.f_pre_mr       = f_pre_mr;
    memb_shft_rxr.mr_inlet_temp  = mr_inlet_temp;
    memb_shft_rxr.f_H2O_CO       = f_H2O_CO;
    memb_shft_rxr.H2O_CO         = H2O_CO;
    memb_shft_rxr.n_modules      = n_modules;

    //memb_shft_rxr.n_modules = gasin->gas_composite.moles() 
    //  * 8.314 * gasin->gas_composite.T / gasin->gas_composite.P /
    //  (3.14159/4.0 * (memb_shft_rxr.shell_diameter*memb_shft_rxr.shell_diameter - 
    //	    memb_shft_rxr.memb_diameter*memb_shft_rxr.memb_diameter)) 
    // / 20 / memb_shft_rxr.memb_diameter / 0.969 * 2.0;
    
    exit_condition = memb_shft_rxr.calculate(*gasin, *gasw, *sweepin, *sweepout);
  
    if(exit_condition.first.size() != exit_condition.second.size()) {
      error("ERROR EXIT CONDITION PAIR SIZES NOT THE SAME");
      return;
    }
    if(exit_condition.first.size() != 0) {
      unsigned int i;
      string s1;
      for(i=0; i < exit_condition.first.size(); i++) 
	if(exit_condition.first[i] == 1)
	  warning(exit_condition.second[i]);
	else {
	  error(exit_condition.second[i]);
	  return;
	}
    }
    
    summaries.insert_summary_val("Footprint UNITS:m2 FORMAT:10.2f",
				 memb_shft_rxr.footprint);
    summaries.insert_summary_val("CO Conversion UNITS:frac FORMAT:10.2f",
				 memb_shft_rxr.CO_conv);
    summaries.insert_summary_val("H2 Recovery UNITS:frac FORMAT:10.2f",
				 memb_shft_rxr.H2_recovery);
    summaries.insert_summary_val("Sweep H2 Concentration UNITS:frac FORMAT:10.2f",
				 gasw->gas_composite.getFrac("H2"));
    summaries.insert_summary_val("Exit Gas Temp UNITS:K FORMAT:10.2f",
				 gasw->gas_composite.T);
    summaries.insert_summary_val("Catalyst Space Velocity UNITS:frac FORMAT:10.2f",
				 memb_shft_rxr.space_velocity);
    summaries.insert_summary_val("Pre-Shift Heat Duty UNITS:MW FORMAT:10.2f",
				 memb_shft_rxr.pre_mr_heatex/1000);
    if(case_type) {
      summaries.insert_summary_val("Sweep Flow UNITS:kg/sec FORMAT:10.2f",
				   sweepin->gas_composite.M);
      summaries.insert_summary_val("Length UNITS:m FORMAT:10.2f",
				   memb_shft_rxr.L_rxr);
      summaries.insert_summary_val("Catalyst Space Velocity UNITS:(1/h) FORMAT:10.2f",
				   memb_shft_rxr.space_velocity);
    }
    
    if(f_pre_mr) {
      summaries.insert_summary_val("Pre- MR CO Conversion UNITS:frac FORMAT:10.2f",
				   memb_shft_rxr.pre_mr_conv);
    }
    
    if(f_H2O_CO) {
      summaries.insert_summary_val("Steam Required UNITS:kg/sec FORMAT:10.4f",
				   memb_shft_rxr.steam_injection_flow);
    }
    
    p.intfs.resize(1); //each port has its own package
    p.SetPackName("ExportData");
    p.SetSysId("test.xml");
    
    gashelper.GasToInt(gasw, p.intfs[0]);
    ogas = p.Save(rv);
    executive_->SetExportData(id_, 0, ogas);
    gashelper.GasToInt(sweepout, p.intfs[0]);
    osweep = p.Save(rv);
    executive_->SetExportData(id_, 1, osweep);

    // Check incoming
    if(gasw->gas_composite.T <= 500 || gasw->gas_composite.T >= 900) {
      warning("Outgoing Gas temperature out of range.");
    }

    p.intfs.resize(1);
    gashelper.SumToInt(&summaries, p.intfs[0]);
    result = p.Save(rv); 
    std::cout<<"cp5\n";
    executive_->SetModuleResult(id_, result); //marks the end the execution

    
    delete gasw;
    delete sweepout;
    delete gasin;
    delete sweepin;

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
    if (string(param)=="")
		return;
      
    std::cout<<UnitName_<<" :SetParams called"<<endl;
    Package p;
        
    p.SetSysId("gui.xml");
    p.Load(param, strlen(param));
    //Now make use of p.intfs to get your GUI vars out
    case_type = p.intfs[0].getInt("case_type");
    n_modules = p.intfs[0].getInt("n_modules");
    f_pre_mr = p.intfs[0].getInt("f_pre_mr");
    f_H2O_CO = p.intfs[0].getInt("f_H2O_CO");
    memb_diameter = p.intfs[0].getDouble("memb_diameter");
    Pd_thickness = p.intfs[0].getDouble("Pd_thickness");
    L_rxr = p.intfs[0].getDouble("L_rxr");
    CO_conv_want = p.intfs[0].getDouble("CO_conv_want");
    shell_diameter = p.intfs[0].getDouble("shell_diameter");
    mr_inlet_temp = p.intfs[0].getDouble("mr_inlet_temp");
    H2O_CO = p.intfs[0].getDouble("H2O_CO");
    
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
