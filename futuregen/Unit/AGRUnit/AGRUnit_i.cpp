#include "V21Helper.h"
#include "AGRUnit_i.h"

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
    const char* result;
    summary_values summaries;

    fflush(NULL);

    igas = executive_->GetImportData(id_, 0); //port 0 will be the gas input port;

    if (string(igas)=="")
      {
	error("AGR: Missing input input.");
	return;
      }

    p.SetSysId("gas_in.xml");
    p.Load(igas, strlen(igas)); 

    Gas *gas_in = new Gas;

    V21Helper gashelper(therm_path.c_str());
    gashelper.IntToGas(&(p.intfs[0]), *gas_in);

    // Actual gas flow rate, MMCFD
    double MM = gas_in->gas_composite.M / gas_in->gas_composite.density() 
      * 60 * 60 * 24 * 3.2808 * 3.2808 * 3.2808 / 1.0e6;
    
    // Mole Loading
    double ML =  (gas_in->gas_composite.moles("H2S") + gas_in->gas_composite.moles("CO2"))
      / gas_in->gas_composite.moles();
    
    // Amine solution weight % circulated
    double WT = 50.0;
    
    // Amine circulation rate
    double GPM = 0.206 * MM * (gas_in->gas_composite.getFrac("H2S") + gas_in->gas_composite.getFrac("CO2")) 
      * solv_mw * 100.0 / (ML * WT);
    
    // Tray type factor
    double TTF = 0.0;
    if(tray_type == 0)     TTF = 0.25;
    else if(tray_type ==1) TTF = 0.20;
    else                   TTF = 0.0; /* TEMI - sieve tray */
    
    // Density of syngas at tray conditions
    double DEN_SYN = gas_in->gas_composite.density()*2.20462 * 0.028316; 
    
    // Bubbling area
    double BA = gas_in->gas_composite.M / gas_in->gas_composite.density() * 35.31 / 
      (TTF * sqrt((solv_den * 2.20462 * 0.028316 - DEN_SYN ) / sqrt(DEN_SYN)));
    
    // Area of downcomer
    // Assume downflow velocity of 0.25 ft/sec
    double AREA_DOWN = GPM / (60 * 7.48 * 0.25);
    
    // Total required tray area
    double AREA_TRAY = (BA + AREA_DOWN + AREA_DOWN) * 1.15;
    
    // Tray diameter
    double DIAM_TRAY = sqrt(4 * AREA_TRAY / 3.14159);
    
    // Clear liquid residence time
    double CLRT = BA * (3.1 / 12) * 0.3 / (GPM / (7.48 * 60));
    
    // Liquid molar flowrate / unit area
    // i.e. solvent entering at top of tower
    double LM = solv_den * GPM * 0.3048 * 0.3048 * 0.3048 
      / (7.48 * 60 * solv_mw * AREA_TRAY * 0.09290304);
    
    // Gas molar flow rate / unit area
    // i.e. syngas entering at bottom of tower
    double GM = gas_in->gas_composite.M / (gas_in->gas_composite.mw() * AREA_TRAY * 0.09290304);
    
    // Slope of equilibrium curve
    // for 50% MDEA at 40degC, the slope of the equilibrium curve: m = 0.0304;
    double m = 0.0304; /* switch this ? */
    
    // Absorption factor
    double A = LM / (m * GM);
    
    // Theoretical number of trays
    double T_NTRAYS = log((1-1/A)*gas_in->gas_composite.getFrac("H2S") /
			  1.0e-6 + 1/A) / log(A);
    
    // Actual number of trays
    int A_NTRAYS = ceil(T_NTRAYS);
    
    // Absorber height
    double ABS_HEIGHT = A_NTRAYS * 2;
    
    //
    // Fill in the gas out data structure and send it on it's way...
    //

    Gas *gas_out = new Gas; 
    
    gas_out->copy(*gas_in);
        
    gas_out->gas_composite.normalize_specie();
    double mole_CO2_s = gas_out->gas_composite.moles("CO2"), change_by = 0.0;
    if(mole_CO2_s>0.0){
       change_by = -0.5*mole_CO2_s;
       gas_out->gas_composite.moles(change_by,"CO2");
    }
    double mole_H2S_s = gas_out->gas_composite.moles("H2S");
    if(mole_H2S_s>0.0){
       if(mole_H2S_s>gas_out->gas_composite.moles()*1.0e-6){
          change_by = gas_out->gas_composite.moles()*1.0e-6 - mole_H2S_s;
          gas_out->gas_composite.moles(change_by,"H2S");
       }
    }

    p.intfs.resize(1);
    gashelper.GasToInt(gas_out, p.intfs[0]);

    p.SetPackName("ExportData");
    p.SetSysId("test.xml");
    ogas = p.Save(rv);
    executive_->SetExportData(id_, 0, ogas);

    //
    // Fill in summary table
    //

    summaries.clear();
    
    summaries.insert_summary_val("Amine Circulation Rate UNITS:GPM FORMAT:10.2f", GPM);
    double H2S_out = gas_out->gas_composite.getFrac("H2S");
    if(H2S_out<0.0) H2S_out = 0.0;
    summaries.insert_summary_val("H2S UNITS:ppm FORMAT:10.2f", H2S_out*1.0e6);
    double CO2_out = gas_out->gas_composite.getFrac("CO2");
    if(CO2_out<0.0) CO2_out = 0.0;
    summaries.insert_summary_val("CO2 UNITS:mol_frac FORMAT:10.2f", CO2_out);
    summaries.insert_summary_val("Area of Downcomer UNITS:ft^2 FORMAT:10.2e", AREA_DOWN);
    summaries.insert_summary_val("Tray Diameter UNITS:ft FORMAT:10.2e", DIAM_TRAY);
    summaries.insert_summary_val("Number of Trays FORMAT:10.0f", A_NTRAYS);
    summaries.insert_summary_val("Absorber Height UNITS:ft FORMAT:10.2e", ABS_HEIGHT);
    
    p.intfs.resize(1);
    
    gashelper.SumToInt(&summaries, p.intfs[0]);
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
    tray_type = p.intfs[0].getInt("tray_type");
    solv_type = p.intfs[0].getInt("solv_type");
    solv_mw = p.intfs[0].getDouble("solv_mw");
    solv_den = p.intfs[0].getDouble("solv_den");
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

