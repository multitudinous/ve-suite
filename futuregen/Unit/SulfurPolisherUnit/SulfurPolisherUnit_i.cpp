#include "V21Helper.h"
#include <ThirdParty/Components/Sulfur_Polisher.h>
#include "SulfurPolisherUnit_i.h"

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
	error("Missing input input.");
	return;
      }

    p.SetSysId("gas_in.xml");
        
    Gas *gas_in_data = new Gas();
    V21Helper gashelper(therm_path.c_str());

    p.Load(igas, strlen(igas)); 
    gashelper.IntToGas(&(p.intfs[0]), *gas_in_data);


  // Check incoming
    if(gas_in_data->gas_composite.T <= 500 || gas_in_data->gas_composite.T >= 1100) {
      warning("Incoming gas temperature out of range.");
    }
    
    if(gas_in_data->gas_composite.getFrac("H2S")<=0) {
      warning("No H2S in gas stream.\n");
      //ogas->send(GasHandle(gas_in_data));
      //return;
    }
    if(gas_in_data->gas_composite.getFrac("COS")<=0) {
      warning("No COS in gas stream.\n");
      //ogas->send(GasHandle(gas_in_data));
      //return;
    }
    
    Gas *gas_out_data = new Gas;
    
    Sulfur_Polisher Polisher;
    
    Polisher.Toperating = Temp;
    
    if (H2Sradio == 1) {
      Polisher.H2Sflag = 0;
      Polisher.H2Seff = effentry;
    } else {
      Polisher.H2Sflag = 1;
      Polisher.H2Sppm = ppmentry;
    }
    
    if (COSradio == 1) {
      Polisher.COSflag = 0;
      Polisher.COSeff = COSeffi;
    } else {
      Polisher.COSflag = 1;
      Polisher.COSppm = COSppm;
    }
    
    if (pdrop==1) {
      Polisher.Pdrop  =pdropentry;
    } else {
      Polisher.calcDp = pdrop;
      Polisher.Bed_diam = bedDiameter;
      Polisher.Bed_L   = bedDepth;
      Polisher.void_frac =bedFrac ;
      Polisher.sphericity = sphericity;
      Polisher .particle_size = particleSize;
    }
    
    if (!Polisher.calculate(*gas_in_data, *gas_out_data)) {
      error("Error in model calculate.\n");
      return;
    }
    
    summaries.insert_summary_val("Temperature Out UNITS:K FORMAT:10.2f",
				 gas_out_data->gas_composite.T);
    summaries.insert_summary_val("Pressure Drop UNITS:psi FORMAT:10.2f", 
				 (gas_in_data->gas_composite.P - gas_out_data->gas_composite.P)
				 / 6894.757);
    summaries.insert_summary_val("H2S Reduction Efficiency UNITS:% FORMAT:10.2f",
				 (gas_in_data->gas_composite.moles("H2S") -
				  gas_out_data->gas_composite.moles("H2S")) /
				 gas_in_data->gas_composite.moles("H2S") * 100);
    summaries.insert_summary_val("COS Reduction Efficiency UNITS:% FORMAT:10.2f",
				 (gas_in_data->gas_composite.moles("COS") -
				  gas_out_data->gas_composite.moles("COS")) /
				 gas_in_data->gas_composite.moles("COS") * 100);
    
    p.intfs.resize(1); //each port has its own package
    p.SetPackName("ExportData");
    p.SetSysId("test.xml");
    
    gashelper.GasToInt(gas_out_data, p.intfs[0]);
    ogas = p.Save(rv);
    executive_->SetExportData(id_, 0, ogas);
    
    if(gas_out_data->gas_composite.T <= 500 || gas_out_data->gas_composite.T >= 1100) {
      warning("Outgoing gas temperature out of range");
    }
    
    p.intfs.resize(1);
    gashelper.SumToInt(&summaries, p.intfs[0]);
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
    if (string(param)=="")
      return;
    std::cout<<UnitName_<<" :SetParams called"<<endl;
    Package p;
        
    p.SetSysId("gui.xml");
    p.Load(param, strlen(param));
    //Now make use of p.intfs to get your GUI vars out
    Temp = p.intfs[0].getDouble("Temp_change");
    effentry = p.intfs[0].getDouble("H2S_eff");
    ppmentry = p.intfs[0].getDouble("H2S_ppm");
    pdropentry = p.intfs[0].getDouble("Pres_drop");
    bedDiameter = p.intfs[0].getDouble("Bed_diameter");
    bedDepth = p.intfs[0].getDouble("Bed_depth");
    bedFrac = p.intfs[0].getDouble("Bed_void_frac");
    particleSize = p.intfs[0].getDouble("Particle_size");
    sphericity = p.intfs[0].getDouble("Particle_sphericity");
    COSeffi = p.intfs[0].getDouble("COS_eff");
    COSppm = p.intfs[0].getDouble("COS_ppm");
    H2Sradio = p.intfs[0].getInt("rH2S_eff_ppm");
    pdrop = p.intfs[0].getInt("PresDrop_spec_calc");
    COSradio = p.intfs[0].getInt("rCOS_eff_ppm");
    
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
