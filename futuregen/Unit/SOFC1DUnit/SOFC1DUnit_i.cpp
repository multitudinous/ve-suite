#include "V21Helper.h"
#include <ThirdParty/Components/FCMcontroller.h>
#include "SOFC1DUnit_i.h"
#include "FC_Process.h"

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
    const char* igas1;
    const char* igas2;
    const char* ogas1;
    const char* ogas2;
    bool rv;
    Package p;
    string therm_path="thermo";
    int i;
    const char* result;
    summary_values summaries;

    igas1 = executive_->GetImportData(id_, 0); //port 0 will be the gas input port;
    igas2 = executive_->GetImportData(id_, 1); //port 1 will be the second gas input port;
    
    if (string(igas1)=="" || string(igas2)=="")
      {
	error("Missing input input.");
	return;
      }

    p.SetSysId("gas_in.xml");
        
    Gas *gas_in_anode = new Gas;
    Gas *gas_in_cathode = new Gas;
    V21Helper gashelper(therm_path.c_str());
    
    p.Load(igas1, strlen(igas1)); 
    gashelper.IntToGas(&(p.intfs[0]), *gas_in_anode);
    
    p.Load(igas2, strlen(igas2)); 
    gashelper.IntToGas(&(p.intfs[0]), *gas_in_cathode);
    
    // Check incoming data

    if(gas_in_anode->gas_composite.T <= 200 || gas_in_anode->gas_composite.T >= 3000) {
      warning("Incoming gas temperature out of range.");
    }
    
    if(gas_in_cathode->gas_composite.T <= 200 || gas_in_cathode->gas_composite.T >= 3000) 
      {
      warning("Incoming gas temperature out of range.");
    }
    
    Vision21::REIInput rei_inp;
    
  // Anode
    
    double a_vel = gas_in_anode->gas_composite.M / gas_in_anode->gas_composite.density()
      / (int)(l_length / (a_width + a_space)) 
      / a_height / a_width / l_numCells;
    
    rei_inp.a_height = a_height;
    rei_inp.a_width  = a_width;
    rei_inp.a_space  = a_space;
    rei_inp.a_ecd    = a_ecd;
    rei_inp.a_tcoeff = a_tcoeff;
    rei_inp.a_thick  = a_thick;
    rei_inp.a_vel    = a_vel;
    rei_inp.a_temp   = gas_in_anode->gas_composite.T;
    rei_inp.a_ipres  = gas_in_anode->gas_composite.P;
    rei_inp.a_opres  = gas_in_anode->gas_composite.P - a_presdrop;
    
    rei_inp.a_specie[H2] = gas_in_anode->gas_composite.getFrac("H2");
    if (rei_inp.a_specie[H2] < 0.0) rei_inp.a_specie[H2] = 0.0;
    
    rei_inp.a_specie[CO] = gas_in_anode->gas_composite.getFrac("CO");
    if (rei_inp.a_specie[CO] < 0.0) rei_inp.a_specie[CO] = 0.0;
    
    rei_inp.a_specie[H2O] = gas_in_anode->gas_composite.getFrac("H2O");
    if (rei_inp.a_specie[H2O] < 0.0) rei_inp.a_specie[H2O] = 0.0;
    
    rei_inp.a_specie[CO2] = gas_in_anode->gas_composite.getFrac("CO2");
    if (rei_inp.a_specie[CO2] < 0.0) rei_inp.a_specie[CO2] = 0.0;
    
    rei_inp.a_specie[O2] = gas_in_anode->gas_composite.getFrac("O2");
    if (rei_inp.a_specie[O2] < 0.0) rei_inp.a_specie[O2] = 0.0;
    
    rei_inp.a_specie[N2] = gas_in_anode->gas_composite.getFrac("N2");
    if (rei_inp.a_specie[N2] < 0.0) rei_inp.a_specie[N2] = 0.0;
    
    rei_inp.a_specie[CH4] = gas_in_anode->gas_composite.getFrac("CH4");
    if (rei_inp.a_specie[CH4] < 0.0) rei_inp.a_specie[CH4] = 0.0;
    
    // Cathode
    
    double c_vel = gas_in_cathode->gas_composite.M / gas_in_cathode->gas_composite.density()
      / (int)(l_width / (c_width + c_space))
      / c_height / c_width / l_numCells ;
    
    rei_inp.c_height  = c_height;
    rei_inp.c_width   = c_width;
    rei_inp.c_space   = c_space;
    rei_inp.c_ecdb    = c_ecdb;
    rei_inp.c_ecdm    = c_ecdm;
    rei_inp.c_tcoeffb = c_tcoeffb;
    rei_inp.c_tcoeffm = c_tcoeffm;
    rei_inp.c_thick   = c_thick;
    rei_inp.c_vel     = c_vel;
    rei_inp.c_temp    = gas_in_cathode->gas_composite.T;
    rei_inp.c_ipres   = gas_in_cathode->gas_composite.P;
    rei_inp.c_opres   = gas_in_cathode->gas_composite.P - c_presdrop;
    
    rei_inp.c_specie[H2] = gas_in_cathode->gas_composite.getFrac("H2");
    if (rei_inp.c_specie[H2] < 0.0) rei_inp.c_specie[H2] = 0.0;
    
    rei_inp.c_specie[CO] = gas_in_cathode->gas_composite.getFrac("CO");
    if (rei_inp.c_specie[CO] < 0.0) rei_inp.c_specie[CO] = 0.0;
    
    rei_inp.c_specie[H2O] = gas_in_cathode->gas_composite.getFrac("H2O");
    if (rei_inp.c_specie[H2O] < 0.0) rei_inp.c_specie[H2O] = 0.0;

    rei_inp.c_specie[CO2] = gas_in_cathode->gas_composite.getFrac("CO2");
    if (rei_inp.c_specie[CO2] < 0.0) rei_inp.c_specie[CO2] = 0.0;
    
    rei_inp.c_specie[O2] = gas_in_cathode->gas_composite.getFrac("O2");
    if (rei_inp.c_specie[O2] < 0.0) rei_inp.c_specie[O2] = 0.0;
    
    rei_inp.c_specie[N2] = gas_in_cathode->gas_composite.getFrac("N2");
    if (rei_inp.c_specie[N2] < 0.0) rei_inp.c_specie[N2] = 0.0;
    
    // Separator
    rei_inp.s_thick   = s_thick;
    rei_inp.s_heatcap = s_heatcap;
    rei_inp.s_density = s_density;
    
    // Electrolyte
    rei_inp.e_thick  = e_thick;
    rei_inp.e_preexp = e_preexp;
    rei_inp.e_exp    = e_exp;
    
    // Electrodes
    rei_inp.f_preexp = f_preexp;
    rei_inp.f_exp    = f_exp;
    rei_inp.a_preexp = a_preexp;
    rei_inp.a_exp    = a_exp;
    
    // Interconect
    rei_inp.i_preexp = i_preexp;
    rei_inp.i_exp    = i_exp;
    
    // Cells
    rei_inp.l_heatcap = l_heatcap;
    rei_inp.l_density = l_density;
    rei_inp.l_length  = l_length;
    rei_inp.l_width   = l_width;
    
    // Operational
    rei_inp.const_cp  = false;
    rei_inp.radiation = false;
    rei_inp.qs_gas    = true;
    rei_inp.qs_solid  = true;
    rei_inp.ax_nodes  = ax_nodes;
    rei_inp.loadres   = loadres;
    
    // RUN MODEL

    CFCMcontroller fcm;
    
    fcm.OnAnalyzedynamics(stop_time, &rei_inp,(work_dir + "/Data.dat").c_str());
    
    // GET OUTPUTS
    
    Gas *gas_out_anode = new Gas;
    Gas *gas_out_cathode = new Gas;
    
    FC_Process fc_p('0'); // Can be 0, 2, or 4
    
    if(!fc_p.parse(work_dir + "/Data.dat")) {
      error("Error in fuel cell.");
      return_state = 1;
      return;
    }
    
    cout << "0 ============================================\n";
    fc_p.dump();
        
    FC_Process fc_p1('1'); // Can be 0, 2, or 4
    if(!fc_p1.parse(work_dir + "/Data.dat")) {
      error("Error in fuel cell.");
      return_state = 1;
      return;
    }
    cout << "1 ============================================\n";
    fc_p1.dump();
    
    FC_Process fc_p2('2'); // Can be 0, 2, or 4
    if(!fc_p2.parse(work_dir + "/Data.dat")) {
      error("Error in fuel cell.");
      return_state = 1;
      return;
    }
    cout << "2 ============================================\n";
    fc_p2.dump();
    
    
    FC_Process fc_p3('3'); // Can be 0, 2, or 4
    if(!fc_p3.parse(work_dir + "/Data.dat")) {
      error("Error in fuel cell.");
    return_state = 1;
    return;
    }
    cout << "3 ============================================\n";
    fc_p3.dump();
    
    FC_Process fc_p4('4'); // Can be 0, 2, or 4
    if(!fc_p4.parse(work_dir + "/Data.dat")) {
      error("Error in fuel cell.");
      return_state = 1;
      return;
    }
    cout << "4 ============================================\n";
    fc_p4.dump();
    
    //////////////////////
    
    gas_out_anode->gas_composite.T = fc_p.anode_T;
    gas_out_anode->gas_composite.P = fc_p.anode_P;
    gas_out_anode->gas_composite.M = gas_in_anode->gas_composite.M;
    
    std::map<std::string, double>::iterator iter;
    for(iter=fc_p.anode_s.begin(); iter!=fc_p.anode_s.end(); iter++)
      gas_out_anode->gas_composite.setFrac(iter->first, iter->second);
    
    gas_out_cathode->gas_composite.T = fc_p.cathode_T;
    gas_out_cathode->gas_composite.P = fc_p.cathode_P;
    gas_out_cathode->gas_composite.M = gas_in_cathode->gas_composite.M;
    
    for(iter=fc_p.cathode_s.begin(); iter!=fc_p.cathode_s.end(); iter++)
      gas_out_cathode->gas_composite.setFrac(iter->first, iter->second);
    
    // SUMMARIES
    
    double pow = fc_p.Produced_Power * l_numCells / 1e6;
    
    summaries.insert_summary_val("Produced Power UNITS:MW FORMAT:12.2f", pow);
    summaries.insert_summary_val("Overall Q UNITS:MW FORMAT:12.2f", 
				 fc_p.Overall_Q * l_numCells / 1e6);
    
    
    p.intfs.resize(1); //each port has its own package
    p.SetPackName("ExportData");
    p.SetSysId("test.xml");
    
    gashelper.GasToInt(gas_out_anode, p.intfs[0]);
    ogas1 = p.Save(rv);
    executive_->SetExportData(id_, 0, ogas1);
    gashelper.GasToInt(gas_out_cathode, p.intfs[0]);
    ogas2 = p.Save(rv);
    executive_->SetExportData(id_, 1, ogas2);


    p.intfs.resize(1);
    gashelper.SumToInt(&summaries, p.intfs[0]);
    result = p.Save(rv); 

    executive_->SetModuleResult(id_, result); //marks the end the execution
    
    if(gas_in_anode)    delete gas_in_anode;
    if(gas_in_cathode)  delete gas_in_cathode;
    if(gas_out_anode)   delete gas_out_anode;
    if(gas_out_cathode) delete gas_out_cathode;
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
    
    work_dir   = p.intfs[0].getString("work_dir");
    a_height   = p.intfs[0].getDouble("a_height");
    a_width    = p.intfs[0].getDouble("a_width");
    a_space    = p.intfs[0].getDouble("a_space");
    a_ecd      = p.intfs[0].getDouble("a_ecd");
    a_tcoeff   = p.intfs[0].getDouble("a_tcoeff");
    a_thick    = p.intfs[0].getDouble("a_thick");
    a_presdrop = p.intfs[0].getDouble("a_presdrop");
    
    c_height   = p.intfs[0].getDouble("c_height");
    c_width    = p.intfs[0].getDouble("c_width");
    c_space    = p.intfs[0].getDouble("c_space");
    c_ecdb     = p.intfs[0].getDouble("c_ecdb");
    c_ecdm     = p.intfs[0].getDouble("c_ecdm");
    c_tcoeffb  = p.intfs[0].getDouble("c_tcoeffb");
    c_tcoeffm  = p.intfs[0].getDouble("c_tcoeffm");
    c_thick    = p.intfs[0].getDouble("c_thick");
    c_presdrop = p.intfs[0].getDouble("c_presdrop");
    
    s_thick    = p.intfs[0].getDouble("s_thick");
    s_heatcap  = p.intfs[0].getDouble("s_heatcap");
    s_density  = p.intfs[0].getDouble("s_density");
    
    e_thick    = p.intfs[0].getDouble("e_thick");
    e_preexp   = p.intfs[0].getDouble("e_preexp");
    e_exp      = p.intfs[0].getDouble("e_exp");
    
    f_preexp   = p.intfs[0].getDouble("f_preexp");
    f_exp      = p.intfs[0].getDouble("f_exp");
    a_preexp   = p.intfs[0].getDouble("a_preexp");
    a_exp      = p.intfs[0].getDouble("a_exp");
    
    i_preexp   = p.intfs[0].getDouble("i_preexp");
    i_exp      = p.intfs[0].getDouble("i_exp");
    
    l_numCells = p.intfs[0].getInt("l_numCells");
    l_heatcap  = p.intfs[0].getDouble("l_heatcap");
    l_density  = p.intfs[0].getDouble("l_density");
    l_length   = p.intfs[0].getDouble("l_length");
    l_width    = p.intfs[0].getDouble("l_width");
    
    stop_time  = p.intfs[0].getDouble("stop_time");
    ax_nodes   = p.intfs[0].getInt("ax_nodes");
    loadres    = p.intfs[0].getDouble("loadres");

    
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
  msg = "SOFC1D: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
  p.intfs.clear();
  result = p.Save(rv);
  return_state = 1;
  executive_->SetModuleResult(id_, result); //this marks the end the execution
}

void Body_Unit_i::warning (std::string msg)
{
  msg = "SOFC1D: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
}
// Interpolates to find f(xt) along the line defined by the other four variables.
