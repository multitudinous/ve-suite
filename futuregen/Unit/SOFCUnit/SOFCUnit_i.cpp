#include "V21Helper.h"
#include <ThirdParty/Components/fc.h>
#include "SOFCUnit_i.h"

using namespace Vision21;

// Implementation skeleton constructor
Body_Unit_i::Body_Unit_i (Body::Executive_ptr exec, std::string name)
  : executive_(Body::Executive::_duplicate(exec))
{
  UnitName_= name;
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
    const char* ogas;
    bool rv;
    Package p;
    string therm_path="thermo";
    int i;
    const char* result;
    summary_values summaries;

    igas1 = executive_->GetImportData(id_, 0); //port 0 will be the gas input port;
    igas2 = executive_->GetImportData(id_, 1); //port 1 will be the second gas input port;
    
    if (string(igas1)=="" || string(igas2)=="") {
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
    
    if(gas_in_cathode->gas_composite.T <= 200 || gas_in_cathode->gas_composite.T >= 3000)  {
      warning("Incoming gas temperature out of range.");
    }
    
    Gas *gas_out_data = new Gas;

    //set fuel cell parameters
    fuel_cell::fc_params fcp;
    
    fcp.pressure = gas_in_anode->gas_composite.P; // Anode or Cathode?
    fcp.fuel_util = fuel_util / 100; // % --> fraction
    
    fcp.athick = a_thickness;
    fcp.cthick = c_thickness;
    fcp.ethick = e_thickness;
    
    fcp.anode_A       =  a_A;
    fcp.cathode_A     =  c_A;
    fcp.electrolyte_A =  e_A;
    fcp.anode_E       =  a_E;
    fcp.cathode_E     =  c_E;
    fcp.electrolyte_E =  e_E;
    
    fcp.area_per_unit_cell = cell_area;
    fcp.number_of_cells    = num_cell;
    
    fcp.internal_reformer = false;
    
    thermo thm(therm_path);
  
    aspen_stream cathin,anodein;
    
    // heat up streams to reaction temperatures
    double tin_anode   = gas_in_anode->gas_composite.T;
    double tin_cathode = gas_in_cathode->gas_composite.T;

    /*
      double q_heater = 0;
      if(tin_anode < 750) {
      cerr << "Anode temperature < 750, setting to 750.\n";
      tin_anode = 750;
      
      // how many species are in the thermo file?
      int nspecies = thm.get_spec_nam().size();
      
      // initialize composition
      std::vector<REAL> composition(nspecies, 0.0);
      
      // grab map for species names
      const std::map<const std::string, int>& name_map = thm.get_nam_spec();
      
      // look up and set names for species of interest
      std::map< const std::string, int >::const_iterator iter, iter2;
      std::vector<int> species_indexes;
      
      // find and store thermo file indexes for species of interest
      for(iter2=gas_in_anode->specie.begin(); iter2!=gas_in_anode->specie.end(); iter2++)
      {	
      iter = name_map.find(iter2->first);
      
      if(iter!=name_map.end()){
      species_indexes.push_back((*iter).second);
      // now manually set mole fractions (this again will be replaced by scirun code)
      composition[(*iter).second] = gas_in_anode->gas_composite.comp_specie[iter2->second];
      } else {
      cout << "species " << iter2->first << " not found - critical error!!!" << endl;
      }
      }
      
      double h1 = thm.enthalpy_mix(composition, gas_in_anode->gas_composite.T);
      double h2 = thm.enthalpy_mix(composition, tin_anode);
      
      q_heater += (gas_in_anode->gas_composite.M / gas_in_anode->gas_composite.mw())
      * (h2 - h1) / 1e6;
      }
      if(tin_cathode < 750) {
      cerr << "Cathode temperature < 750, setting to 750.\n";
      tin_cathode = 750;
      
      // how many species are in the thermo file?
      int nspecies = thm.get_spec_nam().size();
      
      // initialize composition
      std::vector<REAL> composition(nspecies, 0.0);
      
      // grab map for species names
      const std::map<const std::string, int>& name_map = thm.get_nam_spec();
      
      // look up and set names for species of interest
      std::map< const std::string, int >::const_iterator iter, iter2;
      std::vector<int> species_indexes;
      
      // find and store thermo file indexes for species of interest
      for(iter2=gas_in_cathode->specie.begin(); iter2!=gas_in_cathode->specie.end(); iter2++)
      {	
      iter = name_map.find(iter2->first);
      
      if(iter!=name_map.end()){
      species_indexes.push_back((*iter).second);
      // now manually set mole fractions (this again will be replaced by scirun code)
      composition[(*iter).second] = gas_in_cathode->gas_composite.comp_specie[iter2->second];
      } else {
      cout << "species " << iter2->first << " not found - critical error!!!" << endl;
      }
      }
      
      double h1 = thm.enthalpy_mix(composition, gas_in_cathode->gas_composite.T);
      double h2 = thm.enthalpy_mix(composition, tin_cathode);
      
      q_heater += (gas_in_cathode->gas_composite.M / gas_in_cathode->gas_composite.mw())
      * (h2 - h1) / 1e6;
      }
    */

    anodein.set_thermo(&thm);
    anodein.set_temp(tin_anode);
    anodein.set_mass_flow_rate(gas_in_anode->gas_composite.M);
    
    map<const string, int>::iterator iter;
    
    for(iter=gas_in_anode->specie.begin(); iter!=gas_in_anode->specie.end(); iter++)
      anodein.set_mole_fraction(iter->first,
				gas_in_anode->gas_composite.comp_specie[iter->second]);
    
    
    cathin.set_thermo(&thm);
    cathin.set_temp(tin_cathode);
    cathin.set_mass_flow_rate(gas_in_cathode->gas_composite.M);
    
    for(iter=gas_in_cathode->specie.begin(); iter!=gas_in_cathode->specie.end(); iter++)
      cathin.set_mole_fraction(iter->first,
			       gas_in_cathode->gas_composite.comp_specie[iter->second]);
    
    // construct fuel cell obj and go
    fuel_cell fc(thm);
  
    // set incoming anode and cathode streams
    fc.set_cathode_stream(cathin);
    fc.set_anode_stream(anodein);
    
    fc.set_fc_params(fcp);
  
    double tanode, fc_power;
    if(!fc.calc_power(tanode,fc_power)) {
      error("Error in calc_power.");
      return;
    } 
    
    summaries.insert_summary_val("Anode Temperature UNITS:K FORMAT:12.2f", tanode);
    summaries.insert_summary_val("Fuel Cell Power UNITS:MW FORMAT:12.2f", fc_power/1e06);
    //summaries.insert_summary_val("Auxiliary Power UNITS:MW FORMAT:12.2f", q_heater);
    //summaries.insert_summary_val("Net Power UNITS:MW FORMAT:12.2f", fc_power/1e6-q_heater);
    
    //  gui->eval(id + " module_power " + to_string(fc_power/1e6), result);

    aspen_stream output_stream;
    fc.get_output_stream(output_stream);

    // Fill in Gas out - it's brand new
    // vector<double> mf = output_stream.get_mole_fractions(); // whats the order?
    std::vector<std::string> nam_spec = thm.get_spec_nam();
    std::vector<std::string>::iterator iter2;
    for(iter2=nam_spec.begin(); iter2!=nam_spec.end(); iter2++) {
      gas_out_data->addSpecie(*iter2);
      gas_out_data->gas_composite.setFrac(*iter2, output_stream.get_mole_fraction(*iter2));
    }
    gas_out_data->gas_composite.T = output_stream.get_temp();
    gas_out_data->gas_composite.M = output_stream.get_mass_flow_rate();
    gas_out_data->gas_composite.P = gas_in_anode->gas_composite.P - press_drop;
    
    
    // Check incoming
    if(gas_out_data->gas_composite.T <= 200 || gas_out_data->gas_composite.T >= 3000) {
      warning("Outgoing gas temperature out of range.");
    }
  
    p.intfs.resize(1); //each port has its own package
    p.SetPackName("ExportData");
    p.SetSysId("test.xml");
    
    gashelper.GasToInt(gas_out_data, p.intfs[0]);
    ogas = p.Save(rv);
    executive_->SetExportData(id_, 0, ogas);
    
    p.intfs.resize(1);
    gashelper.SumToInt(&summaries, p.intfs[0]);
    result = p.Save(rv); 

    executive_->SetModuleResult(id_, result); //marks the end the execution
    
    
    if(gas_in_anode)   delete gas_in_anode;
    if(gas_in_cathode) delete gas_in_cathode;
    if(gas_out_data)   delete gas_out_data;
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

    a_thickness = p.intfs[0].getDouble("a_thickness");
    c_thickness = p.intfs[0].getDouble("c_thickness");
    e_thickness = p.intfs[0].getDouble("e_thickness"); 
    a_A         = p.intfs[0].getDouble("a_A");             
    a_E         = p.intfs[0].getDouble("a_E");
    c_A         = p.intfs[0].getDouble("c_A");           
    c_E         = p.intfs[0].getDouble("c_E");
    e_A         = p.intfs[0].getDouble("e_A");             
    e_E         = p.intfs[0].getDouble("e_E");
    cell_area   = p.intfs[0].getDouble("cell_area"); 
    num_cell    = p.intfs[0].getDouble("num_cell");
    fuel_util   = p.intfs[0].getDouble("fuel_util");
    press_drop  = p.intfs[0].getDouble("press_drop");
    
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
  msg = "SOFC: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
  p.intfs.clear();
  result = p.Save(rv);
  return_state = 1;
  executive_->SetModuleResult(id_, result); //this marks the end the execution
}

void Body_Unit_i::warning (std::string msg)
{
  msg = "SOFC: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
}
// Interpolates to find f(xt) along the line defined by the other four variables.
