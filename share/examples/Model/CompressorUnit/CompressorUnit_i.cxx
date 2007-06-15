/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "V21Helper.h"
#include "CompressorUnit_i.h"

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
	error("Compressor: Missing input input.");
	return;
      }

    p.SetSysId("gas_in.xml");
    p.Load(igas, strlen(igas)); 

    Gas *gas_in_data = new Gas;

    V21Helper gashelper(therm_path.c_str());
    gashelper.IntToGas(&(p.intfs[0]), *gas_in_data);

    // Check incoming
    if(gas_in_data->gas_composite.T <= 200 || gas_in_data->gas_composite.T >= 3000) {
      warning("Compressor: Incoming gas temperature out of range.");
    }
    
    Gas *gas_out_data = new Gas; 

    REAL tinlet                = gas_in_data->gas_composite.T;   // kelvin
    REAL pinlet                = gas_in_data->gas_composite.P;   // pascals
    REAL eta                   = eff;                      // isentropic efficiency
    REAL total_molar_flow_rate = gas_in_data->gas_composite.M / 
      gas_in_data->gas_composite.mw();                             // kmol/sec
    
    REAL poutlet;
    if(case_type==0) {
      poutlet = pinlet + pressure_change; // pascals
    } else {
      poutlet = pressure_out; // pascals
      pressure_change=poutlet-pinlet;
    }

    if(poutlet<0) {
      error("Compressor: Pressure drop too large.");
      return;
    }

    thermo thm;
    thm.read_thermo(therm_path.c_str());

    // how many species are in the thermo file?
    int nspecies = thm.get_spec_nam().size();
    
    // initialize composition
    std::vector<REAL> composition;
    for(int i=0;i<nspecies;i++)
      composition.push_back(0.0);

// grab map for species names
  const std::map< std::string, int>& name_map = thm.get_nam_spec();
	
  // look up and set names for species of interest
  std::map< std::string, int >::const_iterator iter, iter2;
  std::vector<int> species_indexes;

  // find and store thermo file indexes for species of interest
  for(iter2=gas_in_data->specie.begin(); iter2!=gas_in_data->specie.end(); iter2++)
    {	
      iter = name_map.find(iter2->first);
      
      if(iter!=name_map.end()){
	species_indexes.push_back((*iter).second);
	// now manually set mole fractions (this again will be replaced by scirun code)
	composition[(*iter).second] = gas_in_data->gas_composite.comp_specie[iter2->second];
      } else {
	error("Compressor: Species " + iter2->first + " not found - critical error!!!");
	return;
      }
    }
  // find entropy of mixture
  REAL total_entropy = thm.entropy_mix(composition, tinlet, pinlet);
  
  // now find exit temperature assuming this process is isentropic
  REAL ts_exit = tinlet;  // used for initial guess
  
  if(!thm.find_temperature(ts_exit, total_entropy, composition, poutlet)) {
    error("Compressor: Convergence problem - finding temp from entropy.");
    return;
  }

  // now find h2s (isentropic enthalpy at outlet)
  REAL h2s = thm.enthalpy_mix(composition, ts_exit);
  
  // now find h2a (actual enthalpy at outlet) using isentropic efficiency (eta) of
  // compressor/turbine
  REAL h2a;
  REAL h1 = thm.enthalpy_mix(composition, tinlet);  // entrance enthalpy

  if(pinlet<poutlet)   // compressor
    h2a = -1*((h1-h2s)/eta-h1);
  else                 // expander/turbine
    h2a = -1*((h1-h2s)*eta-h1);
  
  // find temperature at exit and power
  REAL texit = tinlet;   // initial guess
  REAL cp_mix;
  
  if(!thm.find_temperature(texit, cp_mix, h2a, composition)){
    warning("Compressor: Convergence problem - temp from enthalpy.");
    return;
  }

  //fill out the output stream  
  p.intfs.resize(1); //each port has its own package
  gas_out_data->copy(*gas_in_data);

  gas_out_data->gas_composite.T = texit;
  gas_out_data->gas_composite.P = poutlet;
  
  gashelper.GasToInt(gas_out_data, p.intfs[0]);
 
  p.SetPackName("ExportData");
  p.SetSysId("test.xml");
  ogas = p.Save(rv);
  executive_->SetExportData(id_, 0, ogas);
  
  if(gas_out_data->gas_composite.T <= 200 || gas_out_data->gas_composite.T >= 3000) {
    warning("Compressor: Outgoing gas temperature out of range");
  }

  //Here is the result table
  summaries.clear();
  
  // Fill in summary tables
  summaries.insert_summary_val("Power UNITS:MW FORMAT:10.2f",
			       -total_molar_flow_rate*(h2a-h1)/1e6);
  if(case_type == 1) {
    summaries.insert_summary_val("Pressure Change UNITS:Pa FORMAT:10.2f",
				 poutlet-pinlet);
  }
  
  summaries.insert_summary_val("Power UNITS:MW FORMAT:12.2f", -total_molar_flow_rate*(h2a-h1)/1e6);

  p.intfs.resize(1);
  
  gashelper.SumToInt(&summaries, p.intfs[0]);
  result = p.Save(rv);

  executive_->SetModuleResult(id_, result); //marks the end the execution

  if(gas_in_data)  delete gas_in_data;
  if(gas_out_data) delete gas_out_data;
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
    eff = p.intfs[0].getDouble("eff");
    pressure_out = p.intfs[0].getDouble("pressure_out");
    pressure_change = p.intfs[0].getDouble("pressure_change");
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
