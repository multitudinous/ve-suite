#include "V21Helper.h"
#include "CarbonBedUnit_i.h"

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
	error("CarbonBed: Missing input input.");
	return;
      }

    p.SetSysId("gas_in.xml");
    p.Load(igas, strlen(igas)); 

    Gas *gas_in = new Gas;

    V21Helper gashelper(therm_path.c_str());
    gashelper.IntToGas(&(p.intfs[0]), *gas_in);

    gas_in->gas_composite.normalize_specie();
    double gas_temp = gas_in->gas_composite.T;
    double gas_density = gas_in->gas_composite.density();
    double gas_viscosity = gas_in->gas_composite.Visc();
    double gas_flow_rate = gas_in->gas_composite.M;
    double hg_feed_rate = gas_in->gas_composite.moles("HG");
    double change_by = 0.0;
    if(hg_feed_rate>0.0){
       change_by = -0.99*hg_feed_rate;
       hg_feed_rate *= gas_in->thermo_database->mweight("HG")/1000.0*7936.6;
    }else{
       hg_feed_rate=0.0;
    }

    // Fluidization velocity calculation
    
    double Ga = ( pow(part_diam, 3) * gas_density * ( bulk_density - gas_density ) * 9.81 ) / pow(gas_viscosity, 2);
    double Remf = 25.7 * ( sqrt( 1+.0000553*Ga) - 1 );
    double Umf = Remf * ( gas_viscosity / (part_diam * gas_density) );  // Fluid velocity
  
    // Pressure drop / ft
    double Uo = 0.85 * Umf;  // Superficial velocity
    double exp = 0.7;  // Kunii's constant deal or whatever
    double pressure_drop = 150*pow((1-exp), 2) / pow(exp, 3) * gas_viscosity * Uo / part_diam;
    pressure_drop = pressure_drop + 1.75*(1-exp) / pow(exp, 3) * gas_density * pow(Uo, 2) / part_diam;
    
    // Bed height
    double tau = 20.0;  // Calgon-Carbon / Eastman (in seconds)
    //if( false /* If tau is different for AGR, make the change here */ )
    //  ; // tau = 20.0;
    double bed_height = Uo * tau;  // In meters
    
    // Calculate the mass of coal needed
    double area = gas_flow_rate / gas_density / Uo;
    double mass_of_carbon = 592.7 /* kg/m3 Carbon */ * area * bed_height;
    
    // Absorber diameter
    double absorber_diam = sqrt( 4 * area / 3.14 ); // Meters

    // Bed total pressure drop
    double bed_pressure_drop = pressure_drop*bed_height;
    
    // Mercury leaving absorber (Assuming dry coal) We don't get a feed rate so we'll just use what Temi calc'd
    double coal_feed_rate = 20.531; // kg/s  *** Hardwired for now until we figure out how to get this form the gasifier ***
    double hg_in_Illinois06 = 100; // ug/kg d.b.
    // double mercury_feed_rate = coal_feed_rate * hg_in_Illinois06;
    double hg_leaving_absorber = 0.01 * hg_feed_rate;  // This is absolutely pointless!
    
    // Carbon loading
    double carbon_loading = hg_feed_rate * 0.99 / mass_of_carbon / 7936.6;
 
    //
    // Fill in the gas out data structure and send it on it's way...
    //

    // Martin,
    // gas_out begins as an exact copy of gas_in -
    // Just for the ease of filling everything in.

    Gas *gas_out = new Gas; 
    
    gas_out->copy(*gas_in);
        
    gas_out->gas_composite.P -= bed_pressure_drop;
    gas_out->gas_composite.moles(change_by,"HG");
    // now do HGCL2
    change_by = gas_in->gas_composite.moles("HGCL2");
    if(change_by>0.0){
       change_by *= -0.99;
       gas_out->gas_composite.moles(change_by,"HGCL2");
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
    
    summaries.insert_summary_val("Min. Fluidization Vel. UNITS:m/sec FORMAT:10.2f", Umf);
    summaries.insert_summary_val("Pressure Drop UNITS:Pa/m FORMAT:10.2f", pressure_drop );
    summaries.insert_summary_val("Total Bed Pressure Drop UNITS:Pa FORMAT:10.2f", bed_pressure_drop);
    summaries.insert_summary_val("Superficial Velocity UNITS:m/sec FORMAT:10.2f", Uo);
    summaries.insert_summary_val("Bed Height UNITS:m FORMAT:10.2f", bed_height);
    //summaries.insert_summary_val("Initial Carbon Charge FORMAT:10.2f", carbon_loading); // Temi says that this is the same as loading
    summaries.insert_summary_val("Adsorber Diameter UNITS:m FORMAT:10.2f", absorber_diam);
    summaries.insert_summary_val("Hg Leaving Adsorber UNITS:lb/hr FORMAT:10.2e", hg_leaving_absorber);
    //summaries.insert_summary_val("Carbon Loading UNITS:1/sec FORMAT:10.2f", carbon_loading);
    summaries.insert_summary_val("Number of Adsorbers UNITS:# FORMAT:10.2f", 1);
    
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
    carbon_type = p.intfs[0].getInt("carbon_type");
    press_drop = p.intfs[0].getDouble("press_drop");
    part_diam = p.intfs[0].getDouble("part_diam")*0.001;
    bulk_density = p.intfs[0].getDouble("bulk_density");
    temp = p.intfs[0].getDouble("temp");
    press = p.intfs[0].getDouble("press");
    cr_time = p.intfs[0].getDouble("cr_time");
    porosity = p.intfs[0].getDouble("porosity");
    res_time = p.intfs[0].getDouble("res_time");
   
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

double Body_Unit_i::Meters2Feet(double meters)
{
  return meters * 100 / 2.54 / 12;
}
