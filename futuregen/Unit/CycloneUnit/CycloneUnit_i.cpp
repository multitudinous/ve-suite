#include "V21Helper.h"
#include <ThirdParty/Math/logfit.h>
#include <ThirdParty/Components/cyclone.h>
#include "CycloneUnit_i.h"

using namespace Vision21 ;

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
    cout << "StartCalc\n";

    // Add your implementation here
    const char* igas;
    const char* ogas[2];
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
    p.Load(igas, strlen(igas)); 
    
    Gas *gas_in_data = new Gas();
 
    V21Helper gashelper(therm_path.c_str());
    gashelper.IntToGas(&(p.intfs[0]), *gas_in_data);
    
    // Check incoming
    if(gas_in_data->gas_composite.T <= 200 || gas_in_data->gas_composite.T >= 3000) {
      warning("Incoming gas temperature out of range.");
    }    

    float cyclone_diameter = diameter; // 2.0
    float N = _N; // 5.0
    float K = _K; // 8.0
    
    // Typical values
    float Wi = cyclone_diameter * 0.25;
    float H  = cyclone_diameter * 0.5;
    
    // From gas stream
    float particle_density = 100.0;  // kg/m^3
    float gas_density      = gas_in_data->gas_composite.density(); // kg/m^3
    float gas_viscosity    = gas_in_data->gas_composite.Visc();
    float gas_mass_flow_rate  = gas_in_data->gas_composite.M; // kg/sec
    float part_mass_flow_rate = gas_in_data->gas_composite.M_particle; // kg/sec
    // current particle size distribution is from De Nevers book pg 214
    // it is a log normal distribution with mean = 20 and std dev = 1.25
    //double mean = log(20);
    //double sd = 1.25;
    double mean = gas_in_data->gas_composite.mean_size * 1e+6;
    double sd = pow(gas_in_data->gas_composite.size_variance, 0.5) * 1e+6;
    
    //cout << "SIZE " << mean << " " << sd << endl;
    double velocity = gas_mass_flow_rate/(gas_density*Wi*H);
    
    cyclone cycl(N, velocity, particle_density, 
		 Wi, K, gas_density, gas_viscosity);
    
    // evenly subdivide log distribution by mass fraction (NOT DIAMETER) - then solve for diameter
    // each mass fraction "slice" is 1.0/n
    
    const int n = 5;
    double fract_size[20], diameters_mid[20];
    double passed_areas[19] = {0.05, 0.2, 0.5, 0.2, 0.05, 0.0, 0.0,0.00, 0.00,
			       0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    int n1 = n;
    createbins_(diameters_mid, fract_size, passed_areas, &n1, &mean, &sd);
    
    double new_part_flow_rate=0.0,collected_flow_rate=0.0;	
    
    std::vector<double> etas(n);
    std::vector<double> p_delta_phi(n), c_delta_phi(n); //passed and collected
    std::vector<double> sum_p_delta_phi(n), sum_c_delta_phi(n);
    
    for(i=0; i<n; i++) {
      
      // find etas
      etas[i] = cycl.calc_removal_eff(diameters_mid[i]/1.0e6);
      
      // find p_delta_phi (amount of mass in this size interval that passes through uncollected)
      // (fi*(1-etai))
      p_delta_phi[i] = (1.0-etas[i])*fract_size[i];
      
      // find c_delta_phi (amount of mass in this size interval that is collected)
      // (fi*etai)
      c_delta_phi[i] = etas[i]*fract_size[i];
      
      // find sum_p_delta_phi (SUM(fi*(1-etai))
      if(i==0)sum_p_delta_phi[i] = p_delta_phi[i];
      else sum_p_delta_phi[i] = sum_p_delta_phi[i-1] + p_delta_phi[i];
      
      // find sum_c_delta_phi (SUM(fi*etai))
      if(i==0)sum_c_delta_phi[i] = c_delta_phi[i];
      else sum_c_delta_phi[i] = sum_c_delta_phi[i-1] + c_delta_phi[i];
      
      // find new particle mass flow rate
      new_part_flow_rate += part_mass_flow_rate*fract_size[i]*(1.0-etas[i]);
      collected_flow_rate += part_mass_flow_rate*fract_size[i]*etas[i];
    }
    
    // now calc mass mean diameter and std dev of new distribution using standard techniques
    double mmd = 0.0, mmd_c = 0.0, var_c = 0.0;
    std::vector<double> new_phis(n, 0.0), c_phis(n, 0.0);
    
    double mmd_test = 0.0, var_test = 0.0;
    for(i=0; i<n; i++) {
      // fi(new) = fi*(1-etai)/SUM(fi*(1-etai)) these are new fractions for each region
      if(sum_p_delta_phi[n-1]>0.0) new_phis[i] = fract_size[i]*(1.0-etas[i])/sum_p_delta_phi[n-1];  
      
      // mass mean diameter new = SUM(di*fi(new))
      mmd += diameters_mid[i]*new_phis[i];   
      
      if(sum_c_delta_phi[n-1]>0.0) c_phis[i] = fract_size[i]*etas[i]/sum_c_delta_phi[n-1];
      //cout << new_phis[i] << " " << c_phis[i] << " " << fract_size[i]<< " " << diameters_mid[i] << endl;
      mmd_c += diameters_mid[i]*c_phis[i];
      mmd_test += diameters_mid[i]*fract_size[i];
    }
    //cout << "MMD_C " << mmd_c << " mmd " << mmd << " " << sum_c_delta_phi[n-1] << endl;
    
    sd = 0.0;
    
    for(i=0;i<n;i++) {
      // sd^2 = SUM((di-d(mean,new))^2*fi(new))
      sd += pow((diameters_mid[i]-mmd),2.0)*new_phis[i];  
      var_c += pow((diameters_mid[i]-mmd_c),2.0)*c_phis[i];
      var_test += pow((diameters_mid[i]-mmd_test),2.0)*fract_size[i];
      //cout << var_test << endl;
    }
    //cout << "SIZE 2 " << mmd_test << " " << var_test << endl;
    
    // outputs
    summaries.clear();
    summaries.insert_summary_val("Particle Flowrate Exit UNITS:kg/sec FORMAT:10.2f",
				 part_mass_flow_rate * (sum_p_delta_phi[n-1]));
    summaries.insert_summary_val("Particle flow collected UNITS:% FORMAT:10.2f",
				 (1.0 - sum_p_delta_phi[n-1]) * 100.0);
    summaries.insert_summary_val("Particle flow passed UNITS:% FORMAT:10.2f",
				 (sum_p_delta_phi[n-1]) * 100.0);
    summaries.insert_summary_val("Mass Mean diameter UNITS:microns FORMAT:10.2f", mmd);
    summaries.insert_summary_val("Std Dev UNITS:microns FORMAT:10.2f", sqrt(sd));
    summaries.insert_summary_val("Pressure Drop UNITS:kPa FORMAT:10.2f",
				 cycl.calc_pdrop() / 1000.0);
    summaries.insert_summary_val("Cyclone cut diameter UNITS:micron FORMAT:10.2f", 
				 cycl.calc_dcut() * 1.0e6);
    summaries.insert_summary_val("Inlet gas velocity UNITS:m/s FORMAT:10.2f",velocity);
    
    //# Fill in gas_out_data
    Gas *gas_out_data = new Gas;
    gas_out_data->copy(*gas_in_data);
    delete gas_in_data;
    gas_out_data->gas_composite.mean_size = mmd * 1.0e-6;
    gas_out_data->gas_composite.size_variance = sd * 1.0e-12;
    gas_out_data->gas_composite.M_particle = part_mass_flow_rate * (sum_p_delta_phi[n-1]); 
    gas_out_data->gas_composite.P -= cycl.calc_pdrop();
    gas_out_data->pressure_drop += cycl.calc_pdrop();
    
    p.intfs.resize(1); //each port has its own package
    p.SetPackName("ExportData");
    p.SetSysId("test.xml");

    gashelper.GasToInt(gas_out_data, p.intfs[0]);
    ogas[0] = p.Save(rv);
    executive_->SetExportData(id_, 0, ogas[0]);
  
    Gas *part_out_data = new Gas;
    part_out_data->copy(*gas_out_data);
    part_out_data->gas_composite.M = 0.0;
    part_out_data->gas_composite.mean_size = mmd_c * 1.0e-6;
    part_out_data->gas_composite.size_variance = var_c * 1.0e-12;
    part_out_data->gas_composite.M_particle = part_mass_flow_rate * (sum_c_delta_phi[n-1]); 
    gashelper.GasToInt(part_out_data, p.intfs[0]);
    ogas[1] = p.Save(rv);
    executive_->SetExportData(id_, 1, ogas[1]);

    if(gas_out_data->gas_composite.T <= 200 || gas_out_data->gas_composite.T >= 3000) {
      warning("Outgoing gas temperature out of range");
    }

    p.intfs.resize(1);
    gashelper.SumToInt(&summaries, p.intfs[0]);
    result = p.Save(rv);

    executive_->SetModuleResult(id_, result); //marks the end the execution
    delete gas_out_data;
    delete part_out_data;
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
    if (string(param)=="") 
		return;
   
    std::cout<<UnitName_<<" :SetParams called"<<endl;
    
    Package p;
    p.SetSysId("gui.xml");
    p.Load(param, strlen(param));
    
    //Now make use of p.intfs to get your GUI vars out
    diameter = p.intfs[0].getDouble("diameter");
    _N = p.intfs[0].getDouble("particle_turn_count");
    _K = p.intfs[0].getDouble("velocity_heads");
    
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
    std::cout<<UnitName_<<" :SetID called"<<endl;

    id_=id;
  }
  
CORBA::Long Body_Unit_i::GetID (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
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
