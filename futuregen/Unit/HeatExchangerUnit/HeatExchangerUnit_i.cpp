#include "V21Helper.h"
#include "HeatExchangerUnit_i.h"

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
    const char* iwater;
    const char* ogas;
    const char* owater;
    bool rv;
    Package pack;
    string therm_path="thermo";
    int i;
    const char* result;
    summary_values summaries;
    double Q = 0.0;
    
    const double pi = 3.14159265359;

    igas = executive_->GetImportData(id_, 0); //port 0 will be the gas input port;
    iwater = executive_->GetImportData(id_, 1); //port 1 will be the sweep input port;
    
    if (!igas || !iwater)
      {
	error("Missing input input.");
	return;
      }

    pack.SetSysId("water_in.xml");
      
    Gas *gas_in_data = new Gas;
    Water *water_in_data = new Water;

    V21Helper v21helper(therm_path.c_str());
    
    pack.Load(igas, strlen(igas)); 
    v21helper.IntToGas(&(pack.intfs[0]), *gas_in_data);
    
    pack.Load(iwater, strlen(iwater)); 
    v21helper.IntToWat(&(pack.intfs[0]), *water_in_data);

    // Check incoming
    if(gas_in_data->gas_composite.T <= 200 || gas_in_data->gas_composite.T >= 3000) {
      warning("Incoming gas temperature out of range.");
    }
   

    Steam67 steam_table;
    
    // CONDITIONS OF INCOMING WATER
    double enthalpy = water_in_data->H;
    double quality = water_in_data->Q;
    double pressure = water_in_data->P;
    double temperature = water_in_data->T;
    double entropy, sat_temp, weight, sat_press, sup_heat;
    double sub_cool, visc, crit_vel;;
    
    if(enthalpy < 1e-03) {
      // Get H,Q from T, P
      quality=weight=enthalpy=entropy=sat_temp=sat_press=sup_heat=sub_cool=visc=crit_vel=0;
      steam_table.calculate(&temperature, &pressure, &quality,
			    &weight, &enthalpy, &entropy, &sat_temp,
			    &sat_press, &sup_heat, &sub_cool, &visc,
			    &crit_vel, 0);
    }
    
    int NTU_eq;
    if(quality > 0.0 && quality < 1.0){
      // all HX's if water is at saturation
      NTU_eq = 0;
    }else {
      // shell and tube
      NTU_eq = 1;
    }
    // CALCULATE C's
    double cgas = gas_in_data->gas_composite.cp_mix(gas_in_data->gas_composite.T);
    double Cgas = gas_in_data->gas_composite.M * cgas;
    
    double p = water_in_data->P * 1.0e-5;
    double cwat;
    if(quality<1.0){ // cwat is not used for saturated water since cwat is infinite
      cwat = 1.83191e-3 * pow(p, 3) - 0.02339152 * pow(p, 2) + 24.28962 * p + 4186.514;
    }else{
      double mw_h2o = gas_in_data->thermo_database->mweight("H2O");
      cwat = gas_in_data->thermo_database->cp_is("H2O",temperature);
      if(cwat==-1.0){
	error("H2O gas must be in thermo database.");
	return;
      }
      cwat /= mw_h2o;
    }
    double Cwat = water_in_data->M * cwat;
    
    double Cmin, Cmax;
    if(Cgas < Cwat) { Cmin = Cgas; Cmax = Cwat; }
    else            { Cmin = Cwat; Cmax = Cgas; }
    
    if(quality > 0.0 && quality < 1.0){ Cmin = Cgas; Cmax = Cwat;} // Cmax is not used for this case
    
    double C = Cmin / Cmax;
    
    // OVERALL C  VECTIVE HEAT TRANSFER COEFFICIENT
    
    // Molecular Weight of Gas.
    double gasWeight = gas_in_data->gas_composite.mw();
    
    // AREA
    
    double hxA = pi * tube_od * tube_length * num_tubeL * num_tubeX;
    double xxA;
    if(arrangement=="Counter Flow" || arrangement=="Parallel Flow")
      xxA = num_tubeL*Sl*num_tubeX*St 
	- num_tubeL*num_tubeX*tube_od*tube_od*pi/4;
    else
      xxA = tube_length*St*num_tubeX;
    
    // Tube Diameter Ratio.
    double tratio = tube_od / tube_id;
    
    // Heat Transfer Coefficient.
    
    double GoutT = gas_in_data->gas_composite.T;
    double WoutT = water_in_data->T;
    double gout = GoutT;
    double wout = WoutT;
    
    double enthalpy_out = enthalpy, enthalpy_out1;
    
    // LOOP HERE
    int itcnt = 0;
    
    do {
      GoutT = (0.5 * gout) + (0.5 * GoutT);
      WoutT = (0.5 * wout) + (0.5 * WoutT);
      enthalpy_out1 = enthalpy_out;    
      
      double temp = (gas_in_data->gas_composite.T + GoutT) / 2;
      
      cgas = gas_in_data->gas_composite.cp_mix(temp);
      Cgas = gas_in_data->gas_composite.M * cgas;
      if(Cgas < Cwat) { Cmin = Cgas; Cmax = Cwat; }
      else            { Cmin = Cwat; Cmax = Cgas; }
      
      double Rho = ((gas_in_data->gas_composite.P - ext_press_drop / 2))
	/ (8315 * temp) * gasWeight;
      
      // Maximum Velocity.
      double velocity = gas_in_data->gas_composite.M / (xxA * Rho); 
      double Sd = sqrt(pow(Sl, 2) + pow(St / 2, 2));
      double Vmax = velocity;
      
      if(arrangement=="Cross Flow" || arrangement=="Shell & Tube") {
	Vmax = St/(St - tube_od) * velocity;
	if(tube_config=="Staggered" && (2 * (Sd - tube_od) < (St - tube_od)))
	  Vmax = St / (2 * (Sd - tube_od)) * velocity;
      }
      
      double k = gas_in_data->gas_composite.thermal_conduct(temp); 
      double mu = gas_in_data->gas_composite.Visc();
      double Pr = mu * gas_in_data->gas_composite.cp_mix(temp) / k;
      double Re = Rho * Vmax * tube_od / mu; 
      double fa = arrFactor(Re);
      double Nu, ho;
      if(arrangement=="Counter Flow" || arrangement=="Parallel Flow") {
	double hyd_dia = 4.0*xxA/(pi*tube_od*num_tubeL*num_tubeX);
	Re = Rho * Vmax * hyd_dia / mu;
	Nu = 0.023*pow(Re,0.8)*pow(Pr,0.3);
	ho = Nu * k / hyd_dia;
      } else { 
	Nu = 0.287 * pow(Re, 0.61) * pow(Pr, 0.33) * fa; // * fd ?
	ho = Nu * k / tube_od; // External Heat Transfer Coefficient.
      }
      
      // Fin Effect.
      if (use_fins) ho *= fin_effect;
      
      double FFO = 0; // fouling factor in
      double FFI = 0; // fouling factor out
      
      double U = 1 / ( 1 / ho + FFO + tratio * FFI );
      
      // FINISH CALCULATION
      
      double NTU = U * hxA / Cmin;
      
      double eff;
      
      if(NTU_eq==0){
	// all HX's if water is at saturation
	eff = 1 - exp(-NTU);
      } else {
	
	int i_arrangement;
	if (arrangement=="Counter Flow")
	  i_arrangement = 0;
	else if (arrangement=="Parallel Flow")
	  i_arrangement = 1;
	else if (arrangement=="Cross Flow")
	  i_arrangement = 2;
	else if (arrangement=="Shell & Tube")
	  i_arrangement = 3;

	switch(i_arrangement) {
	case 0: // Counter flow
	  eff = (1 - exp(-NTU*(1-C)))/(1 - C*exp(-NTU*(1-C)));
	  break;
	case 1: // Parallel Flow
	  eff = (1 - exp(-NTU*(1-C)))/(1+C);
	  break;
	case 2: // Cross Flow - single pass - gas is mixed
	  if(Cmin==Cgas)
	    eff = 1 - exp(-(1/C)*(1-exp(-C*NTU)));
	  else if (Cmax==Cgas)
	    eff = (1/C)*(1-exp(-C*(1-exp(-NTU))));
	  else
	    eff =  1 - exp((1/C)*pow(NTU,0.22)*(exp(-C*pow(NTU,0.78))-1));
	  break;
	case 3: // shell and tube
	  eff = 2/(1. + C + sqrt(1+C*C)*(1+exp(-NTU*sqrt(1+C*C)))/(1-exp(-NTU*sqrt(1+C*C))));
	  break;
	default: // shell and tube
	  eff = 2/(1. + C + sqrt(1+C*C)*(1+exp(-NTU*sqrt(1+C*C)))/(1-exp(-NTU*sqrt(1+C*C))));
	  break;
	}	
      }
      
      Q = Cmin * eff * fabs(gas_in_data->gas_composite.T - water_in_data->T);
      
      int s = (gas_in_data->gas_composite.T > water_in_data->T)?-1:1;
      
      gout = gas_in_data->gas_composite.T + s * Q / Cgas;
      enthalpy_out = enthalpy - s * Q / water_in_data->M;
      
      //    enthalpy_out = 0.5*enthalpy_out + 0.5*enthalpy_out1;
      
      // Get T,Q from H, P
      wout = 0;
      pressure = water_in_data->P - int_press_drop;
      quality=weight=entropy=sat_temp=sat_press=sup_heat=sub_cool=visc=crit_vel=0;
      steam_table.calculate(&wout, &pressure, &quality,
			    &weight, &enthalpy_out, &entropy, &sat_temp,
			    &sat_press, &sup_heat, &sub_cool, &visc,
			    &crit_vel, 0);
      itcnt++;
    } while ((fabs(GoutT-gout) > 1.0 || fabs(WoutT-wout) > 1.0)&&itcnt<1000);
    
    if(itcnt>=1000) warning("Maximum iterations exceeded.");
    
    Gas *gas_out_data = new Gas;
    Water *water_out_data = new Water;
    
    gas_out_data->copy(*gas_in_data);

    // SUMMARIES
    summaries.insert_summary_val("Heat Transferred UNITS:MW FORMAT:10.2f", Q / 1e6);
    summaries.insert_summary_val("Heat Transfer Area UNITS:m^2 FORMAT:10.5f", hxA);
    
    
    // FILL IN OUTGOING DATA
    
    gas_out_data->gas_composite.T = gout;
    gas_out_data->gas_composite.P = gas_in_data->gas_composite.P - ext_press_drop;
    gas_out_data->pressure_drop += ext_press_drop;
    gas_out_data->gas_composite.T_particle = gas_out_data->gas_composite.T;
    
    water_out_data->T = wout;
    water_out_data->P = water_in_data->P - int_press_drop;
    water_out_data->M = water_in_data->M;
    water_out_data->H = enthalpy_out;
    water_out_data->Q = quality;
    
    
    // Check incoming
    if(gas_out_data->gas_composite.T <= 200 || gas_out_data->gas_composite.T >= 3000) {
      warning("Outgoing gas temperature out of range.");
    }

    // SEND OUTGOING DATA   
    
    pack.intfs.resize(1); //each port has its own package
    pack.SetPackName("ExportData");
    pack.SetSysId("test.xml");
    
    v21helper.GasToInt(gas_out_data, pack.intfs[0]);
    ogas = pack.Save(rv);
    executive_->SetExportData(id_, 0, ogas);
    v21helper.WatToInt(water_out_data, pack.intfs[0]);
    owater = pack.Save(rv);
    executive_->SetExportData(id_, 1, owater);
    
    pack.intfs.clear();
    result = pack.Save(rv); 
    std::cout<<"cp5\n";
    executive_->SetModuleResult(id_, result); //marks the end the execution
    
    delete gas_in_data;
    delete water_in_data;
    delete gas_out_data;
    delete water_out_data;

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
    Package pack;
    pack.SetPackName("Status");
    pack.SetSysId("status.xml");
    pack.intfs.resize(1);
    pack.intfs[0].setInt("return_state", return_state);
    status = pack.Save(rv);
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
    Package pack;
        
    pack.SetSysId("gui.xml");
    pack.Load(param, strlen(param));
    //Now make use of pack.intfs to get your GUI vars out
    arrangement= pack.intfs[0].getString("arrangement");
    tube_config= pack.intfs[0].getString("tube_config");
    num_tubeL= pack.intfs[0].getInt("num_tubeL");
    num_tubeX= pack.intfs[0].getInt("num_tubeX");
    Sl= pack.intfs[0].getDouble("Sl"); 
    St= pack.intfs[0].getDouble("St"); 
    tube_id= pack.intfs[0].getDouble("tube_id");
    tube_od= pack.intfs[0].getDouble("tube_od");
    tube_length= pack.intfs[0].getDouble("tube_length");
    int_press_drop= pack.intfs[0].getDouble("int_press_drop");
    ext_press_drop= pack.intfs[0].getDouble("ext_press_drop"); 
    use_fins= pack.intfs[0].getInt("use_fins");
    fin_effect= pack.intfs[0].getDouble("fin_effect");
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
  Package pack;
  const char* result;
  bool rv;
  pack.SetPackName("result");
  pack.SetSysId("result.xml");
  msg+="\n";
  executive_->SetModuleMessage(id_, msg.c_str());
  pack.intfs.clear();
  result = pack.Save(rv);
  return_state = 1;
  executive_->SetModuleResult(id_, result); //this marks the end the execution
}

void Body_Unit_i::warning (std::string msg)
{
  msg+="\n";
  executive_->SetModuleMessage(id_, msg.c_str());
}
// Interpolates to find f(xt) along the line defined by the other four variables.
// Interpolates to find f(xt) along the line defined by the other four variables.
double Body_Unit_i::interpolate(double x1,double x2,double y1,double y2, double xt) {
  return(y1 + (y2 - y1) * (xt - x1) / (x2 - x1));
}

// Ranger determines the upper and lower bounds for given value, for
// interpolation in a table.  It will also adjust the given value to the
// maximum or minimum allowable if the value is out of the range of the table.
int Body_Unit_i::Ranger(double var, double *varadjusP, double *varloP, double *varhiP, 
			  double varange[][6], int index, int limit) {
  double varadjus = *varadjusP;
  double varlo = *varloP;
  double varhi = *varhiP;
  int i=-1;
  int done = 0;
  if (var > varange[index][limit + 1]) 
    varadjus = varange[index][limit + 1];
  else if (var < varange[index][1])
    varadjus = varange[index][1];
  else varadjus = var;
  while((!done) && (i < limit)) {
    i = i + 1;
    if ((varadjus >= varange[index][i]) && (varadjus <= varange[index][i + 1])) {
      varlo = varange[index][i];
      varhi = varange[index][i + 1];
      done = 1;
    }
  }
  *varadjusP = varadjus;
  *varloP = varlo;
  *varhiP = varhi;
  return(i);
}

// Calculate the arrangement factor (Fa) for a given cross
// pitch/tube OD ratio, longitudinal pitch/tube OD ratio, and Reynold's
// number.  The calculation uses a digitized family of curves (from
// Steam, p. 4-11) and an interpolation routine.
double Body_Unit_i::arrFactor(double Re) {
  double Sl_D, St_D, fa, fd;
  double St_Dx, St_Dlo, St_Dhi;
  double Sl_Dx, Sl_Dlo, Sl_Dhi;
  double rex, relo, rehi;
  double intert1, intert2, inter1, inter2;
  int i, j, k;
  double fadata[6][5][4] = {{{.39, .55, .69, .72},
			     {.50, .71, .91, .89},
			     {.80, .87, .99, .99},
			     {.96, .90, 1.07, 1.05},
			     {.97, .90, 1.10, 1.09}},
			    {{.36, .45, .59, .68},
			     {.47, .61, .82, .84},
			     {.72, .79, .92, .92},
			     {.93, .84, 1.00, 1.00},
			     {.95, .87, 1.05, 1.04}},
			    {{.33, .39, .52, .59},
			     {.42, .56, .74, .79},
			     {.67, .73, .85, .84},
			     {.90, .79, .91, .91},
			     {.92, .82, .97, .96}},
			    {{.33, .37, .48, .53},
			     {.41, .54, .70, .78},
			     {.64, .71, .81, .80},
			     {.84, .77, .88, .88},
			     {.89, .80, .93, .96}},
			    {{.35, .38, .46, .50},
			     {.43, .55, .71, .79},
			     {.63, .70, .80, .82},
			     {.79, .75, .88, .89},
			     {.85, .79, .93, .96}},
			    {{.40, .39, .45, .48},
			     {.47, .57, .73, .82},
			     {.62, .68, .82, .89},
			     {.71, .74, .89, .91},
			     {.80, .78, .93, .96}}};
  double faranges[3][6] = {{1.5, 2, 3, 4, 5, 6},
			   {1, 1.1, 1.25, 1.5, 2, 2},
			   {2000, 8000, 20000, 40000, 40000, 40000}};
  double fdcurve[9] = {.70, .82, .87, .91, .93, .95, .97, .98, .99};

  Sl_D = Sl / tube_od;
  St_D = St / tube_od;
  
  i = Ranger(St_D, &St_Dx, &St_Dlo, &St_Dhi, faranges, 0, 4);
  j = Ranger(Sl_D, &Sl_Dx, &Sl_Dlo, &Sl_Dhi, faranges, 1, 3);
  k = Ranger(Re, &rex, &relo, &rehi, faranges, 2, 2);
  // These next few are intermediate interpolations.
  intert1 = interpolate(St_Dlo, St_Dhi, fadata[i][j][k], fadata[i + 1][j][k], St_Dx);
  intert2 = interpolate(St_Dlo, St_Dhi, fadata[i][j+1][k], fadata[i+1][j+1][k], St_Dx);
  inter1 = interpolate(Sl_Dlo, Sl_Dhi, intert1, intert2, Sl_Dx);
  intert1 = interpolate(St_Dlo, St_Dhi, fadata[i][j][k+1], fadata[i+1][j][k+1], St_Dx);
  intert2 = interpolate(St_Dlo, St_Dhi, fadata[i][j+1][k+1], fadata[i+1][j+1][k+1], St_Dx);
  inter2 = interpolate(Sl_Dlo, Sl_Dhi, intert1, intert2, Sl_Dx);
  // This one calculates the answer.
  fa = interpolate(relo, rehi, inter1, inter2, rex);

  // Calculate the depth factor (as defined in Steam, Ch. 4).
  if (num_tubeL > 9)
    fd = 1;
  else
    fd = fdcurve[num_tubeL-1];
  
  return(fa);
}

