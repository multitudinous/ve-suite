#include "V21Helper.h"
#include "RecuperatorUnit_i.h"

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
    const double pi = 3.14159265359;
    const char* igas[2];
    const char* ogas[2];
    bool rv;
    Package p;
    string therm_path="thermo";
    int i;
    const char* result;
    summary_values summaries;

    igas[0] = executive_->GetImportData(id_, 0); //port 0 will be the gas input port;
    igas[1] = executive_->GetImportData(id_, 1); //port 0 will be the gas input port;

    if (string(igas[0])==""||string(igas[1])=="")
      {
	error("Missing input input.");
	return;
      }

    p.SetSysId("gas_in.xml");
        
    Gas *gas_in_data1 = new Gas;
    Gas *gas_in_data2 = new Gas;

    V21Helper gashelper(therm_path.c_str());
    p.Load(igas[0], strlen(igas[0])); 
    gashelper.IntToGas(&(p.intfs[0]), *gas_in_data1);
    p.Load(igas[1], strlen(igas[1])); 
    gashelper.IntToGas(&(p.intfs[0]), *gas_in_data2);

    // Check incoming
    if(gas_in_data1->gas_composite.T <= 200 || gas_in_data1->gas_composite.T >= 3000) {
      warning("Incoming gas temperature out of range.");
    }
    if(gas_in_data2->gas_composite.T <= 200 || gas_in_data2->gas_composite.T >= 3000) {
      warning("Incoming gas temperature out of range.");
    }

    // CALCULATE C's
    double cgas1 = gas_in_data1->gas_composite.cp_mix(gas_in_data1->gas_composite.T);
    double Cgas1 = gas_in_data1->gas_composite.M * cgas1;
    
    double cgas2 = gas_in_data2->gas_composite.cp_mix(gas_in_data2->gas_composite.T);
    double Cgas2 = gas_in_data2->gas_composite.M * cgas2;
    
    double Cmin, Cmax;
    if(Cgas1 < Cgas2) { Cmin = Cgas1; Cmax = Cgas2; }
    else              { Cmin = Cgas2; Cmax = Cgas1; }
    
    double C = Cmin / Cmax;
    
    // OVERALL C  VECTIVE HEAT TRANSFER COEFFICIENT

    // Molecular Weight of Gas.
    double gasWeight = gas_in_data1->gas_composite.mw();
    
    // AREA
    
    double hxA = pi * tube_od * tube_length * num_tubeL * num_tubeX;
    double xxA = tube_length * St * num_tubeX;
    
    // Tube Diameter Ratio.
    double tratio = tube_od / tube_id;
    
    // Heat Transfer Coefficient.
    
    double GoutT1 = gas_in_data1->gas_composite.T;
    double GoutT2 = gas_in_data2->gas_composite.T;
    double gout1 = GoutT1;
    double gout2 = GoutT2;

  // LOOP HERE
  
    do {
      GoutT1 = (0.9 * gout1) + (0.1 * GoutT1);
      GoutT2 = (0.9 * gout2) + (0.1 * GoutT2);    
      
      double temp = (gas_in_data1->gas_composite.T + GoutT1 + gas_in_data2->gas_composite.T + GoutT2) / 4;
      
      cgas1 = gas_in_data1->gas_composite.cp_mix(0.5*(GoutT1+gout1));
      Cgas1 = gas_in_data1->gas_composite.M * cgas1;
      cgas2 = gas_in_data2->gas_composite.cp_mix(0.5*(GoutT2+gout2)); // MIKE, do this?
      Cgas2 = gas_in_data2->gas_composite.M * cgas1;
      if(Cgas1 < Cgas2) { Cmin = Cgas1; Cmax = Cgas2; }
      else              { Cmin = Cgas2; Cmax = Cgas1; }
      
      double Rho = ((gas_in_data1->gas_composite.P - ext_press_drop / 2))
	/ (8315 * temp) * gasWeight;
      
      // Maximum Velocity.
      double velocity = gas_in_data1->gas_composite.M / (xxA * Rho); 
      double Sd = sqrt(pow(Sl, 2) + pow(St / 2, 2));
      double Vmax;
      if(tube_config=="Inline")
	Vmax = St / (St - tube_od) * velocity;
      else {
	if ((arrangement == "Parallel Flow") && (2 * (Sd - tube_od) < (St - tube_od)))
	  Vmax = St / (2 * (Sd - tube_od)) * velocity;
	else
	  Vmax = St/(St - tube_od) * velocity;
      }

    double k = gas_in_data1->gas_composite.thermal_conduct(temp); 
    double mu = gas_in_data1->gas_composite.Visc();
    double Pr = mu * gas_in_data1->gas_composite.cp_mix(temp) / k;
    double Re = Rho * Vmax * tube_od / mu; 
    double fa = arrFactor(Re);
    double Nu = 0.287 * pow(Re, 0.61) * pow(Pr, 0.33) * fa; // * fd ?
    double ho = Nu * k / tube_od; // External Heat Transfer Coefficient.
    
    // Fin Effect.
    if (use_fins) ho *= fin_effect;
    
    double FFO = 0; // fouling factor in
    double FFI = 0; // fouling factor out
    
    double U = 1 / ( 1 / ho + FFO + tratio * FFI );
    
    // FINISH CALCULATION
    
    double NTU = U * hxA / Cmin;
    
    double eff;

    // crossflow
    if(fabs(Cgas1-Cgas2)<1.0e-8){
      eff = (1 / C) * (1 - exp(-C * (1 - exp(-NTU))));
    }else{
      eff = 1 - exp(-(1 / C) * (1 - exp(-C * NTU)));
    }
    
    double Q = Cmin * eff * fabs(gas_in_data1->gas_composite.T - gas_in_data2->gas_composite.T);
    
    int s = (gas_in_data1->gas_composite.T > gas_in_data2->gas_composite.T)?-1:1;
  
    gout1 = gas_in_data1->gas_composite.T + s * Q / Cgas1;
    gout2 = gas_in_data2->gas_composite.T - s * Q / Cgas2;
  } while (fabs(GoutT1-gout1) > 1.0 || fabs(GoutT2-gout2) > 1.0);

    Gas *gas_out_data1 = new Gas;
    Gas *gas_out_data2 = new Gas;
    
    gas_out_data1->copy(*gas_in_data1);
    gas_out_data2->copy(*gas_in_data2);
    
    // FILL IN OUTGOING DATA
    
    gas_out_data1->gas_composite.T = GoutT1;
    gas_out_data1->gas_composite.P = gas_in_data1->gas_composite.P - ext_press_drop;
    gas_out_data1->pressure_drop += ext_press_drop;
    gas_out_data1->gas_composite.T_particle = gas_out_data1->gas_composite.T;
    
    gas_out_data2->gas_composite.T = GoutT2;
    gas_out_data2->gas_composite.P = gas_in_data2->gas_composite.P - int_press_drop;
    gas_out_data2->pressure_drop += int_press_drop;
    gas_out_data2->gas_composite.T_particle = gas_out_data2->gas_composite.T;
    
    p.intfs.resize(1); //each port has its own package
    p.SetPackName("ExportData");
    p.SetSysId("test.xml");
    
    gashelper.GasToInt(gas_out_data1, p.intfs[0]);
    ogas[0] = p.Save(rv);
    executive_->SetExportData(id_, 0, ogas[0]);
    
    gashelper.GasToInt(gas_out_data2, p.intfs[0]);
    ogas[1] = p.Save(rv);
    executive_->SetExportData(id_, 1, ogas[1]);

    if(gas_out_data1->gas_composite.T <= 200 || gas_out_data1->gas_composite.T >= 3000) {
      warning("Outgoing gas temperature out of range");
    }

    if(gas_out_data2->gas_composite.T <= 200 || gas_out_data2->gas_composite.T >= 3000) {
      warning("Outgoing gas temperature out of range.");
    }

    p.intfs.clear();
    result = p.Save(rv); 

    executive_->SetModuleResult(id_, result); //marks the end the execution
   
    if(gas_out_data1) delete gas_out_data1;
    if(gas_out_data2) delete gas_out_data2;
    if(gas_in_data1)  delete gas_in_data1;
    if(gas_in_data2)  delete gas_in_data2;
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
    Sl = p.intfs[0].getDouble("Sl");
    St = p.intfs[0].getDouble("St");
    tube_id = p.intfs[0].getDouble("tube_id");
    tube_od = p.intfs[0].getDouble("tube_od");
    tube_length = p.intfs[0].getDouble("tube_length");
    int_press_drop = p.intfs[0].getDouble("int_press_drop");
    ext_press_drop = p.intfs[0].getDouble("ext_press_drop");
    fin_effect = p.intfs[0].getDouble("fin_effect");
    num_tubeL = p.intfs[0].getInt("num_tubeL");
    num_tubeX = p.intfs[0].getInt("num_tubeX");
    use_fins = p.intfs[0].getInt("use_fins");
    arrangement = p.intfs[0].getString("arrangement");
    tube_config = p.intfs[0].getString("tube_config");
    
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
  msg = "Recuperator: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
  p.intfs.clear();
  result = p.Save(rv);
  return_state = 1;
  executive_->SetModuleResult(id_, result); //this marks the end the execution
}

void Body_Unit_i::warning (std::string msg)
{
  msg = "Recuperator: " + msg + "\n";
  executive_->SetModuleMessage(id_, msg.c_str());
}
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
