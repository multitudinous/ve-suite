// reks_solve.cpp: implementation of the reks_solve class.
//
//////////////////////////////////////////////////////////////////////

#include "reks_solve.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////


reks_solve::reks_solve()
{
	psrinterf = new ckinterf(10);
	output_time = 0;
}

reks_solve::~reks_solve()
{
	delete psrinterf;
}

bool reks_solve::solve(reks_container& reks, FILE* out_file)
{
	REKS_inp_reader& my_reader = reks.get_my_reader();
	REKS_sys& test_sys = reks.get_test_sys();

	output = out_file;

	rxn_time = my_reader.rxn_time;
	if(my_reader.get_case_type() == 6 || my_reader.get_case_type() == 7)        // dol
		rxn_time = my_reader.rxr_length;
	TMULT = my_reader.deltT;
	
	if (my_reader.get_case_type()==5)
	  my_reader.the_gas.temperature = my_reader.tinl;
	my_reader.the_gas.norm_molf();
	my_reader.the_gas.mean_mole_wgt_by_molef();
	my_reader.the_gas.density_by_pres();

	
	test_sys.Initialize();
	test_sys.set_gas_rxn(&(my_reader.the_gas));
	

#ifndef REDUCED	
	test_sys.rate_of_progress();
	test_sys.comp_rate_all();
#else
	test_sys.comp_rate_allRED(the_parser);
#endif
	fprintf(output, "Initial conditions: \n");
	test_sys.dump(output);
	//set up tolerance
	reltol = my_reader.rtol;   
	abstol = my_reader.atol;

	test_sys.rxr_Acs = my_reader.rxr_Acs;
	test_sys.rxr_length = my_reader.rxr_length;       // redundant since rxn_time = rxr_length for dist version
	test_sys.mass_flow_rate = my_reader.mass_flow_rate;
	test_sys.solver = this; // assign this to the reks_sys

#ifndef REDUCED
//	psrinterf->FillWorkArray(&my_parser);
//	psrinterf->WriteBinRecord(&my_parser, test_sys.gas_sys, 0.0);
#endif
	switch (my_reader.get_case_type())
	{
	case 0:	
		CONP(test_sys, TMULT, rxn_time);
		break;
	case 1:
		CONT(test_sys, TMULT, rxn_time);
		break;
	case 2:
		CONV(test_sys, TMULT, rxn_time);
		break;
	case 3:
		ttime_type = my_reader.ttime_type;
		if (ttime_type == 1)
		{
			time_temp_profile_fname = my_reader.time_temp_profile_fname;
			time_temps.clear();
			INIT_TIME_TEMP_PROFILE(time_temp_profile_fname.c_str(),strlen(time_temp_profile_fname.c_str()));
		}
		else
		{
			to_ttim = my_reader.to_ttim;
			slope_ttim = my_reader.slope_ttim;
			start_temp = my_reader.start_temp;
		}
			
		TTIM(test_sys, TMULT, rxn_time);
		break;
	case 4: //TGIV
		rxn_time = my_reader.tau;
		volume = my_reader.volume;
		if_spec_flrt = !(my_reader.is_spec_tau);
		flrt = my_reader.mass_flow_rate;
		TGIV(test_sys, TMULT, rxn_time);
		break;
	case 5: //ENGR
		rxn_time = my_reader.tau;
		volume = my_reader.volume;
		qloss = my_reader.qloss;
		if_spec_flrt = !my_reader.is_spec_tau;
		flrt = my_reader.mass_flow_rate;
		ENRG(test_sys, TMULT, rxn_time, qloss, volume, my_reader.start_temp);
		break;
	case 6:
		CONST_DIST(test_sys, TMULT, rxn_time);
		break;
	case 7:
		if(!CONSP_DIST(test_sys, TMULT, rxn_time)) {
			fclose(output);
			return false;
		}
		break;
	case 8:
		if( !CONSP_DIST_CONV(test_sys, TMULT, my_reader.fuelConvWant, my_reader.rxr_length) ) {
			fclose(output);
			return false;
		}
		break;
	case 9:
	  
	  time_temp_profile_fname = my_reader.time_temp_profile_fname;
	  time_temp_specs.clear();
	  INIT_TIME_TEMP_SPEC_PROFILE(time_temp_profile_fname.c_str());
	  ovrd_specs=my_reader.ovrd_specs;
	  TTIM_SPEC(test_sys, TMULT, rxn_time);
	  break;
	default:
		cout<<"Can't recognize the case type. Quit."<<endl;
		return false;
	}

#ifndef	REDUCED
//	psrinterf->WriteBinRecord(&my_parser, test_sys.gas_sys, rxn_time);
#endif
	cerr<<".............Done! "<<endl;
	fclose(output);
	return true;
}

/////////////////////////////////////////////////////////////////
void reks_solve::TGIV(REKS_sys &test_sys, double TMULT, double residence_time)
{

	real ropt[OPT_SIZE],  t;
	long int iopt[OPT_SIZE];
	N_Vector y;
	
	void *cvode_mem;
	int flag, i;
	
	// Here, assign the number of the equations y=dy*dt
	// Which is the number of the species

	NEQ = test_sys.gas_sys->specs.size();
	y = N_VNew(NEQ, NULL);       /* Allocate y */

	vector<double> y_star;    // store inlet  mass fractions (needed for PSR ode)
       
	// Here, initialize the original y and store for ODE calcs
	for (i=0; i<NEQ; i++){
	  N_VIth(y,i) = test_sys.gas_sys->specs[i].mass_fraction;
	  y_star.push_back(N_VIth(y,i));  // store inlet  mass fractions (needed for PSR ode)
	}

        // build a cluster of data to pass through cvode for access in callback
	bool steady_state = false;
    tgiv_cvode_data_cluster data;
    data.my_data = &test_sys;
    data.y_star  = &y_star;
    data.residence_time = &residence_time;
    data.steady_state = &steady_state;
	data.if_spec_flrt = if_spec_flrt;
	data.flrt = &flrt;   
	data.vol = &volume;
	// A pointer to CVODE problem memory is returned and stored in cvode_mem. 
	cvode_mem = CVodeMalloc(NEQ, f_tgiv, T0, y, BDF, NEWTON, SS, &reltol, &abstol,
		&data, NULL, FALSE, iopt, ropt, NULL);

	if (cvode_mem == NULL) { printf("CVodeMalloc failed.\n"); return ; }
	
	// Call CVDense to specify the CVODE dense linear solver with the
	// user-supplied Jacobian routine Jac. 	
	CVDense(cvode_mem, NULL, NULL);
	
        // now do integration using cvode in NORMAL mode
	int cntr = 0;
	bool error_flag = false;
	double tout = 0.0;

	while(!steady_state)
	{
	  cntr++;
	  if(cntr>1000000){printf("Max integration steps exceeded!\n"); error_flag = true; break;}
	  tout += TMULT;  // somewhat arbitrary - cvode just integrates in this chunk size 
                          // -  cvode still determines the actual time stepping (subdivides tmult)
          flag = CVode(cvode_mem, tout, y, &t, NORMAL);
	  if (flag != SUCCESS) { printf("CVode failed, flag=%d.\n", flag); error_flag = true; break; }
	}

        if(error_flag)printf("Solution to PSR NOT FOUND!!!\n");

        fprintf(output, "\n");
	fprintf(output,"--------------------------------\n");
	fprintf(output, "time = %.15g \n",t);
	test_sys.dump(output);
	fflush(NULL);
	
	N_VFree(y);                  /* Free the y and abstol vectors */
	CVodeFree(cvode_mem);        /* Free the CVODE problem memory */
	PrintFinalStats(iopt);       /* Print some final statistics   */          	
}

////////////////////////////////////////////////////////////////////
void reks_solve::ENRG(REKS_sys &test_sys, double TMULT, double residence_time, 
          double qloss, double vol, double start_temp)
{

	real ropt[OPT_SIZE], t;
	long int iopt[OPT_SIZE];
	N_Vector y;
	
	void *cvode_mem;
	int flag, i;
	
	// Here, assign the number of the equations
	// Which is the number of the species + 1 for the temperature
	NEQ = test_sys.gas_sys->specs.size();
	y = N_VNew(NEQ+1, NULL);       /* Allocate y, 1 for the temperature */
	
	vector<double> y_star;   // store inlet mass fractions (needed for PSR ode)
    vector<double> h_star;   // store inlet enthalpies (needed for PSR ode) 
       
	// Here, initialize the initial y and initial h and store for ODE calcs
	for (i=0; i<NEQ; i++){
	  N_VIth(y,i) = test_sys.gas_sys->specs[i].mass_fraction;
	  y_star.push_back(N_VIth(y,i));  // store inlet  mass fractions (needed for PSR ode)
	  h_star.push_back(test_sys.gas_sys->specs[i].enthalpy_mass_CKHMS());  // store inlet enthalpies (at TINL)
	}

        // build a cluster of data to pass through cvode for access in callback
	bool steady_state = false;
    enrg_cvode_data_cluster data;
    data.my_data = &test_sys;
    data.h_star  = &h_star;
	data.y_star  = &y_star;
    data.residence_time = &residence_time;
    data.vol = &vol;
	data.qloss = &qloss;
    data.steady_state = &steady_state;
	data.if_spec_flrt = if_spec_flrt;
	data.flrt = &flrt; 
	data.vol = &volume;
	
	N_VIth(y, NEQ) = start_temp;  // this is initail guess temp
	
	// A pointer to CVODE problem memory is returned and stored in cvode_mem. 
	
	cvode_mem = CVodeMalloc(NEQ+1, f_enrg, T0, y, BDF, NEWTON, SS, &reltol, &abstol,
		&data, NULL, FALSE, iopt, ropt, NULL);
	if (cvode_mem == NULL) { printf("CVodeMalloc failed.\n"); return ; }
	
	// Call CVDense to specify the CVODE dense linear solver with the
	//   user-supplied Jacobian routine Jac. 
	
	CVDense(cvode_mem, NULL, NULL);

        // now do integration using cvode in NORMAL mode
	int cntr = 0;
	bool error_flag = false;
	double tout = 0.0;
	
	while(!steady_state)
	{
	  cntr++;
	  if(cntr>1000000){printf("Max integration steps exceeded!\n"); error_flag = true; break;}
	  tout += TMULT;  // somewhat arbitrary - cvode just integrates in this chunk size 
                          // -  cvode still determines the actual time stepping (subdivides tmult)
          flag = CVode(cvode_mem, tout, y, &t, NORMAL);
	  if (flag != SUCCESS) { printf("CVode failed, flag=%d.\n", flag); error_flag = true; break; }
	}

        if(error_flag)printf("Solution to PSR NOT FOUND!!!\n");

        fprintf(output, "\n");
	fprintf(output,"--------------------------------\n");
	fprintf(output, "time = %.15g \n",t);
	test_sys.dump(output);
	fflush(NULL);
	
	N_VFree(y);                  /* Free the y and abstol vectors */
	CVodeFree(cvode_mem);        /* Free the CVODE problem memory */
	PrintFinalStats(iopt);       /* Print some final statistics   */          		
}

/////////////////////////////////////////////////////////////////
void reks_solve::CONT(REKS_sys &test_sys, double TMULT, double rxn_time)
{
	real ropt[OPT_SIZE],  t;
	long int iopt[OPT_SIZE];
	N_Vector y;
	
	void *cvode_mem;
	int flag, i;
	
	//Here, assign the number of the equations y=dy*dt
	//Which is the number of the species;
	NEQ = test_sys.gas_sys->specs.size();
	y = N_VNew(NEQ, NULL);       /* Allocate y */
	
	
	//Here, set up the orignal y
	/* Initialize y */
	for (i=0; i<NEQ; i++)
		N_VIth(y,i) = test_sys.gas_sys->specs[i].mass_fraction;
	
	// A pointer to CVODE problem memory is returned and stored in cvode_mem. 
	
	cvode_mem = CVodeMalloc(NEQ, f_cont, T0, y, BDF, NEWTON, SS, &reltol, &abstol,
		&test_sys, NULL, FALSE, iopt, ropt, NULL);
	if (cvode_mem == NULL) { printf("CVodeMalloc failed.\n"); return ; }
	
	// Call CVDense to specify the CVODE dense linear solver with the
	//   user-supplied Jacobian routine Jac. 
	
	CVDense(cvode_mem, NULL, NULL);
	
	//	for (tout=TMULT; tout<rxn_time; tout += TMULT) 
	//	{
	//flag = CVode(cvode_mem, rxn_time, y, &t, NORMAL);
	//fprintf(output, "\n");
	//fprintf(output,"--------------------------------\n");
	//fprintf(output, "time = %.15g \n",t);
	//test_sys.dump(output);
	//fflush(NULL);
	//if (flag != SUCCESS) { printf("CVode failed, flag=%d.\n", flag); break; }
	//}
	
	//tout = rxn_time;
	flag = CVode(cvode_mem, rxn_time, y, &t, NORMAL);
	fprintf(output,"--------------------------------\n");
	fprintf(output, "\n");
	fprintf(output, "time = %.15g \n", t);
	test_sys.dump(output);
	fflush(NULL);	
	
	N_VFree(y);                  /* Free the y and abstol vectors */
	
	CVodeFree(cvode_mem);        /* Free the CVODE problem memory */
	PrintFinalStats(iopt);       /* Print some final statistics   */
	
	
}
////////////////////////////////////////////////////////////////////
void reks_solve::CONST_DIST(REKS_sys &test_sys, double TMULT, double rxn_time)
{   // NOTE CHANGE OF VARIABLES BUT WITH SAME NAME:  TIME IS NOW DIST
	           // ADD AN EQUATION TO THE SET FOR TIME 
	real ropt[OPT_SIZE],  t;
	long int iopt[OPT_SIZE];
	N_Vector y;
	
	void *cvode_mem;
	int flag, i;
	
	//Here, assign the number of the equations y=dy*dt
	//Which is the number of the species;
	NEQ = test_sys.gas_sys->specs.size();
	y = N_VNew(NEQ+1, NULL);       /* Allocate y */
	
	
	//Here, set up the orignal y
	/* Initialize y */
	for (i=0; i<NEQ; i++)
		N_VIth(y,i) = test_sys.gas_sys->specs[i].mass_fraction;
	N_VIth(y,NEQ) = 0.0;            // time variable
	
	// A pointer to CVODE problem memory is returned and stored in cvode_mem. 
	
	cvode_mem = CVodeMalloc(NEQ+1, f_cont_dist, T0, y, BDF, NEWTON, SS, &reltol, &abstol,
		&test_sys, NULL, FALSE, iopt, ropt, NULL);
	if (cvode_mem == NULL) { printf("CVodeMalloc failed.\n"); return ; }
	
	// Call CVDense to specify the CVODE dense linear solver with the
	//   user-supplied Jacobian routine Jac. 

	cout << "CONST_DIST SPECIFICATION, REPLACE ALL TIME WITH DISTANCE IN CM\n\n"; 

	CVDense(cvode_mem, NULL, NULL);
	
	//	for (tout=TMULT; tout<rxn_time; tout += TMULT) 
	//	{
	//flag = CVode(cvode_mem, rxn_time, y, &t, NORMAL);
	//fprintf(output, "\n");
	//fprintf(output,"--------------------------------\n");
	//fprintf(output, "time = %.15g \n",t);
	//test_sys.dump(output);
	//fflush(NULL);
	//if (flag != SUCCESS) { printf("CVode failed, flag=%d.\n", flag); break; }
	//}
	
	//tout = rxn_time;
	flag = CVode(cvode_mem, rxn_time, y, &t, NORMAL);
	fprintf(output,"--------------------------------\n");
	fprintf(output, "\n");
	fprintf(output, "dist = %.15g \n", t);
	fprintf(output, "time = %.15g \n", N_VIth(y,NEQ));
	test_sys.dump(output);
	fflush(NULL);	
	
	N_VFree(y);                  /* Free the y and abstol vectors */
	
	CVodeFree(cvode_mem);        /* Free the CVODE problem memory */
	PrintFinalStats(iopt);       /* Print some final statistics   */
	
	
}
////////////////////////////////////////////////////////////////////
void reks_solve::CONP(REKS_sys &test_sys, double TMULT, double rxn_time)
{
	real ropt[OPT_SIZE], t;
	long int iopt[OPT_SIZE];
	N_Vector y;
	
	void *cvode_mem;
	int flag, i;
	
	//Here, assign the number of the equations y=Omega*dt
	//Which is the number of the species;
	NEQ = test_sys.gas_sys->specs.size();
	y = N_VNew(NEQ+1, NULL);       /* Allocate y, 1 for the temperature */
	
	
	//Here, set up the orignal y
	/* Initialize y */
	for (i=0; i<NEQ; i++)
		N_VIth(y,i) = test_sys.gas_sys->specs[i].mass_fraction; 
	N_VIth(y, NEQ) = test_sys.gas_sys->temperature;
	
	// A pointer to CVODE problem memory is returned and stored in cvode_mem. 
	
	cvode_mem = CVodeMalloc(NEQ+1, f_conp, T0, y, BDF, NEWTON, SS, &reltol, &abstol,
		&test_sys, NULL, FALSE, iopt, ropt, NULL);
	if (cvode_mem == NULL) { printf("CVodeMalloc failed.\n"); return ; }
	
	// Call CVDense to specify the CVODE dense linear solver with the
	//   user-supplied Jacobian routine Jac. 
	
	CVDense(cvode_mem, NULL, NULL);
	
	flag = CVode(cvode_mem, rxn_time, y, &t, NORMAL);
	fprintf(output,"--------------------------------\n");
	fprintf(output, "\n");
	fprintf(output, "time = %.15g \n", t);
	test_sys.dump(output);
	fflush(NULL);	
	
	N_VFree(y);                  /* Free the y and abstol vectors */
	
	CVodeFree(cvode_mem);        /* Free the CVODE problem memory */
	PrintFinalStats(iopt);       /* Print some final statistics   */
	
	
}
////////////////////////////////////////////////////////////////////  dol
bool reks_solve::CONSP_DIST(REKS_sys &test_sys, double TMULT, double rxn_time)
{// NOTE CHANGE OF VARIABLES BUT WITH SAME NAME:  TIME IS NOW DIST
	           // ADD AN EQUATION TO THE SET FOR TIME 
	real ropt[OPT_SIZE], t;
	long int iopt[OPT_SIZE];
	N_Vector y;
	
	void *cvode_mem;
	int flag, i;
	
	//Here, assign the number of the equations y=Omega*dt
	//Which is the number of the species;
	NEQ = test_sys.gas_sys->specs.size();
	y = N_VNew(NEQ+2, NULL);       /* Allocate y, 1 for the temperature */
	
	
	//Here, set up the orignal y
	/* Initialize y */
	for (i=0; i<NEQ; i++)
		N_VIth(y,i) = test_sys.gas_sys->specs[i].mass_fraction; 
	N_VIth(y, NEQ) = test_sys.gas_sys->temperature;
	N_VIth(y, NEQ+1) = 0.0;    //time
	
	// A pointer to CVODE problem memory is returned and stored in cvode_mem. 
	
	cvode_mem = CVodeMalloc(NEQ+2, f_conp_dist, T0, y, BDF, NEWTON, SS, &reltol, &abstol,
		&test_sys, NULL, FALSE, iopt, ropt, NULL);
	if (cvode_mem == NULL) { printf("CVodeMalloc failed.\n"); return false; }
	
	// Call CVDense to specify the CVODE dense linear solver with the
	//   user-supplied Jacobian routine Jac. 
	
	cout << "\nCONSP_DIST SPECIFICATION, REPLACE ALL TIME WITH DISTANCE IN CM\n\n"; 

	CVDense(cvode_mem, NULL, NULL);
	
	flag = CVode(cvode_mem, rxn_time, y, &t, NORMAL);
	fprintf(output,"--------------------------------\n");
	fprintf(output, "\n");
	fprintf(output, "dist= %.15g \n", t);
	fprintf(output, "time= %.15g \n", N_VIth(y,NEQ+1));
	test_sys.dump(output);
	fflush(NULL);	
	
	N_VFree(y);                  /* Free the y and abstol vectors */
	
	CVodeFree(cvode_mem);        /* Free the CVODE problem memory */
	PrintFinalStats(iopt);       /* Print some final statistics   */
	if(flag != SUCCESS) 
		return false;                                // DOL
	return true;
	
	
}
////////////////////////////////////////////////////////////////////  dol

bool reks_solve::CONSP_DIST_CONV(REKS_sys &test_sys, double TMULT,
								 double& fuelConvWant, double& rxr_length)  //dol
{   // NOTE CHANGE OF VARIABLES BUT WITH SAME NAME:  TIME IS NOW DIST
	// ADD AN EQUATION TO THE SET FOR TIME 
	// This version loops over CVODE till desired fuel conversion is achieved
	// This is similar to the PSR codes TGIV and ENRG
	
	// fuelConvWant is input and output: pass the desired value in and the result out
	// rxr_length is output:  set to zero below, then increased till hit fuelConvWant
	
	real ropt[OPT_SIZE], t;
	long int iopt[OPT_SIZE];
	N_Vector y;
	
	void *cvode_mem;
	int flag, i;
	
	//Here, assign the number of the equations y=Omega*dt
	//Which is the number of the species;
	NEQ = test_sys.gas_sys->specs.size();
	y = N_VNew(NEQ+2, NULL);       /* Allocate y, 1 for the temperature */
	
	
	//Here, set up the orignal y
	/* Initialize y */
	for (i=0; i<NEQ; i++)
		N_VIth(y,i) = test_sys.gas_sys->specs[i].mass_fraction; 
	N_VIth(y, NEQ) = test_sys.gas_sys->temperature;
	N_VIth(y, NEQ+1) = 0.0;    //time
	
	// A pointer to CVODE problem memory is returned and stored in cvode_mem. 
	
	cvode_mem = CVodeMalloc(NEQ+2, f_conp_dist, T0, y, BDF, NEWTON, SS, &reltol, &abstol,
		&test_sys, NULL, FALSE, iopt, ropt, NULL);
	if (cvode_mem == NULL) { printf("CVodeMalloc failed.\n"); return false; }
	
	// Call CVDense to specify the CVODE dense linear solver with the
	//   user-supplied Jacobian routine Jac. 
	
	//cout << "\nCONSP_DIST SPECIFICATION, REPLACE ALL TIME WITH DISTANCE IN CM\n\n"; 

	CVDense(cvode_mem, NULL, NULL);
	
	//--------------------------------------------------------
	double mw_init = test_sys.gas_sys->mean_mole_wgt_by_molef();
	double mw_curr = mw_init;
	double y_fuel_init = 0.0;
	double y_fuel_curr = 0.0;
	double conversion = 0.0;
	int    iterationLimit = 100000;
	int    cntr = 0;
	bool   fError = false;
	double L_Add  = 0.1; 

	rxr_length = 0.0;

	for(i=0; i < test_sys.gas_sys->specs.size(); i++) {
		string name = test_sys.gas_sys->specs[i].spec->m_spec_name;
		if(name == "CH4" || name == "H2" || name == "CO") 
			y_fuel_init += test_sys.gas_sys->specs[i].mole_fraction;
	}
	if(y_fuel_init <= 0.0) {
		cerr << "\nError, no fuel in kinetic solver gas";
		fError = true;
	}
	if(fuelConvWant < 0.0 || fuelConvWant > 1.0) {
		cerr << "\nError desired fuel conversion out of range (0-1) in kinetic solver";
		fError = true; 
	}

	while(conversion < fuelConvWant && !fError) {
		//cout << "\ncounter: " << cntr << " conversion: " << conversion;
		if(cntr++ > iterationLimit) {
			cerr << "\nError, desired conversion not reached in max iterations in kinetic solver";
			cerr << "\n\tRequired Residence time is > 100 seconds at inlet conditions";
			fError = true;
			break;
		}	
		rxr_length += L_Add;
		flag = CVode(cvode_mem, rxr_length, y, &t, NORMAL);
		if(flag != SUCCESS) {
				cerr << "\nError in kinetic solver";
				fError = true;
				break;
		}
		// Update conversion
		for(i=0, y_fuel_curr = 0.0; i < test_sys.gas_sys->specs.size(); i++) {
			string name = test_sys.gas_sys->specs[i].spec->m_spec_name;
			if(name == "CH4" || name == "H2" || name == "CO") 
				y_fuel_curr += test_sys.gas_sys->specs[i].mole_fraction;
		}
		mw_curr = test_sys.gas_sys->mean_mole_wgt_by_molef();
		conversion = 1.0 - y_fuel_curr / y_fuel_init * mw_init / mw_curr;
		fprintf(output,"--------------------------------\n");
		fprintf(output, "\n");
		fprintf(output, "dist= %.15g \n", t);
		fprintf(output, "time= %.15g \n", N_VIth(y,NEQ+1));
		test_sys.dump(output);
		fflush(NULL);	
	}        // end loop

	fuelConvWant = conversion;                                 // reassign for pass back

	//------------------------------------------------------------------


	
	N_VFree(y);                  /* Free the y and abstol vectors */
	
	CVodeFree(cvode_mem);        /* Free the CVODE problem memory */
	PrintFinalStats(iopt);       /* Print some final statistics   */
	if(flag != SUCCESS || fError) 
		return false;                                 // DOL
	return true;
	
}
////////////////////////////////////////////////////////////////
void reks_solve::CONV(REKS_sys &test_sys, double TMULT, double rxn_time)
{
	real ropt[OPT_SIZE], t;
	long int iopt[OPT_SIZE];
	N_Vector y;
	
	void *cvode_mem;
	int flag, i;
	
	//Here, assign the number of the equations y=Omega*dt
	//Which is the number of the species;
	NEQ = test_sys.gas_sys->specs.size();
	y = N_VNew(NEQ+1, NULL);       /* Allocate y, 1 for the temperature */
	
	
	//Here, set up the orignal y
	/* Initialize y */
	for (i=0; i<NEQ; i++)
		N_VIth(y,i) = test_sys.gas_sys->specs[i].mass_fraction; 
	N_VIth(y, NEQ) = test_sys.gas_sys->temperature;
	
	
	// A pointer to CVODE problem memory is returned and stored in cvode_mem. 
	
	cvode_mem = CVodeMalloc(NEQ+1, f_conv, T0, y, BDF, NEWTON, SS, &reltol, &abstol,
		&test_sys, NULL, FALSE, iopt, ropt, NULL);
	if (cvode_mem == NULL) { printf("CVodeMalloc failed.\n"); return ; }
	
	// Call CVDense to specify the CVODE dense linear solver with the
	//   user-supplied Jacobian routine Jac. 
	
	CVDense(cvode_mem, NULL, NULL);
	
	flag = CVode(cvode_mem, rxn_time, y, &t, NORMAL);
	fprintf(output,"--------------------------------\n");
	fprintf(output, "\n");
	fprintf(output, "time = %.15g \n", t);
	test_sys.dump(output);
	fflush(NULL);	
	
	N_VFree(y);                  /* Free the y and abstol vectors */
	
	CVodeFree(cvode_mem);        /* Free the CVODE problem memory */
	PrintFinalStats(iopt);       /* Print some final statistics   */
	
	
}

/////////////////////////////////////////////////////////////////
void reks_solve::TTIM(REKS_sys &test_sys, double TMULT, double rxn_time)
{
	real ropt[OPT_SIZE],  t;
	long int iopt[OPT_SIZE];
	N_Vector y;
	
	void *cvode_mem;
	int flag, i;
	double tout;
	//Here, assign the number of the equations y=dy*dt
	//Which is the number of the species;
	NEQ = test_sys.gas_sys->specs.size();
	y = N_VNew(NEQ, NULL);       /* Allocate y */
	
	
	//Here, set up the orignal y
	/* Initialize y */
	for (i=0; i<NEQ; i++)
		N_VIth(y,i) = test_sys.gas_sys->specs[i].mass_fraction;
	
	// A pointer to CVODE problem memory is returned and stored in cvode_mem. 
	
	cvode_mem = CVodeMalloc(NEQ, f_ttim, T0, y, BDF, NEWTON, SS, &reltol, &abstol,
		&test_sys, NULL, FALSE, iopt, ropt, NULL);
	if (cvode_mem == NULL) { printf("CVodeMalloc failed.\n"); return ; }
	
	// Call CVDense to specify the CVODE dense linear solver with the
	//   user-supplied Jacobian routine Jac. 
	
	CVDense(cvode_mem, NULL, NULL);
	
		for (tout=TMULT; tout<rxn_time; tout += TMULT) 
	//	{
	flag = CVode(cvode_mem, tout, y, &t, NORMAL);
	//fprintf(output, "\n");
	//fprintf(output,"--------------------------------\n");
	//fprintf(output, "time = %.15g \n",t);
	//test_sys.dump(output);
	//fflush(NULL);
	//if (flag != SUCCESS) { printf("CVode failed, flag=%d.\n", flag); break; }
	//}
	
	//tout = rxn_time;
	//flag = CVode(cvode_mem, rxn_time, y, &t, NORMAL);
	fprintf(output,"--------------------------------\n");
	fprintf(output, "\n");
	fprintf(output, "time = %.15g \n", t);
	test_sys.dump(output);
	fflush(NULL);	
	
	N_VFree(y);                  /* Free the y and abstol vectors */
	
	CVodeFree(cvode_mem);        /* Free the CVODE problem memory */
	PrintFinalStats(iopt);       /* Print some final statistics   */
	
	
}

/////////////////////////////////////////////////////////////////
void reks_solve::TTIM_SPEC(REKS_sys &test_sys, double TMULT, double rxn_time)
{
	real ropt[OPT_SIZE],  t;
	long int iopt[OPT_SIZE];
	N_Vector y;
	
	void *cvode_mem;
	int flag, i, j;
	double tout;
	//Here, assign the number of the equations y=dy*dt
	//Which is the number of the species;
	NEQ = test_sys.gas_sys->specs.size();
	//assign mol_fraction here for the raticles and normalize
	for (i=0; i<NEQ; i++)
	  for (j=0; j<ovrd_specs.size(); j++)
	    if ((test_sys.gas_sys->specs[i]).spec->m_spec_name==ovrd_specs[j]) //Get the value from the time profile
	      test_sys.gas_sys->specs[i].set_mole_fraction(GET_CUSTOM_VAL(0, j+2));
	
	test_sys.gas_sys->norm_molf();
	test_sys.gas_sys->mean_mole_wgt_by_molef();     

	y = N_VNew(NEQ, NULL);       /* Allocate y */
	
	
	//Here, set up the orignal y
	/* Initialize y */
	for (i=0; i<NEQ; i++)
	  N_VIth(y,i) = test_sys.gas_sys->specs[i].molef2massf_by_MMW();
	
	// A pointer to CVODE problem memory is returned and stored in cvode_mem. 
	
	cvode_mem = CVodeMalloc(NEQ, f_ttim_spec, T0, y, BDF, NEWTON, SS, &reltol, &abstol,
		&test_sys, NULL, FALSE, iopt, ropt, NULL);
	if (cvode_mem == NULL) { printf("CVodeMalloc failed.\n"); return ; }
	
	// Call CVDense to specify the CVODE dense linear solver with the
	//   user-supplied Jacobian routine Jac. 
	
	CVDense(cvode_mem, NULL, NULL);
	
		for (tout=TMULT; tout<rxn_time; tout += TMULT) 
	//	{
		  flag = CVode(cvode_mem, tout, y, &t, NORMAL);
	//fprintf(output, "\n");
	//fprintf(output,"--------------------------------\n");
	//fprintf(output, "time = %.15g \n",t);
	//test_sys.dump(output);
	//fflush(NULL);
	//if (flag != SUCCESS) { printf("CVode failed, flag=%d.\n", flag); break; }
	//}
	
	//tout = rxn_time;
	//flag = CVode(cvode_mem, rxn_time, y, &t, NORMAL);
	fprintf(output,"--------------------------------\n");
	fprintf(output, "\n");
	fprintf(output, "time = %.15g \n", t);
	test_sys.dump(output);
	fflush(NULL);	
	
	N_VFree(y);                  /* Free the y and abstol vectors */
	
	CVodeFree(cvode_mem);        /* Free the CVODE problem memory */
	PrintFinalStats(iopt);       /* Print some final statistics   */
	
	
}

double reks_solve::get_ttim_temp(const double& time)
{

	// a linear profile for example???
	// T = slope*time + intercept
	double result = start_temp;

	if(ttime_type==0){

	if (time < to_ttim)
		;
	else
		result = result + slope_ttim*(time - to_ttim);
	}
	else
		//result = interp(time, time_ttim, temp_ttim, size_ttim);
	{
		double t = time;
		GET_CUSTOM_TEMPERATURE(t, result);
	}
	return result;
//	return(slope*time+intercept);
}

void reks_solve::INIT_TIME_TEMP_PROFILE(const char* fname, unsigned int length)
{

    const int buf_size = 1000;
    char buf[buf_size];
    //Yang's debug
    int line_number=0;

    // fix up the fortran string
/*    fname[length-1]='\0';           // null terminate
    string fname_stl(fname);
    string::reverse_iterator ritr;
    for(ritr=fname_stl.rbegin();ritr!=fname_stl.rend();ritr++)
        if(*ritr!=' ')break;
*/
	
    string fname_stl(fname);

    // open the file
    ifstream ifs;
    ifs.open(fname_stl.c_str());

    if(ifs.rdstate()!=ios_base::goodbit){
        cout<<"ERROR: UNABLE TO OPEN CUSTOM PROFILE FILE: "<<fname_stl.c_str()<<endl;
	cout.flush();
        exit(EXIT_FAILURE);
    }

    // read and store data
    double time,temp;
    while(ifs.rdstate()==ios_base::goodbit){
        buf[0]='\0'; ifs.getline(buf,buf_size);
	line_number++;
        if(sscanf(buf,"%lf %lf",&time,&temp)==2){
            if((time_temps.size()>0)&&time<=(time_temps[time_temps.size()-1].first)){
                cout<<"ERROR: Time/Temp profile data not monotonic"<<endl;
		cout<<"line number "<<line_number<<endl;
		cout<<"The current time is "<<time<<endl;
		cout<<"The last time is "<<time_temps[time_temps.size()-1].first<<endl;
		// exit(EXIT_FAILURE); //Yang
		break; //Yang
            }
            else time_temps.push_back(pair<double,double>(time,temp));
        }
    };

    ifs.close();
}

void reks_solve::INIT_TIME_TEMP_SPEC_PROFILE(const char* fname)
{

  // const int buf_size = 1000;
    char buf[1000];
    //Yang's debug
    int line_number=0;
    std::vector<string> toks;
    std::vector<double> ttspecs;
    int num_toks, i;
    double dtemp;

    string fname_stl(fname);

    // open the file
    ifstream ifs;
    ifs.open(fname_stl.c_str());

    if(ifs.rdstate()!=ios_base::goodbit){
        cout<<"ERROR: UNABLE TO OPEN CUSTOM PROFILE FILE: "<<fname_stl.c_str()<<endl;
	cout.flush();
        exit(EXIT_FAILURE);
    }

    // read and store data
    double time,temp;
    while(ifs.rdstate()==ios_base::goodbit){
      
      //      cout<<"CP1"<<endl;
      //      cout.flush();
        buf[0]='\0'; 
	ifs.getline(buf,1000);
	line_number++;
	
	num_toks=get_token(buf, toks);
	ttspecs.clear();
	if (num_toks<2)
	  continue;
	for(i=0; i<num_toks; i++)
	  {
	    dtemp = atof(toks[i].c_str());
	    //	    cout<<dtemp<<endl;
	    //	    cout.flush();
	    ttspecs.push_back(dtemp);
	  }
	
        time = ttspecs[0];
	//	cout<<"The time is "<<time<<endl;
	//	cout.flush();

	if((time_temp_specs.size()>0)
	   &&(time<=((time_temp_specs[time_temp_specs.size()-1])[0])))
	{
	  cout<<"ERROR: Time/Temp profile data not monotonic"<<endl;
	  cout<<"line number "<<line_number<<endl;
	  cout<<"The current time is "<<time<<endl;
	  cout<<"The last time is "<<(time_temp_specs[time_temp_specs.size()-1])[0]<<endl;

	  break; //Yang
	}
	else time_temp_specs.push_back(ttspecs);
        
    };

    ifs.close();
}


//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
void reks_solve::GET_CUSTOM_TEMPERATURE(double& time, double& temp)
{
    static unsigned int table_index = time_temps.size()/2;
    get_table_index(time,table_index);

    if(((table_index == 0)&& (time<=time_temps[table_index].first)) || (table_index == (time_temps.size() - 1))) {
        temp = time_temps[table_index].second;
        return;
    }
    else {
        double frac = (time - time_temps[table_index].first) /
                      (time_temps[table_index+1].first - time_temps[table_index].first);
        double delta_t = frac * (time_temps[table_index+1].second - time_temps[table_index].second);

        temp = time_temps[table_index].second + delta_t;
    }
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
double reks_solve::GET_CUSTOM_VAL(double time, int index)
{
    static unsigned int table_index = time_temp_specs.size()/2;
    double val;
    get_vals_table_index(time,table_index);

    if(((table_index == 0)&& (time<=time_temp_specs[table_index][0])) || (table_index == (time_temp_specs.size() - 1))) {
        val = time_temp_specs[table_index][index];
        return val;
    }
    else {
        double frac = (time - time_temp_specs[table_index][0]) /
                      (time_temp_specs[table_index+1][0] - time_temp_specs[table_index][0]);
        double delta_t = frac * (time_temp_specs[table_index+1][index] 
				 - time_temp_specs[table_index][index]);

        val = time_temp_specs[table_index][index] + delta_t;
    }

    return val;
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
void reks_solve::get_table_index(double& time, unsigned int& index_low)
{
        
    int ascnd;
    unsigned int im,ihi,inc;
    unsigned int table_size = time_temps.size();
    
    // begin searching algorithm 
    ascnd=(time_temps[table_size-1].first>time_temps[0].first);  // true is ascending order 
    
    if(index_low < 0 || index_low > (table_size-1)){  // guess is out of range, so go to bisection 
        index_low = 0;
        ihi=table_size - 1;
    }
    else{
        inc=1;        // set the hunting increment 
        if(time >= time_temps[index_low].first == ascnd){     // hunt up 
            if(index_low == table_size - 1) return;
            ihi=index_low+1;
            while(time >= time_temps[ihi].first == ascnd){   // not done hunting 
                index_low=ihi;
                inc += inc;
                ihi=index_low+inc;
                if(ihi>table_size-1){              // done hunting since out of range 
                    ihi=table_size;
                    break;
                }
            }
        }
        else{                         // hunt down 
            if(index_low == 0){
                index_low=0;
                return;
            } 
            ihi=index_low--;
            while(time < time_temps[index_low].first == ascnd){
                ihi=(index_low);
                inc <<=1;
                if(inc >= ihi){
                    index_low=0;
                    break;
                }
                else index_low=ihi-inc;
            }
        }
    }
    
    // do the bisection 
    while(ihi-(index_low) != 1){
        im=(ihi+ index_low) >> 1;
        if(time > time_temps[im].first == ascnd)
            index_low=im;
        else
            ihi=im;
    }
    
    // now get the actual location based on index_low and ihi 
    if(ihi!=table_size){
        if(time >= time_temps[ihi].first)
            index_low=ihi;
    }    
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
void reks_solve::get_vals_table_index(double& time, unsigned int& index_low)
{
        
    int ascnd;
    unsigned int im,ihi,inc;
    unsigned int table_size = time_temp_specs.size();
    
    // begin searching algorithm 
    ascnd=(time_temp_specs[table_size-1][0]>time_temp_specs[0][0]);  // true is ascending order 
    
    if(index_low < 0 || index_low > (table_size-1)){  // guess is out of range, so go to bisection 
        index_low = 0;
        ihi=table_size - 1;
    }
    else{
        inc=1;        // set the hunting increment 
        if(time >= time_temp_specs[index_low][0] == ascnd){     // hunt up 
            if(index_low == table_size - 1) return;
            ihi=index_low+1;
            while(time >= time_temp_specs[ihi][0] == ascnd){   // not done hunting 
                index_low=ihi;
                inc += inc;
                ihi=index_low+inc;
                if(ihi>table_size-1){              // done hunting since out of range 
                    ihi=table_size;
                    break;
                }
            }
        }
        else{                         // hunt down 
            if(index_low == 0){
                index_low=0;
                return;
            } 
            ihi=index_low--;
            while(time < time_temp_specs[index_low][0] == ascnd){
                ihi=(index_low);
                inc <<=1;
                if(inc >= ihi){
                    index_low=0;
                    break;
                }
                else index_low=ihi-inc;
            }
        }
    }
    
    // do the bisection 
    while(ihi-(index_low) != 1){
        im=(ihi+ index_low) >> 1;
        if(time > time_temp_specs[im][0] == ascnd)
            index_low=im;
        else
            ihi=im;
    }
    
    // now get the actual location based on index_low and ihi 
    if(ihi!=table_size){
        if(time >= time_temp_specs[ihi][0])
            index_low=ihi;
    }    
}




///////////////////////////////////////////////////////
//////////////////////////////////////////////////////
/////////////// EXTERNAL FUNCTIONS ///////////////////
//////////////////////////////////////////////////////
//////////////////////////////////////////////////////


/************************ Private Helper Function ************************/

/* Print some final statistics located in the iopt array */

static void PrintFinalStats(long int iopt[])
{
	printf("\nFinal Statistics.. \n\n");
	printf("nst = %-6ld nfe  = %-6ld nsetups = %-6ld nje = %ld\n",
		iopt[NST], iopt[NFE], iopt[NSETUPS], iopt[DENSE_NJE]);
	printf("nni = %-6ld ncfn = %-6ld netf = %ld\n \n",
		iopt[NNI], iopt[NCFN], iopt[NETF]);
}


/*************************************************************************/
/***************** Functions Called by the CVODE Solver ******************/
/*************************************************************************/

/* f routine. Compute f(t,y). */
static void f_tgiv(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{
	int i;
	map<REKS_specie_thermo *, double>::iterator iter;

        // grab our data out of f_data cluster
    tgiv_cvode_data_cluster* data = (tgiv_cvode_data_cluster*)f_data;
	REKS_sys* my_data = data->my_data;
    vector<double>* y_star = data->y_star;
    double* residence_time = data->residence_time;
	reks_solve * solver = my_data->solver;

	if (data->if_spec_flrt)
		*residence_time = my_data->gas_sys->density*(*(data->vol))/(*(data->flrt));
	for (i=0; i<solver->NEQ; i++)
	{
		my_data->gas_sys->specs[i].clear_all_flags();
		my_data->gas_sys->specs[i].set_mass_fraction(N_VIth(y, i));
	}
	
	my_data->gas_sys->clear_all_flags();
	my_data->gas_sys->Fpressure=true;
	my_data->gas_sys->mean_mole_wgt_by_massf();
	my_data->gas_sys->density_by_pres();
	//	my_data->gas_sys->pressure_by_molar();
	my_data->update_sys();

#ifndef REDUCED
	my_data->rate_of_progress();
	my_data->comp_rate_all();
#else
	my_data->comp_rate_allRED(the_parser);
#endif
	// load ydot array
	for (i=0; i<solver->NEQ; i++)
	{
                // the governing ODE: dYk/dt = -1/tau*(Yk-Yk*)+omega*MW/rho

	        // masf_prodrate = v*omega*MW or omega*MW/rho
		iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
		N_VIth(ydot, i)=iter->second - 1.0/(*residence_time)*(N_VIth(y,i)-(*y_star)[i]);
	}

        // are we done with the Steady-state calculation?
        // we are at SS when dYk/dt==0

        double max=fabs(N_VIth(ydot,0));

        for(i=1;i<solver->NEQ;i++)
	    if(fabs(N_VIth(ydot,i))>max)max=fabs(N_VIth(ydot,i));

	if(max<steady_state_critera_species)*(data->steady_state)=true;
}

///////////////////////////////////////////////////////////////////////////
static void f_enrg(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{

	int i;
	map<REKS_specie_thermo *, double>::iterator iter;

    // grab our data out of f_data cluster
    enrg_cvode_data_cluster* data = (enrg_cvode_data_cluster*)f_data;
	REKS_sys* my_data = data->my_data;
    vector<double>* h_star = data->h_star;
    vector<double>* y_star = data->y_star;
    double* vol = data->vol;
    double* qloss = data->qloss;
    double* residence_time = data->residence_time;
	if (data->if_spec_flrt)
		*residence_time = my_data->gas_sys->density*(*(data->vol))/(*(data->flrt));
	reks_solve * solver = my_data->solver;
    
	// update reks species concentrations first with latest cvode info
	for (i=0; i<solver->NEQ; i++)
	{
		my_data->gas_sys->specs[i].clear_all_flags();
		my_data->gas_sys->specs[i].set_mass_fraction(N_VIth(y, i));
	}

	my_data->gas_sys->clear_all_flags();
	my_data->gas_sys->set_temperature(N_VIth(y, solver->NEQ));  // update reks with latest temp here
	//cout<<"Current Temperature: "<<N_VIth(y,solver->NEQ)<<"  dT/dt: "<<N_VIth(ydot,solver->NEQ)<<endl;
	my_data->gas_sys->mean_mole_wgt_by_massf();
	my_data->gas_sys->Fpressure=true;
	my_data->gas_sys->density_by_pres();
	
	my_data->update_sys();
#ifndef REDUCED
	my_data->rate_of_progress();
	my_data->comp_rate_all();
#else
	my_data->comp_rate_allRED(the_parser);
#endif
        // set new ydot for species
	for (i=0; i<solver->NEQ; i++)
	{
                // the governing ODE: dYk/dt = -1/tau*(Yk-Yk*)+omega*MW/rho

	        // masf_prodrate = v*omega*MW or omega*MW/rho
		iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
		N_VIth(ydot, i)=iter->second - 1.0/(*residence_time)*(N_VIth(y,i)-(*y_star)[i]);
	}

        // set new ydot for temperature
        // governing equation: dT/dt = -1/(tau*Cp)*sum(Y*(hk-hk*) - 1/Cp*sum(hk*omegak*MWk/rho) - Qdot/(rho*Vol*Cp)

        REKS_gas& gas = *(my_data->gas_sys);   // convenience

        // do first and second sums: sum(Yk(hk-hk*) and sum(hk*omegak*MWk/rho)
        double first_sum=0.0, second_sum=0.0;

	for(i=0;i<solver->NEQ;i++){
          iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
	  first_sum  += (*y_star)[i]*(gas.specs[i].enthalpy_mass_CKHMS()-(*h_star)[i]);
	  second_sum += gas.specs[i].enthalpy_mass_CKHMS()*(iter->second);
	}

        // find Cp for mix
	double cp = gas.mix_cp_mass_CKCPBS();

	// finally, build equation from parts
	N_VIth(ydot, solver->NEQ) = -1.0/((*residence_time)*cp)*first_sum - second_sum/cp 
                            - (*qloss)/(gas.density_by_pres()*(*vol)*cp);

        // are we done with the Steady-state calculation?
        // we are at SS when dYk/dt==0

        double max=fabs(N_VIth(ydot,0));

        for(i=1;i<solver->NEQ;i++) 
	  if(fabs(N_VIth(ydot,i))>max)max=fabs(N_VIth(ydot,i));

	if((max<steady_state_critera_species)&&(fabs(N_VIth(ydot,solver->NEQ))<steady_state_critera_temp))
	  *(data->steady_state)=true;   

        //cout<<"max(dY/dt): "<<max<<endl;    
}

///////////////////////////////////////////////////////////////////////////

static void f_cont(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{
	int i;
	REKS_sys* my_data;
	map<REKS_specie_thermo *, double>::iterator iter;
	
	my_data = (REKS_sys *) f_data;
	reks_solve * solver = my_data->solver;
	
	for (i=0; i<solver->NEQ; i++)
	{
		my_data->gas_sys->specs[i].clear_all_flags();
		my_data->gas_sys->specs[i].set_mass_fraction(N_VIth(y, i));
	}
	
	my_data->gas_sys->clear_all_flags();
	my_data->gas_sys->Fpressure=true;
	my_data->gas_sys->mean_mole_wgt_by_massf();
	my_data->gas_sys->density_by_pres();
	//	my_data->gas_sys->pressure_by_molar();
	my_data->update_sys();

#ifndef REDUCED
	my_data->rate_of_progress();
	my_data->comp_rate_all();
#else
	my_data->comp_rate_allRED(the_parser);
#endif

	for (i=0; i<solver->NEQ; i++)
	{
		iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
		N_VIth(ydot, i)=iter->second;
	}
	
	if (t >= solver->output_time&& t < solver->rxn_time)
	{
		fprintf(solver->output, "\n");
		fprintf(solver->output,"--------------------------------\n");
		fprintf(solver->output, "time = %.15g \n",t);
		my_data->dump(solver->output);
		fflush(NULL);	
		solver->output_time= t+solver->TMULT; 
	}
}

///////////////////////////////////////////////////////////////////////////  dol
static void f_cont_dist(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{              // NOTE CHANGE OF VARIABLES BUT WITH SAME NAME:  TIME IS NOW DIST
	           // ADD AN EQUATION TO THE SET FOR TIME 
	int i;
	REKS_sys* my_data;
	map<REKS_specie_thermo *, double>::iterator iter;

	
	my_data = (REKS_sys *) f_data;
	reks_solve * solver = my_data->solver;	
	
	for (i=0; i<solver->NEQ; i++)
	{
		my_data->gas_sys->specs[i].clear_all_flags();
		my_data->gas_sys->specs[i].set_mass_fraction(N_VIth(y, i));
	}
	
	my_data->gas_sys->clear_all_flags();
	my_data->gas_sys->Fpressure=true;
	my_data->gas_sys->mean_mole_wgt_by_massf();
	my_data->gas_sys->density_by_pres();
	//	my_data->gas_sys->pressure_by_molar();

	my_data->update_sys();
	
#ifndef REDUCED
	my_data->rate_of_progress();
	my_data->comp_rate_all();
#else
	my_data->comp_rate_allRED(the_parser);
#endif
	
	for (i=0; i<solver->NEQ; i++)
	{
		iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
		N_VIth(ydot, i)=iter->second /          // divide by velocity to covert time deriv to space deriv
			my_data->mass_flow_rate * my_data->gas_sys->density * my_data->rxr_Acs; 
	}

	N_VIth(ydot, solver->NEQ) = my_data->gas_sys->density * my_data->rxr_Acs / my_data->mass_flow_rate ;  // tres

	if (t >= solver->output_time&& t < solver->rxn_time)
	{
		fprintf(solver->output, "\n");
		fprintf(solver->output,"--------------------------------\n");
		fprintf(solver->output, "dist = %.15g \n",t);
		fprintf(solver->output, "time = %.15g \n",N_VIth(y,solver->NEQ));
		my_data->dump(solver->output);
		fflush(NULL);	
		solver->output_time= t+solver->TMULT; 
	}
}

///////////////////////////////////////////////////////////////////////////
static void f_conp(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{
	int i;
	REKS_sys* my_data;
	map<REKS_specie_thermo *, double>::iterator iter;
	
	my_data = (REKS_sys *) f_data;
	reks_solve * solver = my_data->solver;

	for (i=0; i<solver->NEQ; i++)
	{
		my_data->gas_sys->specs[i].clear_all_flags();
		my_data->gas_sys->specs[i].set_mass_fraction(N_VIth(y, i));
	}
	
	my_data->gas_sys->clear_all_flags();
	my_data->gas_sys->set_temperature(N_VIth(y, solver->NEQ));
	my_data->gas_sys->mean_mole_wgt_by_massf();
	my_data->gas_sys->Fpressure=true;
	my_data->gas_sys->density_by_pres();
	
	my_data->update_sys();
	
#ifndef REDUCED
	my_data->rate_of_progress();
	my_data->comp_rate_all();
#else
	my_data->comp_rate_allRED(the_parser);
#endif

	for (i=0; i<solver->NEQ; i++)
	{
		iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
		N_VIth(ydot, i)=iter->second;
	}

	

	N_VIth(ydot, solver->NEQ) = my_data->temp_rate();
	
	if (t >= solver->output_time&& t < solver->rxn_time)
	{
		fprintf(solver->output, "\n");
		fprintf(solver->output,"--------------------------------\n");
		fprintf(solver->output, "time = %.15g \n",t);
		my_data->dump(solver->output);
		fflush(NULL);	
		solver->output_time= t+solver->TMULT; 
	}
}

///////////////////////////////////////////////////////////////////////////     dol
static void f_conp_dist(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{				// NOTE CHANGE OF VARIABLES BUT WITH SAME NAME:  TIME IS NOW DIST
	           // ADD AN EQUATION TO THE SET FOR TIME 
	int i;
	REKS_sys* my_data;
	map<REKS_specie_thermo *, double>::iterator iter;
	
	my_data = (REKS_sys *) f_data;
	reks_solve * solver = my_data->solver;

	for (i=0; i<solver->NEQ; i++)
	{
		my_data->gas_sys->specs[i].clear_all_flags();
		my_data->gas_sys->specs[i].set_mass_fraction(N_VIth(y, i));
	}
	
	my_data->gas_sys->clear_all_flags();
	my_data->gas_sys->set_temperature(N_VIth(y, solver->NEQ));
	my_data->gas_sys->mean_mole_wgt_by_massf();
	my_data->gas_sys->Fpressure=true;
	my_data->gas_sys->density_by_pres();
	
	my_data->update_sys();

#ifndef REDUCED
	my_data->rate_of_progress();
	my_data->comp_rate_all();
#else
	my_data->comp_rate_allRED(the_parser);
#endif

	
	for (i=0; i<solver->NEQ; i++)
	{
		iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
		N_VIth(ydot, i)=iter->second  /              // divide by velocity to give spacial deriv from time deriv
			my_data->mass_flow_rate * my_data->gas_sys->density * my_data->rxr_Acs; 
	}
	N_VIth(ydot, solver->NEQ) = my_data->temp_rate() / 
		my_data->mass_flow_rate * my_data->gas_sys->density * my_data->rxr_Acs;
	N_VIth(ydot, solver->NEQ+1) = my_data->gas_sys->density * my_data->rxr_Acs / my_data->mass_flow_rate ; 
	
	if (t >= solver->output_time&& t < solver->rxn_time)
	{ 
		fprintf(solver->output, "\n");
		fprintf(solver->output,"--------------------------------\n");
		fprintf(solver->output, "dist = %.15g \n",t);
		fprintf(solver->output, "time = %.15g \n",N_VIth(y,solver->NEQ+1));
		my_data->dump(solver->output);
		fflush(NULL);	
		solver->output_time= t+solver->TMULT; 
	}
}

///////////////////////////////////////////////////////////////////////////
static void f_conv(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{
	int i;
	REKS_sys* my_data;
	map<REKS_specie_thermo *, double>::iterator iter;
	
	my_data = (REKS_sys *) f_data;
	reks_solve * solver = my_data->solver;	
	
	for (i=0; i<solver->NEQ; i++)
	{
		my_data->gas_sys->specs[i].clear_all_flags();
		my_data->gas_sys->specs[i].set_mass_fraction(N_VIth(y, i));
	}
	
	my_data->gas_sys->clear_all_flags();
	my_data->gas_sys->set_temperature(N_VIth(y, solver->NEQ));
	my_data->gas_sys->mean_mole_wgt_by_massf();
	my_data->gas_sys->Fdensity = true;
	my_data->gas_sys->set_pressure(my_data->gas_sys->density * R * my_data->gas_sys->temperature
								/my_data->gas_sys->mean_mole_wgt);
	
	my_data->update_sys();
#ifndef REDUCED
	my_data->rate_of_progress();
	my_data->comp_rate_all();
#else
	my_data->comp_rate_allRED(the_parser);
#endif
	
	for (i=0; i<solver->NEQ; i++)
	{
		iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
		N_VIth(ydot, i)=iter->second;
	}
	N_VIth(ydot, solver->NEQ) = my_data->temp_rate_v();
	
	if (t >= solver->output_time&& t < solver->rxn_time)
	{
		fprintf(solver->output, "\n");
		fprintf(solver->output,"--------------------------------\n");
		fprintf(solver->output, "time = %.15g \n",t);
		my_data->dump(solver->output);
		fflush(NULL);	
		solver->output_time= t+solver->TMULT; 
	}
}

//////////////////////////////////////////////////////////////////////////
static void f_ttim(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{
	int i;
	REKS_sys* my_data;
	map<REKS_specie_thermo *, double>::iterator iter;
	double check_temp;

	my_data = (REKS_sys *) f_data;
	reks_solve * solver = my_data->solver;	
	//	cout<<"Time "<<t<<endl;
	//	cout<<"N_VITH Y @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"<<endl;
	for (i=0; i<solver->NEQ; i++)
	{
		my_data->gas_sys->specs[i].clear_all_flags();
		check_temp = N_VIth(y,i);
                if (!finite(check_temp)||check_temp>1.1||check_temp<-1E-5)
		  check_temp = 0.0;
		my_data->gas_sys->specs[i].set_mass_fraction(check_temp);
		//		cout<<N_VIth(y, i)<<endl;;
	}
	
	my_data->gas_sys->clear_all_flags();
	my_data->gas_sys->set_temperature(solver->get_ttim_temp(t));
	my_data->gas_sys->Fpressure=true;
	my_data->gas_sys->mean_mole_wgt_by_massf();
	my_data->gas_sys->density_by_pres();
	//my_data->gas_sys->pressure_by_molar();
	my_data->update_sys();

#ifndef REDUCED
	my_data->rate_of_progress();
	my_data->comp_rate_all();
#else
	my_data->comp_rate_allRED(the_parser);
#endif
	
	//	cout<<"N_VITH YDOT **************************************************"<<endl;
	for (i=0; i<solver->NEQ; i++)
	{
		iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
		N_VIth(ydot, i)=iter->second;
		//		cout<<	N_VIth(ydot, i)<<endl;
	}
	
	if (t >= solver->output_time&& t < solver->rxn_time)
	{
		fprintf(solver->output, "\n");
		fprintf(solver->output,"--------------------------------\n");
		fprintf(solver->output, "time = %.15g \n",t);
		my_data->dump(solver->output);
		fflush(NULL);	
		solver->output_time= t+solver->TMULT; 
	}
}

//////////////////////////////////////////////////////////////////////////
static void f_ttim_spec(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{
	int i;
	REKS_sys* my_data;
	map<REKS_specie_thermo *, double>::iterator iter;
	double check_temp, cur_mol;
	vector<int> indexes;
	int j;
	
	my_data = (REKS_sys *) f_data;
	reks_solve * solver = my_data->solver;	
	//	cout<<"Time "<<t<<endl;
	//	cout<<"N_VITH Y @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"<<endl;
	indexes.clear();
	for (i=0; i<solver->NEQ; i++)
	{
		my_data->gas_sys->specs[i].clear_all_flags();

		check_temp = N_VIth(y,i);
                if (!finite(check_temp)||check_temp>1.1||check_temp<-1E-5)
		  check_temp = 0.0;
		my_data->gas_sys->specs[i].set_mass_fraction(check_temp);
	}

	my_data->gas_sys->clear_all_flags();
	my_data->gas_sys->set_temperature(solver->GET_CUSTOM_VAL(t, 1));
	my_data->gas_sys->Fpressure=true;
	my_data->gas_sys->mean_mole_wgt_by_massf();

	for (i=0; i<solver->NEQ; i++)
	  my_data->gas_sys->specs[i].massf2molef_by_MMW();

	for (i=0; i<solver->NEQ; i++)
	  for (j=0; j<solver->ovrd_specs.size(); j++)
	    if ((my_data->gas_sys->specs[i]).spec->m_spec_name==solver->ovrd_specs[j]) 
	      //Get the value from the time profile
	      {
		//cout<<"the mole fraction is "<<	my_data->gas_sys->specs[i].mole_fraction<<endl;
		cur_mol=solver->GET_CUSTOM_VAL(t, j+2);
		my_data->gas_sys->specs[i].set_mole_fraction(cur_mol);
		//
		indexes.push_back(i);
		break;
	      }
	
	my_data->gas_sys->norm_molf();
	my_data->gas_sys->mean_mole_wgt_by_molef();     

	my_data->gas_sys->density_by_pres();
	for (i=0; i<my_data->gas_sys->specs.size(); i++)
	{
		my_data->gas_sys->specs[i].molef2molarc_by_pres();
		my_data->gas_sys->specs[i].molef2massf_by_MMW();
	}

	for (i=0; i<my_data->m_p_reactions.size(); i++)
		(my_data->m_p_reactions[i])->update();



#ifndef REDUCED
	my_data->rate_of_progress();
	my_data->comp_rate_all();
#else
	my_data->comp_rate_allRED(the_parser);
#endif
	
	//	cout<<"N_VITH YDOT **************************************************"<<endl;
	for (i=0; i<solver->NEQ; i++)
	{
	  iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
	  N_VIth(ydot, i)=iter->second;
	  //  for (j=0; j<indexes.size(); j++)
	  //  if (i==j)
	  //   {
	  //	      N_VIth(ydot, i)=0; //set the rate to be zero
	  //	      
	  //	      break;
	  //    }
	  //		cout<<	N_VIth(ydot, i)<<endl;
	  N_VIth(y,i)=my_data->gas_sys->specs[i].mass_fraction;
	}
	
	if (t >= solver->output_time&& t < solver->rxn_time)
	{
		fprintf(solver->output, "\n");
		fprintf(solver->output,"--------------------------------\n");
		fprintf(solver->output, "time = %.15g \n",t);
		my_data->dump(solver->output);
		fflush(NULL);	
		solver->output_time= t+solver->TMULT; 
	}
}

