// reks_solve.cpp: implementation of the reks_solve class.
//
//////////////////////////////////////////////////////////////////////

#include "reks_solve.h"

#include <sys/times.h>
#include <set>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

//static REAL total_user=0;
//static REAL total_sys=0;

reks_solve::reks_solve()
{
	output_time = 0;
	IS_REDUCED = false;
	WRITE_BIN = false;
	edll=NULL;
}

reks_solve::~reks_solve()
{
  if (edll!=NULL)
    delete edll;

}

void reks_solve::init_RED(char* dll_fpath)
{ 
  edll = new expl_dll(dll_fpath);
  try {
    edll->init();
  }
  catch (loadlib_failed) {
    printf("SL NOT FOUND - FATAL ERROR\n");
    exit(-1);
  }
  catch (gpa_failed) {
        printf("FUNC NOT FOUND - FATAL ERROR\n");
        exit(-1);
  }
  set_reduced(true);
  return;
}
bool reks_solve::solve(reks_container& reks, FILE* out_file)
{
	REKS_inp_reader& my_reader = reks.get_my_reader();
	REKS_sys& test_sys = reks.get_test_sys();

	if (IS_REDUCED)
	  test_sys.set_red(edll);
	output = out_file;

	rxn_time = my_reader.rxn_time;
	if(my_reader.get_case_type() == 6 || my_reader.get_case_type() == 7)      // dol
	  rxn_time = my_reader.rxr_length;
	TMULT = my_reader.deltT;
	
	if (my_reader.get_case_type()==5)
		my_reader.the_gas.temperature = my_reader.tinl;
	my_reader.the_gas.norm_molf();
	my_reader.the_gas.mean_mole_wgt_by_molef();
	my_reader.the_gas.density_by_pres();

	
	test_sys.Initialize();
	test_sys.set_gas_rxn(&(my_reader.the_gas));
	

	if (!IS_REDUCED)
	  {
	    test_sys.rate_of_progress();
	    test_sys.comp_rate_all();
	  }
	else 
	  test_sys.comp_rate_allRED();
	
	fprintf(output, "Initial conditions: \n");
	test_sys.dump(output);
	//set up tolerance
	reltol = my_reader.rtol;   
	abstol = my_reader.atol;

	test_sys.rxr_Acs = my_reader.rxr_Acs;
	test_sys.rxr_length = my_reader.rxr_length;       // redundant since rxn_time = rxr_length for dist version
	test_sys.mass_flow_rate = my_reader.mass_flow_rate;
	test_sys.solver = this; // assign this to the reks_sys

	//	if (WRITE_BIN)
	//  {
	    //	    psrinterf->FillWorkArray(&test_sys);
	    //	    psrinterf->WriteBinRecord(&test_sys, 0.0);
	//}

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
		ENRG(test_sys, TMULT, rxn_time, qloss, volume, my_reader.temp);
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
	  radical_specs=my_reader.radical_specs;
	  T_SPEC_TIM(test_sys, TMULT, rxn_time);
	  break;
	case 10:
		if( !CONSP_DIST_CAT_COMB(test_sys, TMULT, rxn_time) ) {
			fclose(output);
			return false;
		}
		break;
	case 11:
		if( !CONSP_DIST_CONV_CAT_COMB(test_sys, TMULT, my_reader.fuelConvWant, my_reader.rxr_length) ) {
			fclose(output);
			return false;
		}
		break;
	case 12:
		if( !CONST_SURF(test_sys, TMULT, rxn_time) ) {
			fclose(output);
			return false;
		}
		break;
	default:
		cout<<"Can't recognize the case type. Quit."<<endl;
		return false;
	}

	//	if (WRITE_BIN)
	//  psrinterf->WriteBinRecord(&test_sys, rxn_time);
	
	cerr<<".............Done! "<<endl;
	fclose(output);
	return true;
}

/////////////////////////////////////////////////////////////////
void reks_solve::TGIV(REKS_sys &test_sys, REAL TMULT, REAL residence_time)
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

	vector<REAL> y_star;    // store inlet  mass fractions (needed for PSR ode)
       
	y_star.resize(NEQ);

	// Here, initialize the original y and store for ODE calcs
	for (i=0; i<NEQ; i++){
	  N_VIth(y,i) = test_sys.gas_sys->specs[i].mass_fraction;
	  y_star[i]=(N_VIth(y,i));  // store inlet  mass fractions (needed for PSR ode)
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
	real tout = 0.0;
	
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
void reks_solve::ENRG(REKS_sys &test_sys, REAL TMULT, REAL residence_time, 
          REAL qloss, REAL vol, REAL start_temp)
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
	
	vector<REAL> y_star;   // store inlet mass fractions (needed for PSR ode)
	vector<REAL> h_star;   // store inlet enthalpies (needed for PSR ode) 

	y_star.resize(NEQ);
	h_star.resize(NEQ);
	// Here, initialize the initial y and initial h and store for ODE calcs
	for (i=0; i<NEQ; i++){
	  N_VIth(y,i) = test_sys.gas_sys->specs[i].mass_fraction;
	  y_star[i]=(N_VIth(y,i));  // store inlet  mass fractions (needed for PSR ode)
	  h_star[i]=(test_sys.gas_sys->specs[i].enthalpy_mass_CKHMS());  // store inlet enthalpies (at TINL)
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
	real tout = 0.0;
	
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
void reks_solve::CONT(REKS_sys &test_sys, REAL TMULT, REAL rxn_time)
{
	real ropt[OPT_SIZE],  t;
	long int iopt[OPT_SIZE];
	N_Vector y;
	//	REAL tout;
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
	// flag = CVode(cvode_mem, tout, y, &t, NORMAL);
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
void reks_solve::CONST_DIST(REKS_sys &test_sys, REAL TMULT, REAL rxn_time)
{   // NOTE CHANGE OF VARIABLES BUT WITH SAME NAME:  TIME IS NOW DIST
	           // ADD AN EQUATION TO THE SET FOR TIME 
	real ropt[OPT_SIZE],  t;
	long int iopt[OPT_SIZE];
	N_Vector y;
	
	void *cvode_mem;
	int flag, i;
	//	REAL tout;
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
	//	  flag = CVode(cvode_mem, tout, y, &t, NORMAL);
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
void reks_solve::CONP(REKS_sys &test_sys, REAL TMULT, REAL rxn_time)
{
	real ropt[OPT_SIZE], t;
	long int iopt[OPT_SIZE];
	N_Vector y;
	
	void *cvode_mem;
	int flag, i;
	//	REAL tout;
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
	
	//	for (tout=TMULT; tout<rxn_time; tout += TMULT) 
	//	  flag = CVode(cvode_mem, rxn_time, y, &t, NORMAL);
	
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
bool reks_solve::CONSP_DIST(REKS_sys &test_sys, REAL TMULT, REAL rxn_time)
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

bool reks_solve::CONSP_DIST_CAT_COMB(REKS_sys &test_sys, double TMULT, double rxn_time)
{// NOTE CHANGE OF VARIABLES BUT WITH SAME NAME:  TIME IS NOW DIST
	           // ADD AN EQUATION TO THE SET FOR TIME 
	real ropt[OPT_SIZE], t, t1;
	long int iopt[OPT_SIZE];
	N_Vector y, y1;
	
	void *cvode_mem;
	void *cvode_mem1;
	int flag, i;
	int flag1;

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
	
	std::cout << "\nCONSP_DIST SPECIFICATION, REPLACE ALL TIME WITH DISTANCE IN CM\n\n"; 

	
	
// surface solution setup---------------------
	//Which is the number of the species;
    NEQ1 = test_sys.usrData->get_nSurfSp();
	y1 = N_VNew(NEQ1, NULL);       
	
	
	//Here, set up the orignal y
	/* Initialize y */
	for (i=0; i<NEQ1; i++)
		N_VIth(y1,i) = test_sys.usrData->get_molf(i); 
	
		
    // now do integration using cvode in NORMAL mode
	bool error_flag = false;
	double tout = 0.0;
	bool steady_state = false;
	test_sys.usrData->steady_state = &steady_state;
	
	
	cvode_mem = CVodeMalloc(NEQ+2, f_conp_dist_cat_comb, T0, y, BDF, NEWTON, SS, &reltol, &abstol,
		&test_sys, NULL, FALSE, iopt, ropt, NULL);
	if (cvode_mem == NULL) { printf("CVodeMalloc failed.\n"); return false; }
	
	// Call CVDense to specify the CVODE dense linear solver with the
	//   user-supplied Jacobian routine Jac. 
	CVDense(cvode_mem, NULL, NULL);
	
//--------------------------------------
	t = 0.0;
	t1=0.0;
	double tout1 = 0.0;
	t1 = 0.0;
	int cntr = 0;

	cvode_mem1 = CVodeMalloc(NEQ1, f_const_surf, T0, y1, BDF, NEWTON, SS, &reltol, &abstol,
			&test_sys, NULL, FALSE, iopt, ropt, NULL);
	if (cvode_mem1 == NULL) { printf("CVodeMalloc failed.\n"); return false; }

	
	CVDense(cvode_mem1, NULL, NULL);

	while(!steady_state)
	{
		cntr++;
		if(cntr>1000000){printf("Max integration steps exceeded!\n"); error_flag = true; break;}
		tout1 += TMULT*100;  // somewhat arbitrary - cvode just integrates in this chunk size 
		// -  cvode still determines the actual time stepping (subdivides tmult)
		flag1 = CVode(cvode_mem1, tout1, y1, &t1, NORMAL);
		if (flag1 != SUCCESS) { printf("Done cvode_mem1\n", flag1); error_flag = true; break; }
	}

	printf("Done cvode_mem1\n");
	steady_state = false;
	CVodeFree(cvode_mem1);

	tout =rxn_time;
	
	flag = CVode(cvode_mem, tout, y, &t, NORMAL);
	fflush(NULL);


        CVodeFree(cvode_mem);        /* Free the CVODE problem memory */

	fprintf(output,"--------------------------------\n");
	fprintf(output, "\n");
	fprintf(output, "dist= %.15g \n", t);
	fprintf(output, "time= %.15g \n", N_VIth(y,NEQ+1));
	test_sys.dump(output);
	fflush(NULL);	
	
	N_VFree(y);                  /* Free the y and abstol vectors */
	N_VFree(y1);
	
	PrintFinalStats(iopt);       /* Print some final statistics   */
	if(flag != SUCCESS) 
		return false;                                // DOL
	return true;
	
	
}

////////////////////////////////////////////////////////////////////  mkd

bool reks_solve::CONST_SURF(REKS_sys &test_sys, double TMULT, double rxn_time)
{// NOTE CHANGE OF VARIABLES BUT WITH SAME NAME:  TIME IS NOW DIST
	           // ADD AN EQUATION TO THE SET FOR TIME 
	real ropt[OPT_SIZE], t;
	long int iopt[OPT_SIZE];
	N_Vector y;
	
	void *cvode_mem;
	int flag, i;
	
	//Here, assign the number of the equations y=Omega*dt
	//Which is the number of the species;
    NEQ1 = test_sys.usrData->get_nSurfSp();
	y = N_VNew(NEQ1, NULL);       /* Allocate y, 1 for the temperature */
	
	
	//Here, set up the orignal y
	/* Initialize y */
	for (i=0; i<NEQ1; i++)
		N_VIth(y,i) = test_sys.usrData->get_molf(i); 
	
	
	// A pointer to CVODE problem memory is returned and stored in cvode_mem. 
	
	cvode_mem = CVodeMalloc(NEQ1, f_const_surf, T0, y, BDF, NEWTON, SS, &reltol, &abstol,
		&test_sys, NULL, FALSE, iopt, ropt, NULL);
	if (cvode_mem == NULL) { printf("CVodeMalloc failed.\n"); return false; }
	
	// Call CVDense to specify the CVODE dense linear solver with the
	//   user-supplied Jacobian routine Jac. 
	
	CVDense(cvode_mem, NULL, NULL);
	
        // now do integration using cvode in NORMAL mode
	int cntr = 0;
	bool error_flag = false;
	double tout = 0.0;
	bool steady_state = false;
	test_sys.usrData->steady_state = &steady_state;
	
	while(!steady_state)
	  {
	    cntr++;
	    if(cntr>1000000){printf("Max integration steps exceeded!\n"); error_flag = true; break;}
	    tout += TMULT;  // somewhat arbitrary - cvode just integrates in this chunk size 
	    // -  cvode still determines the actual time stepping (subdivides tmult)
	    flag = CVode(cvode_mem, tout, y, &t, NORMAL);
	    if (flag != SUCCESS) { printf("CVode failed, flag=%d.\n", flag); error_flag = true; break; }
	  }
	
        if(error_flag)printf("Solution to SURFACE NOT FOUND!!!\n");
	fprintf(output,"--------------------------------\n");
	fprintf(output, "\n");
	fprintf(output, "time= %.15g \n", t);
	test_sys.dump(output);
	fflush(NULL);	
	
	N_VFree(y);                  /* Free the y and abstol vectors */
	
	CVodeFree(cvode_mem);        /* Free the CVODE problem memory */
	PrintFinalStats(iopt);       /* Print some final statistics   */
    return !error_flag;	
	
}
////////////////////////////////////////////////////////////////////  dol

bool reks_solve::CONSP_DIST_CONV(REKS_sys &test_sys, REAL TMULT,
								 REAL& fuelConvWant, REAL& rxr_length)  //dol
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
	REAL mw_init = test_sys.gas_sys->mean_mole_wgt_by_molef();
	REAL mw_curr = mw_init;
	REAL y_fuel_init = 0.0;
	REAL y_fuel_curr = 0.0;
	REAL conversion = 0.0;
	int    iterationLimit = 100000;
	int    cntr = 0;
	bool   fError = false;
	REAL L_Add  = 0.1; 

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
void reks_solve::CONV(REKS_sys &test_sys, REAL TMULT, REAL rxn_time)
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
void reks_solve::TTIM(REKS_sys &test_sys, REAL TMULT, REAL rxn_time)
{
	real ropt[OPT_SIZE],  t;
	long int iopt[OPT_SIZE];
	N_Vector y;
	
	void *cvode_mem;
	int flag, i;
	//REAL tout;
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
	//	struct tms startbuf;
	//	struct tms stopbuf;

	//	times(&startbuf);
	CVDense(cvode_mem, NULL, NULL);
	
	//	for (tout=TMULT; tout<rxn_time; tout += TMULT) 
	//	  {
	//	    flag = CVode(cvode_mem, rxn_time, y, &t, NORMAL);
	//fprintf(output, "\n");
	//fprintf(output,"--------------------------------\n");
	//fprintf(output, "time = %.15g \n",t);
	//test_sys.dump(output);
	//fflush(NULL);
	//if (flag != SUCCESS) { printf("CVode failed, flag=%d.\n", flag); break; }
	//	  }
	
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

	//	times(&stopbuf);
	//	cout<<"Total cvode time user "
	//	    <<stopbuf.tms_utime-startbuf.tms_utime<<endl;
	//	cout<<"Total cvode time sys "
	//	    <<stopbuf.tms_stime-startbuf.tms_stime<<endl;
	//	cout<<"Total fttim routine time user "<<total_user<<endl;
	//	cout<<"Total fttim routine time sys "<<total_sys<<endl;
	
}

/////////////////////////////////////////////////////////////////
void reks_solve::T_SPEC_TIM(REKS_sys &test_sys, REAL TMULT, REAL rxn_time)
{
	real ropt[OPT_SIZE],  t;
	long int iopt[OPT_SIZE];
	N_Vector y;
	
	void *cvode_mem;
	int flag, i;
	REAL tout;
	//Here, assign the number of the equations y=dy*dt
	//Which is the number of the species;
	NEQ = test_sys.gas_sys->specs.size();
	y = N_VNew(NEQ, NULL);       /* Allocate y */
	
	
	//Here, set up the orignal y
	/* Initialize y */
	for (i=0; i<NEQ; i++)
		N_VIth(y,i) = test_sys.gas_sys->specs[i].mass_fraction;
	
	
	//	for (tout=TMULT; tout<rxn_time; tout += TMULT) 
	//	  {
	//	    flag = CVode(cvode_mem, rxn_time, y, &t, NORMAL);
	//fprintf(output, "\n");
	//fprintf(output,"--------------------------------\n");
	//fprintf(output, "time = %.15g \n",t);
	//test_sys.dump(output);
	//fflush(NULL);
	//if (flag != SUCCESS) { printf("CVode failed, flag=%d.\n", flag); break; }
	//	  }
	
	//tout = rxn_time;
	bool found = false;
	int whichmech = -1, isp_gone = -1;
	std::set<int> species_int;
	std::map<int,REAL> species_eta;
	for (i=0; i<NEQ; i++) found = (found||test_sys.gas_sys->specs[i].spec->m_spec_name=="POFMEGB");
	if(found) whichmech = 0;
	found = false;
	for (i=0; i<NEQ; i++){
	  found = (found||test_sys.gas_sys->specs[i].spec->m_spec_name=="LCCSCCL");
          if(test_sys.gas_sys->specs[i].spec->m_spec_name=="LCCSCCL") isp_gone = i;
	}
	if(found) whichmech = 1;
	if(whichmech==0){
	  std::string name;
	  int is;
	  for(is=0; is<NEQ; is++){
	    int num = test_sys.gas_sys->specs[is].spec->m_atom_form.size();
	    for(i=0; i<num; i++){
	      char str[3] = {'\0','\0','\0'};
	      for(int j=0; j<2; j++) str[j] = test_sys.gas_sys->specs[is].spec->m_atom_form[i].symbol[j];
	      if(str[1]==' ') str[1] = '\0';
	      name = str;
	      if(name=="P"){
		species_eta[is] = REAL(test_sys.gas_sys->specs[is].spec->m_atom_form[i].num)/
				   test_sys.gas_sys->specs[is].spec->spec_wgt; // kgmol_P/kg specie;
		break;
	      }
	    } // for(i
	    int ii, nrad = radical_specs.size();
	    for(ii=0; ii<nrad; ii++)
	      if(test_sys.gas_sys->specs[is].spec->m_spec_name==radical_specs[ii]) species_int.insert(is);
	  } // for(is
	}else if(whichmech==1){
	  std::string name;
	  int is;
	  for(is=0; is<NEQ; is++){
	    int num = test_sys.gas_sys->specs[is].spec->m_atom_form.size();
	    for(i=0; i<num; i++){
	      char str[3] = {'\0','\0','\0'};
	      for(int j=0; j<2; j++) str[j] = test_sys.gas_sys->specs[is].spec->m_atom_form[i].symbol[j];
	      if(str[1]==' ') str[1] = '\0';
	      name = str;
	      if(name=="S"){
		species_eta[is] = REAL(test_sys.gas_sys->specs[is].spec->m_atom_form[i].num)/
				   test_sys.gas_sys->specs[is].spec->spec_wgt; // kgmol_S/kg specie;
		break;
	      }
	    } // for(i
	    int ii, nrad = radical_specs.size();
	    for(ii=0; ii<nrad; ii++)
	      if(test_sys.gas_sys->specs[is].spec->m_spec_name==radical_specs[ii]) species_int.insert(is);
	  } // for(is
	} // if(whichmech
	int nsteps = 100;
	REAL drxn_time = rxn_time/REAL(nsteps);
	tout = 0.0;
	for(i=0; i<nsteps; i++){
	// A pointer to CVODE problem memory is returned and stored in cvode_mem. 
	
	  cvode_mem = CVodeMalloc(NEQ, f_t_spec_tim, tout, y, BDF, NEWTON, SS, &reltol, &abstol,
				  &test_sys, NULL, FALSE, iopt, ropt, NULL);
	  if (cvode_mem == NULL) { printf("CVodeMalloc failed.\n"); return ; }
	
	  CVDense(cvode_mem, NULL, NULL);

	  t = tout;
	  tout += drxn_time;
	  flag = CVode(cvode_mem, tout, y, &t, NORMAL);

	  // adjust composition for eta and interpolated species
	  std::set<int>::iterator it;
	  std::map<int,REAL>::iterator it0;
	  REAL mol_at = 0.0;
	  for(it0=species_eta.begin(); it0!=species_eta.end(); it0++) mol_at += it0->second*N_VIth(y,it0->first);
	  REAL ys = 0.0, yms = 0.0, xr = 0.0, xmr = 0.0, yo = 0.0, ymo = 0.0;
	  if(mol_at) for(it0=species_eta.begin(); it0!=species_eta.end(); it0++){
	    N_VIth(y,it0->first) *= get_custom_val(t,7)/mol_at;
	    ys += N_VIth(y,it0->first);
	    yms += N_VIth(y,it0->first)/test_sys.gas_sys->specs[it0->first].spec->spec_wgt;
	  }
	  int ii, nrad = radical_specs.size(), is;
	  for(is=0; is<NEQ; is++)
	    for(ii=0; ii<nrad; ii++)
	      if(test_sys.gas_sys->specs[is].spec->m_spec_name==radical_specs[ii]){
		xr += get_custom_val(t, ii+2);
		xmr += get_custom_val(t, ii+2)*test_sys.gas_sys->specs[is].spec->spec_wgt;
		//std::cout << radical_specs[ii] << " " << get_custom_val(t, ii+2) << std:: endl;
		break;
	      }
	  for(is=0; is<NEQ; is++){
	    it0 = species_eta.find(is); it = species_int.find(is);
	    if(it0==species_eta.end()&&it==species_int.end()){
	      yo += N_VIth(y,is);
	      ymo += N_VIth(y,is)/test_sys.gas_sys->specs[is].spec->spec_wgt;
	    }
	  }
	  REAL ccc = (1.0 - ys - yms*xmr/(1.0 - xr))/((ymo*xmr/(1.0 - xr) + yo));
	  REAL mwt = (1.0 - xr)/(yms + ccc*ymo);
	  for(is=0; is<NEQ; is++)
	    for(ii=0; ii<nrad; ii++)
	      if(test_sys.gas_sys->specs[is].spec->m_spec_name==radical_specs[ii]){
		N_VIth(y,is) = get_custom_val(t,ii+2)*test_sys.gas_sys->specs[is].spec->spec_wgt/mwt;
		//std::cout << radical_specs[ii] << " " << N_VIth(y,is) << std:: endl;
	      }
	  for(is=0; is<NEQ; is++){
	    it0 = species_eta.find(is); it = species_int.find(is);
	    if(it0==species_eta.end()&&it==species_int.end()){
	      N_VIth(y,is) *= ccc;
	    }
	  }
	  REAL sum = 0;
	  for(is=0; is<NEQ; is++) sum += N_VIth(y,is);
	  std::cout << i << " SUM " << sum << " ccc " << ccc << " mwt " << mwt << std::endl;      

	  CVodeFree(cvode_mem);        /* Free the CVODE problem memory */

	  //if(N_VIth(y,isp_gone)<1.66e-24&&i>0) break;

	} // for(i
	fprintf(output,"--------------------------------\n");
	fprintf(output, "\n");
	fprintf(output, "time = %.15g \n", t);
	test_sys.dump(output);
	fflush(NULL);	
	
	N_VFree(y);                  /* Free the y and abstol vectors */
	
	PrintFinalStats(iopt);       /* Print some final statistics   */

	//	times(&stopbuf);
	//	cout<<"Total cvode time user "
	//	    <<stopbuf.tms_utime-startbuf.tms_utime<<endl;
	//	cout<<"Total cvode time sys "
	//	    <<stopbuf.tms_stime-startbuf.tms_stime<<endl;
	//	cout<<"Total fttim routine time user "<<total_user<<endl;
	//	cout<<"Total fttim routine time sys "<<total_sys<<endl;
	
}

/////////////////////////////////////////////////////////////
REAL reks_solve::get_ttim_temp(const REAL& time)
{

	// a linear profile for example???
	// T = slope*time + intercept
	REAL result = start_temp;

	if(ttime_type==0){

	if (time < to_ttim)
		;
	else
		result = result + slope_ttim*(time - to_ttim);
	}
	else
		//result = interp(time, time_ttim, temp_ttim, size_ttim);
	{
		REAL t = time;
		GET_CUSTOM_TEMPERATURE(t, result);
	}
	return result;
//	return(slope*time+intercept);
}

void reks_solve::INIT_TIME_TEMP_PROFILE(const char* fname, unsigned int length)
{

    int buf_size = 1000;
    char buff[1000];
    //Yang's debug
    int line_number=0;
    REAL time,temp;

    // fix up the fortran string
/*    fname[length-1]='\0';           // null terminate
    string fname_stl(fname);
    string::reverse_iterator ritr;
    for(ritr=fname_stl.rbegin();ritr!=fname_stl.rend();ritr++)
        if(*ritr!=' ')break;
*/
	
    string fname_stl(fname);

    // open the file
    FILE* ifs;
    ifs=fopen(fname_stl.c_str(),"r");

    if(ifs==NULL){
        cout<<"ERROR: UNABLE TO OPEN CUSTOM PROFILE FILE: "<<fname_stl.c_str()<<endl;
	cout.flush();
        exit(EXIT_FAILURE);
    }

    // read and store data
   
    while((ifs!=NULL)&&(fgets(buff, buf_size, ifs)!=NULL)){
      line_number++;
      if(sscanf(buff,"%lf %lf",&time,&temp)==2){ //if REAL is double
      //if (sscanf(buff,"%f %f",&time,&temp)==2) {
     
	if((time_temps.size()>0)&&time<=(time_temps[time_temps.size()-1].first)){
	  if (time==(time_temps[time_temps.size()-1].first)) ; //so duplicates will not be inserted
	  else {
	    cout<<"ERROR: Time/Temp profile data not monotonic"<<endl;
	    cout<<"line number "<<line_number<<endl;
	    cout<<"The current time is "<<time<<endl;
	    cout<<"The last time is "<<time_temps[time_temps.size()-1].first<<endl;
	    // exit(EXIT_FAILURE); //Yang
	    break; //Yang
	  }
	}
	else time_temps.push_back(pair<REAL,REAL>(time,temp));
      }
    };

    fclose(ifs);
}
////////////////////////////////////////////////////////////////////
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
	    //cout<<dtemp<<endl;
	    //	    cout.flush();
	    ttspecs.push_back(dtemp);
	  }
	
        time = ttspecs[0];
	//	cout<<"The time is "<<time<<endl;
	//	cout.flush();

	if((time_temp_specs.size()>0)
	   &&(time<=((time_temp_specs[time_temp_specs.size()-1])[0])))
	{
	  if (time==(time_temp_specs[time_temp_specs.size()-1][0])) ;//so duplicates will not be inserted
	  else {
	    cout<<"ERROR: Time/Temp profile data not monotonic"<<endl;
	    cout<<"line number "<<line_number<<endl;
	    cout<<"The current time is "<<time<<endl;
	    cout<<"The last time is "<<(time_temp_specs[time_temp_specs.size()-1])[0]<<endl;
	    
	    break; //Yang
	  }
	}
	else time_temp_specs.push_back(ttspecs);
        
    };

    ifs.close();
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
void reks_solve::GET_CUSTOM_TEMPERATURE(REAL& time, REAL& temp)
{
    static unsigned int table_index = time_temps.size()/2;
    get_table_index(time,table_index);

    if(((table_index == 0)&& (time<=time_temps[table_index].first)) || (table_index == (time_temps.size() - 1))) {
        temp = time_temps[table_index].second;
        return;
    }
    else {
        REAL frac = (time - time_temps[table_index].first) /
                      (time_temps[table_index+1].first - time_temps[table_index].first);
        REAL delta_t = frac * (time_temps[table_index+1].second - time_temps[table_index].second);

        temp = time_temps[table_index].second + delta_t;
    }
}
//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
double reks_solve::get_custom_val(double time, int index)
{
    static unsigned int table_index = time_temp_specs.size()/2;
    double val;
    get_vals_table_index(time,table_index);

    if(((table_index == 0)&& (time<=time_temp_specs[table_index][0])) || (table_index == (time_temp_specs.size() - 1))) {
        val = time_temp_specs[table_index][index];
    }
    else {
      double frac = (time - time_temp_specs[table_index][0]) /
	(time_temp_specs[table_index+1][0] - time_temp_specs[table_index][0]);
      double delta_t = frac * (time_temp_specs[table_index+1][index] 
			       - time_temp_specs[table_index][index]);
      
      val = time_temp_specs[table_index][index] + delta_t;
    }
    //cout<<time<<"    "<<index<<"    "<<val<<endl;
    return val;
}
//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
void reks_solve::get_table_index(REAL& time, unsigned int& index_low)
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
////////////////////////////////////////////////////////////////////  dol

bool reks_solve::CONSP_DIST_CONV_CAT_COMB(REKS_sys &test_sys, double TMULT,
								 double& fuelConvWant, double& rxr_length)  //dol
{   // NOTE CHANGE OF VARIABLES BUT WITH SAME NAME:  TIME IS NOW DIST
	// ADD AN EQUATION TO THE SET FOR TIME 
	// This version loops over CVODE till desired fuel conversion is achieved
	// This is similar to the PSR codes TGIV and ENRG
	
	// fuelConvWant is input and output: pass the desired value in and the result out
	// rxr_length is output:  set to zero below, then increased till hit fuelConvWant
	
	real ropt[OPT_SIZE], t, t1;
	long int iopt[OPT_SIZE];
	N_Vector y, y1;
	
	void *cvode_mem, *cvode_mem1;
	int flag, i, flag1;
	
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
	
	
// surface solution setup---------------------
	//Which is the number of the species;
    NEQ1 = test_sys.usrData->get_nSurfSp();
	y1 = N_VNew(NEQ1, NULL);       
	
	
	//Here, set up the orignal y
	/* Initialize y */
	for (i=0; i<NEQ1; i++)
		N_VIth(y1,i) = test_sys.usrData->get_molf(i); 
	
	
	// A pointer to CVODE problem memory is returned and stored in cvode_mem. 
	
        // now do integration using cvode in NORMAL mode
	int cntr1 = 0;
	bool error_flag = false;
	double L_Add  = 0.1;    // cm
	double tout1 = 0.0, tout = 0.0;
	bool steady_state = false;
	test_sys.usrData->steady_state = &steady_state;
//--------------------------------------
	double mw_init = test_sys.gas_sys->mean_mole_wgt_by_molef();
	double mw_curr = mw_init;
	double y_fuel_init = 0.0;
	double y_fuel_curr = 0.0;
	double conversion = 0.0;
	int    iterationLimit = 1000;
	int    cntr = 0;
	bool   fError = false;

	rxr_length = 0.0;

	for(i=0; i < test_sys.gas_sys->specs.size(); i++) {
		std::string name = test_sys.gas_sys->specs[i].spec->m_spec_name;
		if(name == "CH4" || name == "H2" || name == "CO") 
			y_fuel_init += test_sys.gas_sys->specs[i].mass_fraction;
	}
	if(y_fuel_init <= 0.0) {
		std::cerr << "\nError, no fuel in kinetic solver gas";
		fError = true;
	}
	if(fuelConvWant < 0.0 || fuelConvWant > 1.0) {
		std::cerr << "\nError desired fuel conversion out of range (0-1) in kinetic solver";
		fError = true; 
	}
	t = 0.0;

			tout = L_Add;

			cntr1 = 0;

			// surface solution
	cvode_mem1 = CVodeMalloc(NEQ1, f_const_surf, T0, y1, BDF, NEWTON, SS, &reltol, &abstol,
		&test_sys, NULL, FALSE, iopt, ropt, NULL);
	if (cvode_mem1 == NULL) { printf("CVodeMalloc failed.\n"); return false; }
	
	// Call CVDense to specify the CVODE dense linear solver with the
	//   user-supplied Jacobian routine Jac. 
	
	CVDense(cvode_mem1, NULL, NULL);
	
	while(!steady_state)
	  {
	    cntr1++;
	    if(cntr1>1000000){printf("Max integration steps exceeded!\n"); error_flag = true; break;}
	    tout1 += TMULT*100.0;  // somewhat arbitrary - cvode just integrates in this chunk size 
				// -  cvode still determines the actual time stepping (subdivides tmult)
	    flag1 = CVode(cvode_mem1, tout1, y1, &t1, NORMAL);
	    if (flag1 != SUCCESS) { printf("CVode failed, flag=%d.\n", flag1); error_flag = true; break; }
	  }
	steady_state = false;
	
	if(error_flag)printf("Solution to SURFACE NOT FOUND!!!\n");

	CVodeFree(cvode_mem1);

	cout << "FUELConvWant " << fuelConvWant << endl;
	while(conversion < fuelConvWant && !fError) {
	  //cout << "\ncounter: " << cntr << " conversion: " << conversion;
	  if(cntr++ > iterationLimit) {
	    std::cerr << "\nError, desired conversion not reached in max iterations in kinetic solver";
	    std::cerr << "\n\tRequired channel length > 5 m";
	    fError = true;
	    break;
	  }	
	  rxr_length += L_Add;

     
      
			
	  cvode_mem = CVodeMalloc(NEQ+2, f_conp_dist_cat_comb, T0, y, BDF, NEWTON, SS, &reltol, &abstol,
				  &test_sys, NULL, FALSE, iopt, ropt, NULL);
	  if (cvode_mem == NULL) { printf("CVodeMalloc failed.\n"); return false; }
	
	  // Call CVDense to specify the CVODE dense linear solver with the
	  //   user-supplied Jacobian routine Jac. 
	
	  //cout << "\nCONSP_DIST SPECIFICATION, REPLACE ALL TIME WITH DISTANCE IN CM\n\n"; 

	  CVDense(cvode_mem, NULL, NULL);
	  //std::cout << " TIME " << t << std::endl;
			
	  flag = CVode(cvode_mem, tout, y, &t, NORMAL);

	  CVodeFree(cvode_mem);        /* Free the CVODE problem memory */


	  if(flag != SUCCESS) {
	    std::cerr << "\nError in kinetic solver";
	    fError = true;
	    break;
	  }
	  // Update conversion
	  for(i=0, y_fuel_curr = 0.0; i < test_sys.gas_sys->specs.size(); i++) {
	    std::string name = test_sys.gas_sys->specs[i].spec->m_spec_name;
	    if(name == "CH4" || name == "H2" || name == "CO") 
	      y_fuel_curr += test_sys.gas_sys->specs[i].mass_fraction;
	  }
	  mw_curr = test_sys.gas_sys->mean_mole_wgt_by_molef();
	  conversion = 1.0 - y_fuel_curr / y_fuel_init;
	  cout << "CONVERSION " << conversion << endl;
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
	N_VFree(y1);
	
	PrintFinalStats(iopt);       /* Print some final statistics   */
	if(flag != SUCCESS || fError) 
		return false;                                 // DOL
	return true;
	
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
  REAL check_temp;
  //map<REKS_specie_thermo *, REAL>::iterator iter;
  
  // grab our data out of f_data cluster
  tgiv_cvode_data_cluster* data = (tgiv_cvode_data_cluster*)f_data;
  REKS_sys* my_data = data->my_data;
  vector<REAL>* y_star = data->y_star;
  REAL* residence_time = data->residence_time;
  reks_solve * solver = my_data->solver;
  
  if (data->if_spec_flrt)
    *residence_time = my_data->gas_sys->density*(*(data->vol))/(*(data->flrt));
  for (i=0; i<solver->NEQ; i++)
    {
      my_data->gas_sys->specs[i].clear_all_flags();
      check_temp = N_VIth(y, i);
      if (!FINITE(check_temp)||check_temp > 1.1||check_temp<-1E-5)
	check_temp = 0;
      my_data->gas_sys->specs[i].set_mass_fraction(check_temp);
    }
	
  my_data->gas_sys->clear_all_flags();
  my_data->gas_sys->Fpressure=true;
  my_data->gas_sys->mean_mole_wgt_by_massf();
  my_data->gas_sys->density_by_pres();
  //	my_data->gas_sys->pressure_by_molar();
  my_data->update_sys();
  
  if (!my_data->solver->IS_REDUCED)
    {
      my_data->rate_of_progress();
      my_data->comp_rate_all();
    }
  else
    my_data->comp_rate_allRED();
  
  // load ydot array
  for (i=0; i<solver->NEQ; i++)
    {
      // the governing ODE: dYk/dt = -1/tau*(Yk-Yk*)+omega*MW/rho
      
      // masf_prodrate = v*omega*MW or omega*MW/rho
      //iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
      //N_VIth(ydot, i)=iter->second - 1.0/(*residence_time)*(N_VIth(y,i)-(*y_star)[i]);
      N_VIth(ydot, i)=my_data->masf_prodrate[i] - 1.0/(*residence_time)*(N_VIth(y,i)-(*y_star)[i]);
    }

  // are we done with the Steady-state calculation?
  // we are at SS when dYk/dt==0
  
  REAL max=fabs(N_VIth(ydot,0));
  
  for(i=1;i<solver->NEQ;i++)
    if(fabs(N_VIth(ydot,i))>max)max=fabs(N_VIth(ydot,i));

  if(max<steady_state_critera_species)*(data->steady_state)=true;
}

///////////////////////////////////////////////////////////////////////////
static void f_enrg(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{

  int i;
  REAL check_temp;
  //map<REKS_specie_thermo *, REAL>::iterator iter;
  
  // grab our data out of f_data cluster
  enrg_cvode_data_cluster* data = (enrg_cvode_data_cluster*)f_data;
  REKS_sys* my_data = data->my_data;
  vector<REAL>* h_star = data->h_star;
  vector<REAL>* y_star = data->y_star;
  REAL* vol = data->vol;
  REAL* qloss = data->qloss;
  REAL* residence_time = data->residence_time;
  if (data->if_spec_flrt)
    *residence_time = my_data->gas_sys->density*(*(data->vol))/(*(data->flrt));
  reks_solve * solver = my_data->solver;
  
  // update reks species concentrations first with latest cvode info
  for (i=0; i<solver->NEQ; i++)
    {
      my_data->gas_sys->specs[i].clear_all_flags();
      check_temp = N_VIth(y, i);
      //if (check_temp < 0)
      //  check_temp = 0;
      if (!FINITE(check_temp)||check_temp > 1.1||check_temp<-1E-5)
	check_temp = 0;
      my_data->gas_sys->specs[i].set_mass_fraction(check_temp);
      
    }

  my_data->gas_sys->clear_all_flags();
  my_data->gas_sys->set_temperature(N_VIth(y, solver->NEQ));  // update reks with latest temp here
  //cout<<"Current Temperature: "<<N_VIth(y,solver->NEQ)<<"  dT/dt: "<<N_VIth(ydot,solver->NEQ)<<endl;
  my_data->gas_sys->mean_mole_wgt_by_massf();
  my_data->gas_sys->Fpressure=true;
  my_data->gas_sys->density_by_pres();
	
  my_data->update_sys();
  if(!my_data->solver->IS_REDUCED)
    {
      my_data->rate_of_progress();
      my_data->comp_rate_all();
    }
  else
    my_data->comp_rate_allRED();
  
  // set new ydot for species
  for (i=0; i<solver->NEQ; i++)
    {
      // the governing ODE: dYk/dt = -1/tau*(Yk-Yk*)+omega*MW/rho
      
      // masf_prodrate = v*omega*MW or omega*MW/rho
      //iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
      //N_VIth(ydot, i)=iter->second - 1.0/(*residence_time)*(N_VIth(y,i)-(*y_star)[i]);
      N_VIth(ydot, i)=my_data->masf_prodrate[i] - 1.0/(*residence_time)*(N_VIth(y,i)-(*y_star)[i]);
    }

        // set new ydot for temperature
        // governing equation: dT/dt = -1/(tau*Cp)*sum(Y*(hk-hk*) - 1/Cp*sum(hk*omegak*MWk/rho) - Qdot/(rho*Vol*Cp)

        REKS_gas& gas = *(my_data->gas_sys);   // convenience

        // do first and second sums: sum(Yk(hk-hk*) and sum(hk*omegak*MWk/rho)
        REAL first_sum=0.0, second_sum=0.0;

	for(i=0;i<solver->NEQ;i++){
	  //iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
	  first_sum  += (*y_star)[i]*(gas.specs[i].enthalpy_mass_CKHMS()-(*h_star)[i]);
	   //second_sum += (gas.specs[i].enthalpy_mass_CHMS()) * (iter->second);
	  second_sum += gas.specs[i].enthalpy_mass_CKHMS()*my_data->masf_prodrate[i];
	}

        // find Cp for mix
	REAL cp = gas.mix_cp_mass_CKCPBS();

	// finally, build equation from parts
	N_VIth(ydot, solver->NEQ) = -1.0/((*residence_time)*cp)*first_sum - second_sum/cp 
                            - (*qloss)/(gas.density_by_pres()*(*vol)*cp);

        // are we done with the Steady-state calculation?
        // we are at SS when dYk/dt==0

        REAL max=fabs(N_VIth(ydot,0));

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
	REAL check_temp;
	REKS_sys* my_data;
	//map<REKS_specie_thermo *, REAL>::iterator iter;
	
	my_data = (REKS_sys *) f_data;
	reks_solve * solver = my_data->solver;
	
	for (i=0; i<solver->NEQ; i++)
	{
		my_data->gas_sys->specs[i].clear_all_flags();
		check_temp = N_VIth(y, i);
		//		if (check_temp < 0)
		//		  check_temp = 0;
		if (!FINITE(check_temp)||check_temp > 1.1||check_temp<-1E-5)
		  check_temp = 0;
		my_data->gas_sys->specs[i].set_mass_fraction(check_temp);
	}
	
	my_data->gas_sys->clear_all_flags();
	my_data->gas_sys->Fpressure=true;
	my_data->gas_sys->mean_mole_wgt_by_massf();
	my_data->gas_sys->density_by_pres();
	//	my_data->gas_sys->pressure_by_molar();
	my_data->update_sys();
	
	if (!my_data->solver->IS_REDUCED)
	  {
	    my_data->rate_of_progress();
	    my_data->comp_rate_all();
	  }
	else
	  my_data->comp_rate_allRED();


	for (i=0; i<solver->NEQ; i++)
	{
	  //iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
	  //N_VIth(ydot, i)=iter->second;
	  N_VIth(ydot, i) = my_data->masf_prodrate[i];
	}
	
	if (t >= solver->output_time&& t < solver->rxn_time)
	{
		fprintf(solver->output, "\n");
		fprintf(solver->output,"--------------------------------\n");
		fprintf(solver->output, "time = %.15g \n",t);
		my_data->dump(solver->output);
		//	fflush(NULL);	
		solver->output_time= t+solver->TMULT; 
	}
}

///////////////////////////////////////////////////////////////////////////  dol
static void f_cont_dist(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{              // NOTE CHANGE OF VARIABLES BUT WITH SAME NAME:  TIME IS NOW DIST
	           // ADD AN EQUATION TO THE SET FOR TIME 
	int i;
	REAL check_temp;
	REKS_sys* my_data;
	//map<REKS_specie_thermo *, REAL>::iterator iter;

	
	my_data = (REKS_sys *) f_data;
	reks_solve * solver = my_data->solver;	
	
	for (i=0; i<solver->NEQ; i++)
	{
		my_data->gas_sys->specs[i].clear_all_flags();
		check_temp = N_VIth(y, i);
		//		if (check_temp < 0)
		//		  check_temp = 0;
		if (!FINITE(check_temp)||check_temp > 1.1||check_temp<-1E-5)
		  check_temp = 0;
		my_data->gas_sys->specs[i].set_mass_fraction(check_temp);
	}
	
	my_data->gas_sys->clear_all_flags();
	my_data->gas_sys->Fpressure=true;
	my_data->gas_sys->mean_mole_wgt_by_massf();
	my_data->gas_sys->density_by_pres();
	//	my_data->gas_sys->pressure_by_molar();

	my_data->update_sys();
	
	if (!my_data->solver->IS_REDUCED)
	  {
	    my_data->rate_of_progress();
	    my_data->comp_rate_all();
	  }
	else
	  my_data->comp_rate_allRED();

	
	for (i=0; i<solver->NEQ; i++)
	{
	  //iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
	  //N_VIth(ydot, i)=iter->second /          // divide by velocity to covert time deriv to space deriv
	  //		my_data->mass_flow_rate * my_data->gas_sys->density * my_data->rxr_Acs; 
	  N_VIth(ydot, i)=my_data->masf_prodrate[i] /          // divide by velocity to covert time deriv to space deriv
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
		//		fflush(NULL);	
		solver->output_time= t+solver->TMULT; 
	}
}

///////////////////////////////////////////////////////////////////////////
static void f_conp(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{
	int i;
	REAL check_temp;
	REKS_sys* my_data;
	//map<REKS_specie_thermo *, REAL>::iterator iter;
	
	my_data = (REKS_sys *) f_data;
	reks_solve * solver = my_data->solver;

	for (i=0; i<solver->NEQ; i++)
	{
		my_data->gas_sys->specs[i].clear_all_flags();
		check_temp = N_VIth(y, i);
		//		if (check_temp < 0)
		//		  check_temp = 0;
		if (!FINITE(check_temp)||check_temp > 1.1||check_temp<-1E-5)
		  check_temp = 0;
		my_data->gas_sys->specs[i].set_mass_fraction(check_temp);
	}
	
	my_data->gas_sys->clear_all_flags();
	my_data->gas_sys->set_temperature(N_VIth(y, solver->NEQ));
	my_data->gas_sys->mean_mole_wgt_by_massf();
	my_data->gas_sys->Fpressure=true;
	my_data->gas_sys->density_by_pres();
	
	my_data->update_sys();
	
	if (!my_data->solver->IS_REDUCED)
	  {
	    my_data->rate_of_progress();
	    my_data->comp_rate_all();
	  }
	else
	  my_data->comp_rate_allRED();

	for (i=0; i<solver->NEQ; i++)
	{
	  //iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
	  //N_VIth(ydot, i)=iter->second;
	  N_VIth(ydot, i)=my_data->masf_prodrate[i];
	}

	

	N_VIth(ydot, solver->NEQ) = my_data->temp_rate();
	
	if (t >= solver->output_time&& t < solver->rxn_time)
	{
		fprintf(solver->output, "\n");
		fprintf(solver->output,"--------------------------------\n");
		fprintf(solver->output, "time = %.15g \n",t);
		my_data->dump(solver->output);
		//		fflush(NULL);	
		solver->output_time= t+solver->TMULT; 
	}
}

///////////////////////////////////////////////////////////////////////////     dol
static void f_conp_dist(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{			
  // NOTE CHANGE OF VARIABLES BUT WITH SAME NAME:  TIME IS NOW DIST
  // ADD AN EQUATION TO THE SET FOR TIME 
	int i;
	REAL check_temp;
	REKS_sys* my_data;
	//map<REKS_specie_thermo *, REAL>::iterator iter;
	
	my_data = (REKS_sys *) f_data;
	reks_solve * solver = my_data->solver;

	for (i=0; i<solver->NEQ; i++)
	{
		my_data->gas_sys->specs[i].clear_all_flags();
		check_temp = N_VIth(y, i);
		//		if (check_temp < 0)
		//		  check_temp = 0;
		if (!FINITE(check_temp)||check_temp > 1.1||check_temp<-1E-5)
		  check_temp = 0;
		my_data->gas_sys->specs[i].set_mass_fraction(check_temp);
	}
	
	my_data->gas_sys->clear_all_flags();
	my_data->gas_sys->set_temperature(N_VIth(y, solver->NEQ));
	my_data->gas_sys->mean_mole_wgt_by_massf();
	my_data->gas_sys->Fpressure=true;
	my_data->gas_sys->density_by_pres();
	
	my_data->update_sys();

	if (!my_data->solver->IS_REDUCED)
	  {
	    my_data->rate_of_progress();
	    my_data->comp_rate_all();
	  }
	else
	  my_data->comp_rate_allRED();

	
	for (i=0; i<solver->NEQ; i++)
	{
	  //iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
	  //N_VIth(ydot, i)=iter->second  /              // divide by velocity to give spacial deriv from time deriv
	  //my_data->mass_flow_rate * my_data->gas_sys->density * my_data->rxr_Acs; 
	  N_VIth(ydot, i)=my_data->masf_prodrate[i]  /   // divide by velocity to give spacial deriv from time deriv
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
		//		fflush(NULL);	
		solver->output_time= t+solver->TMULT; 
	}
}

///////////////////////////////////////////////////////////////////////////     dol

static void f_conp_dist_cat_comb(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{				// NOTE CHANGE OF VARIABLES BUT WITH SAME NAME:  TIME IS NOW DIST
	           // ADD AN EQUATION TO THE SET FOR TIME 
	int i;
	double check_temp;
	REKS_sys* my_data;
	void *cvode_mem1;
	double rtol = 1E-7;
	double atol = 1E-10;
	bool error_flag = false;
	int flag1;

	real ropt[OPT_SIZE];
	long int iopt[OPT_SIZE];

	my_data = (REKS_sys *) f_data;



	reks_solve * solver = my_data->solver;

	for (i=0; i<solver->NEQ; i++)
	{
		my_data->gas_sys->specs[i].clear_all_flags();
		check_temp = N_VIth(y, i);
		//		if (check_temp < 0)
		//		  check_temp = 0;
		if (!FINITE(check_temp)||check_temp > 1.1||check_temp<0.0)
		  check_temp = 0;
		my_data->gas_sys->specs[i].set_mass_fraction(check_temp);
	}
	
	my_data->gas_sys->clear_all_flags();
	my_data->gas_sys->set_temperature(N_VIth(y, solver->NEQ));
	my_data->gas_sys->mean_mole_wgt_by_massf();
	my_data->gas_sys->Fpressure=true;
	my_data->gas_sys->density_by_pres();
	
	my_data->update_sys();

	if (!my_data->solver->IS_REDUCED)
	  {
	    my_data->rate_of_progress();
	    my_data->comp_rate_all();
	  }
	else
	  my_data->comp_rate_allRED();


	for (i=0; i<solver->NEQ; i++)
	{
		N_VIth(ydot, i)=my_data->masf_prodrate[i]  /              // divide by velocity to give spacial deriv from time deriv
			my_data->mass_flow_rate * my_data->gas_sys->density * my_data->rxr_Acs; 
	}

	N_VIth(ydot, solver->NEQ) = my_data->temp_rate() /
		my_data->mass_flow_rate * my_data->gas_sys->density * my_data->rxr_Acs;
	//cout << "tempdot " << N_VIth(ydot, solver->NEQ) << endl;
	N_VIth(ydot, solver->NEQ+1) = my_data->gas_sys->density * my_data->rxr_Acs / my_data->mass_flow_rate ; 


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Surface Chemistry Sources 
	// 1-D Homogeneous assumption (bulk = surf) 

	
//	 get sources
	my_data->usrData->position = t/100.;            // meters, t is cm position
	my_data->usrData->bulkTemp = my_data->gas_sys->temperature;
	my_data->usrData->surfTemp = my_data->gas_sys->temperature;
	my_data->gas_sys->molef2massf();
	for(i=0; i<my_data->usrData->nGasSp; i++){
		my_data->usrData->bulkMolf[i] = 
		  my_data->gas_sys->specs[my_data->usrData->surfSp2reksSp[i]].mole_fraction;
		  my_data->usrData->molf[i] = my_data->usrData->bulkMolf[i];
	}
	
	N_Vector y1;
	y1 = N_VNew(my_data->solver->NEQ1,NULL);

	for (i=0; i<my_data->solver->NEQ1; i++)
	  N_VIth(y1,i) = my_data->usrData->get_molf(i); 

	double tout1 = 0.0;
	double t1 = 0.0;
	int cntr = 0;
	cvode_mem1 = CVodeMalloc(my_data->solver->NEQ1, f_const_surf, T0, y1, BDF, NEWTON, SS, &rtol, &atol,
				 my_data, NULL, FALSE, iopt, ropt, NULL);
	if (cvode_mem1 == NULL) { printf("CVodeMalloc failed.\n"); return; }

	
	CVDense(cvode_mem1, NULL, NULL);
	
	while(!*(my_data->usrData->steady_state))
	  {
	    cntr++;
	    if(cntr>1000000){printf("Max integration steps exceeded!\n"); error_flag = true; break;}
	    tout1 += my_data->solver->TMULT*100;  // somewhat arbitrary - cvode just integrates in this chunk size 
	    // -  cvode still determines the actual time stepping (subdivides tmult)
	    flag1 = CVode(cvode_mem1, tout1, y1, &t1, NORMAL);
	    if (flag1 != SUCCESS) { printf("Done cvode_mem1\n", flag1); error_flag = true; break; }
	  }
	*(my_data->usrData->steady_state) = false;
	CVodeFree(cvode_mem1);
	N_VFree(y1);

        if(error_flag)printf("Here Solution to SURFACE NOT FOUND!!!\n");

	//vector<double> sources = my_data->usrData->getBulkGasSources_Homo();
	std::vector<double> sources = my_data->usrData->gasSpProdRates(my_data->usrData->surfTemp);
	int nrxnGasSp = my_data->usrData->rxnGasSpInd.size();

   int is;
	for(i=0; i<nrxnGasSp; i++){
        is = my_data->usrData->rxnGasSpInd[i];
		sources[is] *= 
		(my_data->usrData->Dh*3.141592654*my_data->usrData->spMWs[is]/1000./my_data->usrData->mdot);
	}

	for(i=0; i<nrxnGasSp; i++){
	  is = my_data->usrData->rxnGasSpInd[i];
	  N_VIth(ydot,my_data->usrData->surfSp2reksSp[is]) += sources[is]/100.; // convert from 1.0/m to 1.0/cm
	  /*if(N_VIth(y,my_data->usrData->surfSp2reksSp[is])<=0.0){
	    N_VIth(ydot,my_data->usrData->surfSp2reksSp[is]) = 0.0;
	    }*/
	}
	N_VIth(ydot, solver->NEQ) += my_data->usrData->temp_rate(sources)/100.0;
	//cout << " surf tempdot " << N_VIth(ydot, solver->NEQ) << " temp " << N_VIth(y,solver->NEQ) << endl;


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	if (false) //t >= solver->output_time&& t < solver->rxn_time)
	{ 
	  /*fprintf(solver->output, "\n");
	    fprintf(solver->output,"--------------------------------\n");
	    fprintf(solver->output, "dist = %.15g \n",t);
	    fprintf(solver->output, "time = %.15g \n",N_VIth(y,solver->NEQ+1));
	    my_data->dump(solver->output);*/
	  fflush(NULL);
	  solver->output_time= t+solver->TMULT; 
	  cout << "--------------------------------\n";
	  cout << " temp " << N_VIth(y,solver->NEQ) << " tempdot " << N_VIth(ydot,solver->NEQ) << endl;
	  cout << "dist " << t << " ";
	  cout << N_VIth(y,solver->NEQ+1) << endl;
	  for (i=0; i<solver->NEQ; i++)
	    {
	      cout<< my_data->gas_sys->specs[i].spec->m_spec_name;
	      cout << " " << N_VIth(y,i) << " " << N_VIth(ydot, i) << endl;
	    }
	  std::vector<double> sources1 = my_data->usrData->surfSpProdRates(my_data->usrData->molf,my_data->usrData->surfTemp);
	  for(int l=my_data->usrData->nGasSp; l<my_data->usrData->spNames.size(); l++)
	    cout << my_data->usrData->spNames[l] << '\t' << my_data->usrData->molf[l] << '\t' << 
	      sources1[l-my_data->usrData->nGasSp] << endl;
	}
	fflush(NULL);
}
///////////////////////////////////////////////////////////////////////////     dol

static void f_const_surf(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{				// NOTE CHANGE OF VARIABLES BUT WITH SAME NAME:  TIME IS NOW DIST
	           // ADD AN EQUATION TO THE SET FOR TIME 
	int i;
	double check_temp;
	REKS_sys* my_data;
	
	my_data = (REKS_sys *) f_data;
	reks_solve * solver = my_data->solver;

	for (i=0; i<solver->NEQ1; i++)
	{
		check_temp = N_VIth(y, i);
		//		if (check_temp < 0)
		//		  check_temp = 0;
		if (!FINITE(check_temp)||check_temp > 1.1||check_temp<-1E-5)
		  check_temp = 0;
		my_data->usrData->set_molf(i, check_temp);
	}
	

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Surface Chemistry Sources 
	// 1-D Homogeneous assumption (bulk = surf) 

	
//	 get sources
//	my_data->usrData->position = t/100.;            // meters, t is cm position
	my_data->usrData->bulkTemp = my_data->gas_sys->temperature;
	my_data->usrData->surfTemp = my_data->gas_sys->temperature;
	
	std::vector<double> sources = my_data->usrData->surfSpProdRates(my_data->usrData->molf, my_data->usrData->bulkTemp);

	for(i=0; i<solver->NEQ1; i++)
		N_VIth(ydot,i) = sources[i]/my_data->usrData->siteDensity;


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	/*if (t >= solver->output_time&& t < solver->rxn_time)
	{ 
		fprintf(solver->output, "\n");
		fprintf(solver->output,"--------------------------------\n");
		fprintf(solver->output, "time = %.15g \n",t);
		my_data->dump(solver->output);
		fflush(NULL);
		solver->output_time= t+solver->TMULT; 
		cout << "--------------------------------\n";
		cout << " temp " << my_data->usrData->surfTemp << endl;
		cout << "time " << t << endl;
	}*/
	double max = 0.0;
        for(i=0;i<solver->NEQ1;i++)
	    if(fabs(N_VIth(ydot,i))>max)max=fabs(N_VIth(ydot,i));

	if(max<steady_state_critera_species){
	  *(my_data->usrData->steady_state)=true;
	  /*if(lprint){
	    for (i=0; i<solver->NEQ1; i++)
	    {
	    cout<< my_data->usrData->spNames[i+my_data->usrData->nGasSp];
	    cout << " " << N_VIth(y,i) << " " << N_VIth(ydot, i) << endl;
	    }
	    lprint = false;
		}*/
	}
}

///////////////////////////////////////////////////////////////////////////
static void f_conv(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{
	int i;
	REAL check_temp;
	REKS_sys* my_data;
	//map<REKS_specie_thermo *, REAL>::iterator iter;
	
	my_data = (REKS_sys *) f_data;
	reks_solve * solver = my_data->solver;	
	
	for (i=0; i<solver->NEQ; i++)
	{
		my_data->gas_sys->specs[i].clear_all_flags();
		check_temp = N_VIth(y, i);
		//		if (check_temp < 0)
		//		  check_temp = 0;
		if (!FINITE(check_temp)||check_temp > 1.1||check_temp<-1E-5)
		  check_temp = 0;
		my_data->gas_sys->specs[i].set_mass_fraction(check_temp);
	}
	
	my_data->gas_sys->clear_all_flags();
	my_data->gas_sys->set_temperature(N_VIth(y, solver->NEQ));
	my_data->gas_sys->mean_mole_wgt_by_massf();
	my_data->gas_sys->Fdensity = true;
	my_data->gas_sys->set_pressure(my_data->gas_sys->density * R * my_data->gas_sys->temperature
								/my_data->gas_sys->mean_mole_wgt);
	
	my_data->update_sys();

	if (!my_data->solver->IS_REDUCED)
	  {
	    my_data->rate_of_progress();
	    my_data->comp_rate_all();
	  }
	else
	  my_data->comp_rate_allRED();
	
	for (i=0; i<solver->NEQ; i++)
	{
	  //iter = my_data->masf_prodrate.find((my_data->gas_sys->specs[i]).spec);
	  //N_VIth(ydot, i)=iter->second;
	  N_VIth(ydot, i) = my_data->masf_prodrate[i];
	}
	N_VIth(ydot, solver->NEQ) = my_data->temp_rate_v();
	
	if (t >= solver->output_time&& t < solver->rxn_time)
	{
		fprintf(solver->output, "\n");
		fprintf(solver->output,"--------------------------------\n");
		fprintf(solver->output, "time = %.15g \n",t);
		my_data->dump(solver->output);
		//		fflush(NULL);	
		solver->output_time= t+solver->TMULT; 
	}
}

//////////////////////////////////////////////////////////////////////////
static void f_ttim(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{
	int i;
	REAL check_temp;
	//	struct tms startbuf;
	//	struct tms stopbuf;

	//	times(&startbuf);


	REKS_sys* my_data;
	//map<REKS_specie_thermo *, REAL>::iterator iter;
	
	my_data = (REKS_sys *) f_data;
	reks_solve * solver = my_data->solver;	
	//	cout<<"Time "<<t<<endl;
	//cout<<"N_VITH Y @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"<<endl;
	for (i=0; i<solver->NEQ; i++)
	{
		my_data->gas_sys->specs[i].clear_all_flags();
		check_temp = N_VIth(y, i);
		//		if (check_temp < 0)
		//		  check_temp = 0;
		if (!FINITE(check_temp)||check_temp > 1.1||check_temp<-1E-5)
		  { 
		    //cout<<"Here is a problem! #"<<i<<" : "<<check_temp<<endl;
		    //cout<<"The y dot for that is : "<<N_VIth(ydot, i)<<endl;
		    check_temp = 0;
		  }
		my_data->gas_sys->specs[i].set_mass_fraction(check_temp);
		//cout<<N_VIth(y, i)<<endl;;
	}
	
	my_data->gas_sys->clear_all_flags();
	my_data->gas_sys->set_temperature(solver->get_ttim_temp(t));
	my_data->gas_sys->Fpressure=true;
	my_data->gas_sys->mean_mole_wgt_by_massf();
	//	cout<<"MMW =======  "<< my_data->gas_sys->mean_mole_wgt_by_massf()<<endl;
	my_data->gas_sys->density_by_pres();
	//my_data->gas_sys->pressure_by_molar();
	my_data->update_sys();

	//	times(&stopbuf);

	if (!my_data->solver->IS_REDUCED)
	  {
	    my_data->rate_of_progress();
	    //times(&stopbuf);
	    my_data->comp_rate_all();
	  }
	else
	  my_data->comp_rate_allRED();
	
	//	cout<<"N_VITH YDOT **************************************************"<<endl;
	for (i=0; i<solver->NEQ; i++)
	{
	  check_temp = my_data->masf_prodrate[i];
	  N_VIth(ydot, i)=check_temp;
	  
	}

	
	if (t >= solver->output_time&& t < solver->rxn_time)
	{
		fprintf(solver->output, "\n");
		fprintf(solver->output,"--------------------------------\n");
		fprintf(solver->output, "time = %.15g \n",t);
		my_data->dump(solver->output);
#ifdef DEBUG	
		my_data->dump2(solver->output);
#endif
		//fflush(NULL);	
		solver->output_time= t+solver->TMULT; 
	}


	//		total_user+=(stopbuf.tms_utime-startbuf.tms_utime);
	//		total_sys+=(stopbuf.tms_stime-startbuf.tms_stime);
	
}

//////////////////////////////////////////////////////////////////////////
static void f_t_spec_tim(integer N, real t, N_Vector y, N_Vector ydot, void *f_data)
{
	int i, ii;
	REAL check_temp;
	//	struct tms startbuf;
	//	struct tms stopbuf;

	//	times(&startbuf)
;

	REKS_sys* my_data;
	//map<REKS_specie_thermo *, REAL>::iterator iter;
	

	my_data = (REKS_sys *) f_data;
	reks_solve * solver = my_data->solver;	
	//	cout<<"Time "<<t<<endl;
	//cout<<"N_VITH Y @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"<<endl;

	double total=0.0;

	for (i=0; i<solver->NEQ; i++)
	{
		my_data->gas_sys->specs[i].clear_all_flags();
		//check_temp = N_VIth(y, i);
		//		if (check_temp < 0)
		//		  check_temp = 0;
		//if (!FINITE(check_temp)||check_temp > 1.1||check_temp<-1E-5)
		// { 
		    //cout<<"Here is a problem! #"<<i<<" : "<<check_temp<<endl;
		    //cout<<"The y dot for that is : "<<N_VIth(ydot, i)<<endl;
		//check_temp = 0;
		    // }
		my_data->gas_sys->specs[i].set_mass_fraction(N_VIth(y, i));
		//cout<<N_VIth(y, i)<<endl;;
	}
	
	my_data->gas_sys->clear_all_flags();
	my_data->gas_sys->set_temperature(solver->get_custom_val(t,1));
	my_data->gas_sys->Fpressure=true;
	my_data->gas_sys->mean_mole_wgt_by_massf();
	//	cout<<"MMW =======  "<< my_data->gas_sys->mean_mole_wgt_by_massf()<<endl;
	//my_data->gas_sys->pressure_by_molar();
	//my_data->update_sys();
	my_data->gas_sys->massf2molef();
	
	/*for (i=0; i<solver->NEQ; i++)
	  for (ii=0; ii < solver->radical_specs.size(); ii++)
	    if (my_data->gas_sys->specs[i].spec->m_spec_name==solver->radical_specs[ii])
	     {
	       my_data->gas_sys->specs[i].set_mole_fraction(solver->get_custom_val(t, ii+2));
	       break;
	       }*/

	my_data->gas_sys->mean_mole_wgt_by_molef();
	my_data->gas_sys->density_by_pres();
	//cout<<"time : "<<t<<endl;
	for (i=0; i<solver->NEQ; i++)
	{
	  my_data->gas_sys->specs[i].molef2molarc_by_pres();
	  //cout<<my_data->gas_sys->specs[i].molar_concentration<<endl;
	}

	//cout<<"-------------------------";

	for (i=0; i<my_data->m_p_reactions.size(); i++)
	  (my_data->m_p_reactions[i])->update();
 
	//	times(&stopbuf);

	if (!my_data->solver->IS_REDUCED)
	  {
	    my_data->rate_of_progress();
	    //times(&stopbuf);
	    my_data->comp_rate_all();
	  }
	else
	  my_data->comp_rate_allRED();
	
	//	cout<<"N_VITH YDOT **************************************************"<<endl;
	for (i=0; i<solver->NEQ; i++)
	{
	  check_temp = my_data->masf_prodrate[i];
	  N_VIth(ydot, i)=check_temp;
	  
	  for (ii=0; ii < solver->radical_specs.size(); ii++)
	    if (my_data->gas_sys->specs[i].spec->m_spec_name==solver->radical_specs[ii])
	      {
		N_VIth(ydot, i)=0;
		break;
	      }
	  
	}

	
	if (t >= solver->output_time&& t < solver->rxn_time)
	{
		fprintf(solver->output, "\n");
		fprintf(solver->output,"--------------------------------\n");
		fprintf(solver->output, "time = %.15g \n",t);
		my_data->dump(solver->output);
#ifdef DEBUG	
		my_data->dump2(solver->output);
#endif
		//fflush(NULL);	
		solver->output_time= t+solver->TMULT; 
	}


	//		total_user+=(stopbuf.tms_utime-startbuf.tms_utime);
	//		total_sys+=(stopbuf.tms_stime-startbuf.tms_stime);
	
}


