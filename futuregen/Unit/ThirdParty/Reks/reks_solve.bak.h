// reks_solve.h: interface for the reks_solve class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_REKS_SOLVE_H__D5B3B03D_E19D_44E7_B6D2_06970DA979B8__INCLUDED_)
#define AFX_REKS_SOLVE_H__D5B3B03D_E19D_44E7_B6D2_06970DA979B8__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "REKS_Thrm_Info.h"
#include "ReactionReader.h"
#include "InpReader.h"
#include "reks.h"
#include "ckintf.h"
#include "reks_container.h"
#include <fstream>
#include "StringParseUtil.h"
#include <Packages/REI/Core/ThirdParty/Cvode/llnltyps.h> /* definitions of types real (set to double) and     */
                      /* integer (set to int), and the constant FALSE      */
#include <Packages/REI/Core/ThirdParty/Cvode/cvode.h>    /* prototypes for CVodeMalloc, CVode, and CVodeFree, */
                      /* constants OPT_SIZE, BDF, NEWTON, SV, SUCCESS,     */
                      /* NST, NFE, NSETUPS, NNI, NCFN, NETF                */
#include <Packages/REI/Core/ThirdParty/Cvode/cvdense.h>  /* prototype for CVDense, constant DENSE_NJE         */
#include <Packages/REI/Core/ThirdParty/Cvode/nvector.h>  /* definitions of type N_Vector and macro N_VIth,    */
                      /* prototypes for N_VNew, N_VFree                    */
#include <Packages/REI/Core/ThirdParty/Cvode/dense.h>    /* definitions of type DenseMat, macro DENSE_ELEM    */

#define T0 0.0
//#define T1    0.001          /* first output time      */
//#define TMULT 0.001         /* output time factor     */

// functions called by the CVODE Solver
static void f_cont(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);
static void f_conp(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);
static void f_conv(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);
static void f_ttim(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);
static void f_ttim_spec(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);
static void f_conp_dist(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);   // dol change vars all t is x
static void f_cont_dist(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);   // dol

static void f_tgiv(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);
static void f_enrg(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);
static void PrintFinalStats(long int iopt[]);

class reks_solve  
{
public:
	reks_solve();
	virtual ~reks_solve();

	bool solve(reks_container& reks, FILE* output);
	
	// functions to support various problem types
	void CONT(REKS_sys &test_sys, double TMULT, double rxn_time);
	void CONP(REKS_sys &test_sys, double TMULT, double rxn_time);	
	void CONV(REKS_sys &test_sys, double TMULT, double rxn_time);
	void TTIM(REKS_sys &test_sys, double TMULT, double rxn_time);
	bool CONSP_DIST(REKS_sys &test_sys, double TMULT, double rxr_time);            // dol  change vars all t is x
	bool CONSP_DIST_CONV(REKS_sys &test_sys, double TMULT,
		double& fuelConvWant, double& rxr_length);       // dol  integrate to desired conv
	void CONST_DIST(REKS_sys &test_sys, double TMULT, double rxr_time);            // dol

	void TTIM_SPEC(REKS_sys &test_sys, double TMULT, double rxn_time);

	// functions to support various problem types
	void TGIV(REKS_sys &test_sys, double TMULT, double residence_time);
	void ENRG(REKS_sys &test_sys, double TMULT, double residence_time, 
          double qloss, double vol, double start_temp);
	
	FILE* output;

	double get_ttim_temp(const double& time);
	void get_table_index(double& time, unsigned int& index_low);
	void GET_CUSTOM_TEMPERATURE(double& time, double& temp);
	void INIT_TIME_TEMP_PROFILE(const char* fname, unsigned int length);
	std::vector< std::pair<double, double> > time_temps;
	void INIT_TIME_TEMP_SPEC_PROFILE(const char* fname);
	double GET_CUSTOM_VAL(double time, int index);
	void get_vals_table_index(double& time, unsigned int& index_low);
	std::vector< std::vector<double> > time_temp_specs;
	std::vector<std::string> ovrd_specs;
	
	int ttime_type;
	string time_temp_profile_fname;
	double to_ttim;
	double slope_ttim;
	double start_temp;

	double TMULT;
	double rxn_time;
	double abstol;
	double reltol;
	double qloss;
	double volume;
	double tinl;
	double output_time;
	
	int NEQ;
	bool if_spec_flrt;
	double flrt;
	ckinterf* psrinterf;
};

const double steady_state_critera_species = 1.0e-6;
const double steady_state_critera_temp    = 1.0e-2;
//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
class tgiv_cvode_data_cluster  // passed through cvode for data access (f_data) 
{
public:
  REKS_sys* my_data;
  vector<double>* y_star;
  double* residence_time;
  bool* steady_state;
  bool if_spec_flrt;
  double* flrt;
  double* vol;
};

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
class enrg_cvode_data_cluster  // passed through cvode for data access (f_data) 
{
public:
  REKS_sys* my_data;
  vector<double>* y_star;
  vector<double>* h_star;
  double* residence_time;
  double* qloss;
  bool* steady_state;
  bool if_spec_flrt;
  double* flrt;
  double* vol;
};

#endif // !defined(AFX_REKS_SOLVE_H__D5B3B03D_E19D_44E7_B6D2_06970DA979B8__INCLUDED_)
