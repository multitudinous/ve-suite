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
#include "reks_container.h"
#include <fstream>
#include "expl_dll.h"

#include <ThirdParty/Cvode/cvode.h>
#include <ThirdParty/Cvode/llnltyps.h>
#include <ThirdParty/Cvode/cvdense.h>
#include <ThirdParty/Cvode/nvector.h>
#include <ThirdParty/Cvode/dense.h>

//#include "llnltyps.h" /* definitions of types real (set to REAL) and     */
                      /* integer (set to int), and the constant FALSE      */
//#include "cvode.h"    /* prototypes for CVodeMalloc, CVode, and CVodeFree, */
//                      /* constants OPT_SIZE, BDF, NEWTON, SV, SUCCESS,     */
                      /* NST, NFE, NSETUPS, NNI, NCFN, NETF                */
//#include "cvdense.h"  /* prototype for CVDense, constant DENSE_NJE         */
//#include "nvector.h"  /* definitions of type N_Vector and macro N_VIth,    */
                      /* prototypes for N_VNew, N_VFree                    */
//#include "dense.h"    /* definitions of type DenseMat, macro DENSE_ELEM    */

#define T0 0.0
//#define T1    0.001          /* first output time      */
//#define TMULT 0.001         /* output time factor     */

// functions called by the CVODE Solver
static void f_cont(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);
static void f_conp(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);
static void f_conv(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);
static void f_ttim(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);
static void f_t_spec_tim(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);

static void f_conp_dist(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);   // dol change vars all t is x
static void f_cont_dist(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);   // dol
static void f_conp_dist_cat_comb(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);   // dol
static void f_const_surf(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);   // mkd

static void f_tgiv(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);
static void f_enrg(integer N, real t, N_Vector y, N_Vector ydot, void *f_data);
static void PrintFinalStats(long int iopt[]);

class reks_solve  
{
public:
	reks_solve();
	virtual ~reks_solve();

	void set_reduced(bool red) {IS_REDUCED=red;};
	void set_writebin(bool write_bin) { WRITE_BIN = write_bin;};
	bool solve(reks_container& reks, FILE* output);
	
	// functions to support various problem types
	void CONT(REKS_sys &test_sys, REAL TMULT, REAL rxn_time);
	void CONP(REKS_sys &test_sys, REAL TMULT, REAL rxn_time);	
	void CONV(REKS_sys &test_sys, REAL TMULT, REAL rxn_time);
	void TTIM(REKS_sys &test_sys, REAL TMULT, REAL rxn_time);
	void T_SPEC_TIM(REKS_sys &test_sys, double TMULT, double rxn_time);

	bool CONSP_DIST(REKS_sys &test_sys, REAL TMULT, REAL rxr_time);            // dol  change vars all t is x
	bool CONSP_DIST_CONV(REKS_sys &test_sys, REAL TMULT,
		REAL& fuelConvWant, REAL& rxr_length);       // dol  integrate to desired conv
	void CONST_DIST(REKS_sys &test_sys, REAL TMULT, REAL rxr_time);            // dol
	bool CONSP_DIST_CAT_COMB(REKS_sys &test_sys, double TMULT, double rxr_time);   // dol
	bool CONSP_DIST_CONV_CAT_COMB(REKS_sys &test_sys, double TMULT,
		double& fuelConvWant, double& rxr_length);  // DOL
	bool CONST_SURF(REKS_sys &test_sys, double TMULT, double rxr_time);  // mkd


	// functions to support various problem types
	void TGIV(REKS_sys &test_sys, REAL TMULT, REAL residence_time);
	void ENRG(REKS_sys &test_sys, REAL TMULT, REAL residence_time, 
          REAL qloss, REAL vol, REAL start_temp);
	
	FILE* output;

	REAL get_ttim_temp(const REAL& time);
	void get_table_index(REAL& time, unsigned int& index_low);
	void GET_CUSTOM_TEMPERATURE(REAL& time, REAL& temp);
	void INIT_TIME_TEMP_PROFILE(const char* fname, unsigned int length);
	std::vector< std::pair<REAL, REAL> > time_temps;

	void INIT_TIME_TEMP_SPEC_PROFILE(const char* fname);
	double get_custom_val(double time, int index);
	void get_vals_table_index(double& time, unsigned int& index_low);
	std::vector< std::vector<double> > time_temp_specs;
	std::vector<std::string> radical_specs;
	
	int ttime_type;
	string time_temp_profile_fname;
	REAL to_ttim;
	REAL slope_ttim;
	REAL start_temp;

	REAL TMULT;
	real rxn_time;
	real abstol;
	real reltol;
	REAL qloss;
	REAL volume;
	REAL tinl;
	REAL output_time;
	
	int NEQ, NEQ1;
	bool if_spec_flrt;
	REAL flrt;

	//If the Mechanism is reduced, Make the reduced to be parameter controlled instead of conditional compilaton controled
	bool IS_REDUCED;
	bool WRITE_BIN;
	expl_dll *edll;
	void init_RED(char* dll_fqpath);
};

const REAL steady_state_critera_species = 1.0e-6;
const REAL steady_state_critera_temp    = 1.0e-2;
//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
class tgiv_cvode_data_cluster  // passed through cvode for data access (f_data) 
{
public:
  REKS_sys* my_data;
  vector<REAL>* y_star;
  REAL* residence_time;
  bool* steady_state;
  bool if_spec_flrt;
  REAL* flrt;
  REAL* vol;
};

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
class enrg_cvode_data_cluster  // passed through cvode for data access (f_data) 
{
public:
  REKS_sys* my_data;
  vector<REAL>* y_star;
  vector<REAL>* h_star;
  REAL* residence_time;
  REAL* qloss;
  bool* steady_state;
  bool if_spec_flrt;
  REAL* flrt;
  REAL* vol;
};

#endif // !defined(AFX_REKS_SOLVE_H__D5B3B03D_E19D_44E7_B6D2_06970DA979B8__INCLUDED_)
