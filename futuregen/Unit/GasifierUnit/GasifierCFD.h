#ifndef GASIFIERCFD_H
#define GASIFIERCFD_H

#include <iostream>
#include <fstream>
#include <string>
#include <map>

using namespace std;

// typedefs for profile stuff
typedef vector<float> FloatVector;
typedef map<string, FloatVector> YDataMap;
typedef map<string, FloatVector>::iterator YDataMapIter;
typedef pair<FloatVector, YDataMap> profile_map_pair;
typedef map<string, profile_map_pair> profile_map;
typedef map<string, profile_map_pair>::iterator profile_map_iter;

#ifndef WIN32
extern "C"
{
  void histfit_ (double *ps, double *pmf, double *passed_areas, int *size,
		 double *percent_through_100, double *percent_through_200);
  void createbins_(double *ps, double *pmf, double *passed_areas, int *size,
		   double *m1, double *sg);
}
#else 
extern "C"
{
  void __stdcall HISTFIT (double *ps, double *pmf, double *passed_areas, int *size,
		 double *percent_through_100, double *percent_through_200);
  void __stdcall CREATEBINS(double *ps, double *pmf, double *passed_areas, int *size,
		   double *m1, double *sg);
}
#endif //ifdef WIN32

class GasifierCFD {

public:
  GasifierCFD ();
  ~GasifierCFD ();
  
  void setCoalType (std::string coaltype);
  void normalizeUlt ();
  void calculateProx ();

  bool execute (Gas *ox_in, Gas *gas_out, summary_values *summaries);
  void parse_levels (string level_data);

  void load_and_run_glacier();
  GasCell outlet_cell(int i, int j, int k, int face,
		      int ns_val, int nlm_val,
		      float *sns, float *stb, float *sew,
		      float *part_flow, float *part_temp, float *part_size, float *part_var,
		      float *u, float *v, float *w, float *x, float *y, float *z,
		      float *eff, float *eta, float *chi,
		      float *t, float *p, float *h, float *den,
		      float *spec_val,
		      float *part_char, float *part_ash, float *part_water, float *part_coal,
		      float *hco, float *hwo, float *hao, float *hho,
		      float *ynu, float *tar, float *yc);

  //* From fortran --
  void load_geom(int* ni,int* nj,int* nk,int* nx,int* ny,int* nz,
		 float *x_vals,float *y_vals,float *z_vals,int *icell_array);
  void load_scirun_pd(float *pd, float *pmf, int *nps, int *numstr);
  void load_scirun_wics(float *wic, int *j, int *nlm, int *np, int *nel);
  void load_scirun_groups(float *f, float *t, float *p, int* istage);
  void load_scirun_hhv(float *omegal, float *yy, float *omegaa, float *hc0);
  void load_scirun_coal(float *coal_flows);
  void send_scirun_data(int *ns, int *nlm,
			float *sns, float *stb, float *sew,
			float *part_flow, float *part_temp, float *part_size, float *part_var,
			float *u, float *v, float *w, float *x, float *y, float *z,
			float *eff, float *eta, float *chi,
			float *t, float *p, float *h, float *den,
			float *spec_val, float *wic_val,
			float *part_char, float *part_ash, float *part_water, float *part_coal,
			float *hco, float *hwo, float *hao, float *hho,
			char *spec_name, char *wic_name, char *part_name,
			float *press_in,
			float *ht_conv, float *ht_netwall, float *ht_netexit,
			float *ynu, float *tar, float *yc,
			unsigned int s1len, unsigned int s2len, unsigned int s3len);
  void send_scirun_specie(int *ns, float *spec_val, char *spec_name, unsigned int slen);
  void insert_summary_val(char *description, float *value, unsigned int slen);
  void insert_xdata(char *xname, float *values, int *num_values, unsigned int slen);
  void insert_ydata(char *yname, float *values, int *num_values,
		    char *xdata_name, unsigned int s1len, unsigned int s2len);
  void update_plot();
  void load_scirun_slag(double *deltaw, double *deltar, double *kw, double *kr,
			double *ks, double *kd, double *ha, double *rhos,
			double *ta1, double *tcv, double *emiss, double *ashcomp);
  void load_scirun_flags (int *lrsrt, int *lprst);

  // NOT USED
  void load_scirun_coalMT(float *coal_flows, float *coal_temps) { };
  void send_current_outputs(int* iteration_number) { };
  void send_current_outputs_nox(int* iteration_number) { };
  void update_sr_begin_particles() { };
  void update_sr_end_particles(float* nix) { };
  void sr_begin_particle(int *ips, int *isl) { };
  void sr_end_particle(int *ips, int *isl, int *nps, int *nsl) { }; 
  void load_current_traj_point(float *XP,float *YP,
			       float *ZP, float *TIM, float *PNFRP,
			       float *SIGMAX,float *SIGMAY,float *SIGMAZ,
			       float *ALFT0P,float *PDIA,float *TMP,
			       float *SIGMA0) { };
  void update_dbfile() { };

  // RUNNNING
  bool get_running() {
    bool tmp_running = running;
    return(tmp_running);
  }
  
  void set_running(bool new_running_status) {
    running = new_running_status;
  }
  
  bool get_abort_glacier() {
    bool tmp_abort_glacier = abort_glacier;
    return(tmp_abort_glacier);
  }
  
  void set_abort_glacier(bool new_abort_glacier_status) {
    abort_glacier = new_abort_glacier_status;
  }

  // Flows
  double _ox_temp   [3];
  double _ox_flow   [3];
  double _stm_temp  [3];
  double _stm_flow  [3];
  double _slur_temp [3];
  double _slur_flow [3];
  double _coal_pct  [3];
  double _char_pct  [3];

  // WICS ...
  double _wic_C;
  double _wic_H;
  double _wic_O;
  double _wic_N;
  double _wic_S;
  double _wic_CL;
  double _ash_ult;
  double _ash_prox;
  double _proxH2O;
  double _proxVM;
  double _proxFC;
  double _hhv;

  // Ash composition
  double _comp1;  // SiO2
  double _comp2;  // Al2O3
  double _comp3;  // TiO2
  double _comp4;  // Fe2O3
  double _comp5;  // CaO
  double _comp6;  // MgO
  double _comp7;  // Na2O
  double _comp8;  // K2O
  double _comp9;  // SO3
  double _comp10; // P2O5
  double _comp11; // BaO
  double _comp12; // SrO
 
  // Coal Kinetics
  double _devol_a1;
  double _devol_a2;
  double _devol_e1;

  double _devol_e2;
  double _devol_y1;
  double _devol_y2;

  double _oxid_a;
  double _oxid_n;
  double _oxid_e;

  double _co2gas_a;
  double _co2gas_n;
  double _co2gas_e;

  double _h2ogas_a;
  double _h2ogas_n;
  double _h2ogas_e;

  // particle sizes
  double _size_50;
  double _size_200;

  // Gasifier type (1 : 1-stage)
  int _stage;

  // Pressures
  double _pressure;
  double _press_drop;

  // Misc
  std::string _work_dir;
  int _prt_restart;
  int _gas_restart;
  bool abort_glacier;
  bool running;
  std::string _error;

  // Results
  Gas *_gas_out;
  double _thermal_input;
  summary_values *_summaries;

  // Used to hold plot data.
  map<string, profile_map_pair> plot_data_map;
  // List of data to send to plotter, sent from GUI.
  vector<string> plot_data; // <xname, yname1, yname2, ...>

  //* Geometry information, from fortran
  int ni,nj,nk;
  int nx,ny,nz;
  float* node_x;
  float* node_y;
  float* node_z;
  std::vector<std::vector<std::vector<int > > > pcell;

};


#endif
