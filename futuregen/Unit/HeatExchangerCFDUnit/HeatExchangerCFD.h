#ifndef HEATEXCHANGERCFD_H
#define HEATEXCHANGERCFD_H

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

class HeatExchangerCFD {

public:
  HeatExchangerCFD ();
  ~HeatExchangerCFD ();

  bool execute (Gas *ox_in, Gas *gas_out, summary_values *summaries);

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
  void load_oxidant (float *tf0, float *hsub0, float *densf0, float *erf0, float *smf0,
		     float *cpsf0, float *bf0, float *specfoo, float *spec0, int *nel, int *nsp,
		     char *spec_name, char *wic_name, unsigned int s1len, unsigned int s2len);

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

  // Gas in
  Gas *_gas_in;
  double _pressure;

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
