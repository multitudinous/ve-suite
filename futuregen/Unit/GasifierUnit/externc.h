#ifndef __GAS_EXTERNC_H__
#define __GAS_EXTERNC_H__

#ifdef WIN32
#define WIN_PREFIX __stdcall
#endif //WIN32

extern "C"
{
  // these functions are called FROM FORTRAN
  int WIN_PREFIX gas_abort_status_();
  typedef int WIN_PREFIX gas_abort_status_f();
  typedef gas_abort_status_f * gas_abort_status_fp;

  void WIN_PREFIX gas_load_scirun_groups_(float *f, float *t, float *p, int* istage);
  typedef void WIN_PREFIX gas_load_scirun_groups_f(float *f, float *t, float *p, int* istage);
  typedef gas_load_scirun_groups_f* gas_load_scirun_groups_fp;
 	
  void WIN_PREFIX gas_send_scirun_specie_(int *ns, float *spec_val, char *spec_name,
			       unsigned int slen);
  typedef void WIN_PREFIX gas_send_scirun_specie_f(int *ns, float *spec_val, char *spec_name,
					   unsigned int slen);
  typedef gas_send_scirun_specie_f* gas_send_scirun_specie_fp;

  
  void WIN_PREFIX gas_load_scirun_coal_(float *coal_flows);
  typedef void WIN_PREFIX gas_load_scirun_coal_f(float *coal_flows);
  typedef gas_load_scirun_coal_f* gas_load_scirun_coal_fp;	

  void WIN_PREFIX gas_load_scirun_hhv_(float *omegal, float *yy, float *omegaa, float *hc0);
  typedef void WIN_PREFIX gas_load_scirun_hhv_f(float *omegal, float *yy, float *omegaa, float *hc0);
  typedef gas_load_scirun_hhv_f* gas_load_scirun_hhv_fp;

  void WIN_PREFIX gas_load_scirun_coalMT_(float *coal_flows, float *coal_temps);
  typedef void WIN_PREFIX gas_load_scirun_coalMT_f(float *coal_flows, float *coal_temps);
  typedef gas_load_scirun_coalMT_f* gas_load_scirun_coalMT_fp;

  void WIN_PREFIX gas_update_sr_(int* iteration_number);
  typedef void WIN_PREFIX gas_update_sr_f(int* iteration_number);
  typedef gas_update_sr_f* gas_update_sr_fp;

  void WIN_PREFIX gas_update_sr_nox_(int* iteration_number);
  typedef void WIN_PREFIX gas_update_sr_nox_f(int* iteration_number);
  typedef gas_update_sr_nox_f* gas_update_sr_nox_fp;

  void WIN_PREFIX gas_update_sr_begin_particles_();
  typedef void WIN_PREFIX gas_update_sr_begin_particles_f();
  typedef gas_update_sr_begin_particles_f* gas_update_sr_begin_particles_fp;

  void WIN_PREFIX gas_update_sr_end_particles_(float* nix);
  typedef void WIN_PREFIX gas_update_sr_end_particles_f(float* nix);
  typedef gas_update_sr_end_particles_f* gas_update_sr_end_particles_fp;

  void WIN_PREFIX gas_sr_begin_particle_(int *ips, int *isl);
  typedef void WIN_PREFIX gas_sr_begin_particle_f(int *ips, int *isl);
  typedef gas_sr_begin_particle_f* gas_sr_begin_particle_fp;

  void WIN_PREFIX gas_sr_end_particle_(int *ips, int *isl, int *nps, int *nsl); 
  typedef void WIN_PREFIX gas_sr_end_particle_f(int *ips, int *isl, int *nps, int *nsl);
  typedef gas_sr_end_particle_f* gas_sr_end_particle_fp;

  void WIN_PREFIX gas_load_scirun_pd_(float *pd, float *pmf, int *nps, int *numstr);
  typedef void WIN_PREFIX gas_load_scirun_pd_f(float *pd, float *pmf, int *nps, int *numstr);
  typedef gas_load_scirun_pd_f* gas_load_scirun_pd_fp;

  void WIN_PREFIX gas_load_scirun_wics_(float *wic, int *j, int *nlm, int *np, int *nel);
  typedef void WIN_PREFIX gas_load_scirun_wics_f(float *wic, int *j, int *nlm, int *np, int *nel);
  typedef gas_load_scirun_wics_f* gas_load_scirun_wics_fp;

  void WIN_PREFIX gas_load_geom_sr_(int* ni,int* nj,int* nk,int* nx,int* ny,int* nz,
			 float *x,float *y,float *z,int *icell_array);
  typedef void WIN_PREFIX gas_load_geom_sr_f(int* ni,int* nj,int* nk,int* nx,int* ny,int* nz,
				     float *x,float *y,float *z,int *icell_array);
  typedef gas_load_geom_sr_f* gas_load_geom_sr_fp;

  void WIN_PREFIX gas_load_current_traj_point_(int *IJKNT, float *XP,float *YP,
				    float *ZP, float *TIM, float *PNFRP,
				    float *SIGMAX,float *SIGMAY,float *SIGMAZ,
				    float *ALFT0P,float *PDIA,float *TMP,
				    float *SIGMA0);
  typedef void WIN_PREFIX gas_load_current_traj_point_f(int *IJKNT, float *XP,float *YP,
						float *ZP, float *TIM, float *PNFRP,
						float *SIGMAX,float *SIGMAY,float *SIGMAZ,
						float *ALFT0P,float *PDIA,float *TMP,
						float *SIGMA0);
  typedef gas_load_current_traj_point_f* gas_load_current_traj_point_fp;

  void WIN_PREFIX gas_send_scirun_data_(int *ns, int *nlm,
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
			     unsigned int s1len, unsigned int s2len, unsigned int s3len);
  typedef void WIN_PREFIX gas_send_scirun_data_f(int *ns, int *nlm,
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
					 unsigned int s1len, unsigned int s2len, unsigned int s3len);
  typedef gas_send_scirun_data_f* gas_send_scirun_data_fp;

  void WIN_PREFIX gas_insert_summary_val_(char *description, float *value, unsigned int slen);
  typedef void WIN_PREFIX gas_insert_summary_val_f(char *description, float *value, unsigned int slen);
  typedef gas_insert_summary_val_f* gas_insert_summary_val_fp;

  void WIN_PREFIX gas_insert_xdata_(char *xname, float *values, int *num_values, unsigned int slen);
  typedef void WIN_PREFIX gas_insert_xdata_f(char *xname, float *values, int *num_values, unsigned int slen);
  typedef gas_insert_xdata_f* gas_insert_xdata_fp;

  void WIN_PREFIX gas_insert_ydata_(char *yname, float *values, int *num_values,
			 char *xdata_name, unsigned int s1len, unsigned int s2len);
  typedef void WIN_PREFIX gas_insert_ydata_f(char *yname, float *values, int *num_values,
				     char *xdata_name, unsigned int s1len, unsigned int s2len);
  typedef gas_insert_ydata_f* gas_insert_ydata_fp;

  void WIN_PREFIX gas_update_plot_();
  typedef void WIN_PREFIX gas_update_plot_f();
  typedef gas_update_plot_f* gas_update_plot_fp;
  // DXLink--
  void WIN_PREFIX gas_update_dbfile_();
  typedef void WIN_PREFIX gas_update_dbfile_f();
  typedef gas_update_dbfile_f* gas_update_dbfile_fp;
  // --DXLink

  void WIN_PREFIX gas_load_scirun_slag_(double *deltaw, double *deltar, double *kw, double *kr,
			     double *ks, double *kd, double *ha, double *rhos,
			     double *ta1, double *tcv, double *emiss, double *ashcomp);
  typedef void WIN_PREFIX gas_load_scirun_slag_f(double *deltaw, double *deltar, double *kw, double *kr,
					 double *ks, double *kd, double *ha, double *rhos,
					 double *ta1, double *tcv, double *emiss, double *ashcomp);
  typedef gas_load_scirun_slag_f* gas_load_scirun_slag_fp;

  void WIN_PREFIX gas_load_scirun_flags_ (int *lrsrt, int *lprst);
  typedef void WIN_PREFIX gas_load_scirun_flags_f (int *lrsrt, int *lprst);
  typedef gas_load_scirun_flags_f* gas_load_scirun_flags_fp;
}

#endif

  
