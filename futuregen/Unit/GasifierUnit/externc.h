#ifndef __GAS_EXTERNC_H__
#define __GAS_EXTERNC_H__

extern "C"
{
  // these functions are called FROM FORTRAN
  int __stdcall gas_abort_status_();
  typedef __stdcall int(*gas_abort_status_fp)();

  void __stdcall gas_load_scirun_groups_(float *f, float *t, float *p, int* istage);
  typedef void __stdcall (*gas_load_scirun_groups_fp)(float *f, float *t, float *p, int* istage);

  void __stdcall gas_send_scirun_specie_(int *ns, float *spec_val, char *spec_name,
			       unsigned int slen);
  typedef void __stdcall (*gas_send_scirun_specie_fp)(int *ns, float *spec_val, char *spec_name,
					   unsigned int slen);

  void __stdcall gas_load_scirun_coal_(float *coal_flows);
  typedef void __stdcall (*gas_load_scirun_coal_fp)(float *coal_flows);

  void __stdcall gas_load_scirun_hhv_(float *omegal, float *yy, float *omegaa, float *hc0);
  typedef void __stdcall (*gas_load_scirun_hhv_fp)(float *omegal, float *yy, float *omegaa, float *hc0);

  void __stdcall gas_load_scirun_coalMT_(float *coal_flows, float *coal_temps);
  typedef void __stdcall (*gas_load_scirun_coalMT_fp)(float *coal_flows, float *coal_temps);

  void __stdcall gas_update_sr_(int* iteration_number);
  typedef void __stdcall (*gas_update_sr_fp)(int* iteration_number);

  void __stdcall gas_update_sr_nox_(int* iteration_number);
  typedef void __stdcall (*gas_update_sr_nox_fp)(int* iteration_number);

  void __stdcall gas_update_sr_begin_particles_();
  typedef void __stdcall (*gas_update_sr_begin_particles_fp)();

  void __stdcall gas_update_sr_end_particles_(float* nix);
  typedef void __stdcall (*gas_update_sr_end_particles_fp)(float* nix);

  void __stdcall gas_sr_begin_particle_(int *ips, int *isl);
  typedef void __stdcall (*gas_sr_begin_particle_fp)(int *ips, int *isl);

  void __stdcall gas_sr_end_particle_(int *ips, int *isl, int *nps, int *nsl); 
  typedef void __stdcall (*gas_sr_end_particle_fp)(int *ips, int *isl, int *nps, int *nsl);

  void __stdcall gas_load_scirun_pd_(float *pd, float *pmf, int *nps, int *numstr);
  typedef void __stdcall (*gas_load_scirun_pd_fp)(float *pd, float *pmf, int *nps, int *numstr);

  void __stdcall gas_load_scirun_wics_(float *wic, int *j, int *nlm, int *np, int *nel);
  typedef void __stdcall (*gas_load_scirun_wics_fp)(float *wic, int *j, int *nlm, int *np, int *nel);

  void __stdcall gas_load_geom_sr_(int* ni,int* nj,int* nk,int* nx,int* ny,int* nz,
			 float *x,float *y,float *z,int *icell_array);
  typedef void __stdcall (*gas_load_geom_sr_fp)(int* ni,int* nj,int* nk,int* nx,int* ny,int* nz,
				     float *x,float *y,float *z,int *icell_array);

  void __stdcall gas_load_current_traj_point_(int *IJKNT, float *XP,float *YP,
				    float *ZP, float *TIM, float *PNFRP,
				    float *SIGMAX,float *SIGMAY,float *SIGMAZ,
				    float *ALFT0P,float *PDIA,float *TMP,
				    float *SIGMA0);
  typedef void __stdcall (*gas_load_current_traj_point_fp)(int *IJKNT, float *XP,float *YP,
						float *ZP, float *TIM, float *PNFRP,
						float *SIGMAX,float *SIGMAY,float *SIGMAZ,
						float *ALFT0P,float *PDIA,float *TMP,
						float *SIGMA0);

  void __stdcall gas_send_scirun_data_(int *ns, int *nlm,
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
  typedef void __stdcall (*gas_send_scirun_data_fp)(int *ns, int *nlm,
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

  void __stdcall gas_insert_summary_val_(char *description, float *value, unsigned int slen);
  typedef void __stdcall (*gas_insert_summary_val_fp)(char *description, float *value, unsigned int slen);

  void __stdcall gas_insert_xdata_(char *xname, float *values, int *num_values, unsigned int slen);
  typedef void __stdcall (*gas_insert_xdata_fp)(char *xname, float *values, int *num_values, unsigned int slen);

  void __stdcall gas_insert_ydata_(char *yname, float *values, int *num_values,
			 char *xdata_name, unsigned int s1len, unsigned int s2len);
  typedef void __stdcall (*gas_insert_ydata_fp)(char *yname, float *values, int *num_values,
				     char *xdata_name, unsigned int s1len, unsigned int s2len);

  void __stdcall gas_update_plot_();
  typedef void __stdcall (*gas_update_plot_fp)();

  // DXLink--
  void __stdcall gas_update_dbfile_();
  typedef void __stdcall (*gas_update_dbfile_fp)();
  // --DXLink

  void __stdcall gas_load_scirun_slag_(double *deltaw, double *deltar, double *kw, double *kr,
			     double *ks, double *kd, double *ha, double *rhos,
			     double *ta1, double *tcv, double *emiss, double *ashcomp);
  typedef void __stdcall (*gas_load_scirun_slag_fp)(double *deltaw, double *deltar, double *kw, double *kr,
					 double *ks, double *kd, double *ha, double *rhos,
					 double *ta1, double *tcv, double *emiss, double *ashcomp);
 
  void __stdcall gas_load_scirun_flags_ (int *lrsrt, int *lprst);
  typedef void __stdcall (*gas_load_scirun_flags_fp) (int *lrsrt, int *lprst);
}

#endif

  
