
#include "V21Helper.h"

#include "PipeCFD.h"

#ifndef WIN32
#include <dlfcn.h>
#else
#include <windows.h>
#include <direct.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <cstdlib>
#include <cstdio>
#include <cmath>

class PipeCFD* PIPE_GLACIER_PTR;

#include "externc.h"   // callback prototypes

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

PipeCFD::PipeCFD ()
{
  _prt_restart = 0;
  _gas_restart = 0;

  abort_glacier = false;
  running = false;
  _summaries = NULL;

  _work_dir = "./case";
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

PipeCFD::~PipeCFD ()
{
 
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

bool PipeCFD::execute (Gas *gas_in, Gas *gas_out, summary_values *summaries)
{  
  _gas_out = gas_out;
  _summaries = summaries;

  // Restart?
  bool restart = false;
  struct stat buf;
  if(!stat((_work_dir + "/RESTRT").c_str(), &buf) &&
     !stat((_work_dir + "/INLET").c_str(), &buf) &&
     !stat((_work_dir + "/DATA").c_str(), &buf) &&
     !stat((_work_dir + "/THERMO").c_str(), &buf) &&
     !stat((_work_dir + "/GRID").c_str(), &buf) &&
     !stat((_work_dir + "/PARSOU").c_str(), &buf))
    {
      restart = true;
    }

  if(restart) {

  } else {
    string path = _work_dir;
    string basepath = "./Glacier/Cases/Pipe/";

    system(("cp " + basepath + "DATA " + path + "/DATA").c_str());
    system(("cp " + basepath + "INLET " + path + "/INLET").c_str());
    system(("cp " + basepath + "THERMO " + path + "/THERMO").c_str());
    system(("cp " + basepath + "GRID " + path + "/GRID").c_str());

  }

  _gas_in = gas_in;

  _pressure = gas_in->gas_composite.P;
  
  if(!get_running()) {
    set_running(true);
    if(!get_abort_glacier()) {
      cerr<<"Starting Glacier.....\n";	
      load_and_run_glacier();
    }
 
    if(!get_abort_glacier())
      cerr << "Possible error occured in Glacier run.\n";
     
    set_running(false);
  }

  return true;
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void PipeCFD::load_and_run_glacier()
{
  /*
       If the value of FilePath is NULL, a value for the main application is
       returned. This allows dynamically loaded objects to look up symbols in
       the main executable, or for an application to examine symbols available
       within itself. */

  /* 
     If the value of pathname is 0, dlopen() provides a handle on
     a  global symbol object.  This object provides access to the
     symbols from an ordered set of  objects  consisting  of  the
     original  program image file, together with any dependencies
     loaded at program startup, and any objects that were  loaded
     using  dlopen()  together with the RTLD_GLOBAL flag.  As the
     latter set of objects can change during  process  execution,
     the set identified by handle can also change dynamically. */
  //For windows, the counter part of dlopen will be included in the conditional comple part
  // Initialize global glacier pointer to this
  //LoadDLL, GetSym is the window's version of the dl operation of linux
  PIPE_GLACIER_PTR = this;

  const char *errmsg;

#ifndef WIN32
  void *glacier_handle;
#else
  HINSTANCE glacier_handle;
#endif
  
  typedef void WIN_PREFIX close_io_func_type();
  typedef void glacier_func_type (gas_abort_status_fp,
		       gas_load_scirun_groups_fp,
		       gas_send_scirun_specie_fp,
		       gas_load_scirun_coal_fp,
		       gas_load_scirun_coalMT_fp,
		       gas_update_sr_fp,
		       gas_update_sr_nox_fp,
		       gas_update_sr_begin_particles_fp,
		       gas_update_sr_end_particles_fp,
		       gas_load_scirun_wics_fp,
		       gas_load_geom_sr_fp,
		       gas_load_current_traj_point_fp,
		       gas_send_scirun_data_fp,
		       gas_sr_begin_particle_fp,
		       gas_sr_end_particle_fp,
		       gas_insert_summary_val_fp,
		       gas_insert_xdata_fp,
		       gas_insert_ydata_fp,
		       gas_update_plot_fp,
		       gas_load_scirun_hhv_fp,
		       gas_load_scirun_pd_fp,
		       gas_update_dbfile_fp,
		       gas_load_scirun_slag_fp,
		       gas_load_scirun_flags_fp,
		       gas_load_oxidant_fp);

  close_io_func_type * close_io_func;
  glacier_func_type* glacier_func;
  
  string path = _work_dir;

#ifndef WIN32
  if(chdir(path.c_str())) {
    cerr << "PipeCFD: empty working directory path\n";
    return ;
  }
#else
  if(_chdir(path.c_str())) {
    cerr << "PipeCFD: empty working directory path\n";
    return ;
  }
#endif

#ifndef WIN32
  std::string glac_lib = "./Glacier/make_glacier/glacier_gasifier.so";
#else
  std::string glac_lib = "./Glacier/Glacier.dll";
#endif

#ifndef WIN32
  glacier_handle = dlopen(glac_lib.c_str(), RTLD_NOW);
#else
  glacier_handle = LoadLibrary(glac_lib.c_str());
#endif

  if(glacier_handle=='\0'){

#ifndef WIN32
    cerr<<"Failed to load lib: "<<dlerror()<<endl;
#else
    cerr<<"Failed to open dll: "<<glac_lib<<endl;
#endif

    return;
  }

#ifndef WIN32
  dlerror();  // clear errors

  ((void*)glacier_func) = dlsym(glacier_handle,"start_glacier");
  if((errmsg=dlerror())!=NULL){
    cerr<<"Didn't find glacier in so: "<<errmsg<<endl;
    return;
  }
#else
  glacier_func =  (glacier_func_type*) GetProcAddress(glacier_handle,"start_glacier");
  if(glacier_func=='\0'){
    cerr<<"Didn't find start_glacier in dll: "<<glac_lib<<endl;
    return;
  }
#endif
  
  glacier_func(gas_abort_status_,
	       gas_load_scirun_groups_,
	       gas_send_scirun_specie_,
	       gas_load_scirun_coal_,
	       gas_load_scirun_coalMT_,
	       gas_update_sr_,
	       gas_update_sr_nox_,
	       gas_update_sr_begin_particles_,
	       gas_update_sr_end_particles_,
	       gas_load_scirun_wics_,
	       gas_load_geom_sr_,
	       gas_load_current_traj_point_,
	       gas_send_scirun_data_,
	       gas_sr_begin_particle_,
	       gas_sr_end_particle_,
	       gas_insert_summary_val_,
	       gas_insert_xdata_,
	       gas_insert_ydata_,
	       gas_update_plot_,
	       gas_load_scirun_hhv_, 
	       gas_load_scirun_pd_,
	       gas_update_dbfile_,
	       gas_load_scirun_slag_,
 	       gas_load_scirun_flags_,
	       gas_load_oxidant_);

	cout<<"Done with glacier_func"<<endl;

#ifndef WIN32  
  dlerror();  // clear errors
  ((void*)close_io_func) = dlsym(glacier_handle,"close_io_");
  if((errmsg=dlerror())!=NULL){
    cerr<<"Didn't find close_io in so: "<<errmsg<<endl;
    return;
  }
#else
  close_io_func = (close_io_func_type*) GetProcAddress(glacier_handle,"CLOSE_IO");
  if(close_io_func=='\0'){
    cerr<<"Didn't find close_io_ in dll: "<<glac_lib<<endl;
    return;
  }
#endif

  close_io_func();

#ifndef WIN32
  dlclose(glacier_handle); 
  if(chdir("../")) {
#else
  FreeLibrary(glacier_handle);
  if(_chdir("../")) {
#endif

    cerr << "bad directory path\n";
    return;
  }  //work_dir is ./case
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void PipeCFD::load_geom(int* fni,int* fnj,int* fnk,int* fnx,int* fny,int* fnz,
		        float *x_vals,float *y_vals,float *z_vals,int *icell_array)
{
  ni     = *fni;
  nj     = *fnj;
  nk     = *fnk;
  nx     = *fnx;
  ny     = *fny;
  nz     = *fnz;
  node_x = x_vals;
  node_y = y_vals;
  node_z = z_vals;
  
  // load pcell data
  pcell.resize(ni);
  for(int i=0;i<ni;i++) {
    pcell[i].resize(nj);
    for(int j=0;j<nj;j++) {
      pcell[i][j].resize(nk);
      for(int k=0;k<nk;k++) {
	pcell[i][j][k] = icell_array[i+j*nx+k*nx*ny];
      }
    }
  }
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void PipeCFD::load_scirun_pd(float *pd, float *pmf, int *nps, int *numstr) {

}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void PipeCFD::load_scirun_wics(float *wic, int *j, int *nlm, int *np, int *nel) {

}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void PipeCFD::load_scirun_groups(float *f, float *t, float *p, int *Istage) {

  // *Istage = 1; one stage gasifier
  // *Istage = 2; two stage gasifier
  
  *Istage = 3;

  // Pressure
  *p = _pressure;
  
  return;
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void PipeCFD::load_scirun_hhv(float *omegal, float *yy, float *omegaa, float *hc0) {

}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void PipeCFD::load_scirun_coal(float *coal_flows) {

}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void PipeCFD::send_scirun_data(int *ns, int *nlm,
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
				unsigned int s1len, unsigned int s2len, unsigned int s3len)
{
  int i, j, k;
  int ns_val = (*ns);
  int nlm_val = (*nlm);
  int FF = 7;

  for(k=0; k<nk; k++)
    for(j=0; j<nj; j++) {
      
      if((pcell[0][j][k] == FF) &&
	 (pcell[1][j][k] == FF))
	_gas_out->gas_cell.push_back(outlet_cell(1, j, k, 0,
						 ns_val, nlm_val,
						 sns, stb, sew,
						 part_flow, part_temp, part_size, part_var,
						 u, v, w, x, y, z,
						 eff, eta, chi,
						 t, p, h, den,
						 spec_val,
						 part_char, part_ash, part_water, part_coal,
						 hco, hwo, hao, hho,
						 ynu, tar, yc));
      else if((pcell[ni-1][j][k] == FF) &&
	      (pcell[ni-2][j][k] == FF))
	_gas_out->gas_cell.push_back(outlet_cell(ni-2, j, k, 1,
						 ns_val, nlm_val,
						 sns, stb, sew,
						 part_flow, part_temp, part_size, part_var,
						 u, v, w, x, y, z,
						 eff, eta, chi,
						 t, p, h, den,
						 spec_val,
						 part_char, part_ash, part_water, part_coal,
						 hco, hwo, hao, hho,
						 ynu, tar, yc));
    }
  
  fflush(NULL);
  for(k=1; k<nk-1; k++)
    for(i=1; i<ni-1; i++) {
      if((pcell[i][0][k] == FF) &&
	 (pcell[i][1][k] == FF))
	_gas_out->gas_cell.push_back(outlet_cell(i, 1, k, 2,
						 ns_val, nlm_val,
						 sns, stb, sew,
						 part_flow, part_temp, part_size, part_var,
						 u, v, w, x, y, z,
						 eff, eta, chi,
						 t, p, h, den,
						 spec_val,
						 part_char, part_ash, part_water, part_coal,
						 hco, hwo, hao, hho,
						 ynu, tar, yc));	
      else if((pcell[i][nj-1][k] == FF) &&
	      (pcell[i][nj-2][k] == FF))
	_gas_out->gas_cell.push_back(outlet_cell(i, nj-2, k, 3,
						 ns_val, nlm_val,
						 sns, stb, sew,
						 part_flow, part_temp, part_size, part_var,
						 u, v, w, x, y, z,
						 eff, eta, chi,
						 t, p, h, den,
						 spec_val,
						 part_char, part_ash, part_water, part_coal,
						 hco, hwo, hao, hho,
						 ynu, tar, yc));
    }
  
  for(j=1; j<nj-1; j++)
    for(i=1; i<ni-1; i++) {
      if((pcell[i][j][0] == FF) &&
	 (pcell[i][j][1] == FF)) 
	_gas_out->gas_cell.push_back(outlet_cell(i, j, 1, 4,
						 ns_val, nlm_val,
						 sns, stb, sew,
						 part_flow, part_temp, part_size, part_var,
						 u, v, w, x, y, z,
						 eff, eta, chi,
						 t, p, h, den,
						 spec_val,
						 part_char, part_ash, part_water, part_coal,
						 hco, hwo, hao, hho,
						 ynu, tar, yc));
      else if((pcell[i][j][nk-1] == FF) &&
	      (pcell[i][j][nk-2] == FF))
	_gas_out->gas_cell.push_back(outlet_cell(i, j, nk-2, 5,
						 ns_val, nlm_val,
						 sns, stb, sew,
						 part_flow, part_temp, part_size, part_var,
						 u, v, w, x, y, z,
						 eff, eta, chi,
						 t, p, h, den,
						 spec_val,
						 part_char, part_ash, part_water, part_coal,
						 hco, hwo, hao, hho,
						 ynu, tar, yc));
    }
  
  
  for(i=0; i<nlm_val; i++) {
    _gas_out->comp_wics.push_back((double)*(wic_val + i));
    _gas_out->wics[string(wic_name + i*9)] = i;
  }

  
  for(i=0; i<ns_val; i++)
    _gas_out->specie[string(spec_name + i*9)] = i;
   
  for(i=0; i<4; i++) 
    _gas_out->particle[string(part_name + i*9)] = i;

  _gas_out->hh0.push_back((double)(*hho));
  _gas_out->hh0.push_back((double)(*hao));
  _gas_out->hh0.push_back((double)(*hwo));
  _gas_out->hh0.push_back((double)(*hco));

  _gas_out->average();

  // Pressure at inlet is gage.
  _gas_out->pressure_drop = _pressure - _gas_out->gas_composite.P;
 
  // Summaries
  _summaries->insert_summary_val("Summary", 0.0);  
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

GasCell PipeCFD::outlet_cell(int i, int j, int k, int face,
			      int ns_val, int nlm_val,
			      float *sns, float *stb, float *sew,
			      float *part_flow, float *part_temp, float *part_size, float *part_var,
			      float *u, float *v, float *w, float *x, float *y, float *z,
			      float *eff, float *eta, float *chi,
			      float *t, float *p, float *h, float *den,
			      float *spec_val,
			      float *part_char, float *part_ash, float *part_water, float *part_coal,
			      float *hco, float *hwo, float *hao, float *hho,
			      float *ynu, float *tar, float *yc)
{
  GasCell cell(_gas_out);
  int sp;
  int nx_ny_k = nx*ny*k;
  int nx_j = nx*j;
  float density;

  // One of these is incorrect, see switch below for correction.
  cell.icell.push_back(i);
  cell.icell.push_back(j);
  cell.icell.push_back(k);

  // One of these is incorrect, see switch below for correction.
  cell.node_location.push_back((double)*(x + i));
  cell.node_location.push_back((double)*(y + j));
  cell.node_location.push_back((double)*(z + k));

  // If outlet cell is on a lower face (i,j or k=1), then
  // one of these is incorrect, see switch below for correction.
  cell.velocity.push_back((double)*(u + nx_ny_k + nx_j + i));
  cell.velocity.push_back((double)*(v + nx_ny_k + nx_j + i));   
  cell.velocity.push_back((double)*(w + nx_ny_k + nx_j + i));
 
  // This is 4-DIM array (NX,NY,NZ,NSP)
  for(sp=0; sp<ns_val; sp++)
    cell.comp_specie.push_back((double)*(spec_val + nx*ny*nz*sp
					  + nx_ny_k + nx_j + i));
  
  // Soot
  cell.soot = (double)*(yc + nx_ny_k + nx_j + i);
  cell.tar = (double)*(tar + nx_ny_k + nx_j + i);
  cell.ynu  = (double)*(ynu + nx_ny_k + nx_j + i);

  density = (double)*(den + nx_ny_k + nx_j + i);
  cell.eff = (double)*(eff + nx_ny_k + nx_j + i);
  cell.eta = (double)*(eta + nx_ny_k + nx_j + i);
  cell.chi = (double)*(chi + nx_ny_k + nx_j + i);
  cell.T = (double)*(t + nx_ny_k + nx_j + i);
  cell.P = (double)*(p + nx_ny_k + nx_j + i) + _pressure; // p is relative to mean
  //cell.H = (double)*(h + nx_ny_k + nx_j + i);
  cell.mean_size = (double)*(part_size + nx_ny_k + nx_j + i);
  cell.size_variance = (double)*(part_var + nx_ny_k + nx_j + i);
  cell.T_particle = (double)*(t + nx_ny_k + nx_j + i);
 
  // Outlet cells are on a face (ie. i-face has i=1 or i=ni).  But we
  // passed in the i,j,k for the adjacent interior cell (ie. i=2 or i=ni-1).
  // Normally this is where the "good" data is kept...but not always:
  //
  // node_location:  We definitely want the true (x,y,z) for this.
  // area: Computed differently depending upon face.
  // M: Computed using average of two cell densities (outlet cell and its adjacent
  //   interior), and cell area.
  // M_particle and comp_particle: In psolve.f these are only
  //   computed for - and stored at - outlet cells (interiors are ALL zero).
  // velocity: Stored offset up - ie. velocity for outlet cell (1,j,k) is stored
  //   at (2,j,k), so above is correct; but outlet cell (ni-1,j,k) is stored at
  //   (ni,j,k) so we must adjust.

  switch (face) {
  case 0: // i=1 face
    // velocity is correct from above calculation.
    cell.node_location[0]=(double)*(x + (i-1));
    cell.area = (*(sns + j)) * (*(stb + k));
    cell.M = ((density + (double)*(den + nx_ny_k + nx_j + (i-1))) / 2)
      * cell.velocity[0] * cell.area;
    break;
  case 1: // i=ni face
    cell.velocity[0]=(double)*(u + nx_ny_k + nx_j + (i+1));
    cell.node_location[0]=(double)*(x + (i+1));
    cell.area = (*(sns + j)) * (*(stb + k));
    cell.M = ((density + (double)*(den + nx_ny_k + nx_j + (i+1))) / 2)
      * cell.velocity[0] * cell.area;

    cell.M_particle = (double)*(part_flow + nx_ny_k + nx_j + (i+1));
    cell.comp_particle.push_back((double)*(part_coal + nx_ny_k + nx_j + (i+1)));
    cell.comp_particle.push_back((double)*(part_char + nx_ny_k + nx_j + (i+1)));
    cell.comp_particle.push_back((double)*(part_water + nx_ny_k + nx_j + (i+1)));
    cell.comp_particle.push_back((double)*(part_ash + nx_ny_k + nx_j + (i+1)));
    break;
  case 2: // j=1 face
    // velocity is correct from above calculation.
    cell.node_location[1]=(double)*(x + i);
    cell.area = (*(sew + i)) * (*(stb + k));
    cell.M = ((density + (double)*(den + nx_ny_k + nx_j + i)) / 2)
      * cell.velocity[1] * cell.area;
    break;
  case 3: // j=nj face
    cell.velocity[1]=(double)*(v + nx_ny_k + nx*(j+1) + i);   
    cell.node_location[1]=(double)*(x + i);
    cell.area = (*(sew + i)) * (*(stb + k));
    cell.M = ((density + (double)*(den + nx_ny_k + nx*(j+1) + i)) / 2)
      * cell.velocity[1] * cell.area;
    break;
  case 4: // k=1 face
    // velocity is correct from above calculation.
    cell.node_location[2]=(double)*(x + i);
    cell.area = (*(sew + i)) * (*(sns + j));
    cell.M = ((density + (double)*(den + nx*ny*(k-1) + nx_j + i)) / 2)
      * cell.velocity[2] * cell.area;
    break;
  case 5: // k=nk face 
    cell.velocity[2]=(double)*(w + nx*ny*(k+1) + nx_j + i);
    cell.node_location[2]=(double)*(x + i);
    cell.area = (*(sew + i)) * (*(sns + j));
    cell.M = ((density + (double)*(den + nx*ny*(k+1) + nx_j + i)) / 2)
      * cell.velocity[2] * cell.area;
    break;
  default: // Not on a face.
    cerr << "Outlet cell not on a face?/n";
    break;
  }
  return(cell);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void PipeCFD::send_scirun_specie(int *ns, float *spec_val, char *spec_name,
				  unsigned int slen)
{
  int i, j, k, sp;
  int ns_val = (*ns);
  vector<GasCell>::iterator cell;
  
  for (cell=_gas_out->gas_cell.begin(); cell!=_gas_out->gas_cell.end(); cell++) {
    (*cell).comp_specie.clear();
    for(sp=0; sp<ns_val; sp++) {
      i = (*cell).icell[0];
      j = (*cell).icell[1];
      k = (*cell).icell[2];
      (*cell).comp_specie.push_back
	((double)*(spec_val + nx*ny*nz*sp + nx*ny*k + nx*j + i)); 
    }
  }
  _gas_out->specie.clear();
  for(i=0; i<ns_val; i++)
    _gas_out->specie[string(spec_name + i*9)] = i;
  
  _gas_out->average();
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void PipeCFD::insert_summary_val(char *description, float *value,
				  unsigned int slen)
{
  _summaries->insert_summary_val(description, *value);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void PipeCFD::insert_xdata(char *xname, float *values, int *num_values,
			    unsigned int slen)
{
   // load x data entries
    vector<float> xvalues;
    for(int i=0; i<(*num_values); i++)
      xvalues.push_back(values[i]);
 
    // create an empty YDataMap object
    YDataMap ydm;
  
    profile_map_iter map_iter = plot_data_map.find(xname);
    if(map_iter != plot_data_map.end()) 
      plot_data_map.erase(map_iter);

    // construct a pair using xvalues and ydm
    pair<vector<float>, YDataMap> new_pair(xvalues,ydm);
    plot_data_map[xname] = new_pair;
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void PipeCFD::insert_ydata(char *yname, float *values, int *num_values,
			    char *xdata_name, unsigned int s1len, unsigned int s2len)
{
  // load y data entries
  vector<float> yvalues;
  for(int i=0; i<(*num_values);i++)
    yvalues.push_back((double)values[i]);
 
  profile_map_iter map_iter = plot_data_map.find(xdata_name);
  if(map_iter != plot_data_map.end()) {
    YDataMapIter ymap_iter = map_iter->second.second.find(yname);
    if (ymap_iter != map_iter->second.second.end())
      if ((unsigned)(*num_values) != map_iter->second.first.size()) {
	cerr << "ERROR: Y DATA SET SIZE != X DATA SET SIZE.\n";
	return;
      }
      else
	map_iter->second.second.erase(ymap_iter);
    map_iter->second.second[yname] = yvalues;
  }
  else
    cerr << "ERROR: XDATA SET NOT FOUND.\n";
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void PipeCFD::update_plot()
{
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void PipeCFD::load_scirun_slag(double *deltaw, double *deltar, double *kw, double *kr,
				double *ks, double *kd, double *ha, double *rhos,
				double *ta1, double *tcv, double *emiss, double *ashcomp)
{

}

void PipeCFD::load_scirun_flags (int *lrsrt, int *lprst)
{
  *lrsrt = _gas_restart;
  *lprst = _prt_restart;

  cerr << "GAS RESTART  = " << _gas_restart << endl;
  cerr << "PART RESTART = " << _prt_restart << endl;
}

void PipeCFD::load_oxidant (float *tf0, float *hsub0, float *densf0, float *erf0, float *smf0,
	  float *cpsf0, float *bf0, float *specf00, float *specf0, int *nel, int *nsp,
	  char *spec_name, char *wic_name, unsigned int s1len, unsigned int s2len)
{

}

//******************************************************************************
//******************************  C Functions **********************************
//******************************************************************************

int WIN_PREFIX gas_abort_status_()
{
  int ret;  // watch out for byte-sized bool's!
  
  if(PIPE_GLACIER_PTR->get_abort_glacier()) ret=1;
  else                                     ret=0;

  return(ret);
}

void WIN_PREFIX gas_load_geom_sr_(int* ni,int* nj,int* nk,int* nx,int* ny,int* nz,
		       float *x_vals,float *y_vals,float *z_vals,int *icell_array)
{
  PIPE_GLACIER_PTR->load_geom(ni,nj,nk,nx,ny,nz,x_vals,y_vals,z_vals,icell_array);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_update_sr_(int* iteration_number)
{
  PIPE_GLACIER_PTR->send_current_outputs(iteration_number);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_update_sr_nox_(int* iteration_number)
{
  PIPE_GLACIER_PTR->send_current_outputs_nox(iteration_number);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_load_current_traj_point_(int *IJKNT, float *XP,float *YP,
				  float *ZP, float *TIM, float *PNFRP,
				  float *SIGMAX,float *SIGMAY,float *SIGMAZ,
				  float *ALFT0P,float *PDIA,float *TMP,
				  float *SIGMA0)
{   
  PIPE_GLACIER_PTR->load_current_traj_point(XP, YP, ZP,
					   TIM, PNFRP,
					   SIGMAX, SIGMAY, SIGMAZ,
					   ALFT0P, PDIA, TMP,
					   SIGMA0);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_update_sr_begin_particles_()
{
  PIPE_GLACIER_PTR->update_sr_begin_particles();
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_update_sr_end_particles_(float* nix)
{
  PIPE_GLACIER_PTR->update_sr_end_particles(nix);
}
	
//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_sr_begin_particle_(int *ips, int *isl)
{
  PIPE_GLACIER_PTR->sr_begin_particle(ips,isl);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_sr_end_particle_(int *ips, int *isl, int *nps, int *nsl)
{
  PIPE_GLACIER_PTR->sr_end_particle(ips,isl,nps,nsl);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_load_scirun_wics_(float *wic, int *j, int *nlm, int *np, int *nel)
{
  PIPE_GLACIER_PTR->load_scirun_wics(wic, j, nlm, np, nel);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/	

void WIN_PREFIX gas_load_scirun_groups_(float *f, float *t, float *p, int* istage)
{
  PIPE_GLACIER_PTR->load_scirun_groups(f, t, p, istage);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_load_scirun_hhv_(float *omegal, float *yy, float *omegaa, float *hc0)
{
  PIPE_GLACIER_PTR->load_scirun_hhv(omegal, yy, omegaa, hc0);;
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_load_scirun_pd_(float *pd, float *pmf, int *nps, int *numstr)
{
  PIPE_GLACIER_PTR->load_scirun_pd(pd, pmf, nps, numstr);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_load_scirun_coalMT_(float *coal_flows, float *coal_temps)
{
  PIPE_GLACIER_PTR->load_scirun_coalMT(coal_flows, coal_temps);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_load_scirun_coal_(float *coal_flows)
{
  PIPE_GLACIER_PTR->load_scirun_coal(coal_flows);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

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
			   float *ynu, float *tar, float *yc,
			   unsigned int s1len, unsigned int s2len, unsigned int s3len)
{
  PIPE_GLACIER_PTR->send_scirun_data(ns, nlm,
				    sns, stb, sew,
				    part_flow, part_temp, part_size, part_var,
				    u, v, w, x, y, z,
				    eff, eta, chi,
				    t, p, h, den,
				    spec_val, wic_val,
				    part_char, part_ash, part_water, part_coal,
				    hco, hwo, hao, hho,
				    spec_name, wic_name, part_name,
				    press_in,
				    ht_conv, ht_netwall, ht_netexit,
			            ynu, tar, yc,
				    s1len, s2len, s3len);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_send_scirun_specie_(int *ns, float *spec_val, char *spec_name,
			     unsigned int slen)
{
  PIPE_GLACIER_PTR->send_scirun_specie(ns, spec_val, spec_name, slen);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_insert_summary_val_(char *description, float *value, unsigned int slen)
{
  PIPE_GLACIER_PTR->insert_summary_val(description, value, slen);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_insert_xdata_(char *xname, float *values, int *num_values,
		       unsigned int slen)
{
  PIPE_GLACIER_PTR->insert_xdata(xname, values, num_values, slen);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_insert_ydata_(char *yname, float *values, int *num_values,
		       char *xdata_name, unsigned int s1len, unsigned int s2len)
{
  PIPE_GLACIER_PTR->insert_ydata(yname, values, num_values, xdata_name, s1len, s2len);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_update_plot_()
{
  PIPE_GLACIER_PTR->update_plot();
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_update_dbfile_()
{
  PIPE_GLACIER_PTR->update_dbfile();
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_load_scirun_slag_(double *deltaw, double *deltar, double *kw, double *kr,
			   double *ks, double *kd, double *ha, double *rhos,
			   double *ta1, double *tcv, double *emiss, double *ashcomp)
{
  PIPE_GLACIER_PTR->load_scirun_slag(deltaw, deltar, kw, kr,
				    ks, kd, ha, rhos,
				    ta1, tcv, emiss, ashcomp);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_load_scirun_flags_ (int *lrsrt, int *lprst)
{  
  PIPE_GLACIER_PTR->load_scirun_flags (lrsrt, lprst);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_load_oxidant_ (float *tf0, float *hsub0, float *densf0, float *erf0, float *smf0,
	  float *cpsf0, float *bf0, float *specfoo, float *spec0, int *nel, int *nsp,
	  char *spec_name, char *wic_name, unsigned int s1len, unsigned int s2len)
{
  PIPE_GLACIER_PTR->load_oxidant (tf0, hsub0, densf0, erf0, smf0,
	  cpsf0, bf0, specfoo, spec0, nel, nsp,
	  spec_name, wic_name, s1len, s2len);
}
