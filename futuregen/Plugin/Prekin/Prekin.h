#ifndef Prekin_H
#define Prekin_H

#include "Plugin_base.h"
#include "wx/image.h"

#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)

#include <vector>
#include <string>
using namespace std;

class Prekin : public REI_Plugin
{
  DECLARE_DYNAMIC_CLASS(Prekin)

 public:

  Prekin();
  ~Prekin();

  virtual double GetVersion();
  //Return the version number of the module

  virtual void DrawIcon(wxDC* dc);
  //This call return a window to be displayed on the framework
  
  //To Get around the Memory allocation problem of windows dll
  //Add the calls for the size. So the main program can preallocate memory for it

  virtual int GetNumPoly();
  
  //virtual void GetPoly(POLY &polygon); 
  //Return the outline polygon

  virtual UIDialog* UI(wxWindow* parent);
  //This returns the UI dialog of the module

  virtual wxString GetName();
  //This returns the name of the module

  virtual wxString GetDesc();
  //This returns the description of the module, This should be a short description
  virtual wxString GetHelp();

  //To Get around the Memory allocation problem of windows dll
  //Add the calls for the size. So the main program can preallocate memory for it

  virtual int GetNumIports();
  virtual void GetIPorts(POLY& ports);

  virtual int GetNumOports();
  virtual void GetOPorts(POLY& ports);

 public:
  double mode_burning;
  double linear_swell;
  double fuel_carbon;
  double ash_film;
  double ash_grain_size;
  double ash_therm_cond;
  double size_50;
  double size_200;
  double T_f;
  double pore_radii_macro;
  double pore_radii_micro;
  double pore_macroposity;
  double pore_porosity;
  double HTVL;
  double CPD_AB;
  double CPD_AC;
  double CPD_AG;
  double CPD_ACR;
  double CPD_EB;
  double CPD_EC;
  double CPD_EG;
  double CPD_ECR;
  double CPD_EBSIG;
  double CPD_EGSIG;
  double TS_A1;
  double TS_A2;
  double TS_E1;
  double TS_E2;
  double TS_Y1;
  double TS_Y2;
  double MI_P0;
  double MI_C0;
  double MI_SIGP1;
  double MI_MW;
  double MDEL;
  double heat_rate;
  double max_temp;
  double res_time;
  double num_grid_heating;
  double num_grid_isothermal;
  double MIR_koso;
  double MIR_ko;
  double MIR_so;
  double MIR_IAE;
  double MIR_IRo;
  double MIR_k3o;
  double MIR_k2ok3o;
  double MIR_k3ok1o;
  double MIR_E1;
  double MIR_E2;
  double MIR_E3;
  double IRO_Step2;
  double Aco;
  double Eco;
  double FORL_ko;
  double FORL_so;
  double FORL_IAE;
  double FORL_IRO;
  double LHK_k1o;
  double LHK_k2o;
  double LHK_k3o;
  double LHK_E1;
  double LHK_E2;
  double LHK_E3;
  double PR_ratio_fr;
  double PR_ratio_to;
  double num_steps;
  double mean_rxn_temp;
  double mrt_error;
  double mrt_step;
  double reac_frac_fr;
  double reac_frac_to;
  double reac_pres_step;
  double total_pres;
  double time_intv;
  double time_step;
  double conv_level;
  double optim_kG;
  double optim_EG;
  double optim_m;
  double tolerance;
  string coal_name;
  string FORL;
  string LHK;
  long Pore_Model;
  long mod_sel;
  long manual_input;
  long oxidation_flag;
  long MIR;
  long Gasification_flag;
  long FORL_CH;
  long LHK_CH;
  long Schema;
  //HERE is the GUI variable passed to the Dialog and Packed
  
protected:
  wxBitmap *my_icon;
  int icon_w, icon_h; 
};

#endif

