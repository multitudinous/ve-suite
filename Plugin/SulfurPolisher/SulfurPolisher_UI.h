#ifndef SulfurPolisher_UI_H
#define SulfurPolisher_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

enum {
  R_H2S_EFF,
  R_H2S_PPM,
  R_COS_EFF,
  R_COS_PPM,
  R_PRES_SPEC,
  R_PRES_CALC
};

class SulfurPolisher_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(SulfurPolisher_UI_Dialog);
 public:
  SulfurPolisher_UI_Dialog(wxWindow* parent, int id,
          double* Temp_change,
          double* H2S_eff,
          double* H2S_ppm,
          double* COS_eff,
          double* COS_ppm,
          double* Pres_drop,
          double* Bed_diameter,
          double* Bed_depth,
          double* Bed_void_frac,
          double* Particle_size,
          double* Particle_sphericity,
          long* rH2S_eff_ppm,
          long* rCOS_eff_ppm,
          long* rPresDrop_spec_calc);
  SulfurPolisher_UI_Dialog() {};
  
  virtual ~SulfurPolisher_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables

  wxTextCtrl* t_Temp_change;
  wxTextCtrl* t_H2S_eff;
  wxTextCtrl* t_H2S_ppm;
  wxTextCtrl* t_COS_eff;
  wxTextCtrl* t_COS_ppm;
  wxTextCtrl* t_Pres_drop;
  wxTextCtrl* t_Bed_diameter;
  wxTextCtrl* t_Bed_depth;
  wxTextCtrl* t_Bed_void_frac;
  wxTextCtrl* t_Particle_size;
  wxTextCtrl* t_Particle_sphericity;
 
  wxRadioButton* r_H2S_eff;
  wxRadioButton* r_H2S_ppm;
  wxRadioButton* r_COS_eff;
  wxRadioButton* r_COS_ppm;
  wxRadioButton* r_PresDrop_spec;
  wxRadioButton* r_PresDrop_calc;

 public:
  double* p_Temp_change;
  double* p_H2S_eff;
  double* p_H2S_ppm;
  double* p_COS_eff;
  double* p_COS_ppm;
  double* p_Pres_drop;
  double* p_Bed_diameter;
  double* p_Bed_depth;
  double* p_Bed_void_frac;
  double* p_Particle_size;
  double* p_Particle_sphericity;
  long* p_rH2S_eff_ppm;
  long* p_rCOS_eff_ppm;
  long* p_rPresDrop_spec_calc;
  //GUI Variables

  void OnH2SChange(wxCommandEvent &event);
  void OnCOSChange(wxCommandEvent &event);
  void OnPRESChange(wxCommandEvent &event);
  DECLARE_EVENT_TABLE()
};

#endif

