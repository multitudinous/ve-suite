#ifndef ChlorineBed_UI_H
#define ChlorineBed_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

enum {
  R_HCL_EFF,
  R_HCL_PPM,
  R_PRES_SPEC,
  R_PRES_CALC
};

class ChlorineBed_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(ChlorineBed_UI_Dialog);
 public:
  ChlorineBed_UI_Dialog(wxWindow* parent, int id,
          double* Temp_change,
          double* HCL_eff,
          double* HCL_ppm,
          double* Pres_drop,
          double* Bed_diameter,
          double* Bed_depth,
          double* Bed_void_frac,
          double* Particle_size,
          double* Particle_sphericity,
          long* rHCL_eff_ppm,
          long* rPresDrop_spec_calc);
  ChlorineBed_UI_Dialog() {};
  
  virtual ~ChlorineBed_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  
  wxTextCtrl* t_Temp_change;
  wxTextCtrl* t_HCL_eff;
  wxTextCtrl* t_HCL_ppm;
  wxTextCtrl* t_Pres_drop;
  wxTextCtrl* t_Bed_diameter;
  wxTextCtrl* t_Bed_depth;
  wxTextCtrl* t_Bed_void_frac;
  wxTextCtrl* t_Particle_size;
  wxTextCtrl* t_Particle_sphericity;

  wxRadioButton* r_HCL_eff;
  wxRadioButton* r_HCL_ppm;
  wxRadioButton* r_PresDrop_spec;
  wxRadioButton* r_PresDrop_calc;

 public:
  double* p_Temp_change;
  double* p_HCL_eff;
  double* p_HCL_ppm;
  double* p_Pres_drop;
  double* p_Bed_diameter;
  double* p_Bed_depth;
  double* p_Bed_void_frac;
  double* p_Particle_size;
  double* p_Particle_sphericity;
  long* p_rHCL_eff_ppm;
  long* p_rPresDrop_spec_calc;
  //GUI Variables

  void OnHCLChange(wxCommandEvent &event);
  void OnPRESChange(wxCommandEvent &event);
  DECLARE_EVENT_TABLE()
  
};

#endif

