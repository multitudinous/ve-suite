#ifndef MembraneReactor_UI_H
#define MembraneReactor_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

enum {
  R_CASETYPE_EVA,
  R_CASETYPE_DES,
  F_PRE_MR,
  F_H2O_CO
};

class MembraneReactor_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(MembraneReactor_UI_Dialog);
 public:
  MembraneReactor_UI_Dialog(wxWindow* parent, int id,
          double* memb_diameter,
          double* Pd_thickness,
          double* L_rxr,
          double* CO_conv_want,
          double* shell_diameter,
          double* mr_inlet_temp,
          double* H2O_CO,
          long* case_type,
          long* n_modules,
          long* f_pre_mr,
          long* f_H2O_CO);
  MembraneReactor_UI_Dialog() {};
  
  virtual ~MembraneReactor_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  wxTextCtrl* t_memb_diameter;
  wxTextCtrl* t_Pd_thickness;
  wxTextCtrl* t_L_rxr;
  wxTextCtrl* t_CO_conv_want;
  wxTextCtrl* t_n_modules;
  wxTextCtrl* t_shell_diameter;
  wxTextCtrl* t_mr_inlet_temp;
  wxTextCtrl* t_H2O_CO;

  wxRadioButton* r_case_type_eva;
  wxRadioButton* r_case_type_des;
  wxCheckBox* c_f_pre_mr;
  wxCheckBox* c_f_H2O_CO;

 public:
  double* p_memb_diameter;
  double* p_Pd_thickness;
  double* p_L_rxr;
  double* p_CO_conv_want;
  double* p_shell_diameter;
  double* p_mr_inlet_temp;
  double* p_H2O_CO;
  long* p_case_type;
  long* p_n_modules;
  long* p_f_pre_mr;
  long* p_f_H2O_CO;
  //GUI Variables
  void OnF_PRE_MRChange(wxCommandEvent &event);
  void OnF_H2O_COChange(wxCommandEvent &event);
  void OnCaseTypeChange(wxCommandEvent &event);

  DECLARE_EVENT_TABLE()
};

#endif

