#ifndef Prekin_UI_H
#define Prekin_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>
#include <wx/notebook.h>

using namespace std;

enum {
	
  PORE_MODEL1,
  PORE_MODEL2,
  MOD_SEL,
  MANUAL_INPUT,
  OXIDATION_FLAG1,
  OXIDATION_FLAG2,
  MIR,
  GASIFICATION_FLAG1,
  GASIFICATION_FLAG2,
  FOPL_CH,
  LHK_CH    
};

class PrekinTabs : public wxNotebook
{
 public:
  PrekinTabs(wxWindow *parent, wxWindowID id = -1,
       const wxPoint& pos = wxDefaultPosition,
       const wxSize& size = wxDefaultSize, long style = 0);
 
  void  CreateInitialPages();

 protected:
  wxPanel* CreateFirstPage();
  wxPanel* CreateSecondPage();
  wxPanel* CreateThirdPage();
  wxPanel* CreateForthPage();
  wxPanel* CreateFifthPage();

 public:
  
  wxTextCtrl* t_mode_burning;
  wxTextCtrl* t_linear_swell;
  wxTextCtrl* t_fuel_carbon;
  wxTextCtrl* t_ash_film;
  wxTextCtrl* t_ash_grain_size;
  wxTextCtrl* t_ash_therm_cond;
  wxTextCtrl* t_size_50;
  wxTextCtrl* t_size_200;
  wxTextCtrl* t_T_f;
  wxTextCtrl* t_pore_radii_macro;
  wxTextCtrl* t_pore_radii_micro;
  wxTextCtrl* t_pore_macroposity;
  wxTextCtrl* t_pore_porosity;
  wxTextCtrl* t_CPD_AB;
  wxTextCtrl* t_CPD_AC;
  wxTextCtrl* t_CPD_AG;
  wxTextCtrl* t_CPD_ACR;
  wxTextCtrl* t_CPD_EB;
  wxTextCtrl* t_CPD_EC;
  wxTextCtrl* t_CPD_EG;
  wxTextCtrl* t_CPD_ECR;
  wxTextCtrl* t_CPD_EBSIG;
  wxTextCtrl* t_CPD_EGSIG;
  wxTextCtrl* t_TS_A1;
  wxTextCtrl* t_TS_A2;
  wxTextCtrl* t_TS_E1;
  wxTextCtrl* t_TS_E2;
  wxTextCtrl* t_TS_Y1;
  wxTextCtrl* t_TS_Y2;
  wxTextCtrl* t_MI_P0;
  wxTextCtrl* t_MI_C0;
  wxTextCtrl* t_MI_SIGP1;
  wxTextCtrl* t_MI_MW;
  wxTextCtrl* t_MDEL;
  wxTextCtrl* t_heat_rate;
  wxTextCtrl* t_max_temp;
  wxTextCtrl* t_res_time;
  wxTextCtrl* t_num_grid_heating;
  wxTextCtrl* t_num_grid_isothermal;
  wxTextCtrl* t_MIR_koso;
  wxTextCtrl* t_MIR_ko;
  wxTextCtrl* t_MIR_so;
  wxTextCtrl* t_MIR_IAE;
  wxTextCtrl* t_MIR_IRO;
  wxTextCtrl* t_MIR_k3o;
  wxTextCtrl* t_MIR_k2ok3o;
  wxTextCtrl* t_MIR_k3ok1o;
  wxTextCtrl* t_MIR_E1;
  wxTextCtrl* t_MIR_E2;
  wxTextCtrl* t_MIR_E3;
  wxTextCtrl* t_IRO_Step2;
  wxTextCtrl* t_Aco;
  wxTextCtrl* t_Eco;
  wxTextCtrl* t_FOPL_ko;
  wxTextCtrl* t_FOPL_so;
  wxTextCtrl* t_FOPL_IAE;
  wxTextCtrl* t_FOPL_IRO;
  wxTextCtrl* t_LHK_k1o;
  wxTextCtrl* t_LHK_k2o;
  wxTextCtrl* t_LHK_k3o;
  wxTextCtrl* t_LHK_E1;
  wxTextCtrl* t_LHK_E2;
  wxTextCtrl* t_LHK_E3;
  wxTextCtrl* t_PR_ratio_fr;
  wxTextCtrl* t_PR_ratio_to;
  wxTextCtrl* t_num_steps;
  wxTextCtrl* t_mean_rxn_temp;
  wxTextCtrl* t_mrt_error;
  wxTextCtrl* t_mrt_step;
  wxTextCtrl* t_reac_frac_fr;
  wxTextCtrl* t_reac_frac_to;
  wxTextCtrl* t_reac_pres_step;
  wxTextCtrl* t_total_pres;
  wxTextCtrl* t_time_intv;
  wxTextCtrl* t_time_step;
  wxTextCtrl* t_conv_level;
  wxTextCtrl* t_optim_kG;
  wxTextCtrl* t_optim_EG;
  wxTextCtrl* t_optim_m;
  wxTextCtrl* t_tolerance;

  wxComboBox* cb_coal_name;
  wxComboBox* cb_FOPL;
  wxComboBox* cb_LHK;

  wxRadioButton* rb_Pore_Model1;
  wxRadioButton* rb_Pore_Model2;
  
  wxRadioBox* rb_mod_sel;
  
  wxCheckBox* cbo_manual_input;

  wxRadioButton* rb_oxidation_flag1;
  wxRadioButton* rb_oxidation_flag2;

  wxCheckBox* cbo_MIR;

  wxRadioButton* rb_Gasification_flag1;
  wxRadioButton* rb_Gasification_flag2;

  wxRadioBox* rb_FOPL_CH;
  
  wxRadioBox* rb_LHK_CH;
  
  wxRadioBox* rb_Schema;

  //========================
  void OnPoreModel(wxCommandEvent &event);
  void Onmod_sel(wxCommandEvent &event);
  void Onmanual_input(wxCommandEvent &event);

  void Onoxidation_flag(wxCommandEvent &event);
  
  void OnMIR(wxCommandEvent &event);

  void OnGasification_flag(wxCommandEvent &event);
  void OnFOPL_CH(wxCommandEvent &event);
  
  void OnLHK_CH(wxCommandEvent &event);
  

  DECLARE_EVENT_TABLE()
};

class Prekin_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(Prekin_UI_Dialog);
 public:
  Prekin_UI_Dialog(wxWindow* parent, int id,
          double* mode_burning,
          double* linear_swell,
          double* fuel_carbon,
          double* ash_film,
          double* ash_grain_size,
          double* ash_therm_cond,
          double* size_50,
          double* size_200,
          double* T_f,
          double* pore_radii_macro,
          double* pore_radii_micro,
          double* pore_macroposity,
          double* pore_porosity,
          double* CPD_AB,
          double* CPD_AC,
          double* CPD_AG,
          double* CPD_ACR,
          double* CPD_EB,
          double* CPD_EC,
          double* CPD_EG,
          double* CPD_ECR,
          double* CPD_EBSIG,
          double* CPD_EGSIG,
          double* TS_A1,
          double* TS_A2,
          double* TS_E1,
          double* TS_E2,
          double* TS_Y1,
          double* TS_Y2,
          double* MI_P0,
          double* MI_C0,
          double* MI_SIGP1,
          double* MI_MW,
          double* MDEL,
          double* heat_rate,
          double* max_temp,
          double* res_time,
          double* num_grid_heating,
          double* num_grid_isothermal,
          double* MIR_koso,
          double* MIR_ko,
          double* MIR_so,
		  double* MIR_IAE,
          double* MIR_IRO,
          double* MIR_k3o,
          double* MIR_k2ok3o,
          double* MIR_k3ok1o,
          double* MIR_E1,
          double* MIR_E2,
          double* MIR_E3,
          double* IRO_Step2,
          double* Aco,
          double* Eco,
          double* FORL_ko,
          double* FORL_so,
          double* FORL_IAE,
          double* FORL_IRO,
          double* LHK_k1o,
          double* LHK_k2o,
          double* LHK_k3o,
          double* LHK_E1,
          double* LHK_E2,
          double* LHK_E3,
          double* PR_ratio_fr,
          double* PR_ratio_to,
          double* num_steps,
          double* mean_rxn_temp,
          double* mrt_error,
          double* mrt_step,
          double* reac_frac_fr,
          double* reac_frac_to,
          double* reac_pres_step,
          double* total_pres,
          double* time_intv,
          double* time_step,
          double* conv_level,
          double* optim_kG,
          double* optim_EG,
          double* optim_m,
          double* tolerance,
          string* coal_name,
          string* FORL,
          string* LHK,
          long* Pore_Model,
          long* mod_sel,
          long* manual_input,
          long* oxidation_flag,
          long* MIR,
          long* Gasification_flag,
          long* FORL_CH,
          long* LHK_CH,
          long* Schema);
  Prekin_UI_Dialog() {};
  
  virtual ~Prekin_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  PrekinTabs *m_tabs;
  wxNotebookSizer *m_sizerNotebook;
  
 public:
  double* p_mode_burning;
  double* p_linear_swell;
  double* p_fuel_carbon;
  double* p_ash_film;
  double* p_ash_grain_size;
  double* p_ash_therm_cond;
  double* p_size_50;
  double* p_size_200;
  double* p_T_f;
  double* p_pore_radii_macro;
  double* p_pore_radii_micro;
  double* p_pore_macroposity;
  double* p_pore_porosity;
  double* p_CPD_AB;
  double* p_CPD_AC;
  double* p_CPD_AG;
  double* p_CPD_ACR;
  double* p_CPD_EB;
  double* p_CPD_EC;
  double* p_CPD_EG;
  double* p_CPD_ECR;
  double* p_CPD_EBSIG;
  double* p_CPD_EGSIG;
  double* p_TS_A1;
  double* p_TS_A2;
  double* p_TS_E1;
  double* p_TS_E2;
  double* p_TS_Y1;
  double* p_TS_Y2;
  double* p_MI_P0;
  double* p_MI_C0;
  double* p_MI_SIGP1;
  double* p_MI_MW;
  double* p_MDEL;
  double* p_heat_rate;
  double* p_max_temp;
  double* p_res_time;
  double* p_num_grid_heating;
  double* p_num_grid_isothermal;
  double* p_MIR_koso;
  double* p_MIR_ko;
  double* p_MIR_so;
  double* p_MIR_IAE;
  double* p_MIR_IRO;
  double* p_MIR_k3o;
  double* p_MIR_k2ok3o;
  double* p_MIR_k3ok1o;
  double* p_MIR_E1;
  double* p_MIR_E2;
  double* p_MIR_E3;
  double* p_IRO_Step2;
  double* p_Aco;
  double* p_Eco;
  double* p_FORL_ko;
  double* p_FORL_so;
  double* p_FORL_IAE;
  double* p_FORL_IRO;
  double* p_LHK_k1o;
  double* p_LHK_k2o;
  double* p_LHK_k3o;
  double* p_LHK_E1;
  double* p_LHK_E2;
  double* p_LHK_E3;
  double* p_PR_ratio_fr;
  double* p_PR_ratio_to;
  double* p_num_steps;
  double* p_mean_rxn_temp;
  double* p_mrt_error;
  double* p_mrt_step;
  double* p_reac_frac_fr;
  double* p_reac_frac_to;
  double* p_reac_pres_step;
  double* p_total_pres;
  double* p_time_intv;
  double* p_time_step;
  double* p_conv_level;
  double* p_optim_kG;
  double* p_optim_EG;
  double* p_optim_m;
  double* p_tolerance;
  string* p_coal_name;
  string* p_FORL;
  string* p_LHK;
  long* p_Pore_Model;
  long* p_mod_sel;
  long* p_manual_input;
  long* p_oxidation_flag;
  long* p_MIR;
  long* p_Gasification_flag;
  long* p_FORL_CH;
  long* p_LHK_CH;
  long* p_Schema;

  void double2entry(wxTextCtrl* entry, double * value);
  void entry2double(wxTextCtrl* entry, double * value);
  //GUI Variables
  
};

#endif

