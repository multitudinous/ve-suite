#ifndef FUEL_DIALOG_H
#define FUEL_DIALOG_H

#include <wx/wx.h>
#include <vector>
#include "UIDialog.h"

using namespace std;

enum {
  COMBO_COAL,
  COAL_NAME,
  COAL_SAVE_B,
  ULTI_C,
  ULTI_H,
  ULTI_O,
  ULTI_N,
  ULTI_S,
  ULTI_CL,
  ULTI_ASH,
  ULTI_NORM_B,
  PROX_H2O,
  PROX_VM,
  PROX_ASH,
  PROX_FC,
  PROX_CALC_B,
  ASHC_SiO2,
  ASHC_Al2O3,
  ASHC_TiO2,
  ASHC_Fe2O3,
  ASHC_CaO,
  ASHC_MgO,
  ASHC_Na2O,
  ASHC_K2O,
  ASHC_SO3,
  ASHC_P2O5,
  ASHC_BaO,
  ASHC_SrO,
  ASHC_NORM_B,
  HHV,
  COAL_COST,
  COAL_COST2,
  COAL_COST3
};

//
class FuelDialog : public UIDialog
{
 public:
  FuelDialog(wxWindow *parent, wxWindowID id ,
	     string *coal_name_s,
	     double *ulti_c_d,
	     double *ulti_h_d,
	     double *ulti_o_d,
	     double *ulti_n_d,
	     double *ulti_s_d,
	     double *ulti_cl_d,
	     double *ulti_ash_d,
	     double *prox_h2o_d,
	     double *prox_vm_d,
	     double *prox_ash_d,
	     double *prox_fc_d,
	     double *ashc_sio2_d,
	     double *ashc_al2o3_d,
	     double *ashc_tio2_d,
	     double *ashc_fe2o3_d,
	     double *ashc_cao_d,
	     double *ashc_mgo_d,
	     double *ashc_na2o_d,
	     double *ashc_k2o_d,
	     double *ashc_so3_d,
	     double *ashc_p2o5_d,
	     double *ashc_bao_d,
	     double *ashc_sro_d,
	     double *hhv_d,
	     double *coal_cost_d	     
	     );
  ~FuelDialog();

  wxComboBox *combo_coal;
  wxTextCtrl *coal_name;
  wxTextCtrl *ulti_c;
  wxTextCtrl *ulti_h;
  wxTextCtrl *ulti_o;
  wxTextCtrl *ulti_n;
  wxTextCtrl *ulti_s;
  wxTextCtrl *ulti_cl;
  wxTextCtrl *ulti_ash;
  wxTextCtrl *prox_h2o;
  wxTextCtrl *prox_vm;
  wxTextCtrl *prox_ash;
  wxTextCtrl *prox_fc;
  wxTextCtrl *ashc_sio2;
  wxTextCtrl *ashc_al2o3;
  wxTextCtrl *ashc_tio2;
  wxTextCtrl *ashc_fe2o3;
  wxTextCtrl *ashc_cao;
  wxTextCtrl *ashc_mgo;
  wxTextCtrl *ashc_na2o;
  wxTextCtrl *ashc_k2o;
  wxTextCtrl *ashc_so3;
  wxTextCtrl *ashc_p2o5;
  wxTextCtrl *ashc_bao;
  wxTextCtrl *ashc_sro;
  wxTextCtrl *hhv;
  wxTextCtrl *coal_cost;
  wxTextCtrl *coal_cost2;
  wxTextCtrl *coal_cost3;
  wxButton * coal_save_b;
  wxButton * ulti_norm_b;
  wxButton * prox_calc_b;
  wxButton * ashc_norm_b;
  wxButton * ok_b;
  //  wxButton * cancel_b;

  virtual bool TransferDataToWindow();
  virtual bool TransferDataFromWindow();

  //  void UnPack(wxString inp);
  //  wxString Pack();
 public:
  string  *coal_name_s_;
  double *ulti_c_d_;
  double *ulti_h_d_;
  double *ulti_o_d_;
  double *ulti_n_d_;
  double *ulti_s_d_;
  double *ulti_cl_d_;
  double *ulti_ash_d_;
  double *prox_h2o_d_;
  double *prox_vm_d_;
  double *prox_ash_d_;
  double *prox_fc_d_;
  double *ashc_sio2_d_;
  double *ashc_al2o3_d_;
  double *ashc_tio2_d_;
  double *ashc_fe2o3_d_;
  double *ashc_cao_d_;
  double *ashc_mgo_d_;
  double *ashc_na2o_d_;
  double *ashc_k2o_d_;
  double *ashc_so3_d_;
  double *ashc_p2o5_d_;
  double *ashc_bao_d_;
  double *ashc_sro_d_;
  double *hhv_d_;
  double *coal_cost_d_;

 protected:
  vector<string> coal_name_v;
  vector<double> ulti_c_v;
  vector<double> ulti_h_v;
  vector<double> ulti_o_v;
  vector<double> ulti_n_v;
  vector<double> ulti_s_v;
  vector<double> ulti_cl_v;
  vector<double> ulti_ash_v;
  vector<double> prox_h2o_v;
  vector<double> prox_vm_v;
  vector<double> prox_ash_v;
  vector<double> prox_fc_v;
  vector<double> ashc_sio2_v;
  vector<double> ashc_al2o3_v;
  vector<double> ashc_tio2_v;
  vector<double> ashc_fe2o3_v;
  vector<double> ashc_cao_v;
  vector<double> ashc_mgo_v;
  vector<double> ashc_na2o_v;
  vector<double> ashc_k2o_v;
  vector<double> ashc_so3_v;
  vector<double> ashc_p2o5_v;
  vector<double> ashc_bao_v;
  vector<double> ashc_sro_v;
  vector<double> hhv_v;
  vector<double> coal_cost_v;
 protected:

  void OnChooseCoal(wxCommandEvent& event);
  void OnSaveCoal(wxCommandEvent& event);
  void OnNormUlti(wxCommandEvent& event);
  void OnCalcProx(wxCommandEvent& event);
  void OnNormAsh(wxCommandEvent& event);
  void ChangeCost2(wxCommandEvent& event);
  
  void double2entry(wxTextCtrl* entry, double * value);
  void entry2double(wxTextCtrl* entry, double * value);

  void readDatabase(std::string file);
  bool notready;
  DECLARE_EVENT_TABLE()
};

   
#endif
