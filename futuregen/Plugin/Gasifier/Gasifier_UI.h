#ifndef Gasifier_UI_H
#define Gasifier_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>
#include <wx/notebook.h>


using namespace std;

enum {
  R_STAGE1,
  R_STAGE2,
  SPEC_GEOM,
  DES_MODE
};

class GasiTabs : public wxNotebook
{
 public:
  GasiTabs(wxWindow *parent, wxWindowID id = -1,
       const wxPoint& pos = wxDefaultPosition,
       const wxSize& size = wxDefaultSize, long style = 0);
 
  void  CreateInitialPages();

 protected:
  wxPanel* CreateFirstPage();
  wxPanel* CreateSecondPage();

 public:
  wxTextCtrl* t_steam_temp1;
  wxTextCtrl* t_steam_flrt1;
  wxTextCtrl* t_slurry_temp1;
  wxTextCtrl* t_slurry_flrt1;
  wxTextCtrl* t_coal_percent1;
  wxTextCtrl* t_steam_temp2;
  wxTextCtrl* t_steam_flrt2;
  wxTextCtrl* t_slurry_temp2;
  wxTextCtrl* t_slurry_flrt2;
  wxTextCtrl* t_coal_percent2;
  wxTextCtrl* t_steam_temp3;
  wxTextCtrl* t_steam_flrt3;
  wxTextCtrl* t_slurry_temp3;
  wxTextCtrl* t_slurry_flrt3;
  wxTextCtrl* t_coal_percent3;
  wxTextCtrl* t_geo_diam;
  wxTextCtrl* t_geo_stage1_len;
  wxTextCtrl* t_geo_stage2_len;
  wxTextCtrl* t_geo_stage1_wall;
  wxTextCtrl* t_geo_stage2_wall;
  wxTextCtrl* t_burn_out;
  wxTextCtrl* t_stage1_heatloss;
  wxTextCtrl* t_stage2_heatloss;
  wxTextCtrl* t_LD_ratio;
  wxTextCtrl* t_stage1_emis;
  wxTextCtrl* t_stage2_emis;
  wxTextCtrl* t_backside_temp;
  wxTextCtrl* t_slag_eff;
  wxTextCtrl* t_pres_drop;

  wxRadioButton* r_stage1;
  wxRadioButton* r_stage2;

  wxCheckBox* cb_spec_geometry;
  wxCheckBox* cb_des_mode; 

  void OnChangeStage(wxCommandEvent &event);
  void OnChangeGeom(wxCommandEvent &event);
  void OnChangeMode(wxCommandEvent &event);

  DECLARE_EVENT_TABLE()
};


class Gasifier_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(Gasifier_UI_Dialog);
 public:
  Gasifier_UI_Dialog(wxWindow* parent, int id,
          double* steam_temp1,
          double* steam_flrt1,
          double* slurry_temp1,
          double* slurry_flrt1,
          double* coal_percent1,
          double* steam_temp2,
          double* steam_flrt2,
          double* slurry_temp2,
          double* slurry_flrt2,
          double* coal_percent2,
          double* steam_temp3,
          double* steam_flrt3,
          double* slurry_temp3,
          double* slurry_flrt3,
          double* coal_percent3,
          double* geo_diam,
          double* geo_stage1_len,
          double* geo_stage2_len,
          double* geo_stage1_wall,
          double* geo_stage2_wall,
          double* burn_out,
          double* stage1_heatloss,
          double* stage2_heatloss,
          double* LD_ratio,
          double* stage1_emis,
          double* stage2_emis,
          double* backside_temp,
          double* slag_eff,
	  double* pres_drop,   		     
          long* stage,
          long* spec_geometry,
          long* des_mode);
  Gasifier_UI_Dialog() {};
  virtual ~Gasifier_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  GasiTabs *m_tabs;
  wxNotebookSizer *m_sizerNotebook;
 public:
  double* p_steam_temp1;
  double* p_steam_flrt1;
  double* p_slurry_temp1;
  double* p_slurry_flrt1;
  double* p_coal_percent1;
  double* p_steam_temp2;
  double* p_steam_flrt2;
  double* p_slurry_temp2;
  double* p_slurry_flrt2;
  double* p_coal_percent2;
  double* p_steam_temp3;
  double* p_steam_flrt3;
  double* p_slurry_temp3;
  double* p_slurry_flrt3;
  double* p_coal_percent3;
  double* p_geo_diam;
  double* p_geo_stage1_len;
  double* p_geo_stage2_len;
  double* p_geo_stage1_wall;
  double* p_geo_stage2_wall;
  double* p_burn_out;
  double* p_stage1_heatloss;
  double* p_stage2_heatloss;
  double* p_LD_ratio;
  double* p_stage1_emis;
  double* p_stage2_emis;
  double* p_backside_temp;
  double* p_slag_eff;
  double* p_pres_drop;
  long* p_stage;
  long* p_spec_geometry;
  long* p_des_mode;
  //GUI Variables
};

#endif

