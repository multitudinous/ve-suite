#ifndef SOFC1D_UI_H
#define SOFC1D_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>
#include <wx/bmpbuttn.h>
using namespace std;

enum {
  BROWSE
};

class SOFC1D_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(SOFC1D_UI_Dialog);
 public:
  SOFC1D_UI_Dialog(wxWindow* parent, int id,
          double* a_height,
          double* a_width,
          double* a_space,
          double* a_ecd,
          double* a_tcoeff,
          double* a_thick,
          double* a_presdrop,
          double* c_height,
          double* c_width,
          double* c_space,
          double* c_ecdb,
          double* c_ecdm,
          double* c_tcoeffb,
          double* c_tcoeffm,
          double* c_thick,
          double* c_presdrop,
          double* s_thick,
          double* s_heatcap,
          double* s_density,
          double* e_thick,
          double* e_preexp,
          double* e_exp,
          double* f_preexp,
          double* f_exp,
          double* a_preexp,
          double* a_exp,
          double* i_preexp,
          double* i_exp,
          double* l_heatcap,
          double* l_density,
          double* l_length,
          double* l_width,
          double* stop_time,
          double* loadres,
          string* work_dir,
          long* l_numCells,
          long* ax_nodes);
  SOFC1D_UI_Dialog() {};
  
  virtual ~SOFC1D_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  wxTextCtrl* t_work_dir;

  wxTextCtrl* t_a_height;
  wxTextCtrl* t_a_width;
  wxTextCtrl* t_a_space;
  wxTextCtrl* t_a_ecd;
  wxTextCtrl* t_a_tcoeff;
  wxTextCtrl* t_a_thick;
  wxTextCtrl* t_a_presdrop;

  wxTextCtrl* t_c_height;
  wxTextCtrl* t_c_width;
  wxTextCtrl* t_c_space;
  wxTextCtrl* t_c_ecdb;
  wxTextCtrl* t_c_ecdm;
  wxTextCtrl* t_c_tcoeffb;
  wxTextCtrl* t_c_tcoeffm;
  wxTextCtrl* t_c_thick;
  wxTextCtrl* t_c_presdrop;

  wxTextCtrl* t_s_thick;
  wxTextCtrl* t_s_heatcap;
  wxTextCtrl* t_s_density;

  wxTextCtrl* t_e_thick;
  wxTextCtrl* t_e_preexp;
  wxTextCtrl* t_e_exp;

  wxTextCtrl* t_f_preexp;
  wxTextCtrl* t_f_exp;
  wxTextCtrl* t_a_preexp;
  wxTextCtrl* t_a_exp;

  wxTextCtrl* t_i_preexp;
  wxTextCtrl* t_i_exp;

  wxTextCtrl* t_l_numCells;
  wxTextCtrl* t_l_heatcap;
  wxTextCtrl* t_l_density;
  wxTextCtrl* t_l_length;
  wxTextCtrl* t_l_width;

  wxTextCtrl* t_stop_time;
  wxTextCtrl* t_ax_nodes;
  wxTextCtrl* t_loadres;

  wxBitmapButton* bb_browse;
  wxBitmap* openfileBitmap;

 public:
  double* p_a_height;
  double* p_a_width;
  double* p_a_space;
  double* p_a_ecd;
  double* p_a_tcoeff;
  double* p_a_thick;
  double* p_a_presdrop;
  double* p_c_height;
  double* p_c_width;
  double* p_c_space;
  double* p_c_ecdb;
  double* p_c_ecdm;
  double* p_c_tcoeffb;
  double* p_c_tcoeffm;
  double* p_c_thick;
  double* p_c_presdrop;
  double* p_s_thick;
  double* p_s_heatcap;
  double* p_s_density;
  double* p_e_thick;
  double* p_e_preexp;
  double* p_e_exp;
  double* p_f_preexp;
  double* p_f_exp;
  double* p_a_preexp;
  double* p_a_exp;
  double* p_i_preexp;
  double* p_i_exp;
  double* p_l_heatcap;
  double* p_l_density;
  double* p_l_length;
  double* p_l_width;
  double* p_stop_time;
  double* p_loadres;
  string* p_work_dir;
  long* p_l_numCells;
  long* p_ax_nodes;
  //GUI Variables

  void OnBrowse(wxCommandEvent &event);

  DECLARE_EVENT_TABLE()
};

#endif

