#ifndef COAL_H
#define COAL_H

#include "Plugin_base.h"
#include "wx/image.h"

#ifdef WIN32
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#endif

class Coal : public REI_Plugin
{
  DECLARE_DYNAMIC_CLASS(Coal);
 public:
  Coal();
  string coal_name_s;
  double ulti_c_d;
  double ulti_h_d;
  double ulti_o_d;
  double ulti_n_d;
  double ulti_s_d;
  double ulti_cl_d;
  double ulti_ash_d;
  double prox_h2o_d;
  double prox_vm_d;
  double prox_ash_d;
  double prox_fc_d;
  double ashc_sio2_d;
  double ashc_al2o3_d;
  double ashc_tio2_d;
  double ashc_fe2o3_d;
  double ashc_cao_d;
  double ashc_mgo_d;
  double ashc_na2o_d;
  double ashc_k2o_d;
  double ashc_so3_d;
  double ashc_p2o5_d;
  double ashc_bao_d;
  double ashc_sro_d;
  double hhv_d;
  double coal_cost_d;
  
  wxBitmap *my_icon;
  int icon_w, icon_h;
  
  virtual wxString GetName();
  virtual wxString GetDesc();
  virtual UIDialog* UI(wxWindow* parent);
  virtual void DrawIcon(wxDC *dc);
  virtual int GetNumIports() { return 0; };
  virtual void GetIPorts(POLY &iports){ return; };
  virtual void GetOPorts(POLY& ports); 
};

#endif
