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
};

class Gasi2Tabs : public wxNotebook
{
 public:
  Gasi2Tabs(wxWindow *parent, wxWindowID id = -1,
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
  wxTextCtrl* t_char_percent1;
  wxTextCtrl* t_steam_temp2;
  wxTextCtrl* t_steam_flrt2;
  wxTextCtrl* t_slurry_temp2;
  wxTextCtrl* t_slurry_flrt2;
  wxTextCtrl* t_coal_percent2;
  wxTextCtrl* t_char_percent2;
  wxTextCtrl* t_steam_temp3;
  wxTextCtrl* t_steam_flrt3;
  wxTextCtrl* t_slurry_temp3;
  wxTextCtrl* t_slurry_flrt3;
  wxTextCtrl* t_coal_percent3;
  wxTextCtrl* t_char_percent3;
  wxTextCtrl* t_pres_drop;

  wxComboBox* c_coal_type;
  wxTextCtrl* t_size_50;
  wxTextCtrl* t_size_200;
  wxRadioButton* r_stage1;
  wxRadioButton* r_stage2;

  void OnChangeStage(wxCommandEvent &event);

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
          double* char_percent1,
          double* steam_temp2,
          double* steam_flrt2,
          double* slurry_temp2,
          double* slurry_flrt2,
          double* coal_percent2,
          double* char_percent2,
          double* steam_temp3,
          double* steam_flrt3,
          double* slurry_temp3,
          double* slurry_flrt3,
          double* coal_percent3,
          double* char_percent3,
          double* size_50,
          double* size_200,
          double* pres_drop,
          string* coal_type,
          long* stage);
  Gasifier_UI_Dialog() {};
  
  virtual ~Gasifier_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  Gasi2Tabs *m_tabs;
  wxNotebookSizer *m_sizerNotebook;

 public:
  double* p_steam_temp1;
  double* p_steam_flrt1;
  double* p_slurry_temp1;
  double* p_slurry_flrt1;
  double* p_coal_percent1;
  double* p_char_percent1;
  double* p_steam_temp2;
  double* p_steam_flrt2;
  double* p_slurry_temp2;
  double* p_slurry_flrt2;
  double* p_coal_percent2;
  double* p_char_percent2;
  double* p_steam_temp3;
  double* p_steam_flrt3;
  double* p_slurry_temp3;
  double* p_slurry_flrt3;
  double* p_coal_percent3;
  double* p_char_percent3;
  double* p_size_50;
  double* p_size_200;
  double* p_pres_drop;
  string* p_coal_type;
  long* p_stage;
  //GUI Variables
};

#endif

