#ifndef GasSource_UI_H
#define GasSource_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>
#include <wx/notebook.h>
#include <map>

using namespace std;

enum {
  ADD,
  DEL,
  SPEC_LIST,
  P_ADD,
  P_DEL,
  PART_LIST
};

class EditDialog : public wxDialog
{
 public:
  EditDialog(wxWindow *parent, wxWindowID id = -1);
  wxStaticText* m_var;
  wxTextCtrl* m_val;
};

class GasSourceTabs : public wxNotebook
{
 public:
  GasSourceTabs(wxWindow *parent, wxWindowID id = -1,
       const wxPoint& pos = wxDefaultPosition,
       const wxSize& size = wxDefaultSize, long style = 0);
 
  void  CreateInitialPages();
  wxString ToValue(const wxString inp, wxString &outp);
  wxString ToString(const wxString inp, const wxString val);

 protected:
  wxPanel* CreateFirstPage();
  wxPanel* CreateSecondPage();
 
 public:
  
  wxTextCtrl* t_temp;
  wxTextCtrl* t_pres;
  wxTextCtrl* t_flow;
  wxTextCtrl* t_p_temp;
  wxTextCtrl* t_p_m;
  wxTextCtrl* t_mps;
  wxTextCtrl* t_sv;
  wxTextCtrl* t_coalcal;
  wxTextCtrl* t_ashcal;
  wxTextCtrl* t_ashph;

  wxListBox* lb_species;
  wxListBox* lb_comp_frac;
  wxListBox* lb_particles;
  wxListBox* lb_p_comp_frac;

  wxButton* b_add;
  wxButton* b_del;
  wxButton* b_p_add;
  wxButton* b_p_del;

  void OnAdd(wxCommandEvent& event);
  void OnDel(wxCommandEvent& event);
  void OnPAdd(wxCommandEvent& event);
  void OnPDel(wxCommandEvent& event);
  void OnEditSpec(wxCommandEvent& event);
  void OnEditPart(wxCommandEvent& event);

  DECLARE_EVENT_TABLE()
};

class GasSource_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(GasSource_UI_Dialog);
 public:
  GasSource_UI_Dialog(wxWindow* parent, int id,
          double* temp,
          double* pres,
          double* flow,
          double* p_temp,
          double* p_m,
          double* mps,
          double* sv,
          double* coalcal,
          double* ashcal,
          double* ashph,
          vector<string>* species,
          vector<string>* comp,
          vector<string>* spec_frac,
          vector<string>* particles,
          vector<string>* p_comp,
          vector<string>* p_frac);
  GasSource_UI_Dialog() {};
  
  virtual ~GasSource_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  GasSourceTabs* m_tabs;
  wxNotebookSizer *m_sizerNotebook;
 public:
  double* p_temp;
  double* p_pres;
  double* p_flow;
  double* p_p_temp;
  double* p_p_m;
  double* p_mps;
  double* p_sv;
  double* p_coalcal;
  double* p_ashcal;
  double* p_ashph;
  vector<string>* p_species;
  vector<string>* p_comp;
  vector<string>* p_spec_frac;
  vector<string>* p_particles;
  vector<string>* p_p_comp;
  vector<string>* p_p_frac;
  //GUI Variables
};

#endif

