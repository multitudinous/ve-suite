#ifndef GasFeedback_UI_H
#define GasFeedback_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>
#include <map>
using namespace std;

enum {
  ADD,
  DEL,
  SPEC_LIST,
};

class EditDialog2 : public wxDialog
{
 public:
  EditDialog2(wxWindow *parent, wxWindowID id = -1);
  wxStaticText* m_var;
  wxTextCtrl* m_val;
};

class GasFeedback_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(GasFeedback_UI_Dialog);
 public:
  GasFeedback_UI_Dialog(wxWindow* parent, int id,
          long* iterations,
          vector<string>* species,
          vector<string>* sel_species,
          vector<string>* max_error);
  GasFeedback_UI_Dialog() {};
  
  virtual ~GasFeedback_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  
  wxTextCtrl* t_iterations;
  wxListBox* lb_species;
  wxListBox* lb_selspec_thresh;
  
  wxButton* b_add;
  wxButton* b_del;
  wxString ToValue(const wxString inp, wxString &outp);
  wxString ToString(const wxString inp, const wxString val);
 public:
  long* p_iterations;
  vector<string>* p_species;
  vector<string>* p_sel_species;
  vector<string>* p_max_error;
  //GUI Variables

  void OnAdd(wxCommandEvent& event);
  void OnDel(wxCommandEvent& event);
  void OnEditSpec(wxCommandEvent& event);

  DECLARE_EVENT_TABLE()
};

#endif

