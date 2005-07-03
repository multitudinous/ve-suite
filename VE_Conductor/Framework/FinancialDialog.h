#ifndef FINANCIALDIALOG_H
#define FINANCIALDIALOG_H

#include <wx/wx.h>
#include "VE_Conductor/Framework/interface.h"

enum 
{
  RADIO_FDA,
  RADIO_FDB,
  CC00,
  CC01,
  CC02,
  CC03,
  CC04,
  CC05,
  CC06,
  CC07,
  CC08,
  OM00,
  OM01,
  OM02,
  OM03
};

class FinancialDialog : public wxDialog
{
 public:
  FinancialDialog(wxWindow *parent, wxWindowID id );
  ~FinancialDialog();

 protected:
  
  //wxComboBox* _technology;

  wxRadioButton* specify_a;
  wxRadioButton* specify_b;

  wxTextCtrl* _cc00;
  wxTextCtrl* _cc01;
  wxTextCtrl* _cc02;
  wxTextCtrl* _cc03;
  wxTextCtrl* _cc04;
  wxTextCtrl* _cc05;
  wxTextCtrl* _cc06;
  wxTextCtrl* _cc07;
  wxTextCtrl* _cc08;

  wxTextCtrl* _om00;
  wxTextCtrl* _om01;
  wxTextCtrl* _om02;
  wxTextCtrl* _om03;

  wxButton * ok_b;
  
  void double2entry(wxTextCtrl* entry, double * value);
  void entry2double(wxTextCtrl* entry, double * value);
  virtual bool TransferDataToWindow();
  virtual bool TransferDataFromWindow();
  
 public:
    
  void OnChange(wxCommandEvent &event);
  
  wxString _technology_s;
  
  int _use_data;

  double _cc00_d;
  double _cc01_d;
  double _cc02_d;
  double _cc03_d;
  double _cc04_d;
  double _cc05_d;
  double _cc06_d;
  double _cc07_d;
  double _cc08_d;

  double _om00_d;
  double _om01_d;
  double _om02_d;
  double _om03_d;

  DECLARE_EVENT_TABLE()
  
};

#endif
