#ifndef PORTDIALOG_H
#define PORTDIALOG_H

#include <wx/wx.h>
#include <vector>
#include "VE_Conductor/Framework/ListTable.h"

class PortDialog : public wxDialog
{
 public:
  PortDialog(const wxString& title);
  ~PortDialog();

  void Set3Cols(const std::vector<wxString>& col1, const std::vector<wxString>& col2, const std::vector<wxString>& col3);
  ListTable *syngas;
  wxTextCtrl *temp;
  wxTextCtrl *pres;
  wxTextCtrl *flrt;
  wxButton *ok;
  void SetVal(const wxString &var, const wxString &val);
  DECLARE_EVENT_TABLE()
};

#endif


