#ifndef TEXTRESULTDIALOG_H
#define TEXTRESULTDIALOG_H

#include "UIDialog.h"
#include <vector>
#include "TexTable.h"

class WXPLUGIN_DECLSPEC TextResultDialog : public UIDialog
{
 public:
  TextResultDialog(wxWindow*parent, const wxString& title=wxT("Result Dialog"), wxSize tabsize= wxSize(477, 300));
  ~TextResultDialog();

  void Set2Cols(const std::vector<wxString>& col1, const std::vector<wxString>& col2);
  TexTable *syngas;
  wxButton *ok;

  DECLARE_EVENT_TABLE()
};

#endif


