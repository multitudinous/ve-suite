#include "VE_Conductor/Framework/UIDialog.h"

UIDialog::UIDialog(wxWindow* parent, int id, wxString title)
  :wxDialog(parent, id, title)
{

}

void UIDialog::Lock(bool l)
{
  lock = l;
}
