#include "UIDialog.h"

IMPLEMENT_DYNAMIC_CLASS(UIDialog, wxDialog)

UIDialog::UIDialog(wxWindow* parent, int id, wxString title)
  :wxDialog(parent, id, title)
{

}

void UIDialog::Lock(bool l)
{
  lock = l;
}
