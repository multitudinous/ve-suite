#include "AdiabaticFlameTemp_UI_Dialog.h"

//Here is the constructor with passed in pointers
AdiabaticFlameTemp_UI_Dialog
::AdiabaticFlameTemp_UI_Dialog
(wxWindow* parent, int id,
  double* perc_theor_error)
: UIDialog((wxWindow *) parent, id, "AdiabaticFlameTemp"),
  p_perc_theor_error(perc_theor_error)
{
}

/////////////////////////////////////////////////////
AdiabaticFlameTemp_UI_Dialog
::~AdiabaticFlameTemp_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool AdiabaticFlameTemp_UI_Dialog::TransferDataFromWindow()
{
  return true;
}

////////////////////////////////////////////////////
bool AdiabaticFlameTemp_UI_Dialog::TransferDataToWindow()
{
    return true;
}

void AdiabaticFlameTemp_UI_Dialog::Lock(bool l)
{
}

