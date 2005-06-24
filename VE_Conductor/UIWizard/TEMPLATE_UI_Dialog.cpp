#include "TEMPLATE_UI_Dialog.h"

//Here is the constructor with passed in pointers
TEMPLATE_UI_Dialog
::TEMPLATE_UI_Dialog

/////////////////////////////////////////////////////
TEMPLATE_UI_Dialog
::~TEMPLATE_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool TEMPLATE_UI_Dialog::TransferDataFromWindow()
{
  return true;
}

////////////////////////////////////////////////////
bool TEMPLATE_UI_Dialog::TransferDataToWindow()
{
    return true;
}

void TEMPLATE_UI_Dialog::Lock(bool l)
{
}
