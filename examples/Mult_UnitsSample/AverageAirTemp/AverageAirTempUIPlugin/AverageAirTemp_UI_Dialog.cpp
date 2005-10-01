#include "AverageAirTemp_UI_Dialog.h"

//Here is the constructor with passed in pointers
AverageAirTemp_UI_Dialog
::AverageAirTemp_UI_Dialog
(wxWindow* parent, int id,
  double* intakediam,
  double* airvel,
  double* intaketemp,
  double* airinlettemp,
  double* intakelength)
: UIDialog((wxWindow *) parent, id, "AverageAirTemp"),
  p_intakediam(intakediam),
  p_airvel(airvel),
  p_intaketemp(intaketemp),
  p_airinlettemp(airinlettemp),
  p_intakelength(intakelength)
{
}

/////////////////////////////////////////////////////
AverageAirTemp_UI_Dialog
::~AverageAirTemp_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool AverageAirTemp_UI_Dialog::TransferDataFromWindow()
{
  return true;
}

////////////////////////////////////////////////////
bool AverageAirTemp_UI_Dialog::TransferDataToWindow()
{
    return true;
}

void AverageAirTemp_UI_Dialog::Lock(bool l)
{
}

