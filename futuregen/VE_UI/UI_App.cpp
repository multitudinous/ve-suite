#include "UI_App.h"

IMPLEMENT_APP(UI_App)

//////////////////////////////
//Initialize the application//
//////////////////////////////
bool UI_App::OnInit()
{
   //Create the main window
    _uiFrame = new UI_Frame(wxT("WX Client for VE Suite"));
  
   //What does this do?
   InitObserver();

   //biv--This was from original implementation
   //biv--Not sure if it is true or not-- check logic later

   // Problem with generic wxNotebook implementation whereby it doesn't size
   // properly unless you set the size again
#if defined(__WIN16__) || defined(__WXMOTIF__)
    int width, height;
    _uiFrame->GetSize(& width, & height);
    _uiFrame->SetSize(-1, -1, width, height);
#endif
   
   //display the UI
   _uiFrame->Show();
   return TRUE;

}
