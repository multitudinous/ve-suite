#include "UI_App.h"
#include "UI_TopFrame.h"
IMPLEMENT_APP(UI_App)

//////////////////////////////
//Initialize the application//
//////////////////////////////
bool UI_App::OnInit()
{
   //Create the main window
    _uiTopFrame = new UI_TopFrame(wxT("VE-Conductor"));
   //What does this do?
   InitObserver();
   //biv--This was from original implementation
   //biv--Not sure if it is true or not-- check logic later

   // Problem with generic wxNotebook implementation whereby it doesn't size
   // properly unless you set the size again
#if defined(__WIN16__) || defined(__WXMOTIF__)
    int width, height;
    _uiTopFrame->GetSize(& width, & height);
    _uiTopFrame->SetSize(-1, -1, width, height);
#endif
   //display the UI
   _uiTopFrame->Show();
   return TRUE;

}
