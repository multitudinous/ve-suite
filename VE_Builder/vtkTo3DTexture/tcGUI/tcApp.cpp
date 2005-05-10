#include "tcApp.h"

IMPLEMENT_APP(TCApp)

//////////////////////////////
//Initialize the application//
//////////////////////////////
bool TCApp::OnInit()
{
   //Create the main window
    _frame = new TCFrame(0,-1,wxT("Texture Creator"));
   
   // Problem with generic wxNotebook implementation whereby it doesn't size
   // properly unless you set the size again
#if defined(__WIN16__) || defined(__WXMOTIF__)
    int width, height;
    _frame->GetSize(& width, & height);
    _frame->SetSize(-1, -1, width, height);
#endif
   //display the UI
   _frame->Show();
   return TRUE;

}
