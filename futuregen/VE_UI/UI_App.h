#ifndef _VE_UI_APP_H_
#define _VE_UI_APP_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include "wx/wx.h"
#include "UI_Frame.h"

//The main application  
class UI_App: public wxApp{
public: 
   bool OnInit();
   void InitObserver(){};
protected:
   UI_Frame* _uiFrame;
};
DECLARE_APP(UI_App)
 
#endif //_VE_UI_APP_H_
