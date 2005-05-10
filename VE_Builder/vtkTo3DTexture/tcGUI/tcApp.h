#ifndef _TC_APP_H_
#define _TC_APP_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include "wx/wx.h"
#include "tcFrame.h"

//The main application  
class TCApp: public wxApp{
public: 
   bool OnInit();
protected:
   TCFrame* _frame;
};
DECLARE_APP(TCApp)
 
#endif //_TC_APP_H_
