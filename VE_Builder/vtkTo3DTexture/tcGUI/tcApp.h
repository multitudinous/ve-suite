#ifndef _TC_APP_H_
#define _TC_APP_H_
#include <wx/wx.h>
#include "tcFrame.h"

//The main application  
class TCApp: public wxApp{
public: 
   bool OnInit();
   void OnInitCmdLine(wxCmdLineParser& parser);
   bool OnCmdLineParsed(wxCmdLineParser& parser);
protected:
   TCFrame* _frame;
   bool _isBatch;
   bool _translateFromCmdLine();
private:
   int p;
   int rank;
};
DECLARE_APP(TCApp)
 
#endif //_TC_APP_H_
