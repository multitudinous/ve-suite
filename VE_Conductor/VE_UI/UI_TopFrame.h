#ifndef _VE_UI_TOP_FRAME_H_
#define _VE_UI_TOP_FRAME_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/wx.h>
class UI_Frame;

class UI_TopFrame: public wxFrame{
public:
   UI_TopFrame (const wxString& title,
             const wxPoint& pos = wxDefaultPosition,
             const wxSize& size = wxDefaultSize,
             long style = wxDEFAULT_FRAME_STYLE);

   virtual ~UI_TopFrame();

   UI_Frame* _uiFrame;

};
#endif
