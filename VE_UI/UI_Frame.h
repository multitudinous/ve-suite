#ifndef _VE_UI_FRAME_H_
#define _VE_UI_FRAME_H_

#include "wx/wx.h"
#include "controlIds.h"
#include "UI_Tabs.h"

////////////////////////////////////////////////////
//This is the class that is the frame.            //
//All instances of new controls should be         //
//added appropriately as members of this class.   //
////////////////////////////////////////////////////


class UI_Frame: public wxFrame{
public:
   UI_Frame (const wxString& title,
             const wxPoint& pos = wxDefaultPosition,
             const wxSize& size = wxDefaultSize,
             long style = wxDEFAULT_FRAME_STYLE);
   virtual ~UI_Frame();

   //the events to handle
   void OnTabsEvent(wxNotebookEvent& event);
   void OnIdleEvent(wxIdleEvent& event);

protected:

   //the notebook control that has our tabs
   UI_Tabs* _tabs;
};
#endif //_VE_UI_FRAME_H_
