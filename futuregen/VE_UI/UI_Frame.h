#ifndef _VE_UI_FRAME_H_
#define _VE_UI_FRAME_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include "wx/wx.h"
#include "controlIds.h"
#include "UI_Tabs.h"
//#include "UI_DataSetTab.h"
//#include "UI_ScalarTab.h"
#include "UI_DataSetPanel.h"

////////////////////////////////////////////////////
//This is the class that is the frame.            //
//All instances of new controls should be         //
//added appropriately as members of this class.   //
////////////////////////////////////////////////////


class UI_Frame: public wxPanel{
public:
   UI_Frame (wxWindow* parent, wxWindowID id,
             const wxPoint& pos = wxDefaultPosition,
             const wxSize& size = wxDefaultSize,
             long style = 0);

   UI_Frame(VjObs_ptr ref, wxWindow* parent, wxWindowID =-1,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize, 
            long style = wxMAXIMIZE_BOX|wxMINIMIZE_BOX);

   virtual ~UI_Frame();

   void buildCORBA();
   void buildFrame();

   //the events to handle
   void OnTabsEvent(wxNotebookEvent& event);
   void OnIdleEvent(wxIdleEvent& event);
   void changeActiveScalarOnDataset(const char* activeScalarName);

   //UI_DatasetTab* _datasetPage;
   //UI_ScalarTab* _scalartab;
   UI_DatasetPanel* _datasetPanel;
   //UI_DatasetScrollable* _datasetScrollable;
   VjObs::obj_p_var   datasetTypes;

   //the notebook control that has our tabs
   UI_Tabs* _tabs;

protected:

   VjObs_var vjobs;
};
#endif //_VE_UI_FRAME_H_
