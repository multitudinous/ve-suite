#ifndef _VE_UI_TRANSIENT_TAB_H_
#define _VE_UI_TRANSIENT_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif
#include "wx/wx.h"
#include "wx/notebook.h"
#include <iostream>
using namespace std;

//#include "controlIds.h"
//Transient tab control ids
enum TRANS_TAB_IDS
{
   TRANS_CATEGORY_RAD_BOX,
   TRANS_DIRECTION_RAD_BOX,
//include Time progress bar!!!!!!!!!!!!
   TIMEPROGRESS_GAUGE,  //check!!!!!!!!!!
   RESET_BUTTON,
   BACKWARD_BUTTON,
   START_BUTTON,
   FORWARD_BUTTON,
   PAUSE_BUTTON
};


class UI_TransTab : public wxPanel {
public:

   UI_TransTab(wxNotebook* tControl);
   ~UI_TransTab(){};

protected:
   wxNotebook* _parent;
   //the controls
   wxRadioBox* _categoryRBox;
   wxRadioBox* _directionRBox;
   //include time progress (wxStatusBar or wxGauge)
   wxGauge* _timeProgressGauge;  
   wxButton* _resetButton;
   wxButton* _backwardButton;
   wxButton* _startButton;
   wxButton* _forwardButton;
   wxButton* _pauseButton;


   //create this page
   void _buildPage();
   void createCommandId();
   //vispage control event callbacks
   void _onCategory(wxCommandEvent& event);
   void _onDirection(wxCommandEvent& event);   
   void _onTimeProgress(wxCommandEvent& event);//include time progress bar
   void _onReset(wxCommandEvent& event); 
   void _onBackward(wxCommandEvent& event);
   void _onStart(wxCommandEvent& event);
   void _onForward(wxCommandEvent& event);
   void _onPause(wxCommandEvent& event);
  DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_TRANSIENT_TAB_H_
