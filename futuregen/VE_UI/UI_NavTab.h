#ifndef _VE_UI_NAV_H_
#define _VE_UI_NAV_H_

#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/wx.h>

enum NAV_TAB_IDS {
  LEFT_B,
  RIGHT_B,
  UP_B,
  DOWN_B,
  FORWARD_B,
  BACKWARD_B,
  CCW_B,
  CW_B,
  //NONE= -1000
};
//override the buttons
class UI_NavButton: public wxButton{
public:
   UI_NavButton(wxWindow* parent, wxWindowID id, const wxString& label);
   virtual ~UI_NavButton(){};

   //need to override this function
   void onMouse(wxMouseEvent& event);
   void onMouseUp(wxMouseEvent& event);

protected:
   int _buttonPushed;
   DECLARE_EVENT_TABLE()
};

//the main navigation tab class
class UI_NavigationTab : public wxPanel {
 public:
   UI_NavigationTab(wxNotebook* tControl);
   virtual ~UI_NavigationTab(){};

   //turn off the navigation flag
   void onLeftMouseUp(wxMouseEvent& event);
   void onLeftDown(wxMouseEvent& event);

   //mouse callback
   void onMouse(wxMouseEvent& mouse);

   //update the tab that we're moving/stopping
   void updateParent(int push, int id);
  
   //add a button to the managed list
   void setActiveButton(int id){_activeButton = id;}
   int getActiveButton(){return _activeButton;}
   
 protected:
   int _activeButton;

   UI_NavButton* _leftButton;
   UI_NavButton* _rightButton;
   UI_NavButton* _upButton;
   UI_NavButton* _downButton;
   UI_NavButton* _forwardButton;
   UI_NavButton* _backButton;
   UI_NavButton* _ccwButton;
   UI_NavButton* _cwButton;
   DECLARE_EVENT_TABLE()
};
#endif  //_VE_UI_NAV_H_

