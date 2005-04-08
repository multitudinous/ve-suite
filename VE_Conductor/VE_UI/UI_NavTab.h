#ifndef _VE_UI_NAV_H_
#define _VE_UI_NAV_H_

#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/wx.h>
#include <wx/image.h>

enum NAV_TAB_IDS 
{
   LEFT_B,
   RIGHT_B,
   UP_B,
   DOWN_B,
   FORWARD_B,
   BACKWARD_B,
   CCW_B,
   CW_B,
   TRANS_STEP_SLIDER,
   ROT_STEP_SLIDER,
   HEAD_ROTATE_CHK,
   RESET_NAV_POSITION
   //NONE= -1000
};

//override the buttons
class UI_NavButton: public wxBitmapButton{
public:
   UI_NavButton(wxWindow* parent, wxWindowID id, const wxBitmap& bitmap);
   virtual ~UI_NavButton(){};

   //need to override this function
   void onMouse(wxMouseEvent& event);
   void onMouseUp(wxMouseEvent& event);

protected:
   int _buttonPushed;
   DECLARE_EVENT_TABLE()
};

class UI_NavigateScroll: public wxScrolledWindow{
public:
   UI_NavigateScroll(wxWindow* parent);
   ~UI_NavigateScroll();

   UI_NavButton* _leftButton;
   UI_NavButton* _rightButton;
   UI_NavButton* _upButton;
   UI_NavButton* _downButton;
   UI_NavButton* _forwardButton;
   UI_NavButton* _backButton;
   UI_NavButton* _pitchupButton;
   UI_NavButton* _pitchdownButton;
   UI_NavButton* _rollccwButton;
   UI_NavButton* _rollcwButton;
   UI_NavButton* _yawccwButton;
   UI_NavButton* _yawcwButton;

   wxSlider*   translationStepSize;
   wxSlider*   rotationStepSize;
   wxCheckBox* headRotationChk;
   wxButton*   resetNavPosition;
protected:
   wxBitmap* _createButtonImage(char* directory,
                    char* fileName,
                    wxBitmapType type = wxBITMAP_TYPE_BMP);
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
   
   void OnTransStepSlider( wxScrollEvent& event);
   void OnRotStepSlider( wxScrollEvent& event);
   void OnResetNavPosition( wxCommandEvent& event );

   UI_NavigateScroll* navScroll;

 protected:
   int _activeButton;
   wxNotebook* _parent;

   DECLARE_EVENT_TABLE()
};
#endif  //_VE_UI_NAV_H_

