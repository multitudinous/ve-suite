#ifndef Baffle_UI_H
#define Baffle_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

enum BAFFLE_TAB_IDS{
   DIRECTION_RBOX,
   ERROR_RBOX,
   HEIGHT_SLIDER,
   WIDTH_SLIDER,
   X_LOCATION_SLIDER,
   Y_LOCATION_SLIDER,
   Z_LOCATION_SLIDER,
   UPDATE_BUTTON,
   CLEAR_BUTTON,
   EXIT_BUTTON
};

class Baffle_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(Baffle_UI_Dialog);
 public:
  Baffle_UI_Dialog(wxWindow* parent, int id,
          double* xLocation,
          double* yLocation,
          double* zLocation,
          double* height,
          double* width);
  Baffle_UI_Dialog() {};
  
  virtual ~Baffle_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:

  //UI widgets variables

   wxRadioBox* _directionRBox;
   wxRadioBox* _errorRBox;
   //the controls
   wxSlider* _heightSlider;
   wxSlider* _widthSlider;
   wxSlider* _xLocSlider;
   wxSlider* _yLocSlider;
   wxSlider* _zLocSlider; 

   wxTextCtrl* _xLoc;
   wxTextCtrl* _yLoc;
   wxTextCtrl* _zLoc;

   wxButton* _sliderUpdate;
   wxButton* _clearButton;
   wxButton* _exitButton;

 
 public:
  double* p_xLocation;
  double* p_yLocation;
  double* p_zLocation;
  double* p_height;
  double* p_width;

  //GUI Variables
  void _buildPage();
  void _onDirection(wxCommandEvent& event);
  void _onError(wxCommandEvent& event);
  //vispage control event callbacks
  //void _onHeightSlider(wxCommandEvent& event);
  //void _onWidthSlider(wxCommandEvent& event);
  //void _onXLocSlider(wxCommandEvent& event);
  //void _onYLocSlider(wxCommandEvent& event); 
  //void _onZLocSlider(wxCommandEvent& event); 
  void _onSliderUpdate(wxCommandEvent& event);   
  void _onClear(wxCommandEvent& event);
  void _onExit(wxCommandEvent& event);

  DECLARE_EVENT_TABLE() 

};

#endif

