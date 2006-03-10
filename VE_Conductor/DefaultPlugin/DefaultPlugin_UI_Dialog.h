#ifndef Hummer_UI_H
#define Hummer_UI_H
#include "VE_Conductor/Framework/UIDialog.h"
#include <vector>
#include <string>

enum Hummer_TAB_IDS
{
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
class wxRadioBox;
class wxSlider;
class wxTextCtrl;
class wxButton;

class DefaultPlugin_UI_Dialog : public UIDialog
{
public:
  DefaultPlugin_UI_Dialog(wxWindow* parent, int id );
  DefaultPlugin_UI_Dialog(){;}
  
  virtual ~DefaultPlugin_UI_Dialog();
  
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
  //GUI Variables
  void _buildPage();
  void _onDirection(wxCommandEvent& event);
  void _onError(wxCommandEvent& event);
  void _onSliderUpdate(wxCommandEvent& event);   
  void _onClear(wxCommandEvent& event);
  void _onExit(wxCommandEvent& event);

  DECLARE_EVENT_TABLE()
};

#endif

