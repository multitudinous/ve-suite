#ifndef Exterior_UI_H
#define Exterior_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

enum EXTERIOR_TAB_IDS{
   DIRECTION_WIND_RBOX,
   SCREEN_SIZE_SLIDER,
   EXT_UPDATE_BUTTON,
   EXT_CLEAR_BUTTON,
   EXT_EXIT_BUTTON
};

class Exterior_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(Exterior_UI_Dialog);
 public:
  Exterior_UI_Dialog(wxWindow* parent, int id,
          double* screenDiameter,
          long* directionWind);
  Exterior_UI_Dialog() {};
  
  virtual ~Exterior_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
   wxRadioBox* _directionWindRBox;
   wxSlider* _screenSizeSlider; 
   wxButton* _Update;
   wxButton* _clearButton;
   wxButton* _exitButton;
  
 public:
  double* p_screenDiameter;
  long* p_directionWind;

  //GUI Variables
  void _buildPage();
  void _onDirection(wxCommandEvent& event);
  void _onUpdate(wxCommandEvent& event);   
  void _onClear(wxCommandEvent& event);
  void _onExit(wxCommandEvent& event);
  DECLARE_EVENT_TABLE() 

};

#endif

