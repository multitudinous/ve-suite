#ifndef Cooling_UI_H
#define Cooling_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

enum COOLING_TAB_IDS{
   COOLANT_RBOX,
   HEIGHT_RAD_SLIDER,
   WIDTH_RAD_SLIDER,
   FAN_PITCH_SLIDER,
   UPDATE_COOL_BUTTON,
   CLEAR_COOL_BUTTON,
   EXIT_COOL_BUTTON
};

class Cooling_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(Cooling_UI_Dialog);
 public:
  Cooling_UI_Dialog(wxWindow* parent, int id,
          long* coolantType,
          long* heightRad,
          long* widthRad,
          long* fanPitch);
  Cooling_UI_Dialog() {};
  
  virtual ~Cooling_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
   wxRadioBox* _coolantRBox;
   wxSlider* _heightRadSlider;
   wxSlider* _widthRadSlider;
   wxSlider* _fanPitchSlider;
   wxButton* _Update;
   wxButton* _clearButton;
   wxButton* _exitButton;

   
  
 public:
  long* p_coolantType;
  long* p_heightRad;
  long* p_widthRad;
  long* p_fanPitch;
  //GUI Variables
  void _buildPage();
  void _onCoolant(wxCommandEvent& event);
  void _onUpdate(wxCommandEvent& event);   
  void _onClear(wxCommandEvent& event);
  void _onExit(wxCommandEvent& event);

  DECLARE_EVENT_TABLE() 
};

#endif

