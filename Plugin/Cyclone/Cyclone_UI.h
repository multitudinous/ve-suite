#ifndef Cyclone_UI_H
#define Cyclone_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

class Cyclone_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(Cyclone_UI_Dialog);
 public:
  Cyclone_UI_Dialog(wxWindow* parent, int id,
          double* diameter,
          double* particle_turn_count,
          double* velocity_heads);
  Cyclone_UI_Dialog() {};
  
  virtual ~Cyclone_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  wxTextCtrl* t_diameter;
  wxTextCtrl* t_particle_turn_count;
  wxTextCtrl* t_velocity_heads;

 public:
  double* p_diameter;
  double* p_particle_turn_count;
  double* p_velocity_heads;
  //GUI Variables
};

#endif

