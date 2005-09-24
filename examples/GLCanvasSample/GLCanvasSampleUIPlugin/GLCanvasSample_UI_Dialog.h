#ifndef GLCanvasSample_UI_DIALOG_H
#define GLCanvasSample_UI_DIALOG_H
#include "VE_Conductor/Framework/UIDialog.h"
#include <vector>
#include <string>

using namespace std;

class GLCanvasSample_UI_Dialog : public UIDialog
{
   public:
  GLCanvasSample_UI_Dialog(wxWindow* parent, int id,
          double* radius,
          double* length,
          double* width,
          long* type);
      GLCanvasSample_UI_Dialog() {;}
  
      virtual ~GLCanvasSample_UI_Dialog();
  
      virtual bool TransferDataFromWindow();
      virtual bool TransferDataToWindow();
      virtual void Lock(bool l); 
   protected:
      //UI widgets variables
  
   public:
  double* p_radius;
  double* p_length;
  double* p_width;
  long* p_type;
      //GUI Variables
};

#endif

