#ifndef GLCanvasSampleApp_UI_DIALOG_H
#define GLCanvasSampleApp_UI_DIALOG_H
#include "VE_Conductor/Framework/UIDialog.h"
#include <vector>
#include <string>

class GLCanvasSampleApp_UI_Dialog : public UIDialog
{
   public:
  GLCanvasSampleApp_UI_Dialog(wxWindow* parent, int id,
          double* radius,
          double* length,
          double* width,
          double* xcoord,
          double* ycoord,
          long* type);
      GLCanvasSampleApp_UI_Dialog() {;}
  
      virtual ~GLCanvasSampleApp_UI_Dialog();
  
      virtual bool TransferDataFromWindow();
      virtual bool TransferDataToWindow();
      virtual void Lock(bool l); 
   protected:
      //UI widgets variables
  
   public:
  double* p_radius;
  double* p_length;
  double* p_width;
  double* p_xcoord;
  double* p_ycoord;
  long* p_type;
      //GUI Variables
};

#endif

