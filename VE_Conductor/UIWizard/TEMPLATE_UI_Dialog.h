#ifndef TEMPLATE_UI_DIALOG_H
#define TEMPLATE_UI_DIALOG_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

class TEMPLATE_UI_Dialog : public UIDialog
{
   public:
      TEMPLATE_UI_Dialog() {;}
  
      virtual ~TEMPLATE_UI_Dialog();
  
      virtual bool TransferDataFromWindow();
      virtual bool TransferDataToWindow();
      virtual void Lock(bool l); 
   protected:
      //UI widgets variables
  
   public:
      //GUI Variables
};

#endif
