#ifndef ASU_UI_H
#define ASU_UI_H
#include "UIDialog.h"

enum {
  NUM_SPARE_TRAINS,
   OXYGEN_PURITY
};

class ASU_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(ASU_UI_Dialog);
 public:
  ASU_UI_Dialog() {};
  ASU_UI_Dialog(wxWindow* parent, int id, long *num_idx_spare_trains, double* o2_purity_);
  virtual ~ASU_UI_Dialog();

  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();

 protected:
  wxTextCtrl* o2_purity;
  wxComboBox* num_spare_trains;

 public:
  long* num_spare_trains_;
  double* o2_purity_d_;
};

#endif
