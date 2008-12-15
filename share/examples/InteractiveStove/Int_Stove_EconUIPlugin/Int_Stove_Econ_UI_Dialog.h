#ifndef Int_Stove_Econ_UI_DIALOG_H
#define Int_Stove_Econ_UI_DIALOG_H
#include <ves/conductor/UIDialog.h>
#include <vector>

class wxButton;
class wxTextCtrl;

using namespace std;

enum {
	CLOSE_EXCEL
};

class Int_Stove_Econ_UI_Dialog : public ves::conductor::UIDialog
{
	DECLARE_DYNAMIC_CLASS(Int_Stove_Econ_UI_Dialog);
   public:
  Int_Stove_Econ_UI_Dialog(wxWindow* parent, int id,
          std::vector< double >* cost_array,
		  long* closesheets);
      Int_Stove_Econ_UI_Dialog() {;}
  
      virtual ~Int_Stove_Econ_UI_Dialog();
  
      virtual bool TransferDataFromWindow();
      virtual bool TransferDataToWindow();
      virtual void Lock(bool l);

	  void _onCloseExcel(wxCommandEvent& event);
   protected:
      //UI widgets variables
  
   public:
  std::vector< double >* p_cost_array;
  long* p_closesheets;
      //GUI Variables

  wxTextCtrl* _base_mat_cost_entry;
  wxTextCtrl* _base_const_cost_entry;
  wxTextCtrl* _baffle_mat_cost_entry;
  wxButton*   _closeExcelButton;
  wxButton*   _updateButton;

  int closeSheets;
  
  DECLARE_EVENT_TABLE();
};

#endif

