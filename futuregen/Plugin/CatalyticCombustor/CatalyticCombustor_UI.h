#ifndef CatalyticCombustor_UI_H
#define CatalyticCombustor_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

enum {
  R_CASETYPE_EVA,
  R_CASETYPE_DES
};

class CatalyticCombustor_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(CatalyticCombustor_UI_Dialog);
 public:
  CatalyticCombustor_UI_Dialog(wxWindow* parent, int id,
          double* effect,
          double* site_den,
          double* hydDiameter,
          double* length,
          double* conversion,
          double* velocity,
          long* case_type);
  CatalyticCombustor_UI_Dialog() {};
  
  virtual ~CatalyticCombustor_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables

  wxTextCtrl* t_effect;
  wxTextCtrl* t_site_den;
  wxTextCtrl* t_hydDiameter;
  wxTextCtrl* t_length;
  wxTextCtrl* t_conversion;
  wxTextCtrl* t_velocity;

  wxRadioButton* r_case_type_eva;
  wxRadioButton* r_case_type_des;

 public:
  double* p_effect;
  double* p_site_den;
  double* p_hydDiameter;
  double* p_length;
  double* p_conversion;
  double* p_velocity;
  long* p_case_type;
  //GUI Variables
  void OnCaseTypeChange(wxCommandEvent &event);

  DECLARE_EVENT_TABLE()
};

#endif

