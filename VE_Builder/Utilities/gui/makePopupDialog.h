#include "spinctld.h"
#ifndef _MAKEPOPUPDIALOG_H
#define _MAKEPOPUPDIALOG_H
class makePopupDialog: public wxDialog
{
   private:
         wxButton* okButton;
         wxButton* cancelButton;
         wxRadioBox* radioBox;
         wxStaticBox* stISOBox;         
         wxStaticBox* stTxtBox;
         wxSpinCtrlDbl* spinnerBoxIso;
         
         wxSpinCtrlDbl* spinnerBoxDecim;     
         bool selectedExtVal;
         enum{ RADIO_BOX, SLIDER_SELECTION, SLIDER_ISO };
         double rangeMin, rangeMax;
         
   protected:
         DECLARE_EVENT_TABLE()
         void onRadioBox( wxCommandEvent &event );
         
   public:
         makePopupDialog( wxWindow* frm, int w, int h, double rngMin, double rngMax, wxString st );
         ~makePopupDialog();
         bool getRadioVal();
         double getDecimationValu();
         double getIsoSurfValu();
};


makePopupDialog::makePopupDialog( wxWindow* frm, int w, int h, double rngMin, double rngMax, wxString str )
      :wxDialog( frm, -1, " Save "+str+" As ", 
      wxDefaultPosition, wxSize(w, h) , 
      wxDEFAULT_DIALOG_STYLE, "dlg box" )
{
   wxString list[] = { "Exterior value", "ISO value" }; 
   okButton = new wxButton( this, wxID_OK, wxString("Ok"), wxPoint(110,150),
			wxSize(50,27), wxBU_EXACTFIT, wxDefaultValidator, wxString("ok Button") );
   cancelButton = new wxButton( this, wxID_CANCEL, wxString("Cancel"), 
         wxPoint(220,150), wxSize(50,27), wxBU_EXACTFIT, 
         wxDefaultValidator, wxString("ok Button") );
   
   radioBox = new wxRadioBox( this, RADIO_BOX, 
         wxString("Select Exterior/ISO Value"), wxPoint(2,2), wxSize(395,215), 
         2, list, 1, wxRA_SPECIFY_COLS,	wxDefaultValidator, 
         wxString("radio box") );
  
   stTxtBox = new wxStaticBox( this, -1, wxString("Decimation Value"), wxPoint(225,20), wxSize(165,60), 0, wxString("") );
      
   stISOBox = new wxStaticBox( this, -1, wxString("Iso-surface Value"), wxPoint(35,60), wxSize(155,70), 0, 
         wxString("") );    
   
   spinnerBoxIso = new wxSpinCtrlDbl( *this, -1, wxEmptyString, wxPoint(55,87), 
         wxDefaultSize, 0, rngMin, rngMax, rngMin, rngMax/20.0, -1, 
         wxString("Iso value") );
   
   spinnerBoxDecim = new wxSpinCtrlDbl( *this, -1, wxEmptyString, wxPoint(245,40), 
         wxDefaultSize, 0, 0.0, 1.0, 0.2, 0.1, -1, 
         wxString("Decimation Value") );
   
   stISOBox->Disable();
   spinnerBoxIso->Disable();
   selectedExtVal = true;
}


makePopupDialog::~makePopupDialog()
{
}

void makePopupDialog:: onRadioBox( wxCommandEvent &event )
{
   if ( radioBox->GetStringSelection() == wxString("Exterior value") )
   {
      selectedExtVal = true;     
      stISOBox->Disable();      
      spinnerBoxIso->Disable();      
      
   }
   else
   {
      selectedExtVal = false;
      stISOBox->Enable( true );
      spinnerBoxIso->Enable( true );      
   }
   
}

bool makePopupDialog::getRadioVal()
{
   return selectedExtVal;
}


double makePopupDialog::getDecimationValu()
{
   return spinnerBoxDecim->GetValue();
}

double makePopupDialog::getIsoSurfValu()
{
   return spinnerBoxIso->GetValue();
}

#endif //_MAKEPOPUPDIALOG_H
