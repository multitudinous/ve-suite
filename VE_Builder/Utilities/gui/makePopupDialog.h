//#include "cfdGrid2Surface.h"
//#include <vtkPolyData.h>
#ifndef _MAKEPOPUPDIALOG_H
#define _MAKEPOPUPDIALOG_H
class makePopupDialog: public wxDialog
{
   private:
         wxButton* okButton;
         wxButton* cancelButton;
         wxRadioBox* radioBox;
         wxSlider* sliderSelection;
         wxSlider* sliderISO;
         wxStaticBox* stISOBox;
         wxStaticBox* stTxtBox;
         
         bool selectedExtVal;
         enum{ RADIO_BOX, SLIDER_SELECTION, SLIDER_ISO };
         
         //---------VTK stuff
         //vtkPolyData* surface;
         
   protected:
         DECLARE_EVENT_TABLE()
         void onRadioBox( wxCommandEvent &event );
         
   public:
         makePopupDialog( wxWindow* frm, int w, int h );
         ~makePopupDialog();
         bool getRadioVal();
         float getDecimationValu();
         float getIsoSurfValu();
         void makeExtValSurf( vtkDataSet* vtkdataset );
};


makePopupDialog::makePopupDialog( wxWindow* frm, int w, int h)
      :wxDialog( frm, -1, "Make VTK Surface", 
      wxDefaultPosition, wxSize(w, h) , 
      wxDEFAULT_DIALOG_STYLE, "dlg box" )
{
   wxString list[] = { "Exterior value", "ISO value" }; 
   okButton = new wxButton( this, wxID_OK, wxString("Ok"), wxPoint(110,150),
			wxSize(50,27), wxBU_EXACTFIT, wxDefaultValidator, wxString("ok                   Button") );
   cancelButton = new wxButton( this, wxID_CANCEL, wxString("Cancel"), 
         wxPoint(220,150), wxSize(50,27), wxBU_EXACTFIT, 
         wxDefaultValidator, wxString("ok Button") );
   
   radioBox = new wxRadioBox( this, RADIO_BOX, 
         wxString("Select Exterior/ISO Value"), wxPoint(2,2), wxSize(395,215), 
         2, list, 1, wxRA_SPECIFY_COLS,	wxDefaultValidator, 
         wxString("radio box") );
  
   stTxtBox = new wxStaticBox( this, -1, wxString("Decimation Value"), 
        wxPoint(225,20), wxSize(165,60), 0, wxString("") );
      
	
   sliderSelection = new wxSlider( this, SLIDER_SELECTION, 5, 0, 10, wxPoint(240,35), 
			wxSize(140, 15), wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP,             
			wxDefaultValidator, wxString("Decimation Val") );
      
   sliderISO = new wxSlider( this, SLIDER_ISO, 5, 0, 10, wxPoint(40,75), wxSize(140, 10), 
         wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP, wxDefaultValidator, wxString("Sli2") );
      
 
   stISOBox = new wxStaticBox( this, -1, wxString("Scalar Range"), wxPoint(35,60), wxSize(155,70), 0, 
         wxString("") );      
   
   
   
   stISOBox->Disable();
   sliderISO->Disable();
   selectedExtVal = true;
   //surface = NULL;
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
      sliderISO->Disable();
      
   }
   else
   {
      selectedExtVal = false;
      stISOBox->Enable( true );
      sliderISO->Enable( true );
   }
}

bool makePopupDialog::getRadioVal()
{
   return selectedExtVal;
}


float makePopupDialog::getDecimationValu()
{
   return (float)sliderSelection->GetValue()/(float)(10);
}

float makePopupDialog::getIsoSurfValu()
{
   return (float)sliderISO->GetValue()/(float)(10);
}

void makePopupDialog::makeExtValSurf( vtkDataSet* vtkdataset )
{
   //surface = cfdGrid2Surface( dataset, deciVal );
}

#endif //_MAKEPOPUPDIALOG_H
