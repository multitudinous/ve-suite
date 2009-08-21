//--- Header Include ---//
#include "DataSetArraySelectionDialog.h"

//--- wxWidgets Includes ---//
#include <wx/dialog.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/button.h>

// --- This Plugin Includes --- //
#include "ArrayCheckBox.h"
#include "DataSet.h"
#include "DataSetArray.h"
#include "LoadDataGUI.h"

// --- C++ Includes --- //
#include <iostream>

using namespace vascularsimviewer;

BEGIN_EVENT_TABLE( DataSetArraySelectionDialog, wxDialog )
END_EVENT_TABLE()

// Constructor
DataSetArraySelectionDialog::DataSetArraySelectionDialog( wxWindowID id, const wxString& title, int numArrays, DataSet* parent )
    :
    wxDialog( NULL, id, title,
        wxDefaultPosition,
        wxDefaultSize,
        wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxCLOSE_BOX | wxMAXIMIZE_BOX | wxMINIMIZE_BOX ),
    ArrayNumber( numArrays ),
    parentDataSet( parent )
{
    //wxFormBuilder
    this->SetSizeHints( wxDefaultSize, wxDefaultSize );
	
	wxBoxSizer* mainSizer;
	mainSizer = new wxBoxSizer( wxVERTICAL );
	
	wxStaticBoxSizer* staticBoxSizer;
	staticBoxSizer = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Available Datasets") ), wxVERTICAL );
	
    //not wxFormBuilder since the ArrayCheckBox is class unique to this plugin, but derived from wxCheckBox
    int i; //Loop Counter
    for( i=0; i<numArrays; i++ )
    {
        CheckBoxes.push_back( new ArrayCheckBox( this, parentDataSet->GetPtArray(i)->GetArrayName() ) );
        CheckBoxes.back()->CreateCheckBox();
	    staticBoxSizer->Add( CheckBoxes.back(), 1, wxALIGN_LEFT|wxALL, 5 );
    }
	
	mainSizer->Add( staticBoxSizer, 0, wxALL|wxEXPAND, 5 );
    
    // OK and Cancel Buttons - Begin
	wxStdDialogButtonSizer* stdButtonSizer = new wxStdDialogButtonSizer();
    wxButton* okButton = new wxButton( this, wxID_OK );
	stdButtonSizer->AddButton( okButton );
    
	wxButton* cancelButton = new wxButton( this, wxID_CANCEL );
	stdButtonSizer->AddButton( cancelButton );
    // OK and Cancel Buttons - End
    
	stdButtonSizer->Realize();
	mainSizer->Add( stdButtonSizer, 0, wxALIGN_CENTER, 5 );
	
	this->SetSizer( mainSizer );
	this->Layout();
    mainSizer->Fit( this );
	mainSizer->SetSizeHints( this );
    CenterOnScreen();
    
    //Functionality Connections
    Connect( wxID_OK, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( DataSetArraySelectionDialog::OnOK ) );
}

// Destructor
DataSetArraySelectionDialog::~DataSetArraySelectionDialog()
{
    Disconnect( wxID_OK, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( DataSetArraySelectionDialog::OnOK ) );
}

// What to do when OK is clicked
void DataSetArraySelectionDialog::OnOK( wxCommandEvent &event )
{
    int i;
    
    //This loop updates the true/false status of an array based on the ckeckboxes when the dialog is closed
    for( i=0; i<ArrayNumber; i++ )
    {
        parentDataSet->GetPtArray(i)->SetArrayStatus( CheckBoxes[i]->IsChecked() );
    }
    
    Close();
    
    parentDataSet->GetLoadDataGui()->Close();
    
    parentDataSet->LoadDataInXplorer();
}
