#include <ves/conductor/util/CORBAServiceList.h>

#include "NetworkTwoUIDialog.h"

#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/textctrl.h>
#include <wx/button.h>

BEGIN_EVENT_TABLE( NetworkTwoUIDialog, wxDialog )
EVT_BUTTON(		wxID_OK,		NetworkTwoUIDialog::SetText )
END_EVENT_TABLE()
///////////////////////////////////////////////////////////////////////////////
NetworkTwoUIDialog
::NetworkTwoUIDialog( wxWindow* parent, int id, 
						 ves::conductor::util::CORBAServiceList* service,
						 std::string* mTextTwo)
: UIDialog( (wxWindow*) parent, id, _("NetworkTwo") ),
		   p_mTextTwo( mTextTwo )
{
	BuildPage();

	mServiceList = service;
}
///////////////////////////////////////////////////////////////////////////////
NetworkTwoUIDialog::~NetworkTwoUIDialog()
{
    ;
}
///////////////////////////////////////////////////////////////////////////////
void NetworkTwoUIDialog::BuildPage()
{
	wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);

    wxStaticBox* networkOneStaticBox = 
		new wxStaticBox( this, wxID_ANY, _("Network Two Input") );
    wxStaticBoxSizer* networkOneSizer = 
		new wxStaticBoxSizer( networkOneStaticBox, wxVERTICAL );
    mainSizer->Add( networkOneSizer, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5 );

    mTextTwoCtrl = new wxTextCtrl( this, ID_TEXTCTRL, _T(""), 
		wxDefaultPosition, wxDefaultSize, 0 );
    networkOneSizer->Add( mTextTwoCtrl, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

	mUpdateButton = new wxButton( this, wxID_OK, _("Update"), 
		wxDefaultPosition, wxDefaultSize, 0 );
    networkOneSizer->Add( mUpdateButton, 1, 
		wxALIGN_CENTER_HORIZONTAL|wxALL, 5 );

	SetAutoLayout( true );
	SetSizer( mainSizer );
	mainSizer->Fit( this );
}
///////////////////////////////////////////////////////////////////////////////
void NetworkTwoUIDialog::SetText( wxCommandEvent &event )
{		
	(*p_mTextTwo) = ConvertUnicode( mTextTwoCtrl->GetValue() );
	event.Skip();
}