#include <ves/conductor/util/CORBAServiceList.h>

#include "NetworkTwoUIDialog.h"

#include <wx\sizer.h>
#include <wx\statbox.h>
#include <wx\textctrl.h>
#include <wx\button.h>

BEGIN_EVENT_TABLE( NetworkTwoUIDialog, wxDialog )
EVT_BUTTON(		wxID_OK,		NetworkTwoUIDialog::SetText )
END_EVENT_TABLE()
///////////////////////////////////////////////////////////////////////////////
NetworkTwoUIDialog
::NetworkTwoUIDialog( wxWindow* parent, int id, 
						 ves::conductor::util::CORBAServiceList* service,
						 std::string* mText)
: UIDialog( (wxWindow*) parent, id, _("NetworkTwo") ),
		   p_mText( mText )
{
	BuildPage();

	mServiceList = service;
}
///////////////////////////////////////////////////////////////////////////////
NetworkTwoUIDialog::~NetworkTwoUIDialog()
{
	mServiceList->CleanUp();
}
///////////////////////////////////////////////////////////////////////////////
void NetworkTwoUIDialog::BuildPage()
{
	wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);

    wxStaticBox* networkOneStaticBox = 
		new wxStaticBox( this, wxID_ANY, _("Network One Input") );
    wxStaticBoxSizer* networkOneSizer = 
		new wxStaticBoxSizer( networkOneStaticBox, wxVERTICAL );
    mainSizer->Add( networkOneSizer, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5 );

    mTextCtrl = new wxTextCtrl( this, ID_TEXTCTRL, _T(""), 
		wxDefaultPosition, wxDefaultSize, 0 );
    networkOneSizer->Add( mTextCtrl, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

	mUpdateButton = new wxButton( this, wxID_OK, _("Update"), 
		wxDefaultPosition, wxDefaultSize, 0 );
    networkOneSizer->Add( mUpdateButton, 1, 
		wxALIGN_CENTER_HORIZONTAL|wxALL, 5 );

	SetAutoLayout( true );
	SetSizer( mainSizer );
	mainSizer->Fit( this );
}
///////////////////////////////////////////////////////////////////////////////
bool NetworkTwoUIDialog::TransferDataFromWindow()
{
	return true;
}
///////////////////////////////////////////////////////////////////////////////
bool NetworkTwoUIDialog::TransferDataToWindow()
{
	return true;
}
///////////////////////////////////////////////////////////////////////////////
void NetworkTwoUIDialog::SetText( wxCommandEvent &event )
{		
	(*p_mText) = ConvertUnicode( mTextCtrl->GetValue() );
	event.Skip();
}