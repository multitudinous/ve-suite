#include <ves/conductor/util/CORBAServiceList.h>

#include "NetworkThreeUIDialog.h"

#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/textctrl.h>
#include <wx/button.h>

BEGIN_EVENT_TABLE( NetworkThreeUIDialog, wxDialog )
EVT_BUTTON(		wxID_OK,		NetworkThreeUIDialog::SetText )
END_EVENT_TABLE()
///////////////////////////////////////////////////////////////////////////////
NetworkThreeUIDialog
::NetworkThreeUIDialog( wxWindow* parent, int id, 
						 ves::conductor::util::CORBAServiceList* service,
						 std::string* mTextThree)
: UIDialog( (wxWindow*) parent, id, _("NetworkThree") ),
		   p_mTextThree( mTextThree )
{
	BuildPage();

	mServiceList = service;
}
///////////////////////////////////////////////////////////////////////////////
NetworkThreeUIDialog::~NetworkThreeUIDialog()
{
    ;
}
///////////////////////////////////////////////////////////////////////////////
void NetworkThreeUIDialog::BuildPage()
{
	wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);

    wxStaticBox* networkThreeStaticBox = 
		new wxStaticBox( this, wxID_ANY, _("Network Three Input") );
    wxStaticBoxSizer* networkThreeSizer = 
		new wxStaticBoxSizer( networkThreeStaticBox, wxVERTICAL );
    mainSizer->Add( networkThreeSizer, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5 );

    mTextThreeCtrl = new wxTextCtrl( this, ID_TEXTCTRL, _T(""), 
		wxDefaultPosition, wxDefaultSize, 0 );
    networkThreeSizer->Add( mTextThreeCtrl, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

	mUpdateButton = new wxButton( this, wxID_OK, _("Update"), 
		wxDefaultPosition, wxDefaultSize, 0 );
    networkThreeSizer->Add( mUpdateButton, 1, 
		wxALIGN_CENTER_HORIZONTAL|wxALL, 5 );

	SetAutoLayout( true );
	SetSizer( mainSizer );
	mainSizer->Fit( this );
}
///////////////////////////////////////////////////////////////////////////////
void NetworkThreeUIDialog::SetText( wxCommandEvent &event )
{		
	(*p_mTextThree) = ConvertUnicode( mTextThreeCtrl->GetValue() );
	event.Skip();
}
