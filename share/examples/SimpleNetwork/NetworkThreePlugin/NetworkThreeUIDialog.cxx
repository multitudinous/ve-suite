#include <ves/conductor/util/CORBAServiceList.h>

#include "NetworkThreeUIDialog.h"

#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/textctrl.h>
#include <wx/stattext.h>
#include <wx/button.h>

BEGIN_EVENT_TABLE( NetworkThreeUIDialog, wxDialog )
EVT_BUTTON(		wxID_OK,		NetworkThreeUIDialog::SetText )
END_EVENT_TABLE()
///////////////////////////////////////////////////////////////////////////////
NetworkThreeUIDialog
::NetworkThreeUIDialog( wxWindow* parent, int id, 
						 ves::conductor::util::CORBAServiceList* service,
                         std::vector< std::string >* mNetworkThreeInputs)
: UIDialog( (wxWindow*) parent, id, _("NetworkThree") ),
		   p_mNetworkThreeInputs( mNetworkThreeInputs )
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
    
    wxStaticBox* networkThreeStaticBox = new wxStaticBox(this, wxID_ANY, _("Network Three Inputs"));
    wxStaticBoxSizer* networkThreeSizer = new wxStaticBoxSizer(networkThreeStaticBox, wxVERTICAL);
    mainSizer->Add(networkThreeSizer, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    wxBoxSizer* inputOneSizer = new wxBoxSizer(wxHORIZONTAL);
    networkThreeSizer->Add(inputOneSizer, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    wxStaticText* inputOneText = new wxStaticText( this, wxID_STATIC, _("Input One"), wxDefaultPosition, wxDefaultSize, 0 );
    inputOneSizer->Add(inputOneText, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    mTextCtrlOne = new wxTextCtrl( this, INPUT_ONE_TEXTCTRL, _T(""), wxDefaultPosition, wxDefaultSize, 0 );
    inputOneSizer->Add(mTextCtrlOne, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxBoxSizer* inputTwoSizer = new wxBoxSizer(wxHORIZONTAL);
    networkThreeSizer->Add(inputTwoSizer, 1, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    wxStaticText* inputTwoText = new wxStaticText( this, wxID_STATIC, _("Input Two"), wxDefaultPosition, wxDefaultSize, 0 );
    inputTwoSizer->Add(inputTwoText, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    mTextCtrlTwo = new wxTextCtrl( this, INPUT_TWO_TEXTCTRL, _T(""), wxDefaultPosition, wxDefaultSize, 0 );
    inputTwoSizer->Add(mTextCtrlTwo, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxBoxSizer* inputThreeSizer = new wxBoxSizer(wxHORIZONTAL);
    networkThreeSizer->Add(inputThreeSizer, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    wxStaticText* inputThreeText = new wxStaticText( this, wxID_STATIC, _("Input Three"), wxDefaultPosition, wxDefaultSize, 0 );
    inputThreeSizer->Add(inputThreeText, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    mTextCtrlThree = new wxTextCtrl( this, INPUT_THREE_TEXTCTRL, _T(""), wxDefaultPosition, wxDefaultSize, 0 );
    inputThreeSizer->Add(mTextCtrlThree, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    mUpdateButton = new wxButton( this, wxID_OK, _("Update"), wxDefaultPosition, wxDefaultSize, 0 );
    networkThreeSizer->Add(mUpdateButton, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

	SetAutoLayout( true );
	SetSizer( mainSizer );
	mainSizer->Fit( this );
}
///////////////////////////////////////////////////////////////////////////////
void NetworkThreeUIDialog::SetText( wxCommandEvent &event )
{		
	(*p_mNetworkThreeInputs).push_back( ConvertUnicode( 
        mTextCtrlOne->GetValue() ) );
    (*p_mNetworkThreeInputs).push_back( ConvertUnicode( 
        mTextCtrlTwo->GetValue() ) );
    (*p_mNetworkThreeInputs).push_back( ConvertUnicode( 
        mTextCtrlThree->GetValue() ) );

	event.Skip();
}
