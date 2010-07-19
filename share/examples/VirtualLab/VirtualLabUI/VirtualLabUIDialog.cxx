
#include <ves/conductor/util/CORBAServiceList.h>

#include "VirtualLabUIDialog.h"

#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/textctrl.h>
#include <wx/button.h>

BEGIN_EVENT_TABLE( VirtualLabUIDialog, wxDialog )
EVT_BUTTON(        wxID_OK,        VirtualLabUIDialog::SetText )
END_EVENT_TABLE()

///////////////////////////////////////////////////////////////////////////////
VirtualLabUIDialog
::VirtualLabUIDialog( wxWindow* parent, int id, 
                         ves::conductor::util::CORBAServiceList* service,
                         std::string* mTextOne)
: UIDialog( (wxWindow*) parent, id, _("VirtualLab") ),
           p_mTextOne( mTextOne )
{
    BuildPage();

    mServiceList = service;
}
///////////////////////////////////////////////////////////////////////////////
VirtualLabUIDialog::~VirtualLabUIDialog()
{
    ;
}
///////////////////////////////////////////////////////////////////////////////
void VirtualLabUIDialog::BuildPage()
{
    wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);

    wxStaticBox* VirtualLabStaticBox = 
        new wxStaticBox( this, wxID_ANY, _("Virtual Lab Input") );
    wxStaticBoxSizer* VirtualLabSizer = 
        new wxStaticBoxSizer( VirtualLabStaticBox, wxVERTICAL );
    mainSizer->Add( VirtualLabSizer, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5 );

    mTextOneCtrl = new wxTextCtrl( this, ID_TEXTCTRL, _T(""), 
        wxDefaultPosition, wxDefaultSize, 0 );
    VirtualLabSizer->Add( mTextOneCtrl, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    mUpdateButton = new wxButton( this, wxID_OK, _("Update"), 
        wxDefaultPosition, wxDefaultSize, 0 );
    VirtualLabSizer->Add( mUpdateButton, 1, 
        wxALIGN_CENTER_HORIZONTAL|wxALL, 5 );

    SetAutoLayout( true );
    SetSizer( mainSizer );
    mainSizer->Fit( this );
}
///////////////////////////////////////////////////////////////////////////////
void VirtualLabUIDialog::SetText( wxCommandEvent &event )
{        
    (*p_mTextOne) = ConvertUnicode( mTextOneCtrl->GetValue() );
    event.Skip();
}
