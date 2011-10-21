#include <ves/conductor/util/CORBAServiceList.h>

#include "SwitchDlg.h"
#include <wx/filedlg.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/CommandPtr.h>

BEGIN_EVENT_TABLE(SwitchDlg,wxDialog)	
	EVT_CLOSE(SwitchDlg::OnClose)
	EVT_BUTTON(ID_CANCELBUTTON,SwitchDlg::CancelClick)
	EVT_BUTTON(ID_SETBUTTON,SwitchDlg::SetButtonClick)
	EVT_BUTTON(ID_OFFBUTTONBROWSEBUTTON,SwitchDlg::OffButtonBrowseButtonClick)
	EVT_BUTTON(ID_ONBUTTONBROWSEBUTTON,SwitchDlg::OnButtonBrowseButtonClick)
	EVT_BUTTON(ID_SWITCHBODYBROWSEBUTTON,SwitchDlg::SwitchBodyBrowseButtonClick)
END_EVENT_TABLE()

SwitchDlg::SwitchDlg( wxWindow *parent,
                     ves::conductor::util::CORBAServiceList* service,
                     wxWindowID id, const wxString &title,
                     const wxPoint &position, const wxSize& size, long style )
: wxDialog(parent, id, title, position, size, style),
    mServiceList( service )
{
	CreateGUIControls();
}

SwitchDlg::~SwitchDlg()
{
} 

void SwitchDlg::CreateGUIControls()
{
	Cancel = new wxButton(this, ID_CANCELBUTTON, wxT("Cancel"),
        wxPoint(202, 103), wxSize(75, 25), 0, wxDefaultValidator,
        wxT("Cancel"));
	Cancel->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	SetButton = new wxButton(this, ID_SETBUTTON, wxT("Set"), wxPoint(112, 104),
        wxSize(75, 25), 0, wxDefaultValidator, wxT("SetButton"));
	SetButton->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	OffButtonBrowseBox = new wxTextCtrl(this, ID_OFFBUTTONBROWSEBOX, wxT(""),
        wxPoint(156, 72), wxSize(121, 25), wxTE_READONLY, wxDefaultValidator,
        wxT("OffButtonBrowseBox"));
	OffButtonBrowseBox->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL,
        false));

	OnButtonBrowseBox = new wxTextCtrl(this, ID_ONBUTTONBROWSEBOX, wxT(""),
        wxPoint(156, 40), wxSize(121, 25), wxTE_READONLY, wxDefaultValidator,
        wxT("OnButtonBrowseBox"));
	OnButtonBrowseBox->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	SwitchBodyBrowseBox = new wxTextCtrl(this, ID_SWITCHBODYBROWSEBOX, wxT(""),
        wxPoint(156, 8), wxSize(121, 25), wxTE_READONLY, wxDefaultValidator,
        wxT("SwitchBodyBrowseBox"));
	SwitchBodyBrowseBox->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL,
        false));

	OffButtonBrowseButton = new wxButton(this, ID_OFFBUTTONBROWSEBUTTON,
        wxT("..."), wxPoint(112, 72), wxSize(40, 25), 0, wxDefaultValidator,
        wxT("OffButtonBrowseButton"));
	OffButtonBrowseButton->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL,
        false));

	OnButtonBrowseButton = new wxButton(this, ID_ONBUTTONBROWSEBUTTON,
        wxT("..."), wxPoint(112, 40), wxSize(40, 25), 0, wxDefaultValidator,
        wxT("OnButtonBrowseButton"));
	OnButtonBrowseButton->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL,
        false));

	SwitchBodyBrowseButton = new wxButton(this, ID_SWITCHBODYBROWSEBUTTON,
        wxT("..."), wxPoint(112, 8), wxSize(40, 25), 0, wxDefaultValidator,
        wxT("SwitchBodyBrowseButton"));
	SwitchBodyBrowseButton->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL,
        false));

	OffButtonLabel = new wxStaticText(this, ID_OFFBUTTONLABEL,
        wxT("Off Button"), wxPoint(8, 72), wxDefaultSize, 0,
        wxT("OffButtonLabel"));
	OffButtonLabel->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	OnButtonLabel = new wxStaticText(this, ID_ONBUTTONLABEL, wxT("On Button"),
        wxPoint(8, 40), wxDefaultSize, 0, wxT("OnButtonLabel"));
	OnButtonLabel->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	SwitchBodyLabel = new wxStaticText(this, ID_SWITCHBODYLABEL,
        wxT("Switch Body"), wxPoint(8, 8), wxDefaultSize, 0,
        wxT("SwitchBodyLabel"));
	SwitchBodyLabel->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	SetTitle(wxT("Switch CAD"));
	SetIcon(wxNullIcon);
	SetSize(8,8,295,187);
	Center();
}

void SwitchDlg::OnClose(wxCloseEvent& /*event*/)
{
	Destroy();
}

/*
 * SetButtonClick
 */
void SwitchDlg::SetButtonClick(wxCommandEvent& event)
{
	//send message to the switch plugin containing the CAD parts
    ves::open::xml::DataValuePairSharedPtr switchBody(
          new ves::open::xml::DataValuePair() );
    switchBody->SetData( "SWITCH_BODY",
        ConvertUnicode( SwitchBodyBrowseBox->GetValue().c_str() ) );
    ves::open::xml::DataValuePairSharedPtr onButton(
          new ves::open::xml::DataValuePair() );
    onButton->SetData( "ON_BUTTON", ConvertUnicode( OnButtonBrowseBox->GetValue().c_str() ) );
    ves::open::xml::DataValuePairSharedPtr offButton(
          new ves::open::xml::DataValuePair() );
    offButton->SetData( "OFF_BUTTON", ConvertUnicode( OffButtonBrowseBox->GetValue().c_str() ) );

    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    std::string mCommandName = "SWITCH_CAD";
    command->SetCommandName( mCommandName );

    command->AddDataValuePair( switchBody );
    command->AddDataValuePair( onButton );
    command->AddDataValuePair( offButton );
    
    mServiceList->SendCommandStringToXplorer( command );
    Destroy();
}

/*
 * CancelClick
 */
void SwitchDlg::CancelClick(wxCommandEvent& event)
{
	Destroy();
}

/*
 * SwitchBodyBrowseButtonClick
 */
void SwitchDlg::SwitchBodyBrowseButtonClick(wxCommandEvent& event)
{
	wxFileDialog fd( this, _("Choose the Switch Body" ) );
	if( fd.ShowModal() == wxID_OK )
	{
        SwitchBodyBrowseBox->SetValue( fd.GetPath() );
    }
}

/*
 * StemBrowseButtonClick
 */
void SwitchDlg::OnButtonBrowseButtonClick(wxCommandEvent& event)
{
	wxFileDialog fd( this, _("Choose the On Button" ) );
	if( fd.ShowModal() == wxID_OK )
	{
        OnButtonBrowseBox->SetValue( fd.GetPath() );
    };
}

/*
 * HandWheelBrowseButtonClick
 */
void SwitchDlg::OffButtonBrowseButtonClick(wxCommandEvent& event)
{
	wxFileDialog fd( this, _( "Choose the Off Button" ) );
	if( fd.ShowModal() == wxID_OK )
	{
        OffButtonBrowseBox->SetValue( fd.GetPath() );
    }
}
