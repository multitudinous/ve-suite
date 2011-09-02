#include <ves/conductor/util/CORBAServiceList.h>

#include "TankDlg.h"
#include <wx/filedlg.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/CommandPtr.h>

BEGIN_EVENT_TABLE(TankDlg,wxDialog)
	EVT_CLOSE(TankDlg::OnClose)
	EVT_BUTTON(ID_CANCELBUTTON,TankDlg::CancelClick)
	EVT_BUTTON(ID_SETBUTTON,TankDlg::SetButtonClick)
	EVT_BUTTON(ID_TANKBROWSEBUTTON,TankDlg::TankBrowseButtonClick)
END_EVENT_TABLE()

TankDlg::TankDlg(wxWindow *parent,
                 ves::conductor::util::CORBAServiceList* service,
                 wxWindowID id, const wxString &title, const wxPoint &position,
                 const wxSize& size, long style)
: wxDialog(parent, id, title, position, size, style),
    mServiceList( service )
{
	CreateGUIControls();
}

TankDlg::~TankDlg()
{
} 

void TankDlg::CreateGUIControls()
{
	Cancel = new wxButton(this, ID_CANCELBUTTON, wxT("Cancel"), wxPoint(202, 40), wxSize(75, 25), 0, wxDefaultValidator, wxT("Cancel"));
	Cancel->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	SetButton = new wxButton(this, ID_SETBUTTON, wxT("Set"), wxPoint(112, 40), wxSize(75, 25), 0, wxDefaultValidator, wxT("SetButton"));
	SetButton->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	TankBrowseBox = new wxTextCtrl(this, ID_TANKBROWSEBOX, wxT(""), wxPoint(156, 8), wxSize(121, 25), wxTE_READONLY, wxDefaultValidator, wxT("TankBrowseBox"));
	TankBrowseBox->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	TankBrowseButton = new wxButton(this, ID_TANKBROWSEBUTTON, wxT("..."), wxPoint(112, 8), wxSize(40, 25), 0, wxDefaultValidator, wxT("TankBrowseButton"));
	TankBrowseButton->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	TankLabel = new wxStaticText(this, ID_TANKLABEL, wxT("Tank"), wxPoint(8, 8), wxDefaultSize, 0, wxT("TankLabel"));
	TankLabel->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	SetTitle(wxT("Tank CAD"));
	SetIcon(wxNullIcon);
	SetSize(8,8,295,128);
	Center();
}

void TankDlg::OnClose(wxCloseEvent& /*event*/)
{
	Destroy();
}

void TankDlg::SetButtonClick(wxCommandEvent& event)
{
	//send message to the tank plugin containing the CAD parts
    ves::open::xml::DataValuePairSharedPtr tank(
          new ves::open::xml::DataValuePair() );
    tank->SetData( "TANK",
        TankBrowseBox->GetValue().c_str() );

    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    std::string mCommandName = "TANK_CAD";
    command->SetCommandName( mCommandName );

    command->AddDataValuePair( tank );
    
    mServiceList->SendCommandStringToXplorer( command );
    Destroy();
}

void TankDlg::CancelClick(wxCommandEvent& event)
{
	Destroy();
}

void TankDlg::TankBrowseButtonClick(wxCommandEvent& event)
{
	wxFileDialog fd( this, "Choose the Tank Body" );
	if( fd.ShowModal() == wxID_OK )
	{
        TankBrowseBox->SetValue( fd.GetPath() );
    }
}
