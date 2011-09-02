#include <ves/conductor/util/CORBAServiceList.h>

#include "ValveDlg.h"
#include <wx/filedlg.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/CommandPtr.h>

BEGIN_EVENT_TABLE(ValveDlg,wxDialog)
	EVT_CLOSE(ValveDlg::OnClose)
	EVT_BUTTON(ID_CANCELBUTTON,ValveDlg::CancelClick)
	EVT_BUTTON(ID_SETBUTTON,ValveDlg::SetButtonClick)
	EVT_BUTTON(ID_STEMBROWSEBUTTON,ValveDlg::StemBrowseButtonClick)
	EVT_BUTTON(ID_HANDWHEELBROWSEBUTTON,ValveDlg::HandWheelBrowseButtonClick)
	EVT_BUTTON(ID_VALVEBODYBROWSEBUTTON,ValveDlg::ValveBodyBrowseButtonClick)
END_EVENT_TABLE()

///////////////////////////////////////////////////////////////////////////////
ValveDlg::ValveDlg(wxWindow *parent,
                   ves::conductor::util::CORBAServiceList* service,
                   wxWindowID id, const wxString &title,
                   const wxPoint &position, const wxSize& size, long style)
: wxDialog(parent, id, title, position, size, style),
    mServiceList( service )
{
	CreateGUIControls();
}

///////////////////////////////////////////////////////////////////////////////
ValveDlg::~ValveDlg()
{
} 

///////////////////////////////////////////////////////////////////////////////
void ValveDlg::CreateGUIControls()
{
	Cancel = new wxButton(this, ID_CANCELBUTTON, wxT("Cancel"),
        wxPoint(202, 103), wxSize(75, 25), 0, wxDefaultValidator,
        wxT("Cancel"));
	Cancel->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	SetButton = new wxButton(this, ID_SETBUTTON, wxT("Set"), wxPoint(112, 104),
        wxSize(75, 25), 0, wxDefaultValidator, wxT("SetButton"));
	SetButton->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	StemBrowseBox = new wxTextCtrl(this, ID_STEMBROWSEBOX, wxT(""),
        wxPoint(156, 72), wxSize(121, 25), wxTE_READONLY, wxDefaultValidator,
        wxT("StemBrowseBox"));
	StemBrowseBox->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	HandWheelBrowseBox = new wxTextCtrl(this, ID_HANDWHEELBROWSEBOX, wxT(""),
        wxPoint(156, 40), wxSize(121, 25), wxTE_READONLY, wxDefaultValidator,
        wxT("HandWheelBrowseBox"));
	HandWheelBrowseBox->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL,
        false));

	ValveBodyBrowseBox = new wxTextCtrl(this, ID_VALVEBODYBROWSEBOX, wxT(""),
        wxPoint(156, 8), wxSize(121, 25), wxTE_READONLY, wxDefaultValidator,
        wxT("ValveBodyBrowseBox"));
	ValveBodyBrowseBox->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL,
        false));

	StemBrowseButton = new wxButton(this, ID_STEMBROWSEBUTTON, wxT("..."),
        wxPoint(112, 72), wxSize(40, 25), 0, wxDefaultValidator,
        wxT("StemBrowseButton"));
	StemBrowseButton->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	HandWheelBrowseButton = new wxButton(this, ID_HANDWHEELBROWSEBUTTON,
        wxT("..."), wxPoint(112, 40), wxSize(40, 25), 0, wxDefaultValidator,
        wxT("HandWheelBrowseButton"));
	HandWheelBrowseButton->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL,
        false));

	ValveBodyBrowseButton = new wxButton(this, ID_VALVEBODYBROWSEBUTTON,
        wxT("..."), wxPoint(112, 8), wxSize(40, 25), 0, wxDefaultValidator,
        wxT("ValveBodyBrowseButton"));
	ValveBodyBrowseButton->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL,
        false));

	StemLabel = new wxStaticText(this, ID_STEMLABEL, wxT("Stem"),
        wxPoint(8, 72), wxDefaultSize, 0, wxT("StemLabel"));
	StemLabel->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	HandWheelLabel = new wxStaticText(this, ID_HANDWHEELLABEL,
        wxT("Hand Wheel"), wxPoint(8, 40), wxDefaultSize, 0,
        wxT("HandWheelLabel"));
	HandWheelLabel->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	ValveBodyLabel = new wxStaticText(this, ID_VALVEBODYLABEL,
        wxT("Valve Body"), wxPoint(8, 8), wxDefaultSize, 0,
        wxT("ValveBodyLabel"));
	ValveBodyLabel->SetFont(wxFont(12, wxSWISS, wxNORMAL, wxNORMAL, false));

	SetTitle(wxT("Valve CAD"));
	SetIcon(wxNullIcon);
	SetSize(8,8,295,187);
	Center();
}

///////////////////////////////////////////////////////////////////////////////
void ValveDlg::OnClose(wxCloseEvent& /*event*/)
{
	Destroy();
}

///////////////////////////////////////////////////////////////////////////////
void ValveDlg::SetButtonClick(wxCommandEvent& event)
{
	//send message to the valve plugin containing the CAD parts
    ves::open::xml::DataValuePairSharedPtr valveBody(
          new ves::open::xml::DataValuePair() );
    valveBody->SetData( "VALVE_BODY",
        ValveBodyBrowseBox->GetValue().c_str() );
    ves::open::xml::DataValuePairSharedPtr handWheel(
          new ves::open::xml::DataValuePair() );
    handWheel->SetData( "HAND_WHEEL", HandWheelBrowseBox->GetValue().c_str() );
    ves::open::xml::DataValuePairSharedPtr stem(
          new ves::open::xml::DataValuePair() );
    stem->SetData( "STEM", StemBrowseBox->GetValue().c_str() );

    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    std::string mCommandName = "VALVE_CAD";
    command->SetCommandName( mCommandName );

    command->AddDataValuePair( valveBody );
    command->AddDataValuePair( handWheel );
    command->AddDataValuePair( stem );
    
    mServiceList->SendCommandStringToXplorer( command );
    Destroy();
}

///////////////////////////////////////////////////////////////////////////////
void ValveDlg::CancelClick(wxCommandEvent& event)
{
	Destroy();
}

///////////////////////////////////////////////////////////////////////////////
void ValveDlg::ValveBodyBrowseButtonClick(wxCommandEvent& event)
{
	wxFileDialog fd( this, "Choose the Valve Body" );
	if( fd.ShowModal() == wxID_OK )
	{
        ValveBodyBrowseBox->SetValue( fd.GetPath() );
    }
}

///////////////////////////////////////////////////////////////////////////////
void ValveDlg::StemBrowseButtonClick(wxCommandEvent& event)
{
	wxFileDialog fd( this, "Choose the Stem" );
	if( fd.ShowModal() == wxID_OK )
	{
        StemBrowseBox->SetValue( fd.GetPath() );
    };
}

///////////////////////////////////////////////////////////////////////////////
void ValveDlg::HandWheelBrowseButtonClick(wxCommandEvent& event)
{
	wxFileDialog fd( this, "Choose the Hand Wheel" );
	if( fd.ShowModal() == wxID_OK )
	{
        HandWheelBrowseBox->SetValue( fd.GetPath() );
    }
}
