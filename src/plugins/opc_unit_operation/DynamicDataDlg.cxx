#include <ves/conductor/util/CORBAServiceList.h>

#include "DynamicDataDlg.h"
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLReaderWriter.h>

using namespace ves::open::xml::model;
using namespace ves::open::xml;

BEGIN_EVENT_TABLE(DynamicDataDlg,wxDialog)
	EVT_CLOSE(DynamicDataDlg::OnClose)
	EVT_BUTTON(ID_CLOSEBUTTON, DynamicDataDlg::closeButtonClick )
	//EVT_BUTTON(ID_SETBUTTON,DynamicDataDlg::setButtonClick)
	EVT_TIMER( TIMER_ID, DynamicDataDlg::OnTimer )
END_EVENT_TABLE()

DynamicDataDlg::DynamicDataDlg(wxWindow *parent, wxWindowID id, const wxString &title, const wxPoint &position, const wxSize& size, long style)
: wxDialog(parent, id, title, position, size, style)
{
	CreateGUIControls();
	m_timer = new wxTimer(this, TIMER_ID);
}

DynamicDataDlg::~DynamicDataDlg()
{

} 

void DynamicDataDlg::CreateGUIControls()
{
	WxEdit1 = new wxTextCtrl(this, ID_WXEDIT1, wxT(""), wxPoint(47, 15), wxSize(123, 19), 0, wxDefaultValidator, wxT("WxEdit1"));

	WxStaticText2 = new wxStaticText(this, ID_WXSTATICTEXT2, wxT("Value"), wxPoint(9, 14), wxDefaultSize, 0, wxT("WxStaticText2"));

	closeButton = new wxButton(this, ID_CLOSEBUTTON, wxT("Close"), wxPoint(72, 47), wxSize(75, 25), 0, wxDefaultValidator, wxT("closeButton"));

	setButton = new wxButton(this, ID_SETBUTTON, wxT("Set"), wxPoint(174, 13), wxSize(31, 25), 0, wxDefaultValidator, wxT("setButton"));

	SetTitle(wxT("Dynamic Data"));
	SetIcon(wxNullIcon);
	SetSize(8,8,226,112);
	Center();
}

void DynamicDataDlg::OnClose(wxCloseEvent& /*event*/)
{
	m_timer->Stop();
	Destroy();
}

void DynamicDataDlg::setButtonClick(wxCommandEvent& event)
{

}

void DynamicDataDlg::closeButtonClick(wxCommandEvent& event)
{
	m_timer->Stop();
	Destroy();
}

void DynamicDataDlg::SetCORBAServiceList(
	ves::conductor::util::CORBAServiceList* servicelist )
{
	serviceList = servicelist;
	//m_timer->Start(1000);
}

void DynamicDataDlg::SetName( std::string name )
{
	compName = name;
}

void DynamicDataDlg::ReadValue( )
{
    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "getOPCValue" );
    ves::open::xml::DataValuePairPtr data( new ves::open::xml::DataValuePair() );
	//hardcode the "D1_"  This will need to be parsed from the .tree file
    data->SetData( std::string( "ModuleName" ), "D1_"+compName );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    std::string nw_str = serviceList->Query( status );

    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();
    networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector =
        networkReader.GetLoadedXMLObjects();
    ves::open::xml::CommandPtr cmd =
        boost::dynamic_pointer_cast<ves::open::xml::Command>
        ( objectVector.at( 0 ) );
    ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( 0 );	

    WxEdit1->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
}


void DynamicDataDlg::OnTimer( wxTimerEvent& event )
{
	ReadValue();
}