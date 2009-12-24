///-----------------------------------------------------------------
///
/// @file      DynamicDataDlg.cpp
/// @author    tjordan
/// Created:   12/16/2009 10:58:31 AM
/// @section   DESCRIPTION
///            DynamicDataDlg class implementation
///
///------------------------------------------------------------------
#include <ves/conductor/util/CORBAServiceList.h>

#include "DynamicDataDlg.h"
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLReaderWriter.h>

using namespace ves::open::xml::model;
using namespace ves::open::xml;

//Do not add custom headers
//wxDev-C++ designer will remove them
////Header Include Start
////Header Include End

//----------------------------------------------------------------------------
// DynamicDataDlg
//----------------------------------------------------------------------------
//Add Custom Events only in the appropriate block.
//Code added in other places will be removed by wxDev-C++
////Event Table Start
BEGIN_EVENT_TABLE(DynamicDataDlg,wxDialog)
	////Manual Code Start
	////Manual Code End
	
	EVT_CLOSE(DynamicDataDlg::OnClose)
	EVT_BUTTON(ID_CLOSEBUTTON, DynamicDataDlg::closeButtonClick )
	//EVT_BUTTON(ID_SETBUTTON,DynamicDataDlg::setButtonClick)
	EVT_TIMER( TIMER_ID, DynamicDataDlg::OnTimer )
END_EVENT_TABLE()
////Event Table End

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
	//Do not add custom code between
	//GUI Items Creation Start and GUI Items Creation End.
	//wxDev-C++ designer will remove them.
	//Add the custom code before or after the blocks
	////GUI Items Creation Start

	WxEdit1 = new wxTextCtrl(this, ID_WXEDIT1, wxT(""), wxPoint(47, 15), wxSize(123, 19), 0, wxDefaultValidator, wxT("WxEdit1"));

	WxStaticText2 = new wxStaticText(this, ID_WXSTATICTEXT2, wxT("Value"), wxPoint(9, 14), wxDefaultSize, 0, wxT("WxStaticText2"));

	closeButton = new wxButton(this, ID_CLOSEBUTTON, wxT("Close"), wxPoint(72, 47), wxSize(75, 25), 0, wxDefaultValidator, wxT("closeButton"));

	setButton = new wxButton(this, ID_SETBUTTON, wxT("Set"), wxPoint(174, 13), wxSize(31, 25), 0, wxDefaultValidator, wxT("setButton"));

	SetTitle(wxT("Dynamic Data"));
	SetIcon(wxNullIcon);
	SetSize(8,8,226,112);
	Center();
	
	////GUI Items Creation End
}

void DynamicDataDlg::OnClose(wxCloseEvent& /*event*/)
{
	m_timer->Stop();
	Destroy();
}


/*
 * setButtonClick
 */
void DynamicDataDlg::setButtonClick(wxCommandEvent& event)
{
	// insert your code here
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
	m_timer->Start(1000);
}

void DynamicDataDlg::SetName( std::string name )
{
	compName = name;
}

void DynamicDataDlg::ReadValue( )
{
    //std::string compName = GetVEModel()->GetPluginName();
    //compName = "Data.Blocks." + compName;

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

    WxEdit1->SetValue( pair->GetDataString().c_str() );
}


void DynamicDataDlg::OnTimer( wxTimerEvent& event )
{
	ReadValue();
}