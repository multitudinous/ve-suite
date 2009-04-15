
// --- VE-Suite Includes --- //
//Don't move AppFrame.h below ToolBar.h
#include "AppFrame.h"
#include "MenuBar.h"
#include "DBAppEnums.h"

BEGIN_EVENT_TABLE( MenuBar, wxMenuBar )

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
MenuBar::MenuBar()
    :
    wxMenuBar(),
    m_fileMenu( NULL ),
    m_helpMenu( NULL )
{
    CreateGUI();
}
////////////////////////////////////////////////////////////////////////////////
MenuBar::~MenuBar()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void MenuBar::CreateGUI()
{
	SetBackgroundColour( wxColour( 255, 255, 255 ) );
	
	m_fileMenu = new wxMenu();
	Append( m_fileMenu, wxT( "File" ) );
	
	m_helpMenu = new wxMenu();
	Append( m_helpMenu, wxT( "Help" ) );
}
////////////////////////////////////////////////////////////////////////////////
