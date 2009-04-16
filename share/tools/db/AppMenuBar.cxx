
// --- VE-Suite Includes --- //
#include "AppMenuBar.h"
#include "DBAppEnums.h"

BEGIN_EVENT_TABLE( AppMenuBar, wxMenuBar )

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
AppMenuBar::AppMenuBar()
    :
    wxMenuBar(),
    m_fileMenu( NULL ),
    m_helpMenu( NULL )
{
    CreateGUI();
}
////////////////////////////////////////////////////////////////////////////////
AppMenuBar::~AppMenuBar()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AppMenuBar::CreateGUI()
{
	SetBackgroundColour( wxColour( 255, 255, 255 ) );
	
	m_fileMenu = new wxMenu();
	Append( m_fileMenu, wxT( "File" ) );
	
	m_helpMenu = new wxMenu();
	Append( m_helpMenu, wxT( "Help" ) );
}
////////////////////////////////////////////////////////////////////////////////
