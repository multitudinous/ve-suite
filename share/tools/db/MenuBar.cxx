
// --- VE-Suite Includes --- //
//Don't move AppFrame.h below ToolBar.h
#include "AppFrame.h"
#include "MenuBar.h"
//#include "ConductorAppEnums.h"

//#include <ves/conductor/xpm/ToolBar/NewDocumentButton.xpm>

//#include <ves/conductor/util/CORBAServiceList.h>

//#include <ves/open/xml/Command.h>
//#include <ves/open/xml/DataValuePair.h>

BEGIN_EVENT_TABLE( MenuBar, wxMenuBar )

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
MenuBar::MenuBar()
    :
    wxMenuBar()
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
    ;
}
////////////////////////////////////////////////////////////////////////////////
