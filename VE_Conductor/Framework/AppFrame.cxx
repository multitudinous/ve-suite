/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Includes --- //
#include "VE_Conductor/Utilities/CORBAServiceList.h"

#include "VE_Conductor/Framework/AppFrame.h"

#include <wx/imaglist.h>
#include <wx/artprov.h>
#include <wx/msgdlg.h>
#include <wx/textctrl.h>
#include <wx/image.h>
#include <wx/sizer.h>
#include <wx/splitter.h>
#include <wx/dialog.h>
#include <wx/filename.h>
#include <wx/config.h>
#include <wx/msgdlg.h>

#include "VE_Conductor/GUIPlugin/ResultPanel.h"
#include "VE_Conductor/Utilities/OrbThread.h"
#include "VE_Conductor/GUIPlugin/FinancialDialog.h"
#include "VE_Conductor/GUIPlugin/TextResultDialog.h"
#include "VE_Conductor/GUIPlugin/TexTable.h"
#include "VE_Conductor/GUIPlugin/GlobalParamDialog.h"
#include "VE_Conductor/GUIPlugin/SummaryResultDialog.h"
#include "VE_Conductor/GUIPlugin/FindDialog.h"
#include "VE_Conductor/GUIPlugin/UserPreferencesDataBuffer.h"

#include "VE_Conductor/Framework/ConductorApp.h"
#include "VE_Conductor/Framework/UserPreferences.h"
#include "VE_Conductor/Framework/Avail_Modules.h"
#include "VE_Conductor/Framework/UI_TeacherTab.h"
#include "VE_Conductor/Framework/DeviceProperties.h"
#include "VE_Conductor/Framework/NavigationPane.h"
#include "VE_Conductor/Framework/Splitter.h"
#include "VE_Conductor/Framework/ViewLocPane.h"
#include "VE_Conductor/Framework/Network.h"
#include "VE_Conductor/Framework/MainToolBar.h"
#include "VE_Conductor/Framework/ExportMenu.h"

#include "VE_Conductor/Utilities/CADNodeManagerDlg.h"
#include "VE_Conductor/GUIPlugin/Module.h"
#include "VE_Conductor/Utilities/Tag.h"

#include "VE_Open/XML/DOMDocumentManager.h"
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/XMLObjectFactory.h"
#include "VE_Open/XML/XMLCreator.h"
#include "VE_Open/XML/CAD/CADCreator.h"
#include "VE_Open/XML/Shader/ShaderCreator.h"
#include "VE_Open/XML/Model/ModelCreator.h"
#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/CommandWeakPtr.h"

// --- wxWidgets Includes --- //
#include <wx/image.h>
#include <wx/bitmap.h>
#include <wx/splash.h>
#include <wx/utils.h>
#include <wx/app.h>
#include <wx/cmndata.h>
#include <wx/colordlg.h>
#include <wx/docview.h>
#include <wx/dirdlg.h>

#include "VE_Installer/installer/installerImages/ve_icon64x64.xpm"
#include "VE_Installer/installer/installerImages/ve_icon32x32.xpm"
#include "VE_Installer/include/VEConfig.h"

// --- C/C++ Libraries --- //
#include <sstream>
#include <iomanip>

using namespace VE_Conductor::GUI_Utilities;
using namespace VE_Conductor;
using namespace VE_XML;
using namespace VE_XML::VE_CAD;
using namespace VE_Shader;

#ifdef WIN32
#include <shellapi.h>
#include <math.h>
/* Win32 doesn't seem to have these functions.
** Therefore implement inline versions of these functions here.
*/
__inline long int
lrint( double flt )
{
    int intgr;
    _asm
    {
        fld flt
        fistp intgr
    };

    return intgr ;
}

__inline long int
lrintf( float flt )
{
    int intgr;
    _asm
    {
        fld flt
        fistp intgr
    };

    return intgr ;
}
#endif

BEGIN_EVENT_TABLE( AppFrame, wxFrame )
    EVT_MENU( v21ID_ZOOMIN, AppFrame::ZoomIn )
    EVT_MENU( v21ID_ZOOMOUT, AppFrame::ZoomOut )
    EVT_MENU( wxID_SAVE, AppFrame::Save )
    EVT_MENU( wxID_SAVEAS, AppFrame::SaveAs )
    EVT_MENU( wxID_NEW, AppFrame::NewCanvas )
    //This is probably a bug and needs to be fixed
    EVT_MENU( wxID_EXIT, AppFrame::FrameClose )
    EVT_MENU( ID_PREFERENCES, AppFrame::OnPreferences )
    EVT_MENU( CLEAR_RECENT_FILES, AppFrame::OnClearRecentFiles )
    EVT_MENU( wxID_OPEN, AppFrame::Open )
    EVT_MENU( CHANGE_WORKING_DIRECTORY, AppFrame::OnChangeWorkingDirectory )
    
    EVT_MENU_RANGE(wxID_FILE1, wxID_FILE9  , AppFrame::OpenRecentFile )

    EVT_MENU( v21ID_LOAD, AppFrame::LoadFromServer )
    EVT_MENU( QUERY_FROM_SERVER, AppFrame::QueryFromServer )
    EVT_MENU( v21ID_SUBMIT, AppFrame::SubmitToServer )
    //EVT_MENU( v21ID_CONNECT, AppFrame::ConExeServer )
    EVT_MENU( v21ID_DISCONNECT, AppFrame::DisConExeServer )
    EVT_MENU( v21ID_DISCONNECT_VE, AppFrame::DisConVEServer )
    //EVT_MENU( v21ID_CONNECT_VE, AppFrame::ConVEServer )
    EVT_MENU( v21ID_START_CALC, AppFrame::StartCalc )
    EVT_MENU( v21ID_STOP_CALC, AppFrame::StopCalc )
    EVT_MENU( v21ID_PAUSE_CALC, AppFrame::PauseCalc )
    EVT_MENU( v21ID_RESUME_CALC, AppFrame::ResumeCalc )

    EVT_MENU( v21ID_HELP, AppFrame::ViewHelp )
    EVT_MENU( v21ID_ABOUT, AppFrame::ViewAbout )
    EVT_MENU( v21ID_REVISION, AppFrame::ViewRevision )
    EVT_MENU( v21ID_CONTACTS, AppFrame::ViewContacts )
    EVT_MENU( v21ID_PLATFORM, AppFrame::ViewPlatformInfo )

    EVT_MENU( v21ID_VIEW_RESULT, AppFrame::ViewResult )

    EVT_MENU( WAND, AppFrame::ChangeDevice )
    EVT_MENU( KEYBOARD_MOUSE, AppFrame::ChangeDevice )

    EVT_MENU( DEVICE_PROPERTIES, AppFrame::LaunchDeviceProperties )

    EVT_MENU( FRAME_RATE, AppFrame::DisplaySelection )
    EVT_MENU( COORDINATE_SYSTEM, AppFrame::DisplaySelection )

    EVT_MENU( FRAME_ALL, AppFrame::ViewSelection )
    EVT_MENU( FRAME_SELECTION, AppFrame::ViewSelection )
    EVT_MENU( RESET, AppFrame::ViewSelection )

    EVT_MENU( XPLORER_NAVIGATION, AppFrame::LaunchNavigationPane )
    EVT_MENU( XPLORER_VIEWPOINTS, AppFrame::LaunchViewpointsPane )
    EVT_MENU( XPLORER_SCENES, AppFrame::LaunchRecordScenes )
    EVT_MENU( XPLORER_COLOR, AppFrame::SetBackgroundColor )
    EVT_MENU( XPLORER_EXIT, AppFrame::OnExitXplorer )
    EVT_MENU( JUGGLER_STEREO, AppFrame::JugglerSettings )
    EVT_MENU( JUGGLER_MONO, AppFrame::JugglerSettings )
    EVT_MENU( QUERY_NETWORK, AppFrame::QueryNetwork )
	EVT_MENU( SAVE_SIMULATION, AppFrame::SaveSimulation )
	EVT_MENU( SAVEAS_SIMULATION, AppFrame::SaveAsSimulation )
    EVT_MENU( RUN_ASPEN_NETWORK, AppFrame::RunAspenNetwork )
    EVT_MENU( STEP_ASPEN_NETWORK, AppFrame::StepAspenNetwork )
    EVT_MENU( SHOW_ASPEN_SIMULATION, AppFrame::ShowAspenSimulation )
    EVT_MENU( HIDE_ASPEN_SIMULATION, AppFrame::HideAspenSimulation )
    EVT_MENU( CLOSE_ASPEN_SIMULATION, AppFrame::CloseAspenSimulation )
    EVT_MENU( CONDUCTOR_FIND, AppFrame::FindBlocks )
    EVT_MENU( CHANGE_XPLORER_VIEW_NETWORK, AppFrame::ChangeXplorerViewSettings )
    EVT_MENU( CHANGE_XPLORER_VIEW_CAD, AppFrame::ChangeXplorerViewSettings )
    EVT_MENU( CHANGE_XPLORER_VIEW_LOGO, AppFrame::ChangeXplorerViewSettings )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
AppFrame::AppFrame( wxWindow * parent, wxWindowID id, const wxString& title )
:
wxFrame( parent, id, title, wxDefaultPosition, wxDefaultSize ), 
m_frameNr( 0 ), 
f_financial( true ), 
f_geometry( true ), 
f_visualization( true ),
xplorerMenu( 0 ),
recordScenes( 0 ),
network( 0 ),
m_frame( 0 ),
_treeView( 0 ),
deviceProperties( 0 ),
navPane( 0 ),
viewlocPane( 0 )
{
    char** tempArray = new char*[ ::wxGetApp().argc ];
    for( size_t i = 0; i < ::wxGetApp().argc; ++i )
    {
        tempArray[ i ] = new char[ strlen( ConvertUnicode( ::wxGetApp().argv[ i ] ).c_str() ) + 1 ];
        strcpy( tempArray[ i ], ConvertUnicode( ::wxGetApp().argv[ i ] ).c_str() );
    }
    serviceList = VE_Conductor::CORBAServiceList::instance();
    serviceList->SetArgcArgv( ::wxGetApp().argc, tempArray );

    this->SetIcon( ve_icon32x32_xpm );
    //Initialize recent files menu
    m_recentVESFiles = new wxFileHistory();
    //This must go before preferences so that wxConfig is already loaded
    GetConfig();
    //This must go before CreateMenu so that we can configure the menu properly
    preferences = new UserPreferences( this, ::wxNewId(), 
                                       SYMBOL_USERPREFERENCES_TITLE, SYMBOL_USERPREFERENCES_POSITION, 
                                       SYMBOL_USERPREFERENCES_SIZE, SYMBOL_USERPREFERENCES_STYLE );
    //This must be configured before the menu is created so that the menus for
    //shutdown are configured correctly
    _displayMode = "Tablet";
    _detectDisplayAndCreate();

    CreateMenu();

    mainToolBar = new MainToolBar( this );
    this->SetToolBar( mainToolBar );
    CreateStatusBar();
    SetStatusText( _( "VE-Conductor Status" ) );

    directory = _( "" );
    fname = _( "" );

    m_recentVESFiles->UseMenu(file_menu);
    m_recentVESFiles->AddFilesToMenu(file_menu);
    
    ///Initialize VE-Open
    VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "XML", new VE_XML::XMLCreator() );
    VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "Shader", new VE_Shader::ShaderCreator() );
    VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "Model", new VE_XML::VE_Model::ModelCreator() );
    VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "CAD", new VE_XML::VE_CAD::CADCreator() );

    //Try and load network from server if one is already present
    std::string nw_str = serviceList->GetNetwork();
    network->Load( nw_str, true );

    //Process command line args to see if ves file needs to be loaded
    ProcessCommandLineArgs();

    xplorerColor.push_back( 0.0f );
    xplorerColor.push_back( 0.0f );
    xplorerColor.push_back( 0.0f );
    xplorerColor.push_back( 1.0f );
    xplorerWxColor = new wxColourData();
    xplorerWxColor->SetChooseFull(true);

    if( preferences->GetMode( "Auto Launch Nav Pane" ) )
    {
        wxCommandEvent event;
        LaunchNavigationPane( event );
    }

    if( preferences->GetMode( "Use Preferred Background Color" ) )
    {
        xplorerColor = preferences->GetBackgroundColor();
    }

    VE_XML::DataValuePairWeakPtr dataValuePair = new VE_XML::DataValuePair();
    dataValuePair->SetData(std::string("Background Color"),xplorerColor);
    VE_XML::CommandWeakPtr veCommand = new VE_XML::Command();
    veCommand->SetCommandName(std::string("CHANGE_BACKGROUND_COLOR"));
    veCommand->AddDataValuePair(dataValuePair);
    ///Set the command on the buffer first so that a strong ptr is 
    ///referencing the memory
    UserPreferencesDataBuffer::instance()->SetCommand( 
        "CHANGE_BACKGROUND_COLOR", veCommand );
    
    serviceList->SendCommandStringToXplorer( veCommand );

}
////////////////////////////////////////////////////////////////////////////////
AppFrame::~AppFrame()
{
    //Shutdown xplorer
    if ( (GetDisplayMode() == "Desktop") ||
        ( !preferences->GetMode( "Shut_Down_Xplorer_Option" ) ) )
    {
        ExitXplorer();
    }
    
    //Store settings to wxConfig to be written out
    StoreFrameSize( GetRect() );
    StoreConfig();
    StoreRecentFile();
    
    //We have to mannually destroy these to make sure that things shutdown 
    //properly with CORBA. There may be a possible way to get around this but
    //am not sure.
    network->Destroy();
    network = 0;
    serviceList->CleanUp();
    serviceList = 0;

    delete m_recentVESFiles;
    m_recentVESFiles = 0;
}
////////////////////////////////////////////////////////////////////////////////
std::string AppFrame::GetDisplayMode()
{
    return _displayMode;
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::_detectDisplay()
{
    for( int i = 1; i < wxTheApp->argc; ++i )
    {
        if( ConvertUnicode( wxTheApp->argv[i] ) == std::string( "-VESDesktop" ) )
        {
            _displayMode = std::string("Desktop");

            break;
        }
    }

    //return _displayMode;
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::_createTreeAndLogWindow( wxWindow* parent )
{
    if( GetDisplayMode() == "Tablet" )
    {
        wx_log_splitter = new Splitter( parent, -1 );
        wx_log_splitter->SetMinimumPaneSize( 40 );
        serviceList->GetMessageLog()->Create( wx_log_splitter, MYLOG, _( "" ), 
            wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY );
        wx_nw_splitter = new Splitter( wx_log_splitter, -1 );
    }
    else
    {
        serviceList->GetMessageLog()->Create( this, MYLOG, _( "" ), 
            wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY );
        wx_nw_splitter = new Splitter( parent, -1 );
    }

    wx_nw_splitter->SetMinimumPaneSize( 1 );

    av_modules = new Avail_Modules( wx_nw_splitter, Avail_Modules::TREE_CTRL, 
        wxDefaultPosition, wxDefaultSize, wxTR_HAS_BUTTONS );
    network = new Network( wx_nw_splitter, -1 );
    av_modules->SetNetwork( network );

    if( GetDisplayMode() == "Tablet" )
    {
        wx_log_splitter->SplitHorizontally( serviceList->GetMessageLog(), wx_nw_splitter, -205 );
    }

    wx_nw_splitter->SplitVertically( av_modules, network, 140 );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::_configureDesktop()
{
    SetTitle( _("VE-Suite: www.vesuite.org") );
    _treeView = new wxDialog(this, -1, _("Available Objects"), 
         wxDefaultPosition, wxDefaultSize,
         wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER);
    //wxCAPTION | wxRESIZE_BORDER);//(wxDEFAULT_DIALOG_STYLE&~ 
    //(wxCLOSE_BOX | wxRESIZE_BORDER | wxRESIZE_BOX | wxMAXIMIZE_BOX)));
    //|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX|wxCLOSE_BOX));
    wxBoxSizer* treeViewSizer = new wxBoxSizer(wxHORIZONTAL);

    _treeView->SetAutoLayout(true);
    _treeView->SetSizer(treeViewSizer);

    _createTreeAndLogWindow(_treeView);
    treeViewSizer->Add(wx_nw_splitter,1, wxALIGN_CENTER|wxEXPAND);

    int displayWidth, displayHeight = 0;
    ::wxDisplaySize(&displayWidth,&displayHeight);

    SetSize(wxSize(displayWidth,195/*displayHeight*0.0732421875*/));
    SetPosition(wxPoint(0,0));
    //--need to look into if we can use wxRegion to define our "cut-out" for the sim display
    //wxRegion desktopSize(0,0,displayWidth,displayHeight);
    //wxRegion xplorerWindownSize(%displayWidth,%displayHeight,xplorerWidth,xplorerHeight);
    //if(desktopSize.Subtract(xplorerWindowSize))
    //{
    //   wxRegion frameShape = desktopSize;
    //   SetShape(frameShape);
    //}
    //else
    //{ 
    //   SetSize(DetermineFrameSize(NULL));
    //}
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::_configureTablet()
{
   _createTreeAndLogWindow(this);
   SetSize( DetermineTabletFrameSize() );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::_detectDisplayAndCreate()
{ 
   _detectDisplay();
   if ( GetDisplayMode() == "Desktop")
   {
      _configureDesktop();
      SetWindowStyle( wxDEFAULT_FRAME_STYLE | wxRESIZE_BORDER | wxRESIZE_BOX | wxMAXIMIZE_BOX );
      //Set min size so all buttons still show and message window displays at least one line
      SetMinSize( wxSize( 700, 160) );
   }
   else if ( GetDisplayMode() == "Tablet")
   {
      _configureTablet();
      SetWindowStyle( wxDEFAULT_FRAME_STYLE | wxRESIZE_BORDER | wxRESIZE_BOX | wxMAXIMIZE_BOX );
      //set min size so all buttons still show and message window displays three lines and canvas
      SetMinSize( wxSize( 700, 260) );
   }
   else
   {
      wxMessageBox( _("Unable to create GUI."), _("Unknown display request!"), 
            wxOK | wxICON_INFORMATION );
      _exit(1);
   }
}
////////////////////////////////////////////////////////////////////////////////
bool AppFrame::Show(bool value)
{
    bool status = false;
    status = wxFrame::Show( value );

    if( _displayMode == "Desktop" )
    {
        int displayWidth, displayHeight = 0;
        ::wxDisplaySize(&displayWidth,&displayHeight);
        //std::cout<<"Width: "<<displayWidth<<" Height: "<<displayHeight<<std::endl;
        wxRect bbox = wxTheApp->GetTopWindow()->GetRect();

        wxRect dialogPosition( 2*displayWidth/3, 
                             bbox.GetBottomRight().y, 
                             displayWidth/3, 
                             (displayHeight-bbox.GetBottomRight().y)/2 
                           );
        _treeView->SetSize( dialogPosition );

        status = _treeView->Show();
    }
    return status;
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::GetConfig()
{
    wxConfig* cfg = new wxConfig(wxTheApp->GetAppName());
    //Do not let wx create a new wxConfig with Get calls
    cfg->DontCreateOnDemand();
    wxConfig::Set(cfg);

    bool exist = false;

    wxString key = FEATURE;
    if( cfg->Exists(key) ) 
    {
        cfg->Read( key + _T("/") + F_FINANCIAL, &f_financial);
        cfg->Read( key + _T("/") + F_GEOMETRY, &f_geometry);
        cfg->Read( key + _T("/") + F_VISUALIZATION, &f_visualization);
    }
    else
    {
        f_financial = true;
        f_geometry = true;
        f_visualization = true;
    }

    m_recentVESFiles->Load(*cfg);
}
////////////////////////////////////////////////////////////////////////////////
wxRect AppFrame::GetAppropriateSubDialogSize()
{
    int displayWidth= 0;
    int displayHeight = 0;

    ::wxDisplaySize(&displayWidth,&displayHeight);
    if(_displayMode == "Desktop")
    {
        wxRect bbox = GetRect();
        int xStart = lrint( 2.0f*displayWidth/3.0f );
        int width = lrint( displayWidth/3.0f );
        int height = lrint( 3.0f * (displayHeight-bbox.GetBottomRight().y)/4.0f );
        return wxRect( xStart, bbox.GetBottomRight().y, width, height );
    }
    else
    {
        int xStart = lrint( 2.0f*displayWidth/3.0f );
        int width = lrint( displayWidth/3.0f );
        int height = lrint( 3*displayHeight/4.0f );
        return wxRect( xStart, 0, width, height );
    }
}
////////////////////////////////////////////////////////////////////////////////
wxRect AppFrame::DetermineTabletFrameSize()
{
    const int minFrameWidth = 600;
    const int minFrameHight = 400;

    wxRect rect;
    //wxSize scr = wxGetDisplaySize();

    wxConfig* cfg = static_cast<wxConfig*>(wxConfig::Get());
    wxString key = LOCATION + wxString::Format(_("%d"), 0);
    if( cfg->Exists(key) ) 
    {
        cfg->Read( key + _T("/") + LOCATION_X, &rect.x);
        cfg->Read( key + _T("/") + LOCATION_Y, &rect.y);
        cfg->Read( key + _T("/") + LOCATION_W, &rect.width);
        cfg->Read( key + _T("/") + LOCATION_H, &rect.height);
    }

    ///check for reasonable values (within screen)
    /*rect.x = wxMin( abs(rect.x), (scr.x - minFrameWidth));
    rect.y = wxMin( abs(rect.y), (scr.y - minFrameHight));
    rect.width = wxMax( abs(rect.width), (minFrameWidth));
    rect.width = wxMin( abs(rect.width), (scr.x - rect.x));
    rect.height = wxMax( abs(rect.height), (minFrameHight));
    rect.height = wxMin( abs(rect.height), (scr.y - rect.y));*/

    if( !preferences->GetMode( "Save Last Position and Size" ) )
    {
        rect.x = 0;
        rect.y = 0;
        rect.width = 500;
        rect.height = 500;
    }
  
    return rect;
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::StoreFrameSize( wxRect rect )
{
    // store size
    wxConfig* cfg = static_cast<wxConfig*>(wxConfig::Get());

    wxString key = LOCATION + wxString::Format( _("%d"), 0);
    cfg->Write(key + _T("/") + LOCATION_X, rect.x);
    cfg->Write(key + _T("/") + LOCATION_Y, rect.y);
    cfg->Write(key + _T("/") + LOCATION_W, rect.width);
    cfg->Write(key + _T("/") + LOCATION_H, rect.height);
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::StoreConfig()
{
    //store config
    wxConfig* cfg = static_cast<wxConfig*>(wxConfig::Get());

    wxString key = FEATURE;
    cfg->Write(key+_T("/") + F_FINANCIAL, f_financial);
    cfg->Write(key+_T("/") + F_GEOMETRY, f_geometry);
    cfg->Write(key+_T("/") + F_VISUALIZATION, f_visualization);
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::StoreRecentFile()
{
    //store recent menus in config
    wxConfig* cfg = static_cast<wxConfig*>(wxConfig::Get());

    m_recentVESFiles->Save(*cfg);
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::FrameClose(wxCommandEvent& WXUNUSED(event) )
{
   Close(true);
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::CreateMenu() 
{
    menubar = new wxMenuBar;
    file_menu = new wxMenu;
    con_menu = new wxMenu;
    run_menu = new wxMenu;
    edit_menu = new wxMenu;
    help_menu = new wxMenu;

    file_menu->Append( wxID_NEW, _( "&New\tCtrl+N" ) );
    file_menu->Append( wxID_OPEN, _( "&Open ..\tCtrl+O" ) );

    file_menu->AppendSeparator();

    file_menu->Append( wxID_SAVE, _( "&Save\tCtrl+S" ) );
    file_menu->Append( wxID_SAVEAS, _( "Save &as ..\tCtrl+Shift+S" ) );
    file_menu->AppendSeparator();
    file_menu->Append( EXPORT_MENU_OPT, _( "Export" ), 
        new ExportMenu(), _("Options") );
    file_menu->AppendSeparator();
    file_menu->Append( CHANGE_WORKING_DIRECTORY, _( "Change Working Directory" ) );
    file_menu->AppendSeparator();
    /*file_menu->Append( wxID_PRINT_SETUP, _( "Print Set&up .." ) );
    file_menu->Append( wxID_PREVIEW, _( "Print Pre&view\tCtrl+Shift+P" ) );
    file_menu->Append( wxID_PRINT, _( "&Print ..\tCtrl+P" ) );
    file_menu->Enable( wxID_PRINT_SETUP, false );
    file_menu->Enable( wxID_PREVIEW, false );	
    file_menu->Enable( wxID_PRINT, false );
    file_menu->AppendSeparator();*/
    file_menu->Append( ID_PREFERENCES, _( "Preferences" ) );
    file_menu->AppendSeparator();
    file_menu->Append( CLEAR_RECENT_FILES, _( "&Clear File History" ) );

    file_menu->AppendSeparator();
    file_menu->Append( wxID_EXIT, _( "&Quit\tCtrl+Q" ) );

    //con_menu->Append(v21ID_CONNECT, _("&Connect to Executive\tCtrl+C"));
    //con_menu->Append(v21ID_CONNECT_VE, _("Connect to VE"));
    //con_menu->AppendSeparator();
    con_menu->Append( v21ID_SUBMIT, _( "Sub&mit Job\tCtrl+M" ) );
    con_menu->Append( v21ID_LOAD, _( "&Load Job\tCtrl+L" ) );

    //con_menu->Append(QUERY_FROM_SERVER, _("&Query\tCtrl+U"));
    wxMenu * aspenMenu = new wxMenu();
    aspenMenu->Append( QUERY_NETWORK, _( "Open Simulation" ) );
    aspenMenu->Append( SHOW_ASPEN_SIMULATION, _( "Show Simulation" ) );
    aspenMenu->Append( HIDE_ASPEN_SIMULATION, _( "Hide Simulation" ) );
    aspenMenu->Append( CLOSE_ASPEN_SIMULATION, _( "Close Simulation" ) );
    aspenMenu->Append( RUN_ASPEN_NETWORK, _( "Run" ) );
    aspenMenu->Append( STEP_ASPEN_NETWORK, _( "Step" ) );
    aspenMenu->Append( CONDUCTOR_FIND, _( "Find" ) );
    aspenMenu->Append( SAVE_SIMULATION, _("Save Simulation") );
    aspenMenu->Append( SAVEAS_SIMULATION, _("SaveAs Simulation") );
    con_menu->Append( ASPEN_CONNECTION_MENU,   _( "Aspen" ), aspenMenu, _("Aspen connection") );

    //file_menu->Append( OPEN_RECENT_CONNECTION_MENU, _("Open recent file"), aspenMenu, _("NOTHING") );


    con_menu->AppendSeparator();
    con_menu->Append( v21ID_DISCONNECT, _( "&Disconnect\tCtrl+d" ) );
    con_menu->Append( v21ID_DISCONNECT_VE, _( "&Disconnect VE" ) );

    //con_menu->Enable( v21ID_SUBMIT,false );
    //con_menu->Enable( v21ID_LOAD, false );
    con_menu->Enable( v21ID_DISCONNECT, false );
    con_menu->Enable( v21ID_DISCONNECT_VE, false );


    run_menu->Append( v21ID_START_CALC, _( "Start Simulation" ) );
    run_menu->Append( v21ID_STOP_CALC, _( "Stop Simulation" ) );
    run_menu->Append( v21ID_PAUSE_CALC, _( "Pause Simulation" ) );
    run_menu->Append( v21ID_RESUME_CALC, _( "Resume Simulation" ) );
    run_menu->Append( v21ID_VIEW_RESULT, _( "View Results" ) );
    // run_menu->Append( v21ID_GLOBAL_PARAM, _("Global Parameters"));
    // run_menu->Append( v21ID_VIEW_FINANCIAL, _("View Financial Params"));

    run_menu->Enable(v21ID_START_CALC, false);
    run_menu->Enable(v21ID_STOP_CALC, false);
    run_menu->Enable(v21ID_PAUSE_CALC, false);
    run_menu->Enable(v21ID_RESUME_CALC, false);
    // EPRI TAG run_menu->Enable(v21ID_VIEW_RESULT, false);

    //edit_menu->Append(v21ID_UNDO, _("&Undo\tCtrl+U"));
    //edit_menu->Append(v21ID_REDO, _("&Redo\tCtrl+R"));
    //edit_menu->AppendSeparator();
    edit_menu->Append(v21ID_ZOOMIN, _("Zoom &In\tCtrl+UP"));
    edit_menu->Append(v21ID_ZOOMOUT, _("Zoom &Out\tCtrl+DOWN"));
    //This is needed because on windows the scale must be 1 for the
    //wxAutoBufferedPaintDC to work properly
#ifdef WIN32
    edit_menu->Enable(v21ID_ZOOMIN, false);
    edit_menu->Enable(v21ID_ZOOMOUT, false);
#endif
    //edit_menu->Enable(v21ID_UNDO, false);
    //edit_menu->Enable(v21ID_REDO, false);

    //help_menu->Append(wxID_HELP_CONTENTS, _("&Content\tF1"));
    help_menu->Append (v21ID_HELP, _("&VE-Suite Help"));
    help_menu->Append(v21ID_ABOUT, _("&About"));
    help_menu->Append(v21ID_REVISION, _("&Revision"));
    help_menu->Append(v21ID_CONTACTS, _("&Contacts"));
    help_menu->Append(v21ID_PLATFORM, _("&Platform Info"));
    //help_menu->AppendSeparator();
    //help_menu->Append (wxID_ABOUT, _("&About ..\tShift+F1"));

    //help_menu->Enable(wxID_HELP_CONTENTS, false);
    //help_menu->Enable(v21ID_HELP, false);
    //help_menu->Enable(wxID_ABOUT, true);

    xplorerMenu = new wxMenu();
    xplorerDeviceMenu = new wxMenu();

    xplorerJugglerMenu = new wxMenu();
    xplorerDisplayMenu = new wxMenu();
    xplorerViewMenu = new wxMenu();
    wxMenu* xplorerView = new wxMenu();

    xplorerDeviceMenu->Append( WAND, _( "Wand" ) );
    xplorerDeviceMenu->Append( KEYBOARD_MOUSE,  _( "Keyboard Mouse" ) );
    xplorerDeviceMenu->AppendSeparator();
    xplorerDeviceMenu->Append( DEVICE_PROPERTIES,    _( "Properties" ) );

    xplorerDisplayMenu->AppendCheckItem( FRAME_RATE,        _( "Frame Rate" ) );
    xplorerDisplayMenu->AppendCheckItem( COORDINATE_SYSTEM, _( "Coord System" ) );

    xplorerViewMenu->Append( FRAME_ALL,       _( "Frame All            [f]" ) );
    xplorerViewMenu->Append( FRAME_SELECTION, _( "Frame Selection" ) );
    xplorerViewMenu->Append( RESET,           _( "Reset                  [r]" ) );

    xplorerView->Append( CHANGE_XPLORER_VIEW_NETWORK, _("Network") );
    xplorerView->Append( CHANGE_XPLORER_VIEW_CAD, _("CAD") );
    xplorerView->Append( CHANGE_XPLORER_VIEW_LOGO, _("Logo") );

    xplorerJugglerMenu->Append( JUGGLER_STEREO, _("Stereo") );
    xplorerJugglerMenu->Append( JUGGLER_MONO, _("Mono") );
    xplorerJugglerMenu->Enable( JUGGLER_STEREO, true);
    xplorerJugglerMenu->Enable( JUGGLER_MONO, true);

    xplorerMenu->Append( XPLORER_NAVIGATION, _("Navigation Pane") );
    xplorerMenu->Append( XPLORER_VIEWPOINTS, _("Viewpoints Pane") );
    xplorerMenu->Append( XPLORER_SCENES,     _("Record Scenes") );
    xplorerMenu->Append( XPLORER_COLOR,      _("Background Color") );
    //xplorerMenu->Append( XPLORER_SOUNDS,     _("Sounds Pane") );
    //xplorerMenu->Append( XPLORER_STREAMLINE, _("Streamline Pane") );
    xplorerMenu->Append( XPLORER_DEVICE,     _("Devices"),            
        xplorerDeviceMenu,  _("Used to change device properties") );
    xplorerMenu->Append( JUGGLER_SETTINGS,   _("Juggler Settings"),   
        xplorerJugglerMenu, _("Used to adjust juggler runtime settings") );
    xplorerMenu->Append( XPLORER_DISPLAY,    _("Display"),           
        xplorerDisplayMenu, _("Used to change display preferences") );
    xplorerMenu->Append( XPLORER_VIEW,       _("View"),               
        xplorerViewMenu,    _("Used to change the view") );
    //add the view settings
    xplorerMenu->Append( CHANGE_XPLORER_VIEW, _("Graphical View"),    
        xplorerView,        _("Used to change the view in xplorer") );
    //If the display mode is desktop then we will disconnect when exit is selected
    //and in other modes we will give the user the ability to exit
    if( ( GetDisplayMode() != "Desktop" ) &&
        ( preferences->GetMode( "Shut Down Xplorer Option" ) ) )
    {
        xplorerMenu->Append( XPLORER_EXIT, _("Shutdown Xplorer") );
        xplorerMenu->Enable( XPLORER_EXIT, true);
    }

    xplorerMenu->Enable( XPLORER_NAVIGATION, true);
    xplorerMenu->Enable( XPLORER_VIEWPOINTS, true);
    //xplorerMenu->Enable( XPLORER_SOUNDS, true);
    xplorerMenu->Enable( XPLORER_SCENES, true);
    xplorerMenu->Enable( JUGGLER_SETTINGS, true);

    menubar->Append(file_menu, _("&File"));
    menubar->Append(edit_menu, _("&Edit"));
    menubar->Append(con_menu, _("&Connection"));
    menubar->Append(run_menu, _("&Execution"));
    menubar->Append( xplorerMenu, _("&VE-Xplorer") );
    menubar->Append(help_menu, _("&Help"));

    SetMenuBar( menubar );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ZoomIn( wxCommandEvent& WXUNUSED(event) )
{
    if( network->GetUserScale()->first > 4 )
    {    
        return; // maximum zoom in x3
    }

    network->GetUserScale()->first += 0.1;
    network->GetUserScale()->second += 0.1; 
    network->GetNumPix()->first += 1;
    network->GetNumPix()->second += 1;
    
    int xpos, ypos;
    network->GetViewStart( &xpos, &ypos );
    network->SetScrollbars( 
        network->GetNumPix()->first, network->GetNumPix()->second, 
        network->GetNumUnit()->first, network->GetNumUnit()->second,
        xpos, ypos );
    network->Refresh(true);
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ZoomOut( wxCommandEvent& WXUNUSED(event) )
{
    if( network->GetUserScale()->first < 0.2 )
    {   
        return; //minimum x-5
    }

    network->GetUserScale()->first -= 0.1;
    network->GetUserScale()->second -= 0.1;
    network->GetNumPix()->first -= 1;
    network->GetNumPix()->second -= 1;

    int xpos, ypos;
    network->GetViewStart(&xpos, &ypos);
    network->SetScrollbars( 
        network->GetNumPix()->first, network->GetNumPix()->second, 
        network->GetNumUnit()->first, network->GetNumUnit()->second,
        xpos, ypos );
    network->Refresh(true);
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::Save( wxCommandEvent& event )
{
   //First time call save will be the same as SaveAs
   if ( fname == wxString( "", wxConvUTF8) ) 
   {
      SaveAs( event );
   }
   else
   {
      ///now write the file out from domdocument manager
      //wrtie to path
      std::string data = network->Save( ConvertUnicode( fname.c_str() ) );
   }
}

////////////////////////////////////////////////////////////////////////////////
void AppFrame::SaveAs( wxCommandEvent& WXUNUSED(event) )
{
   wxFileName vesFileName;
   int answer = 0;
   do
   {
      wxTextEntryDialog newDataSetName(this, 
                                       _("Enter the prefix for *.ves filename:"),
                                       _("Save VES file as..."),
                                       _("network"),wxOK|wxCANCEL);
      
      if ( newDataSetName.ShowModal() == wxID_OK )
      {
         vesFileName.ClearExt();
         vesFileName.SetName( newDataSetName.GetValue() ); 
         vesFileName.SetExt( _("ves") );
      }
      else
      {
         break;
      }
      
      if ( vesFileName.FileExists() )
      {
         wxString tempMessage = _("Do you want to replace ") + vesFileName.GetFullName() + _("?");
         wxMessageDialog promptDlg( this, 
                                    tempMessage, 
                                    _("Overwrite File Warning"), 
                                    wxYES_NO|wxNO_DEFAULT|wxICON_QUESTION, 
                                    wxDefaultPosition);
         answer = promptDlg.ShowModal();
      }
   }
   while ( answer == wxID_NO );

   if( vesFileName.FileExists() && !wxFileName::IsFileWritable( vesFileName.GetFullPath() ) )
   {
       wxString tempMessage = _("Cannot write file ") + vesFileName.GetFullName() + _("?");
       wxMessageDialog promptDlg( this, 
                                  tempMessage, 
                                  _("Overwrite File Warning"), 
                                  wxYES_NO|wxNO_DEFAULT|wxICON_QUESTION, 
                                  wxDefaultPosition);
       promptDlg.ShowModal();
       return;
   }
   
   if( vesFileName.HasName() ) 
   {
      directory	= vesFileName.GetPath();
      fname = vesFileName.GetFullName();
      ///now write the file out from domdocument manager
      //wrtie to path
      std::string data = network->Save( ConvertUnicode( fname.c_str() ) );
   }
}

////////////////////////////////////////////////////////////////////////////////
void AppFrame::Open(wxCommandEvent& WXUNUSED(event))
{
    wxFileDialog dialog
              (
                 this,
                 _T("Open File dialog"),
                 ::wxGetCwd(),
                 fname,
                 _T("Network files (*.ves)|*.ves"),
                 wxOPEN|wxFILE_MUST_EXIST
              );

    if( dialog.ShowModal() != wxID_OK )
    {
        return;
    }

    wxFileName vesFileName( dialog.GetPath() );
    bool success = vesFileName.MakeRelativeTo( ::wxGetCwd() );   
    if( !success )
    {
        wxMessageBox( _("Can't open a VES file on another drive."), 
                      _("VES File Read Error"), wxOK | wxICON_INFORMATION );
        return;
    }

    SetTitle(vesFileName.GetFullName());

    directory = vesFileName.GetPath( wxPATH_GET_VOLUME, wxPATH_UNIX);
    //change conductor working dir
    ::wxSetWorkingDirectory( directory );
    directory.Replace( _("\\"), _("/"), true );
    std::string tempDir = ConvertUnicode( directory.c_str() );

    SetRecentFile( wxFileName(dialog.GetPath()) );

    if( tempDir.empty() )
    {
        tempDir = "./";
    }
    //Send Command to change xplorer working dir
    // Create the command and data value pairs
    VE_XML::DataValuePairWeakPtr dataValuePair = 
    new VE_XML::DataValuePair(  std::string("STRING") );
    dataValuePair->SetData( "WORKING_DIRECTORY", tempDir );
    VE_XML::CommandWeakPtr veCommand = new VE_XML::Command();
    veCommand->SetCommandName( std::string("Change Working Directory") );
    veCommand->AddDataValuePair( dataValuePair );
    serviceList->SendCommandStringToXplorer( veCommand );

    //Clear the viewpoints data
    //Since the data is "managed" by Xplorer we need to notify 
    //Xplorer when we load a new ves file to clear viewpoints since
    //They don't go with the new data.

    //Dummy data that isn't used but I don't know if a command will work
    //w/o a DVP 
    VE_XML::DataValuePairWeakPtr dvp = 
        new VE_XML::DataValuePair(  std::string("STRING") );
    dvp->SetData( "Clear Quat Data", tempDir );
    VE_XML::CommandWeakPtr vec = new VE_XML::Command();
    vec->SetCommandName( std::string("QC_CLEAR_QUAT_DATA") );
    vec->AddDataValuePair( dvp );
    serviceList->SendCommandStringToXplorer( vec );

    //Reloading plugins
    av_modules->ResetPluginTree();

    //Now laod the xml data now that we are in the correct directory
    fname=dialog.GetFilename();
    network->Load( ConvertUnicode( fname.c_str() ), true );
    wxCommandEvent event;
    SubmitToServer( event );
    if( recordScenes )
    {
        recordScenes->_buildPage();
    }

    ///This code will be moved in the future. It is Aspen specific code.
    VE_XML::CommandWeakPtr aspenBKPFile = 
        UserPreferencesDataBuffer::instance()->
        GetCommand( "Aspen_Plus_Preferences" );
    if( aspenBKPFile->GetCommandName() != "NULL" )
    {
		VE_XML::DataValuePairStrongPtr bkpPtr = 
            aspenBKPFile->GetDataValuePair( "BKPFileName" );
        std::string bkpFilename;
        bkpPtr->GetData( bkpFilename );
        OpenSimulation( wxString( bkpFilename.c_str(), wxConvUTF8) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::SetRecentFile(wxFileName vesFileName)
{
    size_t numFilesInHistory = m_recentVESFiles->GetCount();
    for( size_t i = 0; i < numFilesInHistory; ++i )
    { 
        if ( !m_recentVESFiles->GetHistoryFile(i).Cmp(vesFileName.GetFullPath()))
        {
            return;
        }
    }
    m_recentVESFiles->AddFileToHistory(vesFileName.GetFullPath());
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OpenRecentFile( wxCommandEvent& event ) 
{
    wxString fileToOpen(m_recentVESFiles->GetHistoryFile(event.GetId() - wxID_FILE1));
    if( fileToOpen.empty() || (!wxFileName::FileExists( fileToOpen ) ) )
    {
        wxString message( _("VES file "));
        message += fileToOpen;
        message += wxString( _(" does not exist!") );
        wxMessageBox( message, 
                      _("VES File Read Error"), wxOK | wxICON_INFORMATION );
        m_recentVESFiles->RemoveFileFromHistory( event.GetId() - wxID_FILE1 );
        return;
    }
    
    int placeChosen = event.GetId();
    wxFileName vesFileName(fileToOpen);
    bool success = vesFileName.MakeRelativeTo( ::wxGetCwd() );   
    if( !success )
    {
        wxMessageBox( _("Can't open a VES file on another drive."), 
                _("VES File Read Error"), wxOK | wxICON_INFORMATION );
        return;
    }
    
    SetTitle(vesFileName.GetFullName());
        
    directory = vesFileName.GetPath( wxPATH_GET_VOLUME, wxPATH_UNIX);
    //change conductor working dir
    ::wxSetWorkingDirectory( directory );
    directory.Replace( _("\\"), _("/"), true );

    // TODO also, make call if file they are trying to call does not exist, call DeleteRecentFile
    fname = vesFileName.GetFullName();

    std::string tempDir = ConvertUnicode( directory.c_str() );
    if( tempDir.empty() )
    {
        tempDir = "./";
    }

    //Send Command to change xplorer working dir
    // Create the command and data value pairs
    VE_XML::DataValuePairWeakPtr dataValuePair = 
                  new VE_XML::DataValuePair(  std::string("STRING") );
    dataValuePair->SetData( "WORKING_DIRECTORY", tempDir );
    VE_XML::CommandWeakPtr veCommand = new VE_XML::Command();
    veCommand->SetCommandName( std::string("Change Working Directory") );
    veCommand->AddDataValuePair( dataValuePair );
    serviceList->SendCommandStringToXplorer( veCommand );

    //Dummy data that isn't used but I don't know if a command will work
    //w/o a DVP 
    VE_XML::DataValuePairWeakPtr dvp = 
                  new VE_XML::DataValuePair(  std::string("STRING") );
    dvp->SetData( "Clear Quat Data", tempDir );
    VE_XML::CommandWeakPtr vec = new VE_XML::Command();
    vec->SetCommandName( std::string("QC_CLEAR_QUAT_DATA") );
    vec->AddDataValuePair( dvp );
    serviceList->SendCommandStringToXplorer( vec );

    //Reloading plugins
    av_modules->ResetPluginTree();

    //Now laod the xml data now that we are in the correct directory
    network->Load( ConvertUnicode( fname.c_str() ), true );
    SubmitToServer( event );
    //Rebuild the teacher tab so that the new stored files are loaded
    if( recordScenes )
    {
        recordScenes->_buildPage();
    }
	
	///This code will be moved in the future. It is Aspen specific code.
    VE_XML::CommandWeakPtr aspenBKPFile = 
        UserPreferencesDataBuffer::instance()->
        GetCommand( "Aspen_Plus_Preferences" );
    if( aspenBKPFile->GetCommandName() != "NULL" )
    {
		VE_XML::DataValuePairStrongPtr bkpPtr = 
            aspenBKPFile->GetDataValuePair( "BKPFileName" );
        std::string bkpFilename;
        bkpPtr->GetData( bkpFilename );
        OpenSimulation( wxString( bkpFilename.c_str(), wxConvUTF8) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OnClearRecentFiles( wxCommandEvent& event ) 
{
    wxMessageDialog confirm(this,
                            _("Are you sure you want to clear the recent files list?"),
                           _("Confirm"),wxOK|wxCANCEL); 
    if(confirm.ShowModal() == wxID_OK)
    {
        size_t numFilesInHistory = m_recentVESFiles->GetCount();
        for( size_t i = 0; i < numFilesInHistory; ++i )
        {
            m_recentVESFiles->RemoveFileFromHistory( ( numFilesInHistory - 1) - i);
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::LoadFromServer( wxCommandEvent& WXUNUSED(event) )
{
   std::string nw_str = serviceList->GetNetwork();
   EnableCEGUIMenuItems();
   network->Load( nw_str, false );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::QueryFromServer( wxCommandEvent& WXUNUSED(event) )
{
   EnableCEGUIMenuItems();

   std::string nw_str;

   try
   {
	   nw_str.assign( serviceList->Query( 0 ));
   }
   catch ( CORBA::Exception& )
   {
      Log("No ves network available\n");
   }

   // If there is nothing on the CE
   if ( !nw_str.empty() )
   {
       network->Load( nw_str, true );
       ///Submit job to xplorer
       // Tell xplorer to ask ce for the new data
       VE_XML::DataValuePairWeakPtr dataValuePair = new VE_XML::DataValuePair();
       dataValuePair->SetData(std::string("Load Data"),xplorerColor);
       VE_XML::CommandWeakPtr veCommand = new VE_XML::Command();
       veCommand->SetCommandName(std::string("veNetwork Update"));
       veCommand->AddDataValuePair(dataValuePair);
       serviceList->SendCommandStringToXplorer( veCommand );
   }
   else
   {
      Log("No ves network available\n");
   }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::QueryNetwork( wxCommandEvent& WXUNUSED(event) )
{
    Log("Opening Simulation...\n");
    wxFileName bkpFileName;
    wxTextEntryDialog newDataSetName(this, 
        wxString("Enter the prefix for *.bkp filename:", wxConvUTF8),
        wxString("Open BKP Filename", wxConvUTF8),
        wxString("", wxConvUTF8),wxOK|wxCANCEL);
        
    if( newDataSetName.ShowModal() != wxID_OK )
    {
        return;
    }

    bkpFileName.ClearExt();
    bkpFileName.SetName( newDataSetName.GetValue() ); 
    //bkpFileName.SetExt( wxString( "bkp", wxConvUTF8 ) );

    VE_XML::Command returnState;
    returnState.SetCommandName("getNetwork");
    VE_XML::DataValuePairWeakPtr data = new VE_XML::DataValuePair();
    data->SetData("NetworkQuery", "getNetwork" );
    returnState.AddDataValuePair( data );
    
    data = new VE_XML::DataValuePair();
    data->SetData("BKPFileName",  ConvertUnicode( bkpFileName.GetFullName().c_str() ) );
    returnState.AddDataValuePair( data );

    std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
    nodes.push_back(std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ));
    VE_XML::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    //Get results
    std::string nw_str = serviceList->Query( status );

    // If there is nothing on the CE
    if( nw_str.empty() )
    {
        Log("No ves network available\n");
        return;
    }

    if( network->modules.empty() )
    { 
        network->Load( nw_str, true );
        Log("Simulation Opened.\n");
        ///
        VE_XML::CommandWeakPtr aspenBKPFile = new VE_XML::Command();
        aspenBKPFile->SetCommandName( "Aspen_Plus_Preferences" );
        data = new VE_XML::DataValuePair();
        data->SetData( "BKPFileName",  
            ConvertUnicode( bkpFileName.GetFullName().c_str() ) );
        aspenBKPFile->AddDataValuePair( data );
        UserPreferencesDataBuffer::instance()->
            SetCommand( "Aspen_Plus_Preferences", aspenBKPFile );
        ///Submit job to xplorer
        // Tell xplorer to ask ce for the new data
        VE_XML::DataValuePairWeakPtr dataValuePair = new VE_XML::DataValuePair();
        dataValuePair->SetData(std::string("Load Data"),xplorerColor);
        VE_XML::CommandWeakPtr veCommand = new VE_XML::Command();
        veCommand->SetCommandName(std::string("veNetwork Update"));
        veCommand->AddDataValuePair(dataValuePair);
        serviceList->SendCommandStringToXplorer( veCommand );
    }
    else
    {    
        Log("Simulation is already open.\n");
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OpenSimulation( wxString simName )
{
    wxFileName bkpFileName;
    bkpFileName.SetName( simName); 

    VE_XML::Command returnState;
    returnState.SetCommandName("openSimulation");
    VE_XML::DataValuePairWeakPtr data = new VE_XML::DataValuePair();
    data->SetData("AspenPlus", "openSimulation" );
    returnState.AddDataValuePair( data );
    
    data = new VE_XML::DataValuePair();
    data->SetData("BKPFileName",  ConvertUnicode( bkpFileName.GetFullName().c_str() ) );
    returnState.AddDataValuePair( data );

    std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
    nodes.push_back(std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ));
    VE_XML::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    //Get results
    std::string nw_str = serviceList->Query( status );
    Log(nw_str.c_str());
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ShowAspenSimulation( wxCommandEvent& WXUNUSED(event) )
{
    Log("Show Simulation.\n");
    VE_XML::Command returnState;
    returnState.SetCommandName("showSimulation");
    VE_XML::DataValuePairWeakPtr data = new VE_XML::DataValuePair();
    data->SetData("NetworkQuery", "showSimulation" );
    returnState.AddDataValuePair( data );

    std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
    nodes.push_back(std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ));

    VE_XML::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    std::string nw_str = serviceList->Query( status ) + "\n";
    Log(nw_str.c_str());
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::HideAspenSimulation( wxCommandEvent& WXUNUSED(event) )
{
    Log("Hide Simulation.\n");
    VE_XML::Command returnState;
    returnState.SetCommandName("hideSimulation");
    VE_XML::DataValuePairWeakPtr data = new VE_XML::DataValuePair();
    data->SetData("NetworkQuery", "hideSimulation" );
    returnState.AddDataValuePair( data );

    std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
    nodes.push_back(std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ));

    VE_XML::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    std::string nw_str = serviceList->Query( status ) + "\n";
    Log(nw_str.c_str());
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::CloseAspenSimulation( wxCommandEvent& WXUNUSED(event) )
{
    Log("Close Simulation.\n");
    VE_XML::Command returnState;
    returnState.SetCommandName("closeSimulation");
    VE_XML::DataValuePairWeakPtr data = new VE_XML::DataValuePair();
    data->SetData("NetworkQuery", "closeSimulation" );
    returnState.AddDataValuePair( data );

    std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
    nodes.push_back(std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ));

    VE_XML::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    std::string nw_str = serviceList->Query( status ) + "\n";
    Log(nw_str.c_str());
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::RunAspenNetwork( wxCommandEvent& WXUNUSED(event) )
{
    Log("Run Simulation.\n");
    VE_XML::Command returnState;
    returnState.SetCommandName("runNetwork");
    VE_XML::DataValuePairWeakPtr data = new VE_XML::DataValuePair();
    data->SetData("NetworkQuery", "runNetwork" );
    returnState.AddDataValuePair( data );

    std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
    nodes.push_back(std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ));

    VE_XML::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    serviceList->Query( status );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::StepAspenNetwork( wxCommandEvent& WXUNUSED(event) )
{
    Log("Run Simulation.\n");
    VE_XML::Command returnState;
    returnState.SetCommandName("stepNetwork");
    VE_XML::DataValuePairWeakPtr data = new VE_XML::DataValuePair();
    data->SetData("NetworkQuery", "runNetwork" );
    returnState.AddDataValuePair( data );

    std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
    nodes.push_back(std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ));

    VE_XML::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    serviceList->Query( status );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::FindBlocks( wxCommandEvent& WXUNUSED(event) )
{
    Log("Find Block.\n");
    FindDialog * fd = new FindDialog(this);
    std::map<int, Module>::iterator iter;
    std::vector< std::string > moduleNames;
    std::vector< unsigned int > moduleIDs;
    for (iter=network->modules.begin(); iter!=network->modules.end(); iter++)
    {
        moduleNames.push_back(network->modules[iter->first].GetClassName());
        moduleIDs.push_back(network->modules[iter->first].GetPlugin()->GetModel()->GetModelID());
    }

    fd->SetModuleList(moduleNames);
    fd->ShowModal();

    int selectedModulePos = fd->GetSelectedModulePos();

    //recenter the flowsheet around the icon
    int xPix, yPix;
    network->GetScrollPixelsPerUnit(&xPix, &yPix);
    network->Scroll(network->modules[moduleIDs[selectedModulePos]].GetPlugin()->GetBBox().GetX()/(xPix),
    network->modules[moduleIDs[selectedModulePos]].GetPlugin()->GetBBox().GetY()/(yPix));

    //highlight the selected icon
    network->SetSelectedModule(moduleIDs[selectedModulePos]);
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::SaveSimulation(wxCommandEvent& WXUNUSED(event))
{
    Log("Saving Simulation...\n");
    VE_XML::Command returnState;
    returnState.SetCommandName("saveSimulation");
    VE_XML::DataValuePairWeakPtr data = new VE_XML::DataValuePair();
    data->SetData("NetworkQuery", "saveSimulation" );
    returnState.AddDataValuePair( data );

    std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
    nodes.push_back(std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ));

    VE_XML::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    serviceList->Query( status );
    //std::string nw_str = serviceList->Query( status ) + "\n";
    //Log(nw_str.c_str());
    Log("Simulation Saved.\n");
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::SaveAsSimulation( wxCommandEvent& WXUNUSED(event) )
{
   wxFileName saveFileName;
   wxTextEntryDialog newDataSetName(this, 
	   wxString("Enter filename (.apw):", wxConvUTF8),
	   wxString("Save Flowsheet", wxConvUTF8),
	   wxString("", wxConvUTF8),wxOK|wxCANCEL);

    if( newDataSetName.ShowModal() != wxID_OK )
    {
        return;
    }

    Log("Saving Simulation...\n");
    saveFileName.ClearExt();
    saveFileName.SetName( newDataSetName.GetValue() ); 
    //bkpFileName.SetExt( wxString( "bkp", wxConvUTF8 ) );

    VE_XML::Command returnState;
    returnState.SetCommandName("saveAsSimulation");
    VE_XML::DataValuePairWeakPtr data = new VE_XML::DataValuePair();
    data->SetData("NetworkQuery", "saveAsSimulation" );
    returnState.AddDataValuePair( data );
    
    data = new VE_XML::DataValuePair();
    data->SetData("SaveFileName",  
        ConvertUnicode( saveFileName.GetFullName().c_str() ) );
    returnState.AddDataValuePair( data );

    std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
    nodes.push_back(std::pair< VE_XML::XMLObject*, 
        std::string >( &returnState, "vecommand" ));
    VE_XML::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    //Get results
    std::string nw_str = serviceList->Query( status );
    Log("Simulation Saved.\n");

    VE_XML::CommandWeakPtr aspenAPWFile = new VE_XML::Command();
    aspenAPWFile->SetCommandName( "Aspen_Plus_Preferences" );
    data = new VE_XML::DataValuePair();
    data->SetData( "BKPFileName",  
        ConvertUnicode( saveFileName.GetFullName().c_str() ) );
    aspenAPWFile->AddDataValuePair( data );
    UserPreferencesDataBuffer::instance()->
        SetCommand( "Aspen_Plus_Preferences", aspenAPWFile );
}
///////////////////////////////////////////////////////////////////////////
void AppFrame::NewCanvas( wxCommandEvent& WXUNUSED(event) )
{
   SetTitle( _("VE-Suite: www.vesuite.org") );
   network->New( true );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::SubmitToServer( wxCommandEvent& WXUNUSED(event) )
{
   EnableCEGUIMenuItems();
   
   std::string nw_str = network->Save( std::string( "returnString" ) );
   // write the domdoc to the string above
   try 
   {
      //first make sure all the units have been initialized with the current
      //ids to get an active xml model
      network->SetIDOnAllActiveModules();
      //Now that we have an active xml model in all units
      // set the network
      serviceList->SetNetwork( CORBA::string_dup( nw_str.c_str() ) );
      // Tell xplorer to ask ce for the new data
      VE_XML::DataValuePairWeakPtr dataValuePair = new VE_XML::DataValuePair();
      dataValuePair->SetData(std::string("Load Data"),xplorerColor);
      VE_XML::CommandWeakPtr veCommand = new VE_XML::Command();
      veCommand->SetCommandName(std::string("veNetwork Update"));
      veCommand->AddDataValuePair(dataValuePair);
      serviceList->SendCommandStringToXplorer( veCommand );
      //enable the menus now
      run_menu->Enable( v21ID_START_CALC, true );
   }
   catch ( CORBA::Exception& ) 
   {
      Log("no exec found!\n");
   }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::StartCalc( wxCommandEvent& WXUNUSED(event) )
{
   try	
   { 
      serviceList->StartCalc();
   }
   catch ( CORBA::Exception& ) 
   {
      Log("no exec found!\n");
   }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::StopCalc(wxCommandEvent& WXUNUSED(event) )
{
   try
   {
      serviceList->StopCalc();
   }
   catch ( CORBA::Exception& )
   {
      Log("no exec found!\n");
   }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::PauseCalc(wxCommandEvent& WXUNUSED(event) )
{
   try
   {
      serviceList->PauseCalc();
   }
   catch ( CORBA::Exception& )
   {
      Log("no exec found!\n");
   }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ResumeCalc(wxCommandEvent& WXUNUSED(event) )
{
   try
   { 
      serviceList->Resume();
   }
   catch ( CORBA::Exception& )
   {
      Log("no exec found!\n");
   }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ViewResult(wxCommandEvent& WXUNUSED(event) )
{
   serviceList->IsConnectedToCE();
   EnableCEGUIMenuItems();

   char* result;
   //char buf[80];
   std::map<int, Module>::iterator iter;
   unsigned int i;
   std::vector<wxString> titles;
   //TextResultDialog * result_dlg;
   SummaryResultDialog * result_dlg = 0;
   std::vector<wxString> v_desc, v_value;
   std::vector<std::string> descs;
   std::vector<int> alignments;

   titles.push_back( _("Description") );
   alignments.push_back (wxALIGN_LEFT);
   titles.push_back( _("Value") );
   alignments.push_back (wxALIGN_RIGHT);

   /*
   result_dlg = new TextResultDialog(NULL);
   result_dlg->syngas->Clear();
   result_dlg->syngas->AddRow(titles);
   */
   if ( result_dlg )
   {
      result_dlg->Destroy();
      result_dlg = 0;
   }
   result_dlg = new SummaryResultDialog(NULL, wxT("Result Summary - All Modules"), wxSize(560, 400));
   result_dlg->syngas->Clear();
   result_dlg->syngas->SetNumofCols( 2 );
   result_dlg->syngas->SetColTitles( titles );
   result_dlg->syngas->SetColAlignments( alignments );

   //if (!CORBA::is_nil( network->exec.in() )) 
   {
      try 
      {
         for (iter=network->modules.begin(); iter!=network->modules.end(); iter++) 
         {
	         //result = network->exec->GetModuleResult(iter->first);
	
	         if ( std::string(result) != "" ) 
            {
              /* Package p;
               p.SetSysId("linkresult.xml");
               p.Load(result, strlen(result));

               descs = p.GetInterfaceVector()[0].getStrings();
               
               // This may not be needed.
               // this should be taken care of with previous statement
               if ( descs.size() < 1 ) 
                  continue;
               
               v_desc.clear();
               v_value.clear();
               wxString str;
               str = iter->second.GetPlugin()->GetName();
               str << _(" (") << iter->first << _(")");
               result_dlg->NewTab( str );

               for (i=0; i<descs.size(); i++) 
               {
	               std::string desc = descs[i];
	               std::string value = p.GetInterfaceVector()[0].getString(descs[i]);

                  if (desc.substr(0,3) == "***") 
                  {
                     desc = desc.substr(9,desc.size()-9);
                  }

                  v_desc.push_back( wxString( desc.c_str(), wxConvUTF8 ) );
                  v_value.push_back( wxString( value.c_str(), wxConvUTF8 ) );
               }

               result_dlg->syngas->AddSeperator(' ');
               result_dlg->syngas->AddSeperator('+');
               result_dlg->syngas->AddSeperator(' ');
               result_dlg->Set2Cols(v_desc, v_value);*/
	         }
         }
    
         //result = network->exec->GetModuleResult(-1); //Global result
      
         if (std::string(result)!="") 
         {
           /* Package p;
            p.SetSysId("linkresult.xml");
            p.Load(result, strlen(result));

            descs = p.GetInterfaceVector()[0].getStrings();
            v_desc.clear();
            v_value.clear();
            //v_desc.push_back("Plant Results");
            //v_value.push_back("   ");
	         result_dlg->NewTab( wxT("Plant Results") );

	         for (i=0; i<descs.size(); i++) 
            {
	            v_desc.push_back( wxString( descs[i].c_str(), wxConvUTF8 ) );
	            v_value.push_back( wxString( (p.GetInterfaceVector()[0].getString(descs[i])).c_str(), wxConvUTF8 ) );
	         }
	         result_dlg->syngas->AddSeperator(' ');
	         result_dlg->syngas->AddSeperator('+');
	         result_dlg->syngas->AddSeperator(' ');
	         result_dlg->Set2Cols(v_desc, v_value);*/
         }
      }
      catch (CORBA::Exception &) 
      {
         std::cerr << "Maybe Computational Engine is down " << std::endl;
         return;
      }
   }
   /*else 
   {
      titles.clear();
      titles.push_back("No Plant Results");
      titles.push_back(" ");
  
      result_dlg->syngas->AddSeperator('+');
      result_dlg->syngas->AddRow(titles);
   }*/
       
   // EPRI TAG
   std::vector<wxString> v_desc2, v_value2;
   double total_cccost = 0;
   double total_omcost = 0;
   v_desc.clear();
   v_value.clear();
   for (iter=network->modules.begin(); iter!=network->modules.end(); iter++) 
   {
      if(iter->second.GetPlugin()->financial_dlg != NULL) 
      {
         if(iter->second.GetPlugin()->financial_dlg->_use_data) 
         {
	         double TPC = iter->second.GetPlugin()->financial_dlg->_cc00_d *
	                        (1 +
	                     iter->second.GetPlugin()->financial_dlg->_cc01_d / 100 +
	                     iter->second.GetPlugin()->financial_dlg->_cc02_d / 100 +
	                     iter->second.GetPlugin()->financial_dlg->_cc03_d / 100 +
	                     iter->second.GetPlugin()->financial_dlg->_cc04_d / 100 +
	                     iter->second.GetPlugin()->financial_dlg->_cc05_d / 100);

	         double TPI = TPC + iter->second.GetPlugin()->financial_dlg->_cc06_d;

	         double cccost = TPI + iter->second.GetPlugin()->financial_dlg->_cc07_d +
	                           iter->second.GetPlugin()->financial_dlg->_cc08_d;

	         total_cccost += cccost;

	         v_desc.push_back(wxString(iter->second.GetPlugin()->GetName()));
            std::ostringstream dirStringStream;
            dirStringStream << std::setprecision(2) << cccost;
            std::string dirString = dirStringStream.str();

	         v_value.push_back( wxString( dirString.c_str(), wxConvUTF8) );

	         double TMC = TPC * iter->second.GetPlugin()->financial_dlg->_om00_d / 100;
	
	         double omcost = TMC + 
	                        iter->second.GetPlugin()->financial_dlg->_om01_d +
	                        iter->second.GetPlugin()->financial_dlg->_om02_d +
	                        iter->second.GetPlugin()->financial_dlg->_om02_d * iter->second.GetPlugin()->financial_dlg->_om03_d / 100;

	         total_omcost += omcost;
	
	         v_desc2.push_back(wxString(iter->second.GetPlugin()->GetName()));
            dirStringStream << std::setprecision(2) << omcost;
	         v_value2.push_back( wxString( dirString.c_str(), wxConvUTF8 ) );
         }
      }
   }

   if ( v_desc.size() > 0 ) 
   {
      result_dlg->syngas->AddSeperator(' ');
      result_dlg->syngas->AddSeperator(' ');

      titles.clear();
      titles.push_back( _("Plant Component") );
      titles.push_back( _("Capital Required (M$)") );

      result_dlg->syngas->AddRow(titles);
      result_dlg->syngas->AddSeperator('+');

      v_desc.push_back( _("Total") );
      std::ostringstream dirStringStream;
      dirStringStream << std::setprecision(2) << total_cccost;
      std::string dirString = dirStringStream.str();
      v_value.push_back( wxString( dirString.c_str(), wxConvUTF8 ) );

      result_dlg->Set2Cols(v_desc, v_value);

      //

      result_dlg->syngas->AddSeperator(' ');

      titles.clear();
      titles.push_back( _("Plant Component") );
      titles.push_back( _("Revenue Required (M$)") );

      result_dlg->syngas->AddRow(titles);
      result_dlg->syngas->AddSeperator('+');

      v_desc2.push_back( _("Total") );
      dirStringStream << std::setprecision(2) << total_omcost;
      v_value2.push_back( wxString( dirString.c_str(), wxConvUTF8 ) );

      result_dlg->Set2Cols(v_desc2, v_value2);
   }

   result_dlg->Show();
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::Log(const char* msg)
{
   serviceList->GetMessageLog()->SetMessage( msg );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::DisConExeServer(wxCommandEvent &WXUNUSED(event))
{
   serviceList->DisconnectFromCE();
   //con_menu->Enable(v21ID_SUBMIT,false);
   //con_menu->Enable(v21ID_LOAD, false);
   //con_menu->Enable(v21ID_CONNECT, true);
   run_menu->Enable(v21ID_START_CALC, false);
   // EPRI TAG run_menu->Enable(v21ID_VIEW_RESULT, false);
   con_menu->Enable(v21ID_DISCONNECT, false);
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::DisConVEServer(wxCommandEvent &WXUNUSED(event))
{
   serviceList->DisconnectFromXplorer();
   con_menu->Enable(v21ID_DISCONNECT_VE, false);
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ViewHelp(wxCommandEvent& WXUNUSED(event))
{
   ::wxLaunchDefaultBrowser( wxString( "http://www.vrac.iastate.edu/%7Ebiv/vesuite_installs/docs/releases/1.0.3/vesuite/vesuite.html", wxConvUTF8 ) );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ViewAbout(wxCommandEvent& WXUNUSED(event))
{
   ::wxLaunchDefaultBrowser( wxString( "http://www.vesuite.org", wxConvUTF8 ) );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ViewRevision(wxCommandEvent& WXUNUSED(event))
{
    std::ostringstream revNum;
    revNum << VES_MAJOR_VERSION << "." 
        << VES_MINOR_VERSION << "." 
        << VES_PATCH_VERSION << "." 
        << SVN_VES_REVISION;


    wxString tempNum = wxString("Current Revision: ", wxConvUTF8 ) + 
        wxString(revNum.str().c_str(), wxConvUTF8 );
    
    wxMessageBox( tempNum, _("Revision"), wxOK | wxICON_INFORMATION );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ViewContacts(wxCommandEvent& WXUNUSED(event))
{
   ::wxLaunchDefaultBrowser( wxString( "http://www.vesuite.org/forum/index.php", wxConvUTF8 ) );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ViewPlatformInfo(wxCommandEvent& WXUNUSED(event))
{
   wxMessageBox( ::wxGetOsDescription(),_("Platform Info"), 
                 wxOK | wxICON_INFORMATION );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::LaunchDeviceProperties( wxCommandEvent& WXUNUSED(event) )
{
   if ( deviceProperties == 0 )
   {
      // create pane and set appropriate vars
      deviceProperties = new DeviceProperties( this );
   }
   // now show it
   deviceProperties->Show();
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::LaunchNavigationPane( wxCommandEvent& WXUNUSED(event) )
{
   if ( navPane == 0 )
   {
      // create pane and set appropriate vars
      navPane = new NavigationPane( this );
   }
   // now show it
   navPane->Show();
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::SetBackgroundColor( wxCommandEvent& WXUNUSED(event) )
{
    //this is kinda confusing...thanks wx!!!
    //wxColourData data;
    //data.SetChooseFull(true);
    VE_XML::CommandWeakPtr bkColor = UserPreferencesDataBuffer::instance()->
        GetCommand( "CHANGE_BACKGROUND_COLOR" );
    
    if( bkColor->GetCommandName() != "NULL" )
    {
        bkColor->GetDataValuePair( "Background Color" )->GetData( xplorerColor );
    }

    xplorerWxColor->GetColour().Set( 
        static_cast<unsigned char>(xplorerColor.at( 0 )), 
        static_cast<unsigned char>(xplorerColor.at( 1 )), 
        static_cast<unsigned char>(xplorerColor.at( 2 )), 
        static_cast<unsigned char>(xplorerColor.at( 3 )) 
        );
    
    wxColourDialog colorDlg(this,xplorerWxColor);
    colorDlg.SetTitle(wxString("Xplorer Background Color", wxConvUTF8));

   if (colorDlg.ShowModal() == wxID_OK)
   {
      *xplorerWxColor = colorDlg.GetColourData();
      wxColour col = xplorerWxColor->GetColour();

      xplorerColor.clear();
      xplorerColor.push_back(static_cast<double>(col.Red())/255.0);
      xplorerColor.push_back(static_cast<double>(col.Green())/255.0);
      xplorerColor.push_back(static_cast<double>(col.Blue())/255.0);
      xplorerColor.push_back(1.0);

      // Create the command and data value pairs
      VE_XML::DataValuePairWeakPtr dataValuePair = new VE_XML::DataValuePair();
      dataValuePair->SetData(std::string("Background Color"),xplorerColor);
      VE_XML::CommandWeakPtr veCommand = new VE_XML::Command();
      veCommand->SetCommandName(std::string("CHANGE_BACKGROUND_COLOR"));
      veCommand->AddDataValuePair(dataValuePair);

      serviceList->SendCommandStringToXplorer( veCommand );
         
      UserPreferencesDataBuffer::instance()->SetCommand( "CHANGE_BACKGROUND_COLOR", veCommand );
   }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ChangeDevice( wxCommandEvent& event )
{
   //Create the command and data value pairs
   VE_XML::DataValuePairWeakPtr dvp = new VE_XML::DataValuePair();
   VE_XML::CommandWeakPtr command = new VE_XML::Command();
   
   std::string device;

   if( event.GetId() == WAND )
   {
      device = "Wand";
   }

   else if( event.GetId() == KEYBOARD_MOUSE )
   {
      device = "KeyboardMouse";
   }

   dvp->SetData( std::string( "Device" ), device );
   
   command->SetCommandName( std::string( "CHANGE_DEVICE" ) );
   command->AddDataValuePair( dvp );

   serviceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::DisplaySelection( wxCommandEvent& event )
{
   //Create the command and data value pairs
   VE_XML::DataValuePairWeakPtr DVP = new VE_XML::DataValuePair();
   VE_XML::CommandWeakPtr command = new VE_XML::Command();

   unsigned int value;

   if(event.GetId() == FRAME_RATE)
	{
      value=xplorerDisplayMenu->IsChecked(FRAME_RATE);
      DVP->SetData(std::string("FrameRateID"),value);
   }

   else if(event.GetId() == COORDINATE_SYSTEM)
	{
      value=xplorerDisplayMenu->IsChecked(COORDINATE_SYSTEM);
      DVP->SetData(std::string("CoordSysID"),value);
   }
   
   command->SetCommandName(std::string("DISPLAY_SELECTION"));
   command->AddDataValuePair(DVP);

   serviceList->SendCommandStringToXplorer(command);
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ViewSelection( wxCommandEvent& event )
{
   //Create the command and data value pairs
   VE_XML::DataValuePairWeakPtr dvp = new VE_XML::DataValuePair();
   VE_XML::CommandWeakPtr command = new VE_XML::Command();
   
   std::string view;

   if( event.GetId() == FRAME_ALL )
   {
      view = "Frame All";
   }
   else if( event.GetId() == FRAME_SELECTION )
   {
      view = "Frame Selection";
   }
   else if( event.GetId() == RESET )
   {
      view = "Reset";
   }

   dvp->SetData( std::string( "View" ), view );
   
   command->SetCommandName( std::string( "VIEW_SELECTION" ) );
   command->AddDataValuePair( dvp );

   serviceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::LaunchRecordScenes( wxCommandEvent& WXUNUSED(event) )
{
   if ( recordScenes == 0 )
   {
      // create pane and set appropriate vars
      recordScenes = new UI_TeacherTab( this );
   }

   // now show it
   recordScenes->Show();
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::LaunchViewpointsPane( wxCommandEvent& WXUNUSED(event) )
{
   if ( viewlocPane == 0 )
   {
      // create pane and set appropriate vars
      viewlocPane = new ViewLocPane( this );
   }
   // now show it
   viewlocPane->Show();
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::JugglerSettings( wxCommandEvent& event )
{
   // Create the command and data value pairs
   VE_XML::DataValuePairWeakPtr dataValuePair = 
                           new VE_XML::DataValuePair(  std::string("FLOAT") );
   dataValuePair->SetDataName( "Stereo" );
   if ( event.GetId() == JUGGLER_STEREO )
   {
      dataValuePair->SetDataValue( 1.0 );
   }
   else
   {
      dataValuePair->SetDataValue( 0.0 );
   }
   VE_XML::CommandWeakPtr veCommand = new VE_XML::Command();
   veCommand->SetCommandName( std::string("Juggler_Display_Data") );
   veCommand->AddDataValuePair( dataValuePair );

   serviceList->SendCommandStringToXplorer( veCommand );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ExitXplorer( void )
{
   VE_XML::DataValuePairWeakPtr dataValuePair = 
                           new VE_XML::DataValuePair( std::string("STRING") );
   dataValuePair->SetData( "EXIT_FLAG", "EXIT" );
   VE_XML::CommandWeakPtr veCommand = new VE_XML::Command();
   veCommand->SetCommandName( std::string( "EXIT_XPLORER" ) );
   veCommand->AddDataValuePair( dataValuePair );
   
   serviceList->SendCommandStringToXplorer( veCommand );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OnExitXplorer( wxCommandEvent& WXUNUSED(event) )
{
   ExitXplorer();
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ProcessCommandLineArgs( void )
{
   int argc = ::wxGetApp().argc;
   char** argv = new char*[ argc ];
   std::string vesFile;
   for ( int i = 0; i < argc; ++ i )
   {
      if ( ( ConvertUnicode( ::wxGetApp().argv[ i ] ) == std::string( "-VESFile" )) &&
           ( (i + 1) < argc )
         )
      {
         Log(std::string(std::string("Loading VES file: ") + ConvertUnicode( ::wxGetApp().argv[ i + 1 ] ) ).c_str() );
         Log("\n");
         vesFile.assign( ConvertUnicode( ::wxGetApp().argv[ i + 1 ] ) );
         break;
      }
   }
   
   if ( vesFile.empty() )
   {
      return;
   }
   
   wxFileName vesFileName( wxString( vesFile.c_str(), wxConvUTF8 ) );
   bool success = vesFileName.MakeRelativeTo( ::wxGetCwd() );   
   if ( !success )
   {
      wxMessageBox( _("Can't open a VES file on another drive."), 
                    _("VES File Read Error"), wxOK | wxICON_INFORMATION );
      return;
   }

   directory = vesFileName.GetPath();
   //change conductor working dir
   ::wxSetWorkingDirectory( directory );
   
   //Send Command to change xplorer working dir
   // Create the command and data value pairs
   VE_XML::DataValuePairWeakPtr dataValuePair = 
      new VE_XML::DataValuePair(  std::string("STRING") );
   dataValuePair->SetData( "WORKING_DIRECTORY", ConvertUnicode( directory.c_str() ) );
   VE_XML::CommandWeakPtr veCommand = new VE_XML::Command();
   veCommand->SetCommandName( std::string("Change Working Directory") );
   veCommand->AddDataValuePair( dataValuePair );
   serviceList->SendCommandStringToXplorer( veCommand );
   
   //Now laod the xml data now that we are in the correct directory
   fname=vesFileName.GetFullName();
   // we submit after new to make sure that the ce and ge ar cleared
   wxCommandEvent event;
   network->Load( ConvertUnicode( fname.c_str() ), true );
   // we submit after load to give ce and ge the new network
   SubmitToServer( event );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::EnableCEGUIMenuItems( void )
{
   if ( !serviceList->IsConnectedToCE() )
   {
      return;
   }

   con_menu->Enable(v21ID_SUBMIT,true);
   con_menu->Enable(v21ID_LOAD, true);
   //frame_->con_menu->Enable(v21ID_CONNECT, false);
   run_menu->Enable(v21ID_VIEW_RESULT, true);
   con_menu->Enable(v21ID_DISCONNECT, true);
}
////////////////////////////////////////////////////////////////////////////////
UserPreferences* AppFrame::GetUserPreferences( void )
{
   return preferences;
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OnPreferences( wxCommandEvent& WXUNUSED(event) )
{
   wxSize displaySize = ::wxGetDisplaySize();
   wxRect dialogPosition( 100, 50, 500, 300 );
   preferences->SetSize( dialogPosition );
   preferences->ShowModal();
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OnChangeWorkingDirectory( wxCommandEvent& event )
{
    wxDirDialog dialog( this, _T("Change Working Directory..."),
                         ::wxGetCwd(),
                         wxDD_DEFAULT_STYLE
                         );
    
    if( dialog.ShowModal() != wxID_OK )
    {
        return;
    }
    
    wxFileName vesFileName( dialog.GetPath() );
    if( !vesFileName.MakeRelativeTo( ::wxGetCwd() ) )
    {
        wxMessageBox( _("Can't change working directory to another drive."), 
                      _("Change Directory Error"), wxOK | wxICON_INFORMATION );
        return;
    }
    
    ///Clear the canvas then change the directory
    NewCanvas( event );
    
    directory = vesFileName.GetPath( wxPATH_GET_VOLUME, wxPATH_UNIX);
    //change conductor working dir
    ::wxSetWorkingDirectory( directory );
    directory.Replace( _("\\"), _("/"), true );
    
    VE_XML::DataValuePairWeakPtr dvp = new VE_XML::DataValuePair();
    VE_XML::CommandStrongPtr command = new VE_XML::Command();
    std::string mode = ConvertUnicode( directory.c_str() );
    dvp->SetData( std::string( "Change Working Directory" ), mode );
    command->SetCommandName( std::string( "WORKING_DIRECTORY" ) );
    command->AddDataValuePair( dvp );
    
    VE_Conductor::CORBAServiceList::instance()->
        SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ChangeXplorerViewSettings( wxCommandEvent& event )
{
   VE_XML::DataValuePairWeakPtr dataValuePair = 
      new VE_XML::DataValuePair(  std::string("STRING") );
   if ( event.GetId() == CHANGE_XPLORER_VIEW_NETWORK )
   {
      dataValuePair->SetData( "CHANGE_XPLORER_VIEW", "CHANGE_XPLORER_VIEW_NETWORK" );
   }
   else if ( event.GetId() == CHANGE_XPLORER_VIEW_CAD )
   {
      dataValuePair->SetData( "CHANGE_XPLORER_VIEW", "CHANGE_XPLORER_VIEW_CAD" );
   }
   else if ( event.GetId() == CHANGE_XPLORER_VIEW_LOGO )
   {
      dataValuePair->SetData( "CHANGE_XPLORER_VIEW", "CHANGE_XPLORER_VIEW_LOGO" );
   }
   else
   {
      dataValuePair->SetData( "CHANGE_XPLORER_VIEW", "ERROR" );
   }
   
   VE_XML::CommandWeakPtr veCommand = new VE_XML::Command();
   veCommand->SetCommandName( std::string("CHANGE_XPLORER_VIEW") );
   veCommand->AddDataValuePair( dataValuePair );
   serviceList->SendCommandStringToXplorer( veCommand );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OnInternalIdle()
{
	//only when not dragging
    if( !network )
    {
        return;
    }
    
    if( !network->IsDragging() )
    {	
        ///Servicelist is initialized before network...
        serviceList->CheckORBWorkLoad();
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ShutdownXplorerOptionOn()
{
    xplorerMenu->Append( XPLORER_EXIT, _("Shutdown Xplorer") );
    xplorerMenu->Enable( XPLORER_EXIT, true);
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ShutdownXplorerOptionOff()
{
    xplorerMenu->Remove( XPLORER_EXIT );
}
