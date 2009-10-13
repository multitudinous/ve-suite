/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>

#include "AppFrame.h"
#include "ConductorAppEnums.h"
#include <ves/conductor/ConductorLibEnums.h>

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

#include <ves/conductor/ResultPanel.h>
#include <ves/conductor/util/OrbThread.h>
#include <ves/conductor/FinancialDialog.h>
#include <ves/conductor/TextResultDialog.h>
#include <ves/conductor/TexTable.h>
#include <ves/conductor/GlobalParamDialog.h>
#include <ves/conductor/SummaryResultDialog.h>
#include <ves/conductor/FindDialog.h>
#include <ves/conductor/UserPreferencesDataBuffer.h>
#include <ves/conductor/XMLDataBufferEngine.h>
#include <ves/conductor/Network.h>
#include <ves/conductor/Canvas.h>
#include <ves/conductor/UITeacherTab.h>
#include <ves/conductor/MinervaDialog.h>
#include <ves/conductor/NavigationPane.h>
#include <ves/conductor/EphemerisDialog.h>
#include <ves/conductor/ViewLocPane.h>

#include "ConductorApp.h"
#include "UserPreferences.h"
#include "AvailableModules.h"
#include "HierarchyTree.h"
#include "DeviceProperties.h"
#include "Splitter.h"
#include "AppToolBar.h"
#include "ExportMenu.h"

#include <ves/conductor/util/CADNodeManagerDlg.h>
#include <ves/conductor/IconChooser.h>
#include <ves/conductor/Module.h>
#include <ves/conductor/util/Tag.h>
#include <ves/conductor/util/DataLoggerEngine.h>

#include <ves/open/xml/DOMDocumentManager.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/TwoDStringArrayPtr.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/cad/CADCreator.h>
#include <ves/open/xml/shader/ShaderCreator.h>
#include <ves/open/xml/model/ModelCreator.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/model/Tag.h>
#include <ves/open/xml/User.h>
#include <ves/open/xml/OneDDoubleArray.h>

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
#include <wx/aboutdlg.h>

#include <ves/util/commands/Minerva.h>
#include <ves/util/icons/ve_icon64x64.xpm>
#include <ves/util/icons/ve_icon32x32.xpm>
#include <ves/VEConfig.h>

// --- C/C++ Libraries --- //
#include <sstream>
#include <iomanip>

using namespace ves::conductor::util;
using namespace ves::conductor;
using namespace ves::open::xml;
using namespace ves::open::xml::cad;
using namespace ves::open::xml::shader;


BEGIN_EVENT_TABLE( AppFrame, wxFrame )
    EVT_TIMER( APPFRAME_TIMER_ID, AppFrame::OnTimer )
    //EVT_CLOSE( AppFrame::OnFrameClose )
    EVT_MENU( APPFRAME_V21ID_ZOOMIN, AppFrame::ZoomIn )
    EVT_MENU( APPFRAME_V21ID_ZOOMOUT, AppFrame::ZoomOut )
    EVT_MENU( APPFRAME_V21ID_ZOOMALL, AppFrame::ZoomAll )
    EVT_CHAR( AppFrame::OnKeyPress )
    EVT_MENU( APPFRAME_SAVE, AppFrame::Save )
    EVT_MENU( APPFRAME_SAVEAS, AppFrame::SaveAs )
    EVT_MENU( APPFRAME_NEW, AppFrame::NewCanvas )
    //This is probably a bug and needs to be fixed
    EVT_MENU( wxID_EXIT, AppFrame::FrameClose )
    EVT_MENU( APPFRAME_PREFERENCES, AppFrame::OnPreferences )
    EVT_MENU( APPFRAME_CLEAR_RECENT_FILES, AppFrame::OnClearRecentFiles )
    EVT_MENU( APPFRAME_OPEN, AppFrame::Open )
    EVT_MENU( APPFRAME_START_DATALOGGING, AppFrame::OnDataLogging )
    EVT_MENU( APPFRAME_STOP_DATALOGGING, AppFrame::OnDataLogging )

    EVT_MENU( APPFRAME_RUN, AppFrame::Run )
    EVT_MENU( APPFRAME_CHANGE_WORKING_DIRECTORY, AppFrame::OnChangeWorkingDirectory )

    EVT_MENU_RANGE( wxID_FILE1, wxID_FILE9  , AppFrame::OpenRecentFile )

    EVT_MENU( APPFRAME_V21ID_LOAD, AppFrame::LoadFromServer )
    EVT_MENU( APPFRAME_QUERY_FROM_SERVER, AppFrame::QueryFromServer )
    EVT_MENU( APPFRAME_V21ID_SUBMIT, AppFrame::SubmitToServer )
    //EVT_MENU( APPFRAME_V21ID_CONNECT, AppFrame::ConExeServer )
    EVT_MENU( APPFRAME_V21ID_DISCONNECT, AppFrame::DisConExeServer )
    EVT_MENU( APPFRAME_V21ID_DISCONNECT_VE, AppFrame::DisConVEServer )
    //EVT_MENU( APPFRAME_V21ID_CONNECT_VE, AppFrame::ConVEServer )
    EVT_MENU( APPFRAME_V21ID_START_CALC, AppFrame::StartCalc )
    EVT_MENU( APPFRAME_V21ID_STOP_CALC, AppFrame::StopCalc )
    EVT_MENU( APPFRAME_V21ID_PAUSE_CALC, AppFrame::PauseCalc )
    EVT_MENU( APPFRAME_V21ID_RESUME_CALC, AppFrame::ResumeCalc )

    EVT_MENU( APPFRAME_V21ID_HELP, AppFrame::ViewHelp )
    EVT_MENU( APPFRAME_V21ID_ABOUT, AppFrame::ViewAbout )
    //EVT_MENU( APPFRAME_V21ID_REVISION, AppFrame::ViewRevision )
    EVT_MENU( APPFRAME_V21ID_CONTACTS, AppFrame::ViewContacts )
    EVT_MENU( APPFRAME_V21ID_PLATFORM, AppFrame::ViewPlatformInfo )

    EVT_MENU( APPFRAME_V21ID_VIEW_RESULT, AppFrame::ViewResult )

    EVT_MENU( APPFRAME_KEYBOARD_MOUSE, AppFrame::ChangeDevice )
    EVT_MENU( APPFRAME_WAND, AppFrame::ChangeDevice )
    EVT_MENU( APPFRAME_TABLET, AppFrame::ChangeDevice )
    EVT_MENU( APPFRAME_GLOVES, AppFrame::ChangeDevice )

    EVT_MENU( APPFRAME_DEVICE_PROPERTIES, AppFrame::LaunchDeviceProperties )

    EVT_MENU( APPFRAME_FRAME_RATE, AppFrame::DisplaySelection )
    EVT_MENU( APPFRAME_COORDINATE_SYSTEM, AppFrame::DisplaySelection )

    EVT_MENU( APPFRAME_FRAME_ALL, AppFrame::ViewSelection )
    EVT_MENU( APPFRAME_FRAME_SELECTION, AppFrame::ViewSelection )
    EVT_MENU( APPFRAME_RESET, AppFrame::ViewSelection )

    EVT_MENU( APPFRAME_XPLORER_NAVIGATION, AppFrame::LaunchNavigationPane )
    EVT_MENU( APPFRAME_XPLORER_VIEWPOINTS, AppFrame::LaunchViewpointsPane )
    EVT_MENU( APPFRAME_XPLORER_SCENES, AppFrame::LaunchRecordScenes )
    EVT_MENU( APPFRAME_XPLORER_COLOR, AppFrame::SetBackgroundColor )
    EVT_MENU( APPFRAME_XPLORER_EPHEMERIS, AppFrame::SetEphemerisData )
    EVT_MENU( APPFRAME_XPLORER_EXIT, AppFrame::OnExitXplorer )
    EVT_MENU( APPFRAME_JUGGLER_STEREO, AppFrame::JugglerSettings )
    EVT_MENU( APPFRAME_JUGGLER_MONO, AppFrame::JugglerSettings )
    EVT_MENU( APPFRAME_CONDUCTOR_FIND, AppFrame::FindBlocks )
    EVT_MENU( APPFRAME_CHANGE_XPLORER_VIEW_NETWORK, AppFrame::ChangeXplorerViewSettings )
    EVT_MENU( APPFRAME_CHANGE_XPLORER_VIEW_CAD, AppFrame::ChangeXplorerViewSettings )
    EVT_MENU( APPFRAME_CHANGE_XPLORER_VIEW_LOGO, AppFrame::ChangeXplorerViewSettings )
    EVT_MENU( APPFRAME_XPLORER_DATALOGGING_LOOPING, AppFrame::OnDataLoggingSettings )
    
    EVT_MENU( EXPORTMENU_SCREEN_SHOT, ExportMenu::OnScreenShot )
    EVT_MENU( EXPORTMENU_DOT_FILE, ExportMenu::OnDOTFile )
    EVT_MENU( UIPLUGINBASE_DEL_MOD, AppFrame::OnDelMod )
    EVT_MENU( UIPLUGINBASE_MAKE_HIER, AppFrame::OnMakeIntoHierarchy )
    EVT_MENU( UIPLUGINBASE_SET_UI_PLUGIN_NAME, AppFrame::SetTreeItemName )
	EVT_MENU( UIPLUGINBASE_SHOW_ICON_CHOOSER, AppFrame::OnShowIconChooser )
	
    EVT_MENU( APPFRAME_UPDATE_HIER_TREE, AppFrame::UpdateHierarchyTree )

    EVT_MENU( APPFRAME_MINERVA_ADD_PLANET, AppFrame::OnAddPlanet )
    EVT_MENU( APPFRAME_MINERVA_REMOVE_PLANET, AppFrame::OnRemovePlanet )
    EVT_MENU( APPFRAME_MINERVA_SHOW_DIALOG, AppFrame::ShowMinervaDialog )

    EVT_WINDOW_CREATE( AppFrame::OnChildCreate ) 
	EVT_BUTTON( ICONCHOOSER_OK, AppFrame::OnChangeIcon )
    EVT_UPDATE_UI( CANVAS_UPDATE_NETWORK_DATA, AppFrame::LoadNewNetwork )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
AppFrame::AppFrame( wxWindow * parent, wxWindowID id, const wxString& title )
        :
        wxFrame(
            parent,
            id,
            title,
            wxDefaultPosition,
            wxDefaultSize ),
        xplorerMenu( 0 ),
        recordScenes( 0 ),
        canvas( 0 ),
        hierarchyTree(),
        _treeView( 0 ),
        deviceProperties( 0 ),
        navPane( 0 ),
        viewlocPane( 0 ),
        m_ephemeris( 0 ),
        iconChooser( 0 ),
        wx_log_splitter( 0 ),
        wx_ve_splitter( 0 ),
        wx_nw_splitter( 0 ),
        menubar( 0 ),
        appToolBar( 0 ),
        serviceList( CORBAServiceList::instance() ),
        newCanvas( false ),
        mTimer( this, APPFRAME_TIMER_ID ),
        _minervaDialog ( 0x0 ),
        mDestoryFrame( false ),
        f_financial( true ),
        f_geometry( true ),
        f_visualization( true )
{
    char** tempArray = new char*[ ::wxGetApp().argc ];
    for( unsigned int i = 0; i < ::wxGetApp().argc; ++i )
    {
        tempArray[ i ] = new char[ strlen( ConvertUnicode( ::wxGetApp().argv[ i ] ).c_str() ) + 1 ];
        strcpy( tempArray[ i ], ConvertUnicode( ::wxGetApp().argv[ i ] ).c_str() );
    }
    serviceList->SetArgcArgv( ::wxGetApp().argc, tempArray );
    delete [] tempArray;
    tempArray = 0;
    
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
    _detectDisplayAndCreate();

    CreateMenu();

    appToolBar = new AppToolBar( this );
    this->SetToolBar( appToolBar );
    CreateStatusBar();
    SetStatusText( _( "VE-Conductor Status" ) );
    //SetAutoLayout(true);

    directory = _( "" );
    //mVESFileName = _( "" );

    m_recentVESFiles->UseMenu( file_menu );
    m_recentVESFiles->AddFilesToMenu( file_menu );

    ///Initialize VE-Open
    XMLObjectFactory::Instance()->RegisterObjectCreator( "XML", new XMLCreator() );
    XMLObjectFactory::Instance()->RegisterObjectCreator( "Shader", new shader::ShaderCreator() );
    XMLObjectFactory::Instance()->RegisterObjectCreator( "Model", new model::ModelCreator() );
    XMLObjectFactory::Instance()->RegisterObjectCreator( "CAD", new cad::CADCreator() );

    xplorerColor.push_back( 0.0f );
    xplorerColor.push_back( 0.0f );
    xplorerColor.push_back( 0.0f );
    xplorerColor.push_back( 1.0f );
    xplorerWxColor = new wxColourData();
    xplorerWxColor->SetChooseFull( true );

    if( preferences->GetMode( "Auto Launch Nav Pane" ) )
    {
        wxCommandEvent event;
        LaunchNavigationPane( event );
    }

    if( preferences->GetMode( "Use Preferred Background Color" ) )
    {
        xplorerColor = preferences->GetBackgroundColor();
    }

    DataValuePairPtr dataValuePair( new DataValuePair() );
    dataValuePair->SetData( std::string( "Background Color" ), xplorerColor );
    CommandPtr veCommand( new Command() );
    veCommand->SetCommandName( std::string( "CHANGE_BACKGROUND_COLOR" ) );
    veCommand->AddDataValuePair( dataValuePair );
    ///Set the command on the buffer first so that a strong ptr is
    ///referencing the memory
    UserPreferencesDataBuffer::instance()->SetCommand(
        "CHANGE_BACKGROUND_COLOR", veCommand );

    CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );

    //Try and load network from server if one is already present
    std::string nw_str = CORBAServiceList::instance()->GetNetwork();
    if( !nw_str.empty() )
    {
        canvas->PopulateNetworks( nw_str );
    }
    
    //create hierarchy page
    hierarchyTree->PopulateTree(); 
                                 
    //Process command line args to see if ves file needs to be loaded
    ProcessCommandLineArgs();
    
    //Setup the orb timer
    mTimer.Start( 500 );
}
////////////////////////////////////////////////////////////////////////////////
AppFrame::~AppFrame()
{
    // Clean up the canvas and plugins first because
    // if left to wx, on windows things get messy with unloading plugins
    // and cleaning up memory at the same time
	//CloseAspenSimulation();
    //Cleanup all the plugins before wx does
    //mDestoryFrame = true;
    canvas->CleanUpNetworks();
    if( canvas )
    {
        //wx_nw_splitter->RemoveChild( canvas );
        canvas->Destroy();
    }

    //Shutdown xplorer
    if (( GetDisplayMode() == "Desktop" ) ||
            ( !preferences->GetMode( "Shut_Down_Xplorer_Option" ) ) )
    {
        ExitXplorer();
    }
    //serviceList->DisconnectFromXplorer();

	//if(AspenSimOpen)
	//{
	//	CloseAspenSimulation();
	//}

    //Store settings to wxConfig to be written out
    StoreFrameSize( GetRect() );
    StoreConfig();
    StoreRecentFile();

    UserPreferencesDataBuffer::instance()->CleanUp();
    
    //We have to mannually destroy these to make sure that things shutdown
    //properly with CORBA. There may be a possible way to get around this but
    //am not sure.
    //serviceList->CleanUp();
    //serviceList = 0;

    //Write the final script file if we need to
    ves::conductor::util::DataLoggerEngine::instance()->CleanUp();
    
    delete m_recentVESFiles;
    m_recentVESFiles = 0;
    for( int i = 0; i < pids.size(); i++ )
    {
        wxProcess::Kill( pids[i] );
    }
}
////////////////////////////////////////////////////////////////////////////////
std::string AppFrame::GetDisplayMode()
{
    return _displayMode;
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::_detectDisplay()
{
    //Set this by default
    _displayMode = "Tablet";

    for( int i = 1; i < wxTheApp->argc; ++i )
    {
        if( ConvertUnicode( wxTheApp->argv[i] ) == std::string( "-VESDesktop" ) )
        {
            _displayMode = std::string( "Desktop" );

            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::_createTreeAndLogWindow( wxWindow* parent )
{
    wxTextCtrl* textctrl = 0;
    if( GetDisplayMode() == "Tablet" )
    {
        wx_log_splitter = new Splitter( parent, -1 );
        wx_log_splitter->SetMinimumPaneSize( 40 );
        textctrl = new wxTextCtrl( wx_log_splitter, 
            APPFRAME_MYLOG, _( "" ), wxDefaultPosition, wxDefaultSize, 
            wxTE_MULTILINE | wxTE_READONLY );
        
        wx_nw_splitter = new Splitter( wx_log_splitter, -1 );
    }
    else
    {
        textctrl = new wxTextCtrl( this, 
            APPFRAME_MYLOG, _( "" ), wxDefaultPosition, wxDefaultSize, 
            wxTE_MULTILINE | wxTE_READONLY );

        wx_nw_splitter = new Splitter( parent, -1 );
    }
    // set our text control as the log target
    wxLogTextCtrl* logWindow = new wxLogTextCtrl( textctrl );
    delete wxLog::SetActiveTarget( logWindow );
    
    wx_nw_splitter->SetMinimumPaneSize( 1 );

    //create side pane - notebook
    side_pane = new wxNotebook( wx_nw_splitter, -1 , wxDefaultPosition, wxDefaultSize, wxNB_BOTTOM );

    //create module page
    wxPanel* modPage = new wxPanel( side_pane, -1, wxDefaultPosition, wxDefaultSize );
    av_modules = new AvailableModules( modPage, AVAILABLEMODULES_TREE_CTRL,
                                    wxDefaultPosition, wxDefaultSize, wxTR_HAS_BUTTONS );

    //make module panel fill the notebook page
    wxBoxSizer *sizerPanel = new wxBoxSizer( wxVERTICAL );
    sizerPanel->Add( av_modules, 1, wxEXPAND );
    modPage->SetSizer( sizerPanel );

    //add the module panel to page
    side_pane->AddPage( modPage, wxT( "Modules" ) );

    //create hierarchy page
    wxPanel * hierPage = new wxPanel( side_pane, -1, wxDefaultPosition,
        wxDefaultSize );
    hierarchyTree = new HierarchyTree( hierPage, HIERARCHYTREE_CTRL,
        wxDefaultPosition, wxDefaultSize, wxTR_HAS_BUTTONS );

    //make hierarchy panel fill the notebook page
    sizerPanel = new wxBoxSizer( wxVERTICAL );
    sizerPanel->Add( hierarchyTree, 1, wxEXPAND );
    hierPage->SetSizer( sizerPanel );

    //add the hierarchy panel to page
    side_pane->AddPage( hierPage, wxT( "Hierarchy" ) );

    //add network to splitter
    canvas = new Canvas( wx_nw_splitter, -1 );
    if( GetDisplayMode() == "Tablet" )
    {
        canvas->SetTreeViewWindow( this );
    }
    else
    {
        canvas->SetTreeViewWindow( _treeView );
    }
    canvas->SetMainFrame(this);


    //tells module panel where to send the selected module
    //Network * network = canvas->GetActiveNetwork();
    av_modules->SetFrame( this );
    av_modules->SetCanvas( canvas );
    hierarchyTree->SetCanvas( canvas );

    if( GetDisplayMode() == "Tablet" )
    {
        wxRect tempPos = GetRect();
        int sashPos = tempPos.GetHeight() - 170;
        wx_log_splitter->SplitHorizontally( textctrl, 
            wx_nw_splitter, sashPos );        
    }

    //wx_nw_splitter->SplitVertically( side_pane, network, 140 );
    wx_nw_splitter->SplitVertically( side_pane, canvas, 140 );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::_configureDesktop()
{
    SetTitle( _( "VE-Suite: www.vesuite.org" ) );
    _treeView = new wxDialog();
    _treeView->SetExtraStyle( ~wxWS_EX_BLOCK_EVENTS );
    _treeView->Create( this, ::wxNewId(), _( "VE-Conductor Network" ),
                              wxDefaultPosition, wxDefaultSize,
                              (wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER) 
                              & ~wxCLOSE_BOX);
    //wxCAPTION | wxRESIZE_BORDER);//(wxDEFAULT_DIALOG_STYLE&~
    //(wxCLOSE_BOX | wxRESIZE_BORDER | wxRESIZE_BOX | wxMAXIMIZE_BOX)));
    //|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX|wxCLOSE_BOX));
    wxBoxSizer* treeViewSizer = new wxBoxSizer( wxHORIZONTAL );

    _treeView->SetAutoLayout( true );
    _treeView->SetSizer( treeViewSizer );

    _createTreeAndLogWindow( _treeView );
    treeViewSizer->Add( wx_nw_splitter, 1, wxALIGN_CENTER | wxEXPAND );

    int displayWidth, displayHeight = 0;
    ::wxDisplaySize( &displayWidth, &displayHeight );
#ifdef WIN32
    SetSize( wxSize( displayWidth, 195/*displayHeight*0.0732421875*/ ) );
#else
    SetSize( wxSize( displayWidth, 120/*displayHeight*0.0732421875*/ ) );
#endif
    SetPosition( wxPoint( 0, 0 ) );
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
    _createTreeAndLogWindow( this );
    SetSize( DetermineTabletFrameSize() );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::_detectDisplayAndCreate()
{
    _detectDisplay();
    if( GetDisplayMode() == "Desktop" )
    {
        _configureDesktop();
        SetWindowStyle( wxDEFAULT_FRAME_STYLE | wxRESIZE_BORDER | wxRESIZE_BOX | wxMAXIMIZE_BOX );
        //Set min size so all buttons still show and message window displays at least one line
        SetMinSize( wxSize( 600, 100 ) );
    }
    else if( GetDisplayMode() == "Tablet" )
    {
        _configureTablet();
        SetWindowStyle( wxDEFAULT_FRAME_STYLE | wxRESIZE_BORDER | wxRESIZE_BOX | wxMAXIMIZE_BOX );
        //set min size so all buttons still show and message window displays three lines and canvas
        SetMinSize( wxSize( 600, 260 ) );
    }
    else
    {
        wxMessageBox( _( "Unable to create GUI." ), _( "Unknown display request!" ),
                      wxOK | wxICON_INFORMATION );
        _exit( 1 );
    }
}
////////////////////////////////////////////////////////////////////////////////
bool AppFrame::Show( bool value )
{
    bool status = false;
    status = wxFrame::Show( value );

    if( GetDisplayMode() == "Desktop" )
    {
        int displayWidth, displayHeight = 0;
        ::wxDisplaySize( &displayWidth, &displayHeight );
        wxRect bbox = wxTheApp->GetTopWindow()->GetRect();

        wxRect dialogPosition( 2*displayWidth / 3,
                               bbox.GetBottomRight().y,
                               displayWidth / 3,
                               ( displayHeight - bbox.GetBottomRight().y ) / 2
                             );
        _treeView->SetSize( dialogPosition );

        status = _treeView->Show();
    }
    return status;
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::GetConfig()
{
    wxConfig* cfg = new wxConfig( wxTheApp->GetAppName() );
    //Do not let wx create a new wxConfig with Get calls
    cfg->DontCreateOnDemand();
    wxConfig::Set( cfg );

    bool exist = false;

    wxString key = FEATURE;
    if( cfg->Exists( key ) )
    {
        cfg->Read( key + _T( "/" ) + F_FINANCIAL, &f_financial );
        cfg->Read( key + _T( "/" ) + F_GEOMETRY, &f_geometry );
        cfg->Read( key + _T( "/" ) + F_VISUALIZATION, &f_visualization );
    }
    else
    {
        f_financial = true;
        f_geometry = true;
        f_visualization = true;
    }

    m_recentVESFiles->Load( *cfg );
}
////////////////////////////////////////////////////////////////////////////////
wxRect AppFrame::DetermineTabletFrameSize()
{
    const int minFrameWidth = 600;
    const int minFrameHight = 400;

    wxRect rect;
    //wxSize scr = wxGetDisplaySize();

    wxConfig* cfg = static_cast<wxConfig*>( wxConfig::Get() );
    wxString key = LOCATION + wxString::Format( _( "%d" ), 0 );
    if( cfg->Exists( key ) )
    {
        cfg->Read( key + _T( "/" ) + LOCATION_X, &rect.x );
        cfg->Read( key + _T( "/" ) + LOCATION_Y, &rect.y );
        cfg->Read( key + _T( "/" ) + LOCATION_W, &rect.width );
        cfg->Read( key + _T( "/" ) + LOCATION_H, &rect.height );
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
    wxConfig* cfg = static_cast<wxConfig*>( wxConfig::Get() );
    if( !cfg )
    {
        return;
    }
    wxString key = LOCATION + wxString::Format( _( "%d" ), 0 );
    cfg->Write( key + _T( "/" ) + LOCATION_X, rect.x );
    cfg->Write( key + _T( "/" ) + LOCATION_Y, rect.y );
    cfg->Write( key + _T( "/" ) + LOCATION_W, rect.width );
    cfg->Write( key + _T( "/" ) + LOCATION_H, rect.height );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::StoreConfig()
{
    //store config
    wxConfig* cfg = static_cast<wxConfig*>( wxConfig::Get() );
    if( !cfg )
    {
        return;
    }
    wxString key = FEATURE;
    cfg->Write( key + _T( "/" ) + F_FINANCIAL, f_financial );
    cfg->Write( key + _T( "/" ) + F_GEOMETRY, f_geometry );
    cfg->Write( key + _T( "/" ) + F_VISUALIZATION, f_visualization );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::StoreRecentFile()
{
    //store recent menus in config
    wxConfig* cfg = static_cast<wxConfig*>( wxConfig::Get() );
    if( !cfg )
    {
        Log( "There is no longer a wxConfig around" );
        return;
    }
    m_recentVESFiles->Save( *cfg );
    //Write out the Config file as it seems in some cases it is not being
    //being written during destruction of the frame.
    cfg->Flush();
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::FrameClose( wxCommandEvent& WXUNUSED( event ) )
{
    Close( true );
    serviceList->DisconnectFromCE();
}
////////////////////////////////////////////////////////////////////////////////
/*void AppFrame::OnFrameClose( wxCloseEvent& WXUNUSED( event ) )
{
    //Close Aspen no matter how conductor us shutdown
	CloseAspenSimulation();
    //Cleanup all the plugins before wx does
    mDestoryFrame = true;
    canvas->CleanUpNetworks();
}*/
////////////////////////////////////////////////////////////////////////////////
void AppFrame::CreateMenu()
{
    menubar = new wxMenuBar;
    file_menu = new wxMenu;
    con_menu = new wxMenu;
    run_menu = new wxMenu;
    edit_menu = new wxMenu;
    help_menu = new wxMenu;

    file_menu->Append( APPFRAME_NEW, _( "&New\tCtrl+N" ) );
    file_menu->Append( APPFRAME_OPEN, _( "&Open ..\tCtrl+O" ) );
    file_menu->Append( APPFRAME_RUN, _( "&Run ..\tCtrl+R" ) );

    file_menu->AppendSeparator();

    file_menu->Append( APPFRAME_SAVE, _( "&Save\tCtrl+S" ) );
    file_menu->Append( APPFRAME_SAVEAS, _( "Save &as ..\tCtrl+Shift+S" ) );

    file_menu->AppendSeparator();

    file_menu->Append( APPFRAME_START_DATALOGGING, _( "Start Data Logging" ) );
    file_menu->Append( APPFRAME_STOP_DATALOGGING, _( "Stop Data Logging" ) );

    file_menu->AppendSeparator();

    file_menu->Append( APPFRAME_EXPORT_MENU_OPT, _( "Export" ),
                       new ExportMenu(), _( "Options" ) );

    file_menu->AppendSeparator();
    file_menu->Append( APPFRAME_CHANGE_WORKING_DIRECTORY, _( "Change Working Directory" ) );
    file_menu->AppendSeparator();
    /*file_menu->Append( wxID_PRINT_SETUP, _( "Print Set&up .." ) );
    file_menu->Append( wxID_PREVIEW, _( "Print Pre&view\tCtrl+Shift+P" ) );
    file_menu->Append( wxID_PRINT, _( "&Print ..\tCtrl+P" ) );
    file_menu->Enable( wxID_PRINT_SETUP, false );
    file_menu->Enable( wxID_PREVIEW, false ); 
    file_menu->Enable( wxID_PRINT, false );
    file_menu->AppendSeparator();*/
    file_menu->Append( APPFRAME_PREFERENCES, _( "Preferences" ) );
    file_menu->AppendSeparator();
    file_menu->Append( APPFRAME_CLEAR_RECENT_FILES, _( "&Clear File History" ) );

    file_menu->AppendSeparator();
    file_menu->Append( wxID_EXIT, _( "&Quit\tCtrl+Q" ) );

    //con_menu->Append(V21ID_CONNECT, _("&Connect to Executive\tCtrl+C"));
    //con_menu->Append(V21ID_CONNECT_VE, _("Connect to VE"));
    //con_menu->AppendSeparator();
    con_menu->Append( APPFRAME_V21ID_SUBMIT, _( "Sub&mit Job\tCtrl+M" ) );
    con_menu->Append( APPFRAME_V21ID_LOAD, _( "&Load Job\tCtrl+L" ) );

    //con_menu->Append(QUERY_FROM_SERVER, _("&Query\tCtrl+U"));
    /*wxMenu * aspenMenu = new wxMenu();
    aspenMenu->Append( QUERY_NETWORK, _( "Open Simulation" ) );
    aspenMenu->Append( SHOW_ASPEN_SIMULATION, _( "Show Simulation" ) );
    aspenMenu->Append( HIDE_ASPEN_SIMULATION, _( "Hide Simulation" ) );
    aspenMenu->Append( CLOSE_ASPEN_SIMULATION, _( "Close Simulation" ) );
    aspenMenu->Append( RUN_ASPEN_NETWORK, _( "Run" ) );
	aspenMenu->Append( REINITIALIZE_ASPEN_SIMULATION, _( "Reinitialize" ) );
    aspenMenu->Append( STEP_ASPEN_NETWORK, _( "Step" ) );
    aspenMenu->Append( CONDUCTOR_FIND, _( "Find" ) );
    aspenMenu->Append( SAVE_SIMULATION, _( "Save Simulation" ) );
    aspenMenu->Append( SAVEAS_SIMULATION, _( "SaveAs Simulation" ) );
    con_menu->Append( ASPEN_CONNECTION_MENU,   _( "Aspen" ), aspenMenu, _( "Aspen connection" ) );
*/
    //file_menu->Append( OPEN_RECENT_CONNECTION_MENU, _("Open recent file"), aspenMenu, _("NOTHING") );


    con_menu->AppendSeparator();
    con_menu->Append( APPFRAME_V21ID_DISCONNECT, _( "&Disconnect\tCtrl+d" ) );
    con_menu->Append( APPFRAME_V21ID_DISCONNECT_VE, _( "&Disconnect VE" ) );

    //con_menu->Enable( V21ID_SUBMIT,false );
    //con_menu->Enable( V21ID_LOAD, false );
    con_menu->Enable( APPFRAME_V21ID_DISCONNECT, false );
    con_menu->Enable( APPFRAME_V21ID_DISCONNECT_VE, false );


    run_menu->Append( APPFRAME_V21ID_START_CALC, _( "Start Simulation" ) );
    run_menu->Append( APPFRAME_V21ID_STOP_CALC, _( "Stop Simulation" ) );
    run_menu->Append( APPFRAME_V21ID_PAUSE_CALC, _( "Pause Simulation" ) );
    run_menu->Append( APPFRAME_V21ID_RESUME_CALC, _( "Resume Simulation" ) );
    run_menu->Append( APPFRAME_V21ID_VIEW_RESULT, _( "View Results" ) );
    // run_menu->Append( V21ID_GLOBAL_PARAM, _("Global Parameters"));
    // run_menu->Append( V21ID_VIEW_FINANCIAL, _("View Financial Params"));

    run_menu->Enable( APPFRAME_V21ID_START_CALC, false );
    run_menu->Enable( APPFRAME_V21ID_STOP_CALC, false );
    run_menu->Enable( APPFRAME_V21ID_PAUSE_CALC, false );
    run_menu->Enable( APPFRAME_V21ID_RESUME_CALC, false );
    // EPRI TAG run_menu->Enable(V21ID_VIEW_RESULT, false);

    //edit_menu->Append(V21ID_UNDO, _("&Undo\tCtrl+U"));
    //edit_menu->Append(V21ID_REDO, _("&Redo\tCtrl+R"));
    //edit_menu->AppendSeparator();
    edit_menu->Append( APPFRAME_V21ID_ZOOMIN, _( "Zoom &In" ) );
    edit_menu->Append( APPFRAME_V21ID_ZOOMOUT, _( "Zoom &Out" ) );
    edit_menu->Append( APPFRAME_V21ID_ZOOMALL, _( "Zoom &All" ) );
    edit_menu->Append( APPFRAME_CONDUCTOR_FIND, _( "Find" ) );
    //This is needed because on windows the scale must be 1 for the
    //wxAutoBufferedPaintDC to work properly
//#ifdef WIN32
//    edit_menu->Enable( V21ID_ZOOMIN, false );
//    edit_menu->Enable( V21ID_ZOOMOUT, false );
//#endif
    //edit_menu->Enable(V21ID_UNDO, false);
    //edit_menu->Enable(V21ID_REDO, false);

    //help_menu->Append(wxID_HELP_CONTENTS, _("&Content\tF1"));
    help_menu->Append( APPFRAME_V21ID_HELP, _( "&VE-Suite Help" ) );
    help_menu->Append( APPFRAME_V21ID_ABOUT, _( "&About" ) );
    //help_menu->Append( APPFRAME_V21ID_REVISION, _( "&Revision" ) );
    help_menu->Append( APPFRAME_V21ID_CONTACTS, _( "&Contacts" ) );
    help_menu->Append( APPFRAME_V21ID_PLATFORM, _( "&Platform Info" ) );
    //help_menu->AppendSeparator();
    //help_menu->Append (wxID_ABOUT, _("&About ..\tShift+F1"));

    //help_menu->Enable(wxID_HELP_CONTENTS, false);
    //help_menu->Enable(V21ID_HELP, false);
    //help_menu->Enable(wxID_ABOUT, true);

    xplorerMenu = new wxMenu();
    xplorerDeviceMenu = new wxMenu();
    wxMenuItem* keyboardMouseMenuItem = new wxMenuItem(
        xplorerDeviceMenu, APPFRAME_KEYBOARD_MOUSE,
        wxT( "Keyboard/Mouse" ), wxT( "" ), wxITEM_CHECK );
    wxMenuItem* wandMenuItem = new wxMenuItem(
        xplorerDeviceMenu, APPFRAME_WAND,
        wxT( "Wand" ), wxT( "" ), wxITEM_CHECK );
    wxMenuItem* tabletMenuItem = new wxMenuItem(
        xplorerDeviceMenu, APPFRAME_TABLET,
        wxT( "Tablet" ), wxT( "" ), wxITEM_CHECK );
    wxMenuItem* glovesMenuItem = new wxMenuItem(
        xplorerDeviceMenu, APPFRAME_GLOVES,
        wxT( "Gloves" ), wxT( "" ), wxITEM_CHECK );
    
    xplorerJugglerMenu = new wxMenu();
    xplorerDisplayMenu = new wxMenu();
    xplorerViewMenu = new wxMenu();
    wxMenu* xplorerView = new wxMenu();

    xplorerDeviceMenu->Append( keyboardMouseMenuItem );
    xplorerDeviceMenu->Append( wandMenuItem );
    xplorerDeviceMenu->Append( tabletMenuItem );
    xplorerDeviceMenu->Append( glovesMenuItem );
    xplorerDeviceMenu->AppendSeparator();
    xplorerDeviceMenu->Append( APPFRAME_DEVICE_PROPERTIES, _( "Properties" ) );

    keyboardMouseMenuItem->Check();
    wandMenuItem->Check();
    tabletMenuItem->Check();
    glovesMenuItem->Check( false );

    xplorerDisplayMenu->AppendCheckItem( APPFRAME_FRAME_RATE,        _( "Frame Rate" ) );
    xplorerDisplayMenu->AppendCheckItem( APPFRAME_COORDINATE_SYSTEM, _( "Coord System" ) );

    xplorerViewMenu->Append( APPFRAME_FRAME_ALL,       _( "Frame All            [f]" ) );
    xplorerViewMenu->Append( APPFRAME_FRAME_SELECTION, _( "Frame Selection" ) );
    xplorerViewMenu->Append( APPFRAME_RESET,           _( "Reset                  [r]" ) );

    xplorerView->Append( APPFRAME_CHANGE_XPLORER_VIEW_NETWORK, _( "Network" ) );
    xplorerView->Append( APPFRAME_CHANGE_XPLORER_VIEW_CAD, _( "CAD" ) );
    xplorerView->Append( APPFRAME_CHANGE_XPLORER_VIEW_LOGO, _( "Logo" ) );

    xplorerJugglerMenu->Append( APPFRAME_JUGGLER_STEREO, _( "Stereo" ) );
    xplorerJugglerMenu->Append( APPFRAME_JUGGLER_MONO, _( "Mono" ) );
    xplorerJugglerMenu->Enable( APPFRAME_JUGGLER_STEREO, true );
    xplorerJugglerMenu->Enable( APPFRAME_JUGGLER_MONO, true );

    xplorerMenu->Append( APPFRAME_XPLORER_NAVIGATION, _( "Navigation Pane" ) );
    xplorerMenu->Append( APPFRAME_XPLORER_VIEWPOINTS, _( "Viewpoints Pane" ) );
    xplorerMenu->Append( APPFRAME_XPLORER_SCENES,     _( "Record Scenes" ) );
    xplorerMenu->Append( APPFRAME_XPLORER_COLOR,      _( "Background Color" ) );
    xplorerMenu->Append( APPFRAME_XPLORER_EPHEMERIS, _( "Ephemeris Data" ) );
    //xplorerMenu->Append( XPLORER_SOUNDS,     _("Sounds Pane") );
    //xplorerMenu->Append( XPLORER_STREAMLINE, _("Streamline Pane") );
    xplorerMenu->Append( APPFRAME_XPLORER_DEVICE,     _( "Devices" ),
                         xplorerDeviceMenu,  _( "Used to change device properties" ) );
    xplorerMenu->Append( APPFRAME_JUGGLER_SETTINGS,   _( "Juggler Settings" ),
                         xplorerJugglerMenu, _( "Used to adjust juggler runtime settings" ) );
    xplorerMenu->Append( APPFRAME_XPLORER_DISPLAY,    _( "Display" ),
                         xplorerDisplayMenu, _( "Used to change display preferences" ) );
    xplorerMenu->Append( APPFRAME_XPLORER_VIEW,       _( "View" ),
                         xplorerViewMenu,    _( "Used to change the view" ) );
    //add the view settings
    xplorerMenu->Append( APPFRAME_CHANGE_XPLORER_VIEW, _( "Graphical View" ),
                         xplorerView,        _( "Used to change the view in xplorer" ) );
    //If the display mode is desktop then we will disconnect when exit is selected
    //and in other modes we will give the user the ability to exit
    if (( GetDisplayMode() != "Desktop" ) &&
            ( preferences->GetMode( "Shut Down Xplorer Option" ) ) )
    {
        xplorerMenu->Append( APPFRAME_XPLORER_EXIT, _( "Shutdown Xplorer" ) );
        xplorerMenu->Enable( APPFRAME_XPLORER_EXIT, true );
    }

    wxMenu* xplorerDataLogginMenu = new wxMenu;
    xplorerMenu->AppendSubMenu ( xplorerDataLogginMenu, _("Data Logging") );
    xplorerDataLogginMenu->Append ( APPFRAME_XPLORER_DATALOGGING_LOOPING, _( "Toggle Looping" ) );
    //xplorerDataLogginMenu->Append ( APPFRAME_MINERVA_REMOVE_PLANET, _( "Remove Planet" ) );
    
#ifdef MINERVA_GIS_SUPPORT

    wxMenu* xplorerMinervaMenu = new wxMenu;
    xplorerMenu->AppendSubMenu ( xplorerMinervaMenu, _("GIS") );
    xplorerMinervaMenu->Append ( APPFRAME_MINERVA_ADD_PLANET, _( "Add Planet" ) );
    xplorerMinervaMenu->Append ( APPFRAME_MINERVA_REMOVE_PLANET, _( "Remove Planet" ) );
    xplorerMinervaMenu->Append ( APPFRAME_MINERVA_SHOW_DIALOG, _( "Configure Layers" ) );

#endif

    xplorerMenu->Enable( APPFRAME_XPLORER_NAVIGATION, true );
    xplorerMenu->Enable( APPFRAME_XPLORER_VIEWPOINTS, true );
    //xplorerMenu->Enable( XPLORER_SOUNDS, true);
    xplorerMenu->Enable( APPFRAME_XPLORER_SCENES, true );
    xplorerMenu->Enable( APPFRAME_JUGGLER_SETTINGS, true );

    menubar->Append( file_menu, _( "&File" ) );
    menubar->Append( edit_menu, _( "&Edit" ) );
    menubar->Append( con_menu, _( "&Connection" ) );
    menubar->Append( run_menu, _( "&Execution" ) );
    menubar->Append( xplorerMenu, _( "&VE-Xplorer" ) );
    menubar->Append( help_menu, _( "&Help" ) );

    SetMenuBar( menubar );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ZoomIn( wxCommandEvent& WXUNUSED( event ) )
{
    Network* network = canvas->GetActiveNetwork();

    //if( network->GetUserScale()->first > 4 )
    //{
    //    return; // maximum zoom in x3
    //}

    network->GetUserScale()->first += 0.1;
    network->GetUserScale()->second += 0.1;

    std::pair< int, int > networkSize = network->GetNetworkSize( );
    networkSize.first *= network->GetUserScale()->first;
    networkSize.second *= network->GetUserScale()->second;
    
    canvas->SetUserScale(network->GetUserScale()->first, 
        network->GetUserScale()->second  );
    canvas->SetVirtualSize( networkSize.first, networkSize.second );
    canvas->Refresh( true );
}

////////////////////////////////////////////////////////////////////////////////
void AppFrame::ZoomAll( wxCommandEvent& WXUNUSED( event ) )
{
    Network * network = canvas->GetActiveNetwork();

    float netWidth = network->GetNetworkSize().first;
    float netHeight = network->GetNetworkSize().second;
    float winWidth = canvas->GetClientSize().GetWidth();
    float winHeight = canvas->GetClientSize().GetHeight();
    float shrink = 1;

    //calculate which aspect needs scaled up the least
    //then uniformly scales the sheet according to that aspect
    float perWidth = winWidth / netWidth;
    float perHeight = winHeight / netHeight;
    if( perWidth > perHeight )
    {
        shrink = perHeight;
    }
    else
    {
        shrink = perWidth;
    }

    //limits the zoom all level to 1
    //if test removed it can full screen a single icon
    if( shrink < 1 )
    {
        network->GetUserScale()->first = shrink;
        network->GetUserScale()->second = shrink;
    }
    else
    {
        network->GetUserScale()->first = 1;
        network->GetUserScale()->second = 1;
    }
    
    std::pair< int, int > networkSize = network->GetNetworkSize( );
    networkSize.first *= network->GetUserScale()->first;
    networkSize.second *= network->GetUserScale()->second;

    canvas->SetUserScale(network->GetUserScale()->first, 
        network->GetUserScale()->second  );
    canvas->SetVirtualSize( networkSize.first, networkSize.second );
    canvas->Refresh( true );
}

////////////////////////////////////////////////////////////////////////////////
void AppFrame::ZoomOut( wxCommandEvent& WXUNUSED( event ) )
{
    Network * network = canvas->GetActiveNetwork();

    //if( network->GetUserScale()->first < 0.2 )
    //{
    //    return; //minimum x-5
    //}

    network->GetUserScale()->first -= 0.1;
    network->GetUserScale()->second -= 0.1;
    
    std::pair< int, int > networkSize = network->GetNetworkSize( );
    networkSize.first *= network->GetUserScale()->first;
    networkSize.second *= network->GetUserScale()->second;

    canvas->SetUserScale(network->GetUserScale()->first, 
        network->GetUserScale()->second  );
    canvas->SetVirtualSize( networkSize.first, networkSize.second );
    canvas->Refresh( true );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::Save( wxCommandEvent& event )
{
    //First time call save will be the same as SaveAs
    if( mVESFileName.IsEmpty() )
    {
        SaveAs( event );
    }
    else
    {
        canvas->Update();
        wxFileName vesFileName( mVESFileName );

        if( !wxFileName::IsFileWritable( vesFileName.GetFullPath() ) )
        {
            wxString tempMessage = 
                _( "Cannot write file " ) + 
                vesFileName.GetFullName() + _( "?" );
            wxMessageDialog promptDlg( this,
                tempMessage,
                _( "Overwrite File Warning" ),
                wxOK | wxICON_QUESTION,
                wxDefaultPosition );
            promptDlg.ShowModal();
            Log( "Unable to save ves file." );
            return;
        }

        ///now write the file out from domdocument manager
        //wrtie to path
        std::string nw_str = XMLDataBufferEngine::instance()->
            SaveVESData( ConvertUnicode( mVESFileName.c_str() ) );
        Log( "Finished updating ves file." );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::SaveAs( wxCommandEvent& WXUNUSED( event ) )
{
    //Update the canvas xml data first
    canvas->Update();

    wxFileName vesFileName;
    int answer = 0;
    do
    {
        //Set this here so that after correcting for not replacing the file name
        //we can exit the do while loop
        answer = 0;
        wxTextEntryDialog * newDataSetName;
        
        if( mVESFileName.IsEmpty() )
        {
            newDataSetName = new wxTextEntryDialog( this,
                _( "Enter the prefix for *.ves filename:" ),
                _( "Save VES file as..." ),
                _( "untitled" ), wxOK | wxCANCEL );
        }
        else
        {
            wxString fileNameNoExt = mVESFileName.Mid( 0, mVESFileName.size() - 4);
            newDataSetName = new wxTextEntryDialog( this,
                _( "Enter the prefix for *.ves filename:" ),
                _( "Save VES file as..." ),
                fileNameNoExt, wxOK | wxCANCEL );
        }

        if( newDataSetName->ShowModal() == wxID_OK )
        {
            vesFileName.ClearExt();
            vesFileName.SetName( newDataSetName->GetValue() );
            vesFileName.SetExt( _( "ves" ) );
        }
        else
        {
            break;
        }

        if( vesFileName.FileExists() )
        {
            wxString tempMessage = _( "Do you want to replace " ) + 
                vesFileName.GetFullName() + _( "?" );
            wxMessageDialog promptDlg( this,
                tempMessage,
                _( "Overwrite File Warning" ),
                wxYES_NO | wxNO_DEFAULT | wxICON_QUESTION,
                wxDefaultPosition );
            answer = promptDlg.ShowModal();
        }
        delete newDataSetName;
    }
    while( answer == wxID_NO );

    if( vesFileName.FileExists() && !wxFileName::IsFileWritable( vesFileName.GetFullPath() ) )
    {
        wxString tempMessage = _( "Cannot write file " ) + vesFileName.GetFullName() + _( "?" );
        wxMessageDialog promptDlg( this,
                                   tempMessage,
                                   _( "Overwrite File Warning" ),
                                   wxYES_NO | wxNO_DEFAULT | wxICON_QUESTION,
                                   wxDefaultPosition );
        promptDlg.ShowModal();
        return;
    }

    if( vesFileName.HasName() )
    {
        directory = vesFileName.GetPath();
        mVESFileName = vesFileName.GetFullName();
        ///now write the file out from domdocument manager
        std::string nw_str = XMLDataBufferEngine::instance()->SaveVESData( ConvertUnicode( mVESFileName.c_str() ) );
        SetTitle( vesFileName.GetFullName() );
        SetRecentFile( vesFileName );
    }
    Log( "Finished saving ves file." );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::Open( wxCommandEvent& WXUNUSED( event ) )
{
    wxFileDialog dialog
    (
        this,
        _T( "Open File" ),
        ::wxGetCwd(),
        mVESFileName,
        _T( "System files (*.ves)|*.ves|Script files (*.vem)|*.vem" ),
        wxOPEN | wxFILE_MUST_EXIST | wxFD_PREVIEW
    );

    if( dialog.ShowModal() != wxID_OK )
    {
        return;
    }

    wxFileName vesFileName( dialog.GetPath() );
    bool success = vesFileName.MakeRelativeTo( ::wxGetCwd() );
    if( !success )
    {
        wxMessageBox( _( "Can't open a VES file on another drive." ),
                     _( "VES File Read Error" ), wxOK | wxICON_INFORMATION );
        return;
    }

    directory = vesFileName.GetPath( wxPATH_GET_VOLUME, wxPATH_UNIX );
    //change conductor working dir
    ::wxSetWorkingDirectory( directory );
    directory.Replace( _( "\\" ), _( "/" ), true );
    std::string tempDir = ConvertUnicode( directory.c_str() );
    if( tempDir.empty() )
    {
        tempDir = "./";
    }
    
    //Send Command to change xplorer working dir
    // Create the command and data value pairs
    DataValuePairPtr dataValuePair( new DataValuePair( std::string( "STRING" ) ) );
    dataValuePair->SetData( "WORKING_DIRECTORY", tempDir );
    CommandPtr veCommand( new Command() );
    veCommand->SetCommandName( std::string( "Change Working Directory" ) );
    veCommand->AddDataValuePair( dataValuePair );
    serviceList->SendCommandStringToXplorer( veCommand );

    //Set the title displayed on the conductor dialog
    SetTitle( vesFileName.GetFullName() );
    //Update the recent file menu in conductor
    SetRecentFile( wxFileName( dialog.GetPath() ) );

    if( vesFileName.GetExt() == wxString( _("ves") ) )
    {
        //Update mVESFileName variable
        mVESFileName = dialog.GetFilename();

        //Clear the viewpoints data
        //Since the data is "managed" by Xplorer we need to notify
        //Xplorer when we load a new ves file to clear viewpoints since
        //they don't go with the new data.
        DataValuePairPtr dvp( new DataValuePair( std::string( "STRING" ) ) );
        dvp->SetData( "Clear Quat Data", tempDir );
        CommandPtr vec( new Command() );
        vec->SetCommandName( std::string( "QC_CLEAR_QUAT_DATA" ) );
        vec->AddDataValuePair( dvp );
        serviceList->SendCommandStringToXplorer( vec );
        
        //clear the old networks so that all the event handlers are removed
        //before cleaning up the rest of the classes
        canvas->New( true );
    }
    else if( vesFileName.GetExt() == wxString( _("vem") ) )
    {
        std::string vemFilename = ConvertUnicode( dialog.GetFilename().c_str() );

        ves::conductor::util::DataLoggerEngine::instance()->LoadVEMFile( vemFilename );
        ves::conductor::util::DataLoggerEngine::instance()->PlayVEMFile();
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::Run( wxCommandEvent& WXUNUSED( event ) )
{
    wxFileDialog dialog
    (
        this,
        wxT( "Open File dialog" ),
        wxT( "" ),
        wxT( "" ),
        wxT( "Exe files (*.exe)|*.exe" ),
        wxOPEN | wxFILE_MUST_EXIST | wxFD_PREVIEW
    );

    if( dialog.ShowModal() != wxID_OK )
    {
        return;
    }

    //::wxExecute( dialog.GetPath(), wxEXEC_ASYNC, process );
    wxProcess * process = wxProcess::Open( dialog.GetPath() );
    pids.push_back( process->GetPid() );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::SetRecentFile( wxFileName vesFileName )
{
    if( !vesFileName.IsAbsolute() )
    {
        vesFileName.MakeAbsolute( ::wxFileName::GetCwd() );
    }
    
    size_t numFilesInHistory = m_recentVESFiles->GetCount();
    for( size_t i = 0; i < numFilesInHistory; ++i )
    {
        if( !m_recentVESFiles->GetHistoryFile( i ).Cmp( vesFileName.GetFullPath() ) )
        {
            //If it is on the list remove it and add it back as 1
            m_recentVESFiles->RemoveFileFromHistory( i );
            break;
        }
    }
    m_recentVESFiles->AddFileToHistory( vesFileName.GetFullPath() );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OpenRecentFile( wxCommandEvent& event )
{
    wxString fileToOpen( m_recentVESFiles->GetHistoryFile( event.GetId() - wxID_FILE1 ) );
    if( fileToOpen.empty() || ( !wxFileName::FileExists( fileToOpen ) ) )
    {
        wxString message( _( "VES file " ) );
        message += fileToOpen;
        message += wxString( _( " does not exist!" ) );
        wxMessageBox( message,
                      _( "VES File Read Error" ), wxOK | wxICON_INFORMATION );
        m_recentVESFiles->RemoveFileFromHistory( event.GetId() - wxID_FILE1 );
        return;
    }

    int placeChosen = event.GetId();
    wxFileName vesFileName( fileToOpen );
    bool success = vesFileName.MakeRelativeTo( ::wxGetCwd() );
    if( !success )
    {
        wxMessageBox( _( "Can't open a VES file on another drive." ),
                      _( "VES File Read Error" ), wxOK | wxICON_INFORMATION );
        return;
    }

    directory = vesFileName.GetPath( wxPATH_GET_VOLUME, wxPATH_UNIX );
    //change conductor working dir
    ::wxSetWorkingDirectory( directory );
    directory.Replace( _( "\\" ), _( "/" ), true );

    std::string tempDir = ConvertUnicode( directory.c_str() );
    if( tempDir.empty() )
    {
        tempDir = "./";
    }

    //Send Command to change xplorer working dir
    // Create the command and data value pairs
    DataValuePairPtr dataValuePair( new DataValuePair( std::string( "STRING" ) ) );
    dataValuePair->SetData( "WORKING_DIRECTORY", tempDir );
    CommandPtr veCommand( new Command() );
    veCommand->SetCommandName( std::string( "Change Working Directory" ) );
    veCommand->AddDataValuePair( dataValuePair );
    serviceList->SendCommandStringToXplorer( veCommand );

    //Update recent file history and make it number 1
    SetRecentFile( fileToOpen );

    SetTitle( vesFileName.GetFullName() );

    if( vesFileName.GetExt() == wxString( _("ves") ) )
    {
        mVESFileName = vesFileName.GetFullName();

        DataValuePairPtr dvp( new DataValuePair( std::string( "STRING" ) ) );
        dvp->SetData( "Clear Quat Data", tempDir );
        CommandPtr vec( new Command() );
        vec->SetCommandName( std::string( "QC_CLEAR_QUAT_DATA" ) );
        vec->AddDataValuePair( dvp );
        serviceList->SendCommandStringToXplorer( vec );

        //clear the old networks so that all the event handlers are removed
        //before cleaning up the rest of the classes
        canvas->New( true );
    }
    else if( vesFileName.GetExt() == wxString( _("vem") ) )
    {
        std::string vemFilename = ConvertUnicode( vesFileName.GetFullName().c_str() );
        ves::conductor::util::DataLoggerEngine::instance()->LoadVEMFile( vemFilename );
        ves::conductor::util::DataLoggerEngine::instance()->PlayVEMFile();
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OnClearRecentFiles( wxCommandEvent& event )
{
    wxMessageDialog confirm( this,
                             _( "Are you sure you want to clear the recent files list?" ),
                             _( "Confirm" ), wxOK | wxCANCEL );
    if( confirm.ShowModal() == wxID_OK )
    {
        size_t numFilesInHistory = m_recentVESFiles->GetCount();
        for( size_t i = 0; i < numFilesInHistory; ++i )
        {
            m_recentVESFiles->RemoveFileFromHistory(( numFilesInHistory - 1 ) - i );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::LoadFromServer( wxCommandEvent& WXUNUSED( event ) )
{
    std::string nw_str = serviceList->GetNetwork();
    EnableCEGUIMenuItems();
    canvas->PopulateNetworks( nw_str, false );

    //create hierarchy page
    hierarchyTree->PopulateTree();
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::QueryFromServer( wxCommandEvent& WXUNUSED( event ) )
{
    EnableCEGUIMenuItems();

    std::string nw_str;

    try
    {
        nw_str.assign( serviceList->Query( 0 ) );
    }
    catch ( CORBA::Exception& )
    {
        Log( "No ves network available." );
    }

    // If there is nothing on the CE
    if( !nw_str.empty() )
    {
        canvas->PopulateNetworks( nw_str );

        //create hierarchy page
        hierarchyTree->PopulateTree(); 
        ///Submit job to xplorer
        wxCommandEvent event;
        SubmitToServer( event );
    }
    else
    {
        Log( "No ves network available." );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OpenSimulation( wxString simName )
{
    wxFileName bkpFileName;
    bkpFileName.SetName( simName );

    CommandPtr returnState ( new Command() );
    returnState->SetCommandName( "openSimulation" );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "AspenPlus", "openSimulation" );
    returnState->AddDataValuePair( data );

    data = DataValuePairPtr( new DataValuePair() );
    data->SetData( "BKPFileName",  ConvertUnicode( bkpFileName.GetFullName().c_str() ) );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );
    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    //Get results
    std::string nw_str = serviceList->Query( status );
    Log( nw_str.c_str() );
	AspenSimOpen = true;
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::FindBlocks( wxCommandEvent& WXUNUSED( event ) )
{
    Network* network = canvas->GetActiveNetwork();

    FindDialog fd( this );

    std::vector< std::string > moduleNames;
    std::vector< unsigned int > moduleIDs;

    //Find for modules
    //alphabetize map
    //std::map< std::string, unsigned int > alphaMap;
    std::multimap< std::string, unsigned int > alphaMap;
    for( std::map<unsigned int, Module>::iterator iter = network->modules.begin();
        iter != network->modules.end(); ++iter )
    {
        if( iter->second.GetPlugin()->GetNameFlag() )
        {
            //alphaMap[ConvertUnicode( iter->second.GetPlugin()->GetName().c_str() )]
            //    = iter->second.GetPlugin()->GetVEModel()->GetModelID();
            alphaMap.insert( std::pair<std::string, unsigned int>
                ( ConvertUnicode( iter->second.GetPlugin()->GetName().c_str() ),
               iter->second.GetPlugin()->GetVEModel()->GetModelID() ) );
        }
    }

    for( std::multimap< std::string, unsigned int >::iterator
            iter = alphaMap.begin(); iter != alphaMap.end(); ++iter )
    {
        moduleNames.push_back( iter->first );
        moduleIDs.push_back( iter->second );
    }

    std::vector< std::string > streamNames;
    std::vector< int > streamIDs;

    //Find for streams
    //alphabetize map
    std::multimap< std::string, int > alphaMapStreams;
    for( int i = 0; i < network->links.size(); i++ )
    {

        alphaMapStreams.insert(std::pair< std::string, int >
            ( ConvertUnicode( network->links[i].GetName().c_str() ), i ) );
    }

    for( std::multimap< std::string, int >::iterator
            iter = alphaMapStreams.begin(); iter != alphaMapStreams.end(); ++iter )
    {
        streamNames.push_back( iter->first );
        streamIDs.push_back( iter->second );
    }

    fd.SetStreamList( streamNames );
    fd.SetModuleList( moduleNames );
    fd.ShowModal();

    std::pair< int, int > selectedModulePos = fd.GetSelectedModulePos();

    //highlight and center block
    if( selectedModulePos.first != wxNOT_FOUND ||
        selectedModulePos.second != wxNOT_FOUND )
    {
        //std::string selectModuleName = "Find Failed!";
        if( selectedModulePos.first == 0)
        {
            network->
                HighlightCenter( moduleIDs[selectedModulePos.second] );
            //selectModuleName = "\nFind Block: " +
            //    std::string( fd.GetSelectedModule() ) + "\n";
        }
        else
        {
            network->
                HighlightCenterLink( streamIDs[selectedModulePos.second] );
            //selectModuleName = "\nFind Link: " +
            //    std::string( fd.GetSelectedModule() ) + "\n";
        }
        //Log( selectModuleName.c_str() );
    }
}
///////////////////////////////////////////////////////////////////////////
void AppFrame::NewCanvas( wxCommandEvent& WXUNUSED( event ) )
{

    wxMessageDialog md( this, 
    wxT( "Would you like to clear VE-Suite?" ),
    wxT( "Confirm" ),
    wxYES_NO);
    if( md.ShowModal() == wxID_NO )
    {
        return;
    }

	newCanvas = true;
    mVESFileName.Clear();

    {
        //Reset Xplorer logo
        DataValuePairPtr 
            dataValuePair( new DataValuePair( std::string( "STRING" ) ) );
        CommandPtr veCommand( new Command() );
        veCommand->SetCommandName( std::string( "CHANGE_XPLORER_VIEW" ) );
        dataValuePair->
            SetData( "CHANGE_XPLORER_VIEW", "CHANGE_XPLORER_VIEW_LOGO" );
        veCommand->AddDataValuePair( dataValuePair );
        serviceList->SendCommandStringToXplorer( veCommand );
    }

    {
        //color
        std::vector< double > xplorerColor;
        xplorerColor.push_back( 0.0 );
        xplorerColor.push_back( 0.0 );
        xplorerColor.push_back( 0.0 );
        xplorerColor.push_back( 1.0 );
        
        // Create the command and data value pairs
        DataValuePairPtr 
            dataValuePair2( new DataValuePair( std::string( "STRING" ) ) );
        dataValuePair2->
            SetData( std::string( "Background Color" ), xplorerColor );
        CommandPtr veCommand2( new Command() );
        veCommand2->SetCommandName( std::string( "CHANGE_BACKGROUND_COLOR" ) );
        veCommand2->AddDataValuePair( dataValuePair2 );
        serviceList->SendCommandStringToXplorer( veCommand2 );
        UserPreferencesDataBuffer::instance()->
            SetCommand( "CHANGE_BACKGROUND_COLOR", veCommand2 );
    }

    //clear the old networks so that all the event handlers are removed
    //before cleaning up the rest of the classes
    canvas->New( true );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::SubmitToServer( wxCommandEvent& WXUNUSED( event ) )
{
    EnableCEGUIMenuItems();

    canvas->Update();

    const std::string nw_str = XMLDataBufferEngine::instance()->
                         SaveVESData( std::string( "returnString" ) );
    // write the domdoc to the string above
    try
    {
        //first make sure all the units have been initialized with the current
        //ids to get an active xml model
        Network* network = canvas->GetActiveNetwork();
        network->SetIDOnAllActiveModules();
        //Now that we have an active xml model in all units
        // set the network
        serviceList->SetNetwork( nw_str );
        // Tell xplorer to ask ce for the new data
        DataValuePairPtr dataValuePair( new DataValuePair() );
        dataValuePair->SetData( std::string( "Load Data" ), xplorerColor );
        CommandPtr veCommand( new Command() );
        veCommand->SetCommandName( std::string( "veNetwork Update" ) );
        veCommand->AddDataValuePair( dataValuePair );
        serviceList->SendCommandStringToXplorer( veCommand );
        //enable the menus now
        run_menu->Enable( APPFRAME_V21ID_START_CALC, true );
        //if there is only one module make it active right away
        if( network->modules.size() == 1 )
        {
            network->modules.begin()->second.GetPlugin()->SetActiveModel();
        }
        
    }
    catch ( CORBA::Exception& )
    {
        Log( "No exec found!" );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::StartCalc( wxCommandEvent& WXUNUSED( event ) )
{
    try
    {
        serviceList->StartCalc();
    }
    catch ( CORBA::Exception& )
    {
        Log( "no exec found!" );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::StopCalc( wxCommandEvent& WXUNUSED( event ) )
{
    try
    {
        serviceList->StopCalc();
    }
    catch ( CORBA::Exception& )
    {
        Log( "no exec found!" );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::PauseCalc( wxCommandEvent& WXUNUSED( event ) )
{
    try
    {
        serviceList->PauseCalc();
    }
    catch ( CORBA::Exception& )
    {
        Log( "no exec found!" );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ResumeCalc( wxCommandEvent& WXUNUSED( event ) )
{
    try
    {
        serviceList->Resume();
    }
    catch ( CORBA::Exception& )
    {
        Log( "no exec found!" );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ViewResult( wxCommandEvent& WXUNUSED( event ) )
{
    Network* network = canvas->GetActiveNetwork();

    serviceList->IsConnectedToCE();
    EnableCEGUIMenuItems();

    //This code has not been tested in ages and needs to be rewritten
    //and moved to its own class. Until this is done this will be
    //commented out.
    return;
    
    char* result = 0;
    //char buf[80];
    std::map<unsigned int, Module>::iterator iter;
    std::vector<wxString> titles;
    //TextResultDialog * result_dlg;
    SummaryResultDialog * result_dlg = 0;
    std::vector<wxString> v_desc, v_value;
    std::vector<std::string> descs;
    std::vector<int> alignments;

    titles.push_back( _( "Description" ) );
    alignments.push_back( wxALIGN_LEFT );
    titles.push_back( _( "Value" ) );
    alignments.push_back( wxALIGN_RIGHT );

    /*
    result_dlg = new TextResultDialog(NULL);
    result_dlg->syngas->Clear();
    result_dlg->syngas->AddRow(titles);
    */
    if( result_dlg )
    {
        result_dlg->Destroy();
        result_dlg = 0;
    }

    //how results are viewed has changed to be on a per model basis
    //minor updates are needed to get this working for an entire network
    /*
    result_dlg = new SummaryResultDialog( NULL, wxT( "Result Summary - All Modules" ), wxSize( 560, 400 ) );
    result_dlg->syngas->Clear();
    result_dlg->syngas->SetNumofCols( 2 );
    result_dlg->syngas->SetColTitles( titles );
    result_dlg->syngas->SetColAlignments( alignments );
    */

    try
    {
        for( iter = network->modules.begin(); iter != network->modules.end(); iter++ )
        {
            //result = network->exec->GetModuleResult(iter->first);

            if( std::string( result ) != "" )
            {
                /* Package p;
                p.SetSysId("linkresult.xml");
                p.Load(result, strlen(result));

                descs = p.GetInterfaceVector()[0].getStrings();

                // This may not be needed.
                // this should be taken care of with previous statement
                if(descs.size() < 1 ) 
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

        if( std::string( result ) != "" )
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
    catch ( CORBA::Exception & )
    {
        std::cerr << "Maybe Computational Engine is down " << std::endl;
        return;
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
    for( iter = network->modules.begin(); iter != network->modules.end(); iter++ )
    {
        if( iter->second.GetPlugin()->financial_dlg != NULL )
        {
            if( iter->second.GetPlugin()->financial_dlg->_use_data )
            {
                double TPC = iter->second.GetPlugin()->financial_dlg->_cc00_d *
                             ( 1 +
                               iter->second.GetPlugin()->financial_dlg->_cc01_d / 100 +
                               iter->second.GetPlugin()->financial_dlg->_cc02_d / 100 +
                               iter->second.GetPlugin()->financial_dlg->_cc03_d / 100 +
                               iter->second.GetPlugin()->financial_dlg->_cc04_d / 100 +
                               iter->second.GetPlugin()->financial_dlg->_cc05_d / 100 );

                double TPI = TPC + iter->second.GetPlugin()->financial_dlg->_cc06_d;

                double cccost = TPI + iter->second.GetPlugin()->financial_dlg->_cc07_d +
                                iter->second.GetPlugin()->financial_dlg->_cc08_d;

                total_cccost += cccost;

                v_desc.push_back( wxString( iter->second.GetPlugin()->GetName() ) );
                std::ostringstream dirStringStream;
                dirStringStream << std::setprecision( 2 ) << cccost;
                std::string dirString = dirStringStream.str();

                v_value.push_back( wxString( dirString.c_str(), wxConvUTF8 ) );

                double TMC = TPC * iter->second.GetPlugin()->financial_dlg->_om00_d / 100;

                double omcost = TMC +
                                iter->second.GetPlugin()->financial_dlg->_om01_d +
                                iter->second.GetPlugin()->financial_dlg->_om02_d +
                                iter->second.GetPlugin()->financial_dlg->_om02_d * iter->second.GetPlugin()->financial_dlg->_om03_d / 100;

                total_omcost += omcost;

                v_desc2.push_back( wxString( iter->second.GetPlugin()->GetName() ) );
                dirStringStream << std::setprecision( 2 ) << omcost;
                v_value2.push_back( wxString( dirString.c_str(), wxConvUTF8 ) );
            }
        }
    }

    if( v_desc.size() > 0 )
    {
        result_dlg->syngas->AddSeperator( ' ' );
        result_dlg->syngas->AddSeperator( ' ' );

        titles.clear();
        titles.push_back( _( "Plant Component" ) );
        titles.push_back( _( "Capital Required (M$)" ) );

        result_dlg->syngas->AddRow( titles );
        result_dlg->syngas->AddSeperator( '+' );

        v_desc.push_back( _( "Total" ) );
        std::ostringstream dirStringStream;
        dirStringStream << std::setprecision( 2 ) << total_cccost;
        std::string dirString = dirStringStream.str();
        v_value.push_back( wxString( dirString.c_str(), wxConvUTF8 ) );

        result_dlg->Set2Cols( v_desc, v_value );

        //

        result_dlg->syngas->AddSeperator( ' ' );

        titles.clear();
        titles.push_back( _( "Plant Component" ) );
        titles.push_back( _( "Revenue Required (M$)" ) );

        result_dlg->syngas->AddRow( titles );
        result_dlg->syngas->AddSeperator( '+' );

        v_desc2.push_back( _( "Total" ) );
        dirStringStream << std::setprecision( 2 ) << total_omcost;
        v_value2.push_back( wxString( dirString.c_str(), wxConvUTF8 ) );

        result_dlg->Set2Cols( v_desc2, v_value2 );
    }

    result_dlg->Show();
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::Log( const char* msg )
{
    ::wxLogMessage(  wxString( msg, wxConvUTF8 ) );
    //serviceList->GetMessageLog()->SetMessage( msg );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::DisConExeServer( wxCommandEvent &WXUNUSED( event ) )
{
    serviceList->DisconnectFromCE();
    //con_menu->Enable(V21ID_SUBMIT,false);
    //con_menu->Enable(V21ID_LOAD, false);
    //con_menu->Enable(V21ID_CONNECT, true);
    run_menu->Enable( APPFRAME_V21ID_START_CALC, false );
    // EPRI TAG run_menu->Enable(V21ID_VIEW_RESULT, false);
    con_menu->Enable( APPFRAME_V21ID_DISCONNECT, false );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::DisConVEServer( wxCommandEvent &WXUNUSED( event ) )
{
    serviceList->DisconnectFromXplorer();
    con_menu->Enable( APPFRAME_V21ID_DISCONNECT_VE, false );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ViewHelp( wxCommandEvent& WXUNUSED( event ) )
{
    ::wxLaunchDefaultBrowser(
        wxString( "http://vesuite.org/content.php?id=421", wxConvUTF8 ) );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ViewAbout( wxCommandEvent& WXUNUSED( event ) )
{
    std::ostringstream revNum;
    revNum << VES_MAJOR_VERSION << "."
        << VES_MINOR_VERSION << "."
        << VES_PATCH_VERSION << "."
        << SVN_VES_REVISION;
    
    wxString tempNum = wxString( "Current Revision: ", wxConvUTF8 ) +
        wxString( revNum.str().c_str(), wxConvUTF8 );
    
    wxAboutDialogInfo info;
    info.SetName( _("VE-Conductor") );
    info.SetVersion( tempNum );
    info.SetDescription( _("VE-Conductor is the user interface to VE-Suite") );
    //info.SetCopyright(_T("(C) 2007 Me <my@email.addre.ss>"));
    info.SetWebSite( _("www.vesuite.org"), _("VE-Suite") );
    info.SetIcon( ve_icon32x32_xpm );
    //info.SetArtists( wxArrayString() );
    //info.SetDocWriters( wxArrayString() );
    //info.SetDevelopers( wxArrayString() );

    ::wxAboutBox(info);
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ViewContacts( wxCommandEvent& WXUNUSED( event ) )
{
    ::wxLaunchDefaultBrowser( wxString( "http://www.vesuite.org/forum/index.php", wxConvUTF8 ) );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ViewPlatformInfo( wxCommandEvent& WXUNUSED( event ) )
{
    wxMessageBox( ::wxGetOsDescription(), _( "Platform Info" ),
                  wxOK | wxICON_INFORMATION );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::LaunchDeviceProperties( wxCommandEvent& WXUNUSED( event ) )
{
    if( deviceProperties == 0 )
    {
        // create pane and set appropriate vars
        deviceProperties = new DeviceProperties( this );
    }
    // now show it
    deviceProperties->Show();
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::LaunchNavigationPane( wxCommandEvent& WXUNUSED( event ) )
{
    if( navPane == 0 )
    {
        // create pane and set appropriate vars
        navPane = new NavigationPane( this );
        navPane->Connect( wxEVT_DESTROY, 
            wxWindowDestroyEventHandler(AppFrame::OnChildDestroy), NULL, this );
    }
    // now show it
    navPane->Show();
}
//////////////////////////////////////////////////////////////////
void AppFrame::SetEphemerisData( wxCommandEvent& WXUNUSED( event ) )
{
    if( !m_ephemeris )
    {
        m_ephemeris = new EphemerisDialog( this, wxID_ANY );
    }

    if( m_ephemeris->Show())
    {}
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::SetBackgroundColor( wxCommandEvent& WXUNUSED( event ) )
{
    //this is kinda confusing...thanks wx!!!
    //wxColourData data;
    //data.SetChooseFull(true);
    CommandPtr bkColor = UserPreferencesDataBuffer::instance()->
                             GetCommand( "CHANGE_BACKGROUND_COLOR" );

    if( bkColor->GetCommandName() != "NULL" )
    {
        bkColor->GetDataValuePair( "Background Color" )->GetData( xplorerColor );
    }

    xplorerWxColor->GetColour().Set(
        static_cast<unsigned char>( xplorerColor.at( 0 ) ),
        static_cast<unsigned char>( xplorerColor.at( 1 ) ),
        static_cast<unsigned char>( xplorerColor.at( 2 ) ),
        static_cast<unsigned char>( xplorerColor.at( 3 ) )
    );

    wxColourDialog colorDlg( this, xplorerWxColor );
    colorDlg.SetTitle( wxString( "Xplorer Background Color", wxConvUTF8 ) );

    if( colorDlg.ShowModal() == wxID_OK )
    {
        *xplorerWxColor = colorDlg.GetColourData();
        wxColour col = xplorerWxColor->GetColour();

        xplorerColor.clear();
        xplorerColor.push_back( static_cast<double>( col.Red() ) / 255.0 );
        xplorerColor.push_back( static_cast<double>( col.Green() ) / 255.0 );
        xplorerColor.push_back( static_cast<double>( col.Blue() ) / 255.0 );
        xplorerColor.push_back( 1.0 );

        // Create the command and data value pairs
        DataValuePairPtr dataValuePair( new DataValuePair() );
        dataValuePair->SetData( std::string( "Background Color" ), xplorerColor );
        CommandPtr veCommand( new Command() );
        veCommand->SetCommandName( std::string( "CHANGE_BACKGROUND_COLOR" ) );
        veCommand->AddDataValuePair( dataValuePair );

        serviceList->SendCommandStringToXplorer( veCommand );

        UserPreferencesDataBuffer::instance()->SetCommand( "CHANGE_BACKGROUND_COLOR", veCommand );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ChangeDevice( wxCommandEvent& event )
{
    int deviceMenuItem = event.GetId();
    
    std::vector< long > data;
    switch( deviceMenuItem )
    {
    case APPFRAME_GLOVES:
    {
        data.push_back( 1 );

        break;
    }
    case APPFRAME_KEYBOARD_MOUSE:
    {
        data.push_back( 2 );

        break;
    }
    case APPFRAME_TABLET:
    {
        data.push_back( 3 );

        break;
    }
    case APPFRAME_WAND:
    {
        data.push_back( 4 );

        break;
    }
    default:
    {
        //Error output
        return;
    }
    } //end switch( event.GetId() )

    if( xplorerDeviceMenu->IsChecked( deviceMenuItem ) )
    {
        data.push_back( 1 );
    }
    else
    {
        data.push_back( 0 );
    }
    
    //Create the command and data value pairs
    DataValuePairPtr dvp( new DataValuePair() );
    dvp->SetData( "EnableDeviceData", data );

    CommandPtr command( new Command() );
    command->SetCommandName( "ENABLE_DEVICE" );
    command->AddDataValuePair( dvp );

    serviceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::DisplaySelection( wxCommandEvent& event )
{
    //Create the command and data value pairs
    DataValuePairPtr DVP( new DataValuePair() );
    CommandPtr command( new Command() );

    unsigned int value;

    if( event.GetId() == APPFRAME_FRAME_RATE )
    {
        value = xplorerDisplayMenu->IsChecked( APPFRAME_FRAME_RATE );
        DVP->SetData( std::string( "FrameRateID" ), value );
    }

    else if( event.GetId() == APPFRAME_COORDINATE_SYSTEM )
    {
        value = xplorerDisplayMenu->IsChecked( APPFRAME_COORDINATE_SYSTEM );
        DVP->SetData( std::string( "CoordSysID" ), value );
    }

    command->SetCommandName( std::string( "DISPLAY_SELECTION" ) );
    command->AddDataValuePair( DVP );

    serviceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ViewSelection( wxCommandEvent& event )
{
    //Create the command and data value pairs
    DataValuePairPtr dvp( new DataValuePair() );
    CommandPtr command( new Command() );

    std::string view;

    if( event.GetId() == APPFRAME_FRAME_ALL )
    {
        view = "Frame All";
    }
    else if( event.GetId() == APPFRAME_FRAME_SELECTION )
    {
        view = "Frame Selection";
    }
    else if( event.GetId() == APPFRAME_RESET )
    {
        view = "Reset";
    }

    dvp->SetData( std::string( "View" ), view );

    command->SetCommandName( std::string( "VIEW_SELECTION" ) );
    command->AddDataValuePair( dvp );

    serviceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::LaunchRecordScenes( wxCommandEvent& WXUNUSED( event ) )
{
    if( recordScenes == 0 )
    {
        // create pane and set appropriate vars
        recordScenes = new UITeacherTab( this );
    }

    // now show it
    recordScenes->Show();
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::LaunchViewpointsPane( wxCommandEvent& WXUNUSED( event ) )
{
    if( viewlocPane == 0 )
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
    DataValuePairPtr dataValuePair( new DataValuePair( std::string( "FLOAT" ) ) );
    dataValuePair->SetDataName( "Stereo" );
    if( event.GetId() == APPFRAME_JUGGLER_STEREO )
    {
        dataValuePair->SetDataValue( 1.0 );
    }
    else
    {
        dataValuePair->SetDataValue( 0.0 );
    }
    CommandPtr veCommand( new Command() );
    veCommand->SetCommandName( std::string( "Juggler_Display_Data" ) );
    veCommand->AddDataValuePair( dataValuePair );

    serviceList->SendCommandStringToXplorer( veCommand );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ExitXplorer( void )
{
    DataValuePairPtr dataValuePair( new DataValuePair( std::string( "STRING" ) ) );
    dataValuePair->SetData( "EXIT_FLAG", "EXIT" );
    CommandPtr veCommand( new Command() );
    veCommand->SetCommandName( std::string( "EXIT_XPLORER" ) );
    veCommand->AddDataValuePair( dataValuePair );

    serviceList->SendCommandStringToXplorer( veCommand );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OnExitXplorer( wxCommandEvent& WXUNUSED( event ) )
{
    ExitXplorer();
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ProcessCommandLineArgs( void )
{
    int argc = ::wxGetApp().argc;
    //char** argv = new char*[ argc ];
    std::string vesFile;
    for( int i = 0; i < argc; ++ i )
    {
        if( ( ConvertUnicode( ::wxGetApp().argv[ i ] ) == "-VESFile" ) &&
                (( i + 1 ) < argc )
           )
        {
            Log( std::string( std::string( "Loading VES file: " ) + 
                ConvertUnicode( ::wxGetApp().argv[ i + 1 ] ) ).c_str() );
            //Log( "\n" );
            vesFile.assign( ConvertUnicode( ::wxGetApp().argv[ i + 1 ] ) );
            break;
        }
    }

    if( vesFile.empty() )
    {
        return;
    }

    wxFileName vesFileName( wxString( vesFile.c_str(), wxConvUTF8 ) );
    bool success = vesFileName.MakeRelativeTo( ::wxGetCwd() );
    if( !success )
    {
        wxMessageBox( _( "Can't open a VES file on another drive." ),
                      _( "VES File Read Error" ), wxOK | wxICON_INFORMATION );
        return;
    }

    directory = vesFileName.GetPath();
    //change conductor working dir
    ::wxSetWorkingDirectory( directory );

    //Send Command to change xplorer working dir
    // Create the command and data value pairs
    DataValuePairPtr dataValuePair( 
        new DataValuePair( std::string( "STRING" ) ) );
    dataValuePair->SetData( "WORKING_DIRECTORY", 
        ConvertUnicode( directory.c_str() ) );
    CommandPtr veCommand( new Command() );
    veCommand->SetCommandName( std::string( "Change Working Directory" ) );
    veCommand->AddDataValuePair( dataValuePair );
    serviceList->SendCommandStringToXplorer( veCommand );

    //Now laod the xml data now that we are in the correct directory
    mVESFileName = vesFileName.GetFullName();
    // we submit after new to make sure that the ce and ge ar cleared
    wxCommandEvent event;
    //network->Load( ConvertUnicode( mVESFileName.c_str() ), true );
    canvas->PopulateNetworks( ConvertUnicode( mVESFileName.c_str() ) );
    //create hierarchy page
    hierarchyTree->PopulateTree(); 
    //XMLDataBufferEngine::instance()->GetTopSystemId() );
    // we submit after load to give ce and ge the new network
    SubmitToServer( event );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::EnableCEGUIMenuItems( void )
{
    if( !serviceList->IsConnectedToCE() )
    {
        return;
    }

    con_menu->Enable( APPFRAME_V21ID_SUBMIT, true );
    con_menu->Enable( APPFRAME_V21ID_LOAD, true );
    //frame_->con_menu->Enable(V21ID_CONNECT, false);
    run_menu->Enable( APPFRAME_V21ID_VIEW_RESULT, false );
    con_menu->Enable( APPFRAME_V21ID_DISCONNECT, true );
}
////////////////////////////////////////////////////////////////////////////////
UserPreferences* AppFrame::GetUserPreferences( void )
{
    return preferences;
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OnPreferences( wxCommandEvent& WXUNUSED( event ) )
{
    wxSize displaySize = ::wxGetDisplaySize();
    wxRect dialogPosition( 100, 50, 500, 300 );
    preferences->SetSize( dialogPosition );
    preferences->ShowModal();
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OnChangeWorkingDirectory( wxCommandEvent& event )
{
    wxDirDialog dialog( this, _T( "Change Working Directory..." ),
                        ::wxGetCwd(),
                        wxDD_DEFAULT_STYLE
                      );

    if( dialog.ShowModal() == wxID_OK )
    {
        wxFileName vesFileName( dialog.GetPath() );
        if( !vesFileName.MakeRelativeTo( ::wxGetCwd() ) )
        {
            wxMessageBox( _( "Can't change working directory to another drive." ),
                          _( "Change Directory Error" ), wxOK | wxICON_INFORMATION );
            return;
        }

        ///Clear the canvas then change the directory
        NewCanvas( event );

        //wxFileName::GetPath treats the last directory as the filename and excludes it from the path
        directory = vesFileName.GetFullPath( wxPATH_UNIX );
        //directory = vesFileName.GetPath( wxPATH_GET_VOLUME, wxPATH_UNIX );
        //change conductor working dir
        ::wxSetWorkingDirectory( directory );
        directory.Replace( _( "\\" ), _( "/" ), true );

        DataValuePairPtr dvp( new DataValuePair() );
        CommandPtr command( new Command() );
        std::string mode = ConvertUnicode( directory.c_str() );
        dvp->SetData( "WORKING_DIRECTORY", mode );
        command->SetCommandName( "Change Working Directory" );
        command->AddDataValuePair( dvp );

        CORBAServiceList::instance()->
            SendCommandStringToXplorer( command );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ChangeXplorerViewSettings( wxCommandEvent& event )
{
    DataValuePairPtr dataValuePair( new DataValuePair( std::string( "STRING" ) ) );
    CommandPtr veCommand( new Command() );
    veCommand->SetCommandName( std::string( "CHANGE_XPLORER_VIEW" ) );

    if( event.GetId() == APPFRAME_CHANGE_XPLORER_VIEW_NETWORK )
    {
        DataValuePairPtr dataValuePair2( new DataValuePair( std::string( "UNSIGNED INT" ) ) );
		dataValuePair2->SetData( "SUBNET_ID", canvas->GetActiveNetworkID() );
		veCommand->AddDataValuePair( dataValuePair2 );
        dataValuePair->SetData( "CHANGE_XPLORER_VIEW", "CHANGE_XPLORER_VIEW_NETWORK" );
    }
    else if( event.GetId() == APPFRAME_CHANGE_XPLORER_VIEW_CAD )
    {
        dataValuePair->SetData( "CHANGE_XPLORER_VIEW", "CHANGE_XPLORER_VIEW_CAD" );
    }
    else if( event.GetId() == APPFRAME_CHANGE_XPLORER_VIEW_LOGO )
    {
        dataValuePair->SetData( "CHANGE_XPLORER_VIEW", "CHANGE_XPLORER_VIEW_LOGO" );
    }
    else
    {
        dataValuePair->SetData( "CHANGE_XPLORER_VIEW", "ERROR" );
    }

    veCommand->AddDataValuePair( dataValuePair );
    serviceList->SendCommandStringToXplorer( veCommand );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OnTimer( wxTimerEvent& WXUNUSED( event ) )
{
    Network* network = 0;
    if( canvas )
    {
        network = canvas->GetActiveNetwork();
    }

    if( !network )
    {
        return;
    }

    //only when not dragging
    if( !network->IsDragging() )
    {
        ///Servicelist is initialized before network...
        serviceList->CheckORBWorkLoad();
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ShutdownXplorerOptionOn()
{
    xplorerMenu->Append( APPFRAME_XPLORER_EXIT, _( "Shutdown Xplorer" ) );
    xplorerMenu->Enable( APPFRAME_XPLORER_EXIT, true );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ShutdownXplorerOptionOff()
{
    xplorerMenu->Remove( APPFRAME_XPLORER_EXIT );
}
////////////////////////////////////////////////////////////////////////////////
HierarchyTree * AppFrame::GetHierarchyTree()
{
    return hierarchyTree;
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OnDelMod( wxCommandEvent& event )
{
    int* mod = static_cast< int* >( event.GetClientData() );
    hierarchyTree->RemoveFromTree( *mod );
}
///////////////////////////////////////////////////////////////////////////////
void AppFrame::SetTreeItemName( wxCommandEvent& event )
{
    unsigned int* id = static_cast< unsigned int* >( event.GetClientData() );
    wxString name = event.GetString();
    hierarchyTree->SetTreeItemName( *id, name );
}
///////////////////////////////////////////////////////////////////////////////
void AppFrame::OnMakeIntoHierarchy( wxCommandEvent& event )
{
    std::pair< unsigned int, unsigned int > *ids =
        static_cast< std::pair< unsigned int, unsigned int > * >
        ( event.GetClientData() );
    hierarchyTree->AppendToTree( ids->first, ids->second );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OnChangeIcon(wxCommandEvent& event )
{
	std::pair <unsigned int, std::string>* data = static_cast< 
		std::pair <unsigned int, std::string>*>( event.GetClientData() );
	hierarchyTree->ChangeLeafIcon( data->first, data->second );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OnShowIconChooser( wxCommandEvent& event )
{
    if( !iconChooser )
    {
        iconChooser = new IconChooser( canvas );
    }

	UIPluginBase* tempPlugin = static_cast< UIPluginBase* >(event.GetClientData());
    iconChooser->AddIconsDir( wxString( "2dicons", wxConvUTF8 ) );
    iconChooser->SetPlugin( tempPlugin );
    iconChooser->Show();
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OnChildCreate( wxWindowCreateEvent& event ) 
{ 
    wxWindow* w = event.GetWindow();
    /*std::cout << "Child created 1 " << std::endl; 
    std::cout << event.GetId() << std::endl;
    std::cout << ConvertUnicode( event.GetEventObject()->GetClassInfo()->GetClassName() ) << std::endl;*/
    if(w) 
    { 
        ///Log( "Child created" ); 
        w->Connect( wxEVT_DESTROY, wxWindowDestroyEventHandler(AppFrame::OnChildDestroy), 
                   NULL, this); 
    } 
} 
////////////////////////////////////////////////////////////////////////////////
void AppFrame::OnChildDestroy( wxWindowDestroyEvent& event ) 
{ 
    wxWindow* w = event.GetWindow(); 
    /*std::cout << ConvertUnicode( event.GetEventObject()->GetClassInfo()->GetClassName() ) << std::endl;
    wxLogMessage( _("destroyed") );
   */
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::LoadNewNetwork( wxUpdateUIEvent& WXUNUSED( event )  )
{
    {
        //Send a new start position for all apps
        //do this before loading the ves data
        // so in case a file has a start position it will be used
        CommandPtr viewPointGUIData( new Command() );
        viewPointGUIData->SetCommandName( "Navigation_Data" );
        
        DataValuePairPtr quatStartPosition( new DataValuePair());
        OneDDoubleArrayPtr quatData( new OneDDoubleArray( 0 ) );
        quatData->AddElementToArray( 0 );
        quatData->AddElementToArray( 0 );
        quatData->AddElementToArray( 0 );
        quatData->AddElementToArray( 1 );
        quatStartPosition->SetData( "QUAT_START_POSITION", quatData );
        viewPointGUIData->AddDataValuePair( quatStartPosition );
        
        DataValuePairPtr positionStartPosition( new DataValuePair() );
        OneDDoubleArrayPtr positionsData( new OneDDoubleArray( 0 ) );
        positionsData->AddElementToArray( 0 );
        positionsData->AddElementToArray( 0 );
        positionsData->AddElementToArray( 0 );
        positionStartPosition->SetData( "POSITION_START_POSITION", positionsData );
        viewPointGUIData->AddDataValuePair( positionStartPosition );
        serviceList->SendCommandStringToXplorer( viewPointGUIData );
    }
    {
        //Reset the center point when a new app is loaded
        //do this first in case a file has a default center point it will be used
        CommandPtr centerPointUpdateData( new Command() );
        centerPointUpdateData->SetCommandName( "CENTER_POINT_UPDATE" );
        
        DataValuePairPtr resetDVP( new DataValuePair() );
        resetDVP->SetData( "CENTER_POINT_UPDATE_DVP", "Reset" );
        centerPointUpdateData->AddDataValuePair( resetDVP );
        serviceList->SendCommandStringToXplorer( centerPointUpdateData );
    }

    //Reloading plugins
    av_modules->ResetPluginTree();

    if( newCanvas )
    {
        //clear any current tree
        hierarchyTree->Clear();
        SetTitle( _( "VE-Suite: www.vesuite.org" ) );
        canvas->CreateDefaultNetwork();
    }
    else
    {
        //Now laod the xml data now that we are in the correct directory
        canvas->PopulateNetworks( ConvertUnicode( mVESFileName.c_str() ) );
        
        //create hierarchy page
        hierarchyTree->PopulateTree();
        //XMLDataBufferEngine::instance()->GetTopSystemId() );

        ///This code will be moved in the future. It is Aspen specific code.
        CommandPtr aspenBKPFile = UserPreferencesDataBuffer::instance()->
            GetCommand( "Aspen_Plus_Preferences" );
        
        if( aspenBKPFile->GetCommandName() != "NULL" )
        {
            DataValuePairPtr bkpPtr =
            aspenBKPFile->GetDataValuePair( "BKPFileName" );
            std::string bkpFilename;
            bkpPtr->GetData( bkpFilename );
            OpenSimulation( wxString( bkpFilename.c_str(), wxConvUTF8 ) );
        }
        
        wxCommandEvent submitEvent;
        SubmitToServer( submitEvent );
        
        if( recordScenes )
        {
            recordScenes->_buildPage();
        }
    }
    
    //Now manage the data that is user specific to this ves file
    UserPtr userInfo = 
        XMLDataBufferEngine::instance()->GetXMLUserDataObject( "Network" );
    
    //If there was no color data in the ves file
    if( !userInfo->GetUserStateInfo() )
    {
        ///Color vector
        std::vector<double> backgroundColor;
        backgroundColor.clear();
        backgroundColor.push_back( 0.0f );
        backgroundColor.push_back( 0.0f );
        backgroundColor.push_back( 0.0f );
        backgroundColor.push_back( 1.0f );
        
        DataValuePairPtr dataValuePair( new DataValuePair() );
        dataValuePair->SetData( std::string( "Background Color" ), backgroundColor );
        CommandPtr veCommand( new Command() );
        veCommand->SetCommandName( std::string( "CHANGE_BACKGROUND_COLOR" ) );
        veCommand->AddDataValuePair( dataValuePair );
        UserPreferencesDataBuffer::instance()->
            SetCommand( std::string( "CHANGE_BACKGROUND_COLOR" ), veCommand );
    }
   
    {
        // Create the command and data value pairs
        CommandPtr tempCommand = UserPreferencesDataBuffer::instance()->
            GetCommand( "CHANGE_BACKGROUND_COLOR" );
        
        serviceList->SendCommandStringToXplorer( tempCommand );
        
        // Create the command and data value pairs
        tempCommand = 
            UserPreferencesDataBuffer::instance()->
            GetCommand( "Navigation_Data" );
        
        serviceList->SendCommandStringToXplorer( tempCommand );

        // Create the command and data value pairs
        tempCommand = 
            UserPreferencesDataBuffer::instance()->
            GetCommand( "CHANGE_NEAR_FAR_RATIO" );
        if( tempCommand->GetCommandName().compare( "NULL" ) )
        {
            serviceList->SendCommandStringToXplorer( tempCommand );
        }
        
        // Create the command and data value pairs
        tempCommand = 
            UserPreferencesDataBuffer::instance()->
            GetCommand( "Update LOD Scale" );
        if( tempCommand->GetCommandName().compare( "NULL" ) )
        {
            serviceList->SendCommandStringToXplorer( tempCommand );
        }
    }
    
    //Send the new commands after the new data is loaded not before
    //Change view to CAD to make sure
    {
        DataValuePairPtr dataValuePair( new DataValuePair( std::string( "STRING" ) ) );
        CommandPtr veCommand( new Command() );
        veCommand->SetCommandName( std::string( "CHANGE_XPLORER_VIEW" ) );
        dataValuePair->SetData( "CHANGE_XPLORER_VIEW", "CHANGE_XPLORER_VIEW_CAD" );
        veCommand->AddDataValuePair( dataValuePair );
        bool connected = serviceList->SendCommandStringToXplorer( veCommand );
    }    
    
    // Initialze Minerva.
    {
      CommandPtr earthCommand ( UserPreferencesDataBuffer::instance()->GetCommand ( ves::util::commands::ADD_EARTH_COMMAND_NAME ) );
      if ( earthCommand )
      {
        serviceList->SendCommandStringToXplorer( earthCommand );

        CommandPtr rasterGroupCommand ( UserPreferencesDataBuffer::instance()->GetCommand ( ves::util::commands::ADD_RASTER_GROUP ) );
        CommandPtr elevationGroupCommand ( UserPreferencesDataBuffer::instance()->GetCommand ( ves::util::commands::ADD_ELEVATION_GROUP ) );

        if ( rasterGroupCommand )
        {
          serviceList->SendCommandStringToXplorer( rasterGroupCommand );
        }

        if ( elevationGroupCommand )
        {
          serviceList->SendCommandStringToXplorer( elevationGroupCommand );
        }

        MinervaDialog* dialog ( this->GetMinervaDialog() );
        dialog->InitalizeFromCommands ( elevationGroupCommand, rasterGroupCommand );
      }
    }

    newCanvas = false;
}
///////////////////////////////////////////////////////////////////////////////
void AppFrame::OnKeyPress( wxKeyEvent &event )
{
    wxCommandEvent tempCommand;
    if( event.GetModifiers() == wxMOD_CONTROL)
    {
    if( event.GetKeyCode() == WXK_UP )
    {
        ZoomIn( tempCommand );
    }
    else if( event.GetKeyCode() == WXK_DOWN )
    {
        ZoomOut( tempCommand );
    }
    }
    else
    {
        event.Skip();
    }
}
///////////////////////////////////////////////////////////////////////////////
void AppFrame::UpdateHierarchyTree( wxCommandEvent& event )
{
    hierarchyTree->PopulateTree();
}
///////////////////////////////////////////////////////////////////////////////
void AppFrame::OnAddPlanet ( wxCommandEvent& event )
{
  CommandPtr veCommand ( new Command );
  veCommand->SetCommandName ( ves::util::commands::ADD_EARTH_COMMAND_NAME );

  // Add a dummy data value pair.
  DataValuePairPtr dataValuePair ( new DataValuePair );
  dataValuePair->SetData( "dummy", "dummy" );
  veCommand->AddDataValuePair( dataValuePair );

  ves::conductor::UserPreferencesDataBuffer::instance()->SetCommand ( ves::util::commands::ADD_EARTH_COMMAND_NAME, veCommand );

  serviceList->SendCommandStringToXplorer ( veCommand );

  MinervaDialog *dialog ( this->GetMinervaDialog() );
  dialog->AddDefaultLayers();
}
///////////////////////////////////////////////////////////////////////////////
void AppFrame::OnRemovePlanet ( wxCommandEvent& event )
{
  CommandPtr veCommand ( new Command );
  veCommand->SetCommandName ( ves::util::commands::REMOVE_EARTH_COMMAND_NAME );

  // Add a dummy data value pair.
  DataValuePairPtr dataValuePair ( new DataValuePair );
  dataValuePair->SetData( "dummy", "dummy" );
  veCommand->AddDataValuePair( dataValuePair );

  ves::conductor::UserPreferencesDataBuffer::instance()->SetCommand ( ves::util::commands::ADD_EARTH_COMMAND_NAME, CommandPtr() );

  serviceList->SendCommandStringToXplorer ( veCommand );
}
///////////////////////////////////////////////////////////////////////////////
void AppFrame::ShowMinervaDialog ( wxCommandEvent& event )
{
  MinervaDialog *dialog ( this->GetMinervaDialog() );
  dialog->Show();
}
///////////////////////////////////////////////////////////////////////////////
MinervaDialog* AppFrame::GetMinervaDialog()
{
  if ( 0x0 == _minervaDialog )
  {
    _minervaDialog = new MinervaDialog ( this, wxID_ANY );
  }
  return _minervaDialog;
}
///////////////////////////////////////////////////////////////////////////////
void AppFrame::OnDataLogging( wxCommandEvent& event )
{
    if( event.GetId() == APPFRAME_START_DATALOGGING )
    {
        wxFileName vesFileName;
        int answer = 0;
        do
        {
            //Set this here so that after correcting for not replacing the file name
            //we can exit the do while loop
            answer = 0;
            wxTextEntryDialog* newDataSetName;
                newDataSetName = new wxTextEntryDialog( this,
                                                       _( "Enter the prefix for *.vem filename:" ),
                                                       _( "Save VEM file as..." ),
                                                       _( "untitled" ), wxOK | wxCANCEL );
            
            if( newDataSetName->ShowModal() == wxID_OK )
            {
                vesFileName.ClearExt();
                vesFileName.SetName( newDataSetName->GetValue() );
                vesFileName.SetExt( _( "vem" ) );
            }
            else
            {
                break;
            }
            
            if( vesFileName.FileExists() )
            {
                wxString tempMessage = _( "Do you want to replace " ) + 
                vesFileName.GetFullName() + _( "?" );
                wxMessageDialog promptDlg( this,
                                          tempMessage,
                                          _( "Overwrite File Warning" ),
                                          wxYES_NO | wxNO_DEFAULT | wxICON_QUESTION,
                                          wxDefaultPosition );
                answer = promptDlg.ShowModal();
            }
            delete newDataSetName;
        }
        while( answer == wxID_NO );
        
        if( vesFileName.FileExists() && !wxFileName::IsFileWritable( vesFileName.GetFullPath() ) )
        {
            wxString tempMessage = _( "Cannot write file " ) + vesFileName.GetFullName() + _( "?" );
            wxMessageDialog promptDlg( this,
                                      tempMessage,
                                      _( "Overwrite File Warning" ),
                                      wxYES_NO | wxNO_DEFAULT | wxICON_QUESTION,
                                      wxDefaultPosition );
            promptDlg.ShowModal();
            return;
        }
        
        if( vesFileName.HasName() )
        {
            directory = vesFileName.GetPath();
            SetRecentFile( vesFileName );
        }
    
    
        std::string vemFilename = ConvertUnicode( vesFileName.GetFullName().c_str() );
        ves::conductor::util::DataLoggerEngine::instance()->SetMovieFilename( vemFilename );

        Log( std::string( "Opening " + vemFilename + " movie file.").c_str() );
    }
    else
    {
        ves::conductor::util::DataLoggerEngine::instance()->ToggleOn( false );
    }
}
///////////////////////////////////////////////////////////////////////////////
void AppFrame::OnDataLoggingSettings( wxCommandEvent& event )
{
    if( event.GetId() == APPFRAME_XPLORER_DATALOGGING_LOOPING )
    {
        ves::conductor::util::DataLoggerEngine::instance()->LoopingOn( true );
    }
}
