/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
#include "VE_Conductor/Framework/Frame.h"
#include <wx/imaglist.h>
#include <wx/artprov.h>
#include <wx/msgdlg.h>

#include "VE_Conductor/GUIPlugin/ResultPanel.h"
#include "VE_Conductor/Framework/App.h"
#include "VE_Conductor/Framework/UserPreferences.h"
#include "VE_Conductor/Network/package.h"
#include "VE_Conductor/GUIPlugin/OrbThread.h"
#include "VE_Conductor/Framework/Avail_Modules.h"
#include "VE_Conductor/Framework/UI_TeacherTab.h"
#include "VE_Conductor/GUIPlugin/FinancialDialog.h"
#include "VE_Conductor/GUIPlugin/TextResultDialog.h"
#include "VE_Conductor/GUIPlugin/TexTable.h"
#include "VE_Conductor/GUIPlugin/GlobalParamDialog.h"
#include "VE_Conductor/GUIPlugin/SummaryResultDialog.h"
#include "VE_Conductor/GUIPlugin/FindDialog.h"
#include "VE_Conductor/Framework/DeviceProperties.h"
#include "VE_Conductor/Framework/NavigationPane.h"
#include "VE_Conductor/Framework/SoundsPane.h"
//#include "VE_Conductor/Framework/StreamersPane.h"

#include "VE_Conductor/Framework/vectors.h"
#include "VE_Conductor/Framework/vistab.h"
#include "VE_Conductor/Framework/Splitter.h"

#include "VE_Conductor/Framework/ViewLocPane.h"
#include "VE_Conductor/Utilities/CADNodeManagerDlg.h"
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

//#include "VE_Conductor/VE_UI/UI_Tabs.h"
//#include "VE_Conductor/VE_UI/UI_Frame.h"
#include "VE_Conductor/Framework/Network.h"
#include "VE_Conductor/GUIPlugin/CORBAServiceList.h"

#include "VE_Conductor/Utilities/Module.h"
#include "VE_Conductor/Utilities/Tag.h"

#include <wx/image.h>
#include <wx/bitmap.h>
#include <wx/splash.h>
#include <wx/utils.h>
#include <wx/app.h>
#include <wx/cmndata.h>
#include <wx/colordlg.h>

#include "VE_Installer/installer/installerImages/ve_ce_banner.xpm"
#include "VE_Installer/installer/installerImages/ve_icon64x64.xpm"
#include "VE_Installer/installer/installerImages/ve_icon32x32.xpm"
#include "VE_Conductor/xpm/selection32x32.xpm"
#include "VE_Conductor/xpm/navigation32x32.xpm"
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
lrint (double flt)
{
  int intgr;
  _asm
  {
      fld flt
      fistp intgr
  } ;
  return intgr ;
}

__inline long int
lrintf (float flt)
{
   int intgr;
   _asm
  {
    fld flt
    fistp intgr
  } ;
  return intgr ;
}
#endif

BEGIN_EVENT_TABLE (AppFrame, wxFrame)
   EVT_CLOSE(AppFrame::OnClose)
   EVT_MENU(v21ID_ZOOMIN, AppFrame::ZoomIn)
   EVT_MENU(v21ID_ZOOMOUT, AppFrame::ZoomOut)
   EVT_MENU(wxID_SAVE, AppFrame::Save)
   EVT_MENU(wxID_SAVEAS, AppFrame::SaveAs)
   EVT_MENU(wxID_NEW, AppFrame::New)
   // this is probably a bug and needs to be fixed
   EVT_MENU(wxID_EXIT, AppFrame::FrameClose)
   EVT_MENU(ID_PREFERENCES, AppFrame::OnPreferences)
   EVT_MENU(wxID_OPEN, AppFrame::Open)

	// use max of 10 for recent file list, bind each ID to OpenRecentFile
	EVT_MENU(v21ID_BASE_RECENT  , AppFrame::OpenRecentFile)
	EVT_MENU(v21ID_BASE_RECENT+1, AppFrame::OpenRecentFile)
	EVT_MENU(v21ID_BASE_RECENT+2, AppFrame::OpenRecentFile)
	EVT_MENU(v21ID_BASE_RECENT+3, AppFrame::OpenRecentFile)
	EVT_MENU(v21ID_BASE_RECENT+4, AppFrame::OpenRecentFile)
	EVT_MENU(v21ID_BASE_RECENT+5, AppFrame::OpenRecentFile)
	EVT_MENU(v21ID_BASE_RECENT+6, AppFrame::OpenRecentFile)
	EVT_MENU(v21ID_BASE_RECENT+7, AppFrame::OpenRecentFile)
	EVT_MENU(v21ID_BASE_RECENT+8, AppFrame::OpenRecentFile)
	EVT_MENU(v21ID_BASE_RECENT+9, AppFrame::OpenRecentFile)
	EVT_MENU(v21ID_CLEAR_RECENT,  AppFrame::ClearRecentFile)

   EVT_MENU(v21ID_LOAD, AppFrame::LoadFromServer)
   EVT_MENU(QUERY_FROM_SERVER, AppFrame::QueryFromServer)
   EVT_MENU(v21ID_SUBMIT, AppFrame::SubmitToServer)
   //EVT_MENU(v21ID_CONNECT, AppFrame::ConExeServer)
   EVT_MENU(v21ID_DISCONNECT, AppFrame::DisConExeServer)
   EVT_MENU(v21ID_DISCONNECT_VE, AppFrame::DisConVEServer)
   //EVT_MENU(v21ID_CONNECT_VE, AppFrame::ConVEServer)
   EVT_MENU(v21ID_START_CALC, AppFrame::StartCalc)
   EVT_MENU(v21ID_STOP_CALC, AppFrame::StopCalc)
   EVT_MENU(v21ID_PAUSE_CALC, AppFrame::PauseCalc)
   EVT_MENU(v21ID_RESUME_CALC, AppFrame::ResumeCalc)

   EVT_MENU(v21ID_HELP, AppFrame::ViewHelp)
   EVT_MENU(v21ID_ABOUT, AppFrame::ViewAbout)
   EVT_MENU(v21ID_REVISION, AppFrame::ViewRevision)
   EVT_MENU(v21ID_CONTACTS, AppFrame::ViewContacts)
   EVT_MENU(v21ID_PLATFORM, AppFrame::ViewPlatformInfo)

   EVT_MENU(v21ID_VIEW_RESULT, AppFrame::ViewResult)

   EVT_MENU( WAND, AppFrame::ChangeDevice )
   EVT_MENU( KEYBOARD_MOUSE, AppFrame::ChangeDevice )

   EVT_MENU( NAVIGATION_MODE, AppFrame::ChangeDeviceMode )
   EVT_MENU( SELECTION_MODE, AppFrame::ChangeDeviceMode )

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
   //  EVT_MENU( XPLORER_VIEWPOINTS, AppFrame::LaunchSoundsPane )
   EVT_MENU( XPLORER_SOUNDS, AppFrame::LaunchSoundsPane )
   EVT_MENU( JUGGLER_STEREO, AppFrame::JugglerSettings )
   EVT_MENU( JUGGLER_MONO, AppFrame::JugglerSettings )
   EVT_MENU( CAD_NODE_DIALOG, AppFrame::LaunchCADNodePane )
//   EVT_MENU( XPLORER_VISTABS, AppFrame::LaunchVisTabs ) 
//   EVT_MENU( XPLORER_STREAMLINE, AppFrame::LaunchStreamlinePane )
//   EVT_MENU( XPLORER_VISTAB, AppFrame::LaunchVistab )   

   //  EVT_MENU(v21ID_GLOBAL_PARAM, AppFrame::GlobalParam)
   //  EVT_MENU(v21ID_BASE, AppFrame::LoadBase)
   //  EVT_MENU(v21ID_SOUR, AppFrame::LoadSour)
   //  EVT_MENU(v21ID_REI_BASE, AppFrame::LoadREIBase)
   //  EVT_MENU(v21ID_REI_SOUR, AppFrame::LoadREISour)
   EVT_IDLE( AppFrame::IdleEvent )
   EVT_TIMER( TIMER_ID, AppFrame::TimerEvent )
   EVT_MENU( QUERY_NETWORK, AppFrame::QueryNetwork )
   EVT_MENU( RUN_ASPEN_NETWORK, AppFrame::RunAspenNetwork )
   EVT_MENU( SHOW_ASPEN_SIMULATION, AppFrame::ShowAspenSimulation )
   EVT_MENU( HIDE_ASPEN_SIMULATION, AppFrame::HideAspenSimulation )
   EVT_MENU( CLOSE_ASPEN_SIMULATION, AppFrame::CloseAspenSimulation )
   EVT_MENU( CONDUCTOR_FIND, AppFrame::FindBlocks )
   EVT_MENU( CHANGE_XPLORER_VIEW_NETWORK, AppFrame::ChangeXplorerViewSettings )
   EVT_MENU( CHANGE_XPLORER_VIEW_CAD, AppFrame::ChangeXplorerViewSettings )
   EVT_MENU( CHANGE_XPLORER_VIEW_LOGO, AppFrame::ChangeXplorerViewSettings )

//   EVT_SPLITTER_SASH_POS_CHANGED( SPLIT_WINDOW, AppFrame::UnSplitWindow )

END_EVENT_TABLE()

AppFrame::AppFrame(wxWindow * parent, wxWindowID id, const wxString& title)
  :wxFrame(parent, id, title), 
   m_frameNr(0), 
   f_financial(true), 
   f_geometry(true), 
   f_visualization(true),
   timer( this, TIMER_ID )
{
   timer.Start( 1000 );
   
   char** tempArray = new char*[ ::wxGetApp().argc ];
   for ( size_t i = 0; i < ::wxGetApp().argc; ++i )
   {
      tempArray[ i ] = new char[ strlen( ConvertUnicode( ::wxGetApp().argv[ i ] ).c_str() ) + 1 ];
      strcpy( tempArray[ i ], ConvertUnicode( ::wxGetApp().argv[ i ] ).c_str() );
   }
   serviceList = VE_Conductor::CORBAServiceList::instance();
   serviceList->SetArgcArgv( ::wxGetApp().argc, tempArray );
   preferences = new UserPreferences(this, ::wxNewId(), 
                                     SYMBOL_USERPREFERENCES_TITLE, SYMBOL_USERPREFERENCES_POSITION, 
                                     SYMBOL_USERPREFERENCES_SIZE, SYMBOL_USERPREFERENCES_STYLE );
   xplorerMenu = 0;
   recordScenes = 0;

   this->SetIcon( ve_icon32x32_xpm );

   //int displayWidth, displayHeight = 0;
   //::wxDisplaySize(&displayWidth,&displayHeight);

   m_frame = 0;
   is_orb_init= false;
   connectToVE = false;
   connectToCE = false;
   _treeView = 0;
   _displayMode = "Tablet";
   _detectDisplayAndCreate();
   
	path			= _("NO_FILE_OPENED");
	directory	= _("NO_FILE_OPENED");
	fname			= _("NO_FILE_OPENED");

 
   GetConfig(NULL);

	CreateMenu();
   //CreateTB();
   CreateStatusBar();
   SetStatusText( _("VE-Conductor Status") );

   deviceProperties = 0;
   navPane = 0;
   soundsPane = 0;
   viewlocPane = 0;

   _cadDialog = 0;
   
   domManager = new VE_XML::DOMDocumentManager();
   ///Initialize VE-Open
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "XML",new VE_XML::XMLCreator() );
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "Shader",new VE_Shader::ShaderCreator() );
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "Model",new VE_XML::VE_Model::ModelCreator() );
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "CAD",new VE_XML::VE_CAD::CADCreator() );
   
   //Try and load network from server if one is already present
   wxCommandEvent event;
   LoadFromServer( event );
   //Process command line args to see if ves file needs to be loaded
   ProcessCommandLineArgs();
   
   xplorerColor.push_back( 0.0f );
   xplorerColor.push_back( 0.0f );
   xplorerColor.push_back( 0.0f );
   xplorerColor.push_back( 1.0f );
   xplorerWxColor = new wxColourData();
   xplorerWxColor->SetChooseFull(true);
}
///////////////////////////////////////
std::string AppFrame::GetDisplayMode()
{
   return _displayMode;
}
//////////////////////////////////////
void AppFrame::_detectDisplay()
{
   for ( int i = 1; i < wxTheApp->argc ; ++i )
   {
      if ( ConvertUnicode( wxTheApp->argv[i] ) == std::string("-VESDesktop") )
      {
         _displayMode = std::string("Desktop");
         break;
      }
   }
   //return _displayMode;
}
////////////////////////////////////////////////////////
void AppFrame::_createTreeAndLogWindow(wxWindow* parent)
{
   if( GetDisplayMode() == "Tablet")
   {
      wx_log_splitter = new Splitter(parent, -1);
      wx_log_splitter->SetMinimumPaneSize( 40 );
      serviceList->GetMessageLog()->Create( wx_log_splitter, MYLOG, _(""), wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY );
      wx_nw_splitter = new Splitter(wx_log_splitter, -1);
   }
   else
   {
      serviceList->GetMessageLog()->Create( this, MYLOG, _(""), wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY );
      wx_nw_splitter = new Splitter(parent, -1);
   }

   wx_nw_splitter->SetMinimumPaneSize( 1 );

   av_modules = new Avail_Modules(wx_nw_splitter, Avail_Modules::TREE_CTRL, wxDefaultPosition, wxDefaultSize, wxTR_HAS_BUTTONS);
   network = new Network(wx_nw_splitter, -1 );
   av_modules->SetNetwork(network);

   if ( GetDisplayMode() == "Tablet")
   {
      wx_log_splitter->SplitHorizontally(wx_nw_splitter, serviceList->GetMessageLog(), -100);
   }

   wx_nw_splitter->SplitVertically(av_modules, network, 140);
}
//////////////////////////////////
void AppFrame::_configureDesktop()
{
   SetTitle( _("VE-Suite: www.vesuite.org") );
   _treeView = new wxDialog(this, -1, _("Available Objects"), 
                                 wxDefaultPosition, wxDefaultSize,
                                 wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER);//wxCAPTION | wxRESIZE_BORDER);//(wxDEFAULT_DIALOG_STYLE&~ (wxCLOSE_BOX | wxRESIZE_BORDER | wxRESIZE_BOX | wxMAXIMIZE_BOX)));//|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX|wxCLOSE_BOX));
   wxBoxSizer* treeViewSizer = new wxBoxSizer(wxHORIZONTAL);

   _treeView->SetAutoLayout(true);
   _treeView->SetSizer(treeViewSizer);

   _createTreeAndLogWindow(_treeView);
   treeViewSizer->Add(wx_nw_splitter,1, wxALIGN_CENTER|wxEXPAND);

   int displayWidth, displayHeight = 0;
   ::wxDisplaySize(&displayWidth,&displayHeight);

   SetSize(wxSize(displayWidth,160/*displayHeight*0.0732421875*/));
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
/////////////////////////////////
void AppFrame::_configureTablet()
{
   _createTreeAndLogWindow(this);
   SetSize(DetermineFrameSize(NULL));
}
/////////////////////////////////////////
void AppFrame::_detectDisplayAndCreate()
{ 
   _detectDisplay();
   if ( GetDisplayMode() == "Desktop")
   {
      _configureDesktop();
      SetWindowStyle( wxDEFAULT_FRAME_STYLE & ~ (wxRESIZE_BORDER | wxRESIZE_BOX | wxMAXIMIZE_BOX) );
   }
   else if ( GetDisplayMode() == "Tablet")
   {
      _configureTablet();
      SetWindowStyle( wxDEFAULT_FRAME_STYLE | wxRESIZE_BORDER | wxRESIZE_BOX | wxMAXIMIZE_BOX );
   }
   else
   {
      wxMessageBox( _("Unable to create GUI."), _("Unknown display request!"), 
            wxOK | wxICON_INFORMATION );
      _exit(1);
   }
}
///////////////////////////////
bool AppFrame::Show(bool value)
{
   bool status = false;
   status = wxFrame::Show(value);

   if(_displayMode == "Desktop")
   {
      int displayWidth, displayHeight = 0;
      ::wxDisplaySize(&displayWidth,&displayHeight);
      std::cout<<"Width: "<<displayWidth<<" Height: "<<displayHeight<<std::endl;
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
////////////////////////////
void AppFrame::CreateVETab()
{
  //create the image list for the tabs first
  // create a dummy image list with a few icons

/*  wxSize imageSize(32, 32);
  
  m_imageList = new wxImageList( imageSize.GetWidth(), imageSize.GetHeight() );
  
  m_imageList->Add(wxArtProvider::GetIcon(wxART_INFORMATION, wxART_OTHER, imageSize));
  
  m_imageList->Add(wxArtProvider::GetIcon(wxART_QUESTION, wxART_OTHER, imageSize));
  
  m_imageList->Add(wxArtProvider::GetIcon(wxART_WARNING, wxART_OTHER, imageSize));
  
  m_imageList->Add(wxArtProvider::GetIcon(wxART_ERROR, wxART_OTHER, imageSize));
  */
/*   visTabs = new wxDialog(NULL,-1, wxString("Vis Tabs"), 
		  wxDefaultPosition, wxDefaultSize, 
		  (wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX) & ~ wxSTAY_ON_TOP);
   // Set the default location and size for the vis panel
   wxSize displaySize = ::wxGetDisplaySize();
   wxRect dialogPosition( displaySize.GetWidth() - 575, displaySize.GetHeight() - 550, 575, 550 );
   visTabs->SetSize( dialogPosition );
   //Create panel for vis pane
   m_frame = new UI_Frame(vjobs.in(), visTabs, wxID_HIGHEST);
   // Create the notebook's panels
   //m_tabs->AssignImageList(m_imageList);
   //m_frame->_tabs->AssignImageList(m_imageList);
   //m_tabs->createTabPages();
   //wxNotebookSizer *nbs = new wxNotebookSizer(m_tabs);
   //sizerTab->Add(nbs, 1, wxEXPAND | wxALL);
   //sizerTab = new wxBoxSizer(wxVERTICAL);
   //sizerTab->Add(m_frame, 1, wxEXPAND | wxALL);
   //sizerTab->Layout();
 
   //wx_ve_splitter->SetSizer(sizerTab);
   //wx_ve_splitter->SetAutoLayout(TRUE);

   //wx_ve_splitter->SplitVertically(wx_nw_splitter, m_tabs, 0);
   //wx_ve_splitter->SplitVertically(wx_nw_splitter, m_frame, 0);

   //trying to get the tabs to show up on initialization!!!!!
   //wxSize windowSize = m_tabs->GetSize();
   //m_tabs->SetSize(windowSize.GetWidth()+1,windowSize.GetHeight()+1);
   //m_tabs->Refresh();

   //wxSize windowSize = m_frame->GetSize();
   //m_frame->SetSize(windowSize.GetWidth()+1,windowSize.GetHeight()+1);
   //m_frame->Refresh();


   //this->SetIcon( wxIcon( ve_xplorer_banner_xpm ) );
   //SetSizer( sizerTab );*/
}

//////////////////////////////////////////////////////////////////////////
void AppFrame::GetConfig(wxConfig* config)
{
  wxConfig* cfg = config;
  if (!config) cfg = new wxConfig (wxTheApp->GetAppName());
  bool exist = false;

  wxString key = FEATURE;
  if (cfg->Exists (key)) 
  {
      exist  = cfg->Read (key + _T("/") + F_FINANCIAL, &f_financial);
      exist  = cfg->Read (key + _T("/") + F_GEOMETRY, &f_geometry);
      exist  = cfg->Read (key + _T("/") + F_VISUALIZATION, &f_visualization);
  }
  else
	{
		f_financial = true;
		f_geometry = true;
		f_visualization = true;
	}

	key = RECENT_FILE;
	wxString pathDummy;
	exist = true;
	int counter = 0;

	while( exist == true )	
	{
		exist = cfg->Read( key + wxString::Format (_("%i"), counter), &pathDummy );
		if( exist == true )		recentFileArchive.push_back( wxFileName(pathDummy) );
		counter++;
	}

  if (!config) delete cfg;
}

//////////////////////////////////////////////////////////////////////////
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
      return wxRect( xStart, 
                     bbox.GetBottomRight().y, 
                     width, 
                     height 
                   );
   }
   else
   {
      int xStart = lrint( 2.0f*displayWidth/3.0f );
      int width = lrint( displayWidth/3.0f );
      int height = lrint( 3*displayHeight/4.0f );
      return wxRect( xStart, 
                     0, 
                     width, 
                     height 
                   );
   }
}
wxRect AppFrame::DetermineFrameSize (wxConfig* config)
{
  const int minFrameWidth = 600;
  const int minFrameHight = 400;
  
  wxRect rect;
  wxSize scr = wxGetDisplaySize();
  wxConfig* cfg = config;
  if (!config) cfg = new wxConfig (wxTheApp->GetAppName());
  int i;
  //for (i = 0; i <= m_frameNr; i++) { //cyang only 1 frame
    //wxString key = LOCATION + wxString::Format ("%d", m_frameNr - i);
  wxString key = LOCATION + wxString::Format(_("%d"), 0);
  if (cfg->Exists (key)) 
  {
      rect.x = cfg->Read (key + _T("/") + LOCATION_X, rect.x);
      rect.y = cfg->Read (key + _T("/") + LOCATION_Y, rect.y);
      rect.width = cfg->Read (key + _T("/") + LOCATION_W, rect.width);
      rect.height = cfg->Read (key + _T("/") + LOCATION_H, rect.height);
  }
	//}
  if (!config) delete cfg;
  
  // check for reasonable values (within screen)
  rect.x = wxMin (abs (rect.x), (scr.x - minFrameWidth));
  rect.y = wxMin (abs (rect.y), (scr.y - minFrameHight));
  rect.width = wxMax (abs (rect.width), (minFrameWidth));
  rect.width = wxMin (abs (rect.width), (scr.x - rect.x));
  rect.height = wxMax (abs (rect.height), (minFrameHight));
  rect.height = wxMin (abs (rect.height), (scr.y - rect.y));
  
  return rect;
}

void AppFrame::StoreFrameSize (wxRect rect, wxConfig* config)
{
  // store size
  wxConfig* cfg = config;
  if (!config) cfg = new wxConfig (wxTheApp->GetAppName());
  wxString key = LOCATION + wxString::Format( _("%d"), m_frameNr);
  cfg->Write (key + _T("/") + LOCATION_X, rect.x);
  cfg->Write (key + _T("/") + LOCATION_Y, rect.y);
  cfg->Write (key + _T("/") + LOCATION_W, rect.width);
  cfg->Write (key + _T("/") + LOCATION_H, rect.height);
  
  if (!config) delete cfg;
}

//////////////////////////////////////////////////////////////////////////
void AppFrame::StoreConfig(wxConfig* config)
{
	//store config

	wxConfig* cfg = config;
	if (!config) cfg = new wxConfig (wxTheApp->GetAppName());
	wxString key = FEATURE;
	cfg->Write (key+_T("/") + F_FINANCIAL, f_financial);
	cfg->Write (key+_T("/") + F_GEOMETRY, f_geometry);
	cfg->Write (key+_T("/") + F_VISUALIZATION, f_visualization);

	if (!config) delete cfg;
}

//////////////////////////////////////////////////////////////////////////
void AppFrame::StoreRecentFile( wxConfig* config )
{
	//store recent menus in config
	wxConfig* cfg = config;
	if (!config) cfg = new wxConfig (wxTheApp->GetAppName());
	
	wxString key = RECENT_FILE;
	bool exist = false;

	//remove old
	for(int i=0; i<10; i++)
	{
		exist = cfg->HasEntry (key + wxString::Format(_("%i"), i));

		if(exist)	
		{	
			cfg->DeleteEntry (key + wxString::Format(_("%i")), false);
		}
	}

	//store new
	for(int i=0; i<recentFileArchive.size(); i++)
	{
		cfg->Write (key + wxString::Format(_("%i"), i), recentFileArchive.at(i).GetFullPath() );
	}

	if (!config) delete cfg;
}

//////////////////////////////////////////////////////////////////////////
void AppFrame::OnClose(wxCloseEvent& WXUNUSED(event) )
{   
   if ( GetDisplayMode() == "Desktop" )
   {
      ExitXplorer();
   }
   
   serviceList->CleanUp();
   serviceList = 0;
   
   if (is_orb_init)
   {
      //CosNaming::Name UIname(1);
      //UIname.length(1);
      //if (p_ui_i!=NULL && p_ui_i->UIName_!="")
	   //{
	   // UIname[0].id = CORBA::string_dup ((p_ui_i->UIName_).c_str());
	   //naming_context->unbind(UIname);
	   //if ( !CORBA::is_nil( poa.in() ) )
	   //   poa->destroy (1, 1);
	   //}
      //orb->destroy();
   }
   if(_treeView)
   {
      _treeView->Destroy();
   }

   delete domManager;
   domManager = 0;

   if ( deviceProperties )
   {
      deviceProperties->Destroy();
      deviceProperties = 0;
   }
   
   if ( navPane )
   {
      navPane->Destroy();
      navPane = 0;
   }

   if ( viewlocPane )
   {
      viewlocPane->Destroy();
      viewlocPane = 0;
   }
/*
   if ( vistab )
   {
      vistab->Destroy();
      vistab = 0;
   }
*/
   if ( soundsPane )
   {
      soundsPane->Destroy();
      soundsPane = 0;
   }

   if( _cadDialog)
   {
      _cadDialog->Destroy();
      _cadDialog = 0;
   }

   StoreFrameSize(GetRect(), NULL);
   StoreConfig(NULL);
	StoreRecentFile(NULL);
   Destroy();
}

void AppFrame::FrameClose(wxCommandEvent& WXUNUSED(event) )
{
   Close(true);
}

void AppFrame::CreateMenu() 
{
   menubar = new wxMenuBar;
   file_menu = new wxMenu;
   con_menu = new wxMenu;
   run_menu = new wxMenu;
   edit_menu = new wxMenu;
   help_menu = new wxMenu;
//   config_menu = new wxMenu;
	openRecentMenu = new wxMenu;
	// TODO update openRecentMenu

   file_menu->Append( wxID_NEW, _("&New\tCtrl+N") );
   file_menu->Append( wxID_OPEN, _("&Open ..\tCtrl+O") );

	InitRecentFile();

	file_menu->Append( OPEN_RECENT_CONNECTION_MENU, _("Open recent file"), openRecentMenu, _("Open recent menu") );
   file_menu->AppendSeparator();

   file_menu->Append(wxID_SAVE, _("&Save\tCtrl+S"));
   file_menu->Append(wxID_SAVEAS, _("Save &as ..\tCtrl+Shift+S"));
   file_menu->AppendSeparator();
   file_menu->Append (wxID_PRINT_SETUP, _("Print Set&up .."));
   file_menu->Append (wxID_PREVIEW, _("Print Pre&view\tCtrl+Shift+P"));
   file_menu->Append (wxID_PRINT, _("&Print ..\tCtrl+P"));
   file_menu->AppendSeparator();
   file_menu->Append ( ID_PREFERENCES, _("Preferences") );
   file_menu->AppendSeparator();
   file_menu->Append (wxID_EXIT, _("&Quit\tCtrl+Q"));

   file_menu->Enable(wxID_PRINT_SETUP, false);
   file_menu->Enable(wxID_PREVIEW, false);	
   file_menu->Enable(wxID_PRINT, false);


   //con_menu->Append(v21ID_CONNECT, _("&Connect to Executive\tCtrl+C"));
   //con_menu->Append(v21ID_CONNECT_VE, _("Connect to VE"));
   //con_menu->AppendSeparator();
   con_menu->Append(v21ID_SUBMIT, _("Sub&mit Job\tCtrl+M"));
   con_menu->Append(v21ID_LOAD, _("&Load Job\tCtrl+L"));
   
   //con_menu->Append(QUERY_FROM_SERVER, _("&Query\tCtrl+U"));
   wxMenu * aspenMenu = new wxMenu();
   aspenMenu->Append( QUERY_NETWORK, _("Open Simulation") );
   aspenMenu->Append( SHOW_ASPEN_SIMULATION, _("Show Simulation") );
   aspenMenu->Append( HIDE_ASPEN_SIMULATION, _("Hide Simulation") );
   aspenMenu->Append( CLOSE_ASPEN_SIMULATION, _("Close Simulation") );
   aspenMenu->Append( RUN_ASPEN_NETWORK, _("Run Simulation") );
   aspenMenu->Append( CONDUCTOR_FIND, _("Find") );
   con_menu->Append( ASPEN_CONNECTION_MENU,   _("Aspen"), aspenMenu, _("Aspen connection") );

	//file_menu->Append( OPEN_RECENT_CONNECTION_MENU, _("Open recent file"), aspenMenu, _("NOTHING") );


   con_menu->AppendSeparator();
   con_menu->Append(v21ID_DISCONNECT, _("&Disconnect\tCtrl+d"));
   con_menu->Append(v21ID_DISCONNECT_VE, _("&Disconnect VE"));

   //con_menu->Enable(v21ID_SUBMIT,false);
   //con_menu->Enable(v21ID_LOAD, false);
   con_menu->Enable(v21ID_DISCONNECT, false);
   con_menu->Enable(v21ID_DISCONNECT_VE, false);


   run_menu->Append(v21ID_START_CALC, _("Start Simulation"));
   run_menu->Append(v21ID_STOP_CALC, _("Stop Simulation"));
   run_menu->Append(v21ID_PAUSE_CALC, _("Pause Simulation"));
   run_menu->Append(v21ID_RESUME_CALC, _("Resume Simulation"));
   run_menu->Append(v21ID_VIEW_RESULT, _("View Results"));
   // run_menu->Append(v21ID_GLOBAL_PARAM, _("Global Parameters"));
   // run_menu->Append(v21ID_VIEW_FINANCIAL, _("View Financial Params"));

   run_menu->Enable(v21ID_START_CALC, false);
   run_menu->Enable(v21ID_STOP_CALC, false);
   run_menu->Enable(v21ID_PAUSE_CALC, false);
   run_menu->Enable(v21ID_RESUME_CALC, false);
   // EPRI TAG run_menu->Enable(v21ID_VIEW_RESULT, false);

   edit_menu->Append(v21ID_UNDO, _("&Undo\tCtrl+U"));
   edit_menu->Append(v21ID_REDO, _("&Redo\tCtrl+R"));
   edit_menu->AppendSeparator();
   edit_menu->Append(v21ID_ZOOMIN, _("Zoom &In\tCtrl+I"));
   edit_menu->Append(v21ID_ZOOMOUT, _("&Zoom Out\tCtrl+Z"));

   edit_menu->Enable(v21ID_UNDO, false);
   edit_menu->Enable(v21ID_REDO, false);

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

   //if (f_visualization)
   {

	xplorerMenu = new wxMenu();
   xplorerDeviceMenu = new wxMenu();
   xplorerDeviceModeMenu = new wxMenu();
	xplorerJugglerMenu = new wxMenu();
   xplorerDisplayMenu = new wxMenu();
   xplorerViewMenu = new wxMenu();
   wxMenu* xplorerView = new wxMenu();

   xplorerDeviceMenu->AppendRadioItem( WAND, _("Wand") );
   xplorerDeviceMenu->AppendRadioItem( KEYBOARD_MOUSE,  _("Keyboard Mouse") );
   xplorerDeviceMenu->Check( KEYBOARD_MOUSE, true);
   xplorerDeviceMenu->AppendSeparator();
   xplorerDeviceMenu->Append( XPLORER_DEVICE_MODE, _("Mode"), xplorerDeviceModeMenu,  _("Used to change the mode of the active device") );
   xplorerDeviceMenu->Append( DEVICE_PROPERTIES,    _("Properties") );
   //
   xplorerDeviceModeMenu->AppendRadioItem( NAVIGATION_MODE, _("Navigation") );
   xplorerDeviceModeMenu->AppendRadioItem( SELECTION_MODE,  _("Selection") );
   //
   xplorerDisplayMenu->AppendCheckItem( FRAME_RATE,        _("Frame Rate") );
   xplorerDisplayMenu->AppendCheckItem( COORDINATE_SYSTEM, _("Coord System") );
   //
   xplorerViewMenu->Append( FRAME_ALL,       _("Frame All            [f]") );
   xplorerViewMenu->Append( FRAME_SELECTION, _("Frame Selection") );
   xplorerViewMenu->Append( RESET,           _("Reset                  [r]") );
   //
   xplorerView->Append( CHANGE_XPLORER_VIEW_NETWORK, _("Network") );
   xplorerView->Append( CHANGE_XPLORER_VIEW_CAD, _("CAD") );
   xplorerView->Append( CHANGE_XPLORER_VIEW_LOGO, _("Logo") );
   //
	xplorerJugglerMenu->Append( JUGGLER_STEREO, _("Stereo") );
	xplorerJugglerMenu->Append( JUGGLER_MONO, _("Mono") );
	xplorerJugglerMenu->Enable( JUGGLER_STEREO, true);
	xplorerJugglerMenu->Enable( JUGGLER_MONO, true);
   
	xplorerMenu->Append( XPLORER_NAVIGATION, _("Navigation Pane") );
	xplorerMenu->Append( XPLORER_VIEWPOINTS, _("Viewpoints Pane") );
	xplorerMenu->Append( XPLORER_SCENES,     _("Record Scenes") );
	xplorerMenu->Append( XPLORER_COLOR,      _("Background Color") );
	xplorerMenu->Append( XPLORER_SOUNDS,     _("Sounds Pane") );
   //xplorerMenu->Append( XPLORER_STREAMLINE, _("Streamline Pane") );
   xplorerMenu->Append( XPLORER_DEVICE,    _("Devices"),            xplorerDeviceMenu,  _("Used to change the active device") );
	xplorerMenu->Append( JUGGLER_SETTINGS,   _("Juggler Settings"),   xplorerJugglerMenu, _("Used to adjust juggler runtime settings") );
   xplorerMenu->Append( XPLORER_DISPLAY,    _("Display"),            xplorerDisplayMenu, _("Used to change display preferences") );
   xplorerMenu->Append( XPLORER_VIEW,       _("View"),               xplorerViewMenu,    _("Used to change the view") );
   //add the view settings
   xplorerMenu->Append( CHANGE_XPLORER_VIEW, _("Graphical View"),    xplorerView,        _("Used to change the view in xplorer") );
   //If the display mode is desktop then we will disconnect when exit is selected
   //and in other modes we will give the user the ability to exit
   if ( GetDisplayMode() != "Desktop" )
   {
      xplorerMenu->Append( XPLORER_EXIT, _("Shutdown Xplorer") );
   }
	//xplorerMenu->Append( CAD_NODE_DIALOG, _("CAD Hierarchy"));
//	xplorerMenu->Append( XPLORER_VISTABS, _("Vis Tabs"));
//   xplorerMenu->Append( XPLORER_VISTAB, _("Visualization Tabs"));

   xplorerMenu->Enable( XPLORER_NAVIGATION, true);
   xplorerMenu->Enable( XPLORER_VIEWPOINTS, true);
   xplorerMenu->Enable( XPLORER_SOUNDS, true);
   xplorerMenu->Enable( XPLORER_SCENES, true);
   //xplorerMenu->Enable( XPLORER_STREAMLINE, true);
   xplorerMenu->Enable( JUGGLER_SETTINGS, true);
//	xplorerMenu->Enable( CAD_NODE_DIALOG,true);
   }
//  config_menu->Append(v21ID_BASE,_("Base Quench"));
//  config_menu->Append(v21ID_SOUR, _("Base Quench & Sour Shift CO2"));
//  config_menu->Append(v21ID_REI_BASE, _("Base Quench (REI)"));
//  config_menu->Append(v21ID_REI_SOUR, _("Base Quench & Sour Shift CO2 (REI)"));
//  config_menu->Append(v21ID_SWEET, _("Sweet Shift CO2"));
//  config_menu->Append(v21ID_CO_DISPOSAL, _("Co-Disposal of H2S+CO2"));
  
//  config_menu->Enable(v21ID_SWEET, false);
//  config_menu->Enable(v21ID_CO_DISPOSAL, false);

   menubar->Append(file_menu, _("&File"));
//   menubar->Append(config_menu, _("&Configurations"));
   menubar->Append(edit_menu, _("&Edit"));
   menubar->Append(con_menu, _("&Connection"));
   menubar->Append(run_menu, _("&Execution"));
   menubar->Append( xplorerMenu, _("&VE-Xplorer") );
   menubar->Append(help_menu, _("&Help"));
   //if (f_visualization)


   SetMenuBar(menubar);
}

void AppFrame::CreateTB()
{
   toolbar=CreateToolBar(wxTB_FLAT|wxTB_HORIZONTAL);
   toolbar->SetBackgroundColour(wxColour(192,192,192));
   toolbar->SetToolBitmapSize(wxSize(32,32));
   wxBitmap selection_bitmap(selection32x32_xpm);
   toolbar->AddTool(ID_SELECTION_TOOLBAR, _(""),selection_bitmap,_("Selection"),wxITEM_RADIO);
   wxBitmap navigation_bitmap(navigation32x32_xpm);
   toolbar->AddTool(ID_NAVIGATION_TOOLBAR, _(""),navigation_bitmap, _("Navigation"),wxITEM_RADIO);
   toolbar->AddSeparator();
   toolbar->Realize();

   this->SetToolBar(toolbar);
}

void AppFrame::ZoomIn(wxCommandEvent& WXUNUSED(event) )
{
  //  printf("Scale = %g\n", network->m_xUserScale);
  int w, h, sx, sy;
  int xpos, ypos;
  network->GetClientSize(&w, &h);
  //  printf("Client size w: %d, h: %d\n", w, h);
  network->GetVirtualSize(&sx, &sy);
  //  printf("Virtual size sx: %d, sy: %d\n", sx, sy);

   if ( network->GetUserScale()->first > 4 )
      return; // maximum zoom in x3

  network->GetUserScale()->first +=0.1;
  network->GetUserScale()->second +=0.1; 

  
  network->GetNumPix()->first += 1;
  network->GetNumPix()->second += 1;
  network->GetViewStart(&xpos, &ypos);
  network->SetScrollbars( 
            network->GetNumPix()->first, 
            network->GetNumPix()->second, 
            network->GetNumUnit()->first, 
            network->GetNumUnit()->second );
  network->Scroll(xpos, ypos);
  network->ReDrawAll();
}

void AppFrame::ZoomOut(wxCommandEvent& WXUNUSED(event))
{
  //  printf("Scale = %g\n", network->m_xUserScale);
  int w, h, sx, sy;
  int xpos, ypos;
  network->GetClientSize(&w, &h);
  //  printf("Client size w: %d, h: %d\n", w, h);
  network->GetVirtualSize(&sx, &sy);
  //  printf("Virtual size sx: %d, sy: %d\n", sx, sy);

   if ( network->GetUserScale()->first < 0.4 )
      return; //minimum x-5

  network->GetUserScale()->first -= 0.1;
  network->GetUserScale()->second -=0.1;
  network->GetNumPix()->first-=1;
  network->GetNumPix()->second-=1;

  network->GetViewStart(&xpos, &ypos);
  network->SetScrollbars( 
            network->GetNumPix()->first, 
            network->GetNumPix()->second, 
            network->GetNumUnit()->first, 
            network->GetNumUnit()->second );
  network->Scroll(xpos, ypos);
  network->ReDrawAll();
}
////////////////////////////////////////////////////////////////////
void AppFrame::Save( wxCommandEvent& event )
{
   //First time call save will be the same as SaveAs
   if ( path == wxString("", wxConvUTF8) ) 
   {
      SaveAs( event );
   }
   else
   {
      ///now write the file out from domdocument manager
      //wrtie to path
      std::string data = network->Save( ConvertUnicode( path.c_str() ) );
   }
}

//////////////////////////////////////////////////////////////////////////
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
   
   if ( vesFileName.HasName() ) 
   {
		SetRecentFile(vesFileName);

      path			= vesFileName.GetFullPath();
      directory	= vesFileName.GetPath();
      fname			= vesFileName.GetFullName();
      ///now write the file out from domdocument manager
      //wrtie to path
      std::string data = network->Save( ConvertUnicode( path.c_str() ) );
   }
}

//////////////////////////////////////////////////////////////////////////
void AppFrame::Open(wxCommandEvent& WXUNUSED(event))
{
   wxFileDialog dialog
                  (
                     this,
                     _T("Open File dialog"),
                     _T(""),
                     fname,
                     _T("Network files (*.ves)|*.ves"),
                     wxOPEN|wxFILE_MUST_EXIST
                  );
   
   if (dialog.ShowModal() == wxID_OK)
   {
      wxFileName vesFileName( dialog.GetPath() );
      bool success = vesFileName.MakeRelativeTo( ::wxGetCwd() );   
      if ( !success )
      {
         wxMessageBox( _("Can't open a VES file on another drive."), 
                       _("VES File Read Error"), wxOK | wxICON_INFORMATION );
         return;
      }
      
      //We must make this call here 
      //because we set path to null in the New call
      wxCommandEvent event;
      New( event );

      path = vesFileName.GetFullPath();
		path.Replace( _("\\"), _("/"), true );
      directory = vesFileName.GetPath();
      //change conductor working dir
      ::wxSetWorkingDirectory( directory );
      path = vesFileName.GetFullName();
      std::string tempDir = ConvertUnicode( directory.c_str() );
		
		SetRecentFile( wxFileName(dialog.GetPath()) );

      if ( tempDir.empty() )
      {
         tempDir = "./";
      }
      //Send Command to change xplorer working dir
      // Create the command and data value pairs
      VE_XML::DataValuePair* dataValuePair = 
                        new VE_XML::DataValuePair(  std::string("STRING") );
      dataValuePair->SetData( "WORKING_DIRECTORY", tempDir );
      VE_XML::Command* veCommand = new VE_XML::Command();
      veCommand->SetCommandName( std::string("Change Working Directory") );
      veCommand->AddDataValuePair( dataValuePair );
      serviceList->SendCommandStringToXplorer( veCommand );
      delete veCommand;
      
      //Clear the viewpoints data
      //Since the data is "managed" by Xplorer we need to notify 
      //Xplorer when we load a new ves file to clear viewpoints since
      //They don't go with the new data.

      //Dummy data that isn't used but I don't know if a command will work
      //w/o a DVP 
      VE_XML::DataValuePair* dvp = 
                        new VE_XML::DataValuePair(  std::string("STRING") );
      dvp->SetData( "Clear Quat Data", tempDir );
      VE_XML::Command* vec = new VE_XML::Command();
      vec->SetCommandName( std::string("QC_CLEAR_QUAT_DATA") );
      vec->AddDataValuePair( dvp );
      serviceList->SendCommandStringToXplorer( vec );
      delete vec;

      //Now laod the xml data now that we are in the correct directory
      fname=dialog.GetFilename();
      SubmitToServer( event );      
      network->Load( ConvertUnicode( path.c_str() ) );
      SubmitToServer( event );      
   }
}

//////////////////////////////////////////////////////////////////////////
void AppFrame::InitRecentFile()
{
	int i;
	int openRecentFile_ID = v21ID_BASE_RECENT;
	wxString dummyString;

	if(recentFileArchive.size() == 0)
	{
		openRecentMenu->Append (openRecentFile_ID, _("None"));
		openRecentMenu->Enable (openRecentFile_ID, false);
	}
	else 
	{
		for( i=(recentFileArchive.size()-1); i>=0; i-- )
		{
			dummyString = recentFileArchive.at(i).GetFullName();
			openRecentMenu->Append (openRecentFile_ID + i, dummyString.SubString(0, dummyString.size() - 5));
		}
	}

	openRecentMenu->AppendSeparator();
	openRecentMenu->Append( v21ID_CLEAR_RECENT, _("Clear recent file list") );
}

//////////////////////////////////////////////////////////////////////////
void AppFrame::ClearRecentFile( wxCommandEvent& WXUNUSED(event) )
{
	// TODO maintaing list with only the current file will not work due to how Open(.) is structured, it doesn't always save the full path
	
	if(path == _("NO_FILE_OPENED"))
	{
		recentFileArchive.clear();
	}
	else
	{
		wxFileName vesCurrentOpen = recentFileArchive.at(recentFileArchive.size()-1);
		recentFileArchive.clear();
		recentFileArchive.push_back(vesCurrentOpen); 
	}

	


	//wxFileName currentFile = NULL;
	//if( ??? )								currentFile = ???
	
	//if( currentFile )					recentFileArchive.push_back(currentFile);

	SetRecentMenu();
}

//////////////////////////////////////////////////////////////////////////
void AppFrame::DeleteRecentFile(wxFileName vesFileName)
{
	// PROMPT BOX look at SaveAs() -- box that states fill has been moved or deleted, and ask them with radio buttons
	// a) to continue to Open menu, b) cancel and return to current working document
	std::vector< wxFileName > dummyList;
	int i;
	bool deleteFile = false;

	for(i=0; i<recentFileArchive.size(); i++)
	{
		if(vesFileName.GetFullPath() == recentFileArchive.at(i).GetFullPath())
			deleteFile = true;

		if(!deleteFile)	dummyList.push_back(recentFileArchive.at(i));
		else					deleteFile = false;
	}
	recentFileArchive.clear();
	
	for(i=0; i<dummyList.size(); i++)
	{
		recentFileArchive.push_back(dummyList.at(i));
	}
	dummyList.clear();

	SetRecentMenu();
}

//////////////////////////////////////////////////////////////////////////
void AppFrame::SetRecentFile(wxFileName vesFileName)
{
	std::vector< wxFileName > dummyList;
	dummyList.clear();
	int i;
	int offset = 0;
	bool repeat = false;

	// check if path is already in list, erase old location and place up front
	for(i=0; i<recentFileArchive.size(); i++)
	{
		if(vesFileName.GetFullPath() == recentFileArchive.at(i).GetFullPath())
		{
			repeat = true;
		}

		if(!repeat)		dummyList.push_back(recentFileArchive.at(i));
		else				repeat = false;
	}
	recentFileArchive.clear();

	if( dummyList.size() >= 10 )	offset = dummyList.size()-9;

	for(i=0+offset; i<dummyList.size(); i++)
		recentFileArchive.push_back(dummyList.at(i));

	recentFileArchive.push_back( vesFileName.GetFullPath() );
	dummyList.clear();

	SetRecentMenu();
}

//////////////////////////////////////////////////////////////////////////
void AppFrame::SetRecentMenu()
{
	int i;
	int openRecentFile_ID = v21ID_BASE_RECENT;
	wxString dummyFullName, dummyDirectory;

	// clear menu
	openRecentMenu->Delete( v21ID_CLEAR_RECENT );
	openRecentMenu->Delete( wxID_SEPARATOR ); //&wxMenuItem::IsSeparator );

	size_t menuSize = openRecentMenu->GetMenuItemCount();
	while( menuSize > 0 )
	{
		openRecentMenu->Delete(v21ID_BASE_RECENT + menuSize - 1);
		menuSize = openRecentMenu->GetMenuItemCount();
	}

	// set menu
	if( recentFileArchive.size() == 0 )
	{
		openRecentMenu->Append (openRecentFile_ID, _("None"));
		openRecentMenu->Enable (openRecentFile_ID, false);
	}

	for( i=(recentFileArchive.size()-1); i>=0; i-- )
	{
		dummyFullName  = recentFileArchive.at(i).GetFullName();
		dummyDirectory	= recentFileArchive.at(i).GetPath();
		openRecentMenu->Append( openRecentFile_ID + i, dummyFullName.SubString(0, dummyFullName.size() - 5) );
		openRecentMenu->SetHelpString( openRecentFile_ID + i, dummyDirectory );
	}

	openRecentMenu->AppendSeparator();
	openRecentMenu->Append( v21ID_CLEAR_RECENT, _("Clear recent file list") );
}

//////////////////////////////////////////////////////////////////////////
void AppFrame::OpenRecentFile( wxCommandEvent& event ) 
{
	int placeChosen = event.GetId();
	wxFileName vesFileName;

	vesFileName = recentFileArchive.at(placeChosen - v21ID_BASE_RECENT);

	// TODO also, make call if file they are trying to call does not exist, call DeleteRecentFile

	New( event );

	path			= vesFileName.GetFullPath();
	directory	= vesFileName.GetPath();
	fname			= vesFileName.GetFullName();

	SetRecentFile(vesFileName);

   std::string tempDir = ConvertUnicode( directory.c_str() );
	if ( tempDir.empty() )
	{
		tempDir = "./";
	}

	//Send Command to change xplorer working dir
	// Create the command and data value pairs
	VE_XML::DataValuePair* dataValuePair = 
                  new VE_XML::DataValuePair(  std::string("STRING") );
	dataValuePair->SetData( "WORKING_DIRECTORY", tempDir );
	VE_XML::Command* veCommand = new VE_XML::Command();
	veCommand->SetCommandName( std::string("Change Working Directory") );
	veCommand->AddDataValuePair( dataValuePair );
	serviceList->SendCommandStringToXplorer( veCommand );
	delete veCommand;

	//Clear the viewpoints data
	//Since the data is "managed" by Xplorer we need to notify 
	//Xplorer when we load a new ves file to clear viewpoints since
	//They don't go with the new data.

	//Dummy data that isn't used but I don't know if a command will work
	//w/o a DVP 
	VE_XML::DataValuePair* dvp = 
                  new VE_XML::DataValuePair(  std::string("STRING") );
	dvp->SetData( "Clear Quat Data", tempDir );
	VE_XML::Command* vec = new VE_XML::Command();
	vec->SetCommandName( std::string("QC_CLEAR_QUAT_DATA") );
	vec->AddDataValuePair( dvp );
	serviceList->SendCommandStringToXplorer( vec );
	delete vec;

	//Now laod the xml data now that we are in the correct directory
	SubmitToServer( event );      
	network->Load( ConvertUnicode( path.c_str() ) );
	SubmitToServer( event );      	
}
///////////////////////////////////////////////////////////////////////////
void AppFrame::LoadFromServer( wxCommandEvent& WXUNUSED(event) )
{
   if ( !serviceList->IsConnectedToCE() )
   {
      return;
   }
   EnableCEGUIMenuItems();

   std::string nw_str;
   nw_str = serviceList->GetNetwork();
   network->Load( nw_str );
}
///////////////////////////////////////////////////////////////////////////
void AppFrame::QueryFromServer( wxCommandEvent& WXUNUSED(event) )
{
   if ( !serviceList->IsConnectedToCE() )
   {
      return;
   }
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
      network->Load( nw_str );
   }
   else
   {
      Log("No ves network available\n");
   }
}
///////////////////////////////////////////////////////////////////////////
void AppFrame::QueryNetwork( wxCommandEvent& WXUNUSED(event) )
{
   Log("Opening Simulation...\n");
   wxFileName bkpFileName;
   wxTextEntryDialog newDataSetName(this, 
	   wxString("Enter the prefix for *.bkp filename:", wxConvUTF8),
	   wxString("Open BKP Filename", wxConvUTF8),
	   wxString("", wxConvUTF8),wxOK|wxCANCEL);
   if ( newDataSetName.ShowModal() == wxID_OK )
   {
	   bkpFileName.ClearExt();
	   bkpFileName.SetName( newDataSetName.GetValue() ); 
	   //bkpFileName.SetExt( wxString( "bkp", wxConvUTF8 ) );

	   VE_XML::Command returnState;
	   returnState.SetCommandName("getNetwork");
	   VE_XML::DataValuePair* data = returnState.GetDataValuePair(-1);
	   data->SetData("NetworkQuery", "getNetwork" );
	   data = returnState.GetDataValuePair(-1);
	   data->SetData("BKPFileName",  ConvertUnicode( bkpFileName.GetFullName().c_str() ) );

	   std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
	   nodes.push_back(std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ));
	   VE_XML::XMLReaderWriter commandWriter;
	   std::string status="returnString";
	   commandWriter.UseStandaloneDOMDocumentManager();
	   commandWriter.WriteXMLDocument( nodes, status, "Command" );
	   //Get results
	   std::string nw_str = serviceList->Query( status );

	   //Log(nw_str.c_str());
	   // If there is nothing on the CE
	   if ( !nw_str.empty() )
	   {
		   if(network->modules.empty())
		  { 
			   network->Load( nw_str );
			   Log("Simulation Opened.\n");
		   }
		   else
			   Log("Simulation is already open.\n");
	   }
	   else
	   {
	   Log("Don't\n");
		  Log("No ves network available\n");
	   }
   }
}

///////////////////////////////////////////////////////////////////////////
void AppFrame::ShowAspenSimulation( wxCommandEvent& WXUNUSED(event) )
{
   Log("Show Simulation.\n");
   VE_XML::Command returnState;
   returnState.SetCommandName("showSimulation");
   VE_XML::DataValuePair* data = returnState.GetDataValuePair(-1);
   data->SetData("NetworkQuery", "showSimulation" );

   std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
   nodes.push_back(std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ));
   
   VE_XML::XMLReaderWriter commandWriter;
   std::string status="returnString";
   commandWriter.UseStandaloneDOMDocumentManager();
   commandWriter.WriteXMLDocument( nodes, status, "Command" );

   std::string nw_str = serviceList->Query( status ) + "\n";
   Log(nw_str.c_str());
}

///////////////////////////////////////////////////////////////////////////
void AppFrame::HideAspenSimulation( wxCommandEvent& WXUNUSED(event) )
{
   Log("Hide Simulation.\n");
   VE_XML::Command returnState;
   returnState.SetCommandName("hideSimulation");
   VE_XML::DataValuePair* data = returnState.GetDataValuePair(-1);
   data->SetData("NetworkQuery", "hideSimulation" );

   std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
   nodes.push_back(std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ));
   
   VE_XML::XMLReaderWriter commandWriter;
   std::string status="returnString";
   commandWriter.UseStandaloneDOMDocumentManager();
   commandWriter.WriteXMLDocument( nodes, status, "Command" );
   
   std::string nw_str = serviceList->Query( status ) + "\n";
   Log(nw_str.c_str());
}

///////////////////////////////////////////////////////////////////////////
void AppFrame::CloseAspenSimulation( wxCommandEvent& WXUNUSED(event) )
{
   Log("Close Simulation.\n");
   VE_XML::Command returnState;
   returnState.SetCommandName("closeSimulation");
   VE_XML::DataValuePair* data = returnState.GetDataValuePair(-1);
   data->SetData("NetworkQuery", "closeSimulation" );

   std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
   nodes.push_back(std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ));
   
   VE_XML::XMLReaderWriter commandWriter;
   std::string status="returnString";
   commandWriter.UseStandaloneDOMDocumentManager();
   commandWriter.WriteXMLDocument( nodes, status, "Command" );
   
   std::string nw_str = serviceList->Query( status ) + "\n";
   Log(nw_str.c_str());
}

///////////////////////////////////////////////////////////////////////////
void AppFrame::RunAspenNetwork( wxCommandEvent& WXUNUSED(event) )
{
   Log("Run Simulation.\n");
   VE_XML::Command returnState;
   returnState.SetCommandName("runNetwork");
   VE_XML::DataValuePair* data = returnState.GetDataValuePair(-1);
   data->SetData("NetworkQuery", "runNetwork" );

   std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
   nodes.push_back(std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ));
   
   VE_XML::XMLReaderWriter commandWriter;
   std::string status="returnString";
   commandWriter.UseStandaloneDOMDocumentManager();
   commandWriter.WriteXMLDocument( nodes, status, "Command" );
   
   serviceList->Query( status );
}

///////////////////////////////////////////////////////////////////////////
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
   
   //highlight the selected icon
   network->HighlightSelectedIcon(network->modules[moduleIDs[selectedModulePos]].GetPlugin());
   
   //recenter the flowsheet around the icon
   int xPix, yPix;
   network->GetScrollPixelsPerUnit(&xPix, &yPix);
   network->Scroll(network->modules[moduleIDs[selectedModulePos]].GetPlugin()->GetBBox().GetX()/(xPix),
                   network->modules[moduleIDs[selectedModulePos]].GetPlugin()->GetBBox().GetY()/(yPix));
}

///////////////////////////////////////////////////////////////////////////
void AppFrame::New( wxCommandEvent& WXUNUSED(event) )
{
   path.clear();
   network->New();
}
///////////////////////////////////////////////////////////////////////////
void AppFrame::SubmitToServer( wxCommandEvent& WXUNUSED(event) )
{
   if ( !serviceList->IsConnectedToCE() )
   {
      return;
   }
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
      run_menu->Enable( v21ID_START_CALC, true );
   }
   catch ( CORBA::Exception& ) 
   {
      Log("no exec found!\n");
   }
}

void AppFrame::StartCalc( wxCommandEvent& WXUNUSED(event) )
{
   try	
   { 
      serviceList->StartCalc();
      run_menu->Enable(v21ID_START_CALC, false);
   }
   catch ( CORBA::Exception& ) 
   {
      Log("no exec found!\n");
   }
}

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
               Package p;
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

               /*result_dlg->syngas->AddSeperator(' ');
               result_dlg->syngas->AddSeperator('+');
               result_dlg->syngas->AddSeperator(' ');*/
               result_dlg->Set2Cols(v_desc, v_value);
	         }
         }
    
         //result = network->exec->GetModuleResult(-1); //Global result
      
         if (std::string(result)!="") 
         {
            Package p;
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
	         /*result_dlg->syngas->AddSeperator(' ');
	         result_dlg->syngas->AddSeperator('+');
	         result_dlg->syngas->AddSeperator(' ');*/
	         result_dlg->Set2Cols(v_desc, v_value);
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
	         //sprintf(buf,"%.2f", cccost);
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
	         //sprintf(buf,"%.2f", omcost);
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
      //sprintf(buf,"%.2f", total_cccost);
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
      //sprintf(buf,"%.2f", total_omcost);
      dirStringStream << std::setprecision(2) << total_omcost;
      v_value2.push_back( wxString( dirString.c_str(), wxConvUTF8 ) );

      result_dlg->Set2Cols(v_desc2, v_value2);
   }

   result_dlg->Show();
}
////////////////////////////////////////////////////////////////////
void AppFrame::GlobalParam(wxCommandEvent& WXUNUSED(event) )
{
   if ( network->globalparam_dlg != NULL )
   {
      network->globalparam_dlg->Show();
   }
   else
   {
      network->globalparam_dlg = new GlobalParamDialog( this, -1 );
      network->globalparam_dlg->Show();
   }
}
///////////////////////////////////////////////////////////////////
void AppFrame::LoadBase(wxCommandEvent &WXUNUSED(event))
{
  network->Load("IECMBase.nt");
}
///////////////////////////////////////////////////////////////////
void AppFrame::LoadSour(wxCommandEvent &WXUNUSED(event))
{
  network->Load("IECMSour.nt");
}
///////////////////////////////////////////////////////////////////
void AppFrame::LoadREIBase(wxCommandEvent &WXUNUSED(event))
{
  network->Load("REIBase.nt");
}
///////////////////////////////////////////////////////////////////
void AppFrame::LoadREISour(wxCommandEvent &WXUNUSED(event))
{
  network->Load("REISour.nt");
}
///////////////////////////////////////////////////////////////////
void AppFrame::Log(const char* msg)
{
   serviceList->GetMessageLog()->SetMessage( msg );
}
///////////////////////////////////////////////////////////////////
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
//////////////////////////////////////////////////////////////////
void AppFrame::DisConVEServer(wxCommandEvent &WXUNUSED(event))
{
   serviceList->DisconnectFromXplorer();
   con_menu->Enable(v21ID_DISCONNECT_VE, false);
}
//////////////////////////////////////////////////////////////////
void AppFrame::ViewHelp(wxCommandEvent& WXUNUSED(event))
{
   ::wxLaunchDefaultBrowser( wxString( "http://www.vrac.iastate.edu/%7Ebiv/vesuite_installs/docs/releases/1.0.3/vesuite/vesuite.html", wxConvUTF8 ) );
}
//////////////////////////////////////////////////////////////////
void AppFrame::ViewAbout(wxCommandEvent& WXUNUSED(event))
{
   ::wxLaunchDefaultBrowser( wxString( "http://www.vesuite.org", wxConvUTF8 ) );
}
//////////////////////////////////////////////////////////////////
void AppFrame::ViewRevision(wxCommandEvent& WXUNUSED(event))
{
   wxMessageBox( _("Current Revision: 6491"),_("Revision"), 
                 wxOK | wxICON_INFORMATION );
}
//////////////////////////////////////////////////////////////////
void AppFrame::ViewContacts(wxCommandEvent& WXUNUSED(event))
{
   ::wxLaunchDefaultBrowser( wxString( "http://www.vesuite.org/forum/index.php", wxConvUTF8 ) );
}
//////////////////////////////////////////////////////////////////
void AppFrame::ViewPlatformInfo(wxCommandEvent& WXUNUSED(event))
{
   wxMessageBox( ::wxGetOsDescription(),_("Platform Info"), 
                 wxOK | wxICON_INFORMATION );
}
///////////////////////////////////////////////////////////////////
void AppFrame::CloseVE()
{
   ;
}
///////////////////////////////////////////////////////////////////
void AppFrame::LaunchDeviceProperties( wxCommandEvent& WXUNUSED(event) )
{
   if ( deviceProperties == 0 )
   {
      // create pane and set appropriate vars
      deviceProperties = new DeviceProperties();
   }
   // now show it
   deviceProperties->Show();
}
////////////////////////////////////////////////////////////////////
void AppFrame::LaunchNavigationPane( wxCommandEvent& WXUNUSED(event) )
{
   if ( navPane == 0 )
   {
      // create pane and set appropriate vars
      navPane = new NavigationPane();
   }
   // now show it
   navPane->Show();
}
////////////////////////////////////////////////////////////////////
void AppFrame::SetBackgroundColor( wxCommandEvent& WXUNUSED(event) )
{
   //this is kinda confusing...thanks wx!!!
   //wxColourData data;
   //data.SetChooseFull(true);

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
      VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair( );
      dataValuePair->SetData(std::string("Background Color"),xplorerColor);
      VE_XML::Command* veCommand = new VE_XML::Command();
      veCommand->SetCommandName(std::string("CHANGE_BACKGROUND_COLOR"));
      veCommand->AddDataValuePair(dataValuePair);

      serviceList->SendCommandStringToXplorer( veCommand );
   
      delete veCommand;
   }
}
///////////////////////////////////////////////////////////////////
void AppFrame::ChangeDevice( wxCommandEvent& WXUNUSED(event) )
{
   //Create the command and data value pairs
   VE_XML::DataValuePair* DVP = new VE_XML::DataValuePair();
   VE_XML::Command* command = new VE_XML::Command();
   
   std::string device;

   if( xplorerDeviceMenu->IsChecked( WAND ) )
   {
      device = "Wand";
   }

   else if( xplorerDeviceMenu->IsChecked( KEYBOARD_MOUSE ) )
   {
      device = "Keyboard Mouse";
   }

   DVP->SetData( std::string( "Device" ), device );
   
   command->SetCommandName( std::string( "CHANGE_DEVICE" ) );
   command->AddDataValuePair( DVP );

   serviceList->SendCommandStringToXplorer( command );
   
   delete command;
}
///////////////////////////////////////////////////////////////////
void AppFrame::ChangeDeviceMode( wxCommandEvent& WXUNUSED(event) )
{
   //Create the command and data value pairs
   VE_XML::DataValuePair* DVP = new VE_XML::DataValuePair();
   VE_XML::Command* command = new VE_XML::Command();
   
   unsigned int mode;

   if( xplorerDeviceModeMenu->IsChecked( NAVIGATION_MODE ) )
   {
      mode = 0;
   }

   else if( xplorerDeviceModeMenu->IsChecked( SELECTION_MODE ) )
   {
      mode = 1;
   }

   DVP->SetData( std::string( "Mode" ), mode );
   
   command->SetCommandName( std::string( "CHANGE_DEVICE_MODE" ) );
   command->AddDataValuePair( DVP );

   serviceList->SendCommandStringToXplorer( command );
   
   delete command;
}
///////////////////////////////////////////////////////////////////
void AppFrame::DisplaySelection( wxCommandEvent& event )
{
   //Create the command and data value pairs
   VE_XML::DataValuePair* DVP=new VE_XML::DataValuePair();
   VE_XML::Command* command=new VE_XML::Command();

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
   
   delete command;
}
///////////////////////////////////////////////////////////////////
void AppFrame::ViewSelection( wxCommandEvent& event )
{
   //Create the command and data value pairs
   VE_XML::DataValuePair* DVP=new VE_XML::DataValuePair();
   VE_XML::Command* command=new VE_XML::Command();
   
   long int value;

   if(event.GetId() == FRAME_ALL)
   {
      value=0;
   }
   else if(event.GetId() == FRAME_SELECTION)
   {
      value=1;
   }
   else if(event.GetId() == RESET)
   {
      value=2;
   }
   else
   {
      value=-1;
   }

   DVP->SetData(std::string("ViewID"),value);
   
   command->SetCommandName(std::string("VIEW_SELECTION"));
   command->AddDataValuePair(DVP);

   serviceList->SendCommandStringToXplorer(command);
   
   delete command;
}
///////////////////////////////////////////////////////////////////
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
///////////////////////////////////////////////////////////////////
void AppFrame::LaunchViewpointsPane( wxCommandEvent& WXUNUSED(event) )
{
   if ( viewlocPane == 0 )
   {
      // create pane and set appropriate vars
      viewlocPane = new ViewLocPane( );
   }
   else
   {
      // set pointer to corba object for comm
      viewlocPane->SetCommInstance( GetXplorerObject() );
   }
   // now show it
   viewlocPane->Show();
}

///////////////////////////////////////////////////////////////////
void AppFrame::LaunchSoundsPane( wxCommandEvent& WXUNUSED( event ) )
{
   if ( soundsPane == 0 )
   {
      // create pane and set appropriate vars
      soundsPane = new SoundsPane( GetXplorerObject(), domManager );
   }
   else
   {
      // set pointer to corba object for comm
      soundsPane->SetCommInstance( GetXplorerObject() );
   }
   // now show it
   soundsPane->Show();
}
/////////////////////////////////////////////////////////////////
void AppFrame::LaunchCADNodePane( wxCommandEvent& WXUNUSED( event ) )
{
   if( !_cadDialog)
   {
      //this will change once we have a way to retrieve the geometry from the model
      _cadDialog = new VE_Conductor::GUI_Utilities::CADNodeManagerDlg(0,
                                                               this,CAD_NODE_DIALOG);
   }
   /*
   if(_activeModel)
   {   
      //this will change once we have a way to retrieve the geometry from the model
      //_cadDialog->SetRootCADNode(_activeModel->GetGeometry());
   }
   */
   _cadDialog->SetVjObsPtr( GetXplorerObject() );
   _cadDialog->Show();
}
///////////////////////////////////////////////////////////////////
void AppFrame::JugglerSettings( wxCommandEvent& event )
{
   // Create the command and data value pairs
   VE_XML::DataValuePair* dataValuePair = 
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
   VE_XML::Command* veCommand = new VE_XML::Command();
   veCommand->SetCommandName( std::string("Juggler_Display_Data") );
   veCommand->AddDataValuePair( dataValuePair );

   serviceList->SendCommandStringToXplorer( veCommand );
   
   delete veCommand;
}
///////////////////////////////////////////////////////////////////
VjObs_ptr AppFrame::GetXplorerObject( void )
{
   return serviceList->GetXplorerPointer();
}
///////////////////////////////////////////////////////////////////
void AppFrame::IdleEvent( wxIdleEvent& event )
{
   if ( serviceList )
      serviceList->CheckORBWorkLoad();
}
///////////////////////////////////////////////////////////////////
void AppFrame::TimerEvent( wxTimerEvent& WXUNUSED(event) )
{
   ::wxWakeUpIdle();
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ExitXplorer( void )
{
   VE_XML::DataValuePair* dataValuePair = 
                           new VE_XML::DataValuePair( std::string("STRING") );
   dataValuePair->SetData( "EXIT_FLAG", "EXIT" );
   VE_XML::Command* veCommand = new VE_XML::Command();
   veCommand->SetCommandName( std::string( "EXIT_XPLORER" ) );
   veCommand->AddDataValuePair( dataValuePair );
   
   serviceList->SendCommandStringToXplorer( veCommand );
   
   delete veCommand;
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

   // we must make this call here because
   // new sets the path to null
   wxCommandEvent event;
   New( event );

   path = vesFileName.GetFullPath();
   path.Replace( _("\\"), _("/"), true );
   directory = vesFileName.GetPath();
   //change conductor working dir
   ::wxSetWorkingDirectory( directory );
   path = vesFileName.GetFullName();
   
   //Send Command to change xplorer working dir
   // Create the command and data value pairs
   VE_XML::DataValuePair* dataValuePair = 
      new VE_XML::DataValuePair(  std::string("STRING") );
   dataValuePair->SetData( "WORKING_DIRECTORY", ConvertUnicode( directory.c_str() ) );
   VE_XML::Command* veCommand = new VE_XML::Command();
   veCommand->SetCommandName( std::string("Change Working Directory") );
   veCommand->AddDataValuePair( dataValuePair );
   serviceList->SendCommandStringToXplorer( veCommand );
   delete veCommand;
   
   //Now laod the xml data now that we are in the correct directory
   fname=vesFileName.GetFullName();
   // we submit after new to make sure that the ce and ge ar cleared
   SubmitToServer( event );      
   network->Load( ConvertUnicode( path.c_str() ) );
   // we submit after load to give ce and ge the new network
   SubmitToServer( event );
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::EnableCEGUIMenuItems( void )
{
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
   wxRect dialogPosition( 100, 50, 100, 100 );
   preferences->SetSize( dialogPosition );
   preferences->ShowModal();
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::ChangeXplorerViewSettings( wxCommandEvent& event )
{
   VE_XML::DataValuePair* dataValuePair = 
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
   
   VE_XML::Command* veCommand = new VE_XML::Command();
   veCommand->SetCommandName( std::string("CHANGE_XPLORER_VIEW") );
   veCommand->AddDataValuePair( dataValuePair );
   serviceList->SendCommandStringToXplorer( veCommand );
   delete veCommand;
}
////////////////////////////////////////////////////////////////////////////////
std::vector< double >  AppFrame::GetXplorerBackgroundColor( void )
{
   return xplorerColor;
}
