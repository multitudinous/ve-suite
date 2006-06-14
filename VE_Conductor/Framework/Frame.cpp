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
 * File:          $RCSfile: Frame.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Framework/Frame.h"
#include <wx/imaglist.h>
#include <wx/artprov.h>
#include <wx/msgdlg.h>

#include "VE_Conductor/Framework/ResultPanel.h"
#include "VE_Conductor/Framework/App.h"
#include "VE_Conductor/Framework/package.h"
#include "VE_Conductor/Framework/OrbThread.h"
#include "VE_Conductor/Framework/Avail_Modules.h"
#include "VE_Conductor/Framework/FinancialDialog.h"
#include "VE_Conductor/Framework/TextResultDialog.h"
#include "VE_Conductor/Framework/TexTable.h"
#include "VE_Conductor/Framework/GlobalParamDialog.h"
#include "VE_Conductor/Framework/SummaryResultDialog.h"
#include "VE_Conductor/Framework/NavigationPane.h"
#include "VE_Conductor/Framework/SoundsPane.h"
#include "VE_Conductor/Framework/StreamersPane.h"

#include "VE_Conductor/Framework/vectors.h"
#include "VE_Conductor/Framework/vistab.h"

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

#include "VE_Conductor/VE_UI/UI_Tabs.h"
#include "VE_Conductor/VE_UI/UI_Frame.h"
#include "VE_Conductor/Framework/Network.h"
#include "VE_Conductor/Framework/CORBAServiceList.h"

#include "VE_Conductor/Utilities/Module.h"
#include "VE_Conductor/Utilities/Tag.h"

#include <wx/image.h>
#include <wx/bitmap.h>
#include <wx/splash.h>
#include <wx/utils.h>
#include "VE_Installer/installer/installerImages/ve_ce_banner.xpm"
#include "VE_Installer/installer/installerImages/ve_xplorer_banner.xpm"
#include <sstream>
#include <iomanip>

using namespace VE_Conductor::GUI_Utilities;
using namespace VE_Conductor;
using namespace VE_XML;
using namespace VE_CAD;
using namespace VE_Shader;

#ifdef WIN32
#include <shellapi.h>
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
   EVT_MENU(wxID_OPEN, AppFrame::Open)
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
   EVT_MENU(v21ID_VIEW_RESULT, AppFrame::ViewResult)

   EVT_MENU( XPLORER_NAVIGATION, AppFrame::LaunchNavigationPane )
   EVT_MENU( XPLORER_VIEWPOINTS, AppFrame::LaunchViewpointsPane )
   //  EVT_MENU( XPLORER_VIEWPOINTS, AppFrame::LaunchSoundsPane )
   EVT_MENU( XPLORER_SOUNDS, AppFrame::LaunchSoundsPane )
   EVT_MENU( JUGGLER_STEREO, AppFrame::JugglerSettings )
   EVT_MENU( CAD_NODE_DIALOG, AppFrame::LaunchCADNodePane )
//   EVT_MENU( XPLORER_VISTABS, AppFrame::LaunchVisTabs ) 
//   EVT_MENU( XPLORER_STREAMLINE, AppFrame::LaunchStreamlinePane )
//   EVT_MENU( XPLORER_VISTAB, AppFrame::LaunchVistab )   

   //  EVT_MENU(v21ID_GLOBAL_PARAM, AppFrame::GlobalParam)
   //  EVT_MENU(v21ID_BASE, AppFrame::LoadBase)
   //  EVT_MENU(v21ID_SOUR, AppFrame::LoadSour)
   //  EVT_MENU(v21ID_REI_BASE, AppFrame::LoadREIBase)
   //  EVT_MENU(v21ID_REI_SOUR, AppFrame::LoadREISour)

   ///This call back is used by OrbThread
   ///It allows printing text to the message box below conductor
   EVT_UPDATE_UI(7777, AppFrame::OnUpdateUIPop)
   EVT_IDLE( AppFrame::IdleEvent )
END_EVENT_TABLE()

AppFrame::AppFrame(wxWindow * parent, wxWindowID id, const wxString& title)
  :wxFrame(parent, id, title), m_frameNr(0), f_financial(true), f_geometry(true), f_visualization(true)
{
   serviceList = new CORBAServiceList( this );
   SetWindowStyle(wxDEFAULT_FRAME_STYLE & ~ (wxRESIZE_BORDER | wxRESIZE_BOX | wxMAXIMIZE_BOX));

  
   xplorerMenu = 0;
   
   this->SetIcon( wxIcon( ve_xplorer_banner_xpm ) );
  
   //int displayWidth, displayHeight = 0;
   //::wxDisplaySize(&displayWidth,&displayHeight);

   m_frame = 0;
   is_orb_init= false;
   connectToVE = false;
   connectToCE = false;
   _treeView = 0;
   _displayMode = "Tablet";
   _detectDisplayAndCreate();
   
   GetConfig(NULL);
   
   CreateMenu();
   CreateStatusBar();
   SetStatusText("VE-Conductor Status");

   navPane = 0;
   soundsPane = 0;
   viewlocPane = 0;

   _cadDialog = 0;

   domManager = new VE_XML::DOMDocumentManager();
   
   ///Initialize VE-Open
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "XML",new VE_XML::XMLCreator() );
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "Shader",new VE_Shader::ShaderCreator() );
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "Model",new VE_Model::ModelCreator() );
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "CAD",new VE_CAD::CADCreator() );
}
//////////////////////////////////////
std::string AppFrame::_detectDisplay()
{
   for ( int i = 1; i < wxTheApp->argc ; ++i )
   {
      if ( (std::string( wxTheApp->argv[i] ) == std::string("-VESDesktop")))
      {
         _displayMode = std::string("Desktop");
         break;
      }
   }
   return _displayMode;
}
////////////////////////////////////////////////////////
void AppFrame::_createTreeAndLogWindow(wxWindow* parent)
{
   wx_log_splitter = new wxSplitterWindow(parent, -1);
   wx_log_splitter->SetMinimumPaneSize( 40 );
   logwindow = new wxTextCtrl(wx_log_splitter, MYLOG, "", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY);

   wx_nw_splitter = new wxSplitterWindow(wx_log_splitter, -1);
   wx_nw_splitter->SetMinimumPaneSize( 20 );

   av_modules = new Avail_Modules(wx_nw_splitter, TREE_CTRL, wxDefaultPosition, wxDefaultSize, wxTR_HAS_BUTTONS);
   network = new Network(wx_nw_splitter, -1 );
   av_modules->SetNetwork(network);

   wx_log_splitter->SplitHorizontally(wx_nw_splitter, logwindow, -100);
   wx_nw_splitter->SplitVertically(av_modules, network, 140);
}
//////////////////////////////////
void AppFrame::_configureDesktop()
{
   _treeView = new wxDialog(this, -1, "Available Objects", 
                                 wxDefaultPosition, wxDefaultSize,
                                 (wxDEFAULT_DIALOG_STYLE&~ (wxCLOSE_BOX | wxRESIZE_BORDER | wxRESIZE_BOX | wxMAXIMIZE_BOX)));//|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX|wxCLOSE_BOX));
   wxBoxSizer* treeViewSizer = new wxBoxSizer(wxHORIZONTAL);

   _treeView->SetAutoLayout(true);
   _treeView->SetSizer(treeViewSizer);

   _createTreeAndLogWindow(_treeView);
   treeViewSizer->Add(wx_log_splitter,1, wxALIGN_CENTER|wxEXPAND);

   int displayWidth, displayHeight = 0;
   ::wxDisplaySize(&displayWidth,&displayHeight);

   SetSize(wxSize(displayWidth,75/*displayHeight*0.0732421875*/));
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
   if(_detectDisplay() == "Desktop")
   {
      _configureDesktop();
   }
   else if(_detectDisplay() == "Tablet")
   {
      _configureTablet();
   }
   else
   {
      wxMessageBox( "Unable to create GUI.","Unknown display request!", 
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
      wxRect bbox = wxTheApp->GetTopWindow()->GetRect();

      wxRect dialogPosition( 2*displayWidth/3, bbox.GetBottomRight().y, 
                        displayWidth/3, .5*(displayHeight-bbox.GetBottomRight().y) );
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

void AppFrame::GetConfig(wxConfig* config)
{
  wxConfig* cfg = config;
  if (!config) cfg = new wxConfig (wxTheApp->GetAppName());
  int i;
  
  wxString key = FEATURE;
  if (cfg->Exists (key)) 
  {
      bool exist = false;
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
 //}
  if (!config) delete cfg;

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
  wxString key = LOCATION + wxString::Format ("%d", 0);
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
  wxString key = LOCATION + wxString::Format ("%d", m_frameNr);
  cfg->Write (key + _T("/") + LOCATION_X, rect.x);
  cfg->Write (key + _T("/") + LOCATION_Y, rect.y);
  cfg->Write (key + _T("/") + LOCATION_W, rect.width);
  cfg->Write (key + _T("/") + LOCATION_H, rect.height);
  
  if (!config) delete cfg;
}

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

void AppFrame::OnClose(wxCloseEvent& WXUNUSED(event) )
{
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

   file_menu->Append(wxID_NEW, _("&New\tCtrl+N"));
   file_menu->Append(wxID_OPEN, _("&Open ..\tCtrl+O"));
   file_menu->Append(wxID_SAVE, _("&Save\tCtrl+S"));
   file_menu->Append(wxID_SAVEAS, _("Save &as ..\tCtrl+Shift+S"));
   file_menu->AppendSeparator();
   file_menu->Append (wxID_PRINT_SETUP, _("Print Set&up .."));
   file_menu->Append (wxID_PREVIEW, _("Print Pre&view\tCtrl+Shift+P"));
   file_menu->Append (wxID_PRINT, _("&Print ..\tCtrl+P"));
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
   con_menu->Append(QUERY_FROM_SERVER, _("&Query\tCtrl+U"));
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

   help_menu->Append(wxID_HELP_CONTENTS, _("&Content\tF1"));
   help_menu->Append (v21ID_HELP, _("&Index"));
   help_menu->AppendSeparator();
   help_menu->Append (wxID_ABOUT, _("&About ..\tShift+F1"));

   help_menu->Enable(wxID_HELP_CONTENTS, false);
   //help_menu->Enable(v21ID_HELP, false);
   help_menu->Enable(wxID_ABOUT, false);

   //if (f_visualization)
   {
	xplorerMenu = new wxMenu();
	xplorerJugglerMenu = new wxMenu();

	xplorerJugglerMenu->Append( JUGGLER_STEREO, _("Stereo") );
	xplorerJugglerMenu->Append( JUGGLER_MONO, _("Mono") );
	xplorerJugglerMenu->Enable( JUGGLER_STEREO, true);
	xplorerJugglerMenu->Enable( JUGGLER_MONO, true);

	xplorerMenu->Append( XPLORER_NAVIGATION, _("Navigation Pane") );
	xplorerMenu->Append( XPLORER_VIEWPOINTS, _("Viewpoints Pane") );
	xplorerMenu->Append( XPLORER_SOUNDS, _("Sounds Pane") );
//	xplorerMenu->Append( XPLORER_STREAMLINE, _("Streamline Pane") );
	xplorerMenu->Append( JUGGLER_SETTINGS, _("Juggler Settings"), xplorerJugglerMenu, _("Used to adjust juggler runtime settings") );
	//xplorerMenu->Append( CAD_NODE_DIALOG, _("CAD Hierarchy"));
//	xplorerMenu->Append( XPLORER_VISTABS, _("Vis Tabs"));
//   xplorerMenu->Append( XPLORER_VISTAB, _("Visualization Tabs"));

	xplorerMenu->Enable( XPLORER_NAVIGATION, true);
	xplorerMenu->Enable( XPLORER_VIEWPOINTS, true);
	xplorerMenu->Enable( XPLORER_SOUNDS, true);
//	xplorerMenu->Enable( XPLORER_STREAMLINE, true);
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
   menubar->Append(help_menu, _("&Help"));
   //if (f_visualization)
   menubar->Append( xplorerMenu, _("&VE-Xplorer") );

   SetMenuBar(menubar);
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
   if ( path == wxString("") ) 
   {
      SaveAs( event );
   }
   else
   {
      //domManager->CreateCommandDocument();
      //DOMDocument* doc = domManager->GetCommandDocument();
      //network->Save( doc );
      ///now write the file out from domdocument manager
      //wrtie to path
      std::string data = network->Save( std::string( path.c_str() ) );
   }
}

void AppFrame::SaveAs( wxCommandEvent& WXUNUSED(event) )
{
   wxFileDialog dialog
   (
      this,
      _T("Save File dialog"),
      _T(""),
      fname,
      _T("Network files (*.nt)|*.nt"),
      wxSAVE | wxOVERWRITE_PROMPT 
   );

   /*if ( directory == "" )
   {
      dialog.SetDirectory( wxGetHomeDir() );
   }*/

   if ( dialog.ShowModal() == wxID_OK )
   {
      path = dialog.GetPath();
      directory = dialog.GetDirectory();
      fname = dialog.GetFilename();
      ///now write the file out from domdocument manager
      //wrtie to path
      std::string data = network->Save( std::string( path.c_str() ) );
   }
}

void AppFrame::Open(wxCommandEvent& WXUNUSED(event))
{
   wxFileDialog dialog
                  (
                     this,
                     _T("Open File dialog"),
                     _T(""),
                     fname,
                     _T("Network files (*.nt)|*.nt"),
                     wxOPEN|wxFILE_MUST_EXIST
                  );

   //if ( directory=="" )
   //   dialog.SetDirectory(wxGetHomeDir());

   if (dialog.ShowModal() == wxID_OK)
   {
      path=dialog.GetPath();
      directory=dialog.GetDirectory();
      fname=dialog.GetFilename();
      network->Load( path.c_str() );
   }
}
///////////////////////////////////////////////////////////////////////////
void AppFrame::LoadFromServer( wxCommandEvent& WXUNUSED(event) )
{
   //ConExeServer();
   // If not sucessful
   //if ( !is_orb_init )
   //   return;
   serviceList->IsConnectedToCE();

   char* nw_str = 0;
   try
   {
      nw_str=network->exec->GetNetwork();
   }
   catch ( CORBA::Exception& )
   {
      Log("No VE_CE found!\n");
   }

   network->Load( nw_str );
   delete nw_str;
}
///////////////////////////////////////////////////////////////////////////
void AppFrame::QueryFromServer( wxCommandEvent& WXUNUSED(event) )
{
   /*ConExeServer();
   // If not sucessful
   if ( !is_orb_init )
      return;
   */
   serviceList->IsConnectedToCE();
   std::string nw_str;

   try
   {
      nw_str.assign( network->exec->Query() );
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
void AppFrame::New( wxCommandEvent& WXUNUSED(event) )
{
   network->New();
}
///////////////////////////////////////////////////////////////////////////
void AppFrame::SubmitToServer( wxCommandEvent& WXUNUSED(event) )
{
   //ConExeServer();
   // If not sucessful
   //if ( !is_orb_init )
   //   return;
   serviceList->IsConnectedToCE();
   
   std::string nw_str = network->Save( std::string( "returnString" ) );
   // write the domdoc to the string above
   try 
   {
      network->exec->SetNetwork( nw_str.c_str() );
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
      network->exec->StartCalc();
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
      network->exec->StopCalc();
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
      network->exec->PauseCalc();
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
      network->exec->Resume();
   }
   catch ( CORBA::Exception& )
   {
      Log("no exec found!\n");
   }
}

void AppFrame::ViewResult(wxCommandEvent& WXUNUSED(event) )
{
   /*ConExeServer();
   // If not sucessful
   if ( !is_orb_init )
      return;
   */
   serviceList->IsConnectedToCE();
   
   char* result;
   //char buf[80];
   std::map<int, Module>::iterator iter;
   unsigned int i;
   std::vector<wxString> titles;
   //TextResultDialog * result_dlg;
   SummaryResultDialog * result_dlg;
   std::vector<wxString> v_desc, v_value;
   std::vector<std::string> descs;
   std::vector<int> alignments;

   titles.push_back("Description");
   alignments.push_back (wxALIGN_LEFT);
   titles.push_back("Value");
   alignments.push_back (wxALIGN_RIGHT);

   /*
   result_dlg = new TextResultDialog(NULL);
   result_dlg->syngas->Clear();
   result_dlg->syngas->AddRow(titles);
   */
   result_dlg = new SummaryResultDialog(NULL, wxT("Result Summary"), wxSize(560, 400));
   result_dlg->syngas->Clear();
   result_dlg->syngas->SetNumofCols( 2 );
   result_dlg->syngas->SetColTitles( titles );
   result_dlg->syngas->SetColAlignments( alignments );

   if (!CORBA::is_nil( network->exec.in() )) 
   {
      try 
      {
         for (iter=network->modules.begin(); iter!=network->modules.end(); iter++) 
         {
	         result = network->exec->GetModuleResult(iter->first);
	
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
               /*
               v_desc.push_back(iter->second.GetPlugin()->GetName());
               std::ostringstream dirStringStream;
               dirStringStream << "   " << iter->first;
               std::string dirString = dirStringStream.str();
               v_value.push_back(dirString.c_str());
               */
               wxString str;
               str = iter->second.GetPlugin()->GetName();
               str << " (" << iter->first << ")";
               result_dlg->NewTab( str );

               for (i=0; i<descs.size(); i++) 
               {
	               std::string desc = descs[i];
	               std::string value = p.GetInterfaceVector()[0].getString(descs[i]);

                  if (desc.substr(0,3) == "***") 
                  {
                     desc = desc.substr(9,desc.size()-9);
                  }

                  v_desc.push_back( desc.c_str() );
                  v_value.push_back( value.c_str() );
               }

               /*result_dlg->syngas->AddSeperator(' ');
               result_dlg->syngas->AddSeperator('+');
               result_dlg->syngas->AddSeperator(' ');*/
               result_dlg->Set2Cols(v_desc, v_value);
	         }
         }
    
         result = network->exec->GetModuleResult(-1); //Global result
      
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
	            v_desc.push_back(descs[i].c_str());
	            v_value.push_back((p.GetInterfaceVector()[0].getString(descs[i])).c_str());
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
   else 
   {
      titles.clear();
      titles.push_back("No Plant Results");
      titles.push_back(" ");
  
      result_dlg->syngas->AddSeperator('+');
      result_dlg->syngas->AddRow(titles);
   }
       
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

	         v_value.push_back(dirString.c_str());

	         double TMC = TPC * iter->second.GetPlugin()->financial_dlg->_om00_d / 100;
	
	         double omcost = TMC + 
	                        iter->second.GetPlugin()->financial_dlg->_om01_d +
	                        iter->second.GetPlugin()->financial_dlg->_om02_d +
	                        iter->second.GetPlugin()->financial_dlg->_om02_d * iter->second.GetPlugin()->financial_dlg->_om03_d / 100;

	         total_omcost += omcost;
	
	         v_desc2.push_back(wxString(iter->second.GetPlugin()->GetName()));
	         //sprintf(buf,"%.2f", omcost);
            dirStringStream << std::setprecision(2) << omcost;
	         v_value2.push_back(dirString.c_str());
         }
      }
   }

   if ( v_desc.size() > 0 ) 
   {
      result_dlg->syngas->AddSeperator(' ');
      result_dlg->syngas->AddSeperator(' ');

      titles.clear();
      titles.push_back("Plant Component");
      titles.push_back("Capital Required (M$)");

      result_dlg->syngas->AddRow(titles);
      result_dlg->syngas->AddSeperator('+');

      v_desc.push_back("Total");
      //sprintf(buf,"%.2f", total_cccost);
      std::ostringstream dirStringStream;
      dirStringStream << std::setprecision(2) << total_cccost;
      std::string dirString = dirStringStream.str();
      v_value.push_back(dirString.c_str());

      result_dlg->Set2Cols(v_desc, v_value);

      //

      result_dlg->syngas->AddSeperator(' ');

      titles.clear();
      titles.push_back("Plant Component");
      titles.push_back("Revenue Required (M$)");

      result_dlg->syngas->AddRow(titles);
      result_dlg->syngas->AddSeperator('+');

      v_desc2.push_back("Total");
      //sprintf(buf,"%.2f", total_omcost);
      dirStringStream << std::setprecision(2) << total_omcost;
      v_value2.push_back(dirString.c_str());

      result_dlg->Set2Cols(v_desc2, v_value2);
   }

   result_dlg->ShowModal();

   delete result_dlg;
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
   if ( serviceList->GetMessageLog() != NULL )
   {  
      serviceList->GetMessageLog()->SetMessage( msg );
   }
}
///////////////////////////////////////////////////////////////////
void AppFrame::OnUpdateUIPop(wxUpdateUIEvent& event )
{
  logwindow->AppendText(event.GetText());
}
///////////////////////////////////////////////////////////////////
void AppFrame::DisConExeServer(wxCommandEvent &WXUNUSED(event))
{
   serviceList->DisconnectFromCE();
}
//////////////////////////////////////////////////////////////////
void AppFrame::DisConVEServer(wxCommandEvent &WXUNUSED(event))
{
   serviceList->DisconnectFromXplorer();
}
//////////////////////////////////////////////////////////////////
void AppFrame::ViewHelp(wxCommandEvent& WXUNUSED(event))
{
   char browser[1024];
   wxString help;
   wxString fgroot;
   wxString docdir;
   wxString cmd;
   fgroot = getenv("FGROOT");
  
#ifdef WIN32
   docdir=fgroot+"\\Framework\\doc";
   help = fgroot+"\\Framework\\doc\\index.html";
   FindExecutable("index.html", docdir.c_str(), browser);
#endif
   cmd="\"";
   cmd+=browser;
   cmd+="\" \"";
   cmd+=help;
   cmd+="\"";
  
   ::wxExecute(cmd, wxEXEC_ASYNC|wxEXEC_MAKE_GROUP_LEADER);
}
///////////////////////////////////////////////////////////////////
void AppFrame::CloseVE()
{
   ;
}
///////////////////////////////////////////////////////////////////
void AppFrame::LaunchNavigationPane( wxCommandEvent& WXUNUSED(event) )
{
   if ( navPane == 0 )
   {
      // create pane and set appropriate vars
      navPane = new NavigationPane( GetXplorerObject(), domManager );
   }
   else
   {
      // set pointer to corba object for comm
      navPane->SetCommInstance( GetXplorerObject() );
   }
   // now show it
   navPane->Show();
}
///////////////////////////////////////////////////////////////////
void AppFrame::LaunchViewpointsPane( wxCommandEvent& WXUNUSED(event) )
{
   if ( viewlocPane == 0 )
   {
      // create pane and set appropriate vars
      viewlocPane = new ViewLocPane( GetXplorerObject(), domManager );
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
void AppFrame::JugglerSettings( wxCommandEvent& WXUNUSED(event) )
{
   //ConVEServer();
   // Now need to construct domdocument and populate it with the new vecommand
   //VE_XML::XMLReaderWriter netowrkWriter;
   //netowrkWriter.UseStandaloneDOMDocumentManager();
   //netowrkWriter.WriteToString();

   // Create the command and data value pairs
   VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair(  std::string("FLOAT") );
   dataValuePair->SetDataName( "Stereo" );
   dataValuePair->SetDataValue( 1.0 );
   VE_XML::Command* veCommand = new VE_XML::Command();
   veCommand->SetCommandName( std::string("Juggler_Display_Data") );
   veCommand->AddDataValuePair( dataValuePair );

   serviceList->SendCommandStringToXplorer( veCommand );
   
   delete veCommand;
}
///////////////////////////////////////////////////////////////////
VjObs_ptr AppFrame::GetXplorerObject( void )
{
   return vjobs.in();
}
///////////////////////////////////////////////////////////////////
void AppFrame::IdleEvent( wxIdleEvent& event )
{
   serviceList->CheckORBWorkLoad();
}
///////////////////////////////////////////////////////////////////
CORBAServiceList* AppFrame::GetCORBAServiceList( void )
{
   return serviceList;
}
