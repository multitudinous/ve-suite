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
 * File:          $RCSfile: Frame.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef APP_FRAME_H
#define APP_FRAME_H

#include "VE_Open/skel/moduleC.h"
#include "VE_Open/skel/VjObsC.h"
#include <orbsvcs/CosNamingC.h>
#include "VE_Conductor/Framework/UI_i.h"

#include <wx/frame.h>
#include <wx/icon.h>
#include <wx/textctrl.h>
#include <wx/image.h>
#include <wx/sizer.h>
#include <wx/config.h> 
#include <wx/splitter.h>

const wxString LOCATION = _T("Framesize");
const wxString LOCATION_X = _T("LocationX");
const wxString LOCATION_Y = _T("LocationY");
const wxString LOCATION_W = _T("LocationW");
const wxString LOCATION_H = _T("LocationH");

enum 
{
   v21ID_CONNECT,
   v21ID_CONNECT_VE,
   v21ID_SUBMIT,
   v21ID_LOAD, 
   QUERY_FROM_SERVER, 
   v21ID_DISCONNECT,
   v21ID_DISCONNECT_VE, 
   v21ID_UNDO, 
   v21ID_REDO, 
   v21ID_ZOOMIN, 
   v21ID_ZOOMOUT,
   v21ID_START_CALC, 
   v21ID_STOP_CALC,
   v21ID_PAUSE_CALC,
   v21ID_RESUME_CALC, 
   v21ID_VIEW_RESULT,
   v21ID_GLOBAL_PARAM,
   v21ID_BASE,
   v21ID_SOUR,
   v21ID_REI_BASE,
   v21ID_REI_SOUR,
   v21ID_SWEET,
   v21ID_CO_DISPOSAL,
   MYLOG,
   v21ID_HELP,
   XPLORER_NAVIGATION,
   XPLORER_VIEWPOINTS,
   XPLORER_SOUNDS,
   XPLORER_STREAMLINE,
   XPLORER_VISTABS,
   JUGGLER_STEREO,
   JUGGLER_MONO, 
   JUGGLER_SETTINGS,
   CAD_NODE_DIALOG
};

class OrbThread;
class PEThread;
class Avail_Modules;
class UI_Tabs;
class UI_Frame;
class Network;
class NavigationPane;
class SoundsPane;
class ViewLocPane;
class StreamlinePane;
class CORBAServiceList;
namespace VE_Conductor
{
   namespace GUI_Utilities
   {
      class CADNodeManagerDlg;
   }
}

namespace VE_XML
{
   class DOMDocumentManager;
}

class AppFrame : public wxFrame
{
public:
  
   AppFrame(){;}
   AppFrame(wxWindow* parent, wxWindowID id, const wxString& title);
   void OnClose( wxCloseEvent& event );
   void FrameClose( wxCommandEvent& event );

  wxSplitterWindow* wx_log_splitter;
  wxSplitterWindow* wx_ve_splitter;
  wxSplitterWindow* wx_nw_splitter;
  wxMenuBar* menubar;
  wxToolBar* toolbar;
  wxIcon* icon;
  
  wxTextCtrl* logwindow;
  Avail_Modules* av_modules;
  Network* network;
  
  UI_Tabs *m_tabs; 
  UI_Frame *m_frame;
  	
  wxImageList *m_imageList;
  CORBA::ORB_var orb;
  PortableServer::POA_var poa;
  PortableServer::POA_var poa_root;
  CosNaming::NamingContext_var naming_context;
  Body_UI_i* p_ui_i;
  wxMenu *file_menu;
  wxMenu *con_menu;
  wxMenu *run_menu;
  wxMenu *edit_menu;
  wxMenu *help_menu;
   wxMenu* xplorerMenu;
   wxMenu* xplorerJugglerMenu;

  //wxMenu *config_menu;
  
  PEThread* pelog;

  void Log(const char* msg);
  //ACE_Thread_Mutex _mutex; //public mutex for the execution order
  void CloseVE();

 protected:
  int m_frameNr;
  wxString fname;
  wxString directory;
  wxString path;	
	
  VjObs_var vjobs;
  
private:
   bool is_orb_init;

protected:
   wxRect DetermineFrameSize (wxConfig* config);
   void StoreFrameSize (wxRect rect, wxConfig* config);
   void CreateMenu();
   void ZoomIn(wxCommandEvent &event);
   void ZoomOut(wxCommandEvent &evetn);
   void Save(wxCommandEvent &event);
   void SaveAs(wxCommandEvent &event);

   void Open(wxCommandEvent &event);
   void SubmitToServer(wxCommandEvent &event);
   void LoadFromServer(wxCommandEvent &event);
   void QueryFromServer( wxCommandEvent& event );
   void StartCalc(wxCommandEvent &event);	
   void StopCalc(wxCommandEvent &event);	
   void PauseCalc(wxCommandEvent &event);	
   void ResumeCalc(wxCommandEvent &event);	
   void ViewResult(wxCommandEvent &event);
   void ViewHelp(wxCommandEvent &event);
   void GlobalParam(wxCommandEvent &event);

   void ConExeServer( void );
   void DisConExeServer(wxCommandEvent &event);
   void DisConVEServer(wxCommandEvent &event);
   void ConVEServer( void );
   void LoadBase(wxCommandEvent &event);
   void LoadSour(wxCommandEvent &event);
   void LoadREIBase(wxCommandEvent &event);
   void LoadREISour(wxCommandEvent &event);
   void New(wxCommandEvent &event);

   // Controls for VE-Xplorer
   // These are the callbacks for the pull down menu
   void LaunchNavigationPane(wxCommandEvent& event);
   void LaunchViewpointsPane(wxCommandEvent& event);
   void LaunchSoundsPane(wxCommandEvent& event);
   void LaunchStreamlinePane(wxCommandEvent& event);
   void LaunchVisTabs( wxCommandEvent& event );

   ///Launch the CADNode GUI
   ///\param event The wxCommand event.
   void LaunchCADNodePane(wxCommandEvent& event);

   void JugglerSettings( wxCommandEvent& event );

  bool init_orb_naming();
  void CreateVETab();
  void OnUpdateUIPop(wxUpdateUIEvent& event);
  
  wxBoxSizer *sizerTab;

private:
   NavigationPane* navPane;
   SoundsPane* soundsPane;
   ViewLocPane* viewlocPane;
   wxDialog* visTabs;

   VE_Conductor::GUI_Utilities::CADNodeManagerDlg* _cadDialog;///<The CADNode GUI.

   StreamlinePane* streamlinePane;
   CORBAServiceList* serviceList;
   VE_XML::DOMDocumentManager* domManager;

   DECLARE_EVENT_TABLE()
};
#endif
