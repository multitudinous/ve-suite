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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
/*!\file Frame.h
Frame API
*/
/*!\class Frame
* Main wx frame for conductor.
*/
#ifndef APP_FRAME_H
#define APP_FRAME_H

#include "VE_Open/skel/moduleC.h"
#include "VE_Open/skel/VjObsC.h"
#include <orbsvcs/CosNamingC.h>
#include "VE_Conductor/GUIPlugin/UI_i.h"

#include <wx/frame.h>
#include <wx/icon.h>
#include <wx/textctrl.h>
#include <wx/image.h>
#include <wx/sizer.h>
#include <wx/config.h> 
#include <wx/splitter.h>
#include <wx/timer.h>
//Frame size
const wxString LOCATION = _T("Framesize");
const wxString LOCATION_X = _T("LocationX");
const wxString LOCATION_Y = _T("LocationY");
const wxString LOCATION_W = _T("LocationW");
const wxString LOCATION_H = _T("LocationH");

//cyang Features
const wxString FEATURE = _T("Feature");
const wxString F_FINANCIAL = _T("Financial");
const wxString F_GEOMETRY = _T("Geometry");
const wxString F_VISUALIZATION = _T("Visualization");
//cyang

enum 
{
   v21ID_CONNECT,
   v21ID_CONNECT_VE,
   v21ID_SUBMIT,
   v21ID_LOAD, 
   QUERY_FROM_SERVER, 
   QUERY_NETWORK, 
   QUERY_FOR_INPUTS, 
   QUERY_FOR_RESULTS, 
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
   JUGGLER_STEREO,
   JUGGLER_MONO, 
   JUGGLER_SETTINGS,
   CAD_NODE_DIALOG,
   XPLORER_SCENES,
   XPLORER_EXIT,
   TIMER_ID,
   XPLORER_COLOR
};
//      XPLORER_VISTAB,


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
class UI_TeacherTab;
//class Vectors;
//class Vistab;

namespace VE_Conductor
{
   namespace GUI_Utilities
   {
      class CADNodeManagerDlg;
   }
}

namespace VE_Conductor
{
   class CORBAServiceList;
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
   virtual ~AppFrame() { ; }
   void OnClose( wxCloseEvent& event );
   void FrameClose( wxCommandEvent& event );
   VE_Conductor::CORBAServiceList* GetCORBAServiceList( void );

   ///Override so we can show the tree after things are initialized.  
   virtual bool Show(bool value);

   ///Get the active display mode of conductor
   ///\return Returns either:\n "Desktop" or"Tablet"
   std::string GetDisplayMode();

   ///Get an appropriate size for sub dialogs
   wxRect GetAppropriateSubDialogSize();

   wxSplitterWindow* wx_log_splitter;
   wxSplitterWindow* wx_ve_splitter;
   wxSplitterWindow* wx_nw_splitter;
   wxMenuBar* menubar;
   wxToolBar* toolbar;
   wxIcon* icon;
  
   Avail_Modules* av_modules;
   Network* network;

   UI_Tabs *m_tabs; 
   UI_Frame *m_frame;

   wxImageList *m_imageList;

   wxMenu *file_menu;
   wxMenu *con_menu;
   wxMenu *run_menu;
   wxMenu *edit_menu;
   wxMenu *help_menu;
   wxMenu* xplorerMenu;
   wxMenu* xplorerJugglerMenu;

   //configuration flags   //cyang
   bool f_financial;
   bool f_geometry;
   bool f_visualization;

   void Log(const char* msg);
   void CloseVE();

   ///Returns pointer to xplorer corba object
   VjObs_ptr GetXplorerObject( void );

   ///cyang
   void GetConfig(wxConfig* config);
   void StoreConfig(wxConfig* config);

   ///Function to process command line args to conductor and specifically
   ///to load a ves file and set the working directory appropriatley for a
   ///particular application
   void ProcessCommandLineArgs( void );
   
protected:


  int m_frameNr;
  wxString fname;
  wxString directory;
  wxString path;	
	
  VjObs_var vjobs;

  
private:
   bool is_orb_init;
   bool connectToVE;
   bool connectToCE;
   //OrbThread* ot;

protected:
   void _createTreeAndLogWindow(wxWindow* parent);
   void _configureDesktop( void );
   void _configureTablet( void );
   void _detectDisplayAndCreate( void );
   void _detectDisplay( void );
   
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

   void DisConExeServer(wxCommandEvent &event);
   void DisConVEServer(wxCommandEvent &event);
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
   void LaunchRecordScenes( wxCommandEvent& event );

   ///Set the background color in xplorer
   ///\param The command event
   void SetBackgroundColor(wxCommandEvent& event); 

   void OnExitXplorer( wxCommandEvent& event );
//   void LaunchVisTabs( wxCommandEvent& event );
//   void LaunchVistab( wxCommandEvent& event );
   ///Launch the CADNode GUI
   ///\param event The wxCommand event.
   void LaunchCADNodePane(wxCommandEvent& event);

   void JugglerSettings( wxCommandEvent& event );

  void CreateVETab();
  
  wxBoxSizer *sizerTab;

private:
     void ExitXplorer( void );
     void EnableCEGUIMenuItems( void );
   void IdleEvent( wxIdleEvent& event );
   void TimerEvent( wxTimerEvent& event );
   NavigationPane* navPane;
   SoundsPane* soundsPane;
   ViewLocPane* viewlocPane;
   UI_TeacherTab* recordScenes;
//   wxDialog* visTabs;
//   Vistab* vistab;
   wxDialog* _treeView;
   wxTimer timer;
   std::string _displayMode;///<Desktop or Tablet.

   VE_Conductor::GUI_Utilities::CADNodeManagerDlg* _cadDialog;///<The CADNode GUI.

   StreamlinePane* streamlinePane;
   VE_Conductor::CORBAServiceList* serviceList;
   VE_XML::DOMDocumentManager* domManager;

   DECLARE_EVENT_TABLE()
};
#endif
