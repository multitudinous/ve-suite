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
#ifndef APP_FRAME_H
#define APP_FRAME_H

/*!\file Frame.h
*/

/*!\class Frame
* Main wx frame for conductor
*/

// --- VE-Suite Includes --- //

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

class UI_Frame;
class Splitter;
class Network;
class ViewLocPane;
class MainToolBar;
class Avail_Modules;
class UI_TeacherTab;
class NavigationPane;
class StreamlinePane;
class UserPreferences;
class DeviceProperties;


// --- wxWidgets Includes --- //
#include <wx/frame.h>
#include <wx/config.h>
#include <wx/filename.h>

class wxTextCtrl;
class wxImage; 
class wxSplitterWindow;
class wxDialog;
class wxImageList;
class wxSplitterWindow;
class wxColourData;
class wxMenu;
class wxFileHistory;

// --- C/C++ Includes --- //
#include <vector>
#include <string>

//Frame size
const wxString LOCATION = _T( "Framesize" );
const wxString LOCATION_X = _T( "LocationX" );
const wxString LOCATION_Y = _T( "LocationY" );
const wxString LOCATION_W = _T( "LocationW" );
const wxString LOCATION_H = _T( "LocationH" );

//cyang Features
const wxString FEATURE = _T( "Feature" );
const wxString F_FINANCIAL = _T( "Financial" );
const wxString F_GEOMETRY = _T( "Geometry" );
const wxString F_VISUALIZATION = _T( "Visualization" );
const wxString RECENT_FILE = _T( "Recent_File" );

class AppFrame : public wxFrame
{
public:
    AppFrame(){;}
    AppFrame( wxWindow* parent, wxWindowID id, const wxString& title );
    virtual ~AppFrame(){;}

   enum 
   {
      v21ID_CONNECT,
      v21ID_CONNECT_VE,
      v21ID_SUBMIT,
      v21ID_LOAD, 
      QUERY_FROM_SERVER, 
      QUERY_NETWORK,  
      RUN_ASPEN_NETWORK,
      ASPEN_CONNECTION_MENU,
      SHOW_ASPEN_SIMULATION,
      HIDE_ASPEN_SIMULATION,
      CLOSE_ASPEN_SIMULATION,
      CONDUCTOR_FIND,
      SAVE_SIMULATION,  
      SAVEAS_SIMULATION,  
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
        v21ID_ABOUT,
        v21ID_REVISION,
        v21ID_CONTACTS,
        v21ID_PLATFORM,

        XPLORER_DEVICE,
        WAND,
        KEYBOARD_MOUSE,
        DEVICE_PROPERTIES,

        XPLORER_DISPLAY,
        FRAME_RATE,
        COORDINATE_SYSTEM,

        XPLORER_VIEW,
        FRAME_ALL,
        FRAME_SELECTION,
        RESET,

        XPLORER_NAVIGATION,
        XPLORER_VIEWPOINTS,
        XPLORER_STREAMLINE,

        JUGGLER_STEREO,
        JUGGLER_MONO, 
        JUGGLER_SETTINGS,

        CAD_NODE_DIALOG,
        XPLORER_SCENES,
        XPLORER_EXIT,
        XPLORER_COLOR,
        ID_PREFERENCES,
        CHANGE_XPLORER_VIEW,
        CHANGE_XPLORER_VIEW_NETWORK,
        CHANGE_XPLORER_VIEW_CAD,
        CHANGE_XPLORER_VIEW_LOGO,

        //For debugging purposes
        v21ID_DUMMY,

        //Always enum these last 
        OPEN_RECENT_CONNECTION_MENU,
        v21ID_BASE_RECENT
    };

    void OnClose( wxCloseEvent& event );

    void FrameClose( wxCommandEvent& event );

    ///Override so we can show the tree after things are initialized. 
    virtual bool Show( bool value );

    ///Get the active display mode of conductor
    ///\return Returns either:\n "Desktop" or "Tablet"
    std::string GetDisplayMode();

    ///Get an appropriate size for sub dialogs
    wxRect GetAppropriateSubDialogSize();

    Splitter* wx_log_splitter;
    wxSplitterWindow* wx_ve_splitter;
    Splitter* wx_nw_splitter;
    wxMenuBar* menubar;
    MainToolBar* mainToolBar;///<The main toolbar

    Avail_Modules* av_modules;
    Network* network;

    UI_Frame* m_frame;

    wxImageList* m_imageList;

    wxMenu* file_menu;
    wxMenu* con_menu;
    wxMenu* run_menu;
    wxMenu* edit_menu;
    wxMenu* help_menu;
    wxMenu* xplorerMenu;
    wxMenu* xplorerDeviceMenu;
    wxMenu* xplorerJugglerMenu;
    wxMenu* xplorerViewMenu;
    wxMenu* xplorerDisplayMenu;

    wxMenu* openRecentMenu;

    //Configuration flags
    bool f_financial;
    bool f_geometry;
    bool f_visualization;

    void Log( const char* msg );
    void CloseVE();

    void GetConfig(wxConfig* config);
    void StoreConfig(wxConfig* config);

    ///Function to process command line args to conductor and specifically
    ///to load a ves file and set the working directory appropriatley for a
    ///particular application
    void ProcessCommandLineArgs();

protected:
    int m_frameNr;
    wxString fname;
    wxString directory;
    wxString path;

    std::vector< wxFileName > recentFileArchive;

protected:
    void _createTreeAndLogWindow( wxWindow* parent );
    void _configureDesktop();
    void _configureTablet();
    void _detectDisplayAndCreate();
    void _detectDisplay();
   
    wxRect DetermineFrameSize( wxConfig* config );
    void StoreFrameSize( wxRect rect, wxConfig* config );
    void StoreRecentFile( wxConfig* config );
    void CreateMenu();
    void ZoomIn( wxCommandEvent &event );
    void ZoomOut( wxCommandEvent &event );
    void SaveAs( wxCommandEvent &event );
    void OnPreferences( wxCommandEvent &event );
	
    void SetRecentFile( wxFileName vesFileName );
    void OpenRecentFile( wxCommandEvent& event );

public:
    void New( wxCommandEvent &event );
    void Open( wxCommandEvent &event );
    void Save( wxCommandEvent &event );
    void SubmitToServer( wxCommandEvent &event );

protected:
    void LoadFromServer( wxCommandEvent &event );

   void QueryFromServer( wxCommandEvent& event );
   void QueryNetwork( wxCommandEvent& event );
   void RunAspenNetwork( wxCommandEvent& event );
   void ShowAspenSimulation( wxCommandEvent& WXUNUSED(event) );
   void HideAspenSimulation( wxCommandEvent& WXUNUSED(event) );
   void CloseAspenSimulation( wxCommandEvent& WXUNUSED(event) );
   void FindBlocks( wxCommandEvent& WXUNUSED(event) );
   void SaveSimulation( wxCommandEvent& WXUNUSED(event) );
   void SaveAsSimulation( wxCommandEvent& WXUNUSED(event) );
   void StartCalc(wxCommandEvent &event);	
   void StopCalc(wxCommandEvent &event);	
   void PauseCalc(wxCommandEvent &event);	
   void ResumeCalc(wxCommandEvent &event);	
   
    void ViewResult( wxCommandEvent &event );

    void ViewHelp( wxCommandEvent &event );
    void ViewAbout( wxCommandEvent &event );
    void ViewRevision( wxCommandEvent &event );
    void ViewContacts( wxCommandEvent &event );
    void ViewPlatformInfo( wxCommandEvent &event );

    void GlobalParam( wxCommandEvent &event );

    void DisConExeServer( wxCommandEvent &event );
    void DisConVEServer( wxCommandEvent &event );
    void LoadBase( wxCommandEvent &event );
    void LoadSour( wxCommandEvent &event );
    void LoadREIBase( wxCommandEvent &event );
    void LoadREISour( wxCommandEvent &event );

    //Controls for VE-Xplorer
    //These are the callbacks for the pull down menu
    void LaunchDeviceProperties(wxCommandEvent& event);
    void LaunchNavigationPane( wxCommandEvent& event );
    void LaunchViewpointsPane( wxCommandEvent& event );
    void LaunchStreamlinePane( wxCommandEvent& event );
    void LaunchRecordScenes( wxCommandEvent& event );

    void DisplaySelection( wxCommandEvent& event );

    void ViewSelection( wxCommandEvent& event );

    ///Set the background color in xplorer
    ///\param The command event
    void SetBackgroundColor( wxCommandEvent& event ); 

    void OnExitXplorer( wxCommandEvent& event );
    //void LaunchVisTabs( wxCommandEvent& event );
    //void LaunchVistab( wxCommandEvent& event );

    ///Launch the CADNode GUI
    ///\param event The wxCommand event
    void LaunchCADNodePane( wxCommandEvent& event );

    ///Process Juggler settings
    void JugglerSettings( wxCommandEvent& event );

    ///Change the visual view in xplorer 
    void ChangeXplorerViewSettings( wxCommandEvent& event );

    ///Get the user preferences class
    UserPreferences* GetUserPreferences();

    ///Pop up window for build info
    void RevisionInfo( wxCommandEvent& event );

    void OnDoubleClickSash( int, int );

    void OnInternalIdle();

    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }

    ///Change the active device in xplorer
    ///\param event The wxCommand event
    void ChangeDevice( wxCommandEvent& event );
         
private:
    void ExitXplorer();
    void EnableCEGUIMenuItems();
    void IdleEvent( wxIdleEvent& event );
    //void TimerEvent( wxTimerEvent& event );
    NavigationPane* navPane;
    DeviceProperties* deviceProperties;
    ViewLocPane* viewlocPane;
    UI_TeacherTab* recordScenes;

    //wxDialog* visTabs;
    //Vistab* vistab;
    wxDialog* _treeView;

    ///<Desktop or Tablet
    std::string _displayMode;

    ///<The CADNode GUI
    VE_Conductor::GUI_Utilities::CADNodeManagerDlg* _cadDialog;

    StreamlinePane* streamlinePane;
    VE_Conductor::CORBAServiceList* serviceList;
    UserPreferences* preferences;
    std::vector< double > xplorerColor;
    wxColourData* xplorerWxColor;
    wxFileHistory* m_recentVESFiles;///<The list of recently opened VES files.

    DECLARE_EVENT_TABLE()
};

#endif //FRAME_H
