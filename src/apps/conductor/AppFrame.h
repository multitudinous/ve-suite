/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#ifndef APP_FRAME_H
#define APP_FRAME_H

// --- VE-Suite Includes --- //
namespace ves
{
namespace conductor
{
class IconChooser;
class Network;
class Canvas;
class CameraPlacementToolUIDialog;
namespace util
{
class CADNodeManagerDlg;
class CORBAServiceList;
}
}
}

class Splitter;
class ViewLocPane;
class AppToolBar;
class AvailableModules;
class HierarchyTree;
class UITeacherTab;
class NavigationPane;
class UserPreferences;
class DeviceProperties;
class EphemerisDialog;
class MinervaDialog;

// --- wxWidgets Includes --- //
#include <wx/timer.h>
#include <wx/filename.h>
#include <wx/frame.h>

class wxTextCtrl;
class wxImage;
class wxSplitterWindow;
class wxDialog;
class wxSplitterWindow;
class wxColourData;
class wxMenu;
class wxFileHistory;
class wxNotebook;

// --- STL Includes --- //
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

/*!\file AppFrame.h
 *
 */

/*!\class AppFrame
 * Main wx frame for conductor
 */

class AppFrame : public wxFrame
{
public:
    AppFrame();

    AppFrame( wxWindow* parent, wxWindowID id, const wxString& title );

    virtual ~AppFrame();

    ///Process the close event
    void OnCloseWindow( wxCloseEvent& event );
    ///Process file quit option
    void FrameClose( wxCommandEvent& event );
    ///Log the creation of windows that are children of AppFrame
    void OnChildCreate( wxWindowCreateEvent& event );
    ///Watch the children windows be destroyed
    void OnChildDestroy( wxWindowDestroyEvent& event );
    ///Override so we can show the tree after things are initialized.
    virtual bool Show( bool value );
    ///Event handler to delete networks to load new data in
    void LoadNewNetwork( wxUpdateUIEvent& event );

    ///Get the active display mode of conductor
    ///\return Returns either:\n "Desktop" or "Tablet"
    const std::string& GetDisplayMode();
 
    void Log( const char* msg );

    ///Function to process command line args to conductor and specifically
    ///to load a ves file and set the working directory appropriatley for a
    ///particular application
    void ProcessCommandLineArgs();
    ///Clear the canvas to be ready for a new project
    void NewCanvas( wxCommandEvent &event );
    ///Open a VES file with the dialog
    void Open( wxCommandEvent &event );
    ///Open a exe file with the dialog
    void Run( wxCommandEvent &event );
    ///Save the VES file
    void Save( wxCommandEvent &event );
    ///Submit the current canvas to the CE
    void SubmitToServer( wxCommandEvent &event );
    ///Allow Xplorer shutdown option with user preferences
    void ShutdownXplorerOptionOn( void );
    ///Removes Xplorer shutdown option using user preferences
    void ShutdownXplorerOptionOff( void );
    HierarchyTree * GetHierarchyTree();
    ///Change data logging settings
    void OnDataLogging( wxCommandEvent &event );
    ///Internal function to make the orb run
    void OnTimer(wxTimerEvent& event);

protected:
    void _createTreeAndLogWindow( wxWindow* parent );
    void _configureDesktop();
    void _configureTablet();
    void _detectDisplayAndCreate();
    void _detectDisplay();

    ///Get information about the size of conductor in tablet mode
    wxRect DetermineTabletFrameSize();
    ///Write the frame information about the size of conductor in tablet mode
    void StoreFrameSize( wxRect rect );
    ///Store gui specific data
    void StoreConfig();
    ///Store the files for the recent file menu
    void StoreRecentFile();
    ///Get the config to start off with
    ///This must be called early
    void GetConfig();

    void CreateMenu();
    void ZoomIn( wxCommandEvent &event );
    void ZoomOut( wxCommandEvent &event );
    void ZoomAll( wxCommandEvent &event );
    void SaveAs( wxCommandEvent &event );
    void OnPreferences( wxCommandEvent &event );

    void SetRecentFile( wxFileName vesFileName );
    void OpenRecentFile( wxCommandEvent& event );
    ///Clear the recent file history menu
    void OnClearRecentFiles( wxCommandEvent& event );
    void OnKeyPress( wxKeyEvent &event );
    
    void LoadFromServer( wxCommandEvent &event );
    void QueryFromServer( wxCommandEvent& event );
    void OpenSimulation( wxString simName );

    void FindBlocks( wxCommandEvent& WXUNUSED( event ) );

    void StartCalc( wxCommandEvent &event );
    void StopCalc( wxCommandEvent &event );
    void PauseCalc( wxCommandEvent &event );
    void ResumeCalc( wxCommandEvent &event );

    void ViewResult( wxCommandEvent &event );

    void ViewHelp( wxCommandEvent &event );
    void ViewAbout( wxCommandEvent &event );
    void ViewContacts( wxCommandEvent &event );
    void ViewPlatformInfo( wxCommandEvent &event );

    void DisConExeServer( wxCommandEvent &event );
    void DisConVEServer( wxCommandEvent &event );
    void LoadBase( wxCommandEvent &event );
    void LoadSour( wxCommandEvent &event );
    void LoadREIBase( wxCommandEvent &event );
    void LoadREISour( wxCommandEvent &event );

    //Controls for VE-Xplorer
    //These are the callbacks for the pull down menu
    void LaunchDeviceProperties( wxCommandEvent& event );
    void LaunchNavigationPane( wxCommandEvent& event );
    void LaunchViewpointsPane( wxCommandEvent& event );
    void LaunchStreamlinePane( wxCommandEvent& event );
    void LaunchRecordScenes( wxCommandEvent& event );
    void LaunchCPTPane( wxCommandEvent& event );
    void UnSelectAllXplorer( wxCommandEvent& event );

    void DisplaySelection( wxCommandEvent& event );

    void ViewSelection( wxCommandEvent& event );

    ///Set the background color in xplorer
    ///\param The command event
    void SetBackgroundColor( wxCommandEvent& event );

    ///Set the ephemeris data
    ///\param event The command event
    void SetEphemerisData( wxCommandEvent& event );

    void OnExitXplorer( wxCommandEvent& event );

    ///Process Juggler settings
    void JugglerSettings( wxCommandEvent& event );

    ///Change the visual view in xplorer
    void ChangeXplorerViewSettings( wxCommandEvent& event );

    ///Change the data logging settings
    void OnDataLoggingSettings( wxCommandEvent& event );

    ///Get the user preferences class
    UserPreferences* GetUserPreferences();

    ///Pop up window for build info
    void RevisionInfo( wxCommandEvent& event );

    void OnDoubleClickSash( int, int );

    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }

    ///Change the active device in xplorer
    ///\param event The wxCommand event
    void ChangeDevice( wxCommandEvent& event );
    ///Change the current working directory
    ///\param event The wxCommand event
    void OnChangeWorkingDirectory( wxCommandEvent& event );

    void OnDelMod( wxCommandEvent& event );
    void SetTreeItemName( wxCommandEvent& event );
    void OnMakeIntoHierarchy( wxCommandEvent& event );
    void OnChangeIcon(wxCommandEvent& event );
    void OnShowIconChooser(wxCommandEvent& event );
    void UpdateHierarchyTree( wxCommandEvent& event );

    void OnAddPlanet ( wxCommandEvent& event );
    void OnRemovePlanet ( wxCommandEvent& event );
    void ShowMinervaDialog ( wxCommandEvent& event );
    MinervaDialog* GetMinervaDialog();

private:
    void ExitXplorer();
    void CloseAspenSimulation();
    void EnableCEGUIMenuItems();
    void IdleEvent( wxIdleEvent& event );
    bool AspenSimOpen;
    //void TimerEvent( wxTimerEvent& event );
    NavigationPane* navPane;
    DeviceProperties* deviceProperties;
    ViewLocPane* viewlocPane;
    UITeacherTab* recordScenes;
    ///Pane to control camera panes
    ves::conductor::CameraPlacementToolUIDialog* m_cptDialog;
    
    wxDialog* _treeView;

    ///Desktop or Tablet
    std::string _displayMode;
    /// Tell event handlers whether a new canvas is being created
    bool newCanvas;
    ///Ephemeris data dialog
    EphemerisDialog* m_ephemeris;
    ves::conductor::util::CORBAServiceList* serviceList;
    UserPreferences* preferences;
    std::vector< double > xplorerColor;
    wxColourData* xplorerWxColor;
    wxFileHistory* m_recentVESFiles;///<The list of recently opened VES files.
    bool mDestoryFrame;
    wxTimer mTimer;
    std::vector< long > pids;
    MinervaDialog *_minervaDialog;
    
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
    
    HierarchyTree* hierarchyTree;
    wxNotebook* side_pane;
    ves::conductor::Canvas* canvas;
    ves::conductor::IconChooser* iconChooser;
    AvailableModules* av_modules;
    wxString mVESFileName;
    wxString directory;
    
    Splitter* wx_log_splitter;
    wxSplitterWindow* wx_ve_splitter;
    Splitter* wx_nw_splitter;
    wxMenuBar* menubar;
    AppToolBar* appToolBar;///<The app toolbar
    ///Shutting down conductor
    bool m_shuttingDown;
    
    DECLARE_EVENT_TABLE()
};

#endif //FRAME_H
