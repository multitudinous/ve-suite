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
 * Date modified: $Date$f
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef UI_PLUGIN_BASE_H
#define UI_PLUGIN_BASE_H
/*!\file UIPluginBase.h
UIPluginBase API
*/
/*!\class UIPluginBase
*
*/
#include <ves/VEConfig.h>
#include <ves/conductor/Canvas.h>
#include <ves/conductor/Network.h>
#include <ves/open/xml/model/Port.h>
#include <ves/open/xml/model/ModelPtr.h>
#include <ves/open/xml/CommandPtr.h>
#include <vector>
#include <map>
#include <utility>

#define ICON 20000

#include <wx/object.h>
#include <wx/icon.h>

///Forward declarations
class wxPoint;
class wxDC;
class wxRect;
class wxWindow;
class wxDialog;
class wxImage;
class wxScrolledWindow;
class wxMenu;

#include <wx/event.h>

namespace ves
{
namespace conductor
{
class XMLDataBufferEngine;
class UserPreferencesDataBuffer;;

namespace util
{
class CORBAServiceList;
class CADNodeManagerDlg;
class SoundsPane;
class DataSetLoaderUI;
}
}
}

typedef std::vector< wxPoint > POLY;
typedef std::vector< ves::open::xml::model::Port > PORT;

namespace ves
{
namespace conductor
{
class UIDialog;
class TextResultDialog;
class FinancialDialog;
class SummaryResultDialog;
class Vistab;
class IconChooser;
//class Canvas;

class VE_GUIPLUGINS_EXPORTS UIPluginBase : public wxEvtHandler
{

protected:
    ///Defualt constructor
    UIPluginBase();
public:
    ///Defualt destructor for plugins
    virtual ~UIPluginBase();
    ///Return the version number of the module
    virtual double GetVersion();
    ///This call return a window to be displayed on the framework
    virtual void DrawIcon( wxDC* dc );
    ///Draw he id for a particular plugin
    void DrawID( wxDC* dc );
    ///Draw he name for a particular plugin
    void DrawName( wxDC* dc );
    ///Set the start drawing location
    void SetPos( wxPoint pt );
    ///Return the bounding box;
    wxRect GetBBox();
    ///To Get around the Memory allocation problem of windows dll
    ///Add the calls for the size. So the main program can preallocate
    /// memory for it
    int GetNumPoly( void );
    ///Return the outline polygon
    void GetPoly( POLY &polygon );
    ///This returns the UI dialog of the module
    virtual ves::conductor::UIDialog* UI( wxWindow* parent );
    
    ///This returns the Result dialog of the module
    virtual UIDialog* Result( wxWindow* parent );
    ///This returns the PortData dialog of the module
    //virtual UIDialog* PortData(wxWindow* parent,  Interface *intf);
    ///This returns the FinancialData dialog of the module
    virtual void FinancialData();
    ///This is the ID to identify the module
    unsigned int GetID();
    ///This returns the name of the module
    wxString GetName();
    ///This returns the name used by conductor to construct the plugin tree
    virtual wxString GetConductorName();
    ///This sets the name of the module
    virtual void SetName( wxString pluginName );
    ///This returns the description of the module, This should be a short description
    wxString GetDesc();
    ///Return the URL of the online help
    virtual wxString GetHelp();
    ///Get geometry data
    void GeometryData();
    //This is the load function of the module,
    ///unpack the input string and fill up the UI according to this
    ves::open::xml::model::ModelPtr GetVEModel( void );
    ///Set the ve model
    void SetVEModel( ves::open::xml::model::ModelWeakPtr tempModel );
    ///method to start a dialog to ask the user for a plugin name so that the
    ///name can be defined at run time
    void SetPluginNameDialog( void );
    ///Set the network window to use for accessing dc's and setting parent windows
    ///on subdialogs for plugins
    //allows user to set the image to be displayed on the icon
    void SetImageIcon( std::string path, float rotation = 0.0f, int mirror = 0, float scale = 1.0f );
    ///allows users creating new plugins and change the icon
    void SetImage( wxImage& image );
    ///Add port to the plugin
    void AddPort( wxCommandEvent& event );
    ///Delete selected port
    void DeletePort( wxCommandEvent& event );
    ///Watch the children windows be destroyed
    void OnChildDestroy( wxWindowDestroyEvent& event );
    ///Configure the plugin dialogs with some default settings
    void ConfigurePluginDialogs( wxWindow* window );
    ///On exit check to see if there on any dialogs open
    void CheckPluginMapOnExit();
    ///Handle all events to toggle graphics for plugin off/on in cad mode
    ///\param event WX Event for menu event
    void TogglePlugin( wxCommandEvent& event );
    void OnNavigateTo( wxCommandEvent& event );

    //To Get around the Memory allocation problem of windows dll
    //Add the calls for the size. So the main program can preallocate memory for it

    virtual int GetNumIports();
    virtual void GetIPorts( PORT& ports );

    virtual int GetNumOports();
    virtual void GetOPorts( PORT& ports );

    virtual void Lock( bool lock );
    void SetID( int id );
    virtual bool Has3Ddata();
    // EPRI TAG
    FinancialDialog* financial_dlg;

    // virtual functions to launch custom input dialogs
    virtual void ViewInputVariables( void );
    virtual void ViewResultsVariables( void );

    ///Process left double click mouse events
    void OnDClick( wxMouseEvent &event );
    ///Set the network wxFrame that this plugin is associated with so that
    ///the plugin can draw and capture the appropriate events
    void SetCanvas( wxScrolledWindow* canvas );
    void SetNetwork( Network* network );
    ///Set the corba servicelist so that the plugin can talk with the graphical
    ///engine
    void SetCORBAService( ves::conductor::util::CORBAServiceList* serviceList );
    ///Set the databuffer engine so that memory is handled properly on windows
    void SetXMLDataBufferEngine( ves::conductor::XMLDataBufferEngine* bufferEngine );
    ///Set the databuffer engine so that memory is handled properly on windows
    void SetUserPreferencesDataBuffer( ves::conductor::UserPreferencesDataBuffer* prefBuffer );
    ///Set the user scale to enable working with the dc
    void SetDCScale( std::pair< double, double >* scale );
    ///See if this plugin is selected
    bool SelectMod( int x, int y );
    ///Set the highlight flag for this plugin
    void SetHighlightFlag( bool flag );
    ///Get the highlight flag for this plugin
    bool GetHighlightFlag();
    ///Draw function to handle all drawing for a plugin
    ///All draw functions for the plugin should be called in this function
    ///\param dc DC to draw on for the plugin
    void DrawPlugin( wxDC* dc );
    ///Draw the highlight for this plugin
    void HighlightSelectedIcon( wxDC* dc );
    ///Draw the ports for this plugin
    void DrawPorts( bool flag, wxDC* dc );
    ///Find how near the two points are
    double computenorm( wxPoint pt1, wxPoint pt2 );

    void OnMRightDown( wxMouseEvent &event );
    ///Sets the active model in Xplorer
    ///NOTE: Keep in mind that after a user submits a job that the active model
    ///in xplorer is set to NULL. This means that in some use cases the 
    ///developer will have to set the active model manually from the derived
    ///dialog classes.
    ///\return Tell the user that the call to xplorer was successful
    bool SetActiveModel( void );
    ///Still need to be documented
    void OnShowResult( wxCommandEvent& event );
    void OnParaView( wxCommandEvent& event );
    void OnShowDesc( wxCommandEvent& event );
    void OnShowUserDialog( wxCommandEvent& event );
    void OnGeometry( wxCommandEvent& event );
    void OnDataSet( wxCommandEvent& event );
    void OnInputsWindow( wxCommandEvent& event );
    void OnResultsWindow( wxCommandEvent& event );
    void OnVisualization( wxCommandEvent& event );
    void OnSetUIPluginName( wxCommandEvent& event );
    void GlobalNameUpdate( wxCommandEvent& event );
    void OnSetActiveXplorerModel( wxCommandEvent& event );
    void OnSetActivePluginID( wxUpdateUIEvent& event );
    void OnDelMod( wxCommandEvent& event );
    void OnZoomSelected( wxCommandEvent& event );
    void OnMakeIntoHierarchy( wxCommandEvent& event );
    // EPRI TAG
    void OnShowFinancial( wxCommandEvent& event );
    //void OnShowAspenName( wxCommandEvent& event );
    //void OnQueryDynamics( wxCommandEvent& event );
    //void OnQueryInputs( wxCommandEvent& event );
    //void OnQueryOutputs( wxCommandEvent& event );
    void OnSetInput( wxCommandEvent& event );
    void OnQueryInputModuleProperties( std::vector< std::string >, std::string );
    void OnQueryOutputModuleProperties( std::vector< std::string >, std::string );
    void OnQueryModuleProperties( std::vector< std::string > requestedInputs, std::string compName );
    //void OnReinitBlocks( wxCommandEvent& event );
    ///Show the sounds available for this model
    void OnModelSounds( wxCommandEvent& event );
    void OnShowIconChooser( wxCommandEvent& event );
    ///Set the max size for this plugins dialog
    void SetDialogSize( wxRect dialogSize );
    ///Tell the plugin whether to render the name of the plugin during draw
    ///\param flag Render the name or not
    void SetNameFlag( bool flag );
    ///Get the status of rendering the name of the plugin
    ///\return The state of the name rendering
    bool GetNameFlag();
    ///return the right click pop up menu for the plugin
    ///\return The wxWidgets wxMenu for the right click menu
    wxMenu* GetPopupMenu();
    ///Create the dialog for this plugin
    void CreateUserDialog( wxPoint extpos );
    ///Send the id of this plugin to xplorer so that it is active for all 
    ///other xplorer events
    void SendActiveId();
    ///Make this plugin a hierarchy plugin
    void SetAsHierarchy();
    ///Get the icon for this plugin
    wxBitmap* GetIconImage();
    
protected:
    ///Add a port to the model
    ///\param tempPoint The point where to place the port. This location
    ///is in local coordinate space of the icon
    ///\param typePort The type of port either input or output
    void AddPortToModel( wxPoint& tempPoint, unsigned int typePort );
    ///Get the UIPluginBase poup menu
    ///\return The menu for this class
    wxMenu* SetupPluginBasePopupMenu();
    
    virtual wxMenu* GetPluginPopupMenu( wxMenu* baseMenu );

    void GetDataTables( ves::open::xml::CommandPtr inputCommand,
                        std::vector< wxString >& tagNames,
                        std::vector< wxString >& values );

    void RegistVar( std::string vname, long* var );
    void RegistVar( std::string vname, double* var );
    void RegistVar( std::string vname, std::string* var );
    void RegistVar( std::string vname, std::vector< long >* var );
    void RegistVar( std::string vname, std::vector< double >* var );
    void RegistVar( std::string vname, std::vector< std::string >* var );
	void RegistVar( std::string vname, std::vector< std::vector<std::string> >* var );

    ///Check the active id against the plugin id
    bool CheckID();
    ///Disconnect the destroy event handler
    void DisconnectPluginDialogsDestroyEvent( wxWindow* window );
    ///Remove all the dialogs that were opened for this plugin
    void RemovePluginDialogsFromCanvas();
    ///Remove a specific plugin from the canvas dialog
    void RemoveWindowFromCanvas( wxWindow* window );

    //returns the parent of the plugin
    wxWindow * GetPluginParent();

    UIDialog* dlg;
    TextResultDialog* result_dlg;
    TextResultDialog* port_dlg;
    ///Dataset dialog to load and control dataset
    util::DataSetLoaderUI* m_dataSetLoaderDlg;
    ///id for the plugin
    unsigned int id;

    ///The Position to draw Icon
    wxPoint pos;
    ///Point where the mouse event takes place with the right click menu
    wxPoint actionPoint;

    ///Copy of the model element pointer
    ves::open::xml::model::ModelPtr m_veModel;
    ves::open::xml::model::ModelWeakPtr parentModel;
    ///Name seen by the user and rendered on the canvas
    wxString mPluginName;

    ///Port data info
    //int numberOfInputPorts;
    //int numberOfOutputPorts;
    std::vector< ves::open::xml::model::PortPtr > inputPort;
    std::vector< ves::open::xml::model::PortPtr > outputPort;

    //data storage types
    std::vector< wxString > v_desc;
    std::vector< wxString > v_value;

    std::map<std::string, long* >                     _int;
    std::map<std::string, double* >                   _double;
    std::map<std::string, std::string* >              _string;
    std::map<std::string, std::vector<long>* >        _int1D;
    std::map<std::string, std::vector<double>* >      _double1D;
    std::map<std::string, std::vector<std::string>* > _string1D;
	std::map<std::string, std::vector< std::vector<std::string> >* > _string2D;

    // Dynamic input and results dialogs as well as port dialogs
    //wxDialog* inputsDialog;
    SummaryResultDialog* resultsDialog;
    wxDialog* portsDialog;
    SummaryResultDialog* inputsDialog;

    ves::conductor::util::CORBAServiceList* serviceList;
    ves::conductor::XMLDataBufferEngine* mDataBufferEngine;
    ves::conductor::UserPreferencesDataBuffer* mUserPrefBuffer;
    
    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }

    std::string iconFilename;

    std::map< std::string, wxImage > defaultIconMap;

    ///The pointer to the canvas scrolled window
    Canvas* m_canvas;
    ///The network event handler
    ves::conductor::Network* m_network;
    ///The sound pane
    util::SoundsPane* _soundsDlg;
    ///The icon chooser dialog
    IconChooser* m_iconChooser;
    ///The visualization tab
    Vistab* vistab;
    ///CAD dialog for geometry
    ves::conductor::util::CADNodeManagerDlg* cadDialog;
    ///Map to keep track of which dialogs have been created
    std::map< int, bool > mDialogMemoryMap;
    
    int m_selFrPort;
    int m_selToPort;
    int m_selLink;
    int m_selLinkCon;
    int m_selTag;
    int m_selTagCon;
    wxRect dialogSize;
    int activeId;
    ///Determine wether to draw the ports and highlight band
    bool highlightFlag;
    bool nameFlag;

    ///User scale
    /// first = x scale
    /// second = y scale
    std::pair< double, double >* userScale;
    ///Pair to tell network what to delete
    /// first = id
    /// second = memory map size
    std::pair< unsigned int, size_t > pluginDialogPair;
    ///THe plugin delete event
    wxUpdateUIEvent pluginDeleteEvent;

    ///The string used to provide a description of the plugin
    wxString mDescription;

private:
    ///That's the for default implementation of the DrawIcon. 
    ///Not part of the general interface
    ///The outline polygon points list
    wxPoint* poly; 
    ///Number of points in the polygon
    int n_pts;
    ///The pointer for the icon that will be rendered in conductor
    wxBitmap* mMyIcon;
    ///The width and height of the icon
    int mIconW, mIconH;
    ///The member that stores the right click menu
    wxMenu* mPopMenu;

    DECLARE_DYNAMIC_CLASS( UIPluginBase )
    DECLARE_EVENT_TABLE()
};
}
}
#define UIPLUGIN_CHECKID(event)  \
    if( !CheckID() ) \
    { \
        event.Skip(); \
        return; \
    }

#endif
