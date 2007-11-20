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
#ifndef UI_PLUGIN_BASE_H
#define UI_PLUGIN_BASE_H
/*!\file UIPluginBase.h
UIPluginBase API
*/
/*!\class UIPluginBase
* 
*/
#include <ves/VEConfig.h>
#include <ves/open/xml/model/Port.h>
#include <ves/open/xml/model/ModelPtr.h>
#include <vector>
#include <map>
#include <utility>

#define ICON 20000

#include <wx/object.h>
#include <wx/icon.h>
///Default xpm's
#include <ves/conductor/xpm/contour.xpm>
#include <ves/conductor/xpm/cad_tree_selected.xpm>
#include <ves/conductor/xpm/cad_tree_unselected.xpm>
#include <ves/conductor/xpm/cspline.xpm>
#include <ves/conductor/xpm/isosurface.xpm>
#include <ves/conductor/xpm/ROItb.xpm>
#include <ves/conductor/xpm/square.xpm>
#include <ves/conductor/xpm/streamlines.xpm>
#include <ves/conductor/xpm/vector.xpm>
#include <ves/conductor/xpm/vectortb.xpm>

///Forward declarations
class wxPoint;
class wxDC;
class wxRect;
class wxWindow;
class wxDialog;
class wxImage;
class wxScrolledWindow;

#include <wx/event.h>
#define edge_size 10

namespace ves
{
namespace conductor
{
namespace util
{
    class CORBAServiceList;
    class CADNodeManagerDlg;
    class SoundsPane;
    class DataSetLoaderUI;
}
}
}

namespace ves
{
namespace open
{
namespace xml
{
    class Command;
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
class GeometryDialog;
class FinancialDialog;
class GeometryDataBuffer;
class SummaryResultDialog;
class Vistab;
class IconChooser;
    
class VE_GUIPLUGINS_EXPORTS UIPluginBase : public wxEvtHandler//, public wxObject
{
public:
   enum
   {
      SHOW_RESULT = 3500,
      SHOW_DESC,
      PARAVIEW,
      SHOW_FINANCIAL, /* EPRI TAG */
      ASPEN_MENU,
      SHOW_ASPEN_NAME,
      QUERY_INPUTS,
      QUERY_OUTPUTS,
      ASPEN_ICON,
      ICON_MENU,
      SHOW_ICON_CHOOSER,
      GEOMETRY,
      MODEL_INPUTS,
      MODEL_RESULTS,
      DATASET,
      VISUALIZATION,
      SET_UI_PLUGIN_NAME,
      SET_ACTIVE_MODEL,
      ACTIVE_MODEL_SOUNDS,
      DEL_MOD,
      SET_ACTIVE_PLUGIN,
      ADD_INPUT_PORT,
      ADD_OUTPUT_PORT,
      DELETE_PORT
   };
   ///Defualt constructor
   UIPluginBase();
   ///Defualt destructor for plugins
   virtual ~UIPluginBase();
   ///Return the version number of the module
   virtual double GetVersion();
   ///This call return a window to be displayed on the framework
   virtual void DrawIcon(wxDC* dc);
   ///Draw he id for a particular plugin
   void DrawID(wxDC* dc);
   ///Draw he name for a particular plugin
   void DrawName(wxDC* dc);
   ///Set the start drawing location
   void SetPos(wxPoint pt);
   ///Return the bounding box;
   wxRect GetBBox();
   ///To Get around the Memory allocation problem of windows dll
   ///Add the calls for the size. So the main program can preallocate 
   /// memory for it
   int GetNumPoly( void );
   ///Return the outline polygon
   void GetPoly(POLY &polygon); 
   ///This returns the UI dialog of the module
   virtual ves::conductor::UIDialog* UI(wxWindow* parent);
   ///This returns the Result dialog of the module
   virtual UIDialog* Result(wxWindow* parent);
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
   virtual wxString GetDesc();
   ///Return the URL of the online help
   virtual wxString GetHelp();
   ///Get geometry data
   void GeometryData();
   //This is the load function of the module, 
   ///unpack the input string and fill up the UI according to this
   ves::open::xml::model::ModelWeakPtr GetVEModel( void );
   ///Set the ve model
   void SetVEModel( ves::open::xml::model::ModelWeakPtr tempModel );
   ///method to start a dialog to ask the user for a plugin name so that the 
   ///name can be defined at run time
   void SetPluginNameDialog( void );
   ///Set the network window to use for accessing dc's and setting parent windows
   ///on subdialogs for plugins
   //allows user to set the image to be displayed on the icon
   void SetImageIcon(std::string path, float rotation = 0.0f, int mirror = 0, float scale = 1.0f);
   ///Add port to the plugin
   void AddPort( wxCommandEvent& event );
   ///Delete selected port
   void DeletePort( wxCommandEvent& event );
   
   //To Get around the Memory allocation problem of windows dll
   //Add the calls for the size. So the main program can preallocate memory for it

   int GetNumIports();
   virtual void GetIPorts(PORT& ports);

   int GetNumOports();
   virtual void GetOPorts(PORT& ports);

   virtual void Lock(bool lock);
   void SetID(int id);
   virtual bool Has3Ddata();
   // EPRI TAG
   FinancialDialog* financial_dlg;

   // virtual functions to launch custom input dialogs
   virtual void ViewInputVariables( void );
   virtual void ViewResultsVariables( void );
   
   ///Process left double click mouse events
   void OnDClick( wxMouseEvent &event);
   ///Set the network wxFrame that this plugin is associated with so that
   ///the plugin can draw and capture the appropriate events
   void SetCanvas( wxScrolledWindow* canvas );
   void SetNetwork( wxEvtHandler* network );
   ///Set the corba servicelist so that the plugin can talk with the graphical
   ///engine
   void SetCORBAService( ves::conductor::util::CORBAServiceList* serviceList );
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
   bool SetActiveModel( void );
   ///Still need to be documented
   void OnShowResult( wxCommandEvent& event );
   void OnParaView( wxCommandEvent& event );
   void OnShowDesc( wxCommandEvent& event );
   void OnGeometry( wxCommandEvent& event );
   void OnDataSet( wxCommandEvent& event );
   void OnInputsWindow( wxCommandEvent& event );
   void OnResultsWindow( wxCommandEvent& event );
   void OnVisualization( wxCommandEvent& event );
   void OnSetUIPluginName( wxCommandEvent& event );
   void OnSetActiveXplorerModel( wxCommandEvent& event );
   void OnSetActivePluginID( wxUpdateUIEvent& event );
   void OnDelMod( wxCommandEvent &event );
   // EPRI TAG
   void OnShowFinancial(wxCommandEvent &event);
   void OnShowAspenName(wxCommandEvent &event);
   void OnQueryInputs(wxCommandEvent &event);
   void OnQueryOutputs(wxCommandEvent &event);
   void OnSetInput(wxCommandEvent &event);
   void OnQueryInputModuleProperties(std::vector< std::string >, std::string);
   void OnQueryOutputModuleProperties(std::vector< std::string >, std::string);
   void OnQueryModuleProperties(std::vector< std::string > requestedInputs, std::string compName);
   ///Show the sounds available for this model
   void OnModelSounds(wxCommandEvent &event);
   void OnShowIconChooser(wxCommandEvent &event);

   void SetDialogSize( wxRect dialogSize );
protected:
   void GetDataTables( ves::open::xml::Command* inputCommand, 
                        std::vector< wxString >& tagNames, 
                        std::vector< wxString >& values );

   void RegistVar(std::string vname, long *var);
   void RegistVar(std::string vname, double *var);
   void RegistVar(std::string vname, std::string *var);
   void RegistVar(std::string vname, std::vector<long> *var);
   void RegistVar(std::string vname, std::vector<double> *var);
   void RegistVar(std::string vname, std::vector<std::string> *var);

   ///Check the active id against the plugin id
   bool CheckID();
   
   UIDialog* dlg;
   TextResultDialog* result_dlg;
   TextResultDialog* port_dlg;
   GeometryDialog* geom_dlg;
   ///Dataset dialog to load and control dataset
   util::DataSetLoaderUI* m_dataSetLoaderDlg;
   ///id
   unsigned int id;

   ///The Position to draw Icon
   wxPoint pos; 
   ///Point where the mouse event takes place with the right click menu
   wxPoint actionPoint;

   ///Copy of the model element pointer
   ves::open::xml::model::ModelPtr m_veModel;
   ///Name seen by the user and rendered on the canvas
   wxString name;

   //That's the for default implementation of the DrawIcon. Not part of the general interface
   wxPoint* poly; //The outline polygon points list;
   int n_pts; //Number of points

   ///Port data info
   //int numberOfInputPorts;
   //int numberOfOutputPorts;
   std::vector< ves::open::xml::model::Port* > inputPort;
   std::vector< ves::open::xml::model::Port* > outputPort;
   
   //data storage types
   std::vector< wxString > v_desc;
   std::vector< wxString > v_value;

   std::map<std::string, long *>                      _int;
   std::map<std::string, double *>                    _double;
   std::map<std::string, std::string *>               _string;
   std::map<std::string, std::vector<long> * >        _int1D;
   std::map<std::string, std::vector<double> * >      _double1D;
   std::map<std::string, std::vector<std::string> * > _string1D;
   
   wxBitmap* my_icon;
   int icon_w, icon_h;

   // Dynamic input and results dialogs as well as port dialogs
   //wxDialog* inputsDialog;
   SummaryResultDialog* resultsDialog;
   wxDialog* portsDialog;
   SummaryResultDialog* inputsDialog;

   ves::conductor::util::CORBAServiceList* serviceList;

   std::string ConvertUnicode( const wxChar* data )
   {
      std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
      return tempStr;
   }
   
   std::string iconFilename;
   
   std::map< std::string, wxImage > defaultIconMap;
   
   ///The pointer to the canvas scrolled window
   wxScrolledWindow* m_canvas;
   ///The network event handler
   wxEvtHandler* m_network;
   ///The sound pane
   util::SoundsPane* _soundsDlg;
   ///The icon chooser dialog
   IconChooser* m_iconChooser;
   ///The visualization tab
   Vistab* vistab;
   
   ves::conductor::util::CADNodeManagerDlg* cadDialog;
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

   ///User scale
   /// first = x scale
   /// second = y scale
   std::pair< double, double >* userScale;
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
