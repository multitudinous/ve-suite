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
#ifndef NETWORK_H
#define NETWORK_H
/*!\file Network.h
Network API
*/
/*!\class Network
* 
*/
#include "VE_Open/skel/moduleC.h"
#include "VE_Open/skel/VjObsC.h"

#include "VE_Conductor/GUIPlugin/Plugin_base.h"
#include "VE_Conductor/Utilities/Link.h"
#include "VE_Conductor/Utilities/Tag.h"
#include "VE_Conductor/Utilities/Polygon.h"
#include "VE_Conductor/Utilities/Module.h"

#include <xercesc/dom/DOM.hpp>

#include <wx/event.h>
#include <wx/scrolwin.h>
#include <wx/textdlg.h>
#include <wx/menu.h>
#include <wx/thread.h>
#include <vector>
#include <map>
#include <iostream>
class GlobalParamDialog;
class wxProgressDialog;
namespace VE_XML
{
   namespace VE_Model
   {
      class Network;
      class User;
   }
}

class Vistab;
class AppFrame;

namespace VE_Conductor
{
   namespace GUI_Utilities
   {
      class CADNodeManagerDlg;
   }
}

class Network : public wxScrolledWindow, public wxThreadHelper
{
public:
   Network(){;}
   Network(wxWindow* parent, int id );
   virtual ~Network();

   enum 
   {
      ADD_TAG,
      ADD_LINK_CON,
      EDIT_TAG,
      DEL_TAG,
      DEL_LINK,
      DEL_LINK_CON,
      DEL_MOD,
      SHOW_LINK_CONT,
      SHOW_RESULT,
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
      SET_ACTIVE_MODEL
   };

   ///Fucntion called during submit job to send the id of all active
   ///modules to the CE
   void SetIDOnAllActiveModules( void );

   //Body::Executive_var exec; //put this reference here, so ther frame work can still access it YANG

   wxMutex s_mutexProtect;

   GlobalParamDialog * globalparam_dlg;

   bool paraview;

   void ReDrawAll();

   //Event Handlers
   void OnPaint( wxPaintEvent &event );
   void OnMouseMove( wxMouseEvent &event );
   void OnMLeftDown( wxMouseEvent &event );
   void OnMLeftUp( wxMouseEvent &event );
   /// This function opens a plugins dialog when double clicked on the design canvas
   void OnDClick( wxMouseEvent &event );
   void OnMRightDown( wxMouseEvent &event );
   void OnAddTag( wxCommandEvent &event );
   void OnAddLinkCon( wxCommandEvent &event );
   void OnEditTag( wxCommandEvent &event );
   void OnDelTag( wxCommandEvent &event );
   void OnDelLink( wxCommandEvent &event );
   void OnDelLinkCon( wxCommandEvent &event );
   void OnDelMod( wxCommandEvent &event );
   void OnShowLinkContent( wxCommandEvent& event );
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
   // EPRI TAG
   void OnShowFinancial(wxCommandEvent &event);
   void OnShowAspenName(wxCommandEvent &event);
   void OnQueryInputs(wxCommandEvent &event);
   void OnQueryOutputs(wxCommandEvent &event);
   void OnSetInput(wxCommandEvent &event);
   void OnQueryInputModuleProperties(std::vector< std::string >, std::string);
   void OnQueryOutputModuleProperties(std::vector< std::string >, std::string);
   void OnQueryModuleProperties(std::vector< std::string > requestedInputs, std::string compName);

   void OnShowIconChooser(wxCommandEvent &event);

   //Add to network fuctions
   void AddtoNetwork(REI_Plugin *new_mod, std::string cls_name);
   void AddTag(int x, int y, wxString text);

   //Save and Load the network
   std::string Save( std::string fileName );
   ///Load calls new when loading the network
   ///this also calls delete objects on xplorer but the user may not 
   ///always want this action
   void Load( std::string xmlNetwork, bool promptClearXplorer );
   void CreateNetwork( std::string xmlNetwork );
   ///Clear the deisgn canvas and xplorer objects if desired
   void New( bool clearXplorer = false );
   ///Acessors
   std::pair< double, double >* GetUserScale( void );
   std::pair< unsigned int, unsigned int >* GetNumPix( void );
   std::pair< unsigned int, unsigned int >* GetNumUnit( void );
   virtual void* Entry();
   
   void SetXplorerInterface( VjObs_ptr veEngine );
   bool SetActiveModel( void );
   //void HighlightSelectedIcon2(unsigned int);
   void HighlightSelectedIcon( REI_Plugin* cur_module);

protected:

   //Draw functions
   void DrawPorts( REI_Plugin* cur_module, bool flag);
   void DrawPorti( REI_Plugin* cur_module, int index, bool flag);
   //void DrawLinkCon( VE_Conductor::GUI_Utilities::Link l, bool flag);
   //void DrawTagCon( VE_Conductor::GUI_Utilities::Tag t, bool flag);
   //void DrawLink( VE_Conductor::GUI_Utilities::Link *l, bool flag);
   //void DrawTag( VE_Conductor::GUI_Utilities::Tag *t, bool flag);
   void ReDraw(wxDC &dc);

   //Selection functions
   int SelectMod(int x, int y, wxDC& dc);
   void UnSelectMod(wxDC& dc);
   int  SelectLink(int x, int y);
   void UnSelectLink(wxDC& dc);
   int SelectTag(int x, int y);
   void UnSelectTag(wxDC& dc);

   //Move and drop functions
   void MoveModule(int x, int y, int mod, wxDC &dc);
   void DropModule(int x, int y, int mod );
   void MoveLinkCon(int x, int y, int ln, int ln_con, wxDC& dc);
   void DropLinkCon(int x, int y, int ln, int ln_con, wxDC& dc);
   void MoveTagCon(int x, int y, int t, int t_con, wxDC &dc);
   void DropTagCon(int x, int y, int t, int t_con, wxDC &dc);
   void MoveTag(int x, int y, int t, wxDC &dc);
   void DropTag(int x, int y, int t, wxDC &dc);
   void TryLink(int x, int y, int mod, int pt, wxDC &dc, bool flag);
   void DropLink(int x, int y, int mod, int pt, wxDC &dc, bool flag);

   //Math functions for the relationship of points and polygons
   //int ccw( wxPoint pt1, wxPoint pt2, wxPoint pt3 );
   //int intersect( VE_Conductor::GUI_Utilities::Polygon l1, VE_Conductor::GUI_Utilities::Polygon l2);
   //int inside( wxPoint pt, VE_Conductor::GUI_Utilities::Polygon poly);
   double computenorm (wxPoint pt1, wxPoint pt2);
   //double nearpnt( wxPoint pt, VE_Conductor::GUI_Utilities::Polygon poly, 
   //                                 VE_Conductor::GUI_Utilities::Polygon &near);
   //void TransPoly( VE_Conductor::GUI_Utilities::Polygon oldpoly, 
   //                int x, int y, VE_Conductor::GUI_Utilities::Polygon &newpoly);

   //Misc functions
   void CleanRect( wxRect box, wxDC& dc); // for wipeout a rectangular area
   wxPoint GetFreePos( wxRect bbox); // for finding a free start location for a new module
   //VE_Conductor::GUI_Utilities::Polygon CalcLinkPoly( VE_Conductor::GUI_Utilities::Tag l); // calculate the bounding polygon of a link
   //VE_Conductor::GUI_Utilities::Polygon CalcTagPoly( VE_Conductor::GUI_Utilities::Tag t); // calculate the bounding polygon of a tag

   ///Get the point for a port or connector for a selected plugin
   ///portType is either input or output
   wxPoint GetPointForSelectedPlugin( unsigned long moduleID, unsigned int portNumber, std::string portType );

   //Check if the two port is compatible
   bool IsPortCompatible(int frmod, int frport, int tomod, int toport);
protected:
   //Three main list of network objs
   std::vector< VE_Conductor::GUI_Utilities::Link > links; //The list of links between the nodes of moduls.
   std::vector< VE_Conductor::GUI_Utilities::Tag > tags; //The list of text tags  

public:
   std::map< int, VE_Conductor::GUI_Utilities::Module > modules; //The list of modules;

protected:
   int m_selMod; // selected module
   int m_selFrPort; // selected From port
   int m_selToPort; // selected To port;
   int m_selLink; //selected Link
   int m_selLinkCon; //selected Link Connector
   int m_selTag; //selected Tag
   int m_selTagCon; //selected Tag Connector

   wxPoint relative_pt; // the relative point of the polygon, used by the move module function
   wxPoint tag_rpt; // the relative point of the tag

   //void Pack( std::vector<Interface> & UIs );
   //void UnPack( std::vector<Interface> & UIs );

private:
   bool moving;
   int intfssize;
   wxProgressDialog* _fileProgress;
   bool isLoading;
   bool isDataSet;

   Vistab* vistab;///< The visualization tab

   std::string tempXMLNetworkData;
   std::vector< wxRect > sbboxes; //start up bounding box; used by GetFreePos to calc start module location
   int xold, yold; //The old location of the mouse position, used by the TryLink to wipe the old tried link route
   wxPoint action_point; //The mouse position when the right button clicked, used by menu event handlers
   VE_XML::VE_Model::Network* veNetwork;
   ///Parent window pointer to the splitter in AppFrame
   wxWindow* parent;
   ///wxframe pointer for frame.cxx
   AppFrame* frame;
   ///User scale
   /// first = x scale
   /// second = y scale
   std::pair< double, double > userScale;
   ///Num Pixels
   /// first = x pix
   /// second = y pix
   std::pair< unsigned int, unsigned int > numPix;
   ///Num unit
   /// first = x unit
   /// second = y unit
   std::pair< unsigned int, unsigned int > numUnit;
   ///Color vector
   std::vector<double> backgroundColor;
   ///ptr to send data back to explorer
   VjObs_var xplorerPtr;
   ///Used to operate on cad data in a plugin
   VE_Conductor::GUI_Utilities::CADNodeManagerDlg* cadDialog;
   
   std::string ConvertUnicode( const wxChar* data )
   {
      std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
      return tempStr;
   }
   
   DECLARE_EVENT_TABLE() // no semicolon needed
};

#endif
