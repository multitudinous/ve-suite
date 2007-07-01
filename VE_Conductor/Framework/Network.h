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
#ifndef NETWORK_H
#define NETWORK_H
/*!\file Network.h
Network API
*/
/*!\class Network
* 
*/
#include "VE_Conductor/GUIPlugin/UIPluginBase.h"
#include "VE_Conductor/Utilities/Link.h"
#include "VE_Conductor/Utilities/Tag.h"
#include "VE_Conductor/Utilities/Polygon.h"
#include "VE_Conductor/GUIPlugin/Module.h"

#include <wx/event.h>
#include <wx/scrolwin.h>
#include <wx/textdlg.h>
#include <wx/menu.h>
#include <wx/thread.h>
#include <vector>
#include <map>
#include <iostream>
#include <wx/dcclient.h>

#include "VE_Open/XML/Model/Network.h"

class wxProgressDialog;
namespace VE_XML
{
namespace VE_Model
{
  class User;
}
}

class AppFrame;

class Network : public wxScrolledWindow, public wxThreadHelper
{
public:
   Network(){;}
   Network(wxWindow* parent, int id );
   virtual ~Network();

   enum 
   {
      ADD_TAG,
      EDIT_TAG,
      DEL_TAG
   };

   ///Fucntion called during submit job to send the id of all active
   ///modules to the CE
   void SetIDOnAllActiveModules( void );
   wxMutex s_mutexProtect;

   //Event Handlers
   void OnPaint( wxPaintEvent &event );
   ///This is needed to reduce flicker
   ///Erase background callback
   void OnEraseBackground( wxEraseEvent& event );
    void OnMouseMove( wxMouseEvent &event );
   void OnMLeftDown( wxMouseEvent &event );
   void OnMLeftUp( wxMouseEvent &event );

   void OnMRightDown( wxMouseEvent &event );
   void OnAddTag( wxCommandEvent &event );
   void OnEditTag( wxCommandEvent &event );
   void OnDelTag( wxCommandEvent &event );
   void OnDelMod( wxCommandEvent &event );
   void OnDelLink(wxCommandEvent& event );
       
   //Add to network fuctions
   void AddtoNetwork(UIPluginBase *new_mod, std::string cls_name);
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
   
   void HighlightSelectedIcon( UIPluginBase* cur_module, wxDC &dc);
   void DrawPorts( UIPluginBase* cur_module, bool flag, wxDC &dc);
   bool IsDragging();
   void SetSelectedModule(int mod);
   std::map< int, VE_Conductor::GUI_Utilities::Module > modules; //The list of modules;
   
protected:
   //Draw functions
   void DrawPorti( UIPluginBase* cur_module, int index, bool flag);
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
   void MoveModule(int x, int y, int mod);//, wxDC &dc);
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

   int m_selMod; // selected module
   int m_selFrPort; // selected From port
   int m_selToPort; // selected To port;
   int m_selLink; //selected Link
   int m_selLinkCon; //selected Link Connector
   int m_selTag; //selected Tag
   int m_selTagCon; //selected Tag Connector
    //Three main list of network objs
   std::vector< VE_Conductor::GUI_Utilities::Link > links; //The list of links between the nodes of moduls.
   std::vector< VE_Conductor::GUI_Utilities::Tag > tags; //The list of text tags  
   
   wxPoint relative_pt; // the relative point of the polygon, used by the move module function
   wxPoint tag_rpt; // the relative point of the tag

   //void Pack( std::vector<Interface> & UIs );
   //void UnPack( std::vector<Interface> & UIs );

private:
   int intfssize;
   wxProgressDialog* _fileProgress;
   bool isLoading;
   bool isDataSet;
   bool dragging;
   wxBitmap * bitmapBuffer;

   std::string tempXMLNetworkData;
   std::vector< wxRect > sbboxes; //start up bounding box; used by GetFreePos to calc start module location
   int xold, yold; //The old location of the mouse position, used by the TryLink to wipe the old tried link route
   wxPoint action_point; //The mouse position when the right button clicked, used by menu event handlers
   VE_XML::VE_Model::Network veNetwork;
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
   
   std::string ConvertUnicode( const wxChar* data )
   {
      std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
      return tempStr;
   }
   
   DECLARE_EVENT_TABLE() // no semicolon needed
};

#endif
