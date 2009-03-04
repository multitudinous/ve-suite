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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef VES_APPS_CONDUCTOR_NETWORK_H
#define VES_APPS_CONDUCTOR_NETWORK_H
/*!\file Network.h
Network API
*/
/*!\class Network
*
*/
#include <ves/VEConfig.h>
#include <ves/conductor/UIPluginBase.h>
#include <ves/conductor/util/Link.h>
#include <ves/conductor/util/Tag.h>
#include <ves/conductor/util/Polygon.h>
#include <ves/conductor/Module.h>
#include <ves/open/xml/model/SystemPtr.h>

#include <wx/event.h>
#include <wx/scrolwin.h>
#include <wx/textdlg.h>
#include <wx/menu.h>
#include <wx/thread.h>
#include <vector>
#include <map>
#include <iostream>
#include <wx/dcclient.h>
#include <ves/VEConfig.h>

class AppFrame;
class wxProgressDialog;
namespace ves
{
namespace conductor
{
class XMLDataBufferEngine;
class UserPreferencesDataBuffer;;

namespace util
{
class CORBAServiceList;
}
}
}            

namespace ves
{
namespace conductor
{
class Canvas;

class VE_GUIPLUGINS_EXPORTS Network : public wxEvtHandler
{
public:
    ///Constructor
    Network( 
            wxWindow* parent, 
            ves::conductor::util::CORBAServiceList* serviceList,
            ves::conductor::XMLDataBufferEngine* dataBufferEngine,
            ves::conductor::UserPreferencesDataBuffer* userPrefBuffer
            );

    virtual ~Network();

    ///Fucntion called during submit job to send the id of all active
    ///modules to the CE
    void SetIDOnAllActiveModules();

    ///This is needed to reduce flicker
    ///Erase background callback
    void OnEraseBackground( wxEraseEvent& event );
    void OnMouseMove( wxMouseEvent& event );
    void OnMLeftDown( wxMouseEvent& event );
    void OnMLeftUp( wxMouseEvent& event );

    void OnMRightDown( wxMouseEvent& event );
    void OnAddTag( wxCommandEvent& event );
    void OnEditTag( wxCommandEvent& event );
    void OnDelTag( wxCommandEvent& event );
    void OnDelMod( wxCommandEvent& event );
    void OnDelPort( wxCommandEvent& event );
    void OnDelLink( wxCommandEvent& event );

    ///Add to network fuctions
    void AddtoNetwork( ves::conductor::UIPluginBase *new_mod, std::string cls_name );
    void AddTag( int x, int y, wxString text );
    ///Load calls new when loading the network
    ///this also calls delete objects on xplorer but the user may not
    ///always want this action
    ///\param system The system pointer for this network to represent
    ///\param parent The parent canvas for this network
    void LoadSystem( ves::open::xml::model::SystemPtr system, Canvas* parent );
    void SetSystem( ves::open::xml::model::SystemPtr system );
    //add defaultplugin to newly created network system
    void CreateSystem( Canvas* parent, unsigned int id );
    ///Acessors
    std::pair< double, double >* GetUserScale( void );
    std::pair< unsigned int, unsigned int >* GetNumPix( void );
    std::pair< unsigned int, unsigned int >* GetNumUnit( void );

    //void HighlightSelectedIcon( UIPluginBase* cur_module, wxDC &dc);
    //void DrawPorts( UIPluginBase* cur_module, bool flag, wxDC &dc);
    bool IsDragging();
    void SetSelectedModule( int mod );
    void UnSelectMod();
    void HighlightCenter( int modId );
    void SetSelectedLink( int link );
    void HighlightCenterLink( int linkId );
    std::map< int, ves::conductor::Module > modules; //The list of modules;
    //The list of links between the nodes of moduls.
    std::vector< ves::conductor::util::Link > links;
    //void ReDraw(wxDC &dc);
    void DrawNetwork( wxDC &dc );
    ///Push all the event handlers from the tags, plugins, and links onto canvas
    void PushAllEvents();
    ///Remove all of the event handlers from the tags,
    ///plugins, and links from the canvas
    void RemoveAllEvents();
    ///Clear the plugins from the xplorer environment
    void ClearXplorer();
    ///Update all the ve models
    void Update();
    ///Remove plugin dialogs from the canvas 
    void RemovePluginDialogs();
    ///Delete plugins from plugins
    void OnDeletePlugins( wxUpdateUIEvent& event );
    ///Set the id for this network
    void SetNetworkID( std::string id );
    std::pair< int, int > GetNetworkSize( );
    std::pair< int, int > GetScrollPosition();
    void SetScrollPosition( int x, int y );
    
protected:
    //Selection functions
    int SelectMod( int x, int y, wxDC& dc );
    //void UnSelectMod();
    int  SelectLink( int x, int y );
    void UnSelectLink( );
    int SelectTag( int x, int y );
    void UnSelectTag( wxDC& dc );

    //Move and drop functions
    void MoveModule( int x, int y, int mod );//, wxDC &dc);
    void DropModule( int x, int y, int mod );
    void MoveLinkCon( int x, int y, int ln, int ln_con, wxDC& dc );
    void DropLinkCon( int x, int y, int ln, int ln_con, wxDC& dc );
    void MoveTagCon( int x, int y, int t, int t_con, wxDC &dc );
    void DropTagCon( int x, int y, int t, int t_con, wxDC &dc );
    void MoveTag( int x, int y, int t, wxDC &dc );
    void DropTag( int x, int y, int t, wxDC &dc );
    void TryLink( int x, int y, int mod, int pt, wxDC &dc, bool flag );
    void DropLink( int x, int y, int mod, int pt, wxDC &dc, bool flag );

    //Math functions for the relationship of points and polygons
    double computenorm( wxPoint pt1, wxPoint pt2 );

    //Misc functions
    void CleanRect( wxRect box, wxDC& dc ); // for wipeout a rectangular area
    wxPoint GetFreePos( wxRect bbox ); // for finding a free start location for a new module

    ///Get the point for a port or connector for a selected plugin
    ///portType is either input or output
    wxPoint GetPointForSelectedPlugin( unsigned long moduleID, unsigned int portNumber, std::string portType );

    //Check if the two port is compatible
    bool IsPortCompatible( int frmod, int frport, int tomod, int toport );

    //Used to keep track of the network size
    void SetNetworkSize(int x, int y);

    int m_selMod; // selected module
    int m_selFrPort; // selected From port
    int m_selToPort; // selected To port;
    int m_selLink; //selected Link
    int m_selLinkCon; //selected Link Connector
    int m_selTag; //selected Tag
    int m_selTagCon; //selected Tag Connector
    //Three main list of network objs
    //The list of text tags
    std::vector< ves::conductor::util::Tag > tags;
    
    // the relative point of the polygon, used by the move module function
    wxPoint relative_pt;

private:
    int intfssize;
    wxProgressDialog* _fileProgress;
    bool isLoading;
    bool isDataSet;
    bool tryingLink;
    bool dragging;
    wxBitmap * bitmapBuffer;
    wxPoint point1;
    wxPoint point2;
    int mMouseOverPlugin;

    ///UUID for this network
    std::string networkID;
    ///Event to delete networks
    wxUpdateUIEvent networkDeleteEvent;
    
    unsigned int pluginID;
    ves::conductor::util::CORBAServiceList* mServiceList;
    ves::conductor::XMLDataBufferEngine* mDataBufferEngine;
    ves::conductor::UserPreferencesDataBuffer* mUserPrefBuffer;
    
    std::string tempXMLNetworkData;
    //start up bounding box; used by GetFreePos to calc start module location
    std::vector< wxRect > sbboxes;
    //The old location of the mouse position, used by the TryLink to wipe
    //the old tried link route
    int xold, yold;
    //The mouse position when the right button clicked, used by menu event handlers
    wxPoint action_point;
    ///System for this network
    ves::open::xml::model::SystemPtr systemPtr;
    ///Parent window pointer to the splitter in AppFrame
    Canvas* parent;
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
    std::pair< int, int > networkSize;
    std::pair< int, int > scrollPos;

    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }

    DECLARE_EVENT_TABLE() // no semicolon needed
};
}
}

#endif //VES_APPS_CONDUCTOR_NETWORK_H
