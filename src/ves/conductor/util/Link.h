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
#ifndef CONDUCTOR_GUI_UTILITIES_LINK_
#define CONDUCTOR_GUI_UTILITIES_LINK_
/*!\file Link.h
Link API
*/
/*!\class ves::conductor::util::Link
*
*/
#include <vector>
#include <string>
#include <utility>

#include <ves/VEConfig.h>
#include <ves/conductor/util/Polygon.h>
#include <ves/open/xml/model/LinkPtr.h>
#include <ves/open/xml/model/ModelPtr.h>

#include <wx/gdicmn.h>
#include <wx/dc.h>
#include <wx/event.h>

class wxScrolledWindow;

namespace ves
{
namespace conductor
{
namespace util
{
class VE_CONDUCTOR_UTILS_EXPORTS Link : public wxEvtHandler
{
public:
    ///Constructor
    Link( wxScrolledWindow* designCanvas, wxEvtHandler* handler );
    ///Destructor
    ~Link( void );
    ///Copy Constructor
    Link( const Link& );
    ///equal operator
    Link& operator= ( const Link& );
    ///Test for equality operator
    friend inline bool operator== ( const Link& l1, const Link& l2 )
    {
        if (
            ( l1.Fr_mod == l2.Fr_mod ) &&
            ( l1.To_mod == l2.To_mod ) &&
            ( l1.Fr_port == l2.Fr_port ) &&
            ( l1.To_port == l2.To_port )
        )
        {
            return true;
        }
        return false;
    }

    wxPoint* GetPoint( size_t i );
    size_t GetNumberOfPoints( void );
    std::vector< wxPoint >* GetPoints( void );
    void SetPoint( wxPoint* pnt );
    unsigned int GetFromPort( void );
    unsigned int GetToPort( void );
    unsigned long GetFromModule( void );
    unsigned long GetToModule( void );

    void SetFromPort( unsigned int );
    void SetToPort( unsigned int );
    void SetFromModule( unsigned long );
    void SetToModule( unsigned long );
    Polygon* GetPolygon( void );

    void SetName( wxString name );
    wxString GetName();

    void SetType( int type );
    int GetType();

    ///Helper functions
    void DrawLinkCon( wxDC* dc );
    void CalcLinkPoly( void );
    void DrawLinkLine( wxDC* dc );
    void DrawName( wxDC* dc );
    double computenorm( wxPoint pt1, wxPoint pt2 );
    ///Set the user scale to enable working with the dc
    void SetDCScale( std::pair< double, double >* scale );
    ///Set UUID for this link
    void SetUUID( std::string uuid );
    ///Get UUID for this link
    std::string GetUUID();
    ///Set highlight flag for link
    void SetHighlightFlag( bool flag );
    ///Draw link
    void DrawLink( wxDC* dc );
    ///Set ves::open::xml::model::Link
    void SetLink( ves::open::xml::model::LinkPtr inputLink );
    ///Get ves::open::xml::model::Link
    ves::open::xml::model::LinkPtr GetLink();
    size_t GetMaxPointX()
    {
        return maxPointX;
    }
    size_t GetMaxPointY()
    {
        return maxPointY;
    }

protected:
    void OnShowLinkContent( wxCommandEvent& event );
    void OnShowAspenName( wxCommandEvent& event );
    void OnQueryStreamInputs( wxCommandEvent &event );
    void OnQueryStreamOutputs( wxCommandEvent &event );
    void OnAddLinkCon( wxCommandEvent &event );
    void OnDelLink( wxCommandEvent &event );
    void OnDelLinkCon( wxCommandEvent &event );
    void OnSetLinkName( wxCommandEvent &event );
    void OnMRightDown( wxMouseEvent &event );
    void OnSetActiveLinkID( wxUpdateUIEvent& event );
    bool SelectLink( int x, int y );
    void OnShowAspenDynName( wxCommandEvent& event );
    void OnQueryStreamAllVars( wxCommandEvent &event );
    ///Check the active id against the plugin id
    bool CheckID();

private:
    ///Name of the link, should be unique
    wxString linkName;
    wxEvtHandler* mPostHandler;
    //The Type of the link
    //for aspen material = 0, heat = 1, & work = 2 
    int linkType;
    ///The active link uuid for the network
    std::string activeUUID;
    ///From plugin for the link
    unsigned long Fr_mod;
    ///To plugin for the link
    unsigned long To_mod;
    ///From port for the link
    unsigned int Fr_port;
    ///to port for the link
    unsigned int To_port;
    ///Used for the construction of arrow heads
    double sinb;
    ///Used for the construction of arrow heads
    double cosb;
    ///Used for the construction of arrow heads
    double sina;
    ///Used for the construction of arrow heads
    double cosa;
    ///UUID for this link
    std::string m_uuid;
    ///Highlight flag to control drawing
    bool highlightFlag;

    int m_selFrPort; // selected From port
    int m_selToPort; // selected To port;
    int m_selLinkCon; //selected Link Connector

    size_t maxPointX;
    size_t maxPointY;

    std::vector< wxPoint > cons; //connectors
    Polygon poly; //Poly is the current poly on the canvas
    wxScrolledWindow* networkFrame;
    //The mouse position when the right button
    //clicked, used by menu event handlers
    wxPoint action_point;
    ///User scale
    /// first = x scale
    /// second = y scale
    std::pair< double, double >* userScale;
    ///The XML Link rep
    ves::open::xml::model::LinkPtr m_veLink;
    ves::open::xml::model::ModelWeakPtr parentModel;


    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }


    DECLARE_EVENT_TABLE()
};
}
}
}

#define UILINK_CHECKID(event)  \
    if( !CheckID() ) \
    { \
        event.Skip(); \
        return; \
    }

#endif
